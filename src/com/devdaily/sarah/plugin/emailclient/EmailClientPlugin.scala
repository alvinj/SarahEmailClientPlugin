package com.devdaily.sarah.plugin.emailclient

import com.devdaily.sarah.plugins._
import java.util.Date
import scala.collection.mutable.ListBuffer
import net.liftweb.json._
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import javax.mail.MessagingException
import scala.collection.mutable.HashMap
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.Actor
import com.weiglewilczek.slf4s.Logging
import com.weiglewilczek.slf4s.Logger
import com.devdaily.sarah.actors.StartPluginMessage
import com.devdaily.sarah.actors.SetPluginDir

// TODO read configuration file on startup
// TODO handle multiple email accounts
// TODO may have problems when network connection is down, test 
// DONE don't notify about the same message more than one time
class AkkaEmailClientPlugin 
extends SarahAkkaActorBasedPlugin
with Logging
{
  val log = Logger("DDEmailClient")
  var helper:ActorRef = _

  def receive = {
    case SetPluginDir(canonDir) =>          // this message is received first
         startHelper
         helper ! SetPluginDir(canonDir)

    case StartPluginMessage(theBrain) =>    // this message is received second
         brain = theBrain
         helper ! SetBrainReference(theBrain)
         helper ! StartHelperLoop

    case whatTheHeck => 
         log.info("RandomNoisePlugin got an unexpected message:")
         log.info(whatTheHeck.toString)
  }
  
  def startHelper {
    helper = context.actorOf(Props(new EmailClientHelper), name = "EmailClientHelper")
  }
  
  /**
   * TODO These methods are needed to implement the SarahPlugin.
   *      Fix the API so they aren't needed for actors?
   */
  def textPhrasesICanHandle: List[String] = List("foo bar baz")
  def handlePhrase(phrase: String): Boolean = false
  def startPlugin = ()

}

// our messages
case object StartHelperLoop
case class SetBrainReference(brain: ActorRef)

// helper class
class EmailClientHelper
extends Actor
with Logging
{
  
  val log = Logger("DDEmailClientHelper")
  private var brain:ActorRef = _

  // sarah sets this directory for us
  var canonPluginDirectory = ""
  var canonPluginFilename = "" 
  val relativePluginFilename = "DDEmailClient.props"

  var emailAccount: DDEmailAccount = null
  implicit val formats = DefaultFormats // Brings in default date formats etc.
  var minutesToSleep = 1
 
  // don't tell the user about the email message more than once
  val messagesAlreadyToldTheUserAbout = new ListBuffer[SarahEmailMessage]

  def receive = {
    case SetPluginDir(canonDir) =>
         configPluginDirInfo(canonDir)

    case SetBrainReference(theBrain) =>
         brain = theBrain

    case StartHelperLoop =>
         startLoop

    case unknown => 
         log.info(format("got an unknown request(%s), ignoring it", unknown.toString))
  }
  
  def configPluginDirInfo(canonPluginDir: String) {
    canonPluginDirectory = canonPluginDir
    canonPluginFilename = canonPluginDirectory + PluginUtils.getFilepathSeparator + relativePluginFilename
  }
  
  def startLoop {
    while (true) {
      log.info("(EmailClient) in loop ...")
      sleepSpecifiedMinutes
      log.info("(EmailClient) calling checkEmailAccounts ...")
      try {
        checkEmailAccounts
      } catch {
        // TODO i can dig deeper into these exceptions to find out what really happened
        case me: MessagingException => log.info(me.getMessage) 
        case e:  Exception => log.info("(EmailClient) Caught a plain Exception: " + e.getMessage) 
        case unknown => log.info("(EmailClient) Caught an unknown problem/exception.")
      }
    }
  }

  def checkEmailAccounts {

    readConfigFile(canonPluginFilename)
    
    // 1) get list of messages
    val imap = new ImapReader
    log.info("(EmailClient) calling getRecentMessages ...")
    val messages = imap.getRecentMessages(emailAccount, emailAccount.getUsersOfInterest)
    log.info("(EmailClient) got messages, #messages = " + messages.length)
    if (messages.length == 0) return
    
    // 2) got at least one message, continue
    val addresses = new ListBuffer[String]
    for (message <- messages) {
      if (!messagesAlreadyToldTheUserAbout.contains(message)) {
        val addy = message.getEmailAddressAsString
        addresses += addy
        messagesAlreadyToldTheUserAbout += message
      } else {
        log.info("already told user about current message, not going to tell them again")
        log.info(format("person (%s), subject (%s)", message.getEmailAddressAsString, message.subject))
      }
    }
    if (addresses.size == 0) return
    
    // 3) found at least one new address, put a sentence together, and send it to sarah 
    val whatToSay = getEmailTextToSay(addresses.toList)
    if (! whatToSay.trim.equals("")) {
      brain ! PleaseSay(whatToSay)
    }

  }
  
  
  def getEmailTextToSay(addresses: List[String]): String = {
    val emailsByPerson = getEmailCountByPerson(addresses)
    if (emailsByPerson.size == 0) {
      log.info("emailsByPerson.size == 0")
      return "There were no new email messages."
    } else {
      log.info("emailsByPerson.size != 0")
      val sb = new StringBuilder
      var count = 0
      for ((key, value) <- emailsByPerson) {
        log.info("key = " + key)
        log.info("value = " + value)
        if (count == 0) { sb.append("You have "); count += 1 }
        if (value == 1)
          sb.append(format("%d new message from %s. ", value, key))
        else
          sb.append(format("%d new messages from %s. ", value, key))
      }
      return sb.toString
    }
    return ""
  }
  
  // a map of [email address] to [emails by this person]
  def getEmailCountByPerson(addresses: List[String]): HashMap[String, Integer] = {
    val emailsByPerson = new HashMap[String, Integer]
    for (address <- addresses) {
      if (emailsByPerson.keySet.contains(address)) {
        // already in map, increment count
        val currentCount = emailsByPerson.get(address).get
        emailsByPerson(address) = currentCount + 1
      } else {
        // not in map, set count to 1
        emailsByPerson(address) = 1
      }
    }
    return emailsByPerson
  }
  
  def readConfigFile(canonConfigFilename: String) {
    log.info("canonConfigFilename = " + canonConfigFilename)
    log.info("reading file contents, getting 'text'")
    val text = PluginUtils.getFileContentsAsString(canonConfigFilename)
    log.info("parsing text to json")
    val json = parse(text)
    log.info("extracting emailAccount")
    emailAccount = json.extract[DDEmailAccount]
    log.info("printing emailAccount")
    log.info(emailAccount.toString)
    val l = emailAccount.getUsersOfInterest
    l.foreach(println)
    minutesToSleep = emailAccount.getMinutesBetweenChecks
  }
  
  /**
   * TODO these next two methods are needed to implement SarahPlugin.
   *      fix the API so these aren't needed for Actor plugins.
   */
  def textPhrasesICanHandle: List[String] = {
    return List("let me talk to the email client")
  }

  def handlePhrase(phrase: String): Boolean = {
    return false
  }

  def sleepSpecifiedMinutes {
    PluginUtils.sleep(minutesToSleep * 60 * 1000)
  }

  
} // end of EmailClientPlugin

case class DDEmailAccount(
    accountName: String,
    username: String,
    password: String,
    mailbox: String,
    imapServerUrl: String,
    minutesBetweenChecks: Int,
    protocol: String,
    usersOfInterest: List[String]
    )
{ 
  def getAccountName = accountName
  def getUsername = username
  def getPassword = password
  def getMailbox = mailbox
  def getImapServerUrl = imapServerUrl
  def getMinutesBetweenChecks = minutesBetweenChecks
  def getProtocol = protocol
  def getUsersOfInterest = usersOfInterest
  
  override def toString: String = {
    return format("acct (%s), user (%s), url (%s)", accountName, username, imapServerUrl)
  }
}



/**
 * Wrap a JavaMail Message. Needed because we handle messages here, but get them from
 * another function, and that function closes the mailbox when it finishes. If we 
 * try to read JavaMail messages directly from that function, the mailbox will already
 * be closed, and we'll throw a MailboxClosedException (because messages are loaded
 * lazily).
 */
case class SarahEmailMessage(from: Array[javax.mail.Address],
                             subject: String,
                             sentDate: Date,
                             receivedDate: Date,
                             content: AnyRef)
{
  
  // returns the first email address found
  def getEmailAddressAsString: String = {
    for (address <- from) {
      // assuming name address is like "Alvin Alexander <alvin...@..."
      val tmp = address.toString.split("<")
      return tmp(0).trim
    }
    return "unknown"
  }

  // TODO this can be cleaned up once the hash code algorithm is working better
  override def equals(that: Any): Boolean = {
    var result = false
    if (! that.isInstanceOf[SarahEmailMessage]) result = false
    val tmp = that.asInstanceOf[SarahEmailMessage]
    if (this.hashCode == that.hashCode) result = true
    println("this.hash = " + this.hashCode)
    println("that.hash = " + that.hashCode)
    return result
  }

  // TODO improve this
  override def hashCode = subject.hashCode + getEmailAddressAsString.hashCode
                        + sentDate.hashCode
}


                            
                            
                            












