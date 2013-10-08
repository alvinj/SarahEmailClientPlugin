package com.devdaily.sarah.plugin.emailclient

import javax.mail._
import javax.mail.internet._
import javax.mail.search._
import java.util.Properties
import scala.collection.mutable.ListBuffer

/**
 * Shows AndTerm and SentDateTerm: 
 *   stackoverflow.com/questions/4820450/is-it-possible-to-use-imap-query-terms-in-javamail-with-gmail
 * Shows terms at bottom of page:
 *   http://java.sun.com/developer/onlineTraining/JavaMail/contents.html#JavaMailFetching
 * SentDateTerm and operators:
 *   http://geronimo.apache.org/maven/specs/geronimo-javamail_1.4_spec/1.6/apidocs/javax/mail/search/SentDateTerm.html
 */
class ImapReader {

  // TODO get Logger working

  def getRecentMessages(emailAccount: DDEmailAccount, 
                        notifyWhenMailReceivedFromTheseUsers: List[String]): ListBuffer[SarahEmailMessage] = {
    println("Entered EMailClient::getRecentMessages ...")
    val props = System.getProperties()
    props.setProperty("mail.store.protocol", emailAccount.getProtocol)
    val session = Session.getDefaultInstance(props, null)

    // declare these before the 'try' so i can close them in the finally clause
    var store: Store = null
    var inbox: Folder = null
    var messagesForSarah = new ListBuffer[SarahEmailMessage]()
    println("(ImapReader) - entering try block")
    try {
      println("(ImapReader) - connecting to store")
      store = session.getStore(emailAccount.getProtocol)
      connectToStore(store, emailAccount.getImapServerUrl, emailAccount.getUsername, emailAccount.getPassword)
      inbox = getFolder(store, emailAccount.getMailbox)
      inbox.open(Folder.READ_ONLY)

      val unseenFlagTerm = getUnseenTerm
      val recentFlagTerm = getRecentTerm
      val unseenAndRecentTerm = new AndTerm(unseenFlagTerm, recentFlagTerm);

      // get only the message info we need
      // @see http://www.zdnetasia.com/receiving-in-javamail-39304762.htm
      // @see http://javamail.kenai.com/nonav/javadocs/javax/mail/FetchProfile.html
      val fp = getFetchProfile

      // can't get AndTerm and OrTerm working together, so do this loop
      // for each user we're looking for
      for (user <- notifyWhenMailReceivedFromTheseUsers) {
        println("(ImapReader) looking for new, unseen mail from: " + user)
        
        val andTermForCurrentPerson = createAndTermForPerson(user)

        // do the actual search
        println("(ImapReader) - getting messages")
        val messages = inbox.search(andTermForCurrentPerson)
        if (messages.length != 0) {
          // found messages for this person
          inbox.fetch(messages, fp)
          
          // need to retrieve these messages here and re-bundle them, otherwise whoever
          // tries to use them later will get a FolderClosedException. this is part of
          // the "lazy" approach to loading messages that JavaMail uses.
          var count = 0
          val limit = 10
          println("(ImapReader) - found messages, adding to list (limit 10 per user)")
          for (message <- messages) {
            if (count < limit) {
              println("ImapReader: adding message to list")
              val m = SarahEmailMessage(message.getFrom,
                                        message.getSubject,
                                        message.getSentDate,
                                        message.getReceivedDate,
                                        message.getContent)
              messagesForSarah += m
            }
            count += 1
          }
        }
      } // end for loop of users
      return messagesForSarah
    } catch {
      // TOOD handle this with Some/None?
      case e: NoSuchProviderException =>  e.printStackTrace()
                                          return messagesForSarah
      case me: MessagingException =>      me.printStackTrace()
                                          return messagesForSarah
    } finally {
      inbox.close(true)
      store.close
    }
  }  
  
  def createAndTermForPerson(person: String) : AndTerm = {
    // create the AndTerm for this person
    val fromTerm = new FromStringTerm(person)
    val sentDateTerm = new SentDateTerm(ComparisonTerm.EQ, new java.util.Date)
    return new AndTerm(Array(fromTerm, sentDateTerm, getUnseenTerm))
  }
  
  def getFetchProfile: FetchProfile = {
    val fp = new FetchProfile
    fp add "Subject"
    fp.add("From")
    fp.add("SentDate")
    //fp.add("Content")
    return fp
  }
  
  

  // a search term for all "unseen" messages
  def getUnseenTerm: FlagTerm = {
    return new FlagTerm(new Flags(Flags.Flag.SEEN), false);
  }
  
  def getRecentTerm: FlagTerm = {
    return new FlagTerm(new Flags(Flags.Flag.RECENT), true);
  }
  
  def connectToStore(store: Store, imapUrl: String, username: String, password: String) {
    store.connect(imapUrl, username, password)
  }
  
  def getFolder(store: Store, folderName: String): Folder = {
    return store.getFolder(folderName)
  }
  

}

