#
# this script relies on class files being in the 'bin' directory
# above this directory. that's a hack. you can probably just use
# `sbt package` now.
#

rm -r com
rm .DS_Store
rm *jar

cp -r ../bin/com .
jar cvf EmailClient.jar .

echo ""
echo "Copy this jar file to the right place. Make a .info file too."

