#!/bin/sh

# not ready

HOST='mysite.com'
USER='myuser'
TARGETFOLDER='/new'
SOURCEFOLDER='/home/myuser/backups'
# generate with:
# echo TO_ENCRYPT | openssl enc -aes-128-cbc -a -salt -pass pass:PASSWD
PASS="U2FsdGVkX18qAdhqop1SffsewHue6EOPNKv9dXc/0rI="

echo "pass: "
read tPASS
PASS=`echo $PASS | openssl enc -aes-128-cbc -a -d -salt -pass pass:${tPASS}`




# lftp -f "
# open $HOST
# user $USER $PASS
# lcd $SOURCEFOLDER
# mirror --reverse --delete --verbose $SOURCEFOLDER $TARGETFOLDER
# bye
# "
