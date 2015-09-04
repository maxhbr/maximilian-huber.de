#!/bin/sh
#
# generate encrypted strings with:
# echo TO_ENCRYPT | openssl enc -aes-128-cbc -a -salt -pass pass:PASSWD

HOST='U2FsdGVkX187Ydag+Px60qaSZ/Yz3hf2PJiKBWuyvpsX6hskJV5zo+2Yx/EB+duW'
USER='2FsdGVkX19PP81C9/mfaa2QA2OXgzHACNJ7lrIEMUc='
TARGETFOLDER='/'
SOURCEFOLDER="$HOME/maximilian-huber/_site-diff"
if [ $# -eq 1 ]; then
  if [ $1 -eq "full" ]; then
    SOURCEFOLDER="$HOME/maximilian-huber/_site"
  fi
fi
PASS='U2FsdGVkX1/+fRa9RGfxVA92wxUdZ8k6J3TUCZceOELgp8DTBiX3+yZPCGbiIDxy'


###############################################################################

echo "pass: "
read -s tPASS
HOST=`echo $HOST | openssl enc -aes-256-cbc -a -d -salt -pass pass:${tPASS}`
USER=`echo $USER | openssl enc -aes-256-cbc -a -d -salt -pass pass:${tPASS}`
PASS=`echo $PASS | openssl enc -aes-256-cbc -a -d -salt -pass pass:${tPASS}`

###############################################################################

# fix permissions
for dir in "$SOURCEFOLDER"; do
  find "$dir" -type d -exec chmod 755 {} \;
  find "$dir" -type f -exec chmod 644 {} \;
done

# upload
lftp -f "
set ftp:ssl-allow no
open $HOST
user $USER $PASS
lcd $SOURCEFOLDER
mirror --reverse --verbose $SOURCEFOLDER $TARGETFOLDER
bye
"
