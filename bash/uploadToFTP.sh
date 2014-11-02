#!/bin/sh
#
# generate encrypted strings with:
# echo TO_ENCRYPT | openssl enc -aes-128-cbc -a -salt -pass pass:PASSWD

HOST='U2FsdGVkX1+wIZvS7BWx88K0GudG3pghJJoYXw+78fUPeV6u4+DFp6s2PDjNq1fb'
USER='U2FsdGVkX187YKKAYgp+go5L6fqFiiRcXaW8YOal9c8='
TARGETFOLDER='/'
SOURCEFOLDER='/home/hubi/maximilian-huber/_site-diff'
if [ $# -eq 1 ]; then
  if [ $1 -eq "full" ]; then
    SOURCEFOLDER='/home/hubi/maximilian-huber/_site'
  fi
fi
PASS="U2FsdGVkX18AUJ+kKMHsbY2LC9cigLd3JzyKQj6OnPeUdTZegw3WoqZ8c3HlyJLI"

###############################################################################

echo "pass: "
read -s tPASS
HOST=`echo $HOST | openssl enc -aes-128-cbc -a -d -salt -pass pass:${tPASS}`
USER=`echo $USER | openssl enc -aes-128-cbc -a -d -salt -pass pass:${tPASS}`
PASS=`echo $PASS | openssl enc -aes-128-cbc -a -d -salt -pass pass:${tPASS}`

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
