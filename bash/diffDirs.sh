#!/bin/sh

echo 
echo "Make diff"

NEW="/home/hubi/maximilian-huber/_site/"
OLD="/home/hubi/maximilian-huber/_site-old/"
DIFF="/home/hubi/maximilian-huber/_site-diff/"

pushd /home/hubi/maximilian-huber/_site

rm -r ${DIFF}
mydirs=$(find . \( ! -regex '.*/\..*' \) -type d)
for item in $mydirs ; do
  mkdir -p $DIFF$item
done

myfiles=$(find . \( ! -regex '.*/\..*' \) -type f)
for item in $myfiles; do
  if [ -f $OLD$item ]; then
    a=$(md5sum $OLD$item | awk '{print $1}')
    b=$(md5sum $NEW$item | awk '{print $1}')
    if [ "$b" == "$a" ] ; then
      continue
    fi
  fi
  echo "    $item"
  cp "${New}${item}" "${DIFF}${item}"
done
