#!/bin/bash
# scales all images

# Last modified: Thu Mar 26, 2015  10:13

# written by Maximilian Huber
#            maximilian-huber.de

#jhead -autorot -nf%y%m%d-%f **/DSC*.jpg

FAIL=0

###############################################################################
###############################################################################
###############################################################################
IN="$HOME/Bilder/00-galerie/"

[ -d $OUT ] || exit 1
pushd $IN

OUT="$HOME/maximilian-huber/galerie/"
#mkdir -p ${OUT}.md5/
mydirs=$(find . \( ! -regex '.*/\..*' \) -type d)
for item in $mydirs ; do
  mkdir -p $OUT$item
  mkdir -p ${OUT}.md5/${item}
done

###############################################################################
###############################################################################
###############################################################################
myfiles=$(find . \( ! -regex '.*/\..*' \) -name '*.jpg' -type f)
echo "$myfiles" | wc -l
for item in $myfiles ; do
  if [ ! -f "${OUT}.md5/${item}.md5" ] ; then
    touch "${OUT}.md5/${item}.md5"
    a=""
  else
    a=$(cat "${OUT}.md5/${item}.md5")
  fi
  b=$(md5sum $item | awk '{print $1}')
  if [ ! "$b" == "$a" ] ; then
    echo "do       $item"
    ( convert $item \
        -resize '1920x1080>'\
        -quality 85%\
        -font /usr/share/fonts/TTF/DejaVuSans.ttf -pointsize 18 -gravity SouthEast\
        -fill rgba\(200,200,200,1\) -draw "text 7,7 '©Maximilian-Huber.de'"\
        -fill rgba\(55,55,55,1\) -draw "text 6,6 '©Maximilian-Huber.de'"\
        -unsharp 1.5x1.2+1.0+0.10\
        -interlace Plane\
        -strip\
        $OUT$item;\
      echo $b > "${OUT}.md5/${item}.md5"
      echo "done     $item" )&
  fi

  # limit subprocesses to 10
  while [ $(jobs -p | wc -l) -gt 9 ] ; do
    sleep 1
  done
done

###############################################################################
###############################################################################
###############################################################################
for job in `jobs -p` ; do
  wait $job || let "FAIL+=1"
done

#popd
#pushd $OUT
#myfiles=$(find . \( ! -regex '.*/\..*' \) -name '*.jpg' -type f)
#echo "$myfiles" | wc -l
#echo "to REMOVE: "
#for item in $myfiles ; do
  #if [ ! -f $IN$item ] ; then
    #echo $item
  #fi
#done

if [ $FAIL -gt 0 ] ; then
  echo "fails:"
  echo $FAIL
fi

echo "all done"
popd
