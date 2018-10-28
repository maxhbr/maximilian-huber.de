#!/usr/bin/env nix-shell
#! nix-shell -i bash -p imagemagick
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# scales all images


#jhead -autorot -nf%y%m%d-%f **/DSC*.jpg

set -e

FAIL=0

###############################################################################
###############################################################################
###############################################################################
IN="$HOME/Bilder/00-galerie/"
OUT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../galerie/"

cd $IN
mydirs=$(find . \( ! -regex '\./[\._].*' \) -type d)
for item in $mydirs ; do
  mkdir -p $OUT$item
  mkdir -p ${OUT}.md5/${item}
done

###############################################################################
###############################################################################
###############################################################################
myfiles=$(find . \( ! -regex '\./[\._].*' \) \( -name '*.jpg' -o -name '*.png' -o -name '*.JPG' \) -type f)
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
    outItem="${item%.*}.jpg"
    font=/nix/var/nix/profiles/system/sw/share/X11-fonts/DejaVuSans.ttf
    # font=/usr/share/fonts/TTF/DejaVuSans.ttf
    ( convert $item \
        -resize '1920x1080>'\
        -quality 85%\
        -font $font -pointsize 18 -gravity SouthEast\
        -fill rgba\(200,200,200,1\) -draw "text 7,7 '©Maximilian-Huber.de'"\
        -fill rgba\(55,55,55,1\) -draw "text 6,6 '©Maximilian-Huber.de'"\
        -unsharp 1.5x1.2+1.0+0.10\
        -interlace Plane\
        -strip\
        $OUT$outItem;\
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

if [ $FAIL -gt 0 ] ; then
  echo "fails:"
  echo $FAIL
fi
