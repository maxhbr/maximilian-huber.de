Um eine PDF per konsole als Booklet oder Broschüre zu drucken nutze ich:
```Bash
lprBook() {
  [[ -f $1 ]] && {
    pdftops -paperw 420 -paperh 595 $1 - \
      | psbook -q \
      | ps2pdf14 - - \
      | lpr -o sides=two-sided-short-edge \
            -o number-up=2 \
            -o fit-to-page \
            -o media=A4
  } || { echo "file not found"; }
}
```
Dazu ist aber zusätzliche Software nötig.

Dies und mehr ist in meiner 
[.aliasrc](https://github.com/maxhbr/myconfig/blob/master/aliasrc) zu
finden.
