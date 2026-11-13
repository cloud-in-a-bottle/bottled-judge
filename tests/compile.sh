#!/bin/sh
f=0
for p in /src/tests/compile/*; do
  b=${p##*/}; w=${b%%-*}; x=${b##*.}
  d=$(mktemp -d); chown run $d; cp $p $d/main.$x; : > $d/in
  r=$(judge compile $x $d | head -1)
  case $r in "$w "*) ;; *) echo "FAIL $b $r"; f=1; continue;; esac
  if [ $w = OK ]; then
    c=./main; [ $x = py ] && c="python3 main.py"
    judge run 1000 256 $d/in $d/out $d -- $c >/dev/null
    [ "$(cat $d/out)" = hi ] || { echo "FAIL $b output"; f=1; continue; }
  fi
  echo "ok   $b $r"
done
exit $f
