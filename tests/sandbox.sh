#!/bin/sh
d=$(mktemp -d); chown run $d; cd $d
seq 1000 > in
mkdir -p -m 700 /tmp/s; echo x > /tmp/s/x
f=0
judge run 1000 256 in out $d -- cat >/dev/null
cmp -s in out && echo "ok   cat" || { echo "FAIL cat"; f=1; }
for p in /src/tests/sandbox/*; do
  w=${p##*/}; w=${w%%-*}
  r=$(judge run 1000 256 in out $d -- sh $p)
  case $r in "$w "*) echo "ok   $p $r";; *) echo "FAIL $p $r"; f=1;; esac
done
pgrep -u run >/dev/null && { echo "FAIL leftovers"; f=1; }
exit $f
