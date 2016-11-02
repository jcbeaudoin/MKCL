#!/bin/sh

TESTSUITE_PATH=$HOME/gcc/gcc-3.2/gcc/testsuite/gcc.c-torture
MKCC="./mkcc -B. -I. -DNO_TRAMPOLINES" 
rm -f mkcc.sum mkcc.log
nb_failed="0"

for src in $TESTSUITE_PATH/compile/*.c ; do
  echo $MKCC -o /tmp/test.o -c $src 
  $MKCC -o /tmp/test.o -c $src >> mkcc.log 2>&1
  if [ "$?" = "0" ] ; then
     result="PASS"
  else
     result="FAIL"
     nb_failed=$(( $nb_failed + 1 ))
  fi
  echo "$result: $src"  >> mkcc.sum
done

for src in $TESTSUITE_PATH/execute/*.c ; do
  echo $MKCC $src 
  $MKCC $src >> mkcc.log 2>&1
  if [ "$?" = "0" ] ; then
     result="PASS"
  else
     result="FAIL"
     nb_failed=$(( $nb_failed + 1 ))
  fi
  echo "$result: $src"  >> mkcc.sum
done

echo "$nb_failed test(s) failed." >> mkcc.sum
echo "$nb_failed test(s) failed."
