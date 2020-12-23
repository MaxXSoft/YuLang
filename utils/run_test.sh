#!/bin/bash

base=`dirname $0`
build_dir=$1

pass=0
total=0

for out in $base/output/*.out; do
  name=`basename $out`
  $build_dir/examples/${name%.out} | diff - $out
  if [ ${PIPESTATUS[1]} -eq 0 ]; then
    pass=$((pass+1))
  fi
  total=$((total+1))
done

if [ $pass -eq $total ]; then
  echo "PASSED ($pass/$total)"
else
  echo "FAILED ($pass/$total)"
fi
exit $((total-pass))
