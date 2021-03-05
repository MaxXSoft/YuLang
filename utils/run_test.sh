#!/bin/bash

base=`dirname $0`
build_dir=$1

pass=0
total=0

for out in $base/output/*.out; do
  name=`basename $out`
  input="$base/input/${name%.out}.in"
  exe="$build_dir/examples/${name%.out}"
  if [ -e $input ]; then
    $build_dir/examples/${name%.out} < $input | diff - $out
  else
    $build_dir/examples/${name%.out} | diff - $out
  fi
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
