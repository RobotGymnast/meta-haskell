#!/bin/bash

dir=`pwd`

rm -rf /tmp/testbuild &&
cp -r ./ /tmp/testbuild &&
cd /tmp/testbuild &&
rm -f .git/hooks/pre-commit &&
git commit -m "Temp" &&
git reset --hard HEAD &&
git clean -fd &&
scripts/build.sh &&
echo &&
scripts/linecheck.sh

ret=$?

cd "$dir"
exit $ret
