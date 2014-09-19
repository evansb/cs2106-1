#!/bin/zsh

FLASH_DIR=/Volumes/$1
echo $FLASH_DIR
cabal install > /dev/null
cabal build > /dev/null
./dist/build/PBuddy/pbuddy < $FLASH_DIR/input.txt > $FLASH_DIR/A0112054Y.txt

echo "Ok done."
cd $FLASH_DIR

ls -a
