#!/bin/zsh

FLASH_DIR = /Volumes/$1
cabal build > /dev/null
./dist/build/PBuddy/pbuddy < $FLASH_DIR/input.txt > $FLASH_DIR/A0112054.txt

echo "Ok done."
