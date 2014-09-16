cabal build > /dev/null
./dist/build/PBuddy/pbuddy < input.txt > output.txt
diff output.txt expected.txt
