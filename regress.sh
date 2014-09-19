cabal build > /dev/null
./dist/build/PBuddy/pbuddy < input.txt > output.txt
./dist/build/PBuddy/pbuddy < input1.txt > output1.txt
diff output.txt expected.txt
diff output1.txt expected1.txt
echo "DONE!"
