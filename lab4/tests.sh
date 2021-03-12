#! /bin/bash

echo "TEST 1: should be ok. Single entry, all one line. "
./lab4 < tests/valid_test1

echo "TEST 2: should be ok. Single entry, varrying whitespace. "
./lab4 < tests/valid_test2

echo "TEST 3:  should be ok. Multiple entries"
./lab4 < tests/valid_test3

echo "TEST 4: should fail, missing DATASET"
./lab4 < tests/invalid_test1

echo "TEST 5: should fail, missing ."
./lab4 < tests/invalid_test2

echo "TEST 6: should fail, wrong seperator"
./lab4 < tests/invalid_test3

echo "TEST 7: should fail, integer begins with 0."
./lab4 < tests/invalid_test4


