#! /bin/bash

echo "TEST 1: should be ok, two entries (all on one line)"
./lab4 < tests/oktest1

echo "TEST 2: should be ok, one entry (spread across multiple lines)"
./lab4 < tests/oktest2

echo "TEST 3: should fail, missing the DATASET at start"
./lab4 < tests/errtest1

echo "TEST 4: should fail, wrong kind of seperator"
./lab4 < tests/errtest2

echo "TEST 5: should fail, extra number at end"
./lab4 < tests/errtest3

