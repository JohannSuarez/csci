#! /bin/bash

echo "TEST 1: should be ok, just one postal"
./lab4 < regex_individual_tests/1_postal

echo "TEST 2: should be error, a wrong postal"
./lab4 < regex_individual_tests/1_wrong_postal

echo "TEST 3: should be ok, three-name entry"
./lab4 < regex_individual_tests/3_name_entries

echo "TEST 4: should be ok, just one name"
./lab4 < regex_individual_tests/just1_name
