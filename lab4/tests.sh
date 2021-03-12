#! /bin/bash


# --- Using the vanilla tests that came from the original lab4 pull ----- #

echo -e "\e[0;34mBeginning of vanilla tests that came from the original lab4 pull. \n \e[0m"

echo -e "\e[1;31m\nAll these tests are expected to fail, because the vanilla grammar is expected to be  \e[0m"
echo -e "\e[1;31mDATASET integer , integer , integer , integer \n \e[0m"

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


echo -e "\e[0;34mEnd of vanilla tests that came from the original lab4 pull. \n \e[0m"



# --- Using the tests from the Lab4 Specification Page

echo -e "\033[33;32mBeginning of Lab-4 Tests from Specification Page \n \e[0m"


echo -e "\033[33;32m\nTEST 1: should be ok. Single entry, all one line.\e[0m"
./lab4 < tests/valid_test1

echo -e "\033[33;32m\nTEST 2: should be ok. Single entry, varrying whitespace.\e[0m"
./lab4 < tests/valid_test2

echo -e "\033[33;32m\nTEST 3:  should be ok. Multiple entries\e[0m"
./lab4 < tests/valid_test3

echo -e "\e[1;31m\nTEST 4: should fail, missing DATASET\e[0m"
./lab4 < tests/invalid_test1

echo -e "\e[1;31m\nTEST 5: should fail, missing .\e[0m"
./lab4 < tests/invalid_test2

echo -e "\e[1;31m\nTEST 6: should fail, wrong seperator\e[0m"
./lab4 < tests/invalid_test3

echo -e "\e[1;31m\nTEST 7: should fail, integer begins with 0.\e[0m"
./lab4 < tests/invalid_test4


echo -e "\033[33;32mEnd of Lab-4 Tests from Specification Page \n \e[0m"
