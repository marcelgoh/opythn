#!/bin/bash

set -e  # abort if any command fails

echo "Running make..."
cd ..
make > /dev/null
cd test
echo "Compiled OK."

all_passed=1

rm -f "diff_log.txt" # ensure we start with blank log

echo "********** START TESTS **********"

for f in $(ls *.opy)
do
    name=${f%.opy}
    correct_file="${name}_correct.txt"
    output_file="${name}_output.txt"
    python3 "$f" > "$correct_file" # use python3 to generate correct output
    ../main "$f" > "$output_file"
    if cmp -s "$correct_file" "$output_file"; then  # test output against python3's
        echo "Passed:" "$f"
        rm "$correct_file" "$output_file" # remove outputs each time if correct
    else
        echo "Failed:" "$f"
        diff -u "$correct_file" "$output_file" >> "diff_log.txt"
        all_passed=0
    fi
done

echo "*********** END TESTS ***********"

if (($all_passed)) ; then
    echo "All tests passed."
else
    echo "Some tests failed. See contents of diff_log.txt."
fi
