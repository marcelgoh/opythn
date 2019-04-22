#!/bin/bash

cd ..
make
cd test

all_passed="true"

echo "********** START TESTS **********"

for f in `ls *.opy`
do
    python3 "$f" > "correct.txt" # use python3 to generate correct output
    ../main "$f" > "output.txt"
    if cmp -s "correct.txt" "output.txt"; then  # test output against python3's
        echo "Passed:" "$f"
    else
        echo "Failed:" "$f"
        echo "Aborting. Check output files."
        all_passed="false"
        break
    fi
    rm "correct.txt" "output.txt" # remove outputs each time if correct
done

if [ "$all_passed" = "true" ]; then
    echo "All tests passed."
fi

echo "*********** END TESTS ***********"
