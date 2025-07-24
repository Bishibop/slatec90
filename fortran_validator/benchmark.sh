#!/bin/bash

echo "Generating 1000 PYTHAG tests..."
python generate_random_tests.py pythag > /tmp/pythag_1000.txt
python generate_random_tests.py pythag >> /tmp/pythag_1000.txt
# ... repeat to get 1000

echo "Benchmarking validator..."
time ./validator < /tmp/pythag_1000.txt > /tmp/results.txt

echo "Summary:"
tail -10 /tmp/results.txt