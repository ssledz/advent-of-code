#!/bin/bash

set -eux

day=$(echo "0$1" | fold -w1 | tail -2 | tr -d '\n')

echo "day: $day"

curl "https://adventofcode.com/2023/day/$1/input" \
    -H "cookie: session=$SESSION" \
    > input/day$day.txt
