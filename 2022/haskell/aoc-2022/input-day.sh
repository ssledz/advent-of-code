#!/bin/bash

set -eux

day=$(echo "0$1" | fold -w1 | tail -2 | tr -d '\n')

echo "day: $day"

curl "https://adventofcode.com/2022/day/$1/input" \
    -H 'cookie: _ga=GA1.2.936621570.1671826960; _gid=GA1.2.940276587.1671826960; session=53616c7465645f5f71e82dc50ec82d1c8b4bb112f1209b420865f546b08684635e779da883a6e9187d50e846041852fa547d10cc5469a6666dff7c7fa8e066fa' \
    > input/day$day.txt
