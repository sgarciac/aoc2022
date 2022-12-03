#!/bin/bash

# Execute:
# cat input.txt | ./part1.sh | paste -sd+ - | bc
priorities="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
while read line; do
    dup=$( ((echo -n ${line:0:${#line}/2} | sed -e 's/\(.\)/\1\n/g' | sort | uniq ) && (echo -n ${line:${#line}/2} | sed -e 's/\(.\)/\1\n/g' | sort | uniq )) | sort | uniq -d )
    rest=${priorities#*$dup}
    echo $(( ${#priorities} - ${#rest} ))
done
