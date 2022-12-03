#!/bin/bash

# Execute:
# cat input.txt | ./part2.sh | paste -sd+ - | bc
priorities="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
while read line1 && read line2 && read line3; do
    dups=$( ((echo -n $line1 | sed -e 's/\(.\)/\1\n/g' | sort | uniq ) && (echo -n $line2 | sed -e 's/\(.\)/\1\n/g' | sort | uniq )) | sort | uniq -d )
    dup=$( ((echo -n $dups | sed -e 's/ /\n/g' | sort | uniq ) && (echo -n $line3 | sed -e 's/\(.\)/\1\n/g' | sort | uniq )) | sort | uniq -d )
    rest=${priorities#*$dup}
    echo $(( ${#priorities} - ${#rest} ))
done
