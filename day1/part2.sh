cat input1.txt| tr '\n' '+' | sed 's/++/\n/g' | sed 's/+$/\n/g' | bc| sort -n -r | head -n 3 | tr '\n' '+' | sed 's/+$/\n/g'  | bc
