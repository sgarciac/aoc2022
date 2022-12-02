cat input.txt | sed -e 's/B X/1/g' -e 's/C Y/2/g' -e 's/A Z/3/g' -e 's/A X/4/g' -e 's/B Y/5/g' -e 's/C Z/6/g' -e 's/C X/7/g' -e 's/A Y/8/g' -e 's/B Z/9/g' | paste -sd+ - | bc
