~/cc65/bin/ca65 -t nes ~/nes_practice/practice.s
~/cc65/bin/ld65 -t nes ~/nes_practice/practice.o -o ~/nes_practice/practice.nes
open ~/nes_practice/practice.nes -a openemu
