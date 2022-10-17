#! /bin/bash -x
m68k-elf-as -m68010 --register-prefix-optional -o rom.o rom.68k
m68k-elf-ld -z max-page-size=1 -T rom.ld -o rom.elf rom.o
m68k-elf-objcopy -O binary rom.elf rom.bin

