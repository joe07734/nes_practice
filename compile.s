.segment "HEADER"
  .byte "NES"
  .byte $1A
  .byte $02
  .byte $02   ;amount of PRG ROM in 16k units
  .byte $01   ;amount of CHR ROM in 8k units
  .byte $00   ;which mapper and mirroring mode wee're using
  .byte $00, $00, $00, $00  ;not relevant till we get more advanced
  .byte $00, $00, $00, $00, $00 
.segment "ZEROPAGE"   ;because its values are quicker to access, the zero page usually stores variables used in the NES game
VAR:  .RES 1  ;reserves 1 byte of memory in 
.segment "STARTUP"

.segment "VECTORS"
  .word NMI   ;Non-Maskable Interrupt
  .word RESET ;When someone hits the reset button on the NES
  ;3rd word is for specialized hardware interrupts (like DMA?)
.segment "CHARS"
  .incbin "rom.chr" ; "include binary [file]"