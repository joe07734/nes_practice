.segment "HEADER"
  .byte "NES" ;identification string necessary for every NES game
  .byte $1A
  .byte $02   ;amount of PRG ROM in 16k units
  .byte $01   ;amount of CHR ROM in 8k units
  .byte $00   ;which mapper and mirroring mode we're using
  .byte $00, $00, $00, $00  ;not relevant till we get more advanced
  .byte $00, $00, $00, $00, $00 
.segment "ZEROPAGE"   ;because its values are quicker to access, the zero page usually stores variables used in the NES game
VAR:  .RES 1  ;reserves 1 byte of memory in
CONTROLLER: .RES 1
.segment "STARTUP"

RESET:
  SEI   ;turns off interrupts
  CLD   ;turn off decimal mode (best practice)

  LDX #%1000000  ;could we also just put #$40 here? Or does it matter that this value is 7 bytes instead of 8?
  STX $4017   ;4017 is where one of the audio (APU) registers are
  LDX #$00
  STX $4010 ;disable PCM ($4010 is where the first APU register is)

  ;initialize the stack register
  LDX #$FF
  TXS   ;transfer value in X to the stack

  ;clear PPU registers (just the first 2 though it seems)
  LDX #$00
  STX $2000
  STX $2001

:
  ;wait for VBLANK
  BIT $2002 ;wait for the first bit at $2002 to be a 1 ($2002 is the PPU status register)
  BPL :-

  ;CLearing the NES CPU RAM (2k memory)
  TXA   ;X register is currently 0
CLEARMEMORY:
  STA $0000, X
  STA $0100, X
  STA $0300, X
  STA $0400, X
  STA $0500, X
  STA $0600, X
  STA $0700, X
    LDA #$FF    ;$0200-$02FF contains our sprite data so we want to fill with ones instead of zeros
    STA $0200, X
    LDA #$00
  INX
  CPX #$00
  BNE CLEARMEMORY

  :
  ;wait for VBLANK
  BIT $2002 ;wait for the first bit at $2002 to be a 1 ($2002 is the PPU status register)
  BPL :-

  ;setting sprite range (tbh I don't fully understand this, need to look more into the DMA register) --> JOE
  LDA #$02
  STA $4014 ;$4014 is where the most significant byte of the OAMDMA register is

  NOP ;(I think) we call a noop here because it takes 512 cycles to populate the OAM via DMA, during which the CPU is suspended

  ;setting the palette data (palette memory starts at $3F00)
    ;2006 is the PPU ADDR register, which tells the PPU which address in its(?) memory we want to modify
    ;2007 is the PPU DATA register, which tells the PPU what we want to insert into the address contained in its ADDR register
    ;by using the PPU's ADDR and DATA registers, we can communicate instructions to the PPU from the CPU (the CPU cannot directly modyify the PPU's data)
  LDA #$3F  ;most significant byte of address we care about
  STA $2006
  LDA #$00  ;least significant byte
  STA $2006 ;don't understand why this doesn't overwrite the #$3F --> JOE

  LDX #$00
LOADPALETTES:
  LDA PALETTEDATA, X
  STA $2007 ;whenever we read or write to $2007, the PPU ADDR reigtser ($2006) increments itself, so we don't have to do it manually
  INX
  CPX #$20
  BNE LOADPALETTES

  LDX #$00
LOADSPRITES:
  LDA SPRITEDATA, X
  STA $0200, X
  INX
  CPX #$20 ;4 bytes per sprite, 8 sprites
  BNE LOADSPRITES

LOADBACKGROUND:
  LDA $2002 ;read status register to reset high/low latch (don't really understand this)
  LDA #$21  ;high byte
  STA $2006 ;PPU ADDR register
  LDA #$00  ;low byte
  STA $2006
  LDX #$00
LOADBACKGROUNDP1:
  LDA BACKGROUNDDATA, X
  STA $2007
  INX
  CPX #$00
  BNE LOADBACKGROUNDP1
LOADBACKGROUNDP2:
  LDA BACKGROUNDDATA+256, X
  STA $2007
  INX
  CPX #$00
  BNE LOADBACKGROUNDP2

;load background palettes
  LDA #$23
  STA $2006
  LDA #$D0
  STA $2006
  LDX #$00
LOADBACKGROUNDPALETTEDATA:
  LDA BACKGROUNDPALETTEDATA, X
  STA $2007
  INX
  CPX #$20
  BNE LOADBACKGROUNDPALETTEDATA

  ;reset scroll
  LDA #$00
  STA $2005 ;$2005 is the PPU SCROLL register
  STA $2005
  
  CLI ;enable the interrupts we disabled at the beginning

  ;when VBLANK occurs, call NMI subroutine
  LDA #%10010000 ;AKA #$90 ;the second 1 here tells the PPU to use the 2nd section of 256 sprites on the background
  STA $2000 ;2000 is the PPU CTRL register

  ;show sprites and background
  LDA #%00011110 ; AKA 1E
  STA $2001 ;$2001 is the PPU MASK register

  INFLOOP:
    JMP INFLOOP ;AKA infinite loop (forgot why we do this)

NMI:

  LDX #$00 ;would like to move this into the MOVESPRITES subroutine

  LDA #$01  ;latch controller
  STA $4016
  LDA $00
  STA $4016

  LDA $4016   ;if controller input, move the sprites
  AND #%00000001
  BNE MOVESPRITES

  DRAWSPRITES:
      ;load sprite range. We did this before in the RESET segment, but we have to continually populate this region with sprite data
  ;or else the sprite data will decay over time (why?)
    LDA #$02
    STA $4014
    
  RTI   ;return to interupt

  MOVESPRITES:  ;simple function that moves each sprite every frame
    DEC $0200, X ;decrement y-coord
    INX ;go to the next sprite (each sprite is 4 bytes)
    INX
    INX
    INC $0200, X ;increment x coord
    INX     
    CPX #$20 ;4 bytes per sprite, 8 sprites
    BNE MOVESPRITES
    JMP DRAWSPRITES


PALETTEDATA:
	.byte $00, $0F, $00, $10, 	$00, $0A, $15, $26, 	$00, $29, $28, $27, 	$00, $34, $24, $14 	;background palettes
	.byte $31, $0F, $15, $30, 	$00, $0F, $11, $30, 	$00, $0F, $30, $27, 	$00, $3C, $2C, $1C 	;sprite palettes

SPRITEDATA:
;each sprite is defined by 4 bytes, representing:
  ;Y-coord, SPRITE NUM (AKA whichof the 256 sprites in CHR ROM are we working with?), attributes, X-coord


;attribute byte description
  ;76543210
  ;||||||||
  ;||||||++- Palette (4 to 7) of sprite
  ;|||+++--- Unimplemented
  ;||+------ Priority (0: in front of background; 1: behind background)
  ;|+------- Flip sprite horizontally
  ;+-------- Flip sprite vertically

	;mushroom
  .byte $40, $00, $00, $40
	.byte $40, $01, $00, $48
	.byte $48, $10, $00, $40
	.byte $48, $11, $00, $48
	
	;sword
	.byte $50, $08, %00000001, $80
	.byte $50, $08, %01000001, $88
	.byte $58, $18, %00000001, $80
	.byte $58, $18, %01000001, $88

BACKGROUNDDATA:	;512 BYTES (prolly not normal to hard code it like this)
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$03,$04,$05,$00,$00,$00,$00,$00,$00,$00,$06,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$08,$09,$0a,$0b,$0b,$0b,$0c,$0d,$0e,$0f,$10,$11,$56,$13,$14,$0b,$15,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$16,$17,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$18,$19,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$1a,$1b,$1c,$1d,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$1e,$06,$1f,$00,$00,$00,$00,$00
	.byte $00,$00,$20,$21,$22,$23,$18,$24,$25,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$26,$27,$28,$00,$29,$2a,$00,$00,$00,$00,$00
	.byte $00,$00,$2b,$2c,$2d,$0b,$11,$2e,$2f,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$30,$31,$32,$33,$34,$35,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$36,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$18,$37,$38,$39,$3a,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$3b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$3c,$3d,$3e,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$3f,$40,$0b,$0b,$0b,$41,$42,$43,$44,$0b,$0b,$45,$0b,$0b,$0b,$0b,$46,$47,$48,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$49,$0b,$0b,$4a,$4b,$00,$4c,$4d,$0b,$4e,$4f,$50,$0b,$0b,$51,$00,$52,$53,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$3f,$54,$55,$12,$00,$00,$00,$57,$58,$59,$00,$5a,$5b,$5c,$5d,$00,$5e,$5f,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$60,$61,$00,$00,$62,$63,$64,$65,$00,$66,$67,$68,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$69,$00,$00,$6a,$6b,$6c,$00,$6d,$6e,$6f,$70,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$71,$72,$73,$0b,$74,$75,$76,$77,$78,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$4c,$83,$84,$85,$86,$35,$87,$00,$00,$00,$00,$00,$00

BACKGROUNDPALETTEDATA:	;32 bytes, all set to the value of the first palette
  ;this part was the most confusing for me
	.byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55
	.byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55

.segment "VECTORS"
  .word NMI   ;Non-Maskable Interrupt
  .word RESET ;When someone hits the reset button on the NES
  ;3rd word is for specialized hardware interrupts IRQ
.segment "CHARS"
  .incbin "rom.chr" ; "include binary [file]"