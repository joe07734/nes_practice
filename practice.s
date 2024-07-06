; define some NES things - can be moved into an include file later
; these take the place of using the hex addresses in code, making the code easier to read

; NES PPU register addresses - https://www.nesdev.org/wiki/PPU_registers
PPUCTRL		= $2000
PPUMASK		= $2001
PPUSTATUS	= $2002
OAMADDR		= $2003
OAMDATA		= $2004
PPUSCROLL	= $2005
PPUADDR		= $2006
PPUDATA		= $2007
OAMDMA		= $4014

; NES PPU addresses - https://www.nesdev.org/wiki/PPU_memory_map and https://www.nesdev.org/wiki/PPU_palettes
PATTERNTABLE0	= $0000
PATTERNTABLE1	= $1000
NAMETABLE0	= $2000
NAMETABLE1	= $2400
NAMETABLE2	= $2800
NAMETABLE3	= $2C00
PALETTES	= $3F00
BKGNDPALETTE	= PALETTES
SPRITEPALETTE	= $3F10

; PPUCTRL bits
VBLANKON	= $80
SPRITE8X16	= $20
BKGNDPATTERN1	= $10
INC32		= $04

; PPUMASK bits
SHOWSPRITES	= $10
SHOWBKGND	= $08
SHOWLEFT8	= $06

; NES sprite attribute bits - https://www.nesdev.org/wiki/PPU_OAM
VERTFLIP	= $80
HORIZFLIP	= $40
PALETTE0	= $00
PALETTE1	= $01
PALETTE2	= $02
PALETTE3	= $03

; NES APU register addresses - https://www.nesdev.org/wiki/APU
APUDMC0		= $4010
APUFRAME	= $4017

; NES controller addresses - https://www.nesdev.org/wiki/Controller_reading_code
JOYPAD1		= $4016
JOYPAD2		= $4017


; the program's variables
SPRITES		= $200			; copy of the sprites data, $200-$2FF - https://www.nesdev.org/wiki/PPU_OAM
SPRITEY		= SPRITES		; these are used to access the fields within a sprite, given a sprite offset (see usage)
SPRITETILE	= SPRITES+1
SPRITEATTR	= SPRITES+2
SPRITEX		= SPRITES+3

; sprite indexes
; when used to locate the address for a sprite in the SPRITES data, it's multiplied by 4
MUSHROOM	= 0			; index of mushroom
SWORD		= 1			; index of sword


; NES ROM header
		.segment "HEADER"
		.byte "NES"		; identification string necessary for every NES game
		.byte $1A
		.byte $02		; amount of PRG ROM in 16k units
		.byte $01		; amount of CHR ROM in 8k units
		.byte $00		; which mapper and mirroring mode we're using
		.byte $00,$00,$00,$00	; not relevant till we get more advanced
		.byte $00,$00,$00,$00
		.byte $00 


		.segment "ZEROPAGE"
MOVEX:  	.res 2			; array of sprite movement x directions (2)
MOVEY:  	.res 2			; array of sprite movement y directions (2)
DX:		.res 1
DY:		.res 1


		.segment "STARTUP"
; NES reset - https://www.nesdev.org/wiki/Init_code
RESET:		SEI
		CLD
		LDX #$FF
		TXS

; initialize APU
		LDA #$40
		STA APUFRAME		; disable frame IRQs
		LDA #0
		STA APUDMC0		; disable DMC IRQs

; initialize PPU
		LDA #0
		STA PPUCTRL		; disable NMIs
		STA PPUMASK		; disable rendering

; first wait to make sure PPU has stabilized
		BIT PPUSTATUS
	:	BIT PPUSTATUS
		BPL :-

; Clear the NES RAM (2k memory, $000-$7FF)
		LDA #0
		TAX
	:	STA $0, X
		STA $100, X
		STA $200, X
		STA $300, X
		STA $400, X
		STA $500, X
		STA $600, X
		STA $700, X
		INX
		BNE :-

; second wait to make sure PPU has stabilized
	:	BIT PPUSTATUS
		BPL :-

		BIT PPUSTATUS		; reset PPUADDR loading

; -------------------------
; HERE BEGINS OUR GAME INIT
; -------------------------

; initialize PPU background and sprite palettes
		LDA #>PALETTES
		STA PPUADDR
		LDA #<PALETTES
		STA PPUADDR
		LDX #0
	:	LDA INITIALPALETTES, X
		STA PPUDATA
		INX
		CPX #32
		BNE :-

		LDA #>PALETTES		; workaround for palette corruption bug - https://www.nesdev.org/wiki/PPU_registers#Palette_corruption
		STA PPUADDR
		LDA #<PALETTES
		STA PPUADDR
		STA PPUADDR
		STA PPUADDR

; load background into PPU
		LDA #>NAMETABLE0
		STA PPUADDR
		LDA #<NAMETABLE0
		STA PPUADDR

		LDX #0
	:	LDA BACKGROUND, X
		STA PPUDATA
		INX
		BNE :-

	:	LDA BACKGROUND+$100, X
		STA PPUDATA
		INX
		BNE :-

	:	LDA BACKGROUND+$200, X
		STA PPUDATA
		INX
		BNE :-

	:	LDA BACKGROUND+$300, X
		STA PPUDATA
		INX
		BNE :-

		LDA #VBLANKON|BKGNDPATTERN1
		STA PPUCTRL

		LDA #SHOWSPRITES|SHOWBKGND|SHOWLEFT8
		STA PPUMASK

; initialize the sprites (location, etc)
		LDX #0
	:	LDA INITIALSPRITES, X
		STA SPRITES, X
		INX
		CPX #8*4		; 8 sprites
		BNE :-

; set the remaining sprites off-screen
	:	LDA #$FF
		STA SPRITEX, X
		STA SPRITEY, X
		LDA #0
		STA SPRITETILE, X
		STA SPRITEATTR, X
		INX
		BNE :-

		LDA #1
		STA MOVEX + MUSHROOM
		STA MOVEY + MUSHROOM
		STA MOVEX + SWORD
		LDA #256-1
		STA MOVEY + SWORD

		CLI			; let NMI run
INFLOOP:	JMP INFLOOP		; all the action happens on the VBLANK


; NMI (non-maskable interrupt) routine, which on the NES is called at the start of every vertical blanking
NMI:		PHA			; save registers
		TXA
		PHA
		TYA
		PHA

		JSR DRAW

	     	PLA			; restore registers
	     	TAY
	     	PLA
	     	TAX
	     	PLA
		RTI

DRAW:		LDA #1			; read joypads
		STA JOYPAD1
		LDA #0
		STA JOYPAD1
		LDA JOYPAD1		; get first botton (A button), if pressed move the sprites
		AND #%00000001
		BEQ :+

		JSR MOVE		; move sprites

	:	LDA #$02		; load sprite data into OAM
		STA OAMDMA

	 	BIT PPUSTATUS		; reset loading
		LDA #0
		STA PPUSCROLL
		STA PPUSCROLL
		RTS


; subroutine that moves the sprites
MOVE:		LDA #MUSHROOM		; talking about the mushroom
		JSR BOUNCE
		LDA #SWORD		; now do the sword

; bounce an object around, reversing direction when it hits an edge
; the position of the upper left sprite is used for testing
; the direction values 1 or -1 are kept in the MOVEX and MOVEY arrays

BOUNCE:		TAY			; save index for MOVEX, MOVEY arrays
		ASL
		ASL
		ASL
		ASL
		TAX			; multiply by 16 for index into SPRITES

; test if the object's hit an edge
		LDA SPRITEX, X		; upper left sprite's X
		CMP #256-16		; at the right edge?
		BNE BOUNCE1

		LDA #256-1		; (-1) yes, now move left
		STA MOVEX, Y
		BNE BOUNCE2		; always taken

BOUNCE1:	CMP #0			; at the left edge?
		BNE BOUNCE2

		LDA #1			; yes, now move right
		STA MOVEX, Y

BOUNCE2:	LDA SPRITEY, X		; upper left sprite's Y
		CMP #240-16		; at the bottom edge?
		BNE BOUNCE3

		LDA #256-1		; (-1) yes, now move up
		STA MOVEY, Y
		BNE BOUNCE4		; always taken

BOUNCE3:	CMP #0			; at the top edge?
		BNE BOUNCE4

		LDA #1			; yes, now move down
		STA MOVEY, Y

; update the four sprites with the move deltas
BOUNCE4:	LDA MOVEX, Y		; put the directions into temp variables
		STA DX
		LDA MOVEY, Y
		STA DY
		LDY #4			; number of sprites to update
BOUNCE5:	LDA SPRITEX, X
		CLC
		ADC DX
		STA SPRITEX, X
		LDA SPRITEY, X
		CLC
		ADC DY
		STA SPRITEY, X
		INX
		INX
		INX
		INX
		DEY			; count down
		BNE BOUNCE5

		RTS


INITIALSPRITES:
		; mushroom
	 	.byte 50,  0, PALETTE0, 50	; y position, tile #, palette and transform, x position
		.byte 50,  1, PALETTE0, 58
		.byte 58, 16, PALETTE0, 50
		.byte 58, 17, PALETTE0, 58
		
		; sword
		.byte 100,  8, PALETTE1, 100
		.byte 100,  8, PALETTE1|HORIZFLIP, 108
		.byte 108, 24, PALETTE1, 100
		.byte 108, 24, PALETTE1|HORIZFLIP, 108

INITIALPALETTES:
	 	;background palettes - https://www.nesdev.org/wiki/PPU_palettes#Palettes
		.byte $00
		.byte $0A, $15, $31, $00	; background palette 0
		.byte $00, $00, $00, $00	; background palette 1 (unused)
		.byte $00, $00, $00, $00	; background palette 2 (unused)
		.byte $00, $00, $00		; background palette 3 (unused)
		; sprite palettes
		.byte $31
		.byte $0F, $15, $30, $00	; sprite palette 0
		.byte $0F, $11, $30, $00	; sprite palette 1
		.byte $00, $00, $00, $00	; sprite palette 2 (unused)
		.byte $00, $00, $00		; sprite palette 3 (unused)

BACKGROUND:
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

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
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

		; last 64 bytes are the image's attribute table - https://www.nesdev.org/wiki/PPU_attribute_tables
		.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

		.segment "CHARS"
		.incbin "rom.chr"

; 6502 vectors
		.segment "VECTORS"
		.word NMI
		.word RESET
