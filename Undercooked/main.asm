  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  .rsset $0000    ;put variables starting at 0
  ;.include "Level.asm"

;;;;;;;;;;;;;;;
  .rsset $0000  ;;start variables at ram location 0
buttons1   .rs 1  ; player 1 gamepad buttons, one bit per button
isCarryingTomato .rs 1 ; is player carrying tomato
isCarryingLettuce .rs 1 ; is Player Carrying lettuce

CONTROLLER_A      = %10000000
CONTROLLER_B      = %01000000
CONTROLLER_SELECT = %00100000
CONTROLLER_START  = %00010000
CONTROLLER_UP     = %00001000
CONTROLLER_DOWN   = %00000100
CONTROLLER_LEFT   = %00000010
CONTROLLER_RIGHT  = %00000001
    
  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0200, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0300, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2

  
  
  ;-----------------------------------------------------------------------------------------------------------------
  ;LOAD SPRITES/BACKGROUND

LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down

						
LoadPlayerSprite:
  LDX #$00              ; start at 0
LoadPlayerSpriteLoop:
  LDA playerSprite, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $20, decimal 32
  BNE LoadPlayerSpriteLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down

				
LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0
  
;Loop through all the background layers
LoadBackgroundLoop:
  LDA background, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$80              ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
						; if compare was equal to 128, keep going down
  
LoadBackgroundLoop2:
  LDA background2, x     ; load data from address (background2 + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$80              ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop2  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down
  					
LoadBackgroundLoop3:
  LDA background3, x     ; load data from address (background2 + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$80              ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop3  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
  
                        ; if compare was equal to 128, keep going down
LoadBackgroundLoop4:
  LDA background4, x     ; load data from address (background2 + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$80              ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop4  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down
              
LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
  
LoadAttributeLoop:
  LDA attribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$40              ; Compare X to hex $08, decimal 64 - copying 64 bytes
  BNE LoadAttributeLoop

  LDA #%10010000        ;enable NMI, sprites from Pattern 0, background from Pattern 1
  STA $2000

  LDA #%00011110      ; enable sprites, enable background
  STA $2001

  
  LDA #$00             ; Tell the PPU that we are not doing any scrolling at the end of NMI
  STA $2005


Forever:
  JMP Forever     ;jump back to Forever, infinite loop
  
 

NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer


  
  
  
  
  
  
  ;-----------------------------------------------------------------------------------------
  ;Collisions
  
UpdateTomato:
  ;LDA isCarryingTomato
  ;BEQ CollisionDone
  
  LDA $0203 ; player X
  SEC
  SBC $0217 ; tomato X
  CLC
  SBC #4
  BMI CollisionDone ; Branch if player - tomato + 4 < 0
  SEC
  ADC #8
  BPL CollisionDone ; branch if player - tomato - 4 > 0
  
; Check collision
  LDA $0200 ; player Y
  SEC
  SBC FoodSprites ; tomato y
  CLC
  ADC #4
  BMI CollisionDone ; Branch if player - tomato + 4 < 0
  SEC
  SBC #8
  BPL CollisionDone ; branch if player - tomato - 4 > 0
  

  
  ; Collision happened
  LDA #1
  STA isCarryingTomato ; playerCarryingtomato = 1
  
  
  ; Move tomato to inventory place
  LDA #$02
  STA $0213
  STA $0217
  
  
CollisionDone:


  
  ;---------------------------------------------------------------------------------------------------------------------
  ; Movement / INPUT

  
  ;Load four first key presses to get to arrow keys
  LDA $4016       ; player 1 - A
  LDA $4016       ; player 1 - B
  LDA $4016       ; player 1 - Select
  LDA $4016       ; player 1 - Start


  JSR ReadController1
  
  
ReadA:
  LDA buttons1
  AND #CONTROLLER_A
  BEQ .Done
  
  ;Only if player is carrying tomato
  LDA isCarryingTomato
  BEQ .Done
  
  
  ;Set the value of player pos into tomato pos
  ;X value
  LDA #$00
  LDA $0200
  STA $0214
  LDA $0203
  STA $0217
  
  ;LDX #$04 
  ;LDA FoodSprites
  ;STA $0214
  ;LDA FoodSprites, x
  ;STA $0217
  
  LDA #0
  STA isCarryingTomato
.Done:



  ;Move Up
ReadUp: 
  LDA buttons1       ; player 1 - B
  AND #CONTROLLER_UP  ; only look at bit 0
  BEQ .Done   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  
  LDY #0
.Loop:
  LDA $0200, y ; Load sprite Y position
  SEC             
  SBC #$01         ; Subtract 1
  STA $0200, y        ; Save
  
  INY
  INY
  INY
  INY
  CPY #$10
  BNE .Loop
.Done:


ReadDown: 
  LDA buttons1       ; player 1 - B
  AND #CONTROLLER_DOWN  ; only look at bit 0
  BEQ .Done   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDY #0
.Loop:
  LDA $0200, y    ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0200, y    ; save sprite X position
  INY
  INY
  INY
  INY
  CPY #$10
  BNE .Loop
.Done:        ; handling this button is done


  
ReadRight: 
  LDA buttons1       ; player 1 - B
  AND #CONTROLLER_RIGHT  ; only look at bit 0
  BEQ .Done   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDX #0
.Loop:
  LDA $0203, x    ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0203, x    ; save sprite X position
  INX
  INX
  INX
  INX
  CPX #$10
  BNE .Loop
.Done:        ; handling this button is done


  ; Move Left
ReadLeft: 
  LDA buttons1       ; player 1 - A
  AND #CONTROLLER_LEFT  ; only look at bit 0
  BEQ .Done   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDX #0
.Loop:
  LDA $0203, x    ; load sprite X position
  SEC             ; make sure the carry flag is clear
  SBC #$01        ; A = A + 1
  STA $0203, x    ; save sprite X position
  INX
  INX
  INX
  INX
  CPX #$10
  BNE .Loop       ; Stop looping after 4 sprites (X = 4*4 = 16)
.Done:        ; handling this button is done
; Movement Code End




  
  RTI             ; return from interrupt
  
  
ReadController1:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController1Loop:
  LDA $4016
  LSR A            ; bit0 -> Carry
  ROL buttons1     ; bit0 <- Carry
  DEX
  BNE ReadController1Loop
  
  RTS
;;;;;;;;;;;;;;  
  
  
  
  .bank 1
  .org $E000
palette:
  .db $0F,$22,$16,$27,$18,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F ; Background palette data
  .db $0F,$30,$26,$05,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C ; Sprite palette data
  
  ;04 = Floor
  ;25 = Wall  
  ;24 - 27 = Cooker

;LoadBackground sprites
background:
  .db $06,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 1
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 2
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 3
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 4
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 5
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky

  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 6
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 7
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 8
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
background2:
  .db $06,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 9
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 10
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 11
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 12
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 13
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky

  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 14
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 15
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 16
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
    background3:
  .db $06,$04,$04,$04,$05,$05,$04,$04,$05,$05,$05,$05,$05,$05,$04,$04  ;;row 17
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$05,$05,$04,$04,$05,$05,$05,$05,$05,$05,$04,$04  ;;row 18
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$05,$05,$04,$04,$05,$05,$05,$05,$05,$05,$04,$04  ;;row 19
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$24,$25,$04,$04  ;;some brick tops
  
  .db $04,$04,$04,$04,$05,$05,$04,$04,$05,$05,$05,$05,$05,$05,$04,$04  ;;row 20
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$26,$27,$04,$04  ;;brick bottoms
  
  .db $04,$04,$04,$04,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$04,$04  ;;row 21
  .db $04,$04,$04,$04,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$04,$04  ;;some brick tops
  
  .db $04,$04,$04,$04,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$04,$04  ;;row 22
  .db $04,$04,$04,$04,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$04,$04  ;;brick bottoms
  
  .db $04,$04,$04,$04,$05,$05,$04,$04,$05,$05,$05,$05,$05,$05,$04,$04  ;;row 23
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$53,$54,$04,$04  ;;some brick tops
  
  .db $04,$04,$04,$04,$05,$05,$04,$04,$05,$05,$05,$05,$05,$05,$04,$04  ;;row 04
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;brick bottoms
  
    background4:
  .db $06,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 25
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 26
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 27
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 28
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 29
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky

  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 30
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 31
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky
  
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;row 32
  .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04  ;;all sky

  ;Each nametable has an attribute table that sets which colors in the palette will be used in sections of the screen.
  attribute:
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101 ;row 0-3
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101 ;row 4-8
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101 ;row 9-13
  .db %01010101, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101, %01010101 ;row 14-18
  .db %01010101, %00000000, %00000000, %01010101, %01010101, %01010101, %01010101, %01010101 ;row 19-23
  
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101 
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
  

playerSprite:
     ;vert tile attr horiz
  .db $80, $00, $00, $80   ;sprite 0
  .db $80, $01, $00, $88   ;sprite 1
  .db $88, $10, $00, $80   ;sprite 2
  .db $88, $11, $00, $88   ;sprite 3
  
FoodSprites:
  .db $24, $02, $00, $88   ;Tomato

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "spriteSheet.chr"   ;includes 8KB graphics file from SMB1