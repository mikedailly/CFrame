
    SECTION KERNEL_CODE
    PUBLIC  _WaitVBlank, _Layer2Enable, _ClsATTR, _ClsULA, _PrintHex, _DMACopy, _UploadSprites, _ReadKeyboard, _InitKernel
    PUBLIC  _Keys, _RawKeys, _RomFont, _Print,  _CopySpriteData, _ReadNextReg,_InitSpriteData,_WipeSprites, _Border, _UploadCopper

    EXTERN  _VBlank, _Port123b, _SpriteData, _SpriteShape, _PrintOffset

    include "includes.inc"


; ******************************************************************************************************************************
;   Init the Kernel
; ******************************************************************************************************************************
_InitKernel:
    ld      hl,0x3d00
    ld      de,_RomFont
    ld      bc,0x300
    call    DMACopy
    
    ret

    ; ******************************************************************************************************************************
;   Wipe all sprites
; ******************************************************************************************************************************
_WipeSprites:
    ld      ix, _SpriteData
    ld      b,128
    ld      c,64
    ld      de,5
    xor     a   
@InitSprites: 
    ld      (ix+0),a
    ld      (ix+1),a
    ld      (ix+2),a
    ld      (ix+3),c        ; %01000000 - enable byte 4
    ld      (ix+0),a
    add     ix,de
    djnz    @InitSprites
    ret

; ******************************************************************************************************************************
;   Reset all sprite data, including the extra bit for using 5th byte
; ******************************************************************************************************************************
_InitSpriteData:
    ld      ix, _SpriteData
    ld      b,128
    ld      c,64
    ld      de,5
    xor     a   
@InitSprites: 
    ld      (ix+0),a
    ld      (ix+1),a
    ld      (ix+2),a
    ld      (ix+3),c        ; %01000000 - enable byte 4
    ld      (ix+0),a
    add     ix,de
    djnz    @InitSprites
    ret


; ******************************************************************************************************************************
;   Wait for a Vertical Blank (uses VBlank IRQ)
; ******************************************************************************************************************************
_WaitVBlank:
    xor a
    ld  (_VBlank),a

WaitForVBlank:
    ld  a,(_VBlank)
    and a
    jr  z,WaitForVBlank
    ret



; ************************************************************************
;   Enable the 256 colour Layer 2 bitmap
;
;   In:     L=0  off        (fastcall passes bool as a byte in L)
;           L!=0 on
; ************************************************************************
_Layer2Enable:
    ld  a,l
    and a
    jr  z,@Layer2Off
    ld  l,2

@Layer2Off:
    ld  a,(_Port123b)
    or  l
    ld  (_Port123b),a
    ld  bc, $123b
    out (c),a     
    ret                          



; ************************************************************************
;
;   Function:   Clear the spectrum attribute screen
;   In:     L = colour
;
;   Format: F_B_PPP_III
;
;           F = Flash
;           B = Bright
;           P = Paper
;           I = Ink
;
; ************************************************************************
_Border:
    ld      a,l
    out     ($fe),a
    ret

; ************************************************************************
;
;   Function:   Clear the spectrum attribute screen
;   In:     L = attribute
;
;   Format: F_B_PPP_III
;
;           F = Flash
;           B = Bright
;           P = Paper
;           I = Ink
;
; ************************************************************************
_ClsATTR:
    ld      a,l
    ld      hl,AttrScreen
    ld      (hl),a
    ld      de,AttrScreen+1
    ld      bc,1000
    ldir
    ret


; ************************************************************************
;
;   Function:   clear the normal spectrum screen
;
; ************************************************************************
_ClsULA:
    xor a
    ld      hl,ULAScreen
    ld      (hl),a
    ld      de,ULAScreen+1
    ld      bc,6143
    ldir
    ret


; ******************************************************************************
; Function: DMACopper
; In:       hl = Copper offset into bank
;       bc = size
; Uses:     A,HL,BC
; ******************************************************************************
_UploadCopper:
        pop     hl          ; get return address
        pop     bc          ; get src
        ld      (DMASrcAdd),bc
        pop     bc          ; get size
        ld      (DMASrcLen),bc
        push    hl          ; push return address back

        xor a               ; 
        NextReg $61,a       ; reset copper address
        NextReg $62,a       ; Stop copper - copper address upper bits

        ; now select the copper "data" port
        ld  bc,$243b
        ld  a,$60
        out (c),a

        ld  hl,DMACopyToReg     ; 10
        ld  bc,DMACOPPERSIZE*256 + Z80DMAPORT ; 10
        otir                    ; 21*20  + 240*4

        ret

; ******************************************************************************
; Function: DMACopy
; In:       hl = Src
;           de = Dest
;           bc = size
; ******************************************************************************
_DMACopy:
    pop     hl          ; get return address
    pop     bc          ; get src
    ld      (DMASrc),bc
    pop     bc          ; get dest
    ld      (DMADest),bc
    pop     bc          ; get size
    ld      (DMALen),bc
    push    hl          ; push return address back

DoDMACopy:
    ld  hl,DMACopyProg                  
    ld  bc,DMASIZE*256 + Z80DMAPORT 
    otir
    ret


; ******************************************************************************
; Function: DMACopy
; In:       hl = Src
;           de = Dest
;           bc = size
; ******************************************************************************
DMACopy:
    ld      (DMASrc),hl
    ld      (DMADest),de
    ld      (DMALen),bc
    jp      DoDMACopy


; ******************************************************************************
; 
; Function: Upload a set of sprites
; In:   E = sprite shape to start at
;       D = number of sprites
;       HL = shape data
;
; ******************************************************************************
_UploadSprites:
    pop     bc          ; pop return address
    pop     de          ; get Start Shape
    pop     hl          ; get number of shapes
    ld      d,l
    pop     hl          ; get shape address
    push    bc          ; restore reeturn address

    ; Upload sprite graphics
    ld      a,e     ; get start shape
    ld      e,0     ; each pattern is 256 bytes

@AllSprites:               
    ; select pattern 2
    ld      bc, $303B
    out     (c),a

    ; upload ALL sprite sprite image data
    ld      bc, SpriteShapePort
@UpLoadSprite:           
    outi

    dec     de
    ld      a,d
    or      e               
    jr      nz, @UpLoadSprite
    ret

; ******************************************************************************
;
;   Print HEX to the screen
;   L  = hex value tp print
;   DE= address to print to (normal specturm screen)
;
; ******************************************************************************
_PrintHex:   
        pop     hl          ; get return address
        dec     sp          ; realign AF - Thanks z88dk! 
        pop     af          ; get AF
        pop     de          ; get dest address
        push    hl          ; push return address back

        push    af
        swapnib
        call    DrawHexCharacter
        inc     e           ; next character on screen

        pop af
        and $0f  

        ; fall through (call)

;
; A = NIBBLE hex value to print (0 to 15 only)
; DE= address to print to (normal specturm screen)
; uses: a,hl,de
;
DrawHexCharacter:   
        and $0f
        ld  hl,HexCharset
        add a,a
        add a,a             ; *8
        add a,a
        add hl,a

        ; data is aligned to 256 bytes
        ldws
        ldws
        ldws
        ldws
        ldws
        ldws
        ldws
        ldws
        
        ld  a,d
        sub 8           ; move back to the top of the screen character
        ld  d,a
        ret

TextSample:
        db  "Hello World 12\nEat Pooh\nTesting\n",0

; ******************************************************************************
;
;   Print(X,Y,"Text")
;
; ******************************************************************************
_Print:
    pop     bc              ; return address
    pop     de              ; get YX (D=Y,E=X)
    ld      (PrintCoords),de
    pixelad
    ld      a,(_PrintOffset)
    add     a,h             ; move to the location of our screen
    ld      h,a
    pop     de              ; get text
    push    bc

@DrawAll:
    ld      a,(de)
    cp      10
    jr      z,@NewLine
    cp      13
    jr      z,@NextChar
    and     a
    ret     z               ; ,0 terminated

    sub     32              ; space?
    jr      c,@NonAsci     ; not an asci char?
    and     a
    jr      nz,@Skip
    inc     hl
    jr      @SpaceChar

@NonAsci:
@Skip:
    call    DrawCharacter
@SpaceChar:
    ld      a,(PrintCoords)     ; get X coord
    add     8
    ld      (PrintCoords),a
    jr      nc,@NextChar
@NewLine:
    xor     a
    ld      (PrintCoords),a

    ld      a,(PrintCoords+1)   ; get Y
    add     a,8
    ld      (PrintCoords+1),a   ; get Y
    push    de                  ; save text pointer
    ld      de,(PrintCoords)    ; read coords
    pixelad                     ; and recalc HL
    pop     de                  ; get current text pointer back

@NextChar:
    inc     de
    jp      @DrawAll

;
; A = NIBBLE hex value to print (0 to 15 only)
; HL= address to print to (normal specturm screen)
; uses: a,hl,de
;
DrawCharacter:   
    push    de
    push    hl
    ld      d,0                 ; char * 8
    sla     a
    rl      d
    sla     a
    rl      d
    sla     a
    rl      d
    ld      e,a
    add     de,_RomFont         ; add on base

    ld      b,8
@WholeChar:
    ld      a,(de)
    or      (hl)
    ld      (hl),a
    pixeldn
    inc     de
    djnz    @WholeChar

    pop     hl
    pop     de
    inc     hl
    ret


; ******************************************************************************
; Function: Scan the whole keyboard
; ******************************************************************************
_ReadKeyboard:
        ld  b,39
        ld  hl,_Keys
        xor a
@lp1:   ld  (hl),a
        inc hl
        djnz    @lp1

        ld  iy,_Keys
        ld  bc,$fefe            ; Caps,Z,X,C,V
        ld  hl,_RawKeys
@ReadAllKeys:   
        in  a,(c)
        ld  (hl),a
        inc hl      
        
        ld  d,5
        ld  e,$ff
@DoAll: 
        srl a
        jr  c,@notset
        ld  (iy+0),e
@notset:
        inc iy
        dec d
        jr  nz,@DoAll

        ld  a,b
        sla a
        jr  nc,ExitKeyRead
        or  1
        ld  b,a
        jp  @ReadAllKeys
ExitKeyRead:
        ret

; ******************************************************************************
; Function: Copy sprite data (x,y etc) to BRAM (assumes extended data)
; In:   hl = Src
;       d = slot
;       a = count
;       
;
;   |*|*||0011 0000 0011 1011| 0x303b  |Sprite slot, flags
;   | |*||XXXX XXXX 0101 0111| 0x57    |Sprite attributes
;   | |*||XXXX XXXX 0101 1011| 0x5b    |Sprite pattern

; ******************************************************************************
_CopySpriteData:
        ld      hl,_SpriteData
        ld      (DMASpSrc),hl                       ; 16
        ld      bc,$303b
        out     (c),d
        ld      hl,640                              ; 128 * 5
        ld      (DMASpLen),hl                       ; store size
        ld      hl,DMASpriteCopyProg                ; 10
        ld      bc,Z80DMAPORT+(DMASPCOPYSIZE*256)   ; 10
        otir                                        ; 21*20  + 240*4
        ret

; ******************************************************************************
; Function: Read a next register
;           uint16 v = ReadNextReg(uint16 reg)
; ******************************************************************************
_ReadNextReg:
        pop     de          ; get return address
        pop     hl

        ; read MSB of raster first
        ld      bc,$243b    ; select NEXT register
        out     (c),l
        inc     b           ; $253b to access (read or write) value
        in      l,(c)
        ld      h,0
        push    de          ; push return address back
        ret                 ; return in HL

; ******************************************************************************
; Function: Read a next register
; In:       A = reg
; Out:      A = value
; ******************************************************************************
ReadNextRegsSYS:

        ; read MSB of raster first
        ld      bc,$243b    ; select NEXT register
        out     (c),a
        inc     b           ; $253b to access (read or write) value
        in      a,(c)
        ret




; *******************************************************************************************************
; *******************************************************************************************************
;                               File System
; *******************************************************************************************************
; *******************************************************************************************************


; *******************************************************************************************************
;
;   Get/Set the drive (get default drive)
;
; *******************************************************************************************************
GetSetDrive:    
        push    af          ; no idea what it uses....
        push    bc
        push    de
        push    hl
        push    ix

        xor     a           ; set drive. 0 is default
        rst     $08
        db      M_GETSETDRV
        ld      (DefaultDrive),a

        pop     ix
        pop     hl
        pop     de
        pop     bc
        pop     af
        ret


; *******************************************************************************************************
;   Function:   Create a new file
;   In:     ix = filename pointer
;   ret     a  = handle, 0 on error
; *******************************************************************************************************
fcreate:
        ld      b,$0c
        push    ix
        pop     hl
        ld      a,42
        rst     $08
        db      $9a
        ld      (handle),a
        ret

; *******************************************************************************************************
;   Function:   Open a file read for reading/writing
;   In:     ix = filename
;           b  = Open filemode
;   ret     a  = handle, 0 on error
; *******************************************************************************************************
fOpen:      
        push    hl
        push    ix
        pop     hl
        ld      a,(DefaultDrive)
        rst     $08
        db      F_OPEN
        pop     hl
        ret


; *******************************************************************************************************
;   Function    Read bytes from the open file
;   In:     ix  = address to read into
;           bc  = amount to read
;   ret:        carry set = error
; *******************************************************************************************************
fread:
        or      a             ; is it zero?
        ret     z             ; if so return        

        push    hl

        push    ix
        pop     hl
        rst     $08
        db      F_READ

        pop     hl
        ret

; *******************************************************************************************************
;   Function    Read bytes from the open file
;   In:         ix  = address to read into
;               bc  = amount to read
;   ret:        carry set = error
; *******************************************************************************************************
fwrite:
        or      a             ; is it zero?
        ret     z             ; if so return        

        push    hl

        push    ix
        pop     hl
        rst     $08
        db      F_WRITE

        pop     hl
        ret

; *******************************************************************************************************
;   Function:   Close open file
;   In:     a  = handle
;   ret     a  = handle, 0 on error
; *******************************************************************************************************
fClose:     
        or      a             ; is it zero?
        ret     z             ; if so return        
        rst     $08
        db      F_CLOSE
        ret



; *******************************************************************************************************
;   Function    Read bytes from the open file
;   In:         a   = file handle
;               L   = Seek mode (0=start, 1=rel, 2=-rel)
;               BCDE = bytes to seek
;   ret:        BCDE = file pos from start
; *******************************************************************************************************
fseek:
        push    ix
        push    hl
        rst     $08
        db      F_SEEK
        pop     hl
        pop     ix
        ret


; *******************************************************************************************************
;   Function:   Get file stats
;   In:         a  = handle
;   ret         a  = error code
;               fc = 1 error
;               fc = 0 no error
;
; The following details are returned in the 11-byte buffer:
;   +0(1)  '*'
;   +1(1)  $81
;   +2(1)  file attributes (MS-DOS format)
;   +3(2)  timestamp (MS-DOS format)
;   +5(2)  datestamp (MS-DOS format)
;   +7(4)  file size in bytes
;
; *******************************************************************************************************
fStat:     
        or      a             ; is it zero?
        ret     z             ; if so return     
        push    ix
        ld      ix,FileStatsBuffer 
        rst     $08
        db      F_STAT
        pop     ix
        ret


; *******************************************************************************************************
; Function: Load a whole file into memory   (confirmed working on real machine)
; In:       Load(filename, bank, offset);
; Out:      none
; *******************************************************************************************************
SaveBanks:
        ld      a,LOADING_BANK
        call    ReadNextRegsSYS
        ld      (LoadBankWorkspace),a

SaveBanksFast:
        ld      a,$50
        call    ReadNextRegsSYS
        ld      (Bank50),a
        ld      a,$51
        call    ReadNextRegsSYS
        ld      (Bank51),a
        ret


; *******************************************************************************************************
; Function: Load a whole file into memory   (confirmed working on real machine)
; In:       Load(filename, bank, offset);
; Out:      none
; *******************************************************************************************************
_Load:  
        pop     bc                      ; return address
        ld      (Load_Offset),bc        ; stash here for now

        ; Copy filename, as it may be under the ROM and about to be banked out
        pop     hl                      ; get Filename
        ld      de,Load_Filename
        ld      b,MAX_FILENAME_LEN-1    ;
@CountDone:
        ld      a,(hl)
        and     a
        jr      z,@EndOfString
        ld      (de),a
        inc     hl
        inc     de
        djnz    @CountDone
@EndOfString:
        xor     a                       ; ,0 terminate
        ld      (de),a
        ld      bc,(Load_Offset)        ; get return address back

        pop     hl                      ; get bank (16bit for ease of access)
        ld      a,l
        ld      (Load_Bank),a           ; store just the single byte
        pop     hl
        ld      (Load_Offset),hl        ; store offset

        push    bc                      ; push return back
        push    ix                      ; remember IX as C uses it


        ; Get bank 0 and 1
        call    SaveBanks
        NextReg $50,255                 ; make sure ROM is paged in
        NextReg $51,255

        call    GetSetDrive             ; need to do this each time?!?!?


        ld      ix,Load_Filename
        ld      b,FA_READ               ; mode OPEN for reading
        call    fOpen
        jp      c,@error_opening        ; carry set? so there was an error opening and A=error code
        and     a                       ; was file handle 0?
        jp      z,@error_opening        ; of so there was an error opening.

        ld      (handle),a              ; remember handle
        call    fStat                   ; get file STATS into the fStat buffer - date/time/size etc

        ; -------------------------
        ; Loading Block loop
        ; -------------------------
@LoadingLoop:
        ld      hl,$2000                ; max size to load in a blob
        ld      bc,(Load_Offset)        ; get the offset
        and     a                       ; clear carry
        sbc     hl,bc                   ; Work out the number of bytes left in the bank
        ld      (BytesLeftInBank),hl    ; remember this (just use stats buffer as temp storage)
        

        ld      a,(BytesLeftToLoad+2)   ; get 24bit high - if set, then fill the bank as we're >64k        
        and     a
        jr      nz,@FillBank
        ld      bc,(BytesLeftToLoad)    ; get file size left
        and     a
        sbc     hl,bc                   ; sub total size, if "negative" then we can fill the remainder of the bank
        jr      c,@FillBank             ; if bytes left, then file size (left) won't fill the bank - so use that
        ld      bc,(BytesLeftToLoad)    ; Get total bytes left in file
        jr      @SkipFillBank
@FillBank:
        ld      bc,(BytesLeftInBank)    ; get bytes left in buffer
@SkipFillBank:
        ld      (BytesToRequest),bc     ; remember the number of bytes we're going to load



        ; -------------------------
        ; Now load the next block
        ; -------------------------
        ld      a,(Load_Bank)
        NextReg LOADING_BANK,a          ; bank in where we want to load to - this has been remembered
        ld      ix,(Load_Offset)
        ld      de,LOADING_BASE_ADD
        add     ix,de
        ld      a,(handle)

        call    fread                   ; read data from A to address IX of length BC                
        jr      c,@error_reading
        ld      (FileStatsBuffer+2),bc  ; number of bytes actually read....



        ; -------------------------
        ; Work out if we've more to load, and if so work out next bank/offset/ bytes left
        ; -------------------------

        ; subtract off the bytes loaded to how much is left (full 32bit subtract) - if zero, then we're done.
        ld      hl,(BytesLeftToLoad)
        and     a                               ; reset carry
        sbc     hl,bc                           ; HL-(BC+C)
        push    bc
        ld      (BytesLeftToLoad),hl
        ld      hl,(BytesLeftToLoad+2)          ; HL-(0+C)
        ld      bc,0
        sbc     hl,bc
        ld      (BytesLeftToLoad+2),hl        
        pop     bc

        ; are we at 0?
        ld      a,h
        or      l
        ld      hl,(BytesLeftToLoad)
        or      h
        or      l
        jr      z,@LoadedAll


        ; More to load, so move on load address
        ld      hl,(Load_Offset)
        add     hl,bc
        ld      a,h
        and     $e0                     ; have we overflowed the bank?
        jr      z,@NoOverlow
        ld      a,(Load_Bank)            ; overflow - so increment bank
        inc     a
        ld      (Load_Bank),a

        ld      a,h                     ; wrap bank - hl should technically always be $0000 at this point
        and     $1f
        ld      h,a

@NoOverlow:
        ld      (Load_Offset),hl        ; store new bank offset
        jr      @LoadingLoop


@LoadedAll:
        ld      a,(handle)
        call    fClose                  ; close file
        jr      c,@error_closing

        jp      @skiperrors
        ;pop     ix                     ; fall through instead (change if we get error codes etc)
        ;jp      RestoreBanks

;
; On error, display error code an lock up so we can see it
;
@error_opening:
@error_reading:     
@error_closing:
@NormalError:   
        pop     ix
        ld      a,255
        jp      @skip
@skiperrors:
        pop     ix
        xor     a
@skip:
        push    af
RestoreBanks:
        ; reset all the used banks
        ld      a,(LoadBankWorkspace)
        NextReg LOADING_BANK,a
RestoreBanksFast:
        ld      a,(Bank50)
        NextReg $50,a
        ld      a,(Bank51)
        NextReg $51,a
        pop     af
        ld      l,a
        ret




; ******************************************************************************************************************************
; ******************************************************************************************************************************
; ******************************************************************************************************************************
;       Kernel Data
; ******************************************************************************************************************************
; ******************************************************************************************************************************
; ******************************************************************************************************************************
DefaultDrive:       db  0   ; Current Drive
handle:             db  0   ; current open file handle
Load_Bank:          db  0   ; Loading bank
Load_Offset:        dw  0   ; Loading bank
Load_Filename:      ds  MAX_FILENAME_LEN  ; space to copy the filename - incase it's now under the ROM. "data/test.txt",0
Bank50:             db  0   ; Remmeber Bank 50
Bank51:             db  0   ; Remmeber Bank 51
LoadBankWorkspace:  db  0   ; Rememebr Bank 56
_PrintOffset:       db  0   ; offset from $4000


;   +0(1) '*'
;   +1(1) $81
;   +2(1) file attributes (MS-DOS format)
;   +3(2) timestamp (MS-DOS format)
;   +5(2) datestamp (MS-DOS format)
;   +7(4) file size in bytes
TempBuffer:         ds  64  
                    defc FileStatsBuffer = TempBuffer;              ; 11 byte buffer
                    defc BytesLeftToLoad = FileStatsBuffer+7
                    defc BytesLeftInBank = FileStatsBuffer
                    defc BytesToRequest = FileStatsBuffer+2
                    defc PrintCoords = FileStatsBuffer





DMASpriteCopyProg:
        db $C3          ; R6-RESET DMA
        db $C7          ; R6-RESET PORT A Timing
        db $CB          ; R6-SET PORT B Timing same as PORT A

        db $7D          ; R0-Transfer mode, A -> B
DMASpSrc:
        dw $1234        ; R0-Port A, Start address               (source address)
DMASpLen:    
        dw 240          ; R0-Block length                        (length in bytes)

        db $54          ; R1-Port A address incrementing, variable timing
        db $02          ; R1-Cycle length port A
      
        db $78          ; R2-Port B address fixed, variable timing Write to a "PORT"
        db $02          ; R2-Cycle length (2) port B
      
        db $AD          ; R4-Continuous mode  (use this for block tansfer)
DMASpDest:
        dw $0057        ; R4-Dest address (Sprite DATA)          (destination port)
          
        db $82          ; R5-Restart on end of block, RDY active LOW
     
        db $CF          ; R6-Load
        db $B3          ; R6-Force Ready
        db $87          ; R6-Enable DMA
ENDSPDMA:
        defc    DMASPCOPYSIZE   = ENDSPDMA-DMASpriteCopyProg





HexCharset:
        db %00000000    ;char30  '0'
        db %00111100
        db %01000110
        db %01001010
        db %01010010
        db %01100010
        db %00111100
        db %00000000
        db %00000000    ;char31 '1'
        db %00011000
        db %00101000
        db %00001000
        db %00001000
        db %00001000
        db %00111110
        db %00000000
        db %00000000    ;char32 '2'
        db %00111100
        db %01000010
        db %00000010
        db %00111100
        db %01000000
        db %01111110
        db %00000000
        db %00000000    ;char33 '3'
        db %00111100
        db %01000010
        db %00001100
        db %00000010
        db %01000010
        db %00111100
        db %00000000
        db %00000000    ;char34 '4'
        db %00001000
        db %00011000
        db %00101000
        db %01001000
        db %01111110
        db %00001000
        db %00000000
        db %00000000    ;char35 '5'
        db %01111110
        db %01000000
        db %01111100
        db %00000010
        db %01000010
        db %00111100
        db %00000000
        db %00000000    ;char36 '6'
        db %00111100
        db %01000000
        db %01111100
        db %01000010
        db %01000010
        db %00111100
        db %00000000
        db %00000000    ;char37 '7'
        db %01111110
        db %00000010
        db %00000100
        db %00001000
        db %00010000
        db %00010000
        db %00000000
        db %00000000    ;char38 '8'
        db %00111100
        db %01000010
        db %00111100
        db %01000010
        db %01000010
        db %00111100
        db %00000000
        db %00000000    ;char39 '9'
        db %00111100
        db %01000010
        db %01000010
        db %00111110
        db %00000010
        db %00111100
        db %00000000
        db %00000000    ;char41 'A'
        db %00111100
        db %01000010
        db %01000010
        db %01111110
        db %01000010
        db %01000010
        db %00000000
        db %00000000    ;char42 'B'
        db %01111100
        db %01000010
        db %01111100
        db %01000010
        db %01000010
        db %01111100
        db %00000000
        db %00000000    ;char43 'C'
        db %00111100
        db %01000010
        db %01000000
        db %01000000
        db %01000010
        db %00111100
        db %00000000
        db %00000000    ;char44 'D'
        db %01111000
        db %01000100
        db %01000010
        db %01000010
        db %01000100
        db %01111000
        db %00000000
        db %00000000    ;char45 'E'
        db %01111110
        db %01000000
        db %01111100
        db %01000000
        db %01000000
        db %01111110
        db %00000000
        db %00000000    ;char46 'F'
        db %01111110
        db %01000000
        db %01111100
        db %01000000
        db %01000000
        db %01000000
        db %00000000


; ******************************************************************************
; Writable DMA Program
; ******************************************************************************
DMACopyProg:
            db  $C3             ; R6-RESET DMA
            db  $C7             ; R6-RESET PORT A Timing
            db  $CB             ; R6-SET PORT B Timing same as PORT A
        
            db  $7D             ; R0-Transfer mode, A -> B
DMASrc:     dw  $0000           ; R0-Port A, Start address      (source address)
DMALen:     dw  6912            ; R0-Block length           (length in bytes)
        
            db  $54             ; R1-Port A address incrementing, variable timing
            db  $02             ; R1-Cycle length port A
                
            db  $50             ; R2-Port B address fixed, variable timing
            db  $02             ; R2-Cycle length port B
                
            db  $AD             ; R4-Continuous mode  (use this for block tansfer)
DMADest:    dw  $4000           ; R4-Dest address           (destination address)
                
            db  $82             ; R5-Restart on end of block, RDY active LOW
            
            db  $CF             ; R6-Load
            db  $B3             ; R6-Force Ready
            db  $87             ; R6-Enable DMA
ENDDMA:

            defc DMASIZE = ENDDMA-DMACopyProg


; ******************************************************************************
; DMA to a register (Coppyer)
; ******************************************************************************
DMACopyToReg:
        db $C3          ;R6-RESET DMA
        db $C7          ;R6-RESET PORT A Timing
        db $CB          ;R6-SET PORT B Timing same as PORT A

        db $7D          ;R0-Transfer mode, A -> B (%01 = transfer, %1xx = A->B)
DMASrcAdd:
        dw $1234        ;R0-Port A, Start address               (source address)
DMASrcLen:
        dw 240          ;R0-Block length                    (length in bytes)

        db $54          ;R1-Port A address incrementing, variable timing
        db $02          ;R1-Cycle length port A
          
        db %01101000    ;R2-Port B bit 3 = PORT dest, bit 6 = cycle len.
        db $02          ;R2-Cycle length port B
          
        db $AD          ;R4-Continuous mode  (%01 Continuous, %11 Port Add Lo/Hi, %01 = $ad )
        dw $253B        ;R4-Dest address
          
        db $82          ;R5-Restart on end of block, RDY active LOW
     
        db $CF          ;R6-Load
        db $B3          ;R6-Force Ready
        db $87          ;R6-Enable DMA
DMACopyToReg_End:

        defc   DMACOPPERSIZE = DMACopyToReg_End-DMACopyToReg




_Keys:      ds  40
_RawKeys:   ds  8
_RomFont:   ds  0x300       ; copy of the ROM font
            incbin  "ROMFONT.FNT"

            ; xxxxxxxx
            ; yyyyyyyy
            ; PPPP_XM_YM_R_X8/PR
            ; V_E_NNNNNN
            ; H_N6_T_XX_YY_Y8
            ; +- 0_1_N6_XX_YY_PO
            ; +- 0_1_N6_0000_PO

_SpriteData: ds  128*5           ; raw sprite data

_SpriteShape:
            db  $e3,$e3,$e3,$e3,$e3,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$e3,$ff,$ff,$ff,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$ff,$ff,$ff,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$e3,$ff,$ff,$e3,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$e3,$ff,$ff,$ff,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$e3,$ff,$ff,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$e3,$e3,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$e3,$ff,$ff,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$ff,$ff,$ff,$ff,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$ff,$ff,$ff,$ff,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$ff,$ff,$ff,$e3,$ff,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$ff,$ff,$ff,$ff,$e3,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$e3,$ff,$ff,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$ff,$ff,$ff,$e3,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$ff,$ff,$e3,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $e3,$ff,$ff,$ff,$e3,$ff,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3

            db  $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            db  $ff,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff
            db  $ff,$00,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$00,$ff
            db  $ff,$00,$00,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$00,$00,$ff
            db  $ff,$00,$00,$00,$ff,$00,$00,$00,$00,$00,$00,$ff,$00,$00,$00,$ff
            db  $ff,$00,$00,$00,$00,$ff,$00,$00,$00,$00,$ff,$00,$00,$00,$00,$ff
            db  $ff,$00,$00,$00,$00,$00,$ff,$00,$00,$ff,$00,$00,$00,$00,$00,$ff
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$ff,$00,$00,$00,$00,$00,$00,$ff
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$ff,$00,$00,$00,$00,$00,$00,$ff
            db  $ff,$00,$00,$00,$00,$00,$ff,$00,$00,$ff,$00,$00,$00,$00,$00,$ff
            db  $ff,$00,$00,$00,$00,$ff,$00,$00,$00,$00,$ff,$00,$00,$00,$00,$ff
            db  $ff,$00,$00,$00,$ff,$00,$00,$00,$00,$00,$00,$ff,$00,$00,$00,$ff
            db  $ff,$00,$00,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$00,$00,$ff
            db  $ff,$00,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$00,$ff
            db  $ff,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff
            db  $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

            db  $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$00,$00,$00,$00,$00,$00,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3
            db  $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$e3,$e3,$e3,$e3,$e3,$e3,$e3,$e3

_EndKernel:


