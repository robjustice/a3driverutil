;==============================================================
;SOS GRAPHICS DRIVER
;(C) Apple Computer, Inc., 1983 v1.3
; - edited 3/24/86 to 8/31/86  by Erik Olbrys   CIS 71236,1245
;==============================================================

           .feature labels_without_colons
           .setcpu "6502"
           .reloc
;
;driver comment field
;this is put in the TEXT segment so we can get the lenght and output it ok
;
        .segment "TEXT"
        .word   $FFFF                           ;Comment follows..
        .word   $0039                           ;$39 bytes long
        .byte   "Copyright (C) 1983 Apple Computer, Inc.  Graphics Driver."

;
;Driver code part
;this is put in the DATA segement
;
           
           .segment "DATA"

DEVTYPE         =       $62      ;Character device, read/write
SUBTYPE         =       $01      ;Device subtype
MANID           =       $01      ;Manufacturer ID - Apple Inc.
RELEASE         =       $1300    ;Release number in BCD format

;-------------------
;N-WAY SWITCH MACRO
;SWITCH  [index], [bounds], adrs_table, [*]
;-------------------

           .MACRO  SWITCH index,bounds,adrs_table ;,noexec      ;See SOS Reference
           .IFNBLANK index        ;If PARM1 is present,
           LDA     index          ; load A with switch index
           .ENDIF
           .IFNBLANK bounds       ;If PARM2 is present,
           CMP     #bounds+1      ; perform bounds checking
           BCS     @110           ; on switch index
           .ENDIF
           ASL     A              ;Multiply by 2 for table index
           TAY
           LDA     adrs_table+1,Y ;Get switch address from table
           PHA                    ; and push onto Stack
           LDA     adrs_table,Y
           PHA
;           .IF     noexec <> "*"  ;If PARM4 is omitted,
           RTS                    ; exit to code
;           .ENDIF
@110       .ENDMACRO

;--------------------
;INCREMENT WORD MACRO
;--------------------

           .MACRO  INW inword
           INC     inword        ;Increment low-order
           BNE     @210          ;If 'Carry',
           INC     inword+1      ; increment high-order
@210       .ENDMACRO

;------------------------------
;INCREMENT 3-BYTE ADDRESS MACRO
;------------------------------

           .MACRO  INCADR addr
           INC     addr
           BNE     @310
           INC     addr+1
           BNE     @310         ;Bank overflow?
           LDX     #$80         ;Yes
           STX     addr+1
           INC     addr+1+$1400 ;Increment X-byte
@310       .ENDMACRO

           .PROC   GRAFIX

;---------------
;GENERAL EQUATES
;---------------

TRUE       =       $80
FALSE      =       $00
BITON0     =       $01
BITON1     =       $02
BITON2     =       $04
BITON3     =       $08
BITON4     =       $10
BITON5     =       $20
BITON6     =       $40
BITON7     =       $80

;-----------------------------
;SOS GLOBAL DATA & SUBROUTINES
;-----------------------------

SCR_MODE        =       $1906
MEM_FLAG        =       $1907            ;amount of mem reserved for grafix
SYSERR          =       $1928            ;report error to system

;----------------------------
;SOS DEVICE HANDLER INTERFACE
;----------------------------

SOSINT          =       $C0
REQCODE         =       SOSINT+0        ;SOS request code
BUFFER          =       SOSINT+2        ;Buffer pointer
REQCNT          =       SOSINT+4        ;Requested count/byte count
CTLSTAT         =       SOSINT+2        ;Control/status code
CSLIST          =       SOSINT+3        ;Control/status list pointer
RETPTR          =       SOSINT+8        ;Returned count pointer

;--------------------
;HARDWARE I/O ADDRESS
;--------------------

SETBW           =       $C050           ;black & white on
SETCOL          =       $C051           ;color on
COLM40          =       $C052           ;set 40 columns
COLM80          =       $C053           ;set 80 columns
LOWSCR          =       $C054           ;set page one
HISCR           =       $C055           ;set page two
TEXT            =       $C056           ;set text mode
HIRES           =       $C057           ;set hires grafix mode

;---------------
;SOS ERROR CODES
;---------------

XREQCODE        =       $20              ;invalid request code
XCTLCODE        =       $21              ;invalid control/status code
XCTLPARAM       =       $22              ;invalid control/status param
XNOTOPEN        =       $23              ;device not open
XNOTAVAIL       =       $24              ;device not available
XNORESRC        =       $25              ;resource not available
XBADOP          =       $26              ;invalid operation
XBADMEM         =       $30              ;insufficient amount of memory alocated

;-----------------
;ZERO PAGE EQUATES
;-----------------

ZPC9    =       $C9
ZPCA    =       $CA
ZPCB    =       $CB
ZPCC    =       $CC
ZPCD    =       $CD
ZPCE    =       $CE
ZPCF    =       $CF
ZPD0    =       $D0
ZPD1    =       $D1
ZPD2    =       $D2
ZPD3    =       $D3
ZPD4    =       $D4
ZPD5    =       $D5
ZPD6    =       $D6
ZPD7    =       $D7
ZPD8    =       $D8
ZPD9    =       $D9
ZPDA    =       $DA
ZPDB    =       $DB
ZPDC    =       $DC
ZPDD    =       $DD
ZPDE    =       $DE
ZPDF    =       $DF
ZPE0    =       $E0
ZPE1    =       $E1
ZPE2    =       $E2
ZPE3    =       $E3
ZPE4    =       $E4
ZPE5    =       $E5
ZPE6    =       $E6
ZPE7    =       $E7
ZPE8    =       $E8
ZPE9    =       $E9
ZPEA    =       $EA
ZPEB    =       $EB
ZPEC    =       $EC
ZPED    =       $ED
ZPEE    =       $EE
ZPEF    =       $EF
ZPF0    =       $F0
ZPF1    =       $F1
ZPF2    =       $F2
ZPF3    =       $F3
ZPF4    =       $F4
ZPF5    =       $F5
ZPF6    =       $F6
ZPF7    =       $F7
ZPF8    =       $F8
ZPF9    =       $F9
ZPFA    =       $FA
ZPFB    =       $FB
ZPFC    =       $FC
ZPFD    =       $FD
ZPFE    =       $FE
ZPFF    =       $FF


;       ---------------------------
;       Device Identification res  
;       ---------------------------

DIB     .word   0000                            ; link
        .word   START                           ; entry point
        .byte   07                              ; name len
        .byte   ".GRAFIX        "               ; device name
        .byte   TRUE                            ; status -- active
        .byte   FALSE                           ; slot -- N/A
        .byte   00                              ; unit #
        .byte   DEVTYPE                         ; type
        .byte   SUBTYPE                         ; subtype
        .byte   00                              ; reserved for future use
        .word   0000                            ; # res  s -- none
        .word   MANID                           ; man id -- Apple Inc.
        .word   RELEASE                         ; ver # -- 1.30

CNFBLK  .word   $0096            ; DCB length
DCB     .byte   $00              ;graphics mode
        .byte   $00              ;transfer mode
        .word   $0000            ;cursor x-position
        .word   $0000            ;       y-position
        .word   $0000            ;viewport left edge
        .word   $0117            ;         right edge:= 279.
        .word   $0000            ;         bottom edge
        .word   $00BF            ;         top edge:= 191.
        .byte   $0F              ;pencolor:= white
        .byte   $00              ;fillcolor:= black
        .word   $0C00            ;Character font address
        .byte   $00              ; address X-Byte
        .byte   $07              ;character cell bit width
        .byte   $08              ;character cell bit height
        .byte   $80              ;  "Not currently used"   (but it IS used)

COLT2   .res    8,$00           ; Color operator table
        .res    8,$11
        .res    8,$22
        .res    8,$33
        .res    8,$44
        .res    8,$55
        .res    8,$66
        .res    8,$77
        .res    8,$88
        .res    8,$99
        .res    8,$AA
        .res    8,$BB
        .res    8,$CC
        .res    8,$DD
        .res    8,$EE
        .res    8,$FF

DCB_LN  =       *-DCB

L00F5   .res    $0E,$00                  ;some storage space

;       duplicate DCB -- used when restoring configuration

CNTL_PARAM  .byte   $00
GMODE       .byte   $00              ;graphics mode
XFMODE      .byte   $00              ;transfer mode
XPOS        .word   $0000            ;cursor x-position
YPOS        .word   $0000            ;       y-position
VPORT_L     .word   $0000            ;viewport left edge
VPORT_R     .word   $0117            ;         right edge = 279.
VPORT_B     .word   $0000            ;         bottom edge
VPORT_T     .word   $00BF            ;         top edge = 191.
PN_COL      .byte   $0F              ;pencolor = white
F_COL       .byte   $00              ;fillcolor = black
FONT        .word   $0C00            ;Character font address
FONT_XB     .byte   $00              ; address X-Byte
FWIDTH      .byte   $07              ;character cell bit width
FHEIGHT     .byte   $08              ;character cell bit height
NOT_USED    .byte   $80              ;  "Not currently used"

COL_TAB     .res    8,$00           ; Color operator table
            .res    8,$11
            .res    8,$22
            .res    8,$33
            .res    8,$44
            .res    8,$55
            .res    8,$66
            .res    8,$77
            .res    8,$88
            .res    8,$99
            .res    8,$AA
            .res    8,$BB
            .res    8,$CC
            .res    8,$DD
            .res    8,$EE
            .res    8,$FF

            .byte   "(C) 1980 Apple Computer"

OPENFLG     .byte   00              ;open/close flag
L01B2       .byte   00              ;base addr-hi temp
L01B3       .byte   00              ;alt base addr-hi temp
BYT_OFF     .word   0008            ;byte offset of a font character
L01B6       .byte   00              ; \
L01B7       .byte   00              ;  bit pattern used
L01B8       .byte   00              ;  in CLRVIEW
L01B9       .byte   00              ; /

CHR_TEMP    .byte  00
CHR_INDX    .byte  00              ;input buffer byte count
CHR_BUFF    .res   $20, 00          ;input buffer - copied from the D_WRITE buffer

WIDTH_B  .byte   01              ;byte width of font character

L01DD    .byte   00, 00, 00, 00, 00, 07, 00, 08, 00
L01E6    .byte   00, 00
L01E8    .byte   00

MVR_LO   .byte   $17, $17, $2F, $8B  ;max viewport right, low byte (indexed)
MVT_LO   .byte   $BF                 ;max viewport top, low byte
MVR_HI   .byte   $01, $01, $02, $00  ;max viewport right, high byte
MVT_HI   .byte   $00                 ;max viewport top, high byte

L01F3    .byte   00, 00, 01, 01, 02, 02, 03, 03, 00, 00, 01
L01FE    .byte   01, 02, 02, 03, 03, 00, 00, 01, 01, 02, 02, 03, 03

L020B    .byte   $00,$80,$00,$80,$00,$80,$00,$80  ;base addr lo-byte, by y coord
         .byte   $28,$A8,$28,$A8,$28,$A8,$28,$A8
         .byte   $50,$D0,$50,$D0,$50,$D0,$50,$D0

L0223    .byte   $20,$20,$20,$20,$40,$60,$60,$60  ;base addr hi-byte, by GMODE

;required-memory bits for gmodes 0..7 -- must be =< val in $1907 (MEM_FLAG)
; also doubles as alternate base-address_hi
REQD_MEM  .byte  $20,$40,$40,$40,$40,$80,$80,$80  

L0233    .byte   $00,$01,$1E,$60,$00,$07,$78
L023A    .byte   $0F,$70,$00,$03,$3C,$40,$00,$0F,$0F
         .byte   $0F,$0F,$0F,$0F,$0F
         .byte   $01,$02,$04,$08,$10,$20,$40,$80
L0250    .byte   $15,$0E,$07,$00
L0254    .byte   $0E,$0E,$1C,$07
L0258    .byte   $03
L0259    .byte   $04
L025A    .byte   $1C

START    SWITCH  REQCODE, 8, REQSW

BADREQ   LDA     #XREQCODE       ;switch fell through -
         JSR     SYSERR          ; Invalids request code error

NOTOPEN  LDA     #XNOTOPEN       ;Graphics driver not open error
         JSR     SYSERR

REQSW   =       *               ; dispatch table
        .word   D_READ-1
        .word   D_WRITE-1
        .word   D_STATUS-1
        .word   D_CONTRL-1
        .word    BADREQ-1       ;not implemented
        .word    BADREQ-1       ;not implemented
        .word   D_OPEN-1
        .word   D_CLOSE-1
        .word   D_INIT-1


D_INIT  LDA     #FALSE
        STA     OPENFLG         ;set it as closed
        RTS


D_OPEN  BIT     OPENFLG         ;is it already open?
        BPL     @1
        LDA     #XNOTAVAIL      ;yep -- it's not available,
        JSR     SYSERR          ; complain, never returns

@1      JSR     CONT0           ;initilize device
        LDA     #TRUE           ;set it as open
        STA     OPENFLG
        RTS


D_CLOSE    ASL     OPENFLG         ;close it
           BCS     @1
           JMP     NOTOPEN         ;but it's not open! complain
@1         RTS


D_STATUS   BIT     OPENFLG         ;is it open?
           BMI     @1
           JMP     NOTOPEN
@1         SWITCH  CTLSTAT, 2, STATSW

DST_ERR    LDA     #XCTLCODE       ;invalid status code
           JSR     SYSERR

STATSW    .word   STAT0-1         ;NOP
          .word   STAT1-1         ;preserve status table
          .word   STAT2-1         ;unknown

STAT0     RTS                     ;nop

STAT1     LDY     #$00             ;preserve status table
          LDA     (CSLIST),Y
          CMP     #DCB_LN
          BCS     @3
          LDA     #XCTLPARAM
          JSR     SYSERR
@3        LDA     #DCB_LN
          STA     (CSLIST),Y
          TAY
@4        LDA     CNTL_PARAM,Y
          STA     (CSLIST),Y
          DEY
          BNE     @4
          RTS

STAT2   LDY     #$00             ;Return newline status
        LDA     #$00             ; always =0, only satisfying the proper
        STA     (CSLIST),Y       ; character count will terminate an input,
        RTS                     ; as opposed to terminating at newline char


D_CONTRL BIT     OPENFLG         ;is it open?
         BMI     @1
         JMP     NOTOPEN
@1       SWITCH  CTLSTAT, 2, CNTLSW

@2      JMP     DST_ERR

CNTLSW  .word   CONT0-1         ;Device reset
        .word   CONT1-1         ;Restore status table
        .word   CONT2-1         ;NOP

CONT0   JSR     L03A1           ;Reset
        LDX     #DCB_LN
@3      LDA     CNFBLK+1,X
        STA     CNTL_PARAM,X
        DEX
        BNE     @3
        JSR     L03C9
        JSR     L03BE
        RTS

CONT1   LDY     #$00             ;Restore status table
        LDA     (CSLIST),Y
        CMP     #DCB_LN
        BEQ     @4
        LDA     #XCTLPARAM
        JSR     SYSERR
@4      JSR     L03A1
        LDY     #DCB_LN
@5      LDA     (CSLIST),Y
        DEY
        STA     CNTL_PARAM+1,Y
        BNE     @5
        JSR     L03C9
        JSR     L03BE
        RTS

CONT2   RTS                     ;nop


D_READ  BIT     OPENFLG         ;check if it's open
        BMI     @1
        JMP     NOTOPEN

@1      JSR     L03A1           ;set up things
        LDA     XPOS+1          ;is XPOS<0000?
        BMI     @3              ; br if so
        LDX     ZPD5            ;is XPOS>MAX_VIEWPORT_RIGHT?
        LDA     MVR_LO,X
        CMP     XPOS
        LDA     MVR_HI,X
        SBC     XPOS+1
        BCC     @3              ; br if so
        LDA     YPOS+1          ;is YPOS<0000?
        BMI     @3              ; br if so
        LDA     MVT_LO          ;is YPOS>MAX_VIEWPORT_TOP?
        CMP     YPOS
        LDA     MVT_HI
        SBC     YPOS+1
        BCC     @3              ; br if so
        LDA     YPOS
        JSR     CALCBASE        ;calc base addr
        LDA     XPOS
        LDY     XPOS+1
        JSR     CALCBYTE        ;calc byte offset
        JSR     L10CB           ;read color at pen position
        ORA     #$40             ;add 64 (it's within screen boundaries)
@2      LDY     #$00             ;put char into read buffer
        STA     (BUFFER),Y
        STY     RETPTR+1
        INY
        STY     RETPTR          ;RETPTR:=$0001
        JMP     L03BE           ;reset & leave
@3      LDA     #BITON7         ;flag that pen is out of screen boundary
        BNE     @2              ;always

L03A1   LDX     #$0E            ;setup things: restore stuff to zpage
@1      LDA     L00F5,X
        STA     ZPCA,X
        DEX
        BPL     @1
        LDX     #$35
        LDA     #$00
@2      STA     ZPC9+$1401,X
        DEX
        BPL     @2
        LDA     #$8F             ;X-BYTE for graphics
        STA     ZPCA+$1401
        STA     ZPCC+$1401
        RTS

L03BE   LDX     #$0E             ;reset things: save stuff from zpage
@1      LDA     ZPCA,X
        STA     L00F5,X
        DEX
        BPL     @1
        RTS

; Verify Grafix parms (?) -- lots of jumps to here
L03C9   LDA     GMODE           ;filter and check out GRAFIXMODE
        AND     #$07
        STA     GMODE
        TAY
        AND     #$03             ;filter out page bit (page 1/page 2)
        STA     ZPD5
        TAX
        LDA     L0223,Y         ;base addr hi-byte
        STA     L01B2
        LDA     REQD_MEM,Y
        STA     L01B3
        LDA     #$00
        STA     L01E6
        STA     L01E8
        LDA     #$F0
        STA     ZPD0
        
        LDA     F_COL           ;check out FILL COLOR
        ASL     A
        ASL     A
        ASL     A
        ASL     A               ;times 16
        ORA     F_COL           ;copies low nybble into hi nybble
        PHA
        BEQ     @1              ;branch if fill color is black
        CPX     #$03             ;X-reg contains GMODE
        BEQ     @2              ;br if GMODE=3 (140x192 full color)
        DEX
        TXA
        BEQ     @1
        LDA     #$FF
@1      LDX     #$03
@2      TAY
        LSR     A
        STA     L01B6,X
        TYA
        ROR     A
        TAY
        DEX
        BPL     @2
        PLA
        LDX     ZPD5
        DEX
        BNE     @3
        STA     L01B7
        STA     L01B9
@3      LDX     #$00
        STX     ZPD6
        STX     ZPD7
        STX     ZPD8
        
        LDA     XFMODE          ;check out transfer mode
        AND     #$07
        STA     XFMODE          ;filter it
        LSR     A
        ROR     ZPD6
        LSR     A
        ROR     ZPD6
        BIT     ZPD6
        BVC     @4
        BPL     @4
        EOR     #$01
@4      LSR     A
        ROR     ZPD7

        LDA     #$FF            ;check out the color table
        LDX     #$7F
@5      LDY     #$08
@6      CMP     COL_TAB,X
        BNE     @7
        DEX
        DEY
        BNE     @6
        SEC
        SBC     #$11
        BCS     @5
        BCC     @8              ;always
@7      LDA     ZPD7
        ORA     #$40
        STA     ZPD7

@8      LDA     #$00             ;check out the font
        LDX     #$09
@9      STA     WIDTH_B,X       ;width_b .. width_b+9 := 00
        DEX
        BPL     @9
        STA     BYT_OFF         ;set both to zero
        STA     BYT_OFF+1
        LDA     FONT_XB
        STA     ZPD3+$1401
        LDA     FONT+1          ;check if the font is the system font
        CMP     #$0C
        BNE     @10             ;is hi-byte = $0C?, br if not
        LDA     FONT_XB
        BNE     @10             ;is x-byte = $00, br if not
        STA     ZPD8
        BNE     @11             ;never branches, why is this here? bug?
        LDX     #$07             ;it's the system font (00:0C00), set it up
        STX     FWIDTH          ;width := 7
        INX
        STX     FHEIGHT         ;height := 8
        JMP     @11
@10     LDA     NOT_USED
        STA     ZPD8
@11     LDX     FWIDTH
        BEQ     @12
        DEX
        TXA
        LSR     A
        LSR     A
        LSR     A
        TAX
        INX                     ;(FWIDTH-1) div 8 +1
        STX     WIDTH_B         ; width of font in bytes
@12     LDX     FHEIGHT
        BEQ     @14
@13     CLC                     ;calculate total # of bytes of
        LDA     WIDTH_B         ; a single character
        ADC     BYT_OFF
        STA     BYT_OFF
        LDA     BYT_OFF+1
        ADC     #$00
        STA     BYT_OFF+1
        DEX
        BNE     @13             ;BYT_OFF=FHEIGHT * WIDTH_B

@14     LDX     #$06             ;check out the viewport definitions
L15     LDA     VPORT_L+1,X
        BPL     @16             ;if it's negative, set it to zero
        LDA     #$00
        STA     VPORT_L,X
        STA     VPORT_L+1,X     ;set viewport left, right, top, bott := 0000
@16     DEX
        DEX
        BPL     L15             ;loop back
        LDX     #$06
        LDY     #$04
@17     LDA     MVR_LO,Y        ;is vport set beyond max size of the screen
        CMP     VPORT_L,X       ; for this grafix mode?
        LDA     MVR_HI,Y
        SBC     VPORT_L+1,X
        BCS     @18
        LDA     MVR_LO,Y        ;Yes, so set it to the max allowed
        STA     VPORT_L,X
        LDA     MVR_HI,Y
        STA     VPORT_L+1,X
@18     DEX
        DEX
        BMI     @19
        CPX     #$04
        BCS     @17
        LDY     ZPD5
        BCC     @17
@19     LDX     #$04
@20     LDA     VPORT_R,X
        CMP     VPORT_L,X
        LDA     VPORT_R+1,X
        SBC     VPORT_L+1,X     ;check if left & right are reversed
        BCS     @21             ; branch if they're OK
        LDY     VPORT_L,X       ; otherwise, turn them around the right way
        LDA     VPORT_R,X
        STA     VPORT_L,X
        TYA
        STA     VPORT_R,X
        LDY     VPORT_L+1,X
        LDA     VPORT_R+1,X
        STA     VPORT_L+1,X
        TYA
        STA     VPORT_R+1,X
@21     DEX
        DEX
        DEX
        DEX
        BPL     @20
        BIT     SCR_MODE
        BVC     @29
        BMI     @22
        LDA     $01FE  ;;;;;L01FE   this seems like a bug as these are absolute addresses!
        AND     #$DF
        STA     $01FE  ;;;;;L01FE
        JMP     @29
@22     LDA     $01FE  ;;;;;L01FE
        ORA     #$20
        STA     $01FE  ;;;;;L01FE
        LDA     SCR_MODE
        LSR     A
        BCS     @23
        BIT     SETBW           ;turn on the B&W mode
        BCC     @24             ; always
@23     BIT     SETCOL          ;turn on the color mode
@24     LSR     A
        BCS     @25
        BIT     COLM40          ;40 column mode
        BCC     @26             ; always
@25     BIT     COLM80          ;80 column mode
@26     LSR     A
        BCS     @27
        BIT     LOWSCR          ;page one
        BCC     @28             ; always
@27     BIT     HISCR           ;page two
@28     BIT     HIRES           ;turn on the graphics screen
@29     RTS


D_WRITE BIT     OPENFLG         ;but are we open?
        BMI     @1             ; br if open
        JMP     NOTOPEN

@1     LDX     GMODE
        LDA     MEM_FLAG        ;memory allocation flag byte
        CMP     REQD_MEM,X      ;see if proper amt of mem is allocated
        BCS     @2              ; br if OK
        LDA     #XBADMEM        ;sorry, not enough memory for this mode
        JSR     SYSERR          ; report the problem & leave

@2      JSR     L03A1
        LDA     #$00
        STA     RETPTR
        STA     RETPTR+1

@3      LDA     RETPTR+1        ;check if all the chars in
        CMP     REQCNT+1        ; this DßWRITE call have been
        BCC     @4              ; carefully scrutinized.
        LDA     RETPTR
        CMP     REQCNT          ; If all finished, jump out, otherwise
        BCC     @4              ; branch down a few bytes.
        JMP     L03BE

@4      LDY     #$00
        LDA     (BUFFER),Y       ;get the next character in the buffer
        INCADR  BUFFER          ;increment the address pointer to the next byte
        INW     RETPTR          ;increment the # of bytes handled
        JSR     @5              ;check out the current character
        JMP     @3              ;loop back for the next character

@5      LDX     CHR_TEMP
        BEQ     @7              ;br if a call is not currently being processed
        LDY     CHR_INDX        ;load the index
        STA     CHR_BUFF-1,Y    ;note: first byte stored at CHR_BUFF
        INY                     ;incr index
        TYA
        CMP     CALL_LEN,X      ;Are all the bytes in this call present?
        BCS     @6              ; br if all present
        STY     CHR_INDX        ;otherwise, store index,
        RTS                     ; and get some more bytes

@6      LDA     CHR_TEMP        ;all chars present, process call
        LDY     #$00             ; ...but first, reset some things
        STY     CHR_TEMP        ;     for the next time around
        STY     CHR_INDX        ;clear index & temp
        JMP     CON_CHR         ;now -- handle the call

@7      CMP     #$20             ;is it a control character?
        BCC     @8              ; br if so
        JMP     L0618           ;nope -- print the char onto the screen

@8      TAX
        LDA     CALL_LEN,X      ;get parm length for this call
        BEQ     @9              ; br if this isn't a valid call -- i.e. NOP
        STX     CHR_TEMP        ;store the cntrl char here
        CMP     #$01             ;parm length=1?
        BEQ     @6              ; br if call doesn't have any parms (go to it!)
        LDA     #$01
        STA     CHR_INDX        ;initially put a one here -- used for indexing
@9      RTS

; This list contains the total length (in bytes) of each control
; character call, including the control character itself.
;     Ex.: chr(00) -- 0 bytes (nop)
;          chr(03) -- 6 bytes (char_set)

CALL_LEN .byte  $00,$01,$09,$06,$0E,$00,$00,$00  ;chr( 0)..chr( 7)
        .byte   $00,$00,$01,$00,$00,$01,$01,$01  ;chr( 8)..chr(15)
        .byte   $02,$01,$06,$02,$02,$02,$00,$00  ;chr(16)..chr(23)
        .byte   $05,$05,$05,$00,$01,$00,$00,$00  ;chr(24)..chr(31)

;                        set up to put a character on the graphics screen
L0618   CMP     #$A0
        BCS     @1              ; br is char >= $A0
        CMP     #$80
        BCC     @1              ; br if char <  $80
        AND     #$7F             ;clear hi-bit
@1     LDX     FWIDTH          ;copy font info into temp locations
        STX     L01DD+5
        LDX     FHEIGHT
        STX     L01DD+7
        LDX     #$00
        STX     L01DD+6
        STX     L01DD+8
        LDX     FONT            ;copy font addrs into z-page locations
        STX     ZPD3
        LDX     FONT+1
        STX     ZPD4
        LDX     FONT_XB
        STX     ZPD3+$1401
        TAX
        BEQ     @4

@2      CLC
        LDA     ZPD3             ;set up addr of font character
        ADC     BYT_OFF         ;char_addr:= base_addr + (char# * byte_offset)
        STA     ZPD3
        LDA     ZPD4
        ADC     BYT_OFF+1
        STA     ZPD4
        BCC     @3
        INC     ZPD3+$1401
        INC     ZPD3+$1401
@3      DEX
        BNE     @2
@4      JSR     PUTres          ;put font char on screen
        CLC                     ;update X & Y position after
        LDA     XPOS            ; putting char on screen
        ADC     FWIDTH
        STA     XPOS
        LDA     XPOS+1
        ADC     #$00
        STA     XPOS+1
        RTS

;       Handle those control characters....
CON_CHR SWITCH  ,,CHARS

CHARS   .word    CHR_NOP-1
        .word   RSTVIEW-1      ;01     Reset Viewport
        .word   SETVIEW-1      ;02     Set Viewport
        .word   CHARSET-1      ;03     Character Set
        .word   DRAWBLK-1      ;04     Drawres  
        .word    CHR_NOP-1
        .word    CHR_NOP-1
        .word    CHR_NOP-1
        .word    CHR_NOP-1
        .word    CHR_NOP-1
        .word   MOV_PDN-1      ;10.    Move Pen Down (LF)
        .word    CHR_NOP-1
        .word    CHR_NOP-1
        .word   RET_PEN-1       ;13.    Return Pen (CR)
        .word   SCRNOFF-1      ;14.    Turn Graphics Screen Off
        .word   SCRN_ON-1       ;15.    Turn Graphics Screen On
        .word   SETMODE-1      ;16.    Set Graphics Mode
        .word   RSTCOLT-1     ;17.    Reset Color Operator Table
        .word   SETCOLT-1     ;18.    Set Color Operator Table
        .word   SETPCOL-1     ;19.    Set Pen Color
        .word   SETFCOL-1     ;20.    Set Fill Color
        .word   SETXMOD-1     ;21.    Set Transfer Mode
        .word    CHR_NOP-1
        .word    CHR_NOP-1
        .word   LINE_TO-1       ;24.    Draw Line to X,Y
        .word   DOT_AT-1        ;25.    Plot Point at X,Y
        .word   MOVE_TO-1       ;26.    Move Pen to X,Y
        .word    CHR_NOP-1
        .word   CLRVIEW-1      ;28.    Clear Viewport
        .word    CHR_NOP-1
        .word    CHR_NOP-1
        .word    CHR_NOP-1

CHR_NOP RTS

SETMODE LDA     CHR_BUFF        ;set graphics mode
        STA     GMODE
        JMP     L03C9

SETXMOD LDA     CHR_BUFF        ;set transfer mode
        STA     XFMODE
        JMP     L03C9

SCRNOFF LDA     GMODE           ;turn screen off
        AND     #$07
        ORA     #$40
        STA     SCR_MODE
        JMP     L03C9

SCRN_ON LDA     GMODE           ;enable graphics mode
        AND     #$07
        ORA     #$C0
        STA     SCR_MODE
        JMP     L03C9

MOV_PDN SEC                     ;move pen down  (LF)
        LDA     YPOS            ;subtract font height from the
        SBC     FHEIGHT         ; current x,y position
        STA     YPOS
        LDA     YPOS+1
        SBC     #$00
        STA     YPOS+1
        RTS

RET_PEN LDA     VPORT_L         ;return pen   (CR)
        STA     XPOS            ;set x position to the left
        LDA     VPORT_L+1       ; viewport edge
        STA     XPOS+1
        RTS

CHARSET LDX     #$04             ;character set
@1     LDA     CHR_BUFF,X      ;store chacter set address
        STA     FONT,X
        DEX
        BPL     @1
        JMP     L03C9

RSTVIEW LDX     ZPD5             ;reset viewport
        LDA     #$00
        STA     VPORT_L
        STA     VPORT_L+1
        STA     VPORT_B
        STA     VPORT_B+1       ;left & bottom := 0
        LDA     MVR_LO,X
        STA     VPORT_R         ;viewport-right := max right value
        LDA     MVR_HI,X
        STA     VPORT_R+1
        LDA     MVT_LO          ;viewport-top := max top value
        STA     VPORT_T
        LDA     MVT_HI
        STA     VPORT_T+1
        RTS

SETVIEW LDX     #$08             ;set viewport
@1     LDA     CHR_BUFF-1,X
        STA     VPORT_L-1,X
        DEX
        BNE     @1
        JMP     L03C9

RSTCOLT LDX     #$7F             ;reset color table
@1     LDA     COLT2,X
        STA     COL_TAB,X
        DEX
        BPL     @1
        JMP     L03C9

SETCOLT LDA     CHR_BUFF+4      ;set color table
        AND     #$0F
        STA     CHR_BUFF+4
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        ORA     CHR_BUFF+4
        STA     CHR_BUFF+4
        CLV
        LDX     #$0F
        LDA     CHR_BUFF
@1     ASL     A
        BCC     @7
        PHA
        LDY     #$0F
        LDA     CHR_BUFF+2
@2      ASL     A
        BCC     @5
        PHA
        STY     CHR_BUFF
        TXA
        PHA
        ASL     A
        ASL     A
        ASL     A
        LSR     CHR_BUFF
        ORA     CHR_BUFF
        TAX
        LDA     COL_TAB,X
        EOR     CHR_BUFF+4
        BCS     @3
        AND     #$0F
        BCC     @4
@3      AND     #$F0
@4      EOR     CHR_BUFF+4
        STA     COL_TAB,X
        PLA
        TAX
        PLA
@5      DEY
        BMI     @6
        CPY     #$07
        BNE     @2
        LDA     CHR_BUFF+3
        BVC     @2
@6      PLA
@7      DEX
        BMI     @8
        CPX     #$07
        BNE     @1
        LDA     CHR_BUFF+1
        BVC     @1
@8      JMP     L03C9

SETPCOL LDA     CHR_BUFF        ;set pen color
        AND     #$0F
        STA     PN_COL
        RTS

SETFCOL LDA     CHR_BUFF        ;set fill color
        AND     #$0F
        STA     F_COL
        JMP     L03C9

MOVE_TO LDX     #$03             ;move to
@1     LDA     CHR_BUFF,X
        STA     XPOS,X
        DEX
        BPL     @1
        RTS

DOT_AT  LDX     #$03             ;dot at
@1     LDA     CHR_BUFF,X
        STA     XPOS,X
        STA     ZPF5,X          ; ZPF5..ZPF8
        STA     ZPF0,X          ; ZPF0..ZPF3
        DEX
        BPL     @1
        JSR     L0C21           ;out of bounds?
        BCS     @2              ;yep. Don't bother displaying it
        JMP     L0F9D           ;Display it
@2      RTS

LINE_TO LDX     #$03             ;line to
@1     LDA     XPOS,X
        STA     ZPF0,X          ; ZPF0..ZPF3
        LDA     CHR_BUFF,X
        STA     XPOS,X
        STA     ZPF5,X          ; ZPF5..ZPF8
        DEX
        BPL     @1
        JSR     L0C21
        BCS     @2
        JSR     L0F9D
        JMP     L0F02
@2      RTS

CLRVIEW LDX     #$0A             ;clear viewport
@1      LDA     ZPCA,X
        PHA
        DEX
        BPL     @1
        LDA     #$F0
        STA     L01E6
        LDA     VPORT_T
        JSR     CALCBASE        ;calc base addr
        SEC
        LDA     VPORT_T
        SBC     VPORT_B
        STA     ZPE8
        INC     ZPE8
        LDA     #$00
        STA     ZPF5
        STA     ZPF6
        LDA     VPORT_L
        LDY     VPORT_L+1
        JSR     CALCBYTE        ;calc byte offset
        STA     ZPF2
        LDX     ZPD5
        CPX     #$03
        BEQ     @2
        LDA     #$0F
        STA     ZPD0
@2      LDX     #$04
@3      LDA     ZPCE,X
        STA     ZPE9,X
        DEX
        BPL     @3
        LDA     ZPD7
        ORA     ZPD6
        BEQ     @5
        CLC
        LDA     VPORT_L
        SBC     VPORT_R
        STA     ZPF3
        LDA     VPORT_L+1
        SBC     VPORT_R+1
        STA     ZPF4
        DEC     ZPF3
        BNE     @4
        DEC     ZPF4
@4      JMP     CL19 ; (@19)

@5      STX     ZPF3
        STX     ZPF4
        LDA     ZPCF
        LDX     ZPD5
        CPX     #$03
        BNE     @6
        EOR     #$0F
@6      CMP     #$02
        JSR     CL29 ; (@29)
        BEQ     @9
        INC     ZPF2
        AND     #$03
        LSR     A
        BNE     @7
        INC     ZPF2
@7      ROL     A
        JSR     CL30 ; (@30)
        EOR     #$FF
        STA     ZPF3
        TXA
        LDX     #$15
        CMP     #$03
        BNE     @8
        LDX     #$07
@8      DEX
        DEC     ZPF3
        LDA     L023A,X
        CMP     ZPCF
        BNE     @8
        LDA     L0233,X
        CMP     ZPD0
        BNE     @8
@9      LDA     VPORT_R
        LDY     VPORT_R+1
        JSR     CALCBYTE        ;calc byte offset
        STA     ZPF7
        LDX     ZPD5
        CPX     #$02
        BEQ     @10
        SEC
        ROR     ZPD2
@10     CPX     #$03
        BCC     @11
        LDA     ZPD0
        CMP     #$78
        JMP     @12

@11     LDA     ZPCF
        CMP     #$40
@12     JSR     CL29 ; (@29)
        EOR     #$07
        TAY
        BNE     @13
        INC     ZPF7
        BPL     @14
@13     AND     #$02
        BNE     @14
        DEC     ZPF7
@14     TYA
        BEQ     @16
        AND     #$03
        JSR     CL30 ; (@30)
        STA     ZPF6
        TXA
        LDX     #$0D
        CMP     #$03
        BNE     @15
        LDX     #$FF
@15     INX
        INC     ZPF6
        LDA     L023A,X
        CMP     ZPCF
        BNE     @15
        LDA     L0233,X
        CMP     ZPD0
        BNE     @15
@16     SEC
        LDA     ZPF7
        SBC     ZPF2
        BMI     @17
        LSR     A
        STA     ZPF5
        BNE     CL19 ; (@19)
        SEC
        LDA     ZPF3
        BCS     @18
@17     LDA     ZPF3
        LDX     ZPD5
        ADC     L0254,X
@18     SBC     ZPF6
        STA     ZPF3
        LDA     #$00
        STA     ZPF6
CL19    LDX     #$04
@20     LDA     ZPF3,X
        STA     ZPEE,X
        LDA     ZPE9,X
        STA     ZPCE,X
        DEX
        BPL     @20
        TAY
@21     INC     ZPEE
        BNE     @22
        INC     ZPEF
        BNE     @22
        JMP     @23

@22     JSR     L09B6
        JMP     @21

@23     DEC     ZPF0
        BMI     @25
        LDX     #$00
@24     LDA     L01B6,X
        STA     (ZPCA),Y
        LDA     L01B7,X
        STA     (ZPCC),Y
        INY
        INX
        INX
        CPX     #$02
        BEQ     @24
        BNE     @23             ;always

@25     STY     ZPCE
        DEC     ZPF1
        BMI     @26
        JSR     L09B6
        JMP     @25

@26     DEC     ZPE8
        BEQ     @27
        JSR     L0E16
        JMP     CL19

@27     LDX     #$00
@28     PLA
        STA     ZPCA,X
        INX
        CPX     #$0B
        BNE     @28
        LDA     #$00
        STA     L01E6
        RTS

CL29    ROL     A
        ROR     ZPCE
        ROL     A
        ROL     ZPD2
        ROL     A
        AND     #$07
        RTS

CL30    TAY
        CPX     #$02
        BEQ     @31
        LDY     #$03
        BCS     @31
        CMP     #$02
        BCS     @31
        DEY
@31     LDA     L0250,Y
        RTS

L09B6   JSR     L0FAA
        JSR     L0EA7
        RTS

DRAWBLK LDA     ZPD8
        PHA
        LDA     NOT_USED
        STA     ZPD8
        LDX     #$09
@1      LDA     WIDTH_B,X
        PHA
        LDA     CHR_BUFF+3,X
        STA     WIDTH_B,X
        DEX
        BPL     @1
        LDA     CHR_BUFF
        STA     ZPD3
        LDA     CHR_BUFF+1
        STA     ZPD4
        LDA     CHR_BUFF+2
        STA     ZPD3+$1401
        JSR     PUTres          ;put res   onto graphics screen
        LDX     #$00
@2      PLA
        STA     WIDTH_B,X
        INX
        CPX     #$0A
        BCC     @2
        PLA
        STA     ZPD8
        RTS

; =====================================================================
;
; HARDWARE SPECIFIC CODES FOLLOWS....
;
; =====================================================================

PUTres                  ;put character/res   onto graphics screen
        LDX     #$03
        LDA     #$F0
        STA     L01E8
@4      LDA     XPOS,X
        STA     ZPF5,X
        DEX
        BPL     @4
        SEC
        LDA     VPORT_L
        SBC     ZPF5
        STA     ZPD9
        LDA     VPORT_L+1
        SBC     ZPF6
        STA     ZPDA
        BMI     @5
        SEC
        LDA     L01DD+5
        SBC     ZPD9
        STA     L01DD+5
        LDA     L01DD+6
        SBC     ZPDA
        STA     L01DD+6
        BMI     @7
        LDA     VPORT_L
        STA     ZPF5
        LDA     VPORT_L+1
        STA     ZPF6
        BIT     ZPD8
        BPL     @5
        CLC
        LDA     L01DD+1
        ADC     ZPD9
        STA     L01DD+1
        LDA     L01DD+2
        ADC     ZPDA
        STA     L01DD+2         ;(L01DD+1):=(L01DD+1)+ZPD9
@5      CLC
        LDA     ZPF5
        ADC     L01DD+5
        STA     ZPD9
        LDA     ZPF6
        ADC     L01DD+6
        STA     ZPDA
        SEC
        LDA     ZPD9
        SBC     VPORT_R
        STA     ZPD9
        LDA     ZPDA
        SBC     VPORT_R+1
        STA     ZPDA
        BMI     @8
        INC     L01DD+5
        BNE     @6
        INC     L01DD+6
@6      SEC
        LDA     L01DD+5
        SBC     ZPD9
        STA     L01DD+5
        LDA     L01DD+6
        SBC     ZPDA
        STA     L01DD+6
        BPL     @8
@7      RTS

@8      SEC
        LDA     ZPF7
        SBC     VPORT_T
        STA     ZPD9
        LDA     ZPF8
        SBC     VPORT_T+1
        STA     ZPDA
        BMI     @10
        SEC
        LDA     L01DD+7
        SBC     ZPD9
        STA     L01DD+7
        LDA     L01DD+8
        SBC     ZPDA
        STA     L01DD+8
        BMI     @7
        BNE     @9
        LDA     L01DD+7
        BEQ     @7
@9      LDA     VPORT_T
        STA     ZPF7
        LDA     VPORT_T+1
        STA     ZPF8
        CLC
        LDA     L01DD+3
        ADC     ZPD9
        STA     L01DD+3
        LDA     L01DD+4
        ADC     ZPDA
        STA     L01DD+4
@10     SEC
        LDA     ZPF7
        SBC     VPORT_B
        STA     ZPE8
        LDA     ZPF8
        SBC     VPORT_B+1
        STA     ZPE9
        BMI     @7
        SEC
        LDA     ZPF7
        SBC     L01DD+7
        STA     ZPD9
        LDA     ZPF8
        SBC     L01DD+8
        STA     ZPDA
        SEC
        LDA     ZPD9
        SBC     VPORT_B
        STA     ZPD9
        LDA     ZPDA
        SBC     VPORT_B+1
        STA     ZPDA
        BPL     @11
        INC     ZPE8
        BNE     @12
@11     SEC
        LDA     ZPE8
        SBC     ZPD9
        STA     ZPE8
        LDA     ZPE9
        SBC     ZPDA
        STA     ZPE9
@12     LDX     #$0A
@13     LDA     ZPCA,X
        PHA
        DEX
        BPL     @13
        LDA     ZPF7
        JSR     CALCBASE        ;calc base addr
        LDA     ZPF5
        LDY     ZPF6
        JSR     CALCBYTE        ;calc byte offset
        LDX     #$04
@14     LDA     ZPCE,X
        STA     ZPE9,X
        DEX
        BPL     @14
        CLC
        LDA     L01DD+1
        ADC     L01DD+5
        STA     ZPD9
        LDA     L01DD+2
        ADC     L01DD+6
        STA     ZPDA
        LDA     L01DD+1
        AND     #$07
        STA     ZPEF
        LDA     ZPD9
        AND     #$07
        STA     ZPF1
        LDA     L01DD+1
        ROR     L01DD+2
        ROR     A
        ROR     L01DD+2
        ROR     A
        ROR     L01DD+2
        ROR     A
        STA     ZPEE
        LDA     ZPD9
        ROR     ZPDA
        ROR     A
        ROR     ZPDA
        ROR     A
        ROR     ZPDA
        ROR     A
        STA     ZPF0
DB15    LDA     L01DD+3
        BNE     @16
        LDA     L01DD+4
        BEQ     @18
@16     CLC
        LDA     ZPD3
        ADC     WIDTH_B
        STA     ZPD3
        LDA     ZPD4
        ADC     L01DD
        STA     ZPD4
        BCC     @17
        INC     ZPD3+$1401
        INC     ZPD3+$1401
@17     LDA     L01DD+3
        SEC
        SBC     #$01
        STA     L01DD+3
        LDA     L01DD+4
        SBC     #$00
        STA     L01DD+4
        JMP     DB15

@18     LDX     #$04
@19     LDA     ZPE9,X
        STA     ZPCE,X
        DEX
        BPL     @19
        LDY     ZPEE
        STY     ZPF2
        LDA     (ZPD3),Y
        LDX     #$00
@20     CPX     ZPEF
        BEQ     @21
        ASL     A
        INX
        BNE     @20
@21     STA     ZPF4
@22     STX     ZPF3
        CPX     ZPF1
        BNE     @23
        LDA     ZPF2
        CMP     ZPF0
        BEQ     @27
@23     LDY     ZPCE
        BIT     ZPD8
        BMI     @24
        LSR     ZPF4
        BPL     @25
@24     ASL     ZPF4
@25     LDA     #$00
        BCS     @26
        LDA     #$F0
@26     STA     L01E6
        JSR     L09B6
        LDX     ZPF3
        INX
        CPX     #$08
        BNE     @22
        LDX     #$00
        INC     ZPF2
        LDY     ZPF2
        LDA     (ZPD3),Y
        STA     ZPF4
        JMP     @22

@27     DEC     ZPE8
        BEQ     @29
        JSR     L0E16
        CLC
        LDA     ZPD3
        ADC     WIDTH_B
        STA     ZPD3
        LDA     ZPD4
        ADC     L01DD
        STA     ZPD4
        BCC     @28
        INC     ZPD3+$1401
        INC     ZPD3+$1401
@28     JMP     @18

@29     LDX     #$00
@30     PLA
        STA     ZPCA,X
        INX
        CPX     #$0B
        BNE     @30
        LDA     #$00
        STA     L01E6
        STA     L01E8
        RTS

; See "Principles of Interactive Computer Graphics", second edition
;     Newman & Sproull, pages 65-68

L0C21   LDY     #$0F
        JSR     L0D00           ;check bounds
        STA     ZPF4
        LDY     #$14
        JSR     L0D00           ;check bounds
        STA     ZPF9
@1      LDA     ZPF9
        ORA     ZPF4
        AND     #$0F
        BNE     @2
        CLC                     ;both endpoints are within the viewport
        RTS

@2      LDA     ZPF4
        AND     ZPF9
        AND     #$0F
        BEQ     @4
        SEC                     ;both endpoints are outside viewport and the
@3      RTS                     ; line does not enter viewport

@4      LDA     ZPF4            ;both endpoints outside, but check if the
        AND     #$0F             ; line crosses into the viewport
        BEQ     @5
        LDX     #$00
        LDY     #$0F
        JSR     L0D59           ;copy
        LDX     #$05
        LDY     #$14
        JSR     L0D59           ;copy
        JSR     @6              ;midpoint subdivision
        BCS     @3
        LDX     #$0F
        LDY     #$0A
        JSR     L0D59           ;copy
        BEQ     @1
@5      LDA     ZPF9
        AND     #$0F
        BEQ     @1
        LDX     #$00
        LDY     #$14
        JSR     L0D59           ;copy
        LDX     #$05
        LDY     #$0F
        JSR     L0D59
        JSR     @6
        BCS     @3
        LDX     #$14
        LDY     #$0A
        JMP     L0D59

;                          ----  Mid-Point Subdivision  ----
@6      LDA     ZPE5            ;border bits (from)
        EOR     ZPEA            ;            (to)
        AND     ZPE5
        ASL     A
        ASL     A
        ASL     A
        ASL     A               ;mul 16 (move lo nybble to hi)
        STA     ZPDF
        AND     ZPEA
        BEQ     @8              ;br to try it out
        LDA     ZPEA
        AND     #$0F
        BNE     @7              ;br if it's completely outside
        LDX     #$0A
        LDY     #$05
        CLC
        JMP     L0D59

@7      SEC                     ;completely outside viewport
        RTS

@8      STA     ZPDB
        STA     ZPDD

@9      ROL     ZPDB
        LDA     ZPE1            ;XPOS (from)
        ADC     ZPE6            ;XPOS (to)
        STA     ZPEB
        LDA     ZPE2            ;XPOS+1 (from)
        ADC     ZPE7            ;XPOS+1 (to)
        BVS     @10
        CMP     #$80
@10     ROR     A               ;div 2
        STA     ZPEC
        ROR     ZPEB            ;div 2
        ROR     ZPDB
        ROL     ZPDD
        LDA     ZPE3            ;YPOS (from)
        ADC     ZPE8            ;YPOS (to)
        STA     ZPED
        LDA     ZPE4            ;YPOS+1 (from)
        ADC     ZPE9            ;YPOS+1 (to)
        BVS     @11
        CMP     #$80
@11     ROR     A               ;div 2
        STA     ZPEE
        ROR     ZPED            ;div 2
        ROR     ZPDD
        LDY     #$0A
        JSR     L0D00           ;check if the midpoint is inside vport
        STA     ZPEF
        AND     ZPE5
        AND     #$0F
        BEQ     @13             ;br if inside viewport
        LDX     #$00
        JSR     L0D59
        LDA     ZPE5
        AND     ZPEA
        AND     #$0F
        BEQ     @9
@12     CLC                     ;completely inside viewport
        RTS

@13     LDA     ZPEF
        AND     ZPDF
        BNE     @12             ;all done
        LDX     #$05
        JSR     L0D59
        BEQ     @9              ;go around for another try

; CHECK VIEWPORT BOUNDS         inside   outside
;                         bit # 7 6 5 4  3 2 1 0
;         leaves flag in acc -> T B R L  T B R L

L0D00   LDA     #$00             ;check viewport bounds
        LDX     ZPE2,Y          ;XPOS+1
        BMI     @6              ; br if neg -- outside viewport
        CPX     VPORT_L+1
        BCC     @6              ; br if VPORT_L+1>XPOS+1
        BNE     @2              ; br if not equal
        LDX     ZPE1,Y          ;XPOS
        CPX     VPORT_L
        BCC     @6              ; br if VPORT_L>XPOS
        BEQ     @5              ; br if equal
@1      LDX     ZPE2,Y          ;XPOS+1
@2      CPX     VPORT_R+1
        BCC     @7              ; br if VPORT_R+1>XPOS+1
        BNE     @3              ; br if not equal
        LDX     ZPE1,Y          ;XPOS
        CPX     VPORT_R
        BCC     @7              ; br if VPORT_R>XPOS
        BEQ     @4              ; br if equal
@3      ORA     #BITON1         ;outside of the right edge
        BNE     @7              ; always
@4      ORA     #BITON5         ;inside of the right edge
        BNE     @7              ; always
@5      ORA     #BITON4         ;inside of the left edge
        BNE     @1              ; always -- go check right edge
@6      ORA     #BITON0         ;outside of the left edge
@7      LDX     ZPE4,Y          ;YPOS+1
        BMI     @12             ; br if neg -- outside
        BNE     @9              ; br if hi-byte is not zero -- outside
        LDX     ZPE3,Y          ;YPOS
        CPX     VPORT_B
        BCC     @12
        BEQ     @11
@8      CPX     VPORT_T
        BEQ     @10             ; br if equal
        BCC     @13             ; br if VPORT_T>YPOS
@9      ORA     #BITON3         ;above top edge (outside)
        RTS

@10     ORA     #BITON7         ;at top edge (inside)
        RTS

@11     ORA     #BITON6         ;at bottom edge (inside)
        BNE     @8              ; br always -- go check top edge
@12     ORA     #BITON2         ;below bottom edge (outside)
@13     RTS

L0D59   LDA     #$05
        STA     ZPE0
@1      LDA     ZPE1,Y
        STA     ZPE1,X
        INX
        INY
        DEC     ZPE0
        BNE     @1
        RTS

CALCBASE                ;Calculate base address -- ACC:=YPOS
        STA     ZPD9
        SEC
        LDA     #$BF
        SBC     ZPD9
        TAY
        LSR     A
        LSR     A
        LSR     A               ;div 8
        TAX
        TYA
        ASL     A
        ASL     A               ;mul 4
        AND     #$1C
        ORA     L01F3,X
        TAY
        ORA     L01B2
        STA     ZPCB
        TYA
        ORA     L01B3
        STA     ZPCD
        LDA     L020B,X
        STA     ZPCA
        STA     ZPCC
        STX     ZPE7
        RTS

CALCBYTE                ;Calculate byte offset -- ACC:=XPOS, Y_REG:=XPOS+1
        SEC
        BEQ     @3              ; br if XPOS+1=00
        DEY
        BEQ     @1
        LDY     #$48             ;gets here when X_HI=2
        ADC     #$01
        BCC     @2              ; br if XPOS<=255
@1      LDY     #$23             ;gets here when X_HI=1
        ADC     #$04
        BCC     @2
        INY
        SBC     #$07
@2      INY
@3      SBC     #$07             ; DIV 7 -- the old-fashioned way
        BCS     @2
        TAX                     ; remainder into X
        TYA
        LDY     COL_TAB+$35,X
        STY     ZPCF
        LDY     ZPD5
        CPY     #$02
        STY     ZPD2
        BCC     @4
        BEQ     @5
        ASL     A
        LDY     COL_TAB+$20,X
        STY     ZPD0
        LDY     COL_TAB+$27,X
        STY     ZPCF
        STX     ZPD1            ;remainder (in 2's complement form)
        CPX     #$FC
        ADC     #$00
@4      STA     ZPCE            ;byte offset
        RTS                     ; something get left in ACC

@5      LSR     A
        STA     ZPCE            ;byte offset
        ROR     ZPD2
        RTS

L0DD8   BPL     L0E16
        CLC
        LDA     ZPCB
        BIT     L025A
        BNE     L0E08
        ASL     ZPCA
        BCS     L0E00
        BIT     L0258
        BEQ     L0DF0
        ADC     #$1F
        SEC
        BCS     L0E02
L0DF0   ADC     #$23
        PHA
        LDA     ZPCA
        ADC     #$B0
        BCS     L0DFB
        ADC     #$F0
L0DFB   STA     ZPCA
        PLA
        BCS     L0E02
L0E00   ADC     #$1F
L0E02   ROR     ZPCA
        LDX     ZPCA
        STX     ZPCC
L0E08   ADC     #$FC
L0E0A   STA     ZPCB
        AND     #$1F
        STA     ZPE7
        ORA     L01B3
        STA     ZPCD
        RTS

L0E16   CLC
        LDA     ZPCB
        ADC     #$04
        BIT     L025A
        BNE     L0E0A
        ASL     ZPCA
        BCC     L0E3E
        ADC     #$E0
        CLC
        BIT     L0259
        BEQ     L0E40
        LDA     ZPCA
        ADC     #$50
        EOR     #$F0
        BEQ     L0E36
        EOR     #$F0
L0E36   STA     ZPCA
        LDA     ZPCB
        AND     #$E0
        BCC     L0E40
L0E3E   ADC     #$E0
L0E40   ROR     ZPCA
        LDX     ZPCA
        STX     ZPCC
        BCC     L0E0A
L0E48   BVS     L0E4D
        JMP     L0EA7

L0E4D   LDX     ZPD5
        CPX     #$02
        BEQ     L0E6B
        BCS     L0E84
        LSR     ZPCF
        BCC     L0E6A
        DEY
        BPL     L0E64
L0E5C   ROL     ZPCF
        INY
        STY     ZPCE
        RTS

L0E62   LDY     #$27
L0E64   LDA     #$40
        STA     ZPCF
        STY     ZPCE
L0E6A   RTS

L0E6B   LSR     ZPCF
        BCC     L0E6A
        LDA     #$40
        STA     ZPCF
        ASL     A
        EOR     ZPD2
        STA     ZPD2
        BPL     L0E6A
        DEY
        STY     ZPCE
        BPL     L0E6A
        LSR     ZPD2
        SEC
        BCS     L0E5C
L0E84   LDX     ZPD1
        CPX     #$FC
        BNE     L0E8B
        DEY
L0E8B   DEX
        CPX     #$F9
        BCS     L0E98
        LDX     #$FF
        DEY
        BPL     L0E98
        INY
        LDX     #$F9
L0E98   STY     ZPCE
        STX     ZPD1
        LDA     COL_TAB+$27,X
        STA     ZPCF
        LDA     COL_TAB+$20,X
        STA     ZPD0
        RTS

L0EA7   LDX     ZPD5
        CPX     #$02
        BEQ     @3
        BCS     @4
        ASL     ZPCF
        BPL     @2
        INY
        CPY     #$28
        BCC     @1
        JMP     L0E62

@1      LDA     #$01
        STA     ZPCF
        STY     ZPCE
@2      RTS

@3      ASL     ZPCF
        BPL     @2
        LDA     #$01
        STA     ZPCF
        LDA     #$FF
        EOR     ZPD2
        STA     ZPD2
        BMI     @2
        INY
        STY     ZPCE
        CPY     #$28
        BCC     @2
        ROR     ZPD2
        JMP     L0E62

@4      LDX     ZPD1
        CPX     #$FB
        BNE     @5
        INY
@5      INX
        BNE     @6
        LDX     #$F9
        INY
        CPY     #$28
        BCC     @6
        LDX     #$FF
        LDY     #$27
@6      STY     ZPCE
        STX     ZPD1
        LDA     COL_TAB+$20,X
        STA     ZPD0
        LDA     COL_TAB+$27,X
        STA     ZPCF
        RTS

L0F02   SEC
        LDA     ZPF5
        SBC     ZPF0
        PHA
        LDA     ZPF6
        SBC     ZPF1
        STA     ZPD9
        BCS     @1
        PLA
        EOR     #$FF
        ADC     #$01
        PHA
        LDA     #$00
        SBC     ZPD9
@1      STA     ZPDE
        PLA
        STA     ZPDD
        SEC
        LDA     ZPF7
        SBC     ZPF2
        BCS     @2
        EOR     #$FF
        ADC     #$01
@2      STA     ZPDB
        ROR     ZPD9
        LDY     ZPCE
        LDX     ZPDE
        BNE     @3
        CMP     ZPDD
        BCS     @7
@3      STX     ZPE2
        TXA
        LSR     A
        STA     ZPE4
        LDA     ZPDD
        BNE     @4
        DEC     ZPE2
@4      STA     ZPE1
        ROR     A
        STA     ZPE3
@5      LDA     ZPE3
        SEC
        SBC     ZPDB
        STA     ZPE3
        BCS     @6
        DEC     ZPE4
        BPL     @6
        ADC     ZPDD
        STA     ZPE3
        LDA     ZPE4
        ADC     ZPDE
        STA     ZPE4
        LDA     ZPD9
        JSR     L0DD8
@6      BIT     ZPD9
        JSR     L0E48
        JSR     L0FAA
        DEC     ZPE1
        BNE     @5
        DEC     ZPE2
        BPL     @5
        RTS

@7      STA     ZPE1
        TAX
        BEQ     @10
        LSR     A
        STA     ZPE4
@8      LDA     ZPE4
        SEC
        SBC     ZPDD
        STA     ZPE4
        BCS     @9
        ADC     ZPDB
        STA     ZPE4
        BIT     ZPD9
        JSR     L0E48
@9      LDA     ZPD9
        JSR     L0DD8
        JSR     L0FAA
        DEC     ZPE1
        BNE     @8
@10     RTS

L0F9D   LDA     ZPF2            ;JSR, JMP from LINETO
        JSR     CALCBASE        ;calc base addr
        LDA     ZPF0
        LDY     ZPF1
        JSR     CALCBYTE        ;calc byte offset
        TAY

L0FAA   BIT     ZPD7
        LDX     ZPD5
        DEX
        BEQ     @5
        LDA     L01E6
        ASL     A
        LDA     PN_COL
        BCC     @1
        LDA     F_COL
@1      CPX     #$02
        BNE     @2
        JMP     L1054

@2      BVC     @3
        JSR     L1199
@3      CMP     #$00
        BEQ     @4
        LDA     ZPCF
@4      JMP     L1081

@5      BIT     ZPE7
        BPL     @6
        CPY     ZPE5
        BEQ     @8
@6      LDA     F_COL
        STA     ZPDF
        LDA     PN_COL
        STA     ZPE0
        BIT     ZPD7
        BVC     @7
        LDA     L01E6
        PHA
        LDA     #$F0
        STA     L01E6
        SEC
        JSR     L11AB
        STA     ZPDF
        LDA     #$00
        STA     L01E6
        CLC
        JSR     L11AB
        STA     ZPE0
        PLA
        STA     L01E6
@7      LDA     ZPE0
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        ORA     ZPDF
        STA     L01E6+1
@8      LDX     ZPCF
        LDA     L01E6
        ASL     A
        LDA     #$F0
        BCC     @9
        LDA     #$0F
@9      STA     ZPCF
        AND     L01E6+1
        JSR     L10A8
        BIT     L01E8
        BPL     @11
        LDA     L01E6
        ASL     A
        LDA     #$0F
        BCC     @10
        LDA     #$F0
@10     STA     ZPCF
        LDA     ZPCF
        AND     L01E6+1
        JSR     L10A8
@11     STX     ZPCF
        LDA     (ZPCA),Y
        ORA     ZPCF
        LDX     L01E6
        BEQ     @12
        EOR     ZPCF
@12     STA     (ZPCA),Y
        LDA     #$F0
        STA     ZPE7
        STY     ZPE5
        RTS

L1054   BVC     @1
        JSR     L11B7
@1      JSR     L1148
        TAX
        LDA     ZPCF
        BEQ     @2
        LDA     ZPDF
        JSR     L1085
@2      LDA     ZPCF
        PHA
        LDA     ZPD0
        BEQ     @4
        STA     ZPCF
        CMP     #$60
        PHP
        BNE     @3
        DEY
@3      TXA
        JSR     L10A8
        PLP
        BNE     @4
        INY
@4      PLA
        STA     ZPCF
        RTS

L1081   BIT     ZPD2
        BMI     L10A8

L1085   BIT     ZPD7
        BPL     @1
        EOR     ZPCF
@1      BIT     ZPD6
        BMI     @5
        BVS     @4
        EOR     (ZPCA),Y
        AND     ZPCF
@2      EOR     (ZPCA),Y
@3      STA     (ZPCA),Y
        RTS

@4      ORA     (ZPCA),Y
        BVS     @3
@5      BVC     @2
        EOR     #$FF
        EOR     ZPCF
        AND     (ZPCA),Y
        BVS     @3

L10A8   BIT     ZPD7
        BPL     @1
        EOR     ZPCF
@1      BIT     ZPD6
        BMI     @5
        BVS     @4
        EOR     (ZPCC),Y
        AND     ZPCF
@2      EOR     (ZPCC),Y
@3      STA     (ZPCC),Y
        RTS

@4      ORA     (ZPCC),Y
        BVS     @3
@5      BVC     @2
        EOR     #$FF
        EOR     ZPCF
        AND     (ZPCC),Y
        BVS     @3

L10CB   LDY     ZPCE            ;read color at pen position
        LDX     ZPD5
        CPX     #$03
        BEQ     L10F6
        LDA     ZPCF
        BIT     ZPD2
        BMI     @1
        AND     (ZPCA),Y
        BCC     @2
@1      AND     (ZPCC),Y
@2      BEQ     @3
        LDA     #$0F
@3      CPX     #$01
        BEQ     @4
        RTS

@4      TAX
        LDA     (ZPCC),Y
        CPX     #$00
        BEQ     @5
        LSR     A
        LSR     A
        LSR     A
        LSR     A
@5      AND     #$0F
        RTS

L10F6   LDA     (ZPCA),Y
        AND     ZPCF
        STA     ZPDF
        LDA     ZPD0
        CMP     #$60
        PHP
        BNE     @1
        DEY
@1      AND     (ZPCC),Y
        PLP
        BNE     @2
        INY
@2      LDX     ZPD1
        INX
        BNE     @3
        LSR     A
        LSR     A
        LSR     A
        RTS

@3      INX
        BNE     @4
        ROL     ZPDF
        ROL     ZPDF
        ROL     A
        RTS

@4      INX
        BNE     @5
        LDA     ZPDF
        LSR     A
        LSR     A
        RTS

@5      INX
        BNE     @6
        ROL     A
        ROL     A
        ROL     ZPDF
        ROL     A
        ROL     ZPDF
        LDA     ZPDF
        RTS

@6      INX
        BNE     @7
        LSR     A
        RTS

@7      INX
        BNE     @8
        ASL     ZPDF
        ASL     ZPDF
        ROL     A
        ASL     ZPDF
        ROL     A
        ASL     ZPDF
        ROL     A
        RTS

@8      LDA     ZPDF
        RTS

L1148   LDX     ZPD1
        INX
        BNE     @1
        STX     ZPDF
        ASL     A
        ASL     A
        ASL     A
        RTS

@1      INX
        BNE     @2
        LSR     A
        TAX
        LDA     #$00
        ROR     A
        ROR     A
        STA     ZPDF
        TXA
        RTS

@2      INX
        BNE     @3
        ASL     A
        ASL     A
        STA     ZPDF
        TXA
        RTS

@3      INX
        BNE     @4
        STX     ZPDF
        LSR     A
        ROR     ZPDF
        LSR     A
        ROR     ZPDF
        LSR     ZPDF
        TAX
        LDA     ZPDF
        STX     ZPDF
        RTS

@4      INX
        BNE     @5
        ASL     A
        STX     ZPDF
        RTS

@5      INX
        BNE     @6
        STX     ZPDF
        LSR     A
        ROR     ZPDF
        LSR     A
        ROR     ZPDF
        LSR     A
        ROR     ZPDF
        LSR     ZPDF
        RTS

@6      STA     ZPDF
        LDA     #$00
        RTS

L1199   LDA     ZPCF
        BIT     ZPD2
        BMI     @1
        AND     (ZPCA),Y
        BPL     @2
@1      AND     (ZPCC),Y
@2      BEQ     L11BA
        LDA     #$0F
        BNE     L11BA
L11AB   LDA     (ZPCC),Y
        BCS     @1
        LSR     A
        LSR     A
        LSR     A
        LSR     A
@1      AND     #$0F
        BPL     L11BA
L11B7   JSR     L10F6
L11BA   STA     ZPDC
        LDA     L01E6
        BPL     @1
        LDA     F_COL
        JMP     @2

@1      LDA     PN_COL
@2      ASL     A
        ASL     A
        ASL     A
        LSR     ZPDC
        ORA     ZPDC
        TAX
        LDA     COL_TAB,X
        BCS     @3
        LSR     A
        LSR     A
        LSR     A
        LSR     A
@3      AND     #$0F
        RTS

        .endproc
