;cypherbasic 64
;
;load"cypherbasic",8,1:new:sys9*4096
;compile: ca65 -t c64 cypherbasic.s && ld65 -t c64 -o cypherbasic cypherbasic.o c64.lib
;with cc65 compiler/assembler package
;
;© 2007 Hanno Behrens (pebbles@schattenlauf.de)
;for http://www.forum64.de/wbb2/thread.php?postid=188445#post188445
	.setcpu "6502X"
	.macpack generic
	.macpack cbm
	.import SETLFS,SETNAM,OPEN,CLOSE,GET
	.import BASIN,BSOUT,CHKIN,CKOUT,CLRCH,READST

	.segment "STARTUP"
	
	.define VERSION "v0.9"
	SCREEN=$0400
	ST=$90
	k_scrout=$e716
	pointer=$fb		;+fc
	uintout=$bdcd		;Basic Positive Integerzahl ausgeben (a/x)
	K_CHKSTOP=$ffe1		;$f6ed Stop-Taste abfragen eq->stop
	basic_nmi_vec	= $a002			;Basic NMI-Vector
	
	; Direct entries
CLRSCR 		:= $E544
KBDREAD		:= $E5B4
NMIEXIT		:= $FEBC

; ---------------------------------------------------------------------------
; Processor Port at $01
PP		= $01

LORAM		= $01  		; Enable the basic rom
HIRAM		= $02  		; Enable the kernal rom
IOEN 		= $04  		; Enable I/O
CASSTOK_DATA	= $08  		; Cassette data
CASSPLAY	= $10  		; Cassette: Play
CASSMOT		= $20  		; Cassette motor on
TP_FAST		= $80  		; Switch Rossmoeller TurboProcess to fast mode

RAMONLY		= $F8  		; (~(LORAM | HIRAM | IOEN)) & $FF

; BASIC Erweiterungen
NMIVEK		=$300
TOKENVEK	=$304
LISTVEK		=$306
CMDVEK		=$308
FUNVEK		=$30a
PRINTVEK	=$326

PRINTORG	=$f1ca
Z_ACTIVEDEV	=$13
ADR		=$22
;STRPTR		=$33		;33/34 Zeiger auf Anfang des Strings
FAC1		=$61
FAC2		=$69
FAC3		=$57
FAC4		=$5c
RESSTRPTR	=$33		;nach B_GETSTR
NEWSTRLEN	=$61		;FAC#1,System
NEWSTRPTR	=$62
ARG1STRLEN	=FAC2		;FAC#2, Eigenbenutzung
ARG1STRPTR	=FAC2+1
ARG2STRLEN	=FAC3		;FAC#3, Eigenbenutzung
ARG2STRPTR	=FAC3+1
TMPSTRLEN	=$5c		;FAC#4,Eigenbenutzung
TMPSTRPTR	=$5d
LINENO		=$39
CHRGET		=$73
CHRGOT		=$79
TXTPTR		=CHRGOT+1
Z_SA		=$b9
Z_FA		=$ba
Z_FNADR		=$bb
Z_FNLEN		=$b7
TMPPTR		=$fb
STACK		=$100
PRERROR		=$a445		;Fehlerausgabe
WARMSTART	=$e386

; BASIC Tokens
TOK_DATA	=$83
TOK_INPUT	=$85
TOK_LET		=$88
TOK_REM		=$8f
TOK_PRINT	=$99
TOK_TO		=$a4
; Konstanten
CHAR		=8
COUNT		=11
TYPFLAG		=$0d		;0=Numerisch -1=String
QUOTFLG		=15
QUOTE		='"'
FLAG		=$0f		;hochkomma-Flag
BUFFER		=$200

;ICE Befehle
K_SENDNAM	=$f3d5
K_TALK		=$ffb4
K_SECTALK	=$ff96
K_IECIN		=$ffa5
K_CLSFIL	=$f642
STATUS		=$90

TABLE		=$a09e		;Tabelle der Befehlsworte

B_TESTSTACK	=$a3fb		;test Platz auf Stack
B_TESTNUM	=$ad8d		;Testet das Ergebnis auf NUM (Ende einer Func)
B_TESTSTR	=$ad8f		;Testet das Ergebnis auf STR (Ende einer Func)
B_FRMNUM	=$ad8a		;numerischen Ausdruck holen
B_INTER		=$a7ae		;Interpreterschleife
B_SYNTAX	=$af08		;Syntaxerror
B_NEXTSTAT	=$a906		;nächstes Statement suchen

K_MEMTOP		=$fe25		;clc->setzen sec->holen x/y

B_LNPRT		=$bdcd		;Basic Positive Integerzahl ausgeben (a/x)
;Meldungen ausgeben
K_SYSMSG		=$e422		;systemmeldung ausgeben
B_OVERFLOW	=$b97e
ERROR		=$a445
B_ERRORMSG	=$a43a		;Fehlermeldung in X 1..30

B_NMISTART	=$e37b
B_NMIORGVEK	=$e38b

;Basic Zeilen einlesen-Spezifisch
B_INDELLINE	=$a49c		;Löschen oder einfügen von Programmzeilen
B_ZNRADR	=$a96b		;Zeilennummer in Adressformat umwandeln $14/$15
B_TOKENLINE	=$a579		;Basic-Zeile in Interpretercode wandeln
B_FINDLINE	=$a613		;Adresse der Basiczeile berechnen
B_BINDLINES	=$a533
B_CLR		=$a659
B_MOVBLOCK	=$a3b8		;Block-Verschiebe-Routine

B_OUTBYTE	=$a6f3
B_OLDLIST	=$a6ef
B_LISTTOK	=$a724
B_EXECOLD	=$a7ed
B_END		=$a831
B_PRINT		=$aaa0
B_FRMEVL	=$ad9e		;holt beliebigen Term
B_HOLEAUSDRUCK	=$ae83		;z.B. STRING danach mit $ad8f prüfen
B_FUNKTOLD	=$ae8d
B_GETTERM	=$aef1		;holt einen gesamten Term in Klammern
B_CHKCLOSE	=$aef7		;Testet auf )
B_CHKOPEN	=$aefa		;Testet auf (
B_CHKKOMMA	=$aefd		;Testet auf Komma
B_CHKCHAR	=$aeff		;Testet auf Zeichen im A
B_CHKNUM	=$ad8d
B_CHKSTR	=$ad8f		;prüft ob der letzte geholte Ausdruck ($ae83) ein STR war
B_STROUT		=$ab1e		;A lo/Y hi pointer
B_CHAROUT		=$ab47
B_GETADR	=$b7f7		;nach $14/$15 Y=lo A=hi

B_ISCHAR	=$b113		;in A, Result C=1 wenn wahr (a..z)

B_BYTE2FLOAT	=$b3a2
B_MKSTR		=$b47d		;len in A, x/y bzw. $62/$63 Adresse, Akku/$61 Länge

B_RETSTR	=$b4ca		;aus $61/$62/$63 wird der String richtig fertig gemacht
B_RETBYTEY	=$b3a2		; Yreg zurückgeben
B_RETBYTEA	=$bc3c		; Akku zurückgeben (Nebeneffekte??)
B_RETWORD	=$b391		; Y/Akku zurückgeben
B_RETSTRTMP	=$b487		; Pointer A/Y als temporärern String zurückgeben. Nullbyte oder " ist Ende!

B_FRESTR	=$b6a3		;Rückgabe: Y/X hi/lo A=len
				;auch $b6aa? - B_FRMEVL Rückgabe aufbereiten ($22/$23??)$35/36 A=Len

B_GETSTRBYTE	=$b761		;numarg->A, strdesc->$50/$51
B_GETSTR	=$b782		;Obersten Stringdescripter -> $33/$34 Länge ->A+Y
B_GETSTR_NOCHK  =B_GETSTR+3	;wie oben ohne Stringcheck
B_GETBYTE	=$b79e		;???

B_GETKOMMABYTE	=$e200		; Byte ins XReg
B_GETBYTEX	=$e203		; Byte ins XReg
B_GETWORDKOMMABYTE=$b7eb	; Word nach $14/$15, Komma, Byte nach X holen (wie Poke)
B_GETNUM	=$ad8a		; Irgendwas numerisches in den FAC (siehe oben B_TESTNUM)

K_DIRECTMODE	=$ff90
;Rechnen
B_ADDZIFFER	=$bd7e		; addiert Ziffer zu FAC

K_FINDFN	=$f30f

JUMP		=$54		;JUMP Vector

SEARCHBYTE	=$07
Z_ACTIVEFN	=$13
GENPTR		=$14		;CBM Generic Pointer, Integer-Adrese, z.B. Zeilennummer
STARTBASIC	=$2b
STARTVAR	=$2d
STARTARR	=$2f
ENDARR		=$31
STARTSTR	=$33
BASICEND	=$37

STRDESC		=$50		;..$53

.macro  basicstart nr

;Basic Start
		.word *
		.org *-2
		.word :+
		.ifnblank nr
		   .word nr		; Zeilennummer
		.else
		   .word 0
		.endif
		.byte $9e		; SYS
		.asciiz .sprintf ("%d", *+7)
:		.word 0
.endmacro

;		basicstart 2007
		.org $9000
		.word *
		.org *-2
		
init:		ldx #<init
		ldy #>init
		clc
		jsr K_MEMTOP		;memtop setzen
		stx BASICEND
		sty BASICEND+1
		stx STARTSTR
		sty STARTSTR+1
		lda #<gettoken
		ldy #>gettoken
		sta TOKENVEK
		sty TOKENVEK+1
		lda #<listtoken
		ldy #>listtoken
		sta LISTVEK
		sty LISTVEK+1
		lda #<newcmd
		ldy #>newcmd
		sta CMDVEK
		sty CMDVEK+1
		lda #<newfun
		ldy #>newfun
		sta FUNVEK
		sty FUNVEK+1
			;NMI Handling deaktiv
			;lda #<nmi
			;ldy #>nmi
			;sta NMIVEK
			;sty NMIVEK+1
			;jmp B_NMISTART
		
		ldx #0
		jsr msgout
		;jsr B_CLR
		rts

;.proc nmi	;NMI-Einsprung
;		jmp B_NMIORGVEK
;.endproc

.proc msgout	;Fehlermeldung in X
		txa
		asl
		tax
		lda msg,x
		ldy msg+1,x
		jsr B_STROUT
		rts
		
version:	.byte 13,"cypherbasic 64 ",VERSION,13
		.asciiz "(c) 2007 h. behrens"
msg:		.word version
.endproc

.proc errormsg
		txa
		asl
		tax
		lda err,x
		sta ADR
		lda err+1,x
		jmp ERROR
err:		.word erstrlen,ernotfound,notimplemented,rotate
		.word whilewend,repeatuntil,toomanylines
erstrlen:	.byte "different string lengtH"			;0
ernotfound:	.byte "transposition char not founD"		;1
notimplemented:	.byte "not yet implementeD"			;2
rotate:		.byte "rotate not less than string lengtH"	;3
whilewend:	.byte "while without wenD"			;4
repeatuntil:	.byte "until without repeaT"			;5
toomanylines:	.byte "too many lineS"				;6
.endproc

.proc gettoken
PNT		=$71

		ldx TXTPTR
		ldy #4
		sty FLAG
nextchar:	lda BUFFER,x
		bpl normal
		cmp #$ff
		beq takchar
		inx
		bne nextchar
normal:		cmp #' '
		beq takchar
		sta CHAR
		cmp #QUOTE
		beq getchar
		bit FLAG
		bvs takchar
		cmp #'?'
		bne :+
		lda #TOK_PRINT
		bne takchar
:		cmp #'0'
		bcc :+
		cmp #'<'
		bcc takchar
:		sty PNT
		ldy #0
		sty COUNT
		dey
		stx TXTPTR
		dex
		
cmploop:	iny
		inx
testnext:	lda BUFFER,x
		sub TABLE,y
		beq cmploop
		cmp #$80
		bne nextcmd
		ora COUNT
takchar1:	ldy PNT

takchar:	inx
		iny
		sta BUFFER-5,y
;		lda BUFFER-5,y			;void
		cmp #0
		beq ende
		sub #':'
		beq :+
		cmp #TOK_DATA-':'
		bne :++
:		sta FLAG
:		sub #TOK_REM-':'
		bne nextchar
		sta CHAR
remloop:	lda BUFFER,x
		beq takchar
		cmp CHAR
		beq takchar
getchar:	iny
		sta BUFFER-5,y
		inx
		bne remloop
		
nextcmd:	ldx TXTPTR
		inc COUNT
weiter:		iny
		lda TABLE-1,y
		bpl weiter
		lda TABLE,y
		bne testnext
		beq newtok
		
notfound:	lda BUFFER,x
		bpl takchar1
		
ende: 		sta BUFFER-3,y

		dec TXTPTR+1
		lda #$ff
		sta TXTPTR
		rts
		
;neue Befehle verarbeiten
newtok:		ldy #0
		lda newtab,y
		bne newtest
		
newcmp:		iny
		inx
newtest:	lda BUFFER,x
		sub newtab,y
		beq newcmp
		cmp #$80
		bne nextnew
		ora COUNT
		bne takchar1
		
nextnew:	ldx TXTPTR
		inc COUNT
weiter1:	iny
		lda newtab-1,y
		bpl weiter1
		lda newtab,y
		bne newtest
		beq notfound
.endproc


.proc newcmd
		jsr CHRGET
		;cmp #TOK_PRINT
		;beq printext
		cmp #CMDSTART
		bcc oldcmd
		cmp #CMDEND+1
		bcc oknew
oldcmd:		jsr CHRGOT
		jsr B_EXECOLD
		jmp B_INTER		
		
oknew:		sub #CMDSTART
		asl
		tax
		lda cmdtab+1,x
		pha
		lda cmdtab,x
		pha
		jmp CHRGET
		
;Erweiterung BLOCK für print
;printext:	jsr CHRGET
;		cmp #TOK_BLOCK
;		beq blockprint
;		bne exit
;blockprint:	lda #'a'
;		jsr BSOUT
;		jsr CHRGET
;exit:		jsr B_PRINT
;		jmp B_INTER
.endproc

.proc newfun
		lda #0
		sta TYPFLAG
		jsr CHRGET
		cmp #'$'		;Hexzahlen auswerten
		beq hexzahl
		cmp #'%'
		beq binzahl

		cmp #FUNSTART
		bcc oldfun
		cmp #FUNEND+1
		bcc oknew
oldfun:		jsr CHRGOT
		jmp B_FUNKTOLD

;Erweiterungen
hexzahl:	jmp gethex
binzahl:	jmp getbin
;Erweiterte Token-Funktionen
oknew:		sub #FUNSTART
		asl
		pha
		jsr CHRGET
		jsr B_CHKOPEN		;Offene Klammer holen
		pla
		tay
		lda funtab,y
		sta JUMP+1
		lda funtab+1,y
		sta JUMP+2
		jmp JUMP
		;rts
.endproc

	
.proc listtoken
PNT		=$49
		bpl out
		bit QUOTFLG
		bmi out
		cmp #$ff
		beq out
		cmp #$cc
		bcs newlist
		jmp B_LISTTOK
out:		jmp B_OUTBYTE
		
newlist:	sub #$cb		;offset abziehen
		tax
		sty PNT
		ldy #-1
next:		dex
		beq found
loop:		iny
		lda newtab,y
		bpl loop
		bmi next
		
found:		iny
		lda newtab,y
		bmi oldend
		jsr B_CHAROUT
		bne found
		
oldend:		jmp B_OLDLIST
.endproc

;**************** UTILITIES ************************
.proc clrfac	;Fließkommabereich löschen
		lda #0
		ldx #10
loop:		sta FAC4,x
		dex
		bpl loop
		rts
.endproc

.proc	hexout
		pha		;Accu und X retten
		txa
		pha
		tsx
		lda STACK+2,x
		lsr a
		lsr a
		lsr a
		lsr a
		tax
		lda hexnum,x
		jsr BSOUT
		tsx
		lda STACK+2,x
		and #$0f
		tax
		lda hexnum,x
		jsr BSOUT
		pla		;Accu und X wiederherstellen
		tax
		pla
		rts
hexnum:		.byte "0123456789abcdef"
.endproc


;**************** BEFEHLE **************************

.proc repeat
		lda #3
		jsr B_TESTSTACK
		jsr B_NEXTSTAT
		tya
		add TXTPTR
		pha
		lda TXTPTR+1
		adc #0
		pha
		lda LINENO
		pha
		LDA LINENO+1
		pha
		lda #'r'
		pha
		jmp B_INTER
.endproc

.proc until
		bne noerr
		jmp B_SYNTAX
noerr:		jsr B_FRMNUM
		tay
		tsx
		lda STACK+1,x
		cmp #'r'
		bne rpterr
		tya
		bne rptende
		
		lda STACK+2,x
		sta LINENO+1
		lda STACK+3,x
		sta LINENO
		lda STACK+4,x
		sta TXTPTR+1
		lda STACK+5,x
		sta TXTPTR
		jmp B_INTER
		
rptende:	txa
		axs #-5		;add #5; tax
		txs
		jmp B_INTER
		
rpterr:		ldx #5
		jmp errormsg
.endproc

.proc do
		lda #3
		jsr B_TESTSTACK
		jsr B_NEXTSTAT
		tya
		add TXTPTR
		pha
		lda TXTPTR+1
		adc #0
		pha
		lda LINENO
		pha
		LDA LINENO+1
		pha
		lda #'w'
		pha
		jmp B_INTER
.endproc

.proc while
		bne noerr
		jmp B_SYNTAX
noerr:		jsr B_FRMNUM
		tay
		tsx
		lda STACK+1,x
		cmp #'w'
		bne rpterr
		tya
		beq rptende
		
		lda STACK+2,x
		sta LINENO+1
		lda STACK+3,x
		sta LINENO
		lda STACK+4,x
		sta TXTPTR+1
		lda STACK+5,x
		sta TXTPTR
		jmp B_INTER
		
rptende:	txa
		axs #-5		;add #5; tax
		txs
		jmp B_INTER
		
rpterr:		ldx #4
		jmp errormsg
.endproc


;******* Beginn der Cypher-Programme ********
.proc cypher
		;Stringvariable holen
		jsr B_FRMEVL		; String auf Descriptorstack $61,62,63
		jsr B_CHKSTR		;Ausdruck auch ein String?
		jsr B_FRESTR		;hi y, lo x, len a
		pha			;len
		tya
		pha			;hi adr
		txa
		pha			;lo adr
		
		jsr CHRGOT
		;cmp #TOK_SUBST
		;bne :+
		;beq subst_para
		;Ende der Syntax -> Syntax Error
		ldx #11			;Syntax Error
		jmp B_ERRORMSG

.endproc



;******* Ende der Cypher-Programme **********
.proc block
		jsr B_GETBYTE		;a=chrgot(+flags) x=result ($65)
		bne parameter
weiter:		cpx #0
		beq off
		stx charmax
		sei
		lda #<blockprint
		ldy #>blockprint
		sta PRINTVEK
		sty PRINTVEK+1
		lda #0
		ldx Z_ACTIVEFN
		inx
:		dex
		bmi :+
		sta charnum,x
		sta blocknum,x
		bpl :-
:		cli
exit:		jmp B_INTER

parameter:	txa
		pha
		jsr B_CHKKOMMA
		jsr B_GETBYTE
		stx blockmax
		pla
		tax
		jmp weiter
		
off:		sei
		lda #<PRINTORG
		ldy #>PRINTORG
		sta PRINTVEK
		sty PRINTVEK+1
		cli
		bne exit
.endproc

fnindex:	.byte 0
charnum:	.res 11,0
charmax:	.byte 5
blocknum:	.res 11,0
blockmax:	.byte 5
; BSOUT Erweiterung auf 5er-Prints
.proc blockprint
		pha
		and #$7f
		cmp #' '
		bcc noprintable
		;chartest
		txa 
		pha			;x retten
		ldx Z_ACTIVEFN
		jsr K_FINDFN
		inx
		stx fnindex
		lda charnum,x
		cmp charmax
		bcc noblock
		lda #' '
		jsr PRINTORG
		ldx fnindex
		inc blocknum,x
		lda #0 
		sta charnum,x
		;blocktest
		lda blocknum,x
		cmp blockmax
		bcc noblock
		lda #0
		sta blocknum,x
		lda #13			;CR
		jsr PRINTORG		;Neue Zeile
		bit Z_ACTIVEFN
		bpl noblock
		lda #$0a
		jsr PRINTORG		;bei >=128 LF
noblock:	ldx fnindex
		inc charnum,x
		pla			;x wiederherstellen
		tax
		pla
		jmp PRINTORG
		
noprintable:	pla
		rts

.endproc

.proc old
		ldy #$01		;Basic-Old
		tya			;New-Befehl rückgängig machen
		sta (STARTBASIC),Y	;Basic-Anfang $0801+1
		jsr B_BINDLINES	;Basic-Zeilen neu binden
		txa			;Ende low in x nach A
		adc #$02		;plus 2
		sta STARTVAR		;bei Basic-PRG-Ende speichern
		lda $23			;Ende High
		adc #$00		;plus eventuellen Überlauf
		sta STARTVAR+1		;nach Basic Ende
		jmp B_INTER
.endproc


.proc off
		ldx #<$a000
		ldy #>$a000
		clc
		jsr K_MEMTOP		;memtop setzen
		stx BASICEND
		sty BASICEND+1
		stx STARTSTR
		sty STARTSTR+1
		lda #<$a57c
		ldy #>$a57c
		sta TOKENVEK
		sty TOKENVEK+1
		lda #<$a71a
		ldy #>$a71a
		sta LISTVEK
		sty LISTVEK+1
		lda #<$a7e4
		ldy #>$a7e4
		sta CMDVEK
		sty CMDVEK+1
		lda #<$ae86
		ldy #>$ae86
		sta FUNVEK
		sty FUNVEK+1
		lda #<B_NMIORGVEK
		ldy #>B_NMIORGVEK
		;sta NMIVEK
		;sty NMIVEK+1
		;jmp (NMIVEK)
		jmp B_INTER
.endproc

;********** RENUMBER **************

.proc renumber
exit:		jsr B_CLR
		jsr changeline
		jsr B_CLR
noinput:	clc
		jsr B_END
		jmp B_INTER
.endproc

.proc changeline		;von $a49c
		lda #0
		sta $7a
		lda #2
		sta $7b
		jsr CHRGOT
		jsr B_ZNRADR	;Zeilennr in Adresse $14$15
		jsr B_TOKENLINE	;Zeile in Tokens wandeln
		sty $0b		;Zeiger in Eingabepuffer
		jsr B_FINDLINE	;Adresse in Basic-Zeile berechnen
		bcc skipdel
		jsr delline
skipdel:	jsr insertline
		rts
.endproc

.proc delline			;von $a4a9
		ldy #1
		lda ($5f),y
		sta $23
		lda $2d
		sta $22
		lda $60
		sta $25
		lda $5f
		dey
		sbc ($5f),y
		clc
		adc $2d
		sta $2d
		sta $24
		lda $2e
		adc #$ff
		sta $2e
		sbc $60
		tax
		sec
		lda $5f
		sbc $2d
		tay
		bcs skip
		inx
		dec $25
skip:		clc
		adc $22
		bcc loop
		dec $23
		clc
loop:		lda ($22),y
		sta ($24),y
		iny
		bne loop
		inc $23
		inc $25
		dex
		bne loop
		rts
.endproc

.proc insertline
		;jsr B_CLR
		jsr B_BINDLINES
		lda $200
		beq exit
		clc
		lda $2d
		sta $5a
		adc $0b
		sta $58
		ldy $2e
		sty $5b
		bcc skip
		iny
skip:		sty $59
		jsr B_MOVBLOCK	;Zeile verschieben
		lda $14
		ldy $15
		sta $1fe
		sty $1ff
		lda $31
		ldy $32
		sta $2d
		sty $2e
		ldy $0b
		dey
loop:		lda $1fc,y
		sta ($5f),y
		dey
		bpl loop
		;jsr B_CLR
		jsr B_BINDLINES
exit:		rts	
.endproc

.proc linetobuf
		lda #0
		rts
.endproc

; renumber.828 - renumber BASIC program lines for C64, in 6502 Assembler
; author Neil Franklin, last modification unknown (estimated 1986)
;  this disassembly/source reconstruction 2000.06.07
; renumber BASIC program,  RENUM ff,nn,ss (ff = from, nn = new, ss = step)

.proc renum
		;jsr B_CHKKOMMA	; BASIC check for comma
		jsr B_GETNUM   	; BASIC get numeric argument
		jsr B_GETADR	; BASIC convert to address
		sty from   	; set "from"
		sta from+1

		jsr B_CHKKOMMA	; BASIC
		jsr B_GETNUM
		jsr B_GETADR
		sty new   	; set "new"
		sta new+1

		jsr B_CHKKOMMA	; BASIC
		jsr B_GETNUM
		jsr B_GETADR
		sty step   	; set "step"
		sta step+1
		
		jsr B_CLR	;Variablen löschen
		
		lda STARTBASIC+1	; begin BASIC program HI
		sta GENPTR+1
		ldx STARTBASIC     	; begin BASIC program LO
		dex
		stx GENPTR     	; CBM use for generic pointer
		inx
		bne loop
		dec GENPTR+1     	; set to begin BASIC - 1
loop:  		ldy #$00    	; chech for line begin
		lda (GENPTR),Y

		beq getnxtln   	; 0 = end byte of prev line
		clc
		lda GENPTR
		adc #$01
		sta GENPTR
		bcc skip1
		inc GENPTR+1
skip1:  	jmp loop   	; test for next byte
getnxtln:  	iny  		; get "next line" pointer
		lda (GENPTR),Y
		bne twoinc   	; n l pointer = 0 0, is end
		iny
		lda (GENPTR),Y
		bne oneinc
		jsr B_END
		jsr B_CLR
		jmp B_INTER
		;rts

twoinc:  	iny
oneinc:  	iny  		; after ptr = line number
		lda (GENPTR),Y	; compare ($14,Y) with "from"
		sub from
		iny
		lda (GENPTR),Y
		sbc from+1
		bcc norenum   	; not yet at lines to renumb
		
		lda new+1	; check if line is beyond 64000
		cmp #$FA    	; new >= $FA00 = 64000
		bcs message  	; too many lines, error abort
		
		;renumber line
		jsr savelnref	;store linereference at end of mem ($33)
		sta (GENPTR),Y 	; renumber HI
		dey
		lda new
		sta (GENPTR),Y 	; renumber LO
		add step   	; add step to new
		sta new
		lda new+1
		adc step+1
		sta new+1
		;end renumber line
		
norenum:  	lda GENPTR     	; jump over line header
		add #$06
		sta GENPTR
		bcc nextln
		inc GENPTR+1
nextln:  	jmp loop    	; look for next line

message:  	ldx #6    	;message "too many lines"
		jmp errormsg	; print error msg at ($22)
.endproc

;variables for renumbering lines
from:  		.word 0    	; from
new:  		.word 0    	; new
step:  		.word 0    	; step

;Save linereferences new->old on top of memory
.proc savelnref
		pha
		tya
		pha
		
		lda STARTSTR	;make four bytes place for saving two integers
		sub #4
		sta STARTSTR
		lda STARTSTR+1
		sbc #0
		sta STARTSTR+1
		
		lda (GENPTR),y	;Save original at 2,3
		dey
		sta (STARTSTR),y
		 
		lda (GENPTR),y
		dey
		sta (STARTSTR),y
		lda new+1	;Save new at 0,1
		dey
		sta (STARTSTR),y
		lda new
		dey
		sta (STARTSTR),y
		
		pla
		tay
		pla
		rts
.endproc
;********** ENDE RENUMBER **************

.proc dir
		beq nopara
		jsr B_GETBYTEX
		.byte $2c
nopara:		ldx #8
		jsr directory
		jmp B_INTER
.endproc

.proc directory
		lda #'$'
		sta TMPPTR
		lda #<TMPPTR
		ldy #>TMPPTR
		sta Z_FNADR
		sty Z_FNADR+1
		lda #1
		sta Z_FNLEN
		;lda #8
		stx Z_FA
		lda #$60
		sta Z_SA
		jsr K_SENDNAM
		lda Z_FA
		jsr K_TALK
		lda Z_SA
		jsr K_SECTALK
		lda #0
		sta STATUS
		ldy #3
l1:		sty TMPPTR
		jsr K_CHKSTOP
		beq l4
		jsr K_IECIN
		sta TMPPTR+1
		ldy STATUS
		bne l4
		jsr K_IECIN
		ldy STATUS
		bne l4
		ldy TMPPTR
		dey
		bne l1
		ldx TMPPTR+1
		jsr B_LNPRT
		lda #' '
		jsr BSOUT
l3:		jsr K_IECIN
		ldx STATUS
		bne l4
		tax
		beq l2
		jsr BSOUT
		jmp l3
l2:		lda #13
		jsr BSOUT
		ldy #2
		bne l1
l4:		jsr K_CLSFIL
		rts
.endproc

.proc ver
		ldx #0
		jsr msgout
		jmp B_INTER
.endproc
.proc notimp
		ldx #1
		jsr errormsg
		jmp B_INTER
.endproc
;**************** Funktionen *********************
.proc gethex	;holt eine Hexzahl
		jsr clrfac
getnext:	jsr CHRGET
		bcc ziffer
		cmp #'a'
		bcc end
		cmp #'f'+1
		bcs end
		adc #-7		;sub 7 Offset korrigieren, set carry
ziffer:		sub #'0'
		pha
		lda FAC1
		beq nochnull
		add #4
		bcs over	;Überlauf?
		sta FAC1
nochnull:	pla
		beq getnext
		jsr B_ADDZIFFER	;Ziffer zu FAC addieren
		jmp getnext
end:		jmp CHRGOT
over:		jmp B_OVERFLOW
.endproc

.proc getbin
		jsr clrfac
getbin:		jsr CHRGET
		cmp #'2'
		bcs end
		cmp #'0'
		bcc end
		sbc #'0'
		pha
		lda FAC1
		beq null
		inc FAC1
		beq over
null:		pla
		beq getbin
		jsr B_ADDZIFFER
		jmp getbin
end:		jmp CHRGOT
over:		jmp B_OVERFLOW
.endproc


.proc getnextstr
		;jsr CHRGET
		jsr B_FRMEVL		; String auf Descriptorstack $61,62,63
		jsr B_CHKSTR		;Ausdruck auch ein String?
		jsr B_FRESTR		;hi y, lo x, len a
		rts
.endproc

.proc transpose
		;Transposition Argumente auf Stack nach 3,4,5 von 6,7,8 result 9,10,11 
		jsr getnextstr
		pha
		tya
		pha
		txa
		pha

		jsr B_CHKKOMMA

		jsr getnextstr
		pha
		tya
		pha
		txa
		pha

		jsr B_CHKKOMMA

		jsr getnextstr
		pha
		tya
		pha
		txa
		pha

		jsr B_CHKCLOSE		; )?

		tsx
		lda STACK+9,x
		tax		
		jsr B_MKSTR		;macht einen neuen STR bei $62/$63, Len $61 (NEWSTR)

		;Check ob ARG1LEN=ARG2LEN
		tsx
		lda STACK+3,x
		cmp STACK+6,x
		bne error
		cmp STACK+9,x
		bne error		;Alle 3 Strings müssen gleich lang sein

		lda STACK+1,x
		sta ARG2STRPTR
		lda STACK+2,x
		sta ARG2STRPTR+1
		lda STACK+3,x
		sta ARG2STRLEN
		
		lda STACK+4,x
		sta ARG1STRPTR
		lda STACK+5,x
		sta ARG1STRPTR+1
		lda STACK+6,x
		sta ARG1STRLEN

		lda STACK+7,x
		sta TMPSTRPTR
		lda STACK+8,x
		sta TMPSTRPTR+1
		lda STACK+9,x
		sta TMPSTRLEN
				
		ldy TMPSTRLEN
loop:		beq exit
		dey
		lda (TMPSTRPTR),y
		tax			;Zeichen in X merken
		lda (ARG1STRPTR),y	;Position 1 Suchzeichen von Tabelle 1
		sty TMPSTRPTR+2		;merken
		
		ldy ARG2STRLEN		;in Tabelle 2 suchen
search:		dey
		cpy #-1
		beq notfound
		cmp (ARG2STRPTR),y
		bne search
		txa
		sta (NEWSTRPTR),y	;Transposition ausführen

		ldy TMPSTRPTR+2
		bne loop

exit:		tsx
		txa
		axs #-9		;add #5; tax
		txs
		jmp B_RETSTR
		
error:		ldx #0
		.byte $2c	;bit $xxxx überspringt nächten Befehl
notfound:	ldx #1
		jmp errormsg
.endproc	;transpose


.proc subst
		;Substitution Argumente auf Stack nach 3,4,5 von 6,7,8 result 9,10,11 
		jsr getnextstr
		pha
		tya
		pha
		txa
		pha

		jsr B_CHKKOMMA

		jsr getnextstr
		pha
		tya
		pha
		txa
		pha

		jsr B_CHKKOMMA

		jsr getnextstr
		pha
		tya
		pha
		txa
		pha

		jsr B_CHKCLOSE		; )?

		tsx
		lda STACK+9,x
		tax		
		jsr B_MKSTR		;macht einen neuen STR bei $62/$63, Len $61 (NEWSTR)

		;Check ob ARG1LEN=ARG2LEN
		tsx
		lda STACK+3,x
		cmp STACK+6,x
		bne error

		lda STACK+1,x
		sta ARG2STRPTR
		lda STACK+2,x
		sta ARG2STRPTR+1
		lda STACK+3,x
		sta ARG2STRLEN
		
		lda STACK+4,x
		sta ARG1STRPTR
		lda STACK+5,x
		sta ARG1STRPTR+1
		lda STACK+6,x
		sta ARG1STRLEN

		lda STACK+7,x
		sta TMPSTRPTR
		lda STACK+8,x
		sta TMPSTRPTR+1
		lda STACK+9,x
		sta TMPSTRLEN
				
		ldy TMPSTRLEN
loop:		beq exit
		dey
		lda (TMPSTRPTR),y
		sty TMPSTRPTR+2		;merken
		
		ldy ARG1STRLEN
search:		dey
		cpy #-1
		beq notfound
		cmp (ARG1STRPTR),y
		bne search
		
		lda (ARG2STRPTR),y	;substitute

		ldy TMPSTRPTR+2		;restore Index
		sta (NEWSTRPTR),y
notfound:	ldy TMPSTRPTR+2		;restore Index
		bne loop

exit:		tsx
		txa
		axs #-9		;add #5; tax
		txs
		jmp B_RETSTR
		
error:		ldx #0
		jmp errormsg
.endproc	;subst





.proc reverse	;Invertiert den String, der bei NEWSTR steht
		lda NEWSTRPTR
		add NEWSTRLEN
		sta TMPSTRPTR
		lda NEWSTRPTR+1
		adc #-1
		sta TMPSTRPTR+1

		lda NEWSTRLEN
		lsr 			;Länge durch 2 teilen
		tay			;und bei X merken
		
loop:		dey
		bmi exit
		lda (NEWSTRPTR),y
		tax
		tya
		eor #$ff
		tay
		lda (TMPSTRPTR),y
		pha
		txa
		sta (TMPSTRPTR),y
		tya
		eor #$ff
		tay
		pla
		sta (NEWSTRPTR),y
		jmp loop
		
exit:		rts
.endproc

.proc rotate
		jsr getnextstr
		pha
		tya
		pha
		txa
		pha
		;---- bis hierher Kopie von filter
		jsr B_CHKKOMMA
		pha			;merken ob reverse
		cmp #$ab		;"-" Reverse?
		bne :+
		jsr CHRGET		;Zeichen erstmal überlesen
				
:		jsr B_GETBYTEX
		;jsr B_GETKOMMABYTE	;hole zweiten Parameter (a sowie x)
		stx SEARCHBYTE
					;Rotationsweite
		jsr B_CHKCLOSE		; )?
		
		tsx
		lda STACK+4,x		;Stringlänge für neuen String vom Stack
		
		cmp SEARCHBYTE		;Rotation muss kleiner sein als Stringlänge
		bcc error		;wenn größer oder gleich -> Error
		beq error
		
		tax		
		jsr B_MKSTR		;macht einen neuen STR bei $62/$63, Len $61 (NEWSTR)

		tsx			;Originalstring nach TMPSTR
		lda STACK+4,x
		sta TMPSTRLEN
		lda STACK+3,x
		sta TMPSTRPTR+1	
		lda STACK+2,x
		sta TMPSTRPTR
		
		lda NEWSTRPTR		;Zielpointer berechnen
		add SEARCHBYTE		;Vom Pointer die Rotation addieren
		sta ARG1STRPTR
		lda NEWSTRPTR+1
		adc #0
		sta ARG1STRPTR+1
		
		lda SEARCHBYTE
		sub TMPSTRLEN
		tax			;Lauflänge vor Adresshochschaltung
		ldy #0
loop:		cpy TMPSTRLEN		;Für alle Zeichen des Strings
		beq endl
		lda (TMPSTRPTR),y
		sta (ARG1STRPTR),y
		iny
		inx
		bne loop		;Zieladresse runterschalten?
		lda ARG1STRPTR
		sub TMPSTRLEN
		sta ARG1STRPTR
		lda ARG1STRPTR+1
		sbc #0
		sta ARG1STRPTR+1
		bcs loop

endl:		pla			;soll reverse aktiviert werden?
		cmp #$ab		;"-" das Reverseflag
		bne :+
		jsr reverse
:		tsx
		txa
		axs #-3
		txs
		jmp B_RETSTR
		
error:		ldx #3
		jmp errormsg
.endproc	;rotate

.proc filter	;(STR,BITS)
		jsr getnextstr
		pha
		tya
		pha
		txa
		pha
		
		jsr B_GETKOMMABYTE	;hole zweiten Parameter (a sowie x)
		stx SEARCHBYTE		;filterbyte
		jsr B_CHKCLOSE		; )?
		
		tsx
		lda STACK+3,x
		sta TMPSTRLEN
		lda STACK+2,x
		sta TMPSTRPTR+1
		lda STACK+1,x
		sta TMPSTRPTR
		
		ldx #0
		ldy TMPSTRLEN
		
loop:		dey
		cpy #-1
		beq endl
		jsr isfilter
		bcc loop		;not in filter
		inx
		bne loop		;x=neue Länge
endl:		txa			;x->a ist neue Länge
		jsr B_MKSTR		;macht einen neuen STR bei $62/$63, Len $61 (NEWSTR)
		tsx
		lda STACK+3,x
		sta TMPSTRLEN
		lda STACK+2,x
		sta TMPSTRPTR+1
		lda STACK+1,x
		sta TMPSTRPTR
		
		ldx NEWSTRLEN
		ldy TMPSTRLEN
copy:		dey
		cpy #-1
		beq endcp		;exit unterhalb Schleife
		jsr isfilter
		bcc copy		;not in filter
		dex
		sty TMPSTRPTR+2		;sichern x und y
		stx TMPSTRPTR+3
		ldy TMPSTRPTR+3		;x zu y transferieren
		sta (NEWSTRPTR),y
		ldy TMPSTRPTR+2		;retten
		ldx TMPSTRPTR+3
		bcs copy		;x=neue Länge

endcp:		tsx
		txa
		axs #-3
		txs
		jmp B_RETSTR
		
nullstr:	beq endcp
.endproc

;Sucht duch String in STRPTR,y nach Flags in SEARCH
.proc isfilter	;Übergabe Y und gesetzter Pointer auf STRPTR, SEARCHBYTE ist das gesuchte Flag
		txa
		pha			;x sichern
		lda SEARCHBYTE		;Bitlöschungen vornehmen
		eor #%11100000
		ora #%00011111
		and (TMPSTRPTR),y
		tax			;Wert für die Dauer der Abfrage in X halten
		
		lda SEARCHBYTE
		and #%00010000
		beq :+
		txa
		jsr checkdouble
		bcc exit

:		lda SEARCHBYTE
		and #%00000001		;ASC
		beq :+
		txa
		jsr B_ISCHAR
		bcs exit
		
:		lda SEARCHBYTE
		and #%00000010		;NUM
		beq :+
		txa
		jsr isnum
		bcs exit

:		lda SEARCHBYTE
		and #%00000100		;SPC
		beq :+
		txa
		jsr isspc
		bcs exit
		
:		lda SEARCHBYTE
		and #%00001000		;CAPITAL
		beq exit
		txa
		jsr iscapital
		;bcs exit
		
exit:		
		pla
		tax
		lda SEARCHBYTE
		eor #%11100000
		ora #%00011111
		and (TMPSTRPTR),y
		rts
.endproc

.proc checkdouble	;Carry set if found double
		tya
		pha
		txa
		pha
		tsx
loop:		dey
		cpy #-1
		beq notfound
		lda SEARCHBYTE		;Bitlöschungen vornehmen
		eor #%11100000
		ora #%00011111
		and (TMPSTRPTR),y
		cmp STACK+1,x
		beq found		;Carry set
		bne loop
found:		clc
notfound:	.byte $24		;BIT $xx nächten Befehl überspringen
		sec
		pla
		tax
		pla
		tay
		rts
.endproc

.proc isnum
		cmp #'0'
		bcc :+
		sbc #'9'+1
		sub #-('9'+1)
:		rts
.endproc

.proc iscapital
		eor #$80
		jmp B_ISCHAR
.endproc

.proc isspc
		cmp #' '
		bcc :+
		sbc #'/'+1
		sub #-('/'+1)
		bcs exit
:		cmp #':'
		bcc exit
		sbc #'@'+1
		sub #-('@'+1)
exit:		rts
.endproc

;******* ROTOR$ ************* rotor$( dim ro$(rotor,mitnehmer),"0123","R","AAAA")
.proc rotor
		jsr getnextstr
		pha
		tya
		pha
		txa
		pha
		;---- bis hierher Kopie von filter
		jsr B_CHKKOMMA
		pha			;merken ob reverse
		cmp #$ab		;"-" Reverse?
		bne :+
		jsr CHRGET		;Zeichen erstmal überlesen
				
:		jsr B_GETBYTEX
		;jsr B_GETKOMMABYTE	;hole zweiten Parameter (a sowie x)
		stx SEARCHBYTE
					;Rotationsweite
		jsr B_CHKCLOSE		; )?
		
		tsx
		lda STACK+4,x		;Stringlänge für neuen String vom Stack
		
		cmp SEARCHBYTE		;Rotation muss kleiner sein als Stringlänge
		bcc error		;wenn größer oder gleich -> Error
		beq error
		
		tax		
		jsr B_MKSTR		;macht einen neuen STR bei $62/$63, Len $61 (NEWSTR)

		tsx			;Originalstring nach TMPSTR
		lda STACK+4,x
		sta TMPSTRLEN
		lda STACK+3,x
		sta TMPSTRPTR+1	
		lda STACK+2,x
		sta TMPSTRPTR
		
		lda NEWSTRPTR		;Zielpointer berechnen
		add SEARCHBYTE		;Vom Pointer die Rotation addieren
		sta ARG1STRPTR
		lda NEWSTRPTR+1
		adc #0
		sta ARG1STRPTR+1
		
		lda SEARCHBYTE
		sub TMPSTRLEN
		tax			;Lauflänge vor Adresshochschaltung
		ldy #0
loop:		cpy TMPSTRLEN		;Für alle Zeichen des Strings
		beq endl
		lda (TMPSTRPTR),y
		sta (ARG1STRPTR),y
		iny
		inx
		bne loop		;Zieladresse runterschalten?
		lda ARG1STRPTR
		sub TMPSTRLEN
		sta ARG1STRPTR
		lda ARG1STRPTR+1
		sbc #0
		sta ARG1STRPTR+1
		bcs loop

endl:		pla			;soll reverse aktiviert werden?
		cmp #$ab		;"-" das Reverseflag
		bne :+
		jsr reverse
:		tsx
		txa
		axs #-3
		txs
		jmp B_RETSTR
		
error:		ldx #3
		jmp errormsg
.endproc	;rotor


;******* END ROTOR$ *************
;**************** TABELLEN ********************
cmdtab:		.word repeat-1,until-1,while-1,do-1
		.word notimp-1,notimp-1,ver-1,block-1,ver-1
		.word ver-1,ver-1,ver-1,ver-1,ver-1
		.word ver-1,ver-1,dir-1,renum-1,old-1
		.word off-1, ver-1
funtab:		.word filter, rotate, subst,transpose, rotor


CMDSTART	=$CC		;erstes freies Token
CMDEND		=CMDSTART+20	;letztes freies Token
FUNSTART	=CMDEND+1
FUNEND		=$FE

TOK_BLOCK	=$d3
TOK_TRANS	=$dc
TOK_SUBST	=$dd
TOK_ROTATE	=$e2

newtab:		.byte "repeaT","untiL"	;cc,cd
		.byte "whilE","wenD"	;ce,cf
		.byte "elsE", "endiF"	;d0, d1
		.byte "veR"		;d2
		.byte "blocK"		;d3
		.byte "cracK"		;d4
		.byte "veR"		;d5
		.byte "veR"		;d6
		.byte "veR"		;d7
		.byte "veR"		;d8
		.byte "veR"		;d9
		.byte "veR"		;da
		.byte "veR"		;db
		.byte "directorY"	;dc
		.byte "renuM"		;dd
		.byte "olD"		;de
		.byte "ofF"		;df
		.byte "veR"		;e0
		.byte "filter",'$'+$80	;e1
		.byte "rotate",'$'+$80  ;e2
		.byte "subst",'$'+$80   ;e3
		.byte "trans",'$'+$80	;e4
		.byte "rotor",'$'+$80	;e5
		.byte 0
.out .sprintf("Token-Tabelle bei %d Zeichen Länge", *-newtab)
.if *-newtab > 255 
	.error "Token-Tabelle zu lang!"
.endif
