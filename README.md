# CYPHERBASIC 64 #

![Cypherbasic Image](https://github.com/silizium/cypherbasic/blob/master/cypherbasic_klein.png "Cypherbasic 64")


## loading & compiling ##

```
LOAD "cypherbasic",8,1:new:sys9*4096
compile: ca65 -t c64 cypherbasic.s && ld65 -t c64 -o cypherbasic cypherbasic.o c64.lib
```
with cc65 compiler/assembler package

## description ##

Just a tiny, little fun-project of mine. I was asking the question: “why does anybody use the C=64 as a cyphering machine, crypto-device?” And I started to build some very simple and basic crypto library functions in 6502. 

After a day or so I discovered that most peole are not talking 6502 but have no problems with BASIC, so why not call the functions from a small basic extenstion I asked? 

The result is this project, still unter development. Its consuming some time to code and I ask myself if anybody is interested in such stuff? Cause if nobody uses this I really could put my time in other projects. In a german C=64 forum the echo was friendly but not very broad. So I just ask here. 

There are yet some useful functions in this project, to name some: 

```
ROTATE$( String, Rotation) 
```

Rotates a String around like for the cesar chiffre. 

Example: 
```
?ROTATE$(“ABCDEFG”,2) 
FGABCDE 

SUBST$( Textstring, Alphabetstring, Codestring) 
```

Substitutes the chars in the Text with the substitution table from alphabet and codestring just like the rotor of a rotormachine like the enigma. 

Example: 
```
TE$=“ATTACKATDAWN” 
AL$=“ABCDEFGHIJKLMNOPQRSTUVWXYZ” 
CO$=ROTATE$(AL$,13) : REM CESAR CHIFFRE 
?SUBST$(TE$, AL$, CO$) 
NGGNPXNGQNJA 


TRANS$( Textstring, Sourcepositionstring, Destpositionstring) 
```

Makes transpositions, means changes the position of the characters in a string like its defined in Source and Destination. All three strings have to have the same length of cause and all codes in source have to be in dest also. Else there will result errors. 

Example: 

```
TE$=“ATEST” 
SO$=“12345” 
DE$=“45213” 
?TRANS$(TE$, SO$, DE$) 
STTAE 


FILTER$( String, Filterbyte) 
```

Can filter the input from in filterbyte specified attributes, defined like this: 

* [0] lower char 
* [1] numbers 
* [2] space and punctation 
* [3] upper chars 
* [4] strip doubles 
* [5–7] clears bit 5,6,7 

Examples: 

```
A$=“ABCDEFGHIJKLMNOPQRSTUVWXYZ” 
PA$=“PASSWORT” 
?FILTER$(PA$+A$,$11) 
PASWORTBCDEFGHIJKLMNQUVWXYZ 

?filter$(“abc123 def”, $01) 
abcdef 
?filter$(“abc123 DEF”, %00001001) 
abcDEF 
?filter$(“abc123 DEF”, %10000001) :rem $81 = 129 
abcdef 
a$=“The attack will start at noon” 
?filter$(a$, $81) 
theattackwillstartatnoon 


BLOCK Size (, Perline) 
```

Prints out all output ordered in blocks like its common in cryptography. 


Example: 
```
BLOCK 5 
?“ATTACKATDAWNPREPAREFORHEAVYARTILLERY” 
ATTAC ATDAW NPREP AREFO HEAVY 
ARTIL LERY 

Same with BLOCK 5,1 

ATTAC 
ATDAW 
NPREP 
AREFO 
HEAVY 
ARTIL 
LERY 
```

There are some other commands that have seen their implementation yet like OLD, REPEAT..UNTIL, like reading hexadecimal or binary numbers. And some commands and functions that lure to be coded like a RENUM, some cryptoanalysis functions, WHILE..WEND, DO..DONE construction, IF..THEN..ELSE..ENDIF and so on. 

But before I throw away much time, I am first looking for any feedback. Latest version can be downloaded from here (search end of thread for latest version) I still did not put it on my website, cause its just in development. 

I played around with some cryptomachines and had no problems with implementing basic, classic machines, an enigma should be a easy going with this, but I did not do it yet. I won’t take you away the fun

Take a look at the *.lst files to see the examples in ASCII. Otherwise load the *.prg with your C64 or an emulator.

Have fun! 
Provide feedback, if you had.

Hanno

## versions ##

0.10: update for ca65 V2.16 - Git 947b09ad, assembling again

0.9: initial upload
