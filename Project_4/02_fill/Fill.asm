// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

// 1) The first memory location of the screen is 16384.
// 2) There are 32 memory addresses per line on the screen and there are 256 lines.
//    Therefore the first memory address not on screen anymore is 24575.
// 3) The current position being worked on in the screen is in CurrPos. Assume
//    the starting position to be 16384.
//    All the pixels before CurrPos are considered black.
//    All the pixels on and after CurrPos are considered white (except maybe when the program starts).

	// 1)
	@16384
	D=A
	@ScreenBegin
	M=D
	// 2)
	@CurrPos
	M=D

	// 3)
	@24576
	D=A
	@ScreenEnd
	M=D

// The main program routine:
// Whenever a key is being pressed, it starts writing black pixels 16 at a time from CurrPos
// towards ScreenEnd. When a key is being released, it starts writing white pixels 16 at a time
// from M[CurrPos-1] towards ScreenStart.

(KEYCHECK)
	@24576
	D=M			// If no key is being pressed, D == 0.
	@FILLWHITE
	D;JEQ		// If no key was pressed, jump to FILLWHITE.
	
(FILLBLACK) 	// Fill in a black pixel if possible.
	@CurrPos
	D=M
	@ScreenEnd
	D=D-M
	@KEYCHECK
	D;JEQ
	// Fill in a black pixel:
	@CurrPos
	D=M
	A=D
	M=-1
	// Advance CurrPos:
	@CurrPos
	M=D+1
	@KEYCHECK
	0;JMP
	
(FILLWHITE)		// Fill in a white pixel if possible. 
	@CurrPos
	D=M
	@ScreenBegin
	D=D-M
	@KEYCHECK
	D;JEQ
	// Advance CurrPos:
	@CurrPos
	D=M-1
	M=D
	// Fill in a white pixel:
	A=D
	M=0
	@KEYCHECK
	0;JMP