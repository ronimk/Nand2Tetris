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

// This version is unnecessarily complicated for one reason only:
// I wanted to practice how to write a subroutine in HACK machine language
// with the info provided in chapter 4 (= not using a stack).
// Both the Pixel fill routine and the advance routine have been isolated
// into their own "subroutines". (Since the handling of the return address
// is very limited without a stack, one can argue that these are not real subroutines).

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
	
(FILLBLACK) 	// Fill with black pixels - set Direction to 1 
	@CurrPos
	D=M
	@ScreenEnd
	D=D-M
	@KEYCHECK
	D;JEQ
	
	@FillPixel
	M=-1
	@Direction
	M=1
	
	// First fill, then advance:
	@AFTER_FILL
	D=A
	@Ret
	M=D
	@FILL
	0;JMP
(AFTER_FILL)
	@KEYCHECK
	D=A
	@Ret
	M=D
	@ADVANCE
	0;JMP
	
(FILLWHITE)		// Fill with white pixels - set direction to -1. 
	@CurrPos
	D=M
	@ScreenBegin
	D=D-M
	@KEYCHECK
	D;JEQ
	
	@FillPixel
	M=0
	@Direction
	M=-1
	
	// First Advance, Then fill:
	@AFTER_ADVANCE
	D=A
	@Ret
	M=D
	@ADVANCE
	0;JMP
(AFTER_ADVANCE)
	@KEYCHECK
	D=A
	@Ret
	M=D
	@FILL
	0;JMP
	
// The sub-program to fill a block of selected pixels on the screen:
// FillPixel holds the pixel to be filled at CurrPos.
// Ret holds the return address for the code following the subroutine.
(FILL)
	@FillPixel
	D=M
	@CurrPos
	A=M
	M=D
	@Ret
	A=M
	0;JMP
	
// The sub-program to advance CurrPos:
// Direction holds 1 if CurrPos needs to be moved forward,
// -1 if CurrPos needs to be move backward.
// Ret holds the return address for the code following the subroutine.
(ADVANCE)
	@CurrPos
	D=M
	@Direction
	D=D+M
	@CurrPos
	M=D
	@Ret
	A=M
	0;JMP
