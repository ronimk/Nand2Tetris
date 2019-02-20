// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.

// (We can assume that R0 >= 0, R1 >= 0 and R0*R1 < 32768)
// initialize the result to 0:
	@R2
	M=0

// first check whether R0 or R1 is zero:
	@R0
	D=M
	@END
	D;JEQ	// if R0 is 0, jump to end
	@R1
	D=M
	@END
	D;JEQ	// if R1 is 0, jump to end

// R0 > 0 and R1 > 0, prepare to calculate the result:
// Initialize the iterator to be the smaller of R0 and R1
// Also initialize the adder to be the larger of R0 and R1.
// Worst case scenario: the program will slow down by 14 instructions -
// Best case scenario, it will speed up the process significantly
// (consider 1980*5 and 5*1980)
	@R0
	D=D-M	// D=R1-R0
	@R1ITER
	D;JLT

(R0ITER)	// R0 <= R1:
	@R0
	D=M
	@Iter
	M=D
	@R1
	D=M
	@Adder
	M=D
	@LOOP
	0;JMP

(R1ITER)	// R1 < R0:
	@R1
	D=M
	@Iter
	M=D
	@R0
	D=M
	@Adder
	M=D
	@LOOP
	0;JMP
	
// at this point D contains the value of Adder to be
// added to the result in R2 one more time:
(LOOP)
	@R2
	M=D+M

	@Iter	// Iter is the loop iterator
	D=M-1	// subtract one from the value of Iter
	@END
	D;JEQ	// if the result is 0, jump to the end.
	@Iter
	M=D
	@Adder
	D=M
	@LOOP
	0;JMP

// We are done - the result is in R2:
(END)
	@END
	0;JMP


