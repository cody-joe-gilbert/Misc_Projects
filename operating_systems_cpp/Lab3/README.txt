*** Cody Gilbert **
* Operating Systems, Lab 3: MMU
* Due: 11/20/2018

COMPILATION
The source code for the MMU program is contained within the
C++ source file, "MMU.cpp", and can be compiled using the
associated makefile, "makefile". The program was programmed and
tested with C++ 11, Version 4.4.7. The resulting executable 
will be "mmu". 

EXECUTION
The "mmu" program accepts five arguments:

-a<opt> : Replacement Algorithm. Will run a replacement based 
			on the following inputs to the <opt> option:
		-af : First-In-First-Out (FIFO) Algorithm
		-ar : Random Algorithm
		-ac : Clock Algorithm
		-ae : ESC/NRU Algorithm
		-aa : Aging Algorithm
		-aw : Working Set Algorithm
		
-o<opt> : Output options. Any combination of the following
			may be selected.
		-oO : Per Instruction Output
		-oP : Final page table contents output
		-oF : Frame table contents output
		-oS : Summary Output
		-oa : Aging Output
	

ifile : Required parameter. The pathname of the input file.

rfile : Required parameter. The pathname of the random number file.

OUTPUT
All output is sent to standard output to conform to the
grading shell.

