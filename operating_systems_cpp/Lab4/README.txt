*** Cody Gilbert **
* Operating Systems, Lab 4: IO Scheduler
* Due: 12/11/2018

COMPILATION
The source code for the iosched program is contained within the
C++ source file, "iosched.cpp", and can be compiled using the
associated makefile, "makefile". The program was programmed and
tested with C++ 11, Version 4.4.7. The resulting executable 
will be "iosched". 

EXECUTION
The "iosched" program accepts five arguments:

-s<opt> : Replacement Algorithm. Will run a replacement based 
			on the following inputs to the <opt> option:
		-si : First-In-First-Out (FIFO) Algorithm
		-sj : Shortest Seek Time First (SSTF) Algorithm
		-ss : LOOK Algorithm
		-sc : CLOOK Algorithm
		-sf : FLOOK Algorithm
-v		: Verbose Output. Contains debugging process info
		

ifile : Required parameter. The pathname of the input file.


OUTPUT
All output is sent to standard output to conform to the
grading shell.

