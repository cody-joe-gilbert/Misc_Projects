*** Cody Gilbert **
* Operating Systems, Lab 2: Scheduler
* Due: 10/23/2018

COMPILATION
The source code for the linker program is contained within the
C++ source file, "sched.cpp", and can be compiled using the
associated makefile, "makefile". The program was programmed and
tested with C++ 11, Version 4.4.7. The resulting executable 
will be "sched". 

EXECUTION
The "shed" program accepts four arguments:

-v : Verbose output. Will print to stdout an information regarding
	the current progress of the simulation, including the event list
	at each time step, a description of each process changing state,
	and any information specific to a given scheduler.

-s<opt> : Scheduler option. If not specified, will default to FCFS. 
		Otherwise, will run a scheduler based on the following inputs
		to the <opt> option:
		-sF : First-Come-First-Served (FCFS) scheduler
		-sL : Last-Come-First-Served (LCFS) Scheduler
		-sS : Shortest Remaining Time First Scheduler
		-sR<q> : Round-Robin Preemptive scheduler with an integer
			quantum <q>.
		-sP<q> : Priority Scheduler with an integer
			quantum <q>.
		-sE<q> : Preemptive Priority Scheduler with an integer
			quantum <q>.

ifile : Required parameter. The pathname of the input file.

rfile : Required parameter. The pathname of the random number file.

OUTPUT
All output is sent to standard output to conform to the
grading shell.

