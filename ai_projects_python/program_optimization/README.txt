Cody J. Gilbert
CSCI-GA.2560
Artificial Intelligence: Project 2
4/1/2019

The programming assignments have been implemented with Python Version 3 in
three modules, FrontEnd.py, DavisPutnam.py, and BackEnd.py for the Front End, Davis-Putnam Algorithm, and BackEnd parts, respectively.
A separate module runscript.py was included to run each of the three modules in the correct order.

COMPILATION AND EXECUTION
To execute the given programs for each part of the programming assignment, execute
on the Linux OS with Python Ver. 3 with the following command:
$ python3 runscript.py input.txt
where "input.txt" is the input text file implemented as specified in the assignment,
and python3 is the Python executable for Python Version 3.

OUTPUT
The solution to the given problem will be printed to stdout. The FrontEnd script will produce the file DPInput.txt that contains the clauses 
and atomic information for input to the Davis-Putnam algorithm. The DavisPutnam script will read the DPInput.txt file and create the file 
DPOutput.txt that contains the solution to the given input.