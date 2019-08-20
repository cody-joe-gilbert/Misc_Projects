# -*- coding: utf-8 -*-
"""
Back-end for solving the parallel program optimization problem.
Takes the output of the Davis-Putnum Algorithm solver (DavisPutnam.py) and 
displays the results on the screen.

@author: Cody Gilbert
"""
import sys
def BE():
    inputfile = 'DPOutput.txt'
    #inputfile = sys.argv[1]
    solution = []
    stuff = []  # Extra content to print to the end of the output
    stuffFlag = False
    with open(inputfile) as f:
        for line in f:
            if line.strip() == "No solution":
                print("No Solution found.")
                return
            sline = line.strip().split()
            if stuffFlag:
                stuff.append(sline)
                continue
            if sline[0] != "0":
                solution.append(sline)
            else:
                stuffFlag = True
    NumValue = int(stuff[0][0])
    Atoms = []
    for i in range(2, len(stuff)):
        Atoms.append(stuff[i])
    # Run through assignments to determine which swap values are true
    SolAssign = []
    for solv in solution:
        if solv[1] == "T" and int(solv[0]) > NumValue:
            SolAssign.append(Atoms[int(solv[0]) - 1])
    MaxCycles = 0  #Determine the maximum number of cycles
    for i in SolAssign:
        if int(i[2]) > MaxCycles:
            MaxCycles  = int(i[2]) 
    for i in range(1, MaxCycles + 1):
        print("Cycle " + str(int(i)) + ":")
        for sol in SolAssign:
            if int(sol[2]) == i: 
                if sol[0] != sol[1]:
                    message = str(int(sol[0]) + 1) + "=" + str(int(sol[1]) + 1)
                    print(message)
                    #message = "index:" + str(Atoms.index(sol))
                    #print(message)
    
    
    