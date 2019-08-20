# -*- coding: utf-8 -*-
"""
Front-end for solving the parallel program optimization problem.
Takes in the standard input of initial state, goal state, and time constraint,
and output the input for the D-P algorithm as a file, as well as information
for re-constructing the solution with the back-end program.

@author: Cody Gilbert
"""
import sys

def FE(inputfile):
    #inputfile = 'testFEinput.txt'
    outputfile = 'DPInput.txt'
    #inputfile = sys.argv[1]
    with open(inputfile) as f:
        start_array = f.readline().strip().split()
        goal_array = f.readline().strip().split()
        goaltime = int(f.readline().strip())
    
    start = []
    entry = []
    for i, item in enumerate(start_array):
        if i % 2 == 0:
            entry.append(int(item))
            continue
        else:
            entry.append(item)
            start.append(entry)
            entry = []
            
    goal = []
    entry = []
    for i, item in enumerate(goal_array):
        if i % 2 == 0:
            entry.append(int(item))
            continue
        else:
            entry.append(item)
            goal.append(entry)
            entry = []
    
    VarSet = list(set([x[1] for x in start]))
    # Create Atoms
    Values = []
    # Create Value atoms
    for R in range(len(start)):
        for V in VarSet:
            for I in range(goaltime + 1):
                Values.append([R, V, I])
    # Create Assign atoms
    Assign = []
    for RA in range(len(start)):
        for RB in range(len(start)):
            for I in range(1, goaltime + 1):
                Assign.append([RA, RB, I])
    offSet = len(Values)  
    # Create clauses
    with open(outputfile, 'w') as f:
        # Unique value clauses
        for R in range(len(start)):
            for I in range(goaltime + 1):
                for i, VX in enumerate(VarSet):
                    for j, VY in enumerate(VarSet):
                        if i < j:
                            X_index = Values.index([R, VX, I]) 
                            Y_index = Values.index([R, VY, I])
                            clause = str(-(X_index + 1)) + " " + str(-(Y_index + 1)) + "\n"
                            f.write(clause)
        
        # Positive Effect of Actions Clauses
        for RA in range(len(start)):
            for RB in range(len(start)):
                for I in range(goaltime):
                    for V in VarSet:
                        P_index = Values.index([RB, V, I])
                        Q_index = Values.index([RA, V, I + 1])
                        M_index = Assign.index([RA, RB, I + 1]) + offSet
                        clause = (str(-(P_index + 1)) + " " + 
                                  str(-(M_index + 1)) + " " +
                                  str((Q_index + 1)) + "\n")
                        
                        f.write(clause)
        
        # Frame Axiom Clauses
        for R in range(len(start)):
            for I in range(1, goaltime + 1):
                for VX in VarSet:
                    P_index = Values.index([R, VX, I - 1])
                    clause = str(-(P_index + 1)) + " "
                    for Ri in range(len(start)): 
                        Qi_index = Assign.index([R, Ri, I]) + offSet
                        clause += str(Qi_index + 1) + " "
                    M_index = Values.index([R, VX, (I)])
                    clause += str(M_index + 1) + "\n"
                    f.write(clause)
        
        #Incompatible Assignments Clauses
        for RA in range(len(start)):
            for RB in range(len(start)):
                if RA != RB:
                    for RC in range(len(start)):
                        if RC != RB and RC != RA:
                            for I in range(1, goaltime + 1):
                                P_index = Assign.index([RA, RB, I]) + offSet
                                Q_index = Assign.index([RB, RA, I]) + offSet
                                N_index = Assign.index([RA, RC, I]) + offSet
                                M_index = Assign.index([RB, RC, I]) + offSet
                                clause = str(-(P_index + 1)) + " " + str(-(Q_index + 1)) + "\n"
                                f.write(clause)
                                clause = str(-(P_index + 1)) + " " + str(-(M_index + 1)) + "\n"
                                f.write(clause)
                                clause = str(-(P_index + 1)) + " " + str(-(N_index + 1)) + "\n"
                                f.write(clause)
                                #clause = str(-(P_index + 1)) + " " + str(-(O_index + 1)) + "\n"
                                #f.write(clause)
        #Starting assignment clauses
        for entry in start:
            P_index = Values.index([entry[0] - 1, entry[1], 0])
            clause = str(P_index + 1) + "\n"
            f.write(clause)
            
        #Goal assignment clauses
        for entry in goal:
            P_index = Values.index([entry[0] - 1, entry[1], goaltime])
            clause = str(P_index + 1)  + "\n"
            f.write(clause)
            
        f.write("0 \n")  # Ending clause for D-P program
        
        # Write number of Values atoms:
        f.write(str(len(Values)) + "\n")
        # Write number of Assign atoms:
        f.write(str(len(Assign)) + "\n") 
        # Write out the arrays of the Value atoms 
        for V in Values:
            out = ""
            for i in V:
                out += str(i) + " "                              
            out += "\n"
            f.write(out)
        # Write out the arrays of the Assign atoms 
        for V in Assign:
            out = ""
            for i in V:
                out += str(i) + " "                              
            out += "\n"
            f.write(out)                    
                                
        
    
    
    