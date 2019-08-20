# -*- coding: utf-8 -*-
"""
This modules runs the Davis-Putnam algorithm on the given input file
@author: Cody Gilbert
"""
from copy import deepcopy
def DP():
    # =============================================================================
    # D-P helper functions
    # =============================================================================
    def findPures(S):
        # Function to find pure literals 
        atoms = {}
        for clause in S:
            for a in clause:
                a_key = str(abs(a))
                if a_key in atoms:
                    if atoms[a_key] is False:
                        continue
                    if atoms[a_key] != a:
                        # Different sign: set to false
                        atoms.update({a_key:False})
                else:
                    # not in the dict, so add
                    atoms.update({a_key:a})
        for key in atoms:
            if atoms[key] is False:
                continue
            else:
                return atoms[key]
        
        return 0
    
    def propagate(S, v, atom, atom_val):
        # Propagates the given atom assignment within the set of literals S,
        # returning the resultant updated set of literals and the updated set of 
        # atomic assignments
        v.update({atom:atom_val})
        for i, clause in enumerate(S):
            for j, a in enumerate(clause):
                if abs(a) == atom:
                    a_sign = (a > 0)
                    if a_sign == atom_val:
                        # Same sign, therefore true clause
                        del(S[i])
                        return propagate(S, v, atom, atom_val)
                    else:
                        # dif sign, false literal
                        del(S[i][j])
                        return propagate(S, v, atom, atom_val)
        return S, v
        
    
    
    # =============================================================================
    # Body of D-P algo
    # =============================================================================        
    
    def DP(S, v):
        # S: List of clauses (list of lists of ints)
        # v: dictionary of current atomic assignments
        loopFlag = True
        while loopFlag:
            loopFlag = False  # Loop only until flag ceases to be updated
            # Simplification and propagation loop
            if S == []: # Check if all clauses sat
                return v
            
            for i in S: # Check for empty clauses
                if i == []:
                    return False
            
            for i in S:  # Check for simpleton clauses and propagate
                if len(i) == 1:
                    if i[0] < 0:
                        S, v = propagate(S, v, abs(i[0]), False)
                    else:
                        S, v = propagate(S, v, abs(i[0]), True)
                    loopFlag = True
            
            pure = findPures(S) # Check for pure atoms and propagate
            if pure != 0:  
                if pure < 0:
                    S, v = propagate(S, v, abs(pure), False)
                else:
                    S, v = propagate(S, v, abs(pure), True)
                loopFlag = True
        # Recursive section
        # Set first unassigned atom to true:
        for atom_key in v:
            if v[atom_key] is None:
                S_T, v_T = propagate(deepcopy(S), deepcopy(v), atom_key, True)
                break
        results = DP(S_T, v_T)
        if results == False: # If true assignment doesn't work, set to false
            S_F, v_F = propagate(S, v, atom_key, False)
            return DP(S_F, v_F)
        else:
            return results
            
    
    # =============================================================================
    # Read input file
    # =============================================================================
    inputfile = 'DPInput.txt'
    outputfile = 'DPOutput.txt'
    # inputfile = sys.argv[1]
    clauses = []
    stuff = []  # Extra content to print to the end of the output
    stuffFlag = False
    with open(inputfile) as f:
        for line in f:
            if stuffFlag:
                stuff.append(line)
                continue
            sline = line.strip().split()
            if sline[0] != "0":
                clauses.append([int(x) for x in sline])
            else:
                stuffFlag = True
    # Initialize assignment dictionary
    v = {}
    for i, c in enumerate(clauses):
            for j, a in enumerate(c):
                if abs(a) not in v:
                    v.update({abs(a):None})
    results = DP(clauses, v)  # Run D-P algo on given set
    with open(outputfile, 'w') as f:
        if results == False:
            f.write("No solution\n")
        else:
            for i in sorted(v.keys()):
                if results[i]:   
                    f.write(str(i) + " " + "T" + "\n")
                elif results[i] is None:
                    f.write(str(i) + " " + "T" + "\n")
                else:  #In case an object is invariant, aka remains None
                    f.write(str(i) + " " + "F" + "\n")
            f.write('0 \n')
        for line in stuff:
            f.write(line)