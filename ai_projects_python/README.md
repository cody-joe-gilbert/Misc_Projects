# Artificial Intelligence Project

This folder contains a project created as part of an NYU Artificial Intelligence class taught by Professor Ernest Davis. This course developed skill in artificial intellegence outside of the usual machine learning applications.

## Parallel Program Optimization
This project solves a simple parallel program optimization problem by translating the problem's constraints into propositional logic (a SAT problem) and solving with the Davis-Putnam Algorithm.

* `./program_optimization/AI_assignment.html` contains the project's description written by Professor Ernest Davis
* `./program_optimization/FrontEnd.py` is a Python script that takes in the problem specs and creates a system of disjoint propositional statements
* `./program_optimization/DavisPutnam.py` is a Python script used to solve general SAT problems with the Davis-Putnam Algorithm
* `./program_optimization/BackEnd.py` is a Python script that translates the SAT solver output into the parallel program optimization solution
