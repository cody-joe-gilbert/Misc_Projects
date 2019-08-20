# -*- coding: utf-8 -*-
"""
Runs the three parallel processor solver scripts: FrontEnd, Davis-Putnam, and
BackEnd. Takes as input the given input file.
@author: Cody Gilbert
"""

import sys 
from FrontEnd import FE
from DavisPutnam import DP
from BackEnd import BE
inputfile = sys.argv[1]
FE(inputfile)
DP()
BE()


