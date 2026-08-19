# -*- coding: utf-8 -*-
"""
Consolidate, post-process, and analyze results for South Delta permanent barriers project

Doug Jackson
doug@QEDAconsulting.com
"""
import os
import glob

###########################################################################
# Constants
###########################################################################
workingDir = "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb"

###########################################################################
# Constants
###########################################################################
os.chdir(workingDir)

instanceOutputDirs = glob.glob(os.path.join(workingDir, "instance_*"))