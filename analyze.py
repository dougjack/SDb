# -*- coding: utf-8 -*-
"""
Consolidate, post-process, and analyze results for South Delta permanent barriers project

Doug Jackson
doug@QEDAconsulting.com
"""
import os
import glob
import shutil
import pandas as pd

###########################################################################
# Constants
###########################################################################
workingDir = "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb"

# Specify required output files
reqFiles = {"salmon": {"Freeport":["routeSurvival.csv"],
                       "Vernalis":["routeSurvival.csv"]},
            "particle": ["ptm_out_groupFlux.csv", "ptm_out_nodeFlux.csv"],
            "surface":["ptm_out_groupFlux.csv", "ptm_out_nodeFlux.csv"]}

###########################################################################
# Constants
###########################################################################
os.chdir(workingDir)

###########################################################################
# Consolidate outputs
instanceOutputDirs = glob.glob(os.path.join(workingDir, "instance_*"))

for iOD in instanceOutputDirs:
    
    thisOutputDirs = glob.glob(os.path.join(iOD, "output", "runID*"))
    
    for oD in thisOutputDirs:
        thisDirName = os.path.basename(oD)
    
        # Delete any existing outputDir for this runID
        destDir = os.path.join(workingDir, "output", thisDirName)
        try:
            shutil.rmtree(destDir)
        except:
            pass
        
        shutil.copytree(oD, destDir, dirs_exist_ok=True)

###########################################################################
# Find missing outputs
runs = pd.read_excel(os.path.join(workingDir, "runs.xlsx"))

missingRunIDs = []
for index, row in runs.iterrows():
    
    thisRunID = row["runID"]
    thisAgentType = row["agentType"]
    
    thisReqFiles = reqFiles[thisAgentType]
    
    if thisAgentType=="salmon":
        thisReqFiles = thisReqFiles[row["insertionNode"]]
    
    missing = False
    
    for f in thisReqFiles:
        
        thisOutputPath = os.path.join(workingDir, "output", f"runID_{thisRunID}", "output", f)
        if not os.path.exists(thisOutputPath):
            print(f"Missing output: {thisOutputPath}")
            missing = True
    
    if missing:
        missingRunIDs.append(thisRunID)

missingRuns = runs.loc[runs["runID"].isin(missingRunIDs)].copy()

missingRuns.to_csv(os.path.join(workingDir, "missingRuns.csv"), index=False)

print(f"Number of missing runs: {missingRuns.shape[0]}")
print(missingRunIDs)
    
    

        
    