# -*- coding: utf-8 -*-
"""
Consolidate, post-process, and analyze results for South Delta permanent barriers project

Doug Jackson
doug@QEDAconsulting.com
"""
import os
import glob
import shutil
import yaml
import pandas as pd
from datetime import datetime as dt

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

runs = pd.read_excel(os.path.join(workingDir, "runs.xlsx"))

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

###########################################################################
# Verify that simulations match specs in runs.xlsx
for index, row in runs.iterrows():
    print("="*75)
    
    thisRunID = row["runID"]
    print(f"Running QA on config file for runID {thisRunID}")
    
    thisAgentType = row["agentType"]
    
    thisRunPath = os.path.join(workingDir, "output", f"runID_{thisRunID}")
    
    thisConfigFile = glob.glob(os.path.join(thisRunPath, "*.yaml"))
       
    if len(thisConfigFile)==1:
        thisConfigFile = thisConfigFile[0]
    else:
        print(f"Missing config file for runID {thisRunID}")
    
    with open(thisConfigFile, "r") as fH:
        
        thisConfig = yaml.safe_load(fH)
    
    # Verify that the correct config file template was used
    if thisAgentType=="particle" or thisAgentType=="surface":
        correctConfigFile = os.path.join(thisRunPath, f"ptmConfig_{thisAgentType}_runID_{thisRunID}.yaml")
        
        thisInsertionNode = thisConfig["particle_insertion"][0][0]
        correctInsertionNode = row["insertionNode"]
        
        if thisInsertionNode!=correctInsertionNode:
            print(f"RunID {thisRunID} insertion node of {thisInsertionNode} doesn't match runs.xlsx insertion node of {correctInsertionNode}")
        
        
    elif thisAgentType=="salmon":
        thisInsertionNode = row["insertionNode"]
        if thisInsertionNode=="Freeport":            
            correctConfigFile = os.path.join(thisRunPath, f"ptmConfig_{thisAgentType}_ND_runID_{thisRunID}.yaml")
            
        elif thisInsertionNode=="Vernalis":          
            correctConfigFile = os.path.join(thisRunPath, f"ptmConfig_{thisAgentType}_SD_runID_{thisRunID}.yaml")
    
    if not os.path.samefile(os.path.normpath(thisConfigFile), os.path.normpath(correctConfigFile)):
        print(f"RunID {thisRunID} config file doesn't match agent type in runs.xlsx: {thisAgentType}")
        continue

    # Check start and end dates
    thisStartDate = thisConfig["ptm_start_date"]
    thisEndDate = thisConfig["ptm_end_date"]
    correctStartDate = dt.strftime(row["startDate"].to_pydatetime(), "%d%b%Y").upper()
    correctEndDate = dt.strftime(row["endDate"].to_pydatetime(), "%d%b%Y").upper()
    
    if thisStartDate!=correctStartDate:
        print(f"RunID {thisRunID} start date of {thisStartDate} doesn't match runs.xlsx start date of {correctStartDate}")
    
    if thisEndDate!=correctEndDate:
        print(f"RunID {thisRunID} end date of {thisEndDate} doesn't match runs.xlsx end date of {correctEndDate}")
    
    
    
    
    

        
    