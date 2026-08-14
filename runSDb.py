# -*- coding: utf-8 -*-
"""
Perform ECO-PTM simulations for Tom Paine Slough project

Doug Jackson
doug@QEDAconsulting.com
"""
import os
import shutil
from pathlib import Path
import subprocess
import numpy as np
import pandas as pd
from datetime import datetime as dt
####################################################################################################
# Constants
####################################################################################################
workingDir = "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb"

releaseStep_min = 15

javaPath = "C:/Program Files/Java/jdk-26.0.2/bin/java"
jarPath = "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb/data/ecoptm_06may26.jar"

processOutputPath = "C:/Users/dougj/Documents/QEDA/DWR/programs/EcoPTM_private/scripts/utilities/process_output/process_output.py"

# Number of minutes to wait before aborting and retrying run
timeout_min = 5
maxAttempts = 10

myShell = False
####################################################################################################
# Run
####################################################################################################
os.chdir(workingDir)

outputDir = os.path.join(workingDir, "output")
os.makedirs(outputDir, exist_ok=True)

# Make copies of inputs
inputCopiesDir = os.path.join(workingDir, "output", "inputCopies")
os.makedirs(inputCopiesDir, exist_ok=True)
shutil.copy(os.path.join(workingDir, "runs.xlsx"), os.path.join(inputCopiesDir, "runs.xlsx"))
shutil.copy(os.path.join(workingDir, "runSDb.py"), os.path.join(inputCopiesDir, "runSDb.py"))
shutil.copy(jarPath, os.path.join(inputCopiesDir, os.path.basename(jarPath)))
shutil.copy(os.path.join(workingDir, "data", "ptmConfig_template_neutrallyBuoyant.yaml"), os.path.join(inputCopiesDir, "ptmConfig_template_neutrallyBuoyant.yaml"))
shutil.copy(os.path.join(workingDir, "data", "ptmConfig_template_neutrallyBuoyant.yaml"), os.path.join(inputCopiesDir, "ptmConfig_template_salmon.yaml"))

runs = pd.read_excel(os.path.join(workingDir, "runs.xlsx"))

for index, row in runs.iterrows():
    
    thisRunID = row["runID"]
    thisAgentType = row["agentType"]
    thisNumAgents = row["numAgents"]
    thisStartDate = dt.strftime(row["startDate"], "%d%b%Y").upper()
    thisEndDate = dt.strftime(row["endDate"], "%d%b%Y").upper()
    
    thisOutputDir = os.path.join(outputDir, f"runID_{thisRunID}")
    os.makedirs(thisOutputDir, exist_ok=True)
    
    if thisAgentType=="particle":
        with open(os.path.join(workingDir, "data", "ptmConfig_template_neutrallyBuoyant.yaml")) as fH:
            thisConfig = fH.read()
            
        thisConfig = thisConfig.replace("RELEASE_NUM_PLACEHOLDER", str(row["numAgents"]))
    
    else:
        with open(os.path.join(workingDir, "data", "ptmConfig_template_salmon.yaml")) as fH:
            thisConfig = fH.read()
            
        thisReleaseDate = dt.strftime(row["releaseDate"], "%m/%d/%Y")
        
        numPlaceholders = thisConfig.count("RELEASE_NUM_PLACEHOLDER")
        numPerRelease = int(np.ceil(thisNumAgents/numPlaceholders))
        
        thisConfig = thisConfig.replace("RELEASE_DATE_PLACEHOLDER", thisReleaseDate)
        thisConfig = thisConfig.replace("RELEASE_NUM_PLACEHOLDER", str(numPerRelease))
        
    thisConfig = thisConfig.replace("PTM_START_DATE_PLACEHOLDER", thisStartDate)
    thisConfig = thisConfig.replace("PTM_END_DATE_PLACEHOLDER", thisEndDate)
    
    thisConfigFile = os.path.join(thisOutputDir, f"ptmConfig_{thisAgentType}_runID_{thisRunID}.yaml")
        
    with open(thisConfigFile, "w") as fH:
        print(thisConfig, end="", file=fH)
    
    javaCommand = [f'{javaPath}', "-jar", f'{jarPath}',  f"{thisConfigFile}"]

    # Run the ECO-PTM
    pwd = Path.cwd()
    os.chdir(thisOutputDir)
    
    attempt = 0
    while attempt<maxAttempts:
        try:
            print(f"Launching ECO-PTM for runID {thisRunID}")
            completedProc = subprocess.run(javaCommand, shell=myShell, timeout=timeout_min*60)
            exitCode = completedProc.returncode
            if exitCode!=0:
                raise RuntimeError()
            else:
                break
        except subprocess.TimeoutExpired:
            attempt+=1
            print(f"ECO-PTM timed out. Attempting to run ECO-PTM again. Attempt {attempt}")
        except RuntimeError:
            attempt+=1
            print(f"ECO-PTM RuntimeError. Attempting to run ECO-PTM again. Attempt {attempt}")
      
    os.chdir(pwd)
    
    # Create the process_output config file
    with open(os.path.join(workingDir, "data", "process_output_config_template.yaml")) as fH:
        thisProcessOutputConfig = fH.read()
    
    thisProcessOutputConfig = thisProcessOutputConfig.replace("FLUX_OUTPUT_DIR_PLACEHOLDER", os.path.join(thisOutputDir, "output"))
    thisProcessOutputConfig = thisProcessOutputConfig.replace("FLUX_FILE_PLACEHOLDER", os.path.join(thisOutputDir, "output", "ptm_out.ncd"))
    
    thisProcessOutputConfigFile = os.path.join(thisOutputDir, "output", "process_output_config.yaml")
    with open(thisProcessOutputConfigFile, "w") as fH:
        print(thisProcessOutputConfig, end="", file=fH)

    pythonCommand = ["python", processOutputPath, "--configFile", thisProcessOutputConfigFile]
    
    print(f"Extracting fluxes for runID {thisRunID}")
    completedProc = subprocess.run(pythonCommand, shell=myShell)
    exitCode = completedProc.returncode
    if exitCode!=0:
        raise RuntimeError()
    
    print("="*100)
        
