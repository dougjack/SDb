# -*- coding: utf-8 -*-
"""
Perform ECO-PTM simulations for Tom Paine Slough project

Doug Jackson
doug@QEDAconsulting.com
"""
import os
import time
import json
import shutil
from pathlib import Path
import subprocess
import boto3
import numpy as np
import pandas as pd
from datetime import datetime as dt

import argparse

# Read in command line arguments
parser = argparse.ArgumentParser(description="Poll AWS for South Delta barrier ECO-PTM jobs and execute them.")
parser.add_argument("--instanceNum", action="store", dest="instanceNum", required=False)
args = parser.parse_args()

if args.instanceNum:
    instanceNum = args.instanceNum
else:
    instanceNum = 0
print(f"instanceNum: {instanceNum}")

####################################################################################################
# Constants
####################################################################################################
workingDir = "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb"

releaseStep_min = 15

javaPath = "C:/Program Files/Java/jdk-26.0.2/bin/java"
jarPath = "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb/data/ecoptm-v0.0.0-beta.jar"

processOutputPath = "C:/Users/dougj/Documents/QEDA/DWR/programs/EcoPTM_private/scripts/utilities/process_output/process_output.py"

# AWS setup
pollAWSjobs = False
queueName = "ECOPTM"
AWSconfigFile = "/Users/dougj/Documents/QEDA/AWS/DJackson_config.json"
sleepTime_sec = 10

# Maximum number of times to re-poll when there are no messages
maxRePollCount = 6000

# Number of minutes to wait before aborting and retrying run
timeout_min = 5
maxAttempts = 10

myShell = False

###########################################################################
# Functions
###########################################################################
def establishConnection():
    # Load the AWS credentials
    with open(AWSconfigFile, "r") as fH:
        AWSconfig = json.load(fH)
        
    queue = None
    while queue is None:
        try:
            sqs = boto3.resource("sqs", region_name=AWSconfig["region_name"], 
                                aws_access_key_id=AWSconfig["aws_access_key_id"],
                                aws_secret_access_key=AWSconfig["aws_secret_access_key"])
            queue = sqs.get_queue_by_name(QueueName=queueName)
        except:
            time.sleep(sleepTime_sec)

    return queue

####################################################################################################
# Run
####################################################################################################
os.chdir(workingDir)

outputDir = os.path.join(workingDir, f"instance_{instanceNum}", "output")
os.makedirs(outputDir, exist_ok=True)

# Make copies of inputs
inputCopiesDir = os.path.join(workingDir, "output", "inputCopies")
os.makedirs(inputCopiesDir, exist_ok=True)
shutil.copy(os.path.join(workingDir, "runs.xlsx"), os.path.join(inputCopiesDir, "runs.xlsx"))
shutil.copy(os.path.join(workingDir, "runSDb.py"), os.path.join(inputCopiesDir, "runSDb.py"))
shutil.copy(jarPath, os.path.join(inputCopiesDir, os.path.basename(jarPath)))
shutil.copy(os.path.join(workingDir, "data", "ptmConfig_template_neutrallyBuoyant.yaml"), os.path.join(inputCopiesDir, "ptmConfig_template_neutrallyBuoyant.yaml"))
shutil.copy(os.path.join(workingDir, "data", "ptmConfig_template_surface.yaml"), os.path.join(inputCopiesDir, "ptmConfig_template_surface.yaml"))
shutil.copy(os.path.join(workingDir, "data", "ptmConfig_template_salmon.yaml"), os.path.join(inputCopiesDir, "ptmConfig_template_salmon.yaml"))

runs = pd.read_excel(os.path.join(workingDir, "runs.xlsx"))
runIDs = runs["runID"].unique().tolist()

if pollAWSjobs:
    queue = establishConnection()

runIndex = 0
rePollCount = 0
while runIndex<runs.shape[0]:

    if pollAWSjobs:
        # Poll the queue for available jobs 
        try:
            message = queue.receive_messages(MaxNumberOfMessages=1)
        except:
            queue = establishConnection()
            time.sleep(sleepTime_sec)
            continue
    
    else:
        message = "dummy"
    
    if len(message)>0:
        
        if pollAWSjobs:
            # Read the message from the queue
            print("message:", message[0].body)
            fields = message[0].body.split(",")
            message[0].delete()
            
            row = runs.loc[runs["runID"]==fields[0]]
        else:
            thisRunID = runIDs[runIndex]
            row = runs.loc[runs["runID"]==thisRunID]
            runIndex+=1
    
        thisRunID = int(row["runID"].values[0])
        thisAgentType = str(row["agentType"].values[0])
        thisNumAgents = int(row["numAgents"].values[0])
        thisStartDate = dt.strftime(pd.Timestamp(row["startDate"].values[0]), "%d%b%Y").upper()
        thisEndDate = dt.strftime(pd.Timestamp(row["endDate"].values[0]), "%d%b%Y").upper()
        
        thisOutputDir = os.path.join(outputDir, f"runID_{thisRunID}")
        try:
            shutil.rmtree(thisOutputDir)
        except:
            pass
        os.makedirs(thisOutputDir, exist_ok=True)
        
        if thisAgentType=="particle":
            with open(os.path.join(workingDir, "data", "ptmConfig_template_neutrallyBuoyant.yaml")) as fH:
                thisConfig = fH.read()
                
            thisConfig = thisConfig.replace("INSERTION_NODE_PLACEHOLDER", str(int(row["insertionNode"].values[0])))
            thisConfig = thisConfig.replace("RELEASE_NUM_PLACEHOLDER", str(int(row["numAgents"].values[0])))
        
        elif thisAgentType=="surface":
            with open(os.path.join(workingDir, "data", "ptmConfig_template_surface.yaml")) as fH:
                thisConfig = fH.read()
                
            thisConfig = thisConfig.replace("INSERTION_NODE_PLACEHOLDER", str(int(row["insertionNode"].values[0])))
            thisConfig = thisConfig.replace("RELEASE_NUM_PLACEHOLDER", str(int(row["numAgents"].values[0])))
            
            shutil.copy(os.path.join(workingDir, "data", "particle.bhv"), os.path.join(thisOutputDir, "particle.bhv"))
        
        elif thisAgentType=="salmon":
            with open(os.path.join(workingDir, "data", "ptmConfig_template_salmon.yaml")) as fH:
                thisConfig = fH.read()
                
            thisReleaseDate = dt.strftime(pd.Timestamp(row["releaseDate"].values[0]), "%m/%d/%Y")
            
            numPlaceholders = thisConfig.count("RELEASE_NUM_PLACEHOLDER")
            numPerRelease = int(np.ceil(thisNumAgents/numPlaceholders))
            
            thisConfig = thisConfig.replace("RELEASE_DATE_PLACEHOLDER", thisReleaseDate)
            thisConfig = thisConfig.replace("RELEASE_NUM_PLACEHOLDER", str(numPerRelease))
        
        else:
            print(f"Invalid agent type: {thisAgentType}")
            raise RuntimeError()
            
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
    
    else:
        print("No jobs to run")
        rePollCount+=1
        
        if rePollCount>maxRePollCount:
            break
    
        time.sleep(sleepTime_sec)
        
