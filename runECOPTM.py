# -*- coding: utf-8 -*-
"""
Perform ECO-PTM simulations for all reaches and releases

Doug Jackson
doug@QEDAconsulting.com
"""
import os
import glob
import pandas as pd
import boto3
import json
import shutil
import time
###########################################################################
# Constants
###########################################################################
model = "SouthDelta"
workingDir = f"C:/Users/dougj/Documents/QEDA/DWR/programs/EcoPTM_calibration/{model}"

scen = "SDySTyC"

queueName = "ECOPTM"
AWSconfigFile = "/Users/dougj/Documents/QEDA/AWS/DJackson_config.json"

# Maximum number of attempts to perform missing runs
maxAttempts = 5

purgeWaitTime_sec = 15
sleepTime_sec = 10
maxWaitCount = 5

###########################################################################
# Functions
###########################################################################
def findMissing():

    outFiles = pd.read_csv(os.path.join(workingDir, "output", "outFiles.csv"))
    reachesLocs = pd.read_csv(os.path.join(workingDir, scen, "reachesLocs.csv"))

    presentList = list()
    for index, row in outFiles.iterrows():
        presentList.append(os.path.exists(os.path.join(workingDir, scen, "ttime_out", row["file"])))
    outFiles["present"] = presentList

    missing = outFiles.loc[~outFiles["present"]]
    
    reachList = list()
    relList = list()
    for index, row in missing.iterrows():
        fields = row["file"].split("_")
        reachList.append(reachesLocs.loc[reachesLocs["loc"]==fields[4], "reach"].values[0])
        relList.append(fields[5].replace(".csv", ""))
        
    missing["reach"] = reachList
    missing["release"] = relList
    
    return missing
        
###########################################################################
# Run
###########################################################################
os.chdir(workingDir)

startTime = time.time()

# Load the AWS credentials
with open(AWSconfigFile, "r") as fH:
    AWSconfig = json.load(fH)
    
# Get the service resources
sqs = boto3.resource("sqs", region_name=AWSconfig["region_name"], 
                     aws_access_key_id=AWSconfig["aws_access_key_id"],
                     aws_secret_access_key=AWSconfig["aws_secret_access_key"])

queue = sqs.get_queue_by_name(QueueName=queueName)

sqsClient = boto3.client("sqs", region_name=AWSconfig["region_name"], 
                     aws_access_key_id=AWSconfig["aws_access_key_id"],
                     aws_secret_access_key=AWSconfig["aws_secret_access_key"])   

projectDir = os.path.join(workingDir, scen)

# Remove existing travel time outputs
ttimeDir = os.path.join(projectDir, "ttime_out")
ttimeFiles = glob.glob(os.path.join(ttimeDir, "*.csv"))
for f in ttimeFiles:
    os.remove(f)

missing = findMissing()
numRuns = missing.shape[0]

numAttempts = 0
while missing.shape[0]>0 and numAttempts<maxAttempts:
    numAttempts+=1

    print(f"{missing.shape[0]} runs of {numRuns} missing.")

    # Purge queues
    print(f"Purging queue {queueName}")
    queueURL = sqsClient.get_queue_url(QueueName=queueName)
    response = sqsClient.purge_queue(QueueUrl=queueURL["QueueUrl"])
    
    print(f"Waiting {purgeWaitTime_sec} seconds after purging queues...")
    time.sleep(purgeWaitTime_sec)   
    
    for index, row in missing.iterrows():
        
        runDir = os.path.join(workingDir, scen, row["reach"], row["release"])
    
        paramsFile = os.path.join(runDir, "params.txt")

        configFile = glob.glob(os.path.join(runDir, "*_template.yaml"))[0]
        
        params = pd.read_csv(paramsFile, sep=r"\s+", engine="python")
        
        with open(configFile, "r") as fH:
            config = fH.read()
        
        # Replace placeholders with parameter values
        for index, row in params.iterrows():
            
            config = config.replace(f"{row['name']}_PLACEHOLDER", f"{row['val']}")
        
        newConfigFile = configFile.replace("template", "final")
        with open(newConfigFile, "w") as fH:
            print(config, end="", file=fH)
        
        message = (newConfigFile)
        response = queue.send_message(MessageBody=message)
    
    # Wait until all of the travel time outputs are ready
    waitCount = 0
    prevCount = 0
    sucess = False
    while True:
        
        missing = findMissing()
        missing.to_csv(os.path.join(workingDir, "missing.csv"), index=False)
        thisCount = missing.shape[0]
            
        if thisCount>0:
        
            print(f"Waiting for runs to complete. {thisCount} of {numRuns} missing")
            
            if thisCount==prevCount:
                waitCount+=1
                
            if waitCount>maxWaitCount:
                print(f"Rescheduling {missing.shape[0]} runs.")
                break
        
            prevCount = thisCount
            
            time.sleep(sleepTime_sec)
                
        else:
            success = True
            print("Succesfully completed all ECO-PTM runs")
            break
    
# Rename config files so they can't accidentally be used again
paramsFiles = glob.glob(os.path.join(projectDir, "**", "params.txt"), recursive=True)
for f in paramsFiles:
    
    runDir = os.path.dirname(f)
    
    configFile = glob.glob(os.path.join(runDir, "*_final.yaml"))[0] 
    newConfigFile = configFile.replace("final", "alreadyRun")    
    
    try:
        os.remove(newConfigFile)
    except:
        pass
    
    os.rename(configFile, newConfigFile)

print(f"Execution time: {time.time() - startTime}")

        

