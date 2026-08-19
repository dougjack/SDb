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
workingDir = "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb"

runMissing = True

queueName = "ECOPTM"
AWSconfigFile = "/Users/dougj/Documents/QEDA/AWS/DJackson_config.json"

purgeWaitTime_sec = 60
sleepTime_sec = 10

###########################################################################
# Functions
###########################################################################
        
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

outputDir = os.path.join(workingDir, "output")

# Remove existing outputs
try:
    shutil.rmtree(outputDir)
except:
    pass

# Purge queues
print(f"Purging queue {queueName}")
queueURL = sqsClient.get_queue_url(QueueName=queueName)
response = sqsClient.purge_queue(QueueUrl=queueURL["QueueUrl"])

print(f"Waiting {purgeWaitTime_sec} seconds after purging queues...")
time.sleep(purgeWaitTime_sec) 

if runMissing:
    runs = pd.read_csv(os.path.join(workingDir, "missingRuns.csv"))
else:
    runs = pd.read_excel(os.path.join(workingDir, "runs.xlsx"))
    
runIDs = runs["runID"].unique().tolist()

for i, runID in enumerate(runIDs):
    
    #if i%100==0:
    print(f"Posting run {i+1} of {len(runIDs)}")

    message = str(runID)
    response = queue.send_message(MessageBody=message)

        

