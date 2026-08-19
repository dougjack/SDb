# -*- coding: utf-8 -*-
"""
Launch multiple instances of runSDbWorker.py

Doug Jackson
doug@QEDAconsulting.com
"""
import os
import subprocess

workingDir = "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb"

# Specify the number of instances to launch
numInstances = 3

# Activate conda environment before running runEPTMscenarios.py
activateCondaEnv = True
condaEnv = "spyder-env"

instanceBatDir = os.path.join(workingDir, "instanceBatchFiles")
os.makedirs(instanceBatDir, exist_ok=True)

os.chdir(instanceBatDir)

for i in range(numInstances):
    # Create a batch file to launch the instance
    with open(f"launchInstance_{i}.bat", "w") as fH:
        if activateCondaEnv:
            print(f"call conda activate {condaEnv}", file=fH)
        
        print(f"call python ../runSDbWorker.py --instanceNum {i}", file=fH)

    command = ["start", "cmd", "/k", f"launchInstance_{i}.bat"]
    print("="*100)
    print(" ".join(command))
    subprocess.call(command, shell=True)