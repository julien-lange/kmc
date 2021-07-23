#!/usr/bin/python3
import sys
import subprocess
import os
import os.path
import string
import time
# import numpy as np
import csv


# Number of iterations
maxiterations = 31

logfile = "lit-log-file-benchmarks.txt"

# OUTPUT FILE
prefname = "lit-benchmarks"

# TIMEOUT (in seconds)
# cmdtimeout = 360
# cmdtimeout = 1500 # 25 min
cmdtimeout = 1500 #


    
fsmfiles = [  "tests/benchmarks/client-server-logger.txt" # (1) Client-Server-Logger
            , "tests/benchmarks/gmc-runningexample" # (2) 4 Player Game
            , "tests/benchmarks/Bargain.txt" # (3) Bargain
            , "tests/benchmarks/FilterCollaboration.txt" # (4) Filter Collaboration
            , "tests/benchmarks/AlternatingBit.txt" # (5) Alternating Bit
            , "tests/benchmarks/TPMContract.txt" # (6) TPMContract v2
            , "tests/benchmarks/SanitaryAgency.txt" # (7) Sanitary Agency
            , "tests/benchmarks/Logistic.txt" # (8) Logistic
            , "tests/benchmarks/CloudSystemV4.txt" # (9) Cloud System v4
            , "tests/benchmarks/commit-protocol.txt"# (10) Commit Protocol
            , "tests/benchmarks/elevator-extra.txt" # (11) Elevator
            , "tests/benchmarks/elevator-extra-variant.txt" # (12) Elevator-dashed
            , "tests/benchmarks/elevator-csa.txt" # (13) Elevator-directed
            , "tests/benchmarks/devsystem-fsm.txt" # (14) Dev system
]


dotfiles = [  "tests/Protocols/Fibonacci/fibo.txt" # (15) Fibonacci
            , "tests/Protocols/SAP-Negotiation/negotiate.txt" # (16) SAP-Nego
            , "tests/Protocols/SH/sh.txt" # (17) SH
            , "tests/Protocols/TravelAgency/travelagency.txt" # (18) Travel agency
            , "tests/Protocols/SMTP/smtp.txt" # (19) SMTP
            , "tests/Protocols/HTTP/http.txt"  # (20) HTTP
]



def cleanup(): 
    subprocess.call(["killall","KMC"]
                    , stdout=subprocess.PIPE
                    , stderr=subprocess.PIPE)


def runOverRange(name, filelist, filetype):
    with open(name,"a") as out:    
        write = csv.writer(out)
        with open(name+logfile, "ab") as log_file:
            for ex in filelist:
                timings = []
                nstates = ""
                ntrans = ""
                for it in range(1,maxiterations):
                    print("Running k-MC: ",ex)
                    startt = time.time() # time in seconds
                    #
                    kmccmd = subprocess.Popen(["./KMC",ex,"1","2",filetype,"--debug","+RTS","-N8"], stdout=subprocess.PIPE)
                    #
                    try:
                        kmccmd.wait(timeout=cmdtimeout)
                        endt = time.time()
                        txt = "Measured execution time: "+str(endt-startt)
                        print(txt)
                        for line in kmccmd.stdout:
                            sp = line.decode("utf-8").split("*")
                            if len(sp) > 4:
                                nstates = sp[1]
                                ntrans = sp[3]
                                log_file.write(line)
                        log_file.write((txt+"\n").encode())
                        timings.append(endt-startt)
                    except subprocess.TimeoutExpired:
                        kmccmd.kill()
                        kmccmd.wait()
                        print("KMC timedout")
                        return
                avg = sum(timings)/float(len(timings))
                write.writerow([ex,nstates,ntrans,avg]+timings)
    print("Saved in: ", name)                        


outputfile = "benchmarks-fromlit.csv"

runOverRange(outputfile, fsmfiles, "--fsm")
runOverRange(outputfile, dotfiles, "--scm")
