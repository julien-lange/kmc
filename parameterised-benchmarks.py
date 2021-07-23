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
maxiterations = 4

logfile = "log-file-benchmarks.txt"
tmpfile = "tmp-cfsm.txt"

# OUTPUT FILE
prefname = "parametrised-benchmarks"

# TIMEOUT (in seconds)
# cmdtimeout = 360
# cmdtimeout = 1500 # 25 min
cmdtimeout = 1500 #



def cleanup(): 
    subprocess.call(["killall","KMC"]
                    , stdout=subprocess.PIPE
                    , stderr=subprocess.PIPE)


def runOverRange(minx, maxx, miny, maxy, minp, maxp, cycle):
    name = prefname+"-X-"+str(minx)+"-"+str(maxx)+"-Y-"+str(miny)+"-"+str(maxy)+"-P-"+str(minp)+"-"+str(maxp)+"-"+str(cycle)+".csv"
    with open(name,"w") as out:    
        write = csv.writer(out) 
        with open(name+logfile, "wb") as log_file:
            for x in range(minx,maxx):
                for y in range(miny,maxy):
                    for p in range(minp,maxp):
                        if cycle:
                            c = "1"
                        else:
                            c = "0"
                        # GENERATE EXAMPLE
                        gencmd = subprocess.Popen(["./GenCFSMs",str(x),str(y),str(p),c], stdout=subprocess.PIPE)
                        with open(tmpfile, "wb") as text_file:
                            for line in gencmd.stdout:
                                text_file.write(line)
                        
                        timings = []
                        nstates = ""
                        ntrans = ""
                        for it in range(1,maxiterations):
                            print("Running k-MC: ",str(x), str(y), str(p), "k:",str(y))
                            startt = time.time() # time in seconds
                            kmccmd = subprocess.Popen(["./KMC",tmpfile,"1",str(y),"--fsm","--debug","+RTS","-N8"], stdout=subprocess.PIPE)
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
                                # write.writerow([x,y,p,nstates,ntrans,endt-startt])
                            except subprocess.TimeoutExpired:
                                kmccmd.kill()
                                kmccmd.wait()
                                print("KMC timedout")
                                return
                        avg = sum(timings)/float(len(timings))
                        write.writerow([x,y,p,nstates,ntrans,avg]+timings)
                            



# Measure growing k with 10 peers
runOverRange(1, 2, 2, 101, 5, 6, False)

# Measure growing number of peers with k=10
runOverRange(1, 2, 10, 11, 1, 50, False)

# Measure growing number of branchings with k=2 and peers=2
runOverRange(1, 50, 2, 3, 1, 2, False)


