#!/usr/bin/python

import matplotlib.pyplot as plt
from pylab import * 
import numpy as np
import csv
import string
import os
from matplotlib.ticker import ScalarFormatter 
from scipy.optimize import curve_fit


filetype = ".eps"
ticksfontsize = 12
axisfontsize = 15
legentfontsize = 15
mymarkersize=5


def fitfunc(x, a, b, c ):
    return a * np.power(b, x) - c

def mkPlot(bfile,outpath):
    tab = np.loadtxt(bfile,
                         usecols=(4,5),
                         unpack = True,
                         delimiter = ',',
                         dtype = float
                         )

    stab = tab[0].argsort()
    final = tab[:,stab]
    tr,tk = final
    # maxtr = max(tr)

    
    plt.figure(figsize=(9, 9))    

    fix, ax = plt.subplots()


    linx = np.array(tr)
    liny = np.array(tk)
    lspace = np.linspace(0,max(tr))


    popt, pcov = curve_fit(fitfunc, linx, liny, bounds=(0, [20, 2, 100]))

    print("Fitted curve: "+str(popt))
    # print(pcov)

   
    
    ax.plot(lspace, fitfunc(lspace, *popt),color='orange')


    comps = bfile.split("-")
    xmin = comps[3]
    xmax = comps[4]
    ymin = comps[6]
    ymax = comps[7]
    pmin = comps[9]
    pmax = comps[10]

    preflegend = "Time with "
    legend = "NA"
    if int(xmin) != (int(xmax)-1):
        legend = "k="+ymin+", n=1..10, m="+pmin
    if int(ymin) != (int(ymax)-1):
        legend =  "k=2..100, n="+xmin+", m="+pmin
    if int(pmin) != (int(pmax)-1):
        legend = "k="+ymin+", n="+xmin+", m=1..26"
    
    ax.plot(tr,tk,marker='.',markersize=mymarkersize,linestyle='None',color='blue')
    


    plt.yticks(fontsize=ticksfontsize)
    plt.xticks(rotation=40, ha='right', fontsize=ticksfontsize)
    

    
    plt.legend(
        [r'$F(x)=%5.1f * %5.4f^x -%5.1f$' % tuple(popt),
            preflegend+legend], loc='best',fontsize=legentfontsize)

    plt.ylabel('Time (seconds)',fontsize=axisfontsize)

    plt.yscale('linear')

    plt.xlabel( r'Number of transitions in $\mathit{RTS}_k(S)$',fontsize=axisfontsize)
    plt.xscale('linear')


    posfifx = '-lin'+filetype
    plt.ticklabel_format(style='sci', axis='x',useOffset=True)


    plt.rc('grid', linestyle='dotted', color='gray')

    plt.grid()

    plt.savefig('./plots/'+outpath #bfile.replace(".","-")+posfifx
                    , dpi=300
                    , bbox_inches="tight"
                    , frameone=False)

    # plt.show()


if not os.path.exists('./plots'):
    os.makedirs('./plots')
    
i = 0
for f in os.listdir("./"):
    if (f.startswith("parametrised-benchmarks-")) and (f.endswith(".csv")):
        ostr = f+"plot-"+string.ascii_lowercase[i]+filetype
        print("Converting "+f+" to "+ostr)
        mkPlot(f,ostr)
        i += 1
