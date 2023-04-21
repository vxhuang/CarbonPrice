#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Apr 11 12:25:02 2020

@author: vxs914
"""


from mpi4py import MPI
import numpy as np
import os
from write_query_xh import write_batch_query

# start MPI communicator
comm = MPI.COMM_WORLD
size = comm.Get_size()
rank = comm.Get_rank()


if rank == 0:
    nlevels = [2, 1, 5, 5, 5, 5, 3, 2]
    runs = np.loadtxt('runs.csv', delimiter=',', dtype=float).astype(int)
    idx = np.where(runs[:, len(nlevels)] == 1)
    completed = runs[idx[0], 0:len(nlevels)]
    split = np.array_split(completed, size, axis=0)
else:
    split = None

split = comm.scatter(split, root=0)
sandbox = '/gpfs/group/wvp5117/default/GCAM_xh/queries'
jarlib = os.getenv('JARS_LIB')
classpath = f'{jarlib}:/storage/work/xbh5089/GCAM/ensemble_workflow/jar/ModelInterface.jar'

for lev in split:
    scenName = '-'.join(l for l in lev.astype(str))
  
    query_out = os.path.join('/gpfs/group/wvp5117/default/GCAM_xh/query_out', f'query_01252023_{scenName}.csv')
    if os.path.isfile(query_out):
        pass
    else:
        write_batch_query(lev, sandbox, '/gpfs/group/wvp5117/default/GCAM_xh/query_out')
        javacmd = f'java -cp {classpath} ModelInterface/InterfaceMain -b {sandbox}/query_01252023_{scenName}.xml'
        os.system(javacmd)
