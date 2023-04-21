from mpi4py import MPI
import numpy as np
import os
import subprocess
from manage_files import create_sandbox, create_config_file, check_run_success
from setup_experiment import factorial_design

# start MPI communicator
comm = MPI.COMM_WORLD
size = comm.Get_size()
rank = comm.Get_rank()

# set up experimental design factors
nlevels = [2, 4, 5, 5, 5, 5, 3, 2]

if rank == 0:
    print(size)
    exp_design = factorial_design(nlevels)
    run_status = np.empty(np.prod(nlevels), dtype=bool)
    outpath = '/gpfs/scratch/vxs914/output'
    for i in range(np.prod(nlevels)):
        run_status[i] = check_run_success(exp_design[i,:], outpath)
    # split factor array for scattering
    run = np.where(run_status == 0)[0]
    exp_split = np.array_split(exp_design[run, :], size, axis=0)
    # get sizes of split buffers for sending
    split_sizes=np.zeros(size, dtype=np.int)
    for i in range(0, size, 1):
        split_sizes[i] = len(exp_split[i])
    scatter_sizes=split_sizes*len(nlevels)
    # displacements for each chunk
    displ = np.insert(np.cumsum(scatter_sizes), 0, 0)[0:-1]
else:
    # initialize variables on other workers
    exp_split = None
# broadcast sizes
#comm.Bcast(split_sizes, root=0)
#comm.Bcast(scatter_sizes, root=0)
#comm.Bcast(displ, root=0)
#print(scatter_sizes)
#print(displ)
# initialize receive buffer array
#exp_array = 10*np.ones((split_sizes[rank], len(nlevels)), dtype=np.int)
# scatter experimental design array
#comm.Scatter(exp_design, scatter_sizes, displ, MPI.INT], exp_array, root=0)
exp_split = comm.scatter(exp_split, root=0)
for lev in exp_split:
    if len(lev) != len(nlevels):
        raise ValueError('lev is the wrong size...')
    scenName = '-'.join(l for l in lev.astype(str))
    work_dir = os.getcwd()
    sandbox = create_sandbox(rank)
    os.chdir(os.path.join(sandbox, 'exe'))
    if os.path.isfile('configuration.exe'):
        os.remove('configuration.xml')
    # create configuration file in sandbox exe directory
    create_config_file(lev, sandbox)
    subprocess.call(['./run_gcam.sh', scenName])
    os.chdir(work_dir)
