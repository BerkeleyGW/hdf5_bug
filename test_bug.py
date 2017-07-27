#!/usr/bin/env python

import sys
import os
import numpy as np
import matplotlib.pyplot as plt

sizes = [1000000, 2000000, 4000000, 6000000, 8000000, 10000000]
num_dsets = [1, 2, 4, 6]
app = os.path.join(os.path.dirname(sys.argv[0]), 'hdf5_bug.x')
fname_h5 = 'output.h5'

file_sizes = np.zeros((len(sizes),len(num_dsets)))
statuses = np.zeros_like(file_sizes)
for isize, size in enumerate(sizes):
    for inum, num_dset in enumerate(num_dsets):
        if os.path.exists(fname_h5):
            os.unlink(fname_h5)
        status = os.system('{0} {1} {2} &> output_{1}_{2}.log'.format(app, size, num_dset))
        statinfo = os.stat(fname_h5)
        file_size = float(statinfo.st_size)/1024./1024.
        statuses[isize,inum] = status
        file_sizes[isize,inum] = file_size
        print '{:10d} {:4d} {:9.3f} {:3d}'.format(size, num_dset, file_size, status)

plt.figure(figsize=(7,4))
plt.spy(statuses, origin='upper', aspect='auto')
plt.yticks(np.arange(len(sizes)), sizes)
plt.xticks(np.arange(len(num_dsets)), num_dsets)

plt.title('Problematic files')
plt.ylabel('Dataset size')
plt.xlabel('Number of datasets')
plt.tight_layout()
plt.savefig('hdf5_bug.pdf')
