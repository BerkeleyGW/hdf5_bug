# `hdf5_bug`

Minimal example that shows a bug in the HDF5 library in Cori's burst buffer

# Compiling

Just type `make`. The `Makefile` is configured to use the latests `hdf5-parallel/1.10.1` library on Cori.

# Running

1. Allocate an interactive queue for the `knl` partition:
   `salloc --qos=interactive -t 01:00:00 -C knl,quad,cache -N 1 -S 4 --bbf=bbf.conf`
   You can use the provided `bbf.conf` file.
2. Go to the BB directory and run the test, where `${HDF5_BUG_DIR}` is the folder where you compiled the code:
   ```shell
   export HDF5_USE_FILE_LOCKING=FALSE
   cd $DW_JOB_STRIPED
   ${HDF5_BUG_DIR}/test_bug.py
   display hdf5_bug.pdf
   ```
   Take note at the files that give errors, .e.g.:
   ```shell
   cat output_10000000_1.log
    dset_size =     10000000
    num_dsets =            1
    Creating file output.h5
    Creating dataset dset_0001
    Opening file output.h5
   HDF5-DIAG: Error detected in HDF5 (1.10.1) thread 0:
     #000: H5F.c line 586 in H5Fopen(): unable to open file
       major: File accessibilty
       minor: Unable to open file
     #001: H5Fint.c line 1384 in H5F_open(): unable to read superblock
       major: File accessibilty
       minor: Read failed
     #002: H5Fsuper.c line 530 in H5F__super_read(): truncated file: eof = 33554432, sblock->base_addr = 0, stored_eof = 40002048
       major: File accessibilty
       minor: File has been truncated
    HDF5 error:          -1
   1
   ```
3. Repeat the test in the scratch folder. There should be no errors.
