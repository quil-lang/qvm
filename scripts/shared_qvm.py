#!/usr/bin/env python

### shared_qvm.py
###
### Author: Robert Smith
###
### Copyright (c) 2017 Rigetti Computing

### This file shows a minimal example of how to use the --shared
### option with QVM from Python.
from __future__ import print_function
import posix_ipc as pos
import mmap
import ctypes
import numpy as np

import socket
import json
import sys


from pyquil.api import QVMConnection
from pyquil.quil import Program
from pyquil.gates import X

def query_length_offset(name):
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect('/tmp/' + name)
    s.sendall("?")
    message, peer = s.recvfrom(4096)
    length, offset = message.split(',')
    return int(length), int(offset)

def retrieve_wavefunction(name):
    length, offset = query_length_offset(name)
    shm = pos.SharedMemory(name)
    m = mmap.mmap(shm.fd, shm.size)
    # get the pointer to what appear to be an array of bytes
    ptr = ctypes.POINTER(ctypes.c_ubyte)(ctypes.c_void_p.from_buffer(m, offset))
    # cast to array of complex double floats
    ptr = ctypes.cast(ptr, np.ctypeslib.ndpointer(shape=(length,), dtype=np.complex128))
    return np.ctypeslib.as_array(ptr)

# Example use of this interface.
if __name__ == '__main__':
    if len(sys.argv) != 2:
        print('Syntax: shared_qvm.py <name>')
        sys.exit(1)
    name = sys.argv[1]
    cxn = QVMConnection(sync_endpoint='http://127.0.0.1:5000')
    wf = retrieve_wavefunction(name)

    print("Initial wavefunction:")
    print(wf)
    print("Initializing to W state.")
    wf[0b0000] = 0j
    wf[0b0001] = (1+0j)/np.sqrt(4)
    wf[0b0010] = (1+0j)/np.sqrt(4)
    wf[0b0100] = (1+0j)/np.sqrt(4)
    wf[0b1000] = (1+0j)/np.sqrt(4)
    print(wf)
    print("Evolving with X3X2X1X0 via QVM. Quil program is:")
    p = Program().inst([X(q) for q in range(4)])
    print(p)
    cxn.run(p, [0])
    print("Printing evolved state.")
    for b in range(len(wf)):
        if not np.isclose(wf[b], 0j):
            print("{0:04b} => {1}".format(b, wf[b]))
