from posixpath import join
from pathlib import Path
import subprocess
import sys, os
from typing import Tuple, List, Callable
import functools

vcy_exe = "../src/vcy.exe"
timeout = 60

mb = 1024 * 1024
n_values = [1, 2, 4, 8, 16, 32, 64, 128]

class VcyError(Exception):
    def __init__(self, msg) -> None:
        super().__init__(msg)
        self.msg = msg

def mk_files(n):
    with open('A.txt', 'w') as f:
        f.write('A' * n)
    with open('B.txt', 'w') as f:
        f.write('B' * n)

seq_times = []
par_times = []
for n in n_values:
    mk_files(mb * n)
    
    command_seq = [vcy_exe, 'interp', '--time', '--prover', 'cvc5', '--timeout', str(timeout), '--force-sequential', '../examples/io5.vcy']
    command_par = [vcy_exe, 'interp', '--time', '--prover', 'cvc5', '--timeout', str(timeout), '../examples/io5.vcy']

    def f(command : List[str], floatize : bool):
        popen = subprocess.Popen(
            command, universal_newlines=True,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            env={'LD_LIBRARY_PATH':'../veracity/src'}
        )
        out, err = popen.communicate()
        if err:
            raise VcyError(err)
        try:
            return float(out) if floatize else out
        except TypeError:
            raise TypeError(f'Output {out} could not be parsed into a float')
    
    seq_time = f(command_seq, True)
    par_time = f(command_par, True)
    
    seq_times.append(seq_time)
    par_times.append(par_time)

with open(f'./iobench.csv', 'w') as file:
    res1 = map(lambda l: map(str, l), [n_values, seq_times, par_times])
    res2 = map(','.join, res1)
    file.write('\n'.join(res2))
