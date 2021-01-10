import subprocess
import os
from pathlib import Path

def main():

    c = [
        'gfortran',
        '../src/FFT.f90',
        'fft.f90',
        '-Wall',
        '-pedantic',
        '-fbounds-check',
        '-O',
        '-ffpe-trap=invalid,zero,overflow',
        '-fbacktrace',
        '-o',
        'a.out'
    ]

    subprocess.check_call(c)
    subprocess.check_call(['./a.out'])

if __name__ == "__main__":
    os.chdir(Path(__file__).parent)
    main()