import os
import subprocess
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns


def main():

    c = [
        'gfortran',
        '../../src/FFT.f90',
        'fft.f90',
        '-O2',
        '-xHost',
        '-o',
        'a.out'
    ]

    subprocess.check_call(c)
    subprocess.check_call(['./a.out'])


    fig=plt.figure()
    ax = fig.add_subplot(111)
    ar = np.loadtxt("fft_bench_result.txt")
    df = pd.DataFrame(ar,columns=["data_length","ACF_FFT","ACF_non_FFT","ACF_non_FFT_vec"])
    df.plot(x="data_length",ax=ax,style=[".-",".-",".-"])
    ax.set_ylabel('time(sec)')
    plt.xscale("log")
    plt.yscale("log")
    plt.show()

if __name__ == "__main__":
    os.chdir(Path(__file__).parent)
    main()
