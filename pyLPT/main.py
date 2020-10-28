#!/usr/bin/env python3
#------------------------------------------------------------------------------#
# pyLPT - The Python interface of LPT
# LICENSE: MIT
#
# Copyright 2020 Gajanan Choudhary (gajananchoudhary91@gmail.com)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
"""
Main function of pyLPT, calling initialize, run, and finalize functions.
"""

import time

import pylpt_path
import pylpt as pl

################################################################################
DEBUG_LOCAL = 1

__all__ = ['main'] # The only thing from this module to import if needed.

################################################################################
def main():
    """\
    The main function of pyLPT.

    This function is the equivalent of LPT's lpt_main.F90 file
    containing the main LPT program. It calls the initialize,
    run, and finalize functions of LPT.
    """
    pylpt_comm_init = pl.pylpt_comm_module.pylpt_comm_init
    pylpt_comm_final = pl.pylpt_comm_module.pylpt_comm_final
    pylpt_read = pl.pylpt_read_module.pylpt_read
    pylpt_drog_initialize = pl.pylpt_drog.pylpt_drog_initialize
    pylpt_drog_timestep = pl.pylpt_drog.pylpt_drog_timestep

    print("\nRunning LPT using its Python interface, pyLPT\n")

    print("Initializing pyLPT")
    t0 = time.time()
    pylpt_comm_init()

    t1 = time.time()
    print("Reading input files")
    pylpt_read()

    t2 = time.time()
    print("Performing initial computations")
    pylpt_drog_initialize()

    t3 = time.time()
    print("Running pyLPT")
    pylpt_drog_timestep()

    t4 = time.time()
    print("Finalizing pyLPT")
    pylpt_comm_final()

    tInit = t1-t0
    tRead = t2-t1
    tDrogInit = t3-t2
    tRun = t4-t3
    tTot = t4-t0

    print("Initialize time = {0}".format(tInit))
    print("Input read time = {0}".format(tRead))
    print("DROG init time  = {0}".format(tDrogInit))
    print("Run time        = {0}".format(tRun))
    print("Finalize time   = {0}".format(tFin))
    print("Total time      = {0}".format(tTot))

    print("\nFinished running pyLPT")

################################################################################
if __name__ == '__main__':
    main()

