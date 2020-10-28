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
Unit test for pylpt.pylpt_comm_module.
"""
from __future__ import division
import unittest
import numpy as np

if __name__ == '__main__':
    # Executing test directly over command line
    from unitcontext import *
else:
    # Executing test as module
    from .unitcontext import *

################################################################################
LOCALDEBUG = 1

################################################################################
def setUpModule():
    """Initialize MPI."""
    if (pu.messg==pu.on):
        pc.pylpt_comm_init()
    if (LOCALDEBUG != 0):
        print("Processor {} of {} initialized".format(pc.myrank, pc.numranks))

#------------------------------------------------------------------------------#
def tearDownModule():
    """Finalize MPI."""
    if (LOCALDEBUG != 0):
        print("Processor {} of {} finalizing".format(pc.myrank, pc.numranks))
    if (pu.messg==pu.on):
        pc.pylpt_comm_final()

################################################################################
@unittest.skipIf(pu.messg==0, 'Compiled without MPI')
class Test_MPI_Simple_Communications(unittest.TestCase):
    """Unit test for distributing integer and double arrays."""

    #--------------------------------------------------------------------------#
    def test_int_distribute(self):
        """Test Distribution of integer array."""

        # Setup myint array on PE 0 for broadcasting to all PEs
        nmsgs = 10  # Some random number
        if (pc.myrank == 0):
            self.myintarr = np.asfortranarray(range(nmsgs), dtype=int)
        else:
            self.myintarr = np.asfortranarray(np.zeros(nmsgs), dtype=int)
        # Broadcasted answer.
        self.ans = np.asfortranarray(range(nmsgs), dtype=int)

        if (LOCALDEBUG != 0):
            print("Before: Proc {}".format(pc.myrank), self.myintarr)
        pc.dummyc, self.myintarr, pc.dummyr = pc.pylpt_comm_distribute(
                pc.dummyc, self.myintarr, pc.dummyr, "I", "Int BCast failed")
        if (LOCALDEBUG != 0):
            print("After : Proc {}".format(pc.myrank), self.myintarr)

        try:
            np.testing.assert_array_equal(self.myintarr, self.ans)
            res = True
        except AssertionError as err:
            res = False
            print (err)
        self.assertTrue(res)

    #--------------------------------------------------------------------------#
    def test_dbl_distribute(self):
        """Test Distribution of double array."""

        # Setup mydbl array on PE 0 for broadcasting to all PEs
        nmsgs = 23  # Some random number
        if (pc.myrank == 0):
            self.mydblarr = np.asfortranarray(range(nmsgs), dtype=float)*np.pi
        else:
            self.mydblarr = np.asfortranarray(np.zeros(nmsgs), dtype=float)
        # Broadcasted answer.
        self.ans = np.asfortranarray(range(nmsgs), dtype=float)*np.pi

        if (LOCALDEBUG != 0):
            print("Before: Proc {}".format(pc.myrank), self.mydblarr)
        pc.dummyc, pc.dummyi, self.mydblarr = pc.pylpt_comm_distribute(
                pc.dummyc, pc.dummyi, self.mydblarr, "R", "Double BCast failed")
        if (LOCALDEBUG != 0):
            print("After : Proc {}".format(pc.myrank), self.mydblarr)

        try:
            np.testing.assert_array_equal(self.mydblarr, self.ans)
            res = True
        except AssertionError as err:
            res = False
            print (err)
        self.assertTrue(res)

################################################################################
if __name__ == '__main__':
    unittest.main()

