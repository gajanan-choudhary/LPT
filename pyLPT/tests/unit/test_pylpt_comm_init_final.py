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
Unit test only for MPI initialize and finalize in pylpt.pylpt_comm_module.
"""
import unittest

if __name__ == '__main__':
    # Executing test directly over command line
    from unitcontext import *
else:
    # Executing test as module
    from .unitcontext import *

################################################################################
LOCALDEBUG = 0

################################################################################
@unittest.skipIf(__name__!='__main__', 'Compiled without MPI')
class Test_Comm_Init_Finalize(unittest.TestCase):
    """Unit test for pymsg_init and pymsg_fini functions."""

    def test_main(self):
        """Test MPI initialize and finalize."""
        # Initialize MPI
        if (pu.messg):
            pc.pylpt_comm_init()
        if (LOCALDEBUG != 0):
            print("Processor {} of {} initialized".format(
                pc.myrank, pc.numranks))

        # Finalize MPI
        if (LOCALDEBUG != 0):
            print("Processor {} of {} finalizing".format(
                pc.myranks, pc.numranks))
        if (pu.messg):
            pc.pylpt_comm_final()

################################################################################
if __name__ == '__main__':
    unittest.main()

