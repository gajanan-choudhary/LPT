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
Context module for adding system path for unit tests.
"""
import os
import sys
pyLPT_root=os.path.join(
        os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
            os.path.abspath(__file__)
            ))))
        )
sys.path.insert(0, os.path.abspath(pyLPT_root))

#Trigger addition of pylpt library path to system
from pyLPT import pylpt_path

import pylpt as pl
from pylpt import pylpt_main as pmain
from pylpt import utilities as pu
from pylpt import pylpt_comm_module as pc
from pylpt import pylpt_data_module as pd
from pylpt import pylpt_read_module as pr
from pylpt import pylpt_write_module as pw
from pylpt import pylpt_print_module as pp
from pylpt import pylpt_oil as po
from pylpt import pylpt_drog as pdrog

__all__ = ['pl', 'pmain', 'pu', 'pc', 'pd', 'pr', 'pw', 'pp', 'po', 'pdrog',]

