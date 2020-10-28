!------------------------------------------------------------------------------!
! pyLPT - The Python interface of LPT
! LICENSE: MIT
!
! Copyright 2020 Gajanan Choudhary (gajananchoudhary91@gmail.com)
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!------------------------------------------------------------------------------!
! File: lpt_python.F90
! Author: Gajanan K Choudhary, Postdoctoral Fellow
! Location: The University of Texas at Austin
! Date created: 10/26/2020
! Purpose: Wrapper to generate a Python interface to LPT using 'f2py'
! Dependencies : (1) srcpy/py*.F90 files translating src *.F files to f2py
!------------------------------------------------------------------------------!
!
!------------------------------------------------------------------------------!
! Files dependent on the LPT source code, i.e., src *.F files
#include "pylpt_comm.F90"
#include "pylpt_data.F90"
#include "pylpt_drog.F90"
#ifdef KDTREE
#include "pylpt_kdtree.F90"
#endif
#include "pylpt_netcdf.F90"
#include "pylpt_oil.F90"
#include "pylpt_print.F90"
#include "pylpt_read.F90"
#include "pylpt_write.F90"

#include "pylpt_main.F90"

!------------------------------------------------------------------------------!
! The utilities module contains helper variables for the Python side of the
! LPT Python interface. For example, one way to know if DEBUG compiler
! flag was passed to gfortran is by looking at the debug variable below on the
! Python side.

module utilities
    implicit none

    logical, parameter :: on = .true.
    logical, parameter :: off = .false.
#ifdef MPI
    logical, parameter :: messg = .true.
#else
    logical, parameter :: messg = .false.
#endif
#ifdef DEBUG
    logical, parameter :: debug = .true.
#else
    logical, parameter :: debug = .false.
#endif

end module utilities

!------------------------------------------------------------------------------!

