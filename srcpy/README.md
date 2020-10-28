# pyLPT - The Python interface of LPT

pyLPT is LPT itself wrapped into Python. It allows users to access
LPT's variables and subroutines directly in Python. LPT is compiled into
a shared library, `pylpt`, that can be imported into Python as a module.
`pyLPT` is a package that imports `pylpt` and decorates it Pythonically
for easily using LPT in Python. [f2py](https://numpy.org/doc/stable/f2py/),
which is a part of [NumPy](https://numpy.org/), is the tool used to
implement LPT's Python interface.

### Motivation
`pyLPT` should expand uses of LPT. Interested folks might be able to use LPT
with the plethora of Python's modern libraries, for instance.
Another possible use is multi-software coupling -- interested users may be able
to couple LPT with other software.

## Compiling and setting up things

### CMake way
Currently, users should be able to use either the GNU Compiler Collection or
Intel compilers for compiling the code. The general CMake compilation workflow
is:
```bash
cd <lpt_root_dir>
mkdir build
cd build/
cmake  <CMake_build_options>  ..
make
```
The LPT code may be built with or without the Python interface. This README is
only concerned with the Python interface, `pylpt`, and the Python package built
on top of it, `pyLPT`. These are built using the CMake option given next.

#### Python Interface build
For building the Python shared library, `pylpt`, which is used by the Python
interface/package, `pyLPT`, pass the `-DPYTHON_INTERFACE=ON` flag to CMake. On
successful compilation, the directory, `<lpt_root_dir>/pyLPT:$LD_LIBRARY_PATH`,
should contain two shared libraries named `liblpt_shared.so` and `pylpt*.so` in
case of builds on the Linux operating system. `pylpt*.so` is the Python
interface that is accessible in Python by running `import pylpt`. `pylpt`
depends on `liblpt_shared.so`, which is the shared library created out of the
original Fortran source code of LPT. Because of that, the `LD_LIBRARY_PATH`
environment variable must be set so that `pylpt*.so` can find and use
`liblpt_shared.so`.

The steps involved are given below:
```bash
cd <lpt_root_dir>
mkdir build
cd build/
cmake -DPYTHON_INTERFACE=ON ..
make
export LD_LIBRARY_PATH="<lpt_root_dir>/pyLPT:$LD_LIBRARY_PATH"
```

##### Optional: `pip install`
After compiling the Python interface, you can pip install `pyLPT` by running
`python3 -m pip install .` from `<lpt_root_dir>`. To uninstall the package, you
can run `python3 -m pip uninstall pyLPT`.

#### Parallel/MPI build
For building the `pyLPT` in parallel, only `openmpi` has been tried out till
now. In order to compile the Python shared library/interface, pass the
`-DUSE_MPI=ON` flag to cmake.

#### NetCDF build
For building the `LPT` binary or `pyLPT` with the NetCDF libraries, pass the
`-DUSE_NETCDF=ON` flag to cmake. Note that CMake needs to be able to find HDF5
and NetCDF libraries in order for the build to be successful.

#### Debug build
For building the `LPT` binary or `pyLPT` in debug mode, pass the
`-DBUILD_DEBUG=ON` flag to cmake.

### Legacy Makefile way
To Do

## Using the LPT Python Interface
There two ways of using the Python interface: `pylpt` and `pyLPT`.
`pylpt` is the Make/CMake-compiled Python module in the `pylpt*.so` file,
and `pyLPT` is a Python package in the [pyLPT](../pyLPT) folder that
imports `pylpt` and Pythonically organizes the imported library (under
development). 

The first way of using the Python interface is by directly importing the
compiled shared library into Python, `import pylpt`. In this case, the main LPT
program can be run as follows, for example:
```python3
import pylpt
pylpt.pyltp_main.lpt_main() # Main LPT program.
# Or you could do the following for better readability:
# from pylpt import pylpt_main as pmain
# pmain.lpt_main()
```

The second way is using by importing the `pyLPT` package, i.e., `import pyLPT`:
```python3
import pyLPT
#import pyLPT.pylpt # This is the same as the first way of accessing LPT.
pyLPT.main()
```

The recommended way of importing `LPT` in Python is the second way, i.e.,
`import pyLPT`. This is intended to be the dominant way that the library will be
used in the future as the `f2py`-compiled `pylpt` shared library is decorated
Pythonically inside the `pyLPT` package. Use the first way, i.e.,
`import pylpt`, only if you know what you are doing.

Currently, though, `pyLPT` mainly only contains minor, incomplete unit tests and
a `main()` function that is the equivalent of the LPT main/driver program
defined in [LPT's lpt\_main.F90 file](../src/lpt_main.F90), that allows you to
invoke the main function on the Linux command line using `python3 pyLPT` or
`python3 -m pyLPT`. If MPI build is also enabled, then you can run
`mpirun -np <numprocs> python3 -m pyLPT` from the command line as well.
These commands are equivalent to running the original LPT binary `lpt_serial`
and `lpt_parallel` from the command line.

### Notes
* For now, do not move `liblpt_shared.so` and `pylpt*.so` from the 
  `<lpt_root_dir>/pyLPT` directories to other locations. It is better to use
  symlinks/shortcuts instead of moving the files around.
* Run the `pyLPT` unit tests located at
  [<lpt_root_dir>/pyLPT/tests/unit/](../pyLPT/tests/unit) using the appropriate.
  Make sure they are all successful. Run the command,
  `python3 -m unittest discover`, from the `pyLPT` folder or <lpt_root_dir>
  for that automatically running serial build tests. Manually run all tests in
  case of parallel builds for now. Do not use the library if the tests fail. To
  run the tests, you will need to set the `LD_LIBRARY_PATH` environment
  variable, as mentioned in previous sections.

## License
`pyLPT` is licensed under the [MIT License](../LICENSE), same as that of the
original LPT source code.

