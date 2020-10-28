SHELL:=/bin/bash

#compiler=gnu
compiler=intel

# Current directory
BASEPATH = $(shell pwd)
BUILDDIR := build
DEBUG := disable

# Absolute paths to the directories
SRCDIR = $(BASEPATH)/src
BINDIR = $(BASEPATH)/$(BUILDDIR)/bin
OBJDIR = $(BASEPATH)/$(BUILDDIR)/obj
DBGOBJDIR = $(BASEPATH)/$(BUILDDIR)/dbgobj
MODDIR = $(BASEPATH)/$(BUILDDIR)/mod
VPATH := $(SRCDIR)

#------------------------------------------------------------------------------#

# src files & obj files
#SRC := $(foreach x, $(SRCDIR), $(wildcard $(addprefix $(x)/*,.*F90)))
#SRC := $(wildcard $(SRCDIR)/*.F90)
SRC = lpt_comm.F90   lpt_data.F90  lpt_kdtree.F90 \
	  lpt_netcdf.F90 lpt_read.F90  lpt_write.F90 \
	  lpt_print.F90  lpt_drog.F90  lpt_oil.F90
      #lpt_main.F90

MAIN_SRC := $(SRCDIR)/lpt_main.F90
#------------------------------------------------------------------------------#

# compile marcros
TARGET_SERIAL_NAME := lpt_serial
TARGET_PARALLEL_NAME := lpt_parallel
ifeq ($(OS),Windows_NT)
	TARGET_SERIAL_NAME := $(addsuffix .exe,$(TARGET_SERIAL_NAME))
	TARGET_PARALLEL_NAME := $(addsuffix .exe,$(TARGET_PARALLEL_NAME))
endif
TARGET_SERIAL := $(BINDIR)/$(TARGET_SERIAL_NAME)
TARGET_PARALLEL := $(BINDIR)/$(TARGET_PARALLEL_NAME)

# tool marcros
#------------------------------------------------------------------------------#
ifeq ($(BUILDTYPE),serial)
  ifeq ($(compiler),intel)
    FC := ifort
    FFLAGS := -I$(MODDIR) -DVERBOSE=4 -mod $(MODDIR)
    DBGFLAG := -g -DDEBUG -check all -traceback
  endif
  ifeq ($(compiler),gnu)
    FC := gfortran
    FFLAGS := -I$(MODDIR) -DVERBOSE=4 -J$(MODDIR)
    DBGFLAG := -DDEBUG -g -O0 -fimplicit-none  -Wall \
	           -fcheck=all  -pedantic  -fbacktrace
  endif
endif
#------------------------------------------------------------------------------#
ifeq ($(BUILDTYPE),parallel)
  ifeq ($(compiler),intel)
    FC := mpifort
    FFLAGS := -I$(MODDIR) -DVERBOSE=1 -DMPI -module $(MODDIR)
    DBGFLAG := -g -traceback -check all -DDEBUG -DDEBUG_MPI
  endif
  ifeq ($(compiler),gnu)
    FC := mpif90
    FFLAGS := -I$(MODDIR) -DVERBOSE=1 -DMPI -J$(MODDIR)
    DBGFLAG := -DDEBUG -DDEBUG_MPI -g -O0 -fimplicit-none  -Wall \
	           -fcheck=all  -pedantic  -fbacktrace
  endif
endif
#------------------------------------------------------------------------------#
# Libraries and other flags
ifeq ($(NETCDF),enable)
  NETCDF_FLAGS := -DNETCDF=4 -I${NETCDFHOME}/include/ -L${NETCDFHOME}/lib \
  	            -lnetcdf -lnetcdff
  HDF5_FLAGS := -L${HDF5_LIB} -Wl,-rpath,${HDF5_LIB} -L${HDF5_LIB} -lhdf5 -lz
endif
ifeq ($(DEBUG),enable)
  FFLAGS := $(FFLAGS) $(DBGFLAG)
endif

#------------------------------------------------------------------------------#
# List of files
OBJ := $(addprefix $(OBJDIR)/, $(patsubst %.F90,%.o,$(SRC)))
DBGOBJ := $(addprefix $(DBGOBJDIR)/, $(patsubst %.F90,%.o,$(SRC)))
MOD := $(addprefix $(MODDIR)/, $(addsuffix .mod, $(notdir $(basename $(SRC)))))

DISTCLEAN_LIST := $(OBJDIR) $(DBGOBJDIR) $(MODDIR)

CLEAN_LIST := $(TARGET_SERIAL) \
              $(TARGET_PARALLEL) \
              $(TARGET_SERIAL)-debug \
              $(TARGET_PARALLEL)-debug \
			  $(DISTCLEAN_LIST)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# default rule
default: all

#------------------------------------------------------------------------------#
# Directories
$(BINDIR): $(BUILDDIR) $(DBGOBJDIR) $(OBJDIR) $(MODDIR)
	@mkdir -p $@
$(BUILDDIR):
	@mkdir -p $@
$(OBJDIR): $(BUILDDIR)
	@mkdir -p $@
$(DBGOBJDIR): $(BUILDDIR)
	@mkdir -p $@
$(MODDIR): $(BUILDDIR)
	@mkdir -p $@

#------------------------------------------------------------------------------#
# Object files
$(OBJDIR)/%.o: %.F90
	$(FC) $(FFLAGS) -c $^ -o $@
$(DBGOBJDIR)/%.o: %.F90
	$(FC) $(FFLAGS) -c $^ -o $@

#------------------------------------------------------------------------------#
# Cleaning
.PHONY: clean
clean:
	@echo CLEAN $(CLEAN_LIST)
	@rm -rf $(CLEAN_LIST)

.PHONY: distclean
distclean:
	@echo CLEAN $(DISTCLEAN_LIST)
	@rm -rf $(DISTCLEAN_LIST)

#------------------------------------------------------------------------------#
.PHONY: all
all : release debug

.PHONY: debug
debug: $(BINDIR)
	@echo -e "\n====== BUILDING DEBUG PROGRAM ======\n"
	@FFLAGS="$(FFLAGS) $(DBGFLAG)" SUFFIX="-debug" $(MAKE) BUILDTYPE=serial DEBUG=enable serial
	@FFLAGS="$(FFLAGS) $(DBGFLAG)" SUFFIX="-debug" $(MAKE) BUILDTYPE=parallel DEBUG=enable parallel
.PHONY: release
release : $(BINDIR)
	@echo -e "\n====== BUILDING RELEASE PROGRAM ======\n"
	@FFLAGS="$(FFLAGS) $(DBGFLAG)" $(MAKE) BUILDTYPE=serial DEBUG=disable serial
	@FFLAGS="$(FFLAGS) $(DBGFLAG)" $(MAKE) BUILDTYPE=parallel DEBUG=disable parallel
	
#------------------------------------------------------------------------------#
.PHONY: serial
serial : $(TARGET_SERIAL_NAME)$(SUFFIX)
.PHONY: parallel
parallel : $(TARGET_PARALLEL_NAME)$(SUFFIX)

.PHONY: $(TARGET_SERIAL_NAME)$(SUFFIX)
$(TARGET_SERIAL_NAME)$(SUFFIX) : $(TARGET_SERIAL)$(SUFFIX)
.PHONY: $(TARGET_PARALLEL_NAME)$(SUFFIX)
$(TARGET_PARALLEL_NAME)$(SUFFIX) : $(TARGET_PARALLEL)$(SUFFIX)

#------------------------------------------------------------------------------#
ifeq ($(MAKELEVEL),0)
  $(TARGET_SERIAL)$(SUFFIX) : $(BINDIR)
	@echo "--------------------------------------------------------------------"
	@echo "--------------------------------------------------------------------"
	$(MAKE) BUILDTYPE=serial DEBUG=$(DEBUG) $@
  $(TARGET_PARALLEL)$(SUFFIX) : $(BINDIR)
	@echo "--------------------------------------------------------------------"
	@echo "--------------------------------------------------------------------"
	$(MAKE) BUILDTYPE=parallel DEBUG=$(DEBUG) $@
else
  ifeq ($(DEBUG),enable)
    OBJ_DEPEND := $(DBGOBJ)
  else
    OBJ_DEPEND := $(OBJ)
  endif
  $(TARGET_SERIAL)$(SUFFIX) :: $(MAIN_SRC) $(OBJ_DEPEND)
	@echo "--------------------------------------------------------------------"
	@echo "--------------------------------------------------------------------"
	$(FC) $(FFLAGS) $(NETCDF_FLAGS) $(HDF5_FLAGS) -o $@ $?
  $(TARGET_PARALLEL)$(SUFFIX) :: $(MAIN_SRC) $(OBJ_DEPEND)
	@echo "--------------------------------------------------------------------"
	@echo "--------------------------------------------------------------------"
	$(FC) $(FFLAGS) $(NETCDF_FLAGS) $(HDF5_FLAGS) -o $@ $?
endif


################################################################################
# Original Makefile built for TACC Lonestar Intel compilers
#ifeq ($(PLATFORM),lonestar-intel)
#  ifeq ($(BUILDTYPE),lpt_serial)
#    COMPILER := ifort
#    FLAGS := -DVERBOSE=4 -DDEBUG -check all -traceback
#  endif
#  ifeq ($(BUILDTYPE),lpt_parallel)
#    COMPILER := mpif90
#    FLAGS := -DVERBOSE=1 -DMPI # -traceback # -check all -DDEBUG # -DDEBUG_MPI
#  endif
#  ifeq ($(NETCDF),enable)
#    NETCDF_FLAGS := -DNETCDF=4 -I${TACC_NETCDF_INC} -L${TACC_NETCDF_LIB} -lnetcdf -lnetcdff -L${TACC_HDF5_LIB} -Wl,-rpath,${TACC_HDF5_LIB} -L${TACC_HDF5_LIB} -lhdf5 -lz
#    HDF5_FLAGS :=
#  endif
#endif
#
#SOURCE_FILES = lpt_comm.F90 lpt_data.F90 lpt_kdtree.F90 lpt_netcdf.F90 lpt_read.F90 lpt_write.F90 lpt_main.F90 lpt_print.F90 lpt_drog.F90 lpt_oil.F90
#SOURCE_OBJECTS := $(patsubst %.F, %.o, $(SOURCE_FILES) )
#
#%.o : %.F90
#	$(COMPILER) $(FLAGS) -c $< 
#
#all : lpt_serial lpt_parallel
#
#ifeq ($(MAKELEVEL),0)
#  lpt_serial :
#	$(MAKE) BUILDTYPE=lpt_serial $@
#  lpt_parallel :
#	$(MAKE) BUILDTYPE=lpt_parallel $@
#else
#  lpt_serial :: $(SOURCE_OBJECTS)
#	$(COMPILER) $(FLAGS) $^ $(NETCDF_FLAGS) $(HDF5_FLAGS) -o $@
#  lpt_parallel :: $(SOURCE_OBJECTS)
#	$(COMPILER) $(FLAGS) $^ $(NETCDF_FLAGS) $(HDF5_FLAGS) -o $@
#endif
#
#clean :
#	rm -f *.mod lpt_serial lpt_parallel
