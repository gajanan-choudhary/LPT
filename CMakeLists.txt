# Top Level CMakeLists.txt -- Taken from github.com/georgiastuart/LPT, modified.
cmake_minimum_required(VERSION 3.10)

project(lpt LANGUAGES Fortran)

enable_language(Fortran)


option(USE_MPI "Build parallel version of LPT" OFF)
option(USE_NETCDF "Build with NETCDF" OFF)
option(BUILD_DEBUG "Build in debug mode" OFF)
option(PYTHON_INTERFACE "Build the Python Interface of LPT using f2py" OFF)


# CMake settings
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod")

# Source Files
file(GLOB SOURCES "src/*.F90")
list(FILTER SOURCES EXCLUDE REGEX ".*src/lpt_main.F90$")

#------------------------------------------------------------------------------#
# Build Options
#------------------------------------------#
# Build - Debug
if(BUILD_DEBUG)
    add_definitions(-g -O0 -DDEBUG)
    set(FFLAGS ${FFLAGS} -g -O0 -DDEBUG)
    if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        add_definitions(-fimplicit-none -Wall -fcheck=all -pedantic -fbacktrace)
    elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
        add_definitions(-check all -traceback)
    elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Clang")
        # using Clang -- To Do
    elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "MSVC")
        # using Visual Studio C++ -- To Do
    endif()
else(BUILD_DEBUG)
    set(FFLAGS ${FFLAGS} -Wno-unused-variable)
    set(FFLAGS ${FFLAGS} -Wno-unused-function)
    set(FFLAGS ${FFLAGS} -Wno-unused-parameter)
endif(BUILD_DEBUG)

#------------------------------------------#
# Build - NetCDF
if(USE_NETCDF)
    set (NETCDF_F90 "YES")
    find_package(NetCDF REQUIRED)
    find_package(HDF5 REQUIRED COMPONENTS Fortran)
    include_directories(${NETCDF_INCLUDES} ${HDF5_INCLUDE_DIRS})
endif(USE_NETCDF)

#------------------------------------------#
# Build - MPI
if(USE_MPI)
    find_package(MPI REQUIRED Fortran)
    include_directories(${MPI_Fortran_INCLUDE_PATH})
    set(CMAKE_Fortran_COMPILER "${MPI_Fortran_COMPILER}")
    set(CMAKE_C_COMPILER "${MPI_C_COMPILER}")
    set(CMAKE_CXX_COMPILER "${MPI_CXX_COMPILER}")
    add_definitions(-DMPI -DVERBOSE=1)
    set(FFLAGS ${FFLAGS} -DMPI -DVERBOSE=1)
    if(BUILD_DEBUG)
        add_definitions(-DDEBUG_MPI)
        set(FFLAGS ${FFLAGS} -DDEBUG_MPI)
    endif(BUILD_DEBUG)
else()
    add_definitions(-DVERBOSE=4)
    set(FFLAGS ${FFLAGS} -DVERBOSE=4)
endif(USE_MPI)

#------------------------------------------------------------------------------#
# Targets
#------------------------------------------#
# Static Library
add_library(lpt_static_obj OBJECT ${SOURCES})
add_library(lpt_static STATIC $<TARGET_OBJECTS:lpt_static_obj>)

#------------------------------------------#
# Executable
add_executable(${PROJECT_NAME} "src/lpt_main.F90") 
target_link_libraries(${PROJECT_NAME} lpt_static)

#------------------------------------------#
# Python Interface
if(PYTHON_INTERFACE)
    set(CMAKE_POSITION_INDEPENDENT_CODE ON)
    add_library(lpt_shared_obj OBJECT ${SOURCES})
    add_library(lpt_shared SHARED $<TARGET_OBJECTS:lpt_shared_obj>)

    #find_package(Python3 REQUIRED COMPONENTS Interpreter Development)
    find_package(PythonInterp 3 REQUIRED)
    find_package(PythonLibs 3 REQUIRED)
    find_package(F2PY REQUIRED)

    include_directories(${CMAKE_Fortran_MODULE_DIRECTORY} ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

    set(f2py_module_name "pylpt")
    set(pped_src_filename "${f2py_module_name}.F90")
    set(fortran_src_file "${CMAKE_SOURCE_DIR}/srcpy/lpt_python.F90")
    set(generated_pped_file ${CMAKE_CURRENT_BINARY_DIR}/${pped_src_filename})
    #set(generated_module_file ${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_name}${PYTHON_EXTENSION_MODULE_SUFFIX})
    set(generated_module_file "${f2py_module_name}\*.so")

    ##add_custom_target(${pped_src_filename} ALL
    ##    DEPENDS ${generated_pped_file})
    #add_custom_command(
    #  OUTPUT ${generated_pped_file}
    #  COMMAND "${CMAKE_Fortran_COMPILER}" -I${CMAKE_SOURCE_DIR}/srcpy
    #          -E ${fortran_src_file}
    #          -o ${generated_pped_file}
    #  DEPENDS ${fortran_src_file}
    #  MAIN_DEPENDENCY ${fortran_src_file}
    #  COMMENT "Preprocessing ${fortran_src_file} for use with f2py."
    #  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/srcpy
    #  VERBATIM
    #)
    #add_custom_command(
    #    OUTPUT ${generated_module_file}
    #    COMMAND ${F2PY_EXECUTABLE}
    #    ${F2PY_FLAGS}
    #    -L${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    #    -llpt_shared
    #    -I${CMAKE_Fortran_MODULE_DIRECTORY}
    #    -c
    #    -m ${f2py_module_name}
    #    ${pped_src_file}
    #    DEPENDS ${generated_pped_file}
    #    MAIN_DEPENDENCY ${generated_pped_file}
    #    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    #    VERBATIM
    #    )

    set(pylpt_preproc_FLAGS ${FFLAGS})
    #add_custom_command(
    #  OUTPUT ${pped_src_filename}
    add_custom_target(preprocess_src_file ALL
      COMMAND ${CMAKE_Fortran_COMPILER}
              ${pylpt_preproc_FLAGS}
              -I${CMAKE_SOURCE_DIR}/srcpy
              -E ${fortran_src_file}
              -o ${generated_pped_file}
      DEPENDS ${fortran_src_file}
      COMMENT "Preprocessing ${fortran_src_file} for use with f2py."
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/srcpy
      VERBATIM
    )
    set(F2PY_EXECUTABLE ${F2PY_EXECUTABLE} --f90exec=${CMAKE_Fortran_COMPILER})

    if(BUILD_DEBUG)
        #set(F2PY_FLAGS ${F2PY_FLAGS} --opt="O0")
    else(BUILD_DEBUG)
        #set(F2PY_FLAGS ${F2PY_FLAGS} --opt="O2")
    endif(BUILD_DEBUG)
    #set(F2PY_FLAGS ${F2PY_FLAGS} --f90flags="${FFLAGS}")
    #set(F2PY_FLAGS ${F2PY_FLAGS} --f77flags="${FFLAGS}")
    #set(F2PY_FLAGS ${F2PY_FLAGS} ${FFLAGS})

    add_custom_target(${f2py_module_name} ALL
        COMMAND ${F2PY_EXECUTABLE}
                --verbose ${F2PY_FLAGS}
                -DNPY_NO_DEPRECATED_API=NPY_1_7_API_VERSION
                -L${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
                -llpt_shared
                -I${CMAKE_Fortran_MODULE_DIRECTORY}
                -c
                -m ${f2py_module_name}
                ${generated_pped_file}
        DEPENDS ${generated_pped_file}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        VERBATIM
        )
    add_dependencies(${f2py_module_name} lpt_shared preprocess_src_file)

    #install(FILES ${generated_pped_file} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
    #install(FILES ${generated_module_file} DESTINATION ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
    #add_custom_target(${f2py_module_name} ALL DEPENDS ${generated_module_file})

    set(PROJECT_PYTHON_DIR ${PROJECT_SOURCE_DIR}/pyLPT)
    set(pylptpathfile "${PROJECT_PYTHON_DIR}/pylpt_path.py")
    file(WRITE ${pylptpathfile} "#!/usr/bin/env python3\n")
    string(TIMESTAMP pylpttime)
    file(APPEND ${pylptpathfile} "# Auto-generated by CMake on ${pylpttime}\n")
    file(APPEND ${pylptpathfile} "# This file will automatically be overwritten on every compilation\n")
    file(APPEND ${pylptpathfile} "import os\n")
    file(APPEND ${pylptpathfile} "import sys\n")
    file(APPEND ${pylptpathfile} "sys.path.insert(0,os.path.dirname(os.path.abspath(__file__)))\n")

    add_custom_command(TARGET lpt_shared POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/liblpt_shared.so
                                         ${PROJECT_PYTHON_DIR}
    )
    add_custom_command(TARGET ${f2py_module_name} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy "${f2py_module_name}\*.so"
                                         ${PROJECT_PYTHON_DIR}
    )

endif(PYTHON_INTERFACE)
