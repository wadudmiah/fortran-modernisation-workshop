cmake_minimum_required(VERSION 3.0.0)

project(fortran_workshop VERSION 1.0.0)
enable_language(Fortran)
set( CMAKE_fortran_COMPILER gfortran )
set( SOURCES types_mod.f90
  rhs_mod.f90
  cfl_mod.f90
  solver_mod.f90
  io_mod.f90 )

find_library( NETCDF_LIB netcdf ENV LD_LIBRARY_PATH )
if ( NOT NETCDF_LIB )
  message( FATAL_ERROR "NetCDF library not found" )
else()
  message( STATUS "Found NetCDF library" )
endif()

find_library( NETCDFF_LIB netcdff ENV LD_LIBRARY_PATH )
if ( NOT NETCDFF_LIB )
  message( FATAL_ERROR "NetCDF Fortran library not found" )
else()
  message( STATUS "Found NetCDF Fortran library" )
endif()

find_library( PLPLOT_LIB plplot ENV LD_LIBRARY_PATH )
if ( NOT PLPLOT_LIB )
  message( FATAL_ERROR "PLPlot library not found" )
else()
  message( STATUS "Found PLplot library" )
endif()

find_library( PLPLOTF_LIB plplotfortran ENV LD_LIBRARY_PATH )
if ( NOT PLPLOTF_LIB )
  message( FATAL_ERROR "PLPlot Fortran library not found" )
else()
  message( STATUS "Found PLPlot Fortran library" )
endif()

add_library( fmw SHARED ${SOURCES} )

add_executable( fd1d_heat_explicit.exe fd1d_heat_explicit.f90 )

target_include_directories( fmw PUBLIC /local/software/plplot/5.15.0/lib/fortran/modules/plplot )
target_include_directories( fmw PUBLIC /local/software/netcdf/fmw/4.6.1/include )
target_include_directories( fmw PUBLIC /usr/lib64/gfortran/modules )
target_include_directories( fd1d_heat_explicit.exe PUBLIC /usr/lib64/gfortran/modules )
target_link_libraries( fd1d_heat_explicit.exe fmw )

target_link_libraries( fd1d_heat_explicit.exe plplot -L/local/software/plplot/5.15.0/lib )
target_link_libraries( fd1d_heat_explicit.exe plplotfortran -L/local/software/plplot/5.15.0/lib )
target_link_libraries( fd1d_heat_explicit.exe netcdf -L/local/software/netcdf/fmw/4.6.1/lib )
target_link_libraries( fd1d_heat_explicit.exe netcdff -L/local/software/netcdf/fmw/4.6.1/lib )
