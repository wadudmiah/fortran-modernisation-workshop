# README #

This repository contains the file for the Fortran Modernisation Workshop

Supplementary material can be downloaded from:

https://www.nag.co.uk/content/fortran-modernization-workshop

### What is this repository for? ###

* Files containing source code for the workshop exercises

### NAG Compiler Update ###

* NAG compiler 6.2 is able to build netcdf and plplot with no problem. 
* NAG compiler version 6.1 and 6.2 produces an internal compiler error when I try to build pfunit. 
* Will try to build netcdf and plplot with 6.1 compiler.
* **NetCDF, PLplot and pFUnit all work with NAG compiler 6.1**
* Have reported the internal compiler error for 6.2.

### How do I get set up? ###

Packages and RPMs (for RedHat based Linux) that are required for this workshop:

* netcdf, netcdf-devel, netcdf-fortran, netcdf-fortran-devel, hdf5, hdf5-devel, nco
* plplot, plplot-devel, plplot-libs, plplot-fortran-devel
* doxygen, doxygen-latex, graphviz
* pFUnit (http://pfunit.sourceforge.net/) - serial only. No OpenMP or MPI configuration required
* gcc-gfortran - GNU Fortran version 4.8.5 or later
* make (should be already installed)
* git
* ffmpeg

For Ubuntu based Linux:

* libnetcdf-dev, libnetcdff-dev, libnetcdff5, netcdf-bin, nco, netcdf-doc, libhdf5-dev
* libplplot-dev, libplplot-fortran11, plplot12-driver-xwin, plplot-doc, plplot12-driver-cairo
* doxygen, doxygen-latex, graphviz
* gfortran - GNU Fortran version 4.8.5 or later
* make
* git
* ffmpeg

### Who do I talk to? ###

* wadud <<dot>> miah <<at>> nag <<dot>> co <<dot>> uk
* fs395 <<at>> cam <<dot>> ac <<dot>> uk
* fatima <<dot>> chami <<at>> durham <<dot>> ac <<dot>> uk