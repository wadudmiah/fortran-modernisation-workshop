#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([fmw], [1.0.0], [wadud.miah@gmail.com])

# Checks for programs.
AC_PROG_MAKE_SET

AC_PROG_FC([gfortran ifort nagfor])
AC_PROG_INSTALL

# Checks for libraries.
# FIXME: Replace `main' with a function in `-lfmw':
AC_CHECK_LIB([fmw], [main])
# FIXME: Replace `main' with a function in `-lnetcdf':
AC_CHECK_LIB([netcdf], [main])
# FIXME: Replace `main' with a function in `-lnetcdff':
AC_CHECK_LIB([netcdff], [main])
# FIXME: Replace `main' with a function in `-lpfunit':
AC_CHECK_LIB([pfunit], [main])
# FIXME: Replace `main' with a function in `-lplplot':
AC_CHECK_LIB([plplot], [main])
# FIXME: Replace `main' with a function in `-lplplotfortran':
AC_CHECK_LIB([plplotfortran], [main])

# Checks for header files.

AC_CHECK_HEADERS([netcdf.mod plplot.mod])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
