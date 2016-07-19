
fmw_test.exe: fmw_test.o
	gfortran $< -lnetcdff -lnetcdf -lplplotf95cd -lplplotf95d -o $@

fmw_test.o: fmw_test.f90
	gfortran -I. -I/usr/lib64/gfortran/modules -I/usr/lib/fortran/modules/plplot -c $< -o $@

.PHONY: clean execute

clean:
	rm -rf *.o *.mod *.exe *.nc *.png

execute:
	./fmw_test.exe

ncdump:
	ncdump fmw_test.nc
