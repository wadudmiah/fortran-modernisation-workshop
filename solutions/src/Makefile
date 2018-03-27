FC = gfortran

LDFLAGS = -L. -lfmw -L/usr/lib64 -L/lib64 -L/home/wadudm/netcdf-4.6.0/lib -lnetcdff -lnetcdf -lplplotf95 -lplplot
LIBS = Types_mod.o RHS_mod.o CFL_mod.o IO_mod.o Solver_mod.o
FFLAGS = -I. -I/usr/include -I/usr/lib64/gfortran/modules -I/usr/lib/fortran/modules/plplot -I/home/wadudm/netcdf-4.6.0/include

fd1d_heat_explicit.exe: fd1d_heat_explicit.o libfmw.a
	$(FC) $< $(LDFLAGS) -o $@

libfmw.a: $(LIBS)
	ar rcs $@ $^

fd1d_heat_explicit.o: fd1d_heat_explicit.f90 $(LIBS)
	$(FC) -c $(FFLAGS) $<

Types_mod.o: Types_mod.f90
	$(FC) -c $(FFLAGS) $<

RHS_mod.o: RHS_mod.f90 Types_mod.o
	$(FC) -c $(FFLAGS) $<

CFL_mod.o: CFL_mod.f90 Types_mod.o
	$(FC) -c $(FFLAGS) $<

IO_mod.o: IO_mod.f90 Types_mod.o
	$(FC) -c $(FFLAGS) $<

Solver_mod.o: Solver_mod.f90 Types_mod.o
	$(FC) -c $(FFLAGS) $<

pfunit: tests.exe testSuites.inc
	./$<

tests.exe: $(PFUNIT)/include/driver.F90 CFL_mod.o testCFL.o 
	$(FC) -I$(PFUNIT)/mod $^ -L$(PFUNIT)/lib -lpfunit -I. -o $@

testCFL.F90: testCFL.pf
	$(PFUNIT)/bin/pFUnitParser.py $< $@ -I.

testCFL.o: testCFL.F90
	$(FC) -I$(PFUNIT)/mod -c $<

diff: fd1d_heat_explicit.exe
	./fd1d_heat_explicit.exe
	diff h_test01.txt h_test01.txt_bak
	if [ $$? -eq 0 ]; then echo "fine"; else echo "not fine"; fi

.PHONY: clean ford ffmpeg man

clean:
	rm -rf *.mod *.o *.png *.a *.x *.exe *.mov *.mp4 test*.F90 

ffmpeg:
	ffmpeg -f image2 -i fd1d_heat_explicit_%*.png fd1d_heat_explicit.mp4

man:
	man ./fd1d_heat_explicit.1
