FC = gfortran

LDFLAGS = -L. -L/usr/lib64 -lnetcdff -lnetcdf -lplplotf95cd -lplplotf95d
LIBS = Types_mod.o RHS_mod.o CFL_mod.o IO_mod.o Solver_mod.o
FFLAGS = -I. -I/usr/lib64/gfortran/modules

fd1d_heat_explicit.exe: fd1d_heat_explicit.o $(LIBS)
	$(FC) $^ $(LDFLAGS) -o $@

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

diff: fd1d_heat_explicit.exe
	./fd1d_heat_explicit.exe
	diff h_test01.txt h_test01.txt_bak
	if [ $$? -eq 0 ]; then echo "fine"; else echo "not fine"; fi

.PHONY: clean

clean:
	rm -f *.mod *.o *.png fd1d_heat_explicit.exe
