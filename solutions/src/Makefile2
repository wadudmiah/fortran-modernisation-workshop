FC = nagfor

NETCDF_INC = /usr/local/netcdf-4.6.1/include
NETCDF_LIB = /usr/local/netcdf-4.6.1/lib

PLPLOT_INC = /usr/local/plplot-5.13.0/lib/fortran/modules/plplot
PLPLOT_LIB = /usr/local/plplot-5.13.0/lib

LDFLAGS = -L. -lfmw -L$(PLPLOT_LIB) -L$(NETCDF_LIB) -lnetcdff -lnetcdf -lplplotfortran -lplplot
FFLAGS = -I. -I$(PLPLOT_INC) -I$(NETCDF_INC)

SOURCES := $(sort $(wildcard *_mod.f90))
OBJECTS := $(SOURCES:.f90=.o)

fd1d_heat_explicit.exe: fd1d_heat_explicit.o libfmw.a
	$(FC) $< $(LDFLAGS) -o $@

libfmw.a: $(OBJECTS)
	ar rcs $@ $^

%.o %.mod: %.f90
	nagfor -c $(FFLAGS) $<

DEPS := $(SOURCES:.f90=.P) fd1d_heat_explicit.P

%.P: %.f90
	nagfor =depend -otype=make $< -o $@.tmp
	grep -vi -E '(netcdf|plplot)' $@.tmp > $@

include Depends

Depends: $(DEPS)
	sort $^ | uniq -u > $@

pfunit: tests.exe testSuites.inc
	./$<

tests.exe: $(PFUNIT)/include/driver.F90 cfl_mod.o testcfl.o 
	$(FC) -I$(PFUNIT)/mod $^ -L$(PFUNIT)/lib -lpfunit -I. -o $@

testcfl.F90: testcfl.pf
	$(PFUNIT)/bin/pFUnitParser.py $< $@ -I.

testcfl.o: testcfl.F90
	$(FC) -I$(PFUNIT)/mod -c $<

diff: fd1d_heat_explicit.exe
	./fd1d_heat_explicit.exe
	diff h_test01.txt h_test01.txt_bak
	if [ $$? -eq 0 ]; then echo "fine"; else echo "not fine"; fi

.PHONY: clean ford ffmpeg man

clean:
	rm -rf *.mod *.o *.png *.a *.x *.exe *.mov *.mp4 test*.F90 *.P *.P.tmp

ffmpeg:
	ffmpeg -f image2 -i fd1d_heat_explicit_%*.png fd1d_heat_explicit.mp4

man:
	man ./fd1d_heat_explicit.1

ford: 
	cd ..; ford fmw.md
