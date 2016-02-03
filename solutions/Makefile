FC = gfortran

fd1d_heat_explicit: fd1d_heat_explicit.o
	$(FC) $< -o $@

fd1d_heat_explicit.o: fd1d_heat_explicit.f90 Types_mod.o
	$(FC) -c -I. $<

Types_mod.o: Types_mod.f90
	$(FC) -c -I. $<

diff: fd1d_heat_explicit
	./fd1d_heat_explicit
	diff h_test01.txt h_test01.txt_bak
	if [ $$? -eq 0 ]; then echo "fine"; else echo "not fine"; fi

.PHONY: clean

clean:
	rm -f *.mod *.o fd1d_heat_explicit
