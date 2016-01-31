FC = gfortran

fd1d_heat_explicit_prb: fd1d_heat_explicit_prb.o fd1d_heat_explicit.o
	$(FC) $^ -o $@

fd1d_heat_explicit_prb.o: fd1d_heat_explicit_prb.f
	$(FC) -c $<

fd1d_heat_explicit.o: fd1d_heat_explicit.f
	$(FC) -c $<

.PHONY: clean diff

diff:
	diff h_test01.txt h_test01.txt_bak

clean:
	rm -f *.o fd1d_heat_explicit_prb
