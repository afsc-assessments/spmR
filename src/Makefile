run   = ./main -nox -nohess >/dev/null
data  =  data/$(stock)_spcat.dat
outdir = $(stock)_out
RM = rm
.PHONY: all run

all: 
ifneq "$(wildcard $(data) )" ""
	cp $(data) spp_catch.dat
	@echo $(outdir)
	ls #$(run)
ifneq "$(wildcard $(outdir) )" ""
else
	mkdir $(outdir)
endif
	mv *.out $(outdir)/
	mv main.rep $(outdir)/report.out
	$(RM) rm eigv.rpt variance admodel.* *.r0? *.p0? fmin.log *.b0? 
else
  # if it doesn't:
	@echo   "Oops...error, file " $(data) " appears to be missing...  "
endif


clean:
	$(RM) $(outdir)/*

	
