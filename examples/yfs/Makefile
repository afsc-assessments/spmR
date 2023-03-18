run   = ../../src/spm -nox -nohess >/dev/null
data  =  data/$(stock)_spcat.dat
outdir = $(stock)_out
RM = rm
.PHONY: all run

all: 
ifneq "$(wildcard $(data) )" ""
	cp $(data) spp_catch.dat
	@echo $(outdir)
	$(run)
ifneq "$(wildcard $(outdir) )" ""
else
	mkdir $(outdir)
endif
	mv *.out $(outdir)/
	mv spm.rep $(outdir)/report.out
	$(RM) rm eigv.rpt variance admodel.* *.r0? *.p0? fmin.log *.b0? 
else
  # if it doesn't:
	@echo   "Oops...error, file " $(data) " appears to be missing...  "
endif


clean:
	$(RM) $(outdir)/*

	
EXEC = spm
DIST = ../../src/
ARGS = -nox -iprint 150 

ifdef ComSpec
    RM=del /F /Q
else
    RM=rm -rf
endif

all: mpd $(DIST)$(EXEC).tpl

$(EXEC): $(DIST)$(EXEC).tpl
	ln -sf $(DIST)$(EXEC) $@
	ln -sf $(DIST)$(EXEC).tpl $@.tpl
	$(MAKE) --directory=../../src

$(DIST)$(EXEC).tpl:
	$(MAKE) --directory=../../src


mpd: $(EXEC)
	./$(EXEC) $(ARGS)
	@$(RM) $(EXEC).*[0123456789] *.rpt *.log variance gradient.* *tmp admodel.* *.eva

mcmc: $(EXEC)
	./$(EXEC) $(ARGS) -mcmc 3000000 -mcsave 600
	./$(EXEC) -mceval 
	
proj: $(PROJ)
	./$(PROJ) 

debug: $(EXEC)
	./$(EXEC) $(ARGS)

plot:
	R CMD BATCH plot.R

clean:
	@$(RM) $(EXEC)
	@$(RM) $(EXEC)  $(EXEC).[brces]* $(EXEC).*[0123456789] *.rpt *.log variance gradient.* *tmp
	@$(RM) admodel.*
	@$(RM) checkfile.rep
	@$(RM) mcout.rep
	@$(RM) plot.Rout
	@$(RM) Rplots.pdf
	@$(RM) *.rep
	@$(RM) Fprof.yld
	@$(RM) *.prj
	@$(RM) pm.par
	@$(RM) SIS_out.rep
	@$(RM) mceval.dat
