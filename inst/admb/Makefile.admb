ifdef ComSpec
    RM=del /F /Q
    COPY=copy
else
    RM=rm -rf
    COPY=cp
endif

ADMB ?= admb

all: spm 

spm: spm.tpl 
	@if command -v "$(ADMB)" >/dev/null 2>&1; then \
		"$(ADMB)" -f spm.tpl || echo "ADMB build failed; continuing without spm executable."; \
	else \
		echo "ADMB not found; skipping spm executable build."; \
	fi


clean:
	@$(RM) spm.cpp
	@$(RM) spm.htp
	@$(RM) spm.obj
