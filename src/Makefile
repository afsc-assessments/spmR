ifdef ComSpec
    RM=del /F /Q
    COPY=copy
else
    RM=rm -rf
    COPY=cp
endif

all: spm 

spm: spm.tpl 
	@admb -f spm.tpl 


clean:
	@$(RM) spm.cpp
	@$(RM) spm.htp
	@$(RM) spm.obj

