.SILENT:
CL = clisp
DAY = day
NUM = $(MAKECMDGOALS)
FILENAME = $(DAY)$(NUM)/$(DAY)$(NUM).lisp

use:
	@echo Usage: make '#day'

$(NUM):	$(FILENAME) 
	$(CL) $(FILENAME) 
