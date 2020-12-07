# This Makefiles _is_ recursive and must work stand-alone (it is shipped).
#
# Title: Makefile for demonstration GTH api code in Java
# Author: Matthias Lang (matthias@corelatus.com)
# Created: January 2001
#
# Copyright (C) 2001 Corelatus AB Stockholm

classpath=$(CLASSPATH)

javac=javac -classpath $(classpath):./corelatus/gth/

library_classes=$(addprefix corelatus/gth/, Client_conn Resource xml)

archive_filename=java_16.zip

example_classes=$(addprefix corelatus/gth/, \
	install \
	reboot \
	simple_mtp2 \
	timeseries \
	watchdog \
	)

ifndef CLASSPATH
ship:
	@echo "------------------------------------------------------"
	@echo "Warning: skipping java tools build because you have no CLASSPATH"
	@echo "CLASSPATH should include xerces.jar and xalan.jar, "
	@echo "which are needed for XML parsing."
	@echo ""
	@echo "Warning: faking java build."
	touch gth_upgrade.jar $(archive_filename)
else
ship: gth_upgrade.jar $(archive_filename)
endif


$(archive_filename):
	zip $@ Makefile corelatus/gth/*.java README

gth_upgrade.jar: $(addsuffix .java, $(example_classes) $(library_classes))
	javac $^
	zip gth_upgrade.jar corelatus/gth/*.class

install:

clean:
	rm -f corelatus/gth/*.class *.jar *.zip

# eof
