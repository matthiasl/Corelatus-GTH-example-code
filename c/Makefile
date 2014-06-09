# This Makefile builds all the example code for both native (e.g. linux)
# and Win32 targets (by cross-compiling with the mingw tools).
#
# The win32 executables get a .exe suffix

target_basenames=\
	connect_timeslots \
	duplex_lapd \
	disable \
	enable \
	install_release \
	install_start_script \
	map \
	monitor_cas \
	monitor_dtmf \
	playback_file\
	query_set\
	record\
	save_to_pcap\
	unmap\
	wide_recorder

headers=gth_apilib.h gth_client_xml_parse.h

# For windows, we need a .exe suffix on all executables
win_targets=$(addsuffix .exe, $(target_basenames))
common_c_files=gth_apilib.c gth_client_xml_parse.c
example_c_files=$(addsuffix .c, $(target_basenames))

# present on newer debian systems (2012)
CC_WIN32_64bit=i686-w64-mingw32-gcc
# present on older debian systems
CC_WIN32_32bit=i586-mingw32msvc-gcc

CC_WIN32:=$(shell if test -x `which $(CC_WIN32_64bit)` ; then echo $(CC_WIN32_64bit); else echo $(CC_WIN32_32bit); fi)
LIBS_WIN32=-lws2_32
LIBS=-lrt

CC=gcc

repo_head=$(shell git describe --abbrev=8 --dirty --always)

unix: $(target_basenames)

win32: $(win_targets)

clean:
	rm -f $(win_targets) $(target_basenames) *~ *.zip README.dos version.c

ship: version.c
	make clean
	make
	make win32
	sed -e 's/$$/\r/g' < README.unix > README.txt
	zip gth_c_examples *.c *.h NMakefile Makefile audio/* README.* *.exe

version.c:
	@echo "#include \"gth_apilib.h\"" > $@
	@echo "const char git_head[] = \""$(repo_head)"\";" >> $@
	@echo "const char build_hostname[] = \""$(shell hostname)"\";" >> $@

%: %.c $(headers)

%: %.c $(common_c_files) version.c
	$(CC) -Wall -g -o $@ $^ $(LIBS)

%.exe: %.c $(headers)

%.exe: %.c $(common_c_files) version.c
	$(CC_WIN32) -Wall -o $@ $^ $(LIBS_WIN32)

