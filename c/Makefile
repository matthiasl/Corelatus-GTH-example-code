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
	job_query \
	map \
	monitor_dtmf \
	playback_file\
	query_set\
	raw_xml\
	record\
	save_to_pcap\
	unmap

headers=gth_apilib.h gth_client_xml_parse.h

# For windows, we need a .exe suffix on all executables
win_targets=$(addsuffix .exe, $(target_basenames))
common_c_files=gth_apilib.c gth_client_xml_parse.c
example_c_files=$(addsuffix .c, $(target_basenames))

# We only support cross-compiling to Windows on 64-bit debian systems,
# which is pretty much every debian box since about 2012.
CC_WIN32=i686-w64-mingw32-gcc
LIBS_WIN32=-lws2_32
LIBS=-lrt

repo_head=$(shell git describe --abbrev=8 --dirty --always)

unix: $(target_basenames)

win32: $(win_targets)

clang_build:
	CC=clang make

gcc_build:
	CC=gcc make

clean:
	rm -f $(win_targets) $(target_basenames) *~ *.zip README.dos version.c

ship: version.c
	make clean
	make gcc_build
	make clean
	make clang_build
	make win32
	sed -e 's/$$/\r/g' < README.unix > README.txt
	zip gth_c_examples *.c *.h Corelatus-GTH-example-code.sln Build_all.vcxproj NMakefile Makefile audio/* README.* *.exe

version.c:
	@echo "#include \"gth_apilib.h\"" > $@
	@echo "const char git_head[] = \""$(repo_head)"\";" >> $@
	@echo "const char build_hostname[] = \""$(shell hostname)"\";" >> $@

%: %.c $(headers)

%: %.c $(common_c_files) version.c
	$(CC) -Wall -Wextra -g -o $@ $^ $(LIBS)

%.exe: %.c $(headers)

%.exe: %.c $(common_c_files) version.c
	$(CC_WIN32) -Wall -Wextra -o $@ $^ $(LIBS_WIN32)
