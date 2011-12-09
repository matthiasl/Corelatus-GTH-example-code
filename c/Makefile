# This Makefile builds all the example code for both native (e.g. linux)
# and Win32 targets. The win32 executables are called .exe.

target_basenames=\
	connect_timeslots \
	duplex_lapd \
	install_release \
	install_start_script \
	monitor_cas \
	monitor_dtmf \
	playback_file\
	query_set\
	record\
	save_to_pcap

headers=gth_apilib.h gth_client_xml_parse.h

# For windows, we need a .exe suffix on all executables
win_targets=$(addsuffix .exe, $(target_basenames))
common_c_files=gth_apilib.c gth_client_xml_parse.c


CC_WIN32=i686-w64-mingw32-gcc
LIBS_WIN32=-lws2_32

CC=gcc

unix: $(target_basenames)

win32: $(win_targets)

clean:
	rm -f $(win_targets) $(target_basenames) *~ *.zip README.dos

ship:
	make clean
	make
	make win32
	sed -e 's/$$/\r/g' < README.unix > README.txt
	zip gth_c_examples *.c *.h NMakefile Makefile audio/* README.* *.exe

target_basenames win_targets: $(headers) $(common_c_files)

%: %.c $(common_c_files)
	$(CC) -Wall -g -o $@ $^ $(LIBS)

%.exe: %.c $(common_c_files)
	$(CC_WIN32) -Wall -o $@ $^ $(LIBS_WIN32)

