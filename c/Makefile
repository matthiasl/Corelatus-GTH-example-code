# This Makefile builds all the example code for both native (e.g. linux)
# and Win32 targets. The win32 executables are called .exe.

target_basenames=\
	playback_file\
	record\
	install_release\
	install_start_script\
	save_to_pcap\
	monitor_cas

# For windows, we need a .exe suffix on all executables
targets=$(addsuffix .exe, $(target_basenames)) $(target_basenames)

CC_WIN32=i586-mingw32msvc-gcc
LIBS_WIN32=-lws2_32

CC=gcc

all: $(targets)

clean:
	rm -f $(targets) *~ *.zip

ship:
	make clean
	make
	zip gth_c_examples *.c *.h Makefile audio/* README *.exe

%: %.c gth_apilib.c gth_client_xml_parse.c gth_apilib.h gth_client_xml_parse.h
	$(CC) -Wall -o $@ $^ $(LIBS)

%.exe: %.c gth_apilib.c gth_client_xml_parse.c gth_apilib.h gth_client_xml_parse.h
	$(CC_WIN32) -Wall -o $@ $^ $(LIBS_WIN32)

