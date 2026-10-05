# Thin wrapper around CMake; the build itself is described in CMakeLists.txt.
#
#   make              native build            -> build/unix/
#   make win64        64-bit Windows (MinGW)  -> build/mingw-win64/*.exe
#   make gcc_build    GCC with extra warnings -> build/unix-gcc/
#   make clang_build  Clang with extra warnings -> build/unix-clang/
#   make ship         all of the above checks, then gth_c_examples.zip
#
# Needs cmake 3.21 or later. The Windows targets need the Debian
# packages gcc-mingw-w64-i686 / gcc-mingw-w64-x86-64.

BUILD = cmake --preset $(1) && cmake --build --preset $(1) --parallel

.PHONY: unix win64 gcc_build clang_build clean ship

unix:
	$(call BUILD,unix-gcc)

win64:
	$(call BUILD,mingw-win64)

gcc_build:
	$(call BUILD,unix-gcc)

clang_build:
	$(call BUILD,unix-clang)

clean:
	rm -rf build *~ *.zip version.c

ship:
	$(MAKE) clean
	$(MAKE) gcc_build
	$(MAKE) clang_build
	$(MAKE) win64
	zip gth_c_examples *.c *.h CMakeLists.txt CMakePresets.json cmake/*.cmake Makefile audio/* README.md build/mingw-win64/*.exe
