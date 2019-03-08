// This file collects a few common changes needed to make things compile
// using
//
//    Case 1. gcc native-compiling on a unix-like system
//    Case 2. gcc cross-compiling for win32 (using mingw32msvc-gcc)
//    Case 3. Microsoft visual studio compiling Windows
//
//
// Sources of problems
//
//    A. GNU and Microsoft have fixed strcat/strcpy and friends in different
//       ways with different names and different argument orders.
//
//       Case 3 does it one way, cases 1&2 the other.
//
//    B. Microsoft's socket library often uses different names for things.
//
//       Case 1 does it one way, cases 2 & 3 the other.
//
//    C. GNU and Microsoft have different mechanisms for packing structures.
//
//       Microsoft use #pragma pack(N)
//
//       GNU use __attribute__((__packed__)) but also support Microsoft's
//       pragma, but it's unclear whether they always will; GNU don't
//       seem to think the #pragma approach is a good idea.
//
//    D. Linux (and other *nix, not sure) have a 'z' format modifier
//       for printing size_t. Windows doesn't.
//
//    E. C99 and GNU-89 disagree about what 'extern inline' means.
//
//    F. Microsoft don't have ssize_t
//
//
//
#ifndef GTH_WIN32_COMPAT_H
#define GTH_WIN32_COMPAT_H

//----------------------------------------------------------------------
// Case 3

#ifdef _MSC_VER

// Microsoft's strncat_s isn't exactly a drop-in replacement for strncat.
#define strncat(A,B,C) strcat_s(A,C,B)
#define snprintf sprintf_s

// Microsoft VS 2013 does not support C99 'inline'
#define inline
#define POSSIBLY_EXTERN

#define HANDLE_OR_FILEPTR HANDLE
#define PACK_SUFFIX

// Microsoft VS does not support ssize_t; it's POSIX
#include <BaseTsd.h>
typedef SSIZE_T ssize_t;

//----------------------------------------------------------------------
// Cases 1 & 2
#else

// GCC's pack pragma is a suffix, which is a bit messy.
#define PACK_SUFFIX __attribute__((__packed__))

// C99 'extern inline' generates an externally linkable version.
// GNU 89 'extern inline' (GCC 4 and earlier) is illegal.
//
// This macro takes care of the difference. We can replace the macro
// with an 'extern' once GCC 5.x becomes ubiquitous, say 2018.
#if __STDC_VERSION__ >= 199901L
#define POSSIBLY_EXTERN extern
#else
#define POSSIBLY_EXTERN
#endif

#endif

//----------------------------------------------------------------------
// Case 2
#if ( !defined(_MSC_VER) && defined(WIN32))
#define HANDLE_OR_FILEPTR HANDLE
#endif
//----------------------------------------------------------------------
// Case 1
#ifndef WIN32
#define closesocket close
#define HANDLE_OR_FILEPTR  FILE*
// Workalike for Microsoft's variant of fopen()
int fopen_s(FILE **file, const char *filename, const char *mode);

// Workalike for Microsoft's variant of strncpy
int strncpy_s(char *dest,
	      size_t dest_size,
	      const char *src,
	      size_t copy_count);

#define SIZE_T_FORMAT "%zu"
#else
// Cases 2 and 3
#define SIZE_T_FORMAT "%Iu"
#endif


//----------------------------------------------------------------------
// All cases

// Print message and abort
void die(const char *message);

// atoi which calls die() if passed something which isn't a number
int checked_atoi(const char *s);

// Wrapped memory allocation; exits on failure.
void *checked_realloc(void *ptr, size_t size);
void *checked_malloc(size_t size);
#endif
