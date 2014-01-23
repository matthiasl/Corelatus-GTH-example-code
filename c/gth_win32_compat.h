// This file collects a few common changes needed to make things compile
// using
//
//    Case 1. gcc native-compiling on a unix-like system
//    Case 2. gcc cross-compiling for win32 (using mingw32msvc-gcc)
//    Case 3. visual studio 2010 native compiling on win32
//
//
// There are three sources of problems
//
//    A. GNU and Microsoft have fixed strcat/strcpy and friends in different
//       ways with different names and different argument orders.
//
//       Case 3 does it one way, cases 1&2 the other.
//
//    B. Microsoft's socket library often uses names for things.
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
//

//----------------------------------------------------------------------
// Case 3

#ifdef _MSC_VER

// Microsoft's equivalents of the GNU strncat and strncpy are called
// strcat_s and strcpy_s, and they have the second and third arguments
// the other way around.
#define strncpy(A,B,C) strcpy_s(A,C,B)
#define strncat(A,B,C) strcat_s(A,C,B)
#define snprintf sprintf_s

#define HANDLE_OR_FILEPTR HANDLE
#define PACK_SUFFIX

//----------------------------------------------------------------------
// Cases 1 & 2
#else

// GCC's pack pragma is a suffix, which is a bit messy.
#define PACK_SUFFIX __attribute__((__packed__))

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
#define HANDLE_OR_FILEPTR FILE*
// Workalike for Microsoft's variant of fopen()
int fopen_s(FILE **file, const char *filename, const char *mode);
#endif
