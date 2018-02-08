/*
Binderoo
Copyright (c) 2016, Remedy Entertainment
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the copyright holder (Remedy Entertainment) nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL REMEDY ENTERTAINMENT BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
//----------------------------------------------------------------------------

#if defined( __cplusplus )
	#pragma once
#endif

#if !defined( _BINDEROO_DEFS_H_ )
#define _BINDEROO_DEFS_H_
//----------------------------------------------------------------------------

#include <stdint.h>
#include <stddef.h>

// The macro BIND_COMPILER will evaluate to one of these
#define BIND_COMPILER_MSVC				0
#define BIND_COMPILER_CLANG				1
#define BIND_COMPILER_GCC				2

// The macro BIND_COMPILERVER will evaluate to one of these
#define BIND_COMPILERVER_MSVC2012		0
#define BIND_COMPILERVER_MSVC2015		1
#define BIND_COMPILERVER_MSVC2017		2

#define BIND_COMPILERVER_CLANG380		3

#define BIND_COMPILERVER_GCC540			4

// The macro BIND_STANDARD will evaluate to one of these
#define BIND_STANDARD_MSVC2012			0
#define BIND_STANDARD_CPP11				1
#define BIND_STANDARD_CPP14				2

// The macro BIND_OS will evaluate to one of these
#define BIND_OS_WINDOWS					0
#define BIND_OS_LINUX					1
#define BIND_OS_ANDROID					2
#define BIND_OS_OSX						3
#define BIND_OS_IOS						4

// The macro BIND_OSDETAIL will evaluate to one of these
#define BIND_OSDETAIL_GENERIC			0
#define BIND_OSDETAIL_WIN7				1
#define BIND_OSDETAIL_WIN10				2

// The macro BIND_SYSAPI will evaluate to one of these
#define BIND_SYSAPI_WINAPI				0
#define BIND_SYSAPI_UWP					1
#define BIND_SYSAPI_POSIX				2

#if defined( _MSC_VER ) // Microsoft compiler
	#define BIND_COMPILER 				BIND_COMPILER_MSVC
	#if _MSC_VER >= 1700 && _MSC_VER < 1900
		#define BIND_COMPILERVER 		BIND_COMPILERVER_MSVC2012
	#elif _MSC_VER < 1910
		#define BIND_COMPILERVER 		BIND_COMPILERVER_MSVC2015
	#elif _MSC_VER >= 1901
		#define BIND_COMPILERVER 		BIND_COMPILERVER_MSVC2017
	#else
		static_assert( false, "Unsupported MSVC detected! Versions older than VS2012 are not supported, while versions greater than VS2017 rely on VS2017 support working." );
	#endif // _MSC_VER checks
#elif defined( __clang__ ) // Clang
	#define BIND_COMPILER 				BIND_COMPILER_CLANG
	#if __clang_major__ == 3 && __clang_minor__ >= 8
		#define BIND_COMPILERVER 		BIND_COMPILERVER_CLANG380
	#elif __clang_major__ > 3 // Falling back to clang 3.8 minimum support
		#define BIND_COMPILERVER 		BIND_COMPILERVER_CLANG380
	#else
		static_assert( false, "Unsupported clang detected! 3.8 is the minimum required version." );
	#endif // __clang checks
#elif defined( __GNUC__ )
	#define BIND_COMPILER 				BIND_COMPILER_MSVC
	#if __GNUC__ == 5 && __GNUC_MINOR__ >= 4
		#define BIND_COMPILERVER		BIND_COMPILERVER_GCC_540
	#elif __GNUC__ > 5
		#define BIND_COMPILERVER		BIND_COMPILERVER_GCC_540
	#else
		static_assert( false, "Unsupported GCC detected! 5.4.0 is the minimum required version." );
	#endif
#else // Unsupported
	static_assert( false, "Unsupported compiler detected! Binderoo needs to know your compiler in order to compile correctly. Clang and MSVC are the currently supported C++ compilers." );
#endif // compiler checks


#if BIND_COMPILER == BIND_COMPILER_MSVC && BIND_COMPILERVER == BIND_COMPILERVER_MSVC2012
	#define BIND_STANDARD				BIND_STANDARD_MSVC2012
#else
	#define BIND_STANDARD				BIND_STANDARD_CPP14
#endif // C++ version checks

#if defined( _WIN32 ) && _WIN32 == 1
	// Compiling for Windows
	#define BIND_OS 					BIND_OS_WINDOWS
	#if !defined( BINDEROO_FORCE_POSIX )
		#include <Windows.h>
		#if WINVER >= 0x0601 && WINVER <= 0x0603
			#define BIND_OSDETAIL 		BIND_OSDETAIL_WIN7
			#define BIND_SYSAPI			BIND_SYSAPI_WINAPI
		#elif WINVER >= 0x0A00
			#define BIND_OSDETAIL 		BIND_OSDETAIL_WIN10
			#if WINAPI_FAMILY == WINAPI_FAMILY_DESKTOP_APP
				#define BIND_SYSAPI		BIND_SYSAPI_WINAPI
			#else
				#define BIND_SYSAPI		BIND_SYSAPI_UWP
			#endif // Family check
		#else
			static_assert( false, "Invalid Windows API version detected! You must use the Windows 7 WinAPI or greater." );
		#endif // Windows version checks
	#else // BINDEROO_FORCE_POSIX
		#define BIND_OSDETAIL			BIND_OSDETAIL_GENERIC
		#define BIND_SYSAPI				BIND_SYSAPI_POSIX
	#endif // !defined( BINDEROO_FORCE_POSIX )
#elif defined( __gnu_linux__ ) && __gnu_linux__ == 1
	// Compiling for Linux
	#define BIND_OS 					BIND_OS_LINUX
	#define BIND_SYSAPI					BIND_SYSAPI_POSIX
	#define BIND_OSDETAIL				BIND_OSDETAIL_JUSTLINUX
#else
	static_assert( false, "Invalid OS detected! Binderoo only supports Windows and Linux currently." );
#endif // OS checks

#if BIND_COMPILER == BIND_COMPILER_MSVC
	#if defined( BINDEROO_EXPORT )
		#define BIND_DLL				__declspec( dllexport )
	#else
		#define BIND_DLL				__declspec( dllimport )
	#endif // Host mode check
#else
	#define BIND_DLL
#endif

#if BIND_COMPILER == BIND_COMPILER_MSVC
#pragma warning( disable: 4251 )

// Misfires in VC2015.
#pragma warning( disable: 4814 ) // in C++14 'constexpr' will not imply 'const'; consider explicitly specifying 'const'

#endif

#if defined( __cplusplus )
	#if BIND_COMPILER == BIND_COMPILER_MSVC
		#define BIND_C_CALL					__cdecl
	#elif BIND_COMPILER == BIND_COMPILER_GCC
		#define BIND_C_CALL __attribute__ ( ( __cdecl__ ) )
	#elif BIND_COMPILER == BIND_COMPILER_CLANG
		#define BIND_C_CALL __attribute__( ( cdecl ) )
	#endif
	#define BIND_C_API_BEGIN			extern "C" {
	#define BIND_C_API_END				}
#else
	#define BIND_C_CALL
	#define BIND_C_API_BEGIN
	#define BIND_C_API_END
#endif // C++ check

#if BIND_COMPILER == BIND_COMPILER_MSVC
	#define BIND_INLINE					__forceinline
	#define BIND_NOINLINE				__declspec( noinline )
#elif BIND_COMPILER == BIND_COMPILER_CLANG
	#define BIND_INLINE					inline __attribute__((always_inline))
	#define BIND_NOINLINE				__attribute__((noinline))
#endif

#define BIND_ABSTRACT					abstract
#define BIND_OVERRIDE					override

#if BIND_STANDARD == BIND_STANDARD_MSVC2012
	#define BIND_ALIGN( x )				__declspec( align( x ) )
	#define BIND_ALIGNOF( x )			__alignof( x )
#else // anything but VC2012
	#define BIND_ALIGN( x )				alignas( x )
	#define BIND_ALIGNOF( x )			alignof( x )
#endif // BIND_CPPVERSION check
//----------------------------------------------------------------------------

#define BIND_TOSTRING_IMPL( x ) #x
#define BIND_TOSTRING( x ) BIND_TOSTRING_IMPL( ( x ) )

#define BIND_CONCAT_IMPL( a, b ) a ## b
#define BIND_CONCAT( a, b ) BIND_CONCAT_IMPL( a, b )
//----------------------------------------------------------------------------

namespace binderoo
{
	template< typename _ty >
	struct TypeNames
	{
		constexpr const char* getCName() { static_assert( sizeof( _ty ) == 0, "Undefined type! You must declare your type with BIND_TYPE_NAME!" ); return "<INVALID>"; }
		constexpr const char* getDName() { static_assert( sizeof( _ty ) == 0, "Undefined type! You must declare your type with BIND_TYPE_NAME!" ); return "<INVALID>"; }
	};
}

#define BIND_TYPE_NAME( FullCType, FullDName ) \
namespace binderoo { \
template<> struct TypeNames< FullCType > \
{ \
	constexpr const char* getCName() { return #FullCType; } \
	constexpr const char* getDName() { return #FullDName; } \
};\
template<> struct TypeNames< FullCType* > \
{ \
	constexpr const char* getCName() { return #FullCType "*"; } \
	constexpr const char* getDName() { return #FullDName "*"; } \
};\
template<> struct TypeNames< FullCType& > \
{ \
	constexpr const char* getCName() { return #FullCType "&"; } \
	constexpr const char* getDName() { return "ref " #FullDName; } \
};\
}\

BIND_TYPE_NAME( char, char )
BIND_TYPE_NAME( uint8_t, ubyte )
BIND_TYPE_NAME( int16_t, short )
BIND_TYPE_NAME( uint16_t, ushort )
BIND_TYPE_NAME( int32_t, int )
BIND_TYPE_NAME( uint32_t, uint )
BIND_TYPE_NAME( int64_t, long )
BIND_TYPE_NAME( uint64_t, ulong )
BIND_TYPE_NAME( wchar_t, wchar )

#endif // !defined( _BINDEROO_DEFS_H_ )

//============================================================================
