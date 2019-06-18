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

#include "binderoo/defs.h"
#include "binderoo/hash.h"
#include "binderoo/host.h"

#include "binderoo/imports.h"
#include "binderoo/exports.h"

#include "paramhandler.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
//----------------------------------------------------------------------------

static void* BIND_C_CALL test_unaligned_malloc( size_t objSize )
{
	return malloc( objSize );
}
//----------------------------------------------------------------------------

static void BIND_C_CALL test_unaligned_free( void* pObj )
{
	free( pObj );
}
//----------------------------------------------------------------------------


static void* BIND_C_CALL test_malloc( size_t objSize, size_t alignment )
{
#if BIND_SYSAPI == BIND_SYSAPI_WINDOWS
	return _aligned_malloc( objSize, alignment );
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
	return aligned_alloc( alignment, objSize );
#endif
}
//----------------------------------------------------------------------------

static void BIND_C_CALL test_free( void* pObj )
{
#if BIND_SYSAPI == BIND_SYSAPI_WINDOWS
	_aligned_free( pObj );
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
	free( pObj );
#endif
}
//----------------------------------------------------------------------------

static void* BIND_C_CALL test_calloc( size_t objCount, size_t objSize, size_t alignment )
{
#if BIND_SYSAPI == BIND_SYSAPI_WINDOWS
	return _aligned_malloc( objCount * objSize, alignment );
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
	return aligned_alloc( alignment, objCount * objSize );
#endif
}
//----------------------------------------------------------------------------

static void* BIND_C_CALL test_realloc( void* pObj, size_t newObjSize, size_t alignment )
{
#if BIND_SYSAPI == BIND_SYSAPI_WINDOWS
	return _aligned_realloc( pObj, newObjSize, alignment );
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
	// HOLY HELL THIS IS ENTIRELY UNSECURE GET A CORRECT SOLUTION ASAP
	void* pNewObj = aligned_alloc( alignment, newObjSize );
	memcpy( pNewObj, pObj, newObjSize );
	free( pObj );
	return pNewObj;
#endif
}
//----------------------------------------------------------------------------

#define STR_IMPL( x ) #x
#define STR( x ) STR_IMPL( x )

#define TERMINAL_RESET "\x1b[0m"
#define TERMINAL_RGB( r, g, b ) STR( r )";" STR( g ) ";" STR( B )
#define TERMINAL_FOREGROUNDRGB( r, g, b ) "\x1b[38;2;" TERMINAL_RGB( r, g, b ) "m"
#define TERMINAL_BACKGROUNDRGB( r, g, b ) "\x1b[48;2;" TERMINAL_RGB( r, g, b ) "m"
#define TERMINAL_FOREGROUND256( col ) "\x1b[38;5;" STR( col ) "m"
#define TERMINAL_BACKGROUND256( col ) "\x1b[48;5;" STR( col ) "m"
#define TERMINAL_BOLDON "\x1b[1m"

static void BIND_C_CALL log_info( const char* pMessage )
{
	fprintf( stdout, "%s\n", pMessage );
}
//----------------------------------------------------------------------------

static void BIND_C_CALL log_warning( const char* pMessage )
{
//	fprintf( stdout, TERMINAL_BOLDON TERMINAL_FOREGROUNDRGB( 255, 211, 0 ) "WARNING:" TERMINAL_RESET " %s\n", pMessage );
	fprintf( stdout, TERMINAL_BOLDON TERMINAL_FOREGROUND256( 220 ) "WARNING:" TERMINAL_RESET " %s\n", pMessage );
}
//----------------------------------------------------------------------------

static void BIND_C_CALL log_error( const char* pMessage )
{
//	fprintf( stderr, TERMINAL_BOLDON TERMINAL_FOREGROUNDRGB( 255, 255, 255 ) TERMINAL_BACKGROUNDRGB( 227, 66, 52 ) "ERROR:" TERMINAL_RESET " %s\n", pMessage );
	fprintf( stderr, TERMINAL_BOLDON TERMINAL_FOREGROUND256( 15 ) TERMINAL_BACKGROUND256( 160 ) "ERROR:" TERMINAL_RESET " %s\n", pMessage );
}
//----------------------------------------------------------------------------

enum Options : int
{
	None,
	ExportBindingsForAll				= 0x001,
	ExportBindingsForSpecific			= 0x002,
	ImportBindingsForAll				= 0x004,
	ImportBindingsForSpecific			= 0x008,
	ListCPPStyle						= 0x018,
	ListCSharpStyle						= 0x020,
	SearchFolder						= 0x040,
	FunctionCall						= 0x080,
	FunctionCallParameter				= 0x100,
	DInterfaceFiles						= 0x200,
	Verbose								= 0x400,
	Quiet								= 0x800,
};
//----------------------------------------------------------------------------

void printUsageScreen()
{
	printf( "Binderoo Util (C) 2016-2018 Remedy Entertainment Ltd.\n" );
	printf( "\n" );
	printf( "Usage: binderoo_util [option] [...]\n" );
	printf( "\n" );
	printf( "Valid options:\n" );
	printf( "  -f <folder>         Add folders to search for DLL (use multiple -f definitions)\n" );
	printf( "  -ec++ <class>       Generate C++ style export bindings for provided class\n" );
	printf( "  -ec++A              Generate C++ style export bindings for all classes in all DLLs\n" );
	printf( "  -ec++AV <versions>  As -ec++A, except with specified versions\n" );
	printf( "  -ic#A               Generate C# style import bindings for all objects in all DLLs\n" );
	printf( "  -di <folder>        Generate .di files for all objects in all DLLs, output to folder\n" );
	printf( "  -c                  Call a function\n" );
	printf( "  -p                  Parameter for function call (use multiple -p definitions)\n" );
	printf( "  -v                  Verbose mode, prints all messages generated by Binderoo\n" );
	printf( "  -q                  Quiet mode, supresses all messages generated by Binderoo\n" );
}
//----------------------------------------------------------------------------

int main( int argc, char** argv )
{
	if( argc < 2 )
	{
		printUsageScreen();
	}
	else
	{
		const int			MaxArgCount = 256;
		binderoo::DString	searchFolders[ MaxArgCount ];
		binderoo::DString	listClasses[ MaxArgCount ];
		binderoo::DString	outputFolderDInterface;
		binderoo::DString	functionCall;
		binderoo::DString	functionCallParameters[ MaxArgCount ];

		int					iErrorParameters[ MaxArgCount ];
		int					iErrorCount = 0;

		int					iSearchFolderCount = 0;
		int					iListClassCount = 0;
		int					iFunctionCallParameterCount = 0;
		
		int					eCurrParamMode = Options::None;
		int					eFoundParams = Options::None;

		const char*			pCBindingsForAllVersions = nullptr;

		for( int iCurrArg = 1; iCurrArg < argc && iSearchFolderCount < MaxArgCount; ++iCurrArg )
		{
			switch( eCurrParamMode )
			{
			case Options::None:
				{
					// TODO: Make this less rubbish and less prone to error
					if( argv[ iCurrArg ][ 0 ] == '-' )
					{
						switch( argv[ iCurrArg ][ 1 ] )
						{
						case 'e':
							{
								int iNextCharPosition = 2;
								if( argv[ iCurrArg ][ 2 ] == 'c' && argv[ iCurrArg][ 3 ] == '+' && argv[ iCurrArg][ 4 ] == '+' )
								{
									eFoundParams |= Options::ListCPPStyle;
									iNextCharPosition = 5;
								}
/*								else if( argv[ iCurrArg][ 2 ] == 'c' && argv[ iCurrArg][ 3 ] == '#' )
								{
									eFoundParams |= Options::ListCSharpStyle;
									iNextCharPosition = 4;
								}*/
								else
								{
									iErrorParameters[ iErrorCount++ ] = iCurrArg;
									break;
								}

								if( argv[ iCurrArg ][ iNextCharPosition ] == 'A' )
								{
									eFoundParams |= Options::ExportBindingsForAll;
									if( argv[ iCurrArg ][ iNextCharPosition + 1 ] == 'V' )
									{
										eCurrParamMode = Options::ExportBindingsForAll;
									}
								}
								else
								{
									eFoundParams |= Options::ExportBindingsForSpecific;
									eCurrParamMode = Options::ExportBindingsForSpecific;
								}
							}
							break;

						case 'i':
							{
								int iNextCharPosition = 2;
								/*if( argv[ iCurrArg][ 2 ] == 'c' && argv[ iCurrArg][ 3 ] == '+' && argv[ iCurrArg][ 4 ] == '+' )
								{
									eFoundParams |= Options::ListCPPStyle;
									iNextCharPosition = 5;
								}
								else */ if( argv[ iCurrArg][ 2 ] == 'c' && argv[ iCurrArg][ 3 ] == '#' )
								{
									eFoundParams |= Options::ListCSharpStyle;
									iNextCharPosition = 4;
								}
								else
								{
									iErrorParameters[ iErrorCount++ ] = iCurrArg;
									break;
								}

								if( argv[ iCurrArg ][ iNextCharPosition ] == 'A' )
								{
									eFoundParams |= Options::ImportBindingsForAll;
									if( argv[ iCurrArg ][ iNextCharPosition + 1 ] == 'V' )
									{
//										eCurrParamMode = Options::ImportBindingsForAll;
										iErrorParameters[ iErrorCount++ ] = iCurrArg;
										break;
									}
								}
								else
								{
//									eFoundParams |= Options::ImportBindingsForSpecific;
//									eCurrParamMode = Options::ImportBindingsForSpecific;
									iErrorParameters[ iErrorCount++ ] = iCurrArg;
									break;
								}
							}
							break;

						case 'f':
							eFoundParams |= Options::SearchFolder;
							eCurrParamMode = Options::SearchFolder;
							break;

						case 'd':
							if( argv[ iCurrArg ][ 2 ] == 'i' )
							{
								eFoundParams |= Options::DInterfaceFiles;
								eCurrParamMode = Options::DInterfaceFiles;
							}
							else
							{
								iErrorParameters[ iErrorCount++ ] = iCurrArg;
							}
							break;

						case 'c':
							eFoundParams |= Options::FunctionCall;
							eCurrParamMode = Options::FunctionCall;
							break;

						case 'p':
							eFoundParams |= Options::FunctionCallParameter;
							eCurrParamMode = Options::FunctionCallParameter;
							break;

						case 'v':
							eFoundParams |= Options::Verbose;
							break;

						case 'q':
							eFoundParams |= Options::Quiet;
							break;

						default:
							iErrorParameters[ iErrorCount++ ] = iCurrArg;
							break;
						}
					}
					else
					{
						iErrorParameters[ iErrorCount++ ] = iCurrArg;
					}
				}
				break;

			case Options::SearchFolder:
				searchFolders[ iSearchFolderCount++ ] = binderoo::DString( argv[ iCurrArg ], strlen( argv[ iCurrArg ] ) );
				eCurrParamMode = Options::None;
				break;

			case Options::ExportBindingsForAll:
				pCBindingsForAllVersions = argv[ iCurrArg ];
				eCurrParamMode = Options::None;
				break;

			case Options::ExportBindingsForSpecific:
				listClasses[ iListClassCount++ ] = binderoo::DString( argv[ iCurrArg ], strlen( argv[ iCurrArg ] ) );
				eCurrParamMode = Options::None;
				break;

			case Options::DInterfaceFiles:
				outputFolderDInterface = binderoo::DString( argv[ iCurrArg ], strlen( argv[ iCurrArg ] ) );
				eCurrParamMode = Options::None;
				break;

			case Options::FunctionCall:
				functionCall = binderoo::DString( argv[ iCurrArg ], strlen( argv[ iCurrArg ] ) );
				eCurrParamMode = Options::None;
				break;

			case Options::FunctionCallParameter:
				functionCallParameters[ iFunctionCallParameterCount++ ] = binderoo::DString( argv[ iCurrArg ], strlen( argv[ iCurrArg ] ) );
				eCurrParamMode = Options::None;
				break;

			default:
				iErrorParameters[ iErrorCount++ ] = iCurrArg;
				break;
			}
		}

		if( iErrorCount )
		{
			printf( "The following bad parameters found:\n" );
			for( int iError = 0; iError < iErrorCount; ++iError )
			{
				printf( "  %s\n", argv[ iErrorParameters[ iError ] ] );
			}
			printf( "\n" );

			printUsageScreen();
		}
		else if( eFoundParams == Options::None )
		{
			printf( "No commands found!\n\n" );

			printUsageScreen();
		}
		else if( iSearchFolderCount == 0 )
		{
			printf( "No search folders provided!\n\n" );

			printUsageScreen();
		}
		else
		{
			binderoo::HostConfiguration configuration;

			configuration.strDynamicLibSearchFolders = binderoo::Slice< binderoo::DString >( &searchFolders[ 0 ], (size_t)iSearchFolderCount );
			configuration.alloc = &test_malloc;
			configuration.free = &test_free;
			configuration.calloc = &test_calloc;
			configuration.realloc = &test_realloc;
			configuration.unaligned_alloc = &test_unaligned_malloc;
			configuration.unaligned_free = &test_unaligned_free;
			if( eFoundParams & Options::Quiet )
			{
				configuration.log_info = nullptr;
				configuration.log_warning = nullptr;
				configuration.log_error = nullptr;
			}
			else
			{
				if( eFoundParams & Options::Verbose )
				{
					log_info( "Verbose mode enabled." );
					configuration.log_info = &log_info;
					configuration.log_warning = &log_warning;
				}
				else
				{
					configuration.log_info = nullptr;
					configuration.log_warning = nullptr;
				}
				configuration.log_error = &log_error;
			}
			configuration.bStartInRapidIterationMode = false;

			binderoo::Host host( configuration );

			if( eFoundParams & Options::ExportBindingsForAll )
			{
				if( eFoundParams & Options::ListCPPStyle )
				{
					const char* pDeclarations = host.generateCPPStyleExportDeclarationsForAllObjects( pCBindingsForAllVersions );
					test_unaligned_free( (void*)pDeclarations );
				}
			}

			if( eFoundParams & Options::ImportBindingsForAll )
			{
				if( eFoundParams & Options::ListCSharpStyle )
				{
					const char* pDeclarations = host.generateCSharpStyleImportDeclarationsForAllObjects( pCBindingsForAllVersions );
					test_unaligned_free( (void*)pDeclarations );
				}
			}

			if( eFoundParams & Options::DInterfaceFiles )
			{
				host.generateDInterfaceFiles( outputFolderDInterface.data() );
			}

			if( eFoundParams & Options::FunctionCall )
			{
				ParamHandler handler( binderoo::Slice< binderoo::DString >( &functionCallParameters[ 0 ], (size_t)iFunctionCallParameterCount ) );
				handleFunction( functionCall.data(), handler );
			}
		}
	}

}
//----------------------------------------------------------------------------

//============================================================================
