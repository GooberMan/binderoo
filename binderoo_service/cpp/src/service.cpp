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

#include "binderoo/service.h"
#include "filewatcher.h"

#include "binderoo/fileutils.h"
#include "binderoo/sharedevent.h"

#include <atomic>
#include <map>
#include <set>

#include <Windows.h>
//----------------------------------------------------------------------------

binderoo::AllocatorFunc				binderoo::AllocatorFunctions< binderoo::AllocatorSpace::Service >::fAlloc		= nullptr;
binderoo::DeallocatorFunc			binderoo::AllocatorFunctions< binderoo::AllocatorSpace::Service >::fFree		= nullptr;
binderoo::CAllocatorFunc			binderoo::AllocatorFunctions< binderoo::AllocatorSpace::Service >::fCalloc		= nullptr;
binderoo::ReallocatorFunc			binderoo::AllocatorFunctions< binderoo::AllocatorSpace::Service >::fRealloc		= nullptr;

binderoo::Service*					binderoo::Service::spInstance = nullptr;
//----------------------------------------------------------------------------

namespace binderoo
{
	class Process
	{
	public:
		Process( const Containers< AllocatorSpace::Service >::InternalString& programLocation, const Containers< AllocatorSpace::Service >::InternalString& workingLocation, const Containers< AllocatorSpace::Service >::InternalString& parameters, const Containers< AllocatorSpace::Service >::StringVector& environmentVariables, bool bSynchronous = true )
			: strProgramLocation( programLocation )
			, strProgramParameters( parameters )
			, hStdOutputRead( nullptr )
			, hStdOutputWrite( nullptr )
			, hStdErrorRead( nullptr )
			, hStdErrorWrite( nullptr )
			, dReturnCode( -1 )
			, bLaunched( FALSE )
		{
			std::replace( strProgramLocation.begin(), strProgramLocation.end(), '/', '\\' );

			prepareEnvironmentVariables( environmentVariables );

			ZeroMemory( &startupInfo, sizeof( STARTUPINFO ) );
			ZeroMemory( &processInfo, sizeof( PROCESS_INFORMATION ) );

			SECURITY_ATTRIBUTES security;
			security.nLength				= sizeof( SECURITY_ATTRIBUTES );
			security.bInheritHandle			= TRUE;
			security.lpSecurityDescriptor	= NULL;

			BOOL bCreatedPipe = CreatePipe( &hStdOutputRead, &hStdOutputWrite, &security, 0 )
								&& CreatePipe( &hStdErrorRead, &hStdErrorWrite, &security, 0 );

			if( bCreatedPipe )
			{
				SetHandleInformation( hStdErrorRead, HANDLE_FLAG_INHERIT, 0 );
				SetHandleInformation( hStdOutputRead, HANDLE_FLAG_INHERIT, 0 );

				startupInfo.cb					= sizeof( STARTUPINFO );
				startupInfo.lpTitle				= "Binderoo files compiling...";
				startupInfo.hStdOutput			= hStdOutputWrite;
				startupInfo.hStdError			= hStdErrorWrite;
				startupInfo.hStdInput			= GetStdHandle( STD_INPUT_HANDLE );
				startupInfo.dwFlags				= STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES | STARTF_PREVENTPINNING;
				startupInfo.wShowWindow			= SW_HIDE;

				bLaunched = CreateProcess( strProgramLocation.c_str(), (char*)strProgramParameters.c_str(), NULL, NULL, TRUE, CREATE_NEW_CONSOLE, (void*)vecEnvironmentVariables.data(), workingLocation.c_str(), &startupInfo, &processInfo );

				char messageBuffer[ 2048 ] = { 0 };

				if( !bLaunched )
				{
					DWORD dError = GetLastError();
					FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, nullptr, dError, 0, messageBuffer, 2048, nullptr );
				}

				if( bSynchronous )
				{
					waitForProgramEnd();
				}
			}
		}
		//--------------------------------------------------------------------

		~Process()
		{
			CloseHandle( processInfo.hThread );
			CloseHandle( processInfo.hProcess );
			CloseHandle( hStdErrorRead );
			CloseHandle( hStdErrorWrite );
			CloseHandle( hStdOutputRead );
			CloseHandle( hStdOutputWrite );
		}
		//--------------------------------------------------------------------

		BIND_INLINE bool		launched() const
		{
			return bLaunched != FALSE;
		}
		//--------------------------------------------------------------------

		BIND_INLINE bool		succeeded() const
		{
			return launched() && dReturnCode == 0;
		}
		//--------------------------------------------------------------------

		BIND_INLINE bool		running() const
		{
			if( !launched() )
			{
				return false;
			}
			DWORD dCode = 0;
			BOOL bSucceeded = GetExitCodeProcess( processInfo.hProcess, &dCode );
			return bSucceeded != FALSE && dCode == STILL_ACTIVE;
		}
		//--------------------------------------------------------------------

		void					updateObjects()
		{
			updateStdStream( hStdOutputRead, strStdOut );
			updateStdStream( hStdErrorRead, strStdError );

			GetExitCodeProcess( processInfo.hProcess, &dReturnCode );
		}
		//--------------------------------------------------------------------

		void					waitForProgramEnd()
		{
			while( running() )
			{
				updateObjects();
				Sleep( 0 );
			}

			updateObjects();
		}
		//--------------------------------------------------------------------

		const Containers< AllocatorSpace::Service >::InternalString& stdOut() const { return strStdOut; }
		const Containers< AllocatorSpace::Service >::InternalString& stdError() const { return strStdError; }
		//--------------------------------------------------------------------

	private:

		void					updateStdStream( HANDLE hStream, Containers< AllocatorSpace::Service >::InternalString& strOutput )
		{
			DWORD dBytes = 0;
			PeekNamedPipe( hStream, NULL, 0, NULL, &dBytes, 0 );

			if( dBytes != 0 )
			{
				const DWORD dBufferSize = 1025;
				const DWORD dBufferReadSize = dBufferSize - 1;
				char buffer[ dBufferSize ];

				while( dBytes > 0 )
				{
					DWORD dReadBytes = 0;
					ReadFile( hStream, buffer, min( dBytes, dBufferReadSize ), &dReadBytes, NULL );
					buffer[ dReadBytes ] = 0;
					strOutput += buffer;
					dBytes -= dReadBytes;
				}
			}
		}
		//--------------------------------------------------------------------

		void prepareEnvironmentVariables( const Containers< AllocatorSpace::Service >::StringVector& vecAdditionalVariables )
		{
			char* pOldEnvironmentStrings = GetEnvironmentStrings();

			Containers< AllocatorSpace::Service >::StringVector vecNewVariables;

			int iTotalSize = 0;

			const char* pCurrEnvString = pOldEnvironmentStrings;
			while( *pCurrEnvString != 0 )
			{
				int iStrSize = lstrlen( pCurrEnvString ) + 1;
				vecNewVariables.push_back( Containers< AllocatorSpace::Service >::InternalString( pCurrEnvString ) );
				pCurrEnvString += iStrSize;
				iTotalSize += iStrSize;
			}

			for( const Containers< AllocatorSpace::Service >::InternalString& strEnvVar : vecAdditionalVariables )
			{
				vecNewVariables.push_back( strEnvVar );
				iTotalSize += (int)( strEnvVar.length() + 1 );
			}
			iTotalSize += 1;

			FreeEnvironmentStrings( pOldEnvironmentStrings );

			vecEnvironmentVariables.resize( iTotalSize );

			auto sortFunc = []( const binderoo::Containers< AllocatorSpace::Service >::InternalString& lhs, const binderoo::Containers< AllocatorSpace::Service >::InternalString& rhs ) -> bool
			{
				auto tempLHS = lhs;
				auto tempRHS = rhs;
				std::transform( tempLHS.begin(), tempLHS.end(), tempLHS.begin(), tolower );
				std::transform( tempRHS.begin(), tempRHS.end(), tempRHS.begin(), tolower );
				return tempLHS < tempRHS;
			};

			std::sort( vecNewVariables.begin(), vecNewVariables.end(), sortFunc );

			char* pOutput = vecEnvironmentVariables.data();
			char* pEnd = pOutput + iTotalSize;
			for( const Containers< AllocatorSpace::Service >::InternalString& strEnvVar : vecNewVariables )
			{
//				OutputDebugString( strEnvVar.c_str() );
//				OutputDebugString( "\n" );
				strcpy_s( pOutput, (ptrdiff_t)pEnd - (ptrdiff_t)pOutput, strEnvVar.data() );
				pOutput += (int)( strEnvVar.length() + 1 );
			}
			*pOutput = 0;
		}
		//--------------------------------------------------------------------

		Containers< AllocatorSpace::Service >::InternalString			strProgramLocation;
		Containers< AllocatorSpace::Service >::InternalString			strProgramParameters;
		Containers< AllocatorSpace::Service >::CharVector				vecEnvironmentVariables;

		Containers< AllocatorSpace::Service >::InternalString			strStdOut;
		Containers< AllocatorSpace::Service >::InternalString			strStdError;

		STARTUPINFO				startupInfo;
		PROCESS_INFORMATION		processInfo;

		HANDLE					hStdOutputRead;
		HANDLE					hStdOutputWrite;
		HANDLE					hStdErrorRead;
		HANDLE					hStdErrorWrite;

		DWORD					dReturnCode;
		BOOL					bLaunched;
	};
	//------------------------------------------------------------------------

	void makeSureDirectoryIsThere( const Containers< AllocatorSpace::Service >::InternalString& strFolder )
	{
		if( !FileUtils< AllocatorSpace::Service >::exists( strFolder ) || !FileUtils< AllocatorSpace::Service >::isDirectory( strFolder ) )
		{
			FileUtils< AllocatorSpace::Service >::createDirectory( strFolder );
		}
	}

	bool compile(	const Compiler& compiler, const ModuleVersion& version, const MonitoredFolder& folder,
					const Containers< AllocatorSpace::Service >::StringVector& vecInputFiles,
					Containers< AllocatorSpace::Service >::InternalString& strOutputError,
					bool bForRapidIteration )
	{
		if( !vecInputFiles.empty() )
		{
			Containers< AllocatorSpace::Service >::InternalString strVersionName( version.strVersionName.data(), version.strVersionName.length() );
			Containers< AllocatorSpace::Service >::InternalString strLibraryName( folder.strClientLibraryName.data(), folder.strClientLibraryName.length() );
			Containers< AllocatorSpace::Service >::InternalString strLibraryOutputName = strLibraryName;
			
			Containers< AllocatorSpace::Service >::InternalString strBinderooTemp = FileUtils< AllocatorSpace::Service >::getTempDirectory();
			makeSureDirectoryIsThere( strBinderooTemp );

			strBinderooTemp += "service/";
			makeSureDirectoryIsThere( strBinderooTemp );

			if( strVersionName.length() > 0 )
			{
				strLibraryOutputName += "_" + strVersionName;
				strBinderooTemp += strVersionName + "/";
			}
			else
			{
				strBinderooTemp += "__noversion__/";
			}
			makeSureDirectoryIsThere( strBinderooTemp );

			Containers< AllocatorSpace::Service >::InternalString strTempRoot = strBinderooTemp + strLibraryName + "/";
			makeSureDirectoryIsThere( strTempRoot );

			if( bForRapidIteration )
			{
				strTempRoot += "rapid/";
				makeSureDirectoryIsThere( strTempRoot );
			}

			Containers< AllocatorSpace::Service >::InternalString strCompilerExecutable = Containers< AllocatorSpace::Service >::InternalString( compiler.strCompilerLocation.data(), compiler.strCompilerLocation.length() );
			strCompilerExecutable += "/windows/bin/dmd.exe";

			Containers< AllocatorSpace::Service >::InternalString strOutput = " -of\"";
			strOutput += strTempRoot;
			strOutput += strLibraryOutputName;
			strOutput += ".dll\"";

			Containers< AllocatorSpace::Service >::InternalString strDeps = " -deps=\"";
			strDeps += strTempRoot;
			strDeps += strLibraryOutputName;
			strDeps += ".deps\"";

			Containers< AllocatorSpace::Service >::InternalString strVersions;
			for( const DString& token : version.versionTokens )
			{
				strVersions += " -version=";
				strVersions += Containers< AllocatorSpace::Service >::InternalString( token.data(), token.length() );
			}

			Containers< AllocatorSpace::Service >::InternalString strArguments = "-d -m64 -g -L/DLL";
			if( bForRapidIteration )
			{
				strArguments += " -debug";
			}
			strArguments += strOutput;
			strArguments += strDeps;
			strArguments += strVersions;

			for( const Containers< AllocatorSpace::Service >::InternalString& strInputFile : vecInputFiles )
			{
				strArguments += " \"";
				strArguments += strInputFile;
				strArguments += "\"";
			}

			Containers< AllocatorSpace::Service >::StringVector vecEnvironmentVariables;

			{
				Containers< AllocatorSpace::Service >::InternalString strVSInstallDir( compiler.platformConfig.windows.strVisualStudioInstallDir.data(), compiler.platformConfig.windows.strVisualStudioInstallDir.length() );

				Containers< AllocatorSpace::Service >::InternalString strScratch;

				strScratch = "VSINSTALLDIR=";
				strScratch += strVSInstallDir;
				std::replace( strScratch.begin(), strScratch.end(), '/', '\\' );
				vecEnvironmentVariables.push_back( strScratch );

				strScratch = "VCINSTALLDIR=";
				strScratch += strVSInstallDir;
				strScratch += "\\VC";
				std::replace( strScratch.begin(), strScratch.end(), '/', '\\' );
				vecEnvironmentVariables.push_back( strScratch );

				strScratch = "LINKCMD64=";
				strScratch += strVSInstallDir;
				strScratch += "\\VC\\bin\\amd64\\link.exe";
				std::replace( strScratch.begin(), strScratch.end(), '/', '\\' );
				vecEnvironmentVariables.push_back( strScratch );

				strScratch = "WindowsSdkDir=";
				strScratch += Containers< AllocatorSpace::Service >::InternalString( compiler.platformConfig.windows.strWindowsSdkDir.data(), compiler.platformConfig.windows.strWindowsSdkDir.length() );
				std::replace( strScratch.begin(), strScratch.end(), '/', '\\' );
				vecEnvironmentVariables.push_back( strScratch );

				strScratch = "VisualStudioVersion=";
				strScratch += Containers< AllocatorSpace::Service >::InternalString( compiler.platformConfig.windows.strVisualStudioVersion.data(), compiler.platformConfig.windows.strVisualStudioVersion.length() );
				std::replace( strScratch.begin(), strScratch.end(), '/', '\\' );
				vecEnvironmentVariables.push_back( strScratch );

/*				vecEnvironmentVariables.push_back( Containers< AllocatorSpace::Service >::InternalString( "VSINSTALLDIR=C:\\Program Files (x86)\\Microsoft Visual Studio 11.0" ) );
				vecEnvironmentVariables.push_back( Containers< AllocatorSpace::Service >::InternalString( "VCINSTALLDIR=C:\\Program Files (x86)\\Microsoft Visual Studio 11.0\\VC" ) );
				vecEnvironmentVariables.push_back( Containers< AllocatorSpace::Service >::InternalString( "LINKCMD64=C:\\Program Files (x86)\\Microsoft Visual Studio 11.0\\VC\\bin\\amd64\\link.exe" ) );
				vecEnvironmentVariables.push_back( Containers< AllocatorSpace::Service >::InternalString( "WindowsSdkDir=C:\\Program Files (x86)\\Windows Kits\\8.1" ) );
				vecEnvironmentVariables.push_back( Containers< AllocatorSpace::Service >::InternalString( "VisualStudioVersion=11.0" ) );*/

			}

			Process compileProcess( strCompilerExecutable, strTempRoot, strArguments, vecEnvironmentVariables, true );

			if( compileProcess.succeeded() )
			{
				Containers< AllocatorSpace::Service >::InternalString strOutputPath( folder.strOutputFolder.data(), folder.strOutputFolder.length() );
				makeSureDirectoryIsThere( strOutputPath );

				if( bForRapidIteration )
				{
					strOutputPath += "rapid/";
					makeSureDirectoryIsThere( strOutputPath );
				}

				Containers< AllocatorSpace::Service >::InternalString strDLLSource = strTempRoot + strLibraryOutputName + ".dll";
				Containers< AllocatorSpace::Service >::InternalString strPDBSource = strTempRoot + strLibraryOutputName + ".pdb";

				Containers< AllocatorSpace::Service >::InternalString strDLLDest = strOutputPath + strLibraryOutputName + ".dll";
				Containers< AllocatorSpace::Service >::InternalString strPDBDest = strOutputPath + strLibraryOutputName + ".pdb";

				BOOL bCopiedPDB = CopyFile( strPDBSource.c_str(), strPDBDest.c_str(), FALSE );
				BOOL bCopiedDLL = CopyFile( strDLLSource.c_str(), strDLLDest.c_str(), FALSE );

				DeleteFile( strPDBSource.c_str() );
				DeleteFile( strDLLSource.c_str() );

				return !!bCopiedDLL && !!bCopiedPDB;
			}

			strOutputError = compileProcess.stdError();
		}

		return false;
	}
	//------------------------------------------------------------------------
}
//----------------------------------------------------------------------------

namespace binderoo
{
	class ServiceImplementation
	{
	public:
		ServiceImplementation( ServiceConfiguration& configuration );
		~ServiceImplementation();

		int32_t						threadFunction( ThreadOSUpdateFunction threadOSUpdate );

		void						setRapidIterationMode( bool bSet );
		bool						isInRapidIterationMode( ) const;
		void						compileClients( CompileFinishedCallback callWhenDone );

		bool						compileFolder( binderoo::MonitoredFolder& folder, bool bRapidIterationMode );

	private:
		void						logInfo( const char* pMessage );
		void						logWarning( const char* pMessage );
		void						logError( const char* pMessage );

		SharedEvent					reloadEvent;

		FileWatcher*				pWatcher;

		ServiceConfiguration*		pConfiguration;
		void*						pThread;

		CompileFinishedCallback		compileFinishedCallback;
		std::atomic< int32_t >		iNumCompilesRequested;
		std::atomic< bool >			bInRapidIterationMode;
		std::atomic< bool >			bHaltExecution;
		std::atomic< bool >			bRunning;
	};
}
//----------------------------------------------------------------------------

binderoo::ServiceImplementation::ServiceImplementation( ServiceConfiguration& configuration )
	: reloadEvent( "binderoo_service_reload" )
	, pWatcher( nullptr )
	, pConfiguration( &configuration )
	, pThread( nullptr )
	, iNumCompilesRequested( 0 )
	, bInRapidIterationMode( configuration.bStartInRapidIterationMode )
	, bHaltExecution( false )
	, bRunning( false )
{
	pThread = pConfiguration->create_thread( fastdelegate::MakeDelegate( this, &binderoo::ServiceImplementation::threadFunction ) );
}
//----------------------------------------------------------------------------

binderoo::ServiceImplementation::~ServiceImplementation()
{
	bHaltExecution = true;

	pConfiguration->wait_on_thread( pThread );
	pConfiguration->destroy_thread( pThread );
}
//----------------------------------------------------------------------------

int32_t binderoo::ServiceImplementation::threadFunction( binderoo::ThreadOSUpdateFunction threadOSUpdate )
{
	bRunning = true;

	pWatcher = AllocatorFunctions< AllocatorSpace::Service >::alloc< FileWatcher >( );
	new( pWatcher ) FileWatcher( pConfiguration->folders );

	std::set< MonitoredFolder* > changedFolders;
	for( auto& folder : pConfiguration->folders )
	{
		// std::set doesn't have a reserve function. This is essentially a hack, but should reserve all our memory.
		changedFolders.insert( &folder );
	}

	logInfo( "Binderoo Service: Started" );

	bool bWasInRapidMode = false;
	bool bPerformedInitialCompile = false;

	while( !bHaltExecution )
	{
		threadOSUpdate();

		changedFolders.clear();

		CompileFinishedCallback compileFinished;
		bool bNowInRapidMode = bInRapidIterationMode.load();
		bool bCompileForRapidIteration = false;

		if( !bWasInRapidMode && bNowInRapidMode )
		{
			if( !bPerformedInitialCompile )
			{
				// Flush any file changes already detected
				pWatcher->detectFileChanges();

				logInfo( "Binderoo Service Rapid Iteration: Started, performing initial compile." );

				// Kick off a recompile to begin with since we don't track state anywhere
				bool bInitialCompileSuccessful = true;
				for( auto& folder : pConfiguration->folders )
				{
					bInitialCompileSuccessful &= compileFolder( folder, true );
				}

				if( bInitialCompileSuccessful )
				{
					logInfo( "Binderoo Service Rapid Iteration: Initial compile was successful. Reload is being triggered." );
					reloadEvent.signal();
				}
				else
				{
					logError( "Binderoo Service Rapid Iteration: Errors were encountered in initial compile. Reload is NOT being triggered." );
				}

				bPerformedInitialCompile = true;
			}
			else
			{
				logInfo( "Binderoo Service Rapid Iteration: Started." );
			}
		}
		else if( bWasInRapidMode && !bNowInRapidMode )
		{
			logInfo( "Binderoo Service Rapid Iteration: Stopped." );
		}
		else if( bNowInRapidMode && pWatcher->detectFileChanges() )
		{
			bCompileForRapidIteration = true;

			const ChangedFilesVector& changedFiles = pWatcher->getChangedFiles();

			bool bImportPathsChanged = false;

			for( auto& changedFile : changedFiles )
			{
				changedFolders.insert( changedFile.pThisFolder );
				bImportPathsChanged |= ( changedFile.pThisFolder->eType == binderoo::MonitoredFolderType::ImportsPath );
			}

			if( bImportPathsChanged )
			{
				logInfo( "Binderoo Service Rapid Iteration: Import path change detected. Recompiling all." );

				changedFolders.clear();
				for( auto& folder : pConfiguration->folders )
				{
					// std::set doesn't have a reserve function. This is essentially a hack, but should reserve all our memory.
					changedFolders.insert( &folder );
				}
			}
			else
			{
				logInfo( "Binderoo Service Rapid Iteration: Module change detected. Recompiling some." );
			}
		}
		else if( iNumCompilesRequested.load() > 0 )
		{
			compileFinished = compileFinishedCallback;
			--iNumCompilesRequested;
			logInfo( "Binderoo Service: Recompile requested." );

			for( auto& folder : pConfiguration->folders )
			{
				// std::set doesn't have a reserve function. This is essentially a hack, but should reserve all our memory.
				changedFolders.insert( &folder );
			}
		}

		if( !changedFolders.empty() )
		{
			bool bCompiledAll = true;
			// Recompile only changed clients
			for( auto& changedFolder : changedFolders )
			{
				bCompiledAll &= compileFolder( *changedFolder, bCompileForRapidIteration );
			}

			if( bCompiledAll )
			{
				logInfo( "Binderoo Service: Changed modules recompiled successfully. Reload is being triggered." );
				reloadEvent.signal();
			}
			else
			{
				logError( "Binderoo Service: Errors were encountered. Reload is NOT being triggered." );
			}

			if( !compileFinished.empty() )
			{
				compileFinished( bCompiledAll );
			}
		}

		bWasInRapidMode = bNowInRapidMode;

		::SleepEx( 1000, true );

	}

	bRunning = false;

	return 0;
}
//----------------------------------------------------------------------------

void binderoo::ServiceImplementation::setRapidIterationMode( bool bSet )
{
	bInRapidIterationMode = bSet;
}
//----------------------------------------------------------------------------

bool binderoo::ServiceImplementation::isInRapidIterationMode( ) const
{
	return bInRapidIterationMode.load();
}
//----------------------------------------------------------------------------

void binderoo::ServiceImplementation::compileClients( CompileFinishedCallback callWhenDone )
{
	compileFinishedCallback = callWhenDone;
	++iNumCompilesRequested;
}
//----------------------------------------------------------------------------

bool binderoo::ServiceImplementation::compileFolder( binderoo::MonitoredFolder& folder, bool bRapidIterationMode )
{
	if( folder.eType == binderoo::MonitoredFolderType::ClientPath )
	{
		bool bCompiled = true;

		Containers< AllocatorSpace::Service >::StringVector vecAllFiles;

		for( auto& folder : pConfiguration->folders )
		{
			if( folder.eType == binderoo::MonitoredFolderType::ImportsPath )
			{
				pWatcher->getAllFiles( folder, vecAllFiles );
			}
		}

		pWatcher->getAllFiles( folder, vecAllFiles );

		Containers< AllocatorSpace::Service >::InternalString strError;

		if( pConfiguration->versions.length() )
		{
			for( ModuleVersion& version : pConfiguration->versions )
			{
				strError.clear();
				bCompiled &= binderoo::compile( pConfiguration->compilers[ 0 ], version, folder, vecAllFiles, strError, bRapidIterationMode );
				if( !strError.empty() )
				{
					Containers< AllocatorSpace::Service >::InternalString strErrorMessage;
					strErrorMessage += "Module ";
					strErrorMessage += Containers< AllocatorSpace::Service >::InternalString( folder.strClientLibraryName.data(), folder.strClientLibraryName.length() );
					strErrorMessage += " version ";
					strErrorMessage += Containers< AllocatorSpace::Service >::InternalString( version.strVersionName.data(), version.strVersionName.length() );
					strErrorMessage += " failed to compile. Error log as follows.\n\n";
					strErrorMessage += strError;

					logError( strError.c_str() );
				}
			}
		}
		else
		{
			strError.clear();
			ModuleVersion dummyVersion;
			bCompiled &= binderoo::compile( pConfiguration->compilers[ 0 ], dummyVersion, folder, vecAllFiles, strError, bRapidIterationMode );

			if( !strError.empty() )
			{
				Containers< AllocatorSpace::Service >::InternalString strErrorMessage;
				strErrorMessage += "Module ";
				strErrorMessage += Containers< AllocatorSpace::Service >::InternalString( folder.strClientLibraryName.data(), folder.strClientLibraryName.length() );
				strErrorMessage += " failed to compile. Error log as follows.\n\n";
				strErrorMessage += strError;

				logError( strError.c_str() );
			}
		}

		return bCompiled;
	}
	else if( folder.eType == binderoo::MonitoredFolderType::ImportsPath )
	{
		// True for now until we can actually compile libs...
		return true;
	}

	return false;
}
//----------------------------------------------------------------------------

void binderoo::ServiceImplementation::logInfo( const char* pMessage )
{
	if( pConfiguration->log_info )
	{
		pConfiguration->log_info( pMessage );
	}
}
//----------------------------------------------------------------------------

void binderoo::ServiceImplementation::logWarning( const char* pMessage )
{
	if( pConfiguration->log_warning )
	{
		pConfiguration->log_warning( pMessage );
	}
}
//----------------------------------------------------------------------------

void binderoo::ServiceImplementation::logError( const char* pMessage )
{
	if( pConfiguration->log_error )
	{
		pConfiguration->log_error( pMessage );
	}
}
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------

binderoo::Service::Service( ServiceConfiguration& configuration )
	: config( configuration )
	, pImplementation( nullptr )
{
	binderoo::AllocatorFunctions< binderoo::AllocatorSpace::Service >::setup( configuration.alloc, configuration.free, configuration.calloc, configuration.realloc );

	pImplementation = (ServiceImplementation*)config.alloc( sizeof( ServiceImplementation ), sizeof( size_t ) );
	new( pImplementation ) ServiceImplementation( config );

	spInstance = this;
}
//----------------------------------------------------------------------------

binderoo::Service::~Service()
{
	pImplementation->~ServiceImplementation();

	config.free( pImplementation );

	binderoo::AllocatorFunctions< binderoo::AllocatorSpace::Service >::setup( nullptr, nullptr, nullptr, nullptr );
}
//----------------------------------------------------------------------------

void binderoo::Service::setRapidIterationMode( bool bSet )
{
	pImplementation->setRapidIterationMode( bSet );
}
//----------------------------------------------------------------------------

bool binderoo::Service::isInRapidIterationMode( ) const
{
	return pImplementation->isInRapidIterationMode();
}
//----------------------------------------------------------------------------

void binderoo::Service::compileClients( CompileFinishedCallback callWhenDone )
{
	pImplementation->compileClients( callWhenDone );
}
//----------------------------------------------------------------------------

void binderoo::Service::compileClientBlocking( MonitoredFolder& thisClient )
{
	pImplementation->compileFolder( thisClient, false );
}
//----------------------------------------------------------------------------

//============================================================================
