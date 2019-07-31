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

#include "binderoo/host.h"

#include "binderoo/boundfunction.h"
#include "binderoo/boundobject.h"
#include "binderoo/containers.h"
#include "binderoo/exports.h"
#include "binderoo/hash.h"
#include "binderoo/imports.h"
#include "binderoo/sharedevent.h"
#include "binderoo/slice.h"

#include <unordered_map>
#include <vector>
#include <string>
#include <algorithm>
#include <atomic>

#include <string.h>

#if BIND_SYSAPI == BIND_SYSAPI_WINAPI || BIND_SYSAPI == BIND_SYSAPI_UWP
	#include <Windows.h>

	#if BIND_SYSAPI == BIND_SYSAPI_WINAPI
		// Rapid iteration functionality
		#define BINDEROOHOST_RAPIDITERATION 1
		// Load library from disk
		#define BINDEROOHOST_LOADPACKAGEDLIBRARY 0
		// Multi-byte load library
		#define BINDEROOHOST_LOADLIBRARYA 1
	#else
		// Static loading functionality
		#define BINDEROOHOST_RAPIDITERATION 0
		// Load library from App Package
		#define BINDEROOHOST_LOADPACKAGEDLIBRARY 1
		// Wide char load library
		#define BINDEROOHOST_LOADLIBRARYA 0
	#endif // SYSAPI check

	#define GetModuleSymbolAddress GetProcAddress

	#if BINDEROOHOST_RAPIDITERATION
		#pragma warning( push )
		#pragma warning( disable: 4091 )
		#include <DbgHelp.h>
		#pragma warning( pop )
		
		#pragma comment( lib, "dbghelp.lib" )
		#pragma comment( lib, "Rpcrt4.lib" )
	#endif // BINDEROOHOST_RAPIDITERATION
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
	#include <dirent.h>
	#include <dlfcn.h>
	#include <errno.h>
	#include <unistd.h>

	#define GetModuleSymbolAddress dlsym

	#define BINDEROOHOST_RAPIDITERATION 0
#endif // SYSAPI check

template<>
binderoo::AllocatorFunc				binderoo::AllocatorFunctions< binderoo::AllocatorSpace::Host >::fAlloc		= nullptr;

template<>
binderoo::DeallocatorFunc			binderoo::AllocatorFunctions< binderoo::AllocatorSpace::Host >::fFree		= nullptr;

template<>
binderoo::CAllocatorFunc			binderoo::AllocatorFunctions< binderoo::AllocatorSpace::Host >::fCalloc		= nullptr;

template<>
binderoo::ReallocatorFunc			binderoo::AllocatorFunctions< binderoo::AllocatorSpace::Host >::fRealloc	= nullptr;

binderoo::Host*	binderoo::Host::pActiveHost																		= nullptr;
//----------------------------------------------------------------------------

namespace binderoo
{
	typedef Containers< AllocatorSpace::Host >::InternalString												InternalString;
	typedef std::vector< InternalString, binderoo::Allocator< AllocatorSpace::Host, InternalString > >		InternalStringVector;

	typedef void					( BIND_C_CALL *BinderooInitDeinitPtr )							( );
	typedef void					( BIND_C_CALL *ImportFunctionsFromPtr )							( binderoo::Slice< binderoo::BoundFunction > functions );
	typedef void					( BIND_C_CALL *GetExportedObjectsPtr )							( binderoo::Slice< binderoo::BoundObject >* pOutExportedObjects );
	typedef void					( BIND_C_CALL *GetExportedFunctionsPtr )						( binderoo::Slice< binderoo::BoundFunction >* pOutExportedFunctions );
	typedef void*					( BIND_C_CALL *CreateObjectByNamePtr )							( DString strName );
	typedef void*					( BIND_C_CALL *CreateObjectByHashPtr )							( uint64_t uNameHash );
	typedef void					( BIND_C_CALL *DestroyObjectByNamePtr )							( DString, void* pObject );
	typedef void					( BIND_C_CALL *DestroyObjectByHashPtr )							( uint64_t, void* pObject );
	typedef const char*				( BIND_C_CALL *GenerateDeclarationsForAllObjectsPtr )			( UnalignedAllocatorFunc allocator, const char* pVersion );
	typedef void					( BIND_C_CALL *GenerateDInterfaceFilesPtr )						( const char* pOutputFolder );
	//------------------------------------------------------------------------

	#if BIND_SYSAPI == BIND_SYSAPI_WINAPI || BIND_SYSAPI == BIND_SYSAPI_UWP
		typedef HMODULE ModuleHandle;
		constexpr ModuleHandle INVALID_MODULE = nullptr;
	#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
		typedef void* ModuleHandle;
		constexpr ModuleHandle INVALID_MODULE = nullptr;
	#endif
	//------------------------------------------------------------------------

	enum class DynamicLibStatus : int
	{
		NotLoaded,
		ModuleInMemory,
		LoadFailed,
		CoreInterfaceNotFound,
		Unloaded,

		Ready,

		Max,
		Min = 0
	};
	//------------------------------------------------------------------------

	static const char* pDynamicLibStatusMessages[] =
	{
		"Not loaded",						// NotLoaded
		"Load failed",						// LoadFailed
		"Core interface not found",			// CoreInterfaceNotFound
		"Unloaded",							// Unloaded
		"Ready",							// Ready
	};
	//------------------------------------------------------------------------

	struct HostDynamicLib
	{
		HostDynamicLib()
			: hModule( nullptr )
			, eStatus( DynamicLibStatus::NotLoaded )
			, binderoo_startup( nullptr )
			, binderoo_shutdown( nullptr )
			, importFunctionsFrom( nullptr )
			, getExportedObjects( nullptr )
			, getExportedFunctions( nullptr )
			, createObjectByName( nullptr )
			, createObjectByHash( nullptr )
			, destroyObjectByName( nullptr )
			, destroyObjectByHash( nullptr )
			, generateCPPStyleExportDeclarationsForAllObjects( nullptr )
		{
		}
		//--------------------------------------------------------------------

		InternalString									strName;
		InternalString									strPath;
		InternalString									strScratchPath;
		InternalString									strScratchLib;
		InternalString									strScratchSymbols;
		ModuleHandle									hModule;
		DynamicLibStatus								eStatus;

		BinderooInitDeinitPtr							binderoo_startup;
		BinderooInitDeinitPtr							binderoo_shutdown;
		ImportFunctionsFromPtr							importFunctionsFrom;
		GetExportedObjectsPtr							getExportedObjects;
		GetExportedFunctionsPtr							getExportedFunctions;
		CreateObjectByNamePtr							createObjectByName;
		CreateObjectByHashPtr							createObjectByHash;
		DestroyObjectByNamePtr							destroyObjectByName;
		DestroyObjectByHashPtr							destroyObjectByHash;
		GenerateDeclarationsForAllObjectsPtr			generateCPPStyleExportDeclarationsForAllObjects;
		GenerateDeclarationsForAllObjectsPtr			generateCSharpStyleImportDeclarationsForAllObjects;
		GenerateDInterfaceFilesPtr						generateDInterfaceFiles;
	};
	//------------------------------------------------------------------------

	template< typename _ty, typename _hash >
	struct HostBinding
	{
		typedef _ty			BoundType;
		typedef _hash		HashType;

		HostBinding()
			: pLibrary( nullptr )
			, pObject( nullptr )
			, uSearchHash( HashType() )
		{
		}
		//--------------------------------------------------------------------

		BIND_INLINE bool operator==( const BoundType* pRHS ) const				{ return pObject == pRHS; }
		BIND_INLINE bool operator==( const HashType& RHS ) const				{ return uSearchHash == RHS; }
		//--------------------------------------------------------------------

		HostDynamicLib*									pLibrary;
		BoundType*										pObject;
		HashType										uSearchHash;
	};
	//------------------------------------------------------------------------

	typedef HostBinding< BoundFunction, BoundFunction::Hashes >												HostBoundFunction;
	typedef HostBinding< BoundObject, uint64_t >															HostBoundObject;
	//------------------------------------------------------------------------

	struct HostImportedObjectInstance
	{
		HostImportedObjectInstance()
			: pInstance( nullptr )
			, m_bHadInstance( false )
		{
			_mm_mfence();
		}
		//--------------------------------------------------------------------

		~HostImportedObjectInstance()
		{
			_mm_mfence();
			//OutputDebugString( "There's a timing issue here somewhere in binderoo...\n" );
		}

		BIND_INLINE bool operator==( const ImportedBase* pRHS ) const			{ return pInstance == pRHS; }
		//--------------------------------------------------------------------

		//InternalString									strReloadData;
		//InternalString									strClass;
		ImportedBase*									pInstance;
		bool											m_bHadInstance;
	};
	//------------------------------------------------------------------------

	class HostImplementation // It doesn't look like anything to me.
	{
	public:
		typedef Allocator< AllocatorSpace::Host, void, 16 >													DefaultAllocator;

		typedef Allocator< AllocatorSpace::Host, BoundFunction >											FunctionAllocator;
		typedef std::vector< BoundFunction, FunctionAllocator >												FunctionVector;

		typedef binderoo::FNV1aHasher< BoundFunction::Hashes >												FunctionMapHasher;
		typedef std::pair< const BoundFunction::Hashes, size_t >											FunctionMapPair;
		typedef Allocator< AllocatorSpace::Host, FunctionMapPair >											FunctionMapAllocator;

		typedef std::equal_to< BoundFunction::Hashes >														FunctionMapComparer;
		typedef std::unordered_map< BoundFunction::Hashes, size_t, FunctionMapHasher, FunctionMapComparer, FunctionMapAllocator >	FunctionMap;

		typedef std::vector< HostDynamicLib, Allocator< AllocatorSpace::Host, HostDynamicLib > >			DynamicLibVector;
		typedef std::vector< HostBoundFunction, Allocator< AllocatorSpace::Host, HostBoundFunction > >		HostBoundFunctionVector;
		typedef std::vector< HostBoundObject, Allocator< AllocatorSpace::Host, HostBoundObject > >			HostBoundObjectVector;

		typedef std::vector< binderoo::ImportedBase*, binderoo::Allocator< AllocatorSpace::Host, binderoo::ImportedBase* > > ImportedFunctionVector;
		typedef std::vector< HostImportedObjectInstance, binderoo::Allocator< AllocatorSpace::Host, HostImportedObjectInstance > > ImportedObjectVector;
		//--------------------------------------------------------------------

		HostImplementation( HostConfiguration& config );
		//--------------------------------------------------------------------

		void						setRapidIterationMode( bool bSet );
		bool						isInRapidIterationMode( ) const;
		//--------------------------------------------------------------------

		bool						checkForReloads();
		void						performReloads();
		//--------------------------------------------------------------------

		void						saveObjectData();
		void						loadObjectData();
		//--------------------------------------------------------------------

		void						performLoad();
		void						performUnload();
		//--------------------------------------------------------------------

		void						destroyImportedObjects();
		void						recreateImportedObjects();
		//--------------------------------------------------------------------

		void						collectExports();
		void						collectDynamicLibraries();
		void						collectBoundFunctions();
		void						collectBoundObjects();
		//--------------------------------------------------------------------

		bool						loadDynamicLibrary( HostDynamicLib& lib );
		void						unloadDynamicLibrary( HostDynamicLib& lib );
		//--------------------------------------------------------------------

		void						registerImportedClassInstance( ImportedBase* pInstance );
		void						deregisterImportedClassInstance( ImportedBase* pInstance );

		void						registerImportedFunction( ImportedBase* pInstance );
		void						deregisterImportedFunction( ImportedBase* pInstance );
		//--------------------------------------------------------------------

		void*						createImportedClass( const char* pName );
		bool						destroyImportedClass( const char* pName, void* pObject );
		//--------------------------------------------------------------------

		const HostBoundFunction*	getImportedFunctionDetails( const char* pName, const char* pSignature ) const;
		bool						getImportedFunctionDetails( const char* pName, Host::OverloadContainer& allOverloads ) const;
		const HostBoundObject*		getImportedObjectDetails( const char* pName ) const;
		//--------------------------------------------------------------------

		const char*					generateCPPStyleExportDeclarationsForAllObjects( const char* pVersions );
		const char*					generateCSharpStyleImportDeclarationsForAllObjects( const char* pVersions );
		void						generateDInterfaceFiles( const char* pOutputFolder );
		//--------------------------------------------------------------------

	private:
		void						logInfo( const char* pMessage );
		void						logWarning( const char* pMessage );
		void						logError( const char* pMessage );

		HostConfiguration&			configuration;

		SharedEvent					reloadEvent;

		FunctionMap					mapExportedFunctionIndices;
		FunctionVector				vecExportedFunctions;

		DynamicLibVector			vecDynamicLibs;
		HostBoundFunctionVector		vecBoundFunctions;
		HostBoundObjectVector		vecBoundObjects;

		ImportedFunctionVector		vecImportFunctionInstances;
		std::atomic< bool >			lockImportFunctionInstances;
		ImportedObjectVector		vecImportClassInstances;
		std::atomic< bool >			lockImportClassInstances;

		bool						bReloadLibs;
		bool						bInRapidIterationMode;
	};
	//------------------------------------------------------------------------
}
//----------------------------------------------------------------------------

binderoo::Host::Host( binderoo::HostConfiguration& config )
	: configuration( config )
	, pImplementation( nullptr )
{
	AllocatorFunctions< AllocatorSpace::Host >::setup( config.alloc, config.free, config.calloc, config.realloc );

	pImplementation = (HostImplementation*)config.alloc( sizeof( HostImplementation ), sizeof( size_t ) );
	new( pImplementation ) HostImplementation( config );

	pActiveHost = this;
}
//----------------------------------------------------------------------------

binderoo::Host::~Host()
{
	pImplementation->~HostImplementation();

	pActiveHost = nullptr;

	configuration.free( pImplementation );
}
//----------------------------------------------------------------------------

void binderoo::Host::setRapidIterationMode( bool bSet )
{
	pImplementation->setRapidIterationMode( bSet );
}
//----------------------------------------------------------------------------

bool binderoo::Host::isInRapidIterationMode( ) const
{
	return pImplementation->isInRapidIterationMode();
}
//----------------------------------------------------------------------------

bool binderoo::Host::checkForReloads()
{
	return pImplementation->checkForReloads();
}
//----------------------------------------------------------------------------

void binderoo::Host::performReloads()
{
	pImplementation->performReloads();
}
//----------------------------------------------------------------------------

void binderoo::Host::registerImportedClassInstance( binderoo::ImportedBase* pInstance )
{
	pImplementation->registerImportedClassInstance( pInstance );
}
//--------------------------------------------------------------------

void binderoo::Host::deregisterImportedClassInstance( binderoo::ImportedBase* pInstance )
{
	pImplementation->deregisterImportedClassInstance( pInstance );
}
//--------------------------------------------------------------------

void binderoo::Host::registerImportedFunction( binderoo::ImportedBase* pInstance )
{
	pImplementation->registerImportedFunction( pInstance );
}
//----------------------------------------------------------------------------

void binderoo::Host::deregisterImportedFunction( binderoo::ImportedBase* pInstance )
{
	pImplementation->deregisterImportedFunction( pInstance );
}
//----------------------------------------------------------------------------

void* binderoo::Host::createImportedClass( const char* pName )
{
	return pImplementation->createImportedClass( pName );
}
//--------------------------------------------------------------------

bool binderoo::Host::destroyImportedClass( const char* pName, void* pObject )
{
	return pImplementation->destroyImportedClass( pName, pObject );
}
//--------------------------------------------------------------------

const binderoo::BoundFunction* binderoo::Host::getImportedFunctionDetails( const char* pName, const char* pSignature ) const
{
	const HostBoundFunction* pFunc = pImplementation->getImportedFunctionDetails( pName, pSignature );

	return pFunc ? pFunc->pObject : nullptr;
}
//--------------------------------------------------------------------

bool binderoo::Host::getImportedFunctionDetails( const char* pName, binderoo::Host::OverloadContainer& allOverloads ) const
{
	return pImplementation->getImportedFunctionDetails( pName, allOverloads );
}
//--------------------------------------------------------------------

const char* binderoo::Host::generateCPPStyleExportDeclarationsForAllObjects( const char* pVersions )
{
	return pImplementation->generateCPPStyleExportDeclarationsForAllObjects( pVersions );
}
//----------------------------------------------------------------------------

const char* binderoo::Host::generateCSharpStyleImportDeclarationsForAllObjects( const char* pVersions )
{
	return pImplementation->generateCSharpStyleImportDeclarationsForAllObjects( pVersions );
}
//----------------------------------------------------------------------------

void binderoo::Host::generateDInterfaceFiles( const char* pOutputFolder )
{
	pImplementation->generateDInterfaceFiles( pOutputFolder );
}
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------

// HostImplementation
//----------------------------------------------------------------------------

binderoo::HostImplementation::HostImplementation( HostConfiguration& config )
	: configuration( config )
	, reloadEvent( "binderoo_service_reload" )
	, bInRapidIterationMode( config.bStartInRapidIterationMode )
	, bReloadLibs( false )
{
	// HACK: Reserving so that we don't have to do a resize...
	vecBoundFunctions.reserve( 8192 );
	vecBoundObjects.reserve( 65536 );
	collectExports();

	performLoad();
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::setRapidIterationMode( bool bSet )
{
	if( bInRapidIterationMode != bSet )
	{
		bReloadLibs = true;
	}
	bInRapidIterationMode = bSet;
}
//----------------------------------------------------------------------------

bool binderoo::HostImplementation::isInRapidIterationMode( ) const
{
	return bInRapidIterationMode;
}
//----------------------------------------------------------------------------

bool binderoo::HostImplementation::checkForReloads()
{
	bReloadLibs |= ( reloadEvent.waitOn( 0 ) && bInRapidIterationMode );
	return bReloadLibs;
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::performReloads()
{
	logInfo( "Reload triggered." );

	saveObjectData();
	destroyImportedObjects();
	performUnload();

	logInfo( "Unload completed. Moving on to reload." );

	performLoad();
	recreateImportedObjects();
	loadObjectData();

	logInfo( "Reload completed." );

	bReloadLibs = false;
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::saveObjectData()
{
	/*binderoo::ScopeLock lock( lockImportClassInstances );
	for( HostImportedObjectInstance& obj : vecImportClassInstances )
	{
		if( obj.pInstance->pObjectInstance )
		{
			const HostBoundObject* pObjDescriptor = (const HostBoundObject*)obj.pInstance->pObjectDescriptor.load();
			const char* pSerialised = pObjDescriptor->pObject->serialise( obj.pInstance->pObjectInstance );
			obj.strReloadData = pSerialised ? pSerialised : "";
			obj.m_bHadInstance = true;
		}
		else
		{
			obj.strReloadData.clear();
			obj.m_bHadInstance = false;
		}
	}*/
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::loadObjectData()
{
	/*binderoo::ScopeLock lock( lockImportClassInstances );
	for( HostImportedObjectInstance& obj : vecImportClassInstances )
	{
		if( obj.m_bHadInstance )
		{
			const HostBoundObject* pObjDescriptor = (const HostBoundObject*)obj.pInstance->pObjectDescriptor.load();
			pObjDescriptor->pObject->deserialise( obj.pInstance->pObjectInstance, obj.strReloadData.c_str() );
			obj.strReloadData.clear();
			obj.m_bHadInstance = false;
		}
	}*/
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::performLoad()
{
	vecDynamicLibs.clear();
	vecBoundFunctions.clear();
	vecBoundObjects.clear();

	collectDynamicLibraries();
	collectBoundFunctions();
	collectBoundObjects();
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::performUnload()
{
	destroyImportedObjects();

	vecBoundObjects.clear();
	vecBoundFunctions.clear();

	for( HostDynamicLib& lib : vecDynamicLibs )
	{
		unloadDynamicLibrary( lib );
	}

	vecDynamicLibs.clear();
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::destroyImportedObjects()
{
	{
		binderoo::ScopeLock lock( lockImportClassInstances );
		for( HostImportedObjectInstance& obj : vecImportClassInstances )
		{
			if( obj.pInstance->pObjectInstance )
			{
				const HostBoundObject* pObjDescriptor = (const HostBoundObject*)obj.pInstance->pObjectDescriptor.load();
				pObjDescriptor->pObject->free( obj.pInstance->pObjectInstance );
				obj.pInstance->pObjectInstance = nullptr;
			}

			obj.pInstance->pObjectDescriptor = nullptr;
		}
	}

	binderoo::ScopeLock lock( lockImportFunctionInstances );
	{
		for( binderoo::ImportedBase*& pImportedFunction : vecImportFunctionInstances )
		{
			pImportedFunction->pObjectDescriptor = nullptr;
			pImportedFunction->pObjectInstance = nullptr;
		}
	}
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::recreateImportedObjects()
{
	{
		binderoo::ScopeLock lock( lockImportFunctionInstances );

		for( binderoo::ImportedBase*& pImportedFunction : vecImportFunctionInstances )
		{
			const HostBoundFunction* pFunction = getImportedFunctionDetails( pImportedFunction->pSymbol, pImportedFunction->pSymbolIdent );

			if( pFunction )
			{
				pImportedFunction->pObjectInstance		= (void*)pFunction->pObject->pFunctionCPPDecl;
				pImportedFunction->pObjectDescriptor	= (void*)pFunction;
			}
		}
	}

	{
		binderoo::ScopeLock lock( lockImportClassInstances );
		for( HostImportedObjectInstance& obj : vecImportClassInstances )
		{
			const HostBoundObject* pObject = getImportedObjectDetails( obj.pInstance->pSymbol );
			if( pObject )
			{
				obj.pInstance->pObjectDescriptor = (void*)pObject;
				if( obj.m_bHadInstance )
				{
					obj.pInstance->pObjectInstance = pObject->pObject->alloc( 1 );
				}
			}
		}
	}
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::collectExports()
{
	ExportedClass* pCurrClass = ExportedClass::getFirstClass();

	while( pCurrClass )
	{
		for( auto& method : pCurrClass->getMethods() )
		{
			uint64_t uSymbolHash = fnv1a_64( method.strName.data(), method.strName.length() );
			uint64_t uSignatureHash = fnv1a_64( method.strSignature.data(), method.strSignature.length() );

			BoundFunction bound;

			bound.strFunctionName							= method.strName;
			bound.strFunctionSignature						= method.strSignature;

			bound.functionHashes.uFunctionNameHash			= uSymbolHash;
			bound.functionHashes.uFunctionSignatureHash		= uSignatureHash;
			bound.pFunctionCPPDecl							= method.pFunctionPointer;
			bound.iMinimumVersion							= pCurrClass->getVersion();
			bound.eResolution								= BoundFunction::Resolution::Exported;

			size_t uIndex = vecExportedFunctions.size();
			vecExportedFunctions.push_back( bound );

			mapExportedFunctionIndices[ bound.functionHashes ] = uIndex;
		}

		pCurrClass = pCurrClass->getNextClass();
	}
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::collectDynamicLibraries()
{
	InternalStringVector vecFoundFiles;

	for( auto& searchPath : configuration.strDynamicLibSearchFolders )
	{
		InternalString strSearchPath( searchPath.data(), searchPath.length() );
		std::replace( strSearchPath.begin(), strSearchPath.end(), '\\', '/' );

		if( strSearchPath.back() != '/' )
		{
			strSearchPath += '/';
		}

		if( bInRapidIterationMode )
		{
			strSearchPath += "/rapid/";
		}

#if BIND_SYSAPI == BIND_SYSAPI_WINAPI || BIND_SYSAPI == BIND_SYSAPI_UWP
		WIN32_FIND_DATA		foundData;
		HANDLE				hFoundHandle = INVALID_HANDLE_VALUE;

		InternalString strSearchPattern = strSearchPath;
		strSearchPattern += "*.dll";

		hFoundHandle = FindFirstFileEx( strSearchPattern.c_str(), FindExInfoStandard, &foundData, FindExSearchNameMatch, nullptr, 0 );

		BOOL bFound = hFoundHandle != INVALID_HANDLE_VALUE ? TRUE : FALSE;
		while( bFound )
		{
			InternalString fileName = InternalString( foundData.cFileName );
			std::replace( fileName.begin(), fileName.end(), '\\', '/' );
			InternalString fullFileName = strSearchPath;
			fullFileName += fileName;

			vecFoundFiles.push_back( fullFileName );

			bFound = FindNextFile( hFoundHandle, &foundData );
		}

		FindClose( hFoundHandle );
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
		DIR* pCurrDir = opendir( strSearchPath.c_str() );

		if( pCurrDir )
		{
			InternalString strExtension( ".so" );

			struct dirent* pCurrEntry = nullptr;

			while( ( pCurrEntry = readdir( pCurrDir ) ) != nullptr )
			{
				InternalString strFilename = InternalString( pCurrEntry->d_name );

				if( strFilename.length() > strExtension.length() && strFilename.substr( strFilename.length() - strExtension.length(), strExtension.length() ) == strExtension )
				{
					InternalString strFullFilename = strSearchPath + strFilename;
					vecFoundFiles.push_back( strFullFilename );
				}
			}

			closedir( pCurrDir );
		}
		else
		{
			InternalString strNotification;
			switch( errno )
			{
			case EACCES:
				strNotification = strSearchPath;
				strNotification += "s: Permission denied.";
				break;
			case EBADF:
				strNotification = "fd is not a valid file descriptor opened for reading.";
				break;
			case EMFILE:
				strNotification = "The per-process limit on the number of open file descriptors has been reached.";
				break;
			case ENFILE:
				strNotification = "The system-wide limit on the total number of open files has been reached.";
				break;
			case ENOENT:
				strNotification = "\"";
				strNotification += strSearchPath;
				strNotification += "\" does not exist, or is an empty string.";
				break;
			case ENOMEM:
				strNotification = "Insufficient memory to complete the directory search operation.";
				break;
			case ENOTDIR:
				strNotification = "\"";
				strNotification += strSearchPath;
				strNotification += "\" is not a directory.";
				break;
			default:
				strNotification = "Unspecified error opening folder for search";
				break;
			}

			logError( strNotification.c_str() );
		}
#endif
	}

	for( InternalString& libPath : vecFoundFiles )
	{
		size_t uSlashIndex = libPath.find_last_of( '/' );
		uSlashIndex = ( uSlashIndex == std::string::npos ? 0 : uSlashIndex );

		size_t uDotIndex = libPath.find_last_of( '.' );
		uDotIndex = ( uDotIndex == std::string::npos ? libPath.length() - 1 : uDotIndex );

		HostDynamicLib dynamicLib;
		dynamicLib.strName = libPath.substr( uSlashIndex + 1, uDotIndex - uSlashIndex - 1 );
		dynamicLib.strPath = libPath;
		dynamicLib.hModule = nullptr;
		dynamicLib.eStatus = DynamicLibStatus::NotLoaded;

		InternalString strNotification = "Library \"";
		strNotification += dynamicLib.strName;

		if( loadDynamicLibrary( dynamicLib ) )
		{
			strNotification += "\" ";
			if( bInRapidIterationMode )
			{
				strNotification += "(rapid iteration mode) ";
			}
			strNotification += "loaded successfully.";
			logInfo( strNotification.c_str() );
			vecDynamicLibs.push_back( dynamicLib );
		}
		else
		{
			strNotification += "\" ";
			if( bInRapidIterationMode )
			{
				strNotification += "(rapid iteration mode) ";
			}
			strNotification += "failed to load. Status: ";
			strNotification += pDynamicLibStatusMessages[ (int)dynamicLib.eStatus ];

			logError( strNotification.c_str() );
		}
	}
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::collectBoundFunctions()
{
	size_t uReserveSize = 0;

	for( auto& lib : vecDynamicLibs )
	{
		binderoo::Slice< binderoo::BoundFunction > exportedFunctions;
		lib.getExportedFunctions( &exportedFunctions );

		uReserveSize += exportedFunctions.length();
	}

	vecBoundFunctions.reserve( uReserveSize );


	for( auto& lib : vecDynamicLibs )
	{
		binderoo::Slice< binderoo::BoundFunction > exportedFunctions;
		lib.getExportedFunctions( &exportedFunctions );

		for( auto& exportedFunc : exportedFunctions )
		{
			HostBoundFunction func;
			func.pLibrary = &lib;
			func.pObject = &exportedFunc;
			func.uSearchHash = exportedFunc.functionHashes;

			vecBoundFunctions.push_back( func );
		}
	}

}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::collectBoundObjects()
{
	size_t uReserveSize = 0;

	for( auto& lib : vecDynamicLibs )
	{
		binderoo::Slice< binderoo::BoundObject > exportedObjects;
		lib.getExportedObjects( &exportedObjects );

		uReserveSize += exportedObjects.length();
	}

	vecBoundObjects.reserve( uReserveSize );

	for( auto& lib : vecDynamicLibs )
	{
		binderoo::Slice< binderoo::BoundObject > exportedObjects;
		lib.getExportedObjects( &exportedObjects );

		for( auto& exportedObject : exportedObjects )
		{
			HostBoundObject obj;
			obj.pLibrary = &lib;
			obj.pObject = &exportedObject;
			obj.uSearchHash = exportedObject.uFullyQualifiedNameHash;

			vecBoundObjects.push_back( obj );
		}
	}
}
//----------------------------------------------------------------------------

bool binderoo::HostImplementation::loadDynamicLibrary( binderoo::HostDynamicLib& lib )
{
#if BINDEROOHOST_RAPIDITERATION
	InternalString strTempPath;
	DWORD dTempPathLength = GetTempPath( 0, nullptr );
	strTempPath.resize( dTempPathLength - 1 );
	GetTempPath( dTempPathLength, (char*)strTempPath.data() );
	std::replace( strTempPath.begin(), strTempPath.end(), '\\', '/' );
	strTempPath += "binderoo/";

	DWORD dPathAttributes = GetFileAttributes( strTempPath.c_str() );
	if( dPathAttributes == INVALID_FILE_ATTRIBUTES || ( dPathAttributes & FILE_ATTRIBUTE_DIRECTORY ) == 0 )
	{
		CreateDirectory( strTempPath.c_str(), nullptr );
	}

	UUID newUUID;
	RPC_STATUS uResult = UuidCreate( &newUUID );
	InternalString strUUID;

	const char* pCurr = (const char*)&newUUID;
	const char* pEnd = pCurr + sizeof( UUID );
	while( pCurr != pEnd )
	{
		char outBuffer[ 8 ] = { 0, 0, 0, 0, 0, 0, 0, 0 };
		snprintf( outBuffer, 8, "%02x", *(unsigned char*)pCurr );
		strUUID += outBuffer;
		++pCurr;
	}

	strTempPath += strUUID + "/";
	dPathAttributes = GetFileAttributes( strTempPath.c_str() );
	if( dPathAttributes == INVALID_FILE_ATTRIBUTES || ( dPathAttributes & FILE_ATTRIBUTE_DIRECTORY ) == 0 )
	{
		CreateDirectory( strTempPath.c_str(), nullptr );
	}

	lib.strScratchPath = strTempPath;
	lib.strScratchLib = strTempPath + lib.strName + ".dll";
	lib.strScratchSymbols = strTempPath + lib.strName + ".pdb";

	BOOL bSuccess = CopyFile( lib.strPath.c_str(), lib.strScratchLib.c_str(), FALSE );

	{
		InternalString strPDBSource = lib.strPath;

		strPDBSource.replace( strPDBSource.find_last_of( "." ), 4, ".pdb" );

		DWORD dPDBAttributes = GetFileAttributes( strPDBSource.c_str() );
		if( dPDBAttributes != INVALID_FILE_ATTRIBUTES )
		{
			CopyFile( strPDBSource.c_str(), lib.strScratchSymbols.c_str(), FALSE );
		}
	}

	SetDllDirectory( strTempPath.c_str() );

#else // !BINDEROOHOST_RAPIDITERATION
	lib.strScratchLib = lib.strPath;
#endif // BINDEROOHOST_RAPIDITERATION

#if BIND_SYSAPI == BIND_SYSAPI_WINAPI || BIND_SYSAPI == BIND_SYSAPI_UWP
	#if BINDEROOHOST_LOADLIBRARYA
		HMODULE hModule = LoadLibraryA( lib.strScratchLib.c_str() );
	#else // Wide char load library
		const size_t BufferSize = 1024;
		wchar_t WideBuffer[ BufferSize ];
		MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, lib.strScratchLib.c_str(), -1, WideBuffer, BufferSize );

		HMODULE hModule = LoadLibraryW( WideBuffer );
	#endif // BINDEROOHOST_LOADLIBRARYA
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
	ModuleHandle hModule = dlopen( lib.strScratchLib.c_str(), RTLD_LAZY );

	if( !hModule )
	{
		logError( dlerror() );
	}
#endif // SYSAPI checks

	if( hModule != nullptr )
	{
		lib.hModule													= hModule;
		lib.eStatus													= DynamicLibStatus::ModuleInMemory;

		BinderooInitDeinitPtr		binderoo_startup				= ( BinderooInitDeinitPtr )GetModuleSymbolAddress( hModule, "binderoo_startup" );
		BinderooInitDeinitPtr		binderoo_shutdown				= ( BinderooInitDeinitPtr )GetModuleSymbolAddress( hModule, "binderoo_shutdown" );
		ImportFunctionsFromPtr		importFunctionsFrom				= ( ImportFunctionsFromPtr )GetModuleSymbolAddress( hModule, "importFunctionsFrom" );
		GetExportedObjectsPtr		getExportedObjects				= ( GetExportedObjectsPtr )GetModuleSymbolAddress( hModule, "getExportedObjects" );
		GetExportedFunctionsPtr		getExportedFunctions			= ( GetExportedFunctionsPtr )GetModuleSymbolAddress( hModule, "getExportedFunctions" );
		CreateObjectByNamePtr		createObjectByName				= ( CreateObjectByNamePtr )GetModuleSymbolAddress( hModule, "createObjectByName" );
		CreateObjectByHashPtr		createObjectByHash				= ( CreateObjectByHashPtr )GetModuleSymbolAddress( hModule, "createObjectByHash" );
		DestroyObjectByNamePtr		destroyObjectByName				= ( DestroyObjectByNamePtr )GetModuleSymbolAddress( hModule, "destroyObjectByName" );
		DestroyObjectByHashPtr		destroyObjectByHash				= ( DestroyObjectByHashPtr )GetModuleSymbolAddress( hModule, "destroyObjectByHash" );
		GenerateDeclarationsForAllObjectsPtr generateCPPStyleExportDeclarationsForAllObjects = ( GenerateDeclarationsForAllObjectsPtr )GetModuleSymbolAddress( hModule, "generateCPPStyleExportDeclarationsForAllObjects" );
		GenerateDeclarationsForAllObjectsPtr generateCSharpStyleImportDeclarationsForAllObjects = ( GenerateDeclarationsForAllObjectsPtr )GetModuleSymbolAddress( hModule, "generateCSharpStyleImportDeclarationsForAllObjects" );
		GenerateDInterfaceFilesPtr	generateDInterfaceFiles			= ( GenerateDInterfaceFilesPtr )GetModuleSymbolAddress( hModule, "generateDInterfaceFiles" );

		if( binderoo_startup && binderoo_shutdown && importFunctionsFrom && getExportedObjects && getExportedFunctions && createObjectByName && createObjectByHash && destroyObjectByName && destroyObjectByHash && generateCPPStyleExportDeclarationsForAllObjects && generateDInterfaceFiles )
		{
			lib.binderoo_startup									= binderoo_startup;
			lib.binderoo_shutdown									= binderoo_shutdown;
			lib.importFunctionsFrom									= importFunctionsFrom;
			lib.getExportedObjects									= getExportedObjects;
			lib.getExportedFunctions								= getExportedFunctions;
			lib.createObjectByName									= createObjectByName;
			lib.createObjectByHash									= createObjectByHash;
			lib.destroyObjectByName									= destroyObjectByName;
			lib.destroyObjectByHash									= destroyObjectByHash;
			lib.generateCPPStyleExportDeclarationsForAllObjects		= generateCPPStyleExportDeclarationsForAllObjects;
			lib.generateCSharpStyleImportDeclarationsForAllObjects	= generateCSharpStyleImportDeclarationsForAllObjects;
			lib.generateDInterfaceFiles								= generateDInterfaceFiles;
			lib.eStatus												= DynamicLibStatus::Ready;

			lib.binderoo_startup();
			lib.importFunctionsFrom( binderoo::Slice< binderoo::BoundFunction >( vecExportedFunctions.data(), vecExportedFunctions.size() ) );

			return true;
		}
		else
		{
			unloadDynamicLibrary( lib );
			lib.eStatus = DynamicLibStatus::CoreInterfaceNotFound;
		}
	}

	return false;
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::unloadDynamicLibrary( binderoo::HostDynamicLib& lib )
{
	if( lib.binderoo_shutdown )
	{
		lib.binderoo_shutdown();
	}

	lib.binderoo_startup									= nullptr;
	lib.binderoo_shutdown									= nullptr;
	lib.importFunctionsFrom									= nullptr;
	lib.getExportedObjects									= nullptr;
	lib.getExportedFunctions								= nullptr;
	lib.createObjectByName									= nullptr;
	lib.createObjectByHash									= nullptr;
	lib.destroyObjectByName									= nullptr;
	lib.destroyObjectByHash									= nullptr;
	lib.generateCPPStyleExportDeclarationsForAllObjects		= nullptr;
	lib.generateCSharpStyleImportDeclarationsForAllObjects	= nullptr;

#if BINDEROOHOST_RAPIDITERATION
	HANDLE hProcess = GetCurrentProcess();

	auto pRelevantFunction = &binderoo::HostImplementation::performUnload;

	DWORD64 dwModuleBase = SymGetModuleBase64( hProcess, *(DWORD64*)&pRelevantFunction );
	if( dwModuleBase != 0 )
	{
		SymUnloadModule64( hProcess, dwModuleBase );
	}
#endif // BINDEROOHOST_RAPIDITERATION

	if( lib.hModule )
	{
#if BIND_SYSAPI == BIND_SYSAPI_WINAPI || BIND_SYSAPI == BIND_SYSAPI_UWP
		FreeLibrary( lib.hModule );
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
		dlclose( lib.hModule );
#endif // SYSAPI check
		lib.hModule = nullptr;
	}

#if BINDEROOHOST_RAPIDITERATION
	InternalString strErrorMessage = "Failed to delete ";
	if( !DeleteFile( lib.strScratchSymbols.c_str() ) )
	{
		logWarning( (strErrorMessage + lib.strScratchSymbols ).c_str() );
	}
	if( !DeleteFile( lib.strScratchLib.c_str() ) )
	{
		logWarning( (strErrorMessage + lib.strScratchSymbols ).c_str() );
	}
	if( !RemoveDirectory( lib.strScratchPath.c_str() ) )
	{
		logWarning( (strErrorMessage + lib.strScratchSymbols ).c_str() );
	}
#endif // BINDEROOHOST_RAPIDITERATION

	lib.eStatus = DynamicLibStatus::Unloaded;
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::registerImportedClassInstance( binderoo::ImportedBase* pInstance )
{
	const HostBoundObject* pObject = getImportedObjectDetails( pInstance->pSymbol );

	if( pObject )
	{
		pInstance->pObjectDescriptor = (void*)pObject;
	}

	HostImportedObjectInstance objInstance;
	objInstance.pInstance = pInstance;
	//objInstance.strClass = pInstance->pSymbol;

	binderoo::ScopeLock lock( lockImportClassInstances );
	vecImportClassInstances.push_back( objInstance );
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::deregisterImportedClassInstance( binderoo::ImportedBase* pInstance )
{
	binderoo::ScopeLock lock( lockImportClassInstances );

	// VS2012 was having issues with std::find and a lambda, so the comparison is in the type now...
	auto found = std::find( vecImportClassInstances.begin(), vecImportClassInstances.end(), pInstance );

	if( found != vecImportClassInstances.end() )
	{
		found->pInstance->pObjectDescriptor = nullptr;

		vecImportClassInstances.erase( found );
	}
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::registerImportedFunction( binderoo::ImportedBase* pInstance )
{
	const HostBoundFunction* pFunction = getImportedFunctionDetails( pInstance->pSymbol, pInstance->pSymbolIdent );

	if( pFunction )
	{
		pInstance->pObjectInstance		= (void*)pFunction->pObject->pFunctionCPPDecl;
		pInstance->pObjectDescriptor	= (void*)pFunction;
	}

	binderoo::ScopeLock lock( lockImportFunctionInstances );
	vecImportFunctionInstances.push_back( pInstance );
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::deregisterImportedFunction( binderoo::ImportedBase* pInstance )
{
	binderoo::ScopeLock lock( lockImportClassInstances );

	auto found = std::find( vecImportFunctionInstances.begin(), vecImportFunctionInstances.end(), pInstance );
	if( found != vecImportFunctionInstances.end() )
	{
		vecImportFunctionInstances.erase( found );

		pInstance->pObjectDescriptor = nullptr;
		pInstance->pObjectInstance = nullptr;
	}
}
//----------------------------------------------------------------------------

void* binderoo::HostImplementation::createImportedClass( const char* pName )
{
	uint64_t uNameHash = fnv1a_64( pName, strlen( pName ) );

	for( auto& lib : vecDynamicLibs )
	{
		binderoo::Slice< binderoo::BoundObject > exportedObjects;
		lib.getExportedObjects( &exportedObjects );

		for( auto& exportedObject : exportedObjects )
		{
			if( exportedObject.uFullyQualifiedNameHash == uNameHash )
			{
				return lib.createObjectByHash( uNameHash );
			}
		}
	}
	
	return nullptr;
}
//----------------------------------------------------------------------------

bool binderoo::HostImplementation::destroyImportedClass( const char* pName, void* pObject )
{
	uint64_t uNameHash = fnv1a_64( pName, strlen( pName ) );

	for( auto& lib : vecDynamicLibs )
	{
		binderoo::Slice< binderoo::BoundObject > exportedObjects;
		lib.getExportedObjects( &exportedObjects );

		for( auto& exportedObject : exportedObjects )
		{
			if( exportedObject.uFullyQualifiedNameHash == uNameHash )
			{
				lib.destroyObjectByHash( uNameHash, pObject );
				return true;
			}
		}
	}

	return false;
}
//----------------------------------------------------------------------------

const binderoo::HostBoundFunction* binderoo::HostImplementation::getImportedFunctionDetails( const char* pName, const char* pSignature ) const
{
	BoundFunction::Hashes uNameHash = { fnv1a_64( pName, strlen( pName ) ), fnv1a_64( pSignature, strlen( pSignature ) ) };

	// VS2012 was having issues with std::find and a lambda, so the comparison is in the type now...
	auto found = std::find( vecBoundFunctions.begin(), vecBoundFunctions.end(), uNameHash );

	if( found != vecBoundFunctions.end() )
	{
		// The return of my most hated operation - dereference the iterator to get the address of the object...
		return &*found;
	}

	return nullptr;
}
//----------------------------------------------------------------------------

bool binderoo::HostImplementation::getImportedFunctionDetails( const char* pName, binderoo::Host::OverloadContainer& allOverloads ) const
{
	uint64_t uNameHash = fnv1a_64( pName, strlen( pName ) );

	for( const binderoo::HostBoundFunction& func : vecBoundFunctions )
	{
		if( func.uSearchHash.uFunctionNameHash == uNameHash )
		{
			allOverloads.push_back( func.pObject );
		}
	}

	return !allOverloads.empty();
}
//----------------------------------------------------------------------------

const binderoo::HostBoundObject* binderoo::HostImplementation::getImportedObjectDetails( const char* pName ) const
{
	uint64_t uNameHash = fnv1a_64( pName, strlen( pName ) );

	// VS2012 was having issues with std::find and a lambda, so the comparison is in the type now...
	auto found = std::find( vecBoundObjects.begin(), vecBoundObjects.end(), uNameHash );

	if( found != vecBoundObjects.end() )
	{
		// The return of my most hated operation - dereference the iterator to get the address of the object...
		return &*found;
	}

	return nullptr;
}
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------

const char* binderoo::HostImplementation::generateCPPStyleExportDeclarationsForAllObjects( const char* pVersions )
{
	InternalStringVector vecAllDeclarations;

/*	InternalStringVector vecVersions;

	const char* pVersionCurr = pVersions;
	const char* pVersionStart = pVersions;

	while( pVersionCurr )
	{
		if( *pVersionCurr )
		{
			if( *pVersionCurr == ';' )
			{
				vecVersions.push_back( InternalString( pVersionStart, (size_t)( pVersionCurr - pVersionStart ) ) );
				pVersionStart = ++pVersionCurr;
			}
			else
			{
				++pVersionCurr;
			}
		}
		else
		{
			if( pVersionCurr != pVersionStart )
			{
				vecVersions.push_back( InternalString( pVersionStart, (size_t)( pVersionCurr - pVersionStart ) ) );
			}
			pVersionCurr = nullptr;
		}
	}*/

	const char* const pSeparator = "\n\n";
	const size_t uSeparatorLength = 2;

	size_t uRequiredSpace = 0;
	char* pOutput = nullptr;

	for( auto& lib : vecDynamicLibs )
	{
		const char* pDeclarations = lib.generateCPPStyleExportDeclarationsForAllObjects( configuration.unaligned_alloc, pVersions );

		InternalString strDeclarations( pDeclarations, strlen( pDeclarations ) );
		vecAllDeclarations.push_back( strDeclarations );
		uRequiredSpace += strDeclarations.size();

		configuration.unaligned_free( (void*)pDeclarations );
	}

	if( !vecAllDeclarations.empty() )
	{
		uRequiredSpace += ( vecAllDeclarations.size() - 1 ) * uSeparatorLength + 1;

		pOutput = (char*)configuration.unaligned_alloc( uRequiredSpace );

		char* pDest = pOutput;

		for( auto& declarations : vecAllDeclarations )
		{
			if( pDest != pOutput )
			{
				memcpy( pDest, pSeparator, uSeparatorLength );
				pDest += uSeparatorLength;
			}

			memcpy( pDest, declarations.c_str(), declarations.size() );
			pDest += declarations.size();
		}

		*pDest = 0;
	}

	return pOutput;

}
//----------------------------------------------------------------------------

const char* binderoo::HostImplementation::generateCSharpStyleImportDeclarationsForAllObjects( const char* pVersions )
{
	InternalStringVector vecAllDeclarations;

/*	InternalStringVector vecVersions;

	const char* pVersionCurr = pVersions;
	const char* pVersionStart = pVersions;

	while( pVersionCurr )
	{
		if( *pVersionCurr )
		{
			if( *pVersionCurr == ';' )
			{
				vecVersions.push_back( InternalString( pVersionStart, (size_t)( pVersionCurr - pVersionStart ) ) );
				pVersionStart = ++pVersionCurr;
			}
			else
			{
				++pVersionCurr;
			}
		}
		else
		{
			if( pVersionCurr != pVersionStart )
			{
				vecVersions.push_back( InternalString( pVersionStart, (size_t)( pVersionCurr - pVersionStart ) ) );
			}
			pVersionCurr = nullptr;
		}
	}*/

	const char* const pSeparator = "\n\n";
	const size_t uSeparatorLength = 2;

	size_t uRequiredSpace = 0;
	char* pOutput = nullptr;

	for( auto& lib : vecDynamicLibs )
	{
		const char* pDeclarations = lib.generateCSharpStyleImportDeclarationsForAllObjects( configuration.unaligned_alloc, pVersions );

		InternalString strDeclarations( pDeclarations, strlen( pDeclarations ) );
		vecAllDeclarations.push_back( strDeclarations );
		uRequiredSpace += strDeclarations.size();

		configuration.unaligned_free( (void*)pDeclarations );
	}

	if( !vecAllDeclarations.empty() )
	{
		uRequiredSpace += ( vecAllDeclarations.size() - 1 ) * uSeparatorLength + 1;

		pOutput = (char*)configuration.unaligned_alloc( uRequiredSpace );

		char* pDest = pOutput;

		for( auto& declarations : vecAllDeclarations )
		{
			if( pDest != pOutput )
			{
				memcpy( pDest, pSeparator, uSeparatorLength );
				pDest += uSeparatorLength;
			}

			memcpy( pDest, declarations.c_str(), declarations.size() );
			pDest += declarations.size();
		}

		*pDest = 0;
	}

	return pOutput;

}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::generateDInterfaceFiles( const char* pOutputFolder )
{
	for( auto& lib : vecDynamicLibs )
	{
		lib.generateDInterfaceFiles( pOutputFolder );
	}
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::logInfo( const char* pMessage )
{
	if( configuration.log_info )
	{
		configuration.log_info( pMessage );
	}
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::logWarning( const char* pMessage )
{
	if( configuration.log_warning )
	{
		configuration.log_warning( pMessage );
	}
}
//----------------------------------------------------------------------------

void binderoo::HostImplementation::logError( const char* pMessage )
{
	if( configuration.log_error )
	{
		configuration.log_error( pMessage );
	}
}
//----------------------------------------------------------------------------

static void* BIND_C_CALL host_c_unaligned_malloc( size_t objSize )
{
	return malloc( objSize );
}
//----------------------------------------------------------------------------

static void BIND_C_CALL host_c_unaligned_free( void* pObj )
{
	free( pObj );
}
//----------------------------------------------------------------------------

static void* BIND_C_CALL host_c_malloc( size_t objSize, size_t alignment )
{
#if BIND_SYSAPI == BIND_SYSAPI_WINDOWS
	return _aligned_malloc( objSize, alignment );
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
	return aligned_alloc( alignment, objSize );
#endif
}
//----------------------------------------------------------------------------

static void BIND_C_CALL host_c_free( void* pObj )
{
#if BIND_SYSAPI == BIND_SYSAPI_WINDOWS
	_aligned_free( pObj );
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
	free( pObj );
#endif
}
//----------------------------------------------------------------------------

static void* BIND_C_CALL host_c_calloc( size_t objCount, size_t objSize, size_t alignment )
{
#if BIND_SYSAPI == BIND_SYSAPI_WINDOWS
	return _aligned_malloc( objCount * objSize, alignment );
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
	return aligned_alloc( alignment, objCount * objSize );
#endif
}
//----------------------------------------------------------------------------

static void* BIND_C_CALL host_c_realloc( void* pObj, size_t newObjSize, size_t alignment )
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

static void BIND_C_CALL host_c_log_info( const char* pMessage )
{
	fprintf( stdout, "%s\n", pMessage );
#if BIND_SYSAPI == BIND_SYSAPI_WINAPI || BIND_SYSAPI == BIND_SYSAPI_UWP
	OutputDebugString( pMessage );
#endif 
}
//----------------------------------------------------------------------------

static void BIND_C_CALL host_c_log_warning( const char* pMessage )
{
	fprintf( stdout, TERMINAL_BOLDON TERMINAL_FOREGROUND256( 220 ) "WARNING:" TERMINAL_RESET " %s\n", pMessage );
#if BIND_SYSAPI == BIND_SYSAPI_WINAPI || BIND_SYSAPI == BIND_SYSAPI_UWP
	OutputDebugString( pMessage );
#endif 
}
//----------------------------------------------------------------------------

static void BIND_C_CALL host_c_log_error( const char* pMessage )
{
	fprintf( stderr, TERMINAL_BOLDON TERMINAL_FOREGROUND256( 15 ) TERMINAL_BACKGROUND256( 160 ) "ERROR:" TERMINAL_RESET " %s\n", pMessage );
#if BIND_SYSAPI == BIND_SYSAPI_WINAPI || BIND_SYSAPI == BIND_SYSAPI_UWP
	OutputDebugString( pMessage );
#endif 
}
//----------------------------------------------------------------------------

BIND_C_API_BEGIN

typedef std::vector< std::string > StringVector;
typedef std::vector< binderoo::DString > DStringVector;
//----------------------------------------------------------------------------

struct HostData_C
{
	StringVector					m_vecPathsNative;
	DStringVector					m_vecPaths;
	binderoo::HostConfiguration		m_config;
	binderoo::Host*					m_pHost;
};
//----------------------------------------------------------------------------

binderoo_host_t binderoo_host_create( char** pSearchPaths, int iNumSearchPaths, bool bStartInRapidIteration )
{
	HostData_C* pHostData = new HostData_C;

	pHostData->m_vecPathsNative.resize( iNumSearchPaths );
	pHostData->m_vecPaths.resize( iNumSearchPaths );

	for( int iThisPath = 0; iThisPath < iNumSearchPaths; ++iThisPath )
	{
		pHostData->m_vecPathsNative[ iThisPath ] = std::string( pSearchPaths[ iThisPath ] );
		pHostData->m_vecPaths[ iThisPath ] = binderoo::DString( pHostData->m_vecPathsNative[ iThisPath ].data(), pHostData->m_vecPathsNative[ iThisPath ].length() );
	}

	pHostData->m_config.strDynamicLibSearchFolders = binderoo::Slice< binderoo::DString >( pHostData->m_vecPaths.data(), pHostData->m_vecPaths.size() );
	pHostData->m_config.alloc = &host_c_malloc;
	pHostData->m_config.free = &host_c_free;
	pHostData->m_config.calloc = &host_c_calloc;
	pHostData->m_config.realloc = &host_c_realloc;
	pHostData->m_config.unaligned_alloc = &host_c_unaligned_malloc;
	pHostData->m_config.unaligned_free = &host_c_unaligned_free;
	pHostData->m_config.log_info = &host_c_log_info;
	pHostData->m_config.log_warning = &host_c_log_warning;
	pHostData->m_config.log_error = &host_c_log_error;
	pHostData->m_config.bStartInRapidIterationMode = bStartInRapidIteration;

	pHostData->m_pHost = new binderoo::Host( pHostData->m_config );

	return pHostData;
}
//----------------------------------------------------------------------------

void binderoo_host_destroy( binderoo_host_t* pHost )
{
	HostData_C* pHostData = (HostData_C*)pHost;
	delete pHostData->m_pHost;
	delete pHostData;

}
//----------------------------------------------------------------------------

BIND_C_API_END
//----------------------------------------------------------------------------

//============================================================================
