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

#include "binderoo/imports.h"

#include "binderoo/allocator.h"
#include "binderoo/containers.h"

#include <map>
#include <atomic>
//----------------------------------------------------------------------------

namespace binderoo
{
	template< >
	class ImportedFunction< void > : public ImportedBase
	{
	public:
		BIND_INLINE ImportedFunction( const char* pFunctionName, const char* pFunctionSignature )
			: ImportedBase( pFunctionName, pFunctionSignature )
		{
			Host::getActiveHost()->registerImportedFunction( this );
		}
		//--------------------------------------------------------------------

		BIND_INLINE ~ImportedFunction()
		{
			Host::getActiveHost()->deregisterImportedFunction( this );
		}
		//--------------------------------------------------------------------

		BIND_INLINE void* getFuncPtr()
		{
			return pObjectInstance;
		}
		//--------------------------------------------------------------------
	};
	//------------------------------------------------------------------------
}
//----------------------------------------------------------------------------

BIND_C_API_BEGIN

typedef binderoo::Containers< binderoo::AllocatorSpace::Host >::InternalString InternalString;

struct Host_Function_C
{
	InternalString										m_strName;
	InternalString										m_strSignature;
	InternalString										m_strLookupName;
	binderoo::ImportedFunction< void >*					m_pFunc;
	std::atomic< ptrdiff_t >							m_uRefCount;
};
//----------------------------------------------------------------------------

struct Host_Class_C
{
	InternalString										m_strName;
	binderoo::RefCountedImportedClassInstance< void > 	m_pObj;
	bool												m_bRegistered;
	std::atomic< ptrdiff_t >							m_iOwnRefCount;
};
//----------------------------------------------------------------------------

typedef std::map< InternalString, Host_Function_C*, std::less< InternalString > > FunctionMap;
static FunctionMap s_functions;
static std::atomic< bool > s_rawFunctionsLock;

typedef std::map< void*, Host_Class_C* > RawObjectMap;
typedef std::vector< Host_Class_C* > ErasedObjectsList;
static RawObjectMap s_rawObjects;
static ErasedObjectsList s_erasedObjects;
static std::atomic< bool > s_rawObjectsLock;
//----------------------------------------------------------------------------

binderoo_imported_function_t binderoo_host_create_imported_function( const char* pName, const char* pSignature )
{
	InternalString strName( pName );
	InternalString strSignature( pSignature );
	InternalString strLookupName = strName;
	strLookupName += "_";
	strLookupName += strSignature;

	binderoo::ScopeLock lock( s_rawFunctionsLock );

	auto found = s_functions.find( strLookupName );
	if( found != s_functions.end() )
	{
		++found->second->m_uRefCount;
		return found->second->m_pFunc;
	}

	Host_Function_C* pFunc = new Host_Function_C { strName, strSignature, strLookupName, nullptr, 1 };
	pFunc->m_pFunc = new binderoo::ImportedFunction< void >( pFunc->m_strName.c_str(), pFunc->m_strSignature.c_str() );
	s_functions[ strLookupName ] = pFunc;
	return pFunc;
}
//----------------------------------------------------------------------------

void binderoo_host_destroy_imported_function( binderoo_imported_function_t pFunc )
{
	Host_Function_C* pThisFunc = (Host_Function_C*)pFunc;

	binderoo::ScopeLock lock( s_rawFunctionsLock );

	auto found = s_functions.find( pThisFunc->m_strLookupName );
	if( --found->second->m_uRefCount == 0 )
	{
		s_functions.erase( found );
		delete found->second->m_pFunc;
		delete found->second;
	}
}
//----------------------------------------------------------------------------

binderoo_func_ptr_t binderoo_host_get_function_ptr( binderoo_imported_function_t pFunc )
{
	Host_Function_C* pThisFunc = (Host_Function_C*)pFunc;

	return pThisFunc->m_pFunc->getFuncPtr();
}
//----------------------------------------------------------------------------

binderoo_imported_class_t binderoo_host_create_imported_class( const char* pName )
{
	Host_Class_C* pNewClass = new Host_Class_C { pName, binderoo::RefCountedImportedClassInstance< void >(), false, 1 };

	InternalString strName( pName );

	pNewClass->m_pObj.createNewInstance( strName.c_str() );

	{
		binderoo::ScopeLock lock( s_rawObjectsLock );
		s_rawObjects.insert( RawObjectMap::value_type( pNewClass->m_pObj.get(), pNewClass ) );
	}

	return pNewClass;
}
//----------------------------------------------------------------------------

binderoo_imported_class_t binderoo_host_register_imported_class( void* pObj, const char* pClassName )
{
	binderoo::ScopeLock lock( s_rawObjectsLock );

	auto found = s_rawObjects.find( pObj );
	if( found == s_rawObjects.end() )
	{
		Host_Class_C* pNewClass = new Host_Class_C { pClassName, binderoo::RefCountedImportedClassInstance< void >(), true, 1 };
		pNewClass->m_pObj.registerRawInstance( pObj, pNewClass->m_strName.c_str() );

		s_rawObjects.insert( RawObjectMap::value_type( pObj, pNewClass ) );

		return pNewClass;
	}
	else
	{
		found->second->m_iOwnRefCount++;
		found->second->m_pObj.addReferenceInstance();
		return found->second;
	}
}
//----------------------------------------------------------------------------

void binderoo_host_addref_imported_class( binderoo_imported_class_t pClass )
{
	binderoo::ScopeLock lock( s_rawObjectsLock );

	Host_Class_C* cClass = (Host_Class_C*)pClass;
	{
		cClass->m_pObj.addReferenceInstance();
	}

	cClass->m_iOwnRefCount++;
}
//----------------------------------------------------------------------------

void binderoo_host_release_imported_class( binderoo_imported_class_t pClass )
{
	binderoo::ScopeLock lock( s_rawObjectsLock );

	Host_Class_C* cClass = (Host_Class_C*)pClass;

	if( cClass->m_iOwnRefCount.load() <= 0 )
	{
		int foo = 0;
	}

	cClass->m_iOwnRefCount--;
	void* pObjPtr = ((Host_Class_C*)pClass)->m_pObj.get();

	if( ((Host_Class_C*)pClass)->m_pObj.releaseInstance() == 0 )
	{
		auto found = s_rawObjects.find( pObjPtr );
		if( found != s_rawObjects.end() )
		{
			s_rawObjects.erase( found );
		}
		
		s_erasedObjects.push_back( cClass );
		//delete (Host_Class_C*)pClass;
	}
}
//----------------------------------------------------------------------------

binderoo_class_ptr_t binderoo_host_get_class_ptr( binderoo_imported_class_t pClass )
{
	return ((Host_Class_C*)pClass)->m_pObj.get();
}
//----------------------------------------------------------------------------

BIND_C_API_END
//----------------------------------------------------------------------------

//============================================================================
