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
//----------------------------------------------------------------------------

namespace binderoo
{
	template< >
	class ImportedFunction< void > : public ImportedBase
	{
	public:
		BIND_INLINE ImportedFunction( const char* pFunctionName )
			: ImportedBase( pFunctionName )
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
	binderoo::ImportedFunction< void >*					m_pFunc;
	size_t												m_uRefCount;
};
//----------------------------------------------------------------------------

struct Host_Class_C
{
	InternalString										m_strName;
	binderoo::RefCountedImportedClassInstance< void > 	m_pObj;
};
//----------------------------------------------------------------------------

typedef std::map< InternalString, Host_Function_C*, std::less< InternalString > > FunctionMap;
static FunctionMap s_functions;
//----------------------------------------------------------------------------

binderoo_imported_function_t binderoo_host_create_imported_function( char* pName, char* pSignature )
{
	InternalString strName( pName );

	auto found = s_functions.find( strName );
	if( found != s_functions.end() )
	{
		++found->second->m_uRefCount;
		return found->second->m_pFunc;
	}

	Host_Function_C* pFunc = new Host_Function_C { strName, nullptr, 1 };
	pFunc->m_pFunc = new binderoo::ImportedFunction< void >( pFunc->m_strName.c_str() );
	s_functions[ strName ] = pFunc;
	return pFunc;
}
//----------------------------------------------------------------------------

void binderoo_host_destroy_imported_function( binderoo_imported_function_t pFunc )
{
	Host_Function_C* pThisFunc = (Host_Function_C*)pFunc;

	auto found = s_functions.find( pThisFunc->m_strName );
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

binderoo_imported_class_t binderoo_host_create_imported_class( char* pName )
{
	InternalString strName( pName );

	Host_Class_C* pNewClass = new Host_Class_C { pName };
	pNewClass->m_pObj.createNewInstance( strName.c_str() );
	return pNewClass;
}
//----------------------------------------------------------------------------

void binderoo_host_addref_imported_class( binderoo_imported_class_t pClass )
{
	((Host_Class_C*)pClass)->m_pObj.addReferenceInstance();
}
//----------------------------------------------------------------------------

void binderoo_host_release_imported_class( binderoo_imported_class_t pClass )
{
	if( ((Host_Class_C*)pClass)->m_pObj.releaseInstance() == 0 )
	{
		delete (Host_Class_C*)pClass;
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
