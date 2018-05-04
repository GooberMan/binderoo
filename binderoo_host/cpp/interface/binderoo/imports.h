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

#if !defined( _BINDEROO_IMPORTS_H_ )
#define _BINDEROO_IMPORTS_H_
//----------------------------------------------------------------------------

#if !defined( BIND_HOST_C_API_ONLY )

#include "binderoo/defs.h"

#include "binderoo/allocator.h"
#include "binderoo/functiontraits.h"
#include "binderoo/host.h"
//----------------------------------------------------------------------------

namespace binderoo
{
	template< typename _ty >
	class RefCountedImportedClassInstance;

	class ImportedBase
	{
	public:
		BIND_INLINE ImportedBase( const char* pSymbolName )
			: pObjectInstance( nullptr )
			, pObjectDescriptor( nullptr )
			, pSymbol( pSymbolName )
			, iRefCount( 0 )
		{
		}
		//--------------------------------------------------------------------

		friend class binderoo::Host;
		friend class binderoo::HostImplementation;

	protected:
		BIND_INLINE ImportedBase( void* pInstance, void* pDescriptor, const char* pClassTypeName, std::ptrdiff_t refcount )
			: pObjectInstance( pInstance )
			, pObjectDescriptor( pDescriptor )
			, pSymbol( pClassTypeName )
			, iRefCount( refcount )
		{
		}

		template< typename _ty >
		friend class RefCountedImportedClassInstance;

		void*						pObjectInstance;
		void*						pObjectDescriptor;
		const char*					pSymbol;
		std::ptrdiff_t				iRefCount;
	};
	//------------------------------------------------------------------------

	template< typename _ty >
	class ImportedClassInstance : public ImportedBase
	{
	public:
		typedef _ty ThisType;
		typedef TypeNames< ThisType > NameProvider;

		BIND_INLINE ImportedClassInstance( const char* pClassName = NameProvider::getDName(), bool bInstantiate = false )
			: ImportedBase( pClassName )
		{
			Host::getActiveHost()->registerImportedClassInstance( this );

			if( bInstantiate )
			{
				instantiate();
			}
		}
		//--------------------------------------------------------------------

		// ImportedClassInstance currently does not allow you to duplicate another instance. But that will be coming soon...
		ImportedClassInstance( ImportedClassInstance& otherInst ) = delete;
		//--------------------------------------------------------------------

		BIND_INLINE ImportedClassInstance( ImportedClassInstance&& otherInst )
			: ImportedBase( otherInst.pObjectInstance, otherInst.pObjectDescriptor, otherInst.pSymbol, otherInst.iRefCount )
		{
			otherInst.pObjectInstance = nullptr;
			otherInst.pObjectDescriptor = nullptr;
			otherInst.iRefCount = 0;

			Host::getActiveHost()->registerImportedClassInstance( this );
		}
		//--------------------------------------------------------------------

		// ImportedClassInstance currently does not allow you to duplicate another instance. But that will be coming soon...
		ImportedClassInstance& operator=( ImportedClassInstance& otherInst ) = delete;
		//--------------------------------------------------------------------

		BIND_INLINE ImportedClassInstance& operator=( ImportedClassInstance&& otherInst )
		{
			deinstantiate();

			pObjectInstance = otherInst.pObjectInstance;
			pObjectDescriptor = otherInst.pObjectDescriptor;
			pSymbol = otherInst.pSymbol;
			iRefCount = otherInst.iRefCount;

			otherInst.pObjectInstance = nullptr;
			otherInst.pObjectDescriptor = nullptr;
			otherInst.iRefCount = 0;

			return *this;
		}
		//--------------------------------------------------------------------

		BIND_INLINE ~ImportedClassInstance()
		{
			deinstantiate();

			Host::getActiveHost()->deregisterImportedClassInstance( this );
		}
		//--------------------------------------------------------------------

		BIND_INLINE _ty* const			operator->()							{ return getInternal(); }
		BIND_INLINE const _ty* const	operator->() const						{ return getInternal(); }
		BIND_INLINE						operator _ty* const ()					{ return getInternal(); }
		BIND_INLINE						operator const _ty* const () const		{ return getInternal(); }
		BIND_INLINE	_ty* const			get()									{ return getInternal(); }
		BIND_INLINE	const _ty* const	get() const								{ return getInternal(); }
		//--------------------------------------------------------------------

		BIND_INLINE bool			isInstantiated() const						{ return getInternal() != nullptr; }
		//--------------------------------------------------------------------

		BIND_INLINE void			instantiate()
		{
			pObjectInstance = Host::getActiveHost()->createImportedClass( pSymbol );
		}
		//--------------------------------------------------------------------

		BIND_INLINE void			deinstantiate()
		{
			if( pObjectInstance != nullptr )
			{
				Host::getActiveHost()->destroyImportedClass( pSymbol, pObjectInstance );
				pObjectInstance = nullptr;
			}
		}
		//--------------------------------------------------------------------

	private:
		BIND_INLINE _ty* const		getInternal()
		{
			return (_ty* const)pObjectInstance;
		}
		//--------------------------------------------------------------------
	};
	//------------------------------------------------------------------------

	template< typename _ty >
	class RefCountedImportedClassInstance
	{
		typedef ImportedClassInstance< _ty >						ImportedClass;

	public:
		BIND_INLINE RefCountedImportedClassInstance()
			: pRefCountedInstance( nullptr )
		{
		}
		//--------------------------------------------------------------------

		BIND_INLINE RefCountedImportedClassInstance( const RefCountedImportedClassInstance& lval )
			: pRefCountedInstance( lval.pRefCountedInstance )
		{
			if( isAcquired() )
			{
				++pRefCountedInstance->iRefCount;
			}
		}
		//--------------------------------------------------------------------

		BIND_INLINE RefCountedImportedClassInstance( RefCountedImportedClassInstance&& rval )
			: pRefCountedInstance( rval.pRefCountedInstance )
		{
			rval.pRefCountedInstance = nullptr;
		}
		//--------------------------------------------------------------------

		BIND_INLINE ~RefCountedImportedClassInstance()
		{
			unacquire();
		}
		//--------------------------------------------------------------------

		BIND_INLINE RefCountedImportedClassInstance& operator=( const RefCountedImportedClassInstance& lval )
		{
			unacquire();

			pRefCountedInstance = lval.pRefCountedInstance;
			if( isAcquired() )
			{
				++pRefCountedInstance->iRefCount;
			}

			return *this;
		}
		//--------------------------------------------------------------------

		BIND_INLINE RefCountedImportedClassInstance& operator=( RefCountedImportedClassInstance&& rval )
		{
			unacquire();

			pRefCountedInstance = rval.pRefCountedInstance;
			rval.pRefCountedInstance = nullptr;
			return *this;
		}
		//--------------------------------------------------------------------

		BIND_INLINE void				createNewInstance( const char* pClassName = ImportedClass::NameProvider::getDName() )
		{
			unacquire();
			create( pClassName );
		}
		//--------------------------------------------------------------------

		BIND_INLINE std::ptrdiff_t		addReferenceInstance()
		{
			if( isAcquired() )
			{
				return ++pRefCountedInstance->iRefCount;
			}
			
			return 0;
		}
		//--------------------------------------------------------------------

		BIND_INLINE std::ptrdiff_t		releaseInstance()
		{
			unacquire();

			return getRefCount();
		}
		//--------------------------------------------------------------------

		BIND_INLINE bool				isAcquired() const						{ return pRefCountedInstance != nullptr; }
		BIND_INLINE std::ptrdiff_t		getRefCount() const						{ return isAcquired() ? pRefCountedInstance->iRefCount : 0; }
		BIND_INLINE _ty* const			operator->()							{ return isAcquired() ? pRefCountedInstance->get() : nullptr; }
		BIND_INLINE const _ty* const	operator->() const						{ return isAcquired() ? pRefCountedInstance->get() : nullptr; }
		BIND_INLINE						operator _ty* const ()					{ return isAcquired() ? pRefCountedInstance->get() : nullptr; }
		BIND_INLINE						operator const _ty* const () const		{ return isAcquired() ? pRefCountedInstance->get() : nullptr; }
		BIND_INLINE	_ty* const			get()									{ return isAcquired() ? pRefCountedInstance->get() : nullptr; }
		BIND_INLINE	const _ty* const	get() const								{ return isAcquired() ? pRefCountedInstance->get() : nullptr; }
		//--------------------------------------------------------------------

	private:
		BIND_INLINE void unacquire()
		{
			if( isAcquired() && !--pRefCountedInstance->iRefCount )
			{
				destroy();
			}
			pRefCountedInstance = nullptr;
		}
		//--------------------------------------------------------------------

		BIND_INLINE void create( const char* pClassName )
		{
			pRefCountedInstance = AllocatorFunctions< AllocatorSpace::Host >::allocAndConstruct< ImportedClass >( pClassName, true );
			if( pRefCountedInstance )
			{
				++pRefCountedInstance->iRefCount;
			}
		}
		//--------------------------------------------------------------------

		BIND_INLINE void destroy( )
		{
			AllocatorFunctions< AllocatorSpace::Host >::destructAndFree( pRefCountedInstance );
		}
		//--------------------------------------------------------------------

		ImportedClass*				pRefCountedInstance;
	};
	//------------------------------------------------------------------------

#if BIND_STANDARD != BIND_STANDARD_MSVC2012
	template< typename _functiontraits >
	class ImportedFunction : public ImportedBase
	{
	public:
		typedef _functiontraits ThisType;

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

		template< typename... Args >
		BIND_INLINE typename ThisType::return_type operator() ( Args... args )
		{
			return getInternal()( args... );
		}
		//--------------------------------------------------------------------

		BIND_INLINE const char* getSignature() const
		{
			return getDescriptor()->strFunctionSignature.data();
		}
		//--------------------------------------------------------------------

		BIND_INLINE uint64_t getSignatureHash() const
		{
			return getDescriptor()->functionHashes.uFunctionSignatureHash;
		}
		//--------------------------------------------------------------------

		BIND_INLINE bool valid() const
		{
			return pObjectInstance != nullptr;
		}
		//--------------------------------------------------------------------

	private:
		BIND_INLINE typename ThisType::signature getInternal()
		{
			return (typename ThisType::signature)pObjectInstance;
		}
		//--------------------------------------------------------------------

		BIND_INLINE const BoundFunction*				getDescriptor() const
		{
			return (const BoundFunction*)pObjectDescriptor;
		}
		//--------------------------------------------------------------------
	};
	//------------------------------------------------------------------------

#else // BIND_STANDARD == BIND_STANDARD_MSVC2012
	template< typename _functiontraits >
	class ImportedFunction : public ImportedBase
	{
	public:
		typedef _functiontraits ThisType;

		BIND_INLINE ImportedFunction( const char* pFunctionName )
			: ImportedBase( pFunctionName )
		{
			Host::getActiveHost()->registerImportedFunction( this );
		}

		BIND_INLINE ~ImportedFunction()
		{
			Host::getActiveHost()->deregisterImportedFunction( this );
		}

		// This is all rubbish, but they won't try to compile unless you invoke them. So there's that.
		BIND_INLINE typename ThisType::return_type operator() ( )
		{
			return getInternal()( );
		}

		template< typename Arg0 >
		BIND_INLINE typename ThisType::return_type operator() ( Arg0 p0 )
		{
			return getInternal()( p0 );
		}

		template< typename Arg0, typename Arg1 >
		BIND_INLINE typename ThisType::return_type operator() ( Arg0 p0, Arg1 p1 )
		{
			return getInternal()( p0, p1 );
		}

		template< typename Arg0, typename Arg1, typename Arg2 >
		BIND_INLINE typename ThisType::return_type operator() ( Arg0 p0, Arg1 p1, Arg2 p2 )
		{
			return getInternal()( p0, p1, p2 );
		}

		template< typename Arg0, typename Arg1, typename Arg2, typename Arg3 >
		BIND_INLINE typename ThisType::return_type operator() ( Arg0 p0, Arg1 p1, Arg2 p2, Arg3 p3 )
		{
			return getInternal()( p0, p1, p2, p3 );
		}

	protected:
		BIND_INLINE typename ThisType::signature getInternal()
		{
			return (typename ThisType::signature)pObjectInstance;
		}
	};
#endif //BIND_STANDARD != BIND_STANDARD_MSVC2012

}
//----------------------------------------------------------------------------

#endif // !defined( BIND_HOST_C_API_ONLY )

BIND_C_API_BEGIN
	typedef void* binderoo_imported_function_t;
	typedef void* binderoo_func_ptr_t;
	typedef void* binderoo_imported_class_t;
	typedef void* binderoo_class_ptr_t;

	binderoo_imported_function_t	BIND_DLL binderoo_host_create_imported_function( char* pName, char* pSignature );
	void							BIND_DLL binderoo_host_destroy_imported_function( binderoo_imported_function_t pFunc );
	binderoo_func_ptr_t				BIND_DLL binderoo_host_get_function_ptr( binderoo_imported_function_t pFunc );

	binderoo_imported_class_t		BIND_DLL binderoo_host_create_imported_class( char* pName );
	void							BIND_DLL binderoo_host_addref_imported_class( binderoo_imported_class_t pObj );
	void							BIND_DLL binderoo_host_release_imported_class( binderoo_imported_class_t pObj );
	binderoo_class_ptr_t			BIND_DLL binderoo_host_get_class_ptr( binderoo_imported_class_t pObj );
BIND_C_API_END
//----------------------------------------------------------------------------

#endif // _BINDEROO_IMPORTS_H_

//============================================================================
