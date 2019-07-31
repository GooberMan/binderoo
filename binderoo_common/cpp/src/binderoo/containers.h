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

#pragma once

#if !defined( _BINDEROO_CONTAINERS_H_ )
#define _BINDEROO_CONTAINERS_H_

#include "binderoo/defs.h"

#include "binderoo/allocator.h"
#include <string>
#include <vector>

#include <algorithm>
#include <atomic>

namespace binderoo
{
	template< AllocatorSpace eSpace >
	struct Containers
	{
		typedef std::basic_string< char, std::char_traits< char >, binderoo::Allocator< eSpace, char > >			InternalString;
		typedef std::basic_string< wchar_t, std::char_traits< wchar_t >, binderoo::Allocator< eSpace, wchar_t > >	InternalWString;
		typedef std::vector< InternalString, binderoo::Allocator< eSpace, InternalString > >						StringVector;
		typedef std::vector< char, binderoo::Allocator< eSpace, char > >											CharVector;
	};
	//------------------------------------------------------------------------

	struct ScopeLock
	{
		BIND_INLINE ScopeLock( std::atomic< int32_t >& val )
			: toLock( val )
		{
			lock( 1, 0 );
		}

		BIND_INLINE ~ScopeLock()
		{
			lock( 0, 1 );
			_mm_mfence();
		}

	private:
		BIND_INLINE void lock( int32_t bLock, int32_t bExpected )
		{
			while( !toLock.compare_exchange_strong( bExpected, bLock ) ) { }
		}
		//--------------------------------------------------------------------

		std::atomic< int32_t >& toLock;
	};
	//------------------------------------------------------------------------

}

#endif // !defined( _BINDEROO_CONTAINERS_H_ )

//============================================================================
