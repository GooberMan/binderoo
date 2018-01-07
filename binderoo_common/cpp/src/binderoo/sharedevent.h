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

#if !defined( _BINDEROO_SHAREDEVENT_H_ )
#define _BINDEROO_SHAREDEVENT_H_

#include "binderoo/defs.h"
#include "binderoo/slice.h"

#if BIND_SYSAPI == BIND_SYSAPI_WINAPI
	#include <Windows.h>
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
		#include <sys/stat.h>
		#include <semaphore.h>
#endif // OS check
//----------------------------------------------------------------------------

namespace binderoo
{
	class SharedEvent
	{
	private:
		SharedEvent();

	public:
		enum : uint32_t
		{
			InfiniteWait = 0xFFFFFFFF
		};
		//--------------------------------------------------------------------

		SharedEvent( binderoo::DString strEventName );
		~SharedEvent();
		//--------------------------------------------------------------------

		void signal();
		bool waitOn( uint32_t uMilliseconds = InfiniteWait );
		//--------------------------------------------------------------------

	private:
#if BIND_SYSAPI == BIND_SYSAPI_WINAPI
		HANDLE		hEvent;
#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
		sem_t*		hEvent;
#endif
	};
	//------------------------------------------------------------------------
}
//----------------------------------------------------------------------------

#endif // !defined( _BINDEROO_SHAREDEVENT_H_ )

//============================================================================
