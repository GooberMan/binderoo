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

#include "sharedevent.h"
//----------------------------------------------------------------------------

#if BIND_SYSAPI == BIND_SYSAPI_POSIX
	#include <fcntl.h>
	#include <sys/stat.h>

	#define INVALID_HANDLE_VALUE nullptr
#endif 

binderoo::SharedEvent::SharedEvent()
	: hEvent( INVALID_HANDLE_VALUE )
{
}
//----------------------------------------------------------------------------

#if BIND_SYSAPI == BIND_SYSAPI_WINDOWS
binderoo::SharedEvent::SharedEvent( binderoo::DString strEventName )
	: hEvent( INVALID_HANDLE_VALUE )
{
	HANDLE hResolved = OpenEvent( EVENT_ALL_ACCESS, FALSE, strEventName.data() );

	if( hResolved == NULL )
	{
		hResolved = CreateEvent( NULL, FALSE, FALSE, strEventName.data() );
	}

	if( hResolved != NULL )
	{
		hEvent = hResolved;
	}
}
//----------------------------------------------------------------------------

binderoo::SharedEvent::~SharedEvent()
{
	if( hEvent != INVALID_HANDLE_VALUE )
	{
		CloseHandle( hEvent );
	}
}
//----------------------------------------------------------------------------

void binderoo::SharedEvent::signal()
{
	if( hEvent != INVALID_HANDLE_VALUE )
	{
		SetEvent( hEvent );
	}
}
//----------------------------------------------------------------------------

bool binderoo::SharedEvent::waitOn( uint32_t uMilliseconds )
{
	if( hEvent != INVALID_HANDLE_VALUE )
	{
		DWORD dWaitTime = (DWORD)uMilliseconds;

		return WaitForSingleObject( hEvent, dWaitTime ) == WAIT_OBJECT_0;
	}

	return false;
}
//----------------------------------------------------------------------------

#elif BIND_SYSAPI == BIND_SYSAPI_POSIX
// TODO: Posix semaphores might need to switch over to unnamed, shared memory
// semaphores. See http://man7.org/linux/man-pages/man7/sem_overview.7.html

binderoo::SharedEvent::SharedEvent( binderoo::DString strEventName )
	: hEvent( INVALID_HANDLE_VALUE )
{
	sem_t* hResolved = sem_open( strEventName.data(), O_CREAT, S_IRUSR | S_IWUSR, 0 );

	if( hResolved != NULL )
	{
		// TODO: Work out if immediately unlinking makes things wrong or not...
		//sem_unlink( strEventName.c_str() );
		hEvent = hResolved;
	}
}
//----------------------------------------------------------------------------

binderoo::SharedEvent::~SharedEvent()
{
	if( hEvent != INVALID_HANDLE_VALUE )
	{
		sem_close( hEvent );
	}
}
//----------------------------------------------------------------------------

void binderoo::SharedEvent::signal()
{
	if( hEvent != INVALID_HANDLE_VALUE )
	{
		sem_post( hEvent );
	}
}
//----------------------------------------------------------------------------

bool binderoo::SharedEvent::waitOn( uint32_t uMilliseconds )
{
	if( hEvent != INVALID_HANDLE_VALUE )
	{
		uint32_t uSeconds = uMilliseconds / 1000;
		uint32_t uNanoseconds = ( uMilliseconds % 1000 ) * 1000000;
		timespec waitTime = { uSeconds, uNanoseconds };

		return sem_timedwait( hEvent, &waitTime ) == 0;
	}

	return false;
}
//----------------------------------------------------------------------------

#endif // BIND_SYSAPI checks
//----------------------------------------------------------------------------

//============================================================================
