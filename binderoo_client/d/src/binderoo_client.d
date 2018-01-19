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

module binderoo_client;

// D Runtime can't deal with pointer types being non-cyclic, so we ignore cyclic
// dependencies.
extern(C) __gshared string[] rt_options = [ "oncycle=ignore" ];

version( Windows )
{
	import core.sys.windows.windows;
	import core.sys.windows.dll;

	import core.sync.mutex;

	__gshared HINSTANCE hThisInstance = null;

	// Deferring startup of the D runtime until after DLL_PROCESS_ATTACH
	// often results in threads attaching to the DLL before we call
	// binderoo_init from the host thread. This is bad as the runtime
	// hasn't initialised. So in a quest for sanity - and extreme
	// inefficiency - we mutex lock threads until the runtime has
	// started up. A better way must be found, but this will do for
	// now...
	__gshared CRITICAL_SECTION mThisMutex;

	private void initMutex()
	{
		InitializeCriticalSection( &mThisMutex );
	}

	private void acquireMutex()
	{
		EnterCriticalSection( &mThisMutex );
	}

	private void releaseMutex()
	{
		LeaveCriticalSection( &mThisMutex );
	}

	private void deinitMutex()
	{
		DeleteCriticalSection( &mThisMutex );
	}

	export extern( C ) void binderoo_startup()
	{
		dll_process_attach( hThisInstance, true );
		releaseMutex();
	}

	export extern( C ) void binderoo_shutdown()
	{
		dll_process_detach( hThisInstance, true );
		deinitMutex();
	}

	export extern(Windows)
	BOOL DllMain(HINSTANCE hInstance, ULONG ulReason, LPVOID /*pvReserved*/)
	{
		final switch (ulReason)
		{
			case DLL_PROCESS_ATTACH:
				initMutex();
				acquireMutex();
				hThisInstance = hInstance;
				break;

			case DLL_PROCESS_DETACH:
				break;

			case DLL_THREAD_ATTACH:
				acquireMutex();
				dll_thread_attach( true, true );
				releaseMutex();
				break;

			case DLL_THREAD_DETACH:
				acquireMutex();
				dll_thread_detach( true, true );
				releaseMutex();
				break;
		}
		return true;
	}
}
else version( linux )
{
	import core.runtime;
	extern( C ) void binderoo_startup()
	{
		Runtime.initialize();
	}

	extern( C ) void binderoo_shutdown()
	{
		Runtime.terminate();
	}
}
