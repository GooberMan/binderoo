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

using System;
using System.Runtime.InteropServices;
//----------------------------------------------------------------------------

namespace binderoo
{
	public class ImportedFunction : IDisposable
	{
		public ImportedFunction( string strFuncName, string strSignature )
		{
			m_strFuncName = strFuncName;
			m_strSignature = strSignature;
			Obtain();
		}
		//--------------------------------------------------------------------
		
		public IntPtr FuncPtr
		{
			get { return binderoo_host_get_function_ptr( m_pFunc ); }
		}
		//--------------------------------------------------------------------

#region InternalMagic

		[ DllImport( "binderoo_host", CallingConvention = CallingConvention.Cdecl ) ]
		extern static private IntPtr binderoo_host_create_imported_function(
			[ MarshalAs( UnmanagedType.LPStr ) ]
			string pName,
			[ MarshalAs( UnmanagedType.LPStr ) ]
			string pSignature );
		//--------------------------------------------------------------------

		[ DllImport( "binderoo_host", CallingConvention = CallingConvention.Cdecl ) ]
		extern static private void binderoo_host_destroy_imported_function( IntPtr pFunc );
		//--------------------------------------------------------------------

		[ DllImport( "binderoo_host", CallingConvention = CallingConvention.Cdecl ) ]
		extern static private IntPtr binderoo_host_get_function_ptr( IntPtr pFunc );
		//--------------------------------------------------------------------

		~ImportedFunction()
		{
			Dispose();
		}
		//--------------------------------------------------------------------

		public void Obtain()
		{
			if ( m_pFunc == IntPtr.Zero )
			{
				m_pFunc = binderoo_host_create_imported_function( m_strFuncName, m_strSignature );
			}
		}
		//--------------------------------------------------------------------

		public void Dispose()
		{
			if( m_pFunc != IntPtr.Zero )
			{
				binderoo_host_destroy_imported_function( m_pFunc );
				m_pFunc = IntPtr.Zero;
			}
		}
		//--------------------------------------------------------------------

		private IntPtr		m_pFunc;
		private string		m_strFuncName;
		private string		m_strSignature;
#endregion
	}
	//------------------------------------------------------------------------
}
//----------------------------------------------------------------------------

//============================================================================
