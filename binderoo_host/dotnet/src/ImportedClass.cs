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
	public class ImportedClass : IDisposable
	{
		public ImportedClass( string strClassName )
		{
			m_pObj = binderoo_host_create_imported_class( strClassName );
			m_pInstance = binderoo_host_get_class_ptr( m_pObj );
			m_strClassName = strClassName;
			m_bCreatedNew = true;
		}
		//--------------------------------------------------------------------

		public ImportedClass( IntPtr instance, string strClassName )
		{
			m_pObj = binderoo_host_register_imported_class( instance, strClassName );
			m_pInstance = instance;
			m_strClassName = strClassName;
			m_bCreatedNew = false;
		}
		//--------------------------------------------------------------------

		public IntPtr Ptr
		{
			// TODO: Revisit for rapid iteration
			get { return m_pInstance; } // binderoo_host_get_class_ptr( m_pObj ); }
		}
		//--------------------------------------------------------------------
		
#region InternalMagic
		[ DllImport( "binderoo_host", CallingConvention = CallingConvention.Cdecl ) ]
		extern static private IntPtr binderoo_host_create_imported_class(
			[ MarshalAs( UnmanagedType.LPStr ) ] string pName );
		//--------------------------------------------------------------------

		[ DllImport( "binderoo_host", CallingConvention = CallingConvention.Cdecl ) ]
		extern static private IntPtr binderoo_host_register_imported_class(
			IntPtr pObj,
			[ MarshalAs( UnmanagedType.LPStr ) ] string pClassName );
		//--------------------------------------------------------------------

		[ DllImport( "binderoo_host", CallingConvention = CallingConvention.Cdecl ) ]
		extern static private void binderoo_host_addref_imported_class( IntPtr pFunc );
		//--------------------------------------------------------------------

		[ DllImport( "binderoo_host", CallingConvention = CallingConvention.Cdecl ) ]
		extern static private void binderoo_host_release_imported_class( IntPtr pFunc );
		//--------------------------------------------------------------------

		[ DllImport( "binderoo_host", CallingConvention = CallingConvention.Cdecl ) ]
		extern static private IntPtr binderoo_host_get_class_ptr( IntPtr pFunc );
		//--------------------------------------------------------------------

		~ImportedClass()
		{
			Dispose();
		}
		//--------------------------------------------------------------------

		public void Dispose()
		{
			if( m_pObj != IntPtr.Zero )
			{
				binderoo_host_release_imported_class( m_pObj );
				m_pObj = IntPtr.Zero;
			}
		}
		//--------------------------------------------------------------------

		private IntPtr		m_pObj;
		private IntPtr		m_pInstance;
		private string		m_strClassName;
		private bool		m_bCreatedNew;

#endregion
	}
	//------------------------------------------------------------------------
}
//----------------------------------------------------------------------------

//============================================================================
