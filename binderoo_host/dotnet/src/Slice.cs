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

#if DEBUG
	//#define SliceFinalizerDebug
#endif // DEBUG
//----------------------------------------------------------------------------

using System;
using System.Runtime.InteropServices;
using System.Text;
using System.Diagnostics;
using System.Linq;
//----------------------------------------------------------------------------

namespace binderoo
{
	internal static partial class Util
	{
		// TODO: PROBABLY WON'T WORK ON MONO
		[ DllImport( "msvcrt", EntryPoint = "memcpy", CallingConvention = CallingConvention.Cdecl, SetLastError = false ) ]
		public static extern IntPtr memcpy(IntPtr dest, IntPtr src, UIntPtr count);

		public static void Copy< T >( IntPtr dest, T[] src )
		{
			GCHandle srcP = GCHandle.Alloc( src, GCHandleType.Pinned );
			UIntPtr len = new System.UIntPtr( (uint)( src.Length * Marshal.SizeOf( typeof( T ) ) ) );
			Util.memcpy( dest, srcP.AddrOfPinnedObject(), len );
			srcP.Free( );
		}

		public static void Copy< T >( T[] dest, IntPtr src )
		{
			GCHandle destP = GCHandle.Alloc( dest, GCHandleType.Pinned );
			UIntPtr len = new System.UIntPtr( (uint)( dest.Length * Marshal.SizeOf( typeof( T ) ) ) );
			Util.memcpy( destP.AddrOfPinnedObject(), src, len );
			destP.Free( );
		}
	}

	[ StructLayout( LayoutKind.Explicit, Pack = 8 ) ]
	public struct SliceData
	{
		[ FieldOffset( 0 ) ] private ulong			uLength;
		[ FieldOffset( 8 ) ] private IntPtr			pData;

		public void		Set( IntPtr p, ulong l )
		{
			uLength = l;
			pData = p;
		}

		public IntPtr	Pointer		{ get { return pData; } }
		public int		Length		{ get { return (int)uLength; } }

		public T[] Data< T >()
		{
			if( typeof( T ) == typeof( string ) )
			{
				T[] output = new T[ Length ];
				for( ulong curr = 0; curr < uLength; ++curr )
				{
					IntPtr sliceLengthOffset = (IntPtr)( pData.ToInt64() + (long)curr * 16 );
					ulong sliceLength = (ulong)Marshal.ReadInt64( sliceLengthOffset );
					IntPtr sliceOffset = (IntPtr)( pData.ToInt64() + (long)curr * 16 + 8 );
					IntPtr slice = Marshal.ReadIntPtr( sliceOffset );

					SliceData newData = new SliceData{ pData = slice, uLength = sliceLength };
					output[ curr ] = (T)Convert.ChangeType( new SliceString( newData ).Data, typeof( T ) );
				}

				return output;
			}
			else
			{
				T[] data = new T[ Length ];
				Util.Copy( data, Pointer );
				return data;
			}
		}

		public int DataSizeInBytes< T >()
		{
			return (int)uLength * Marshal.SizeOf( typeof( T ) );
		}

	}

	public struct SliceDataWrapper
	{
		public SliceData			data;
		public bool					bMarshalled;

		public void Set( IntPtr p, ulong l, bool m )
		{
			Dispose();

			data.Set( p, l );
			bMarshalled = m;
		}

		public void Dispose()
		{
			if( bMarshalled )
			{
				Marshal.FreeHGlobal( data.Pointer );
				this = new SliceDataWrapper();
			}
		}
	}

	public class SliceString : IDisposable
	{
		private SliceDataWrapper	sliceWrapper;

		public SliceString( SliceData newData )
		{
			sliceWrapper = new SliceDataWrapper();
			sliceWrapper.Set( newData.Pointer, (ulong)newData.Length, false );
		}

		public SliceString( string newData )
		{
			Data = newData;
		}

		~SliceString( ) { Dispose(); }

		public SliceData SliceData
		{
			get { return sliceWrapper.data; }
		}

		public string Data
		{
			get
			{
				byte[] data = sliceWrapper.data.Data< byte >();
				return Encoding.UTF8.GetString( data );
			}

			set
			{
				Dispose();
				int iByteCount = Encoding.UTF8.GetByteCount( value );
				byte[] data = new byte[ iByteCount + 1 ];
				Encoding.UTF8.GetBytes( value, 0, value.Length, data, 0 );
				IntPtr ptr = Marshal.AllocHGlobal( data.Length );
				Marshal.Copy( data, 0, ptr, data.Length );
				sliceWrapper.Set( ptr, (ulong)iByteCount, true );
			}
		}

		public void Dispose()
		{
#if SliceFinalizerDebug
			Debug.WriteLine( "Disposing " + this.GetType().ToString() + " (" + Data + ")..." );
#endif // SliceFinalizerDebug
			sliceWrapper.Dispose();
		}
	}

	public class Slice< T > : IDisposable
	{
		private SliceDataWrapper	sliceWrapper;

		public Slice( SliceData newData )
		{
			sliceWrapper = new SliceDataWrapper();
			sliceWrapper.Set( newData.Pointer, (ulong)newData.Length, false );
		}

		public Slice( T[] newData )
		{
			Data = newData;
		}

		~Slice( ) { Dispose(); }

		public SliceData SliceData
		{
			get { return sliceWrapper.data; }
		}

		public T[] Data
		{
			get
			{
				return sliceWrapper.data.Data< T >();
			}

			set
			{
				Dispose();
				if( value.Length > 0 )
				{
					if( typeof( T ) == typeof( string ) )
					{
						int dataSize = value.Length * Marshal.SizeOf( SliceData );
						SliceData[] data = new SliceData[ value.Length ];
						for( int index = 0; index < value.Length; ++index )
						{
							data[ index ] = new SliceString( (string)Convert.ChangeType( value[ index ], typeof( string ) ) ).SliceData;
						}
						IntPtr ptr = Marshal.AllocHGlobal( dataSize );
						Util.Copy( ptr, data );
						sliceWrapper.Set( ptr, (ulong)value.Length, true );
					}
					else
					{
						int dataSize = value.Length * Marshal.SizeOf( typeof( T ) );
						IntPtr ptr = Marshal.AllocHGlobal( dataSize );
						Util.Copy( ptr, value );
						sliceWrapper.Set( ptr, (ulong)value.Length, true );
					}
				}
			}
		}

		public void Dispose()
		{
#if SliceFinalizerDebug
			Debug.WriteLine( "Disposing " + this.GetType().ToString() + " (" + String.Join( ", ", Data.Select( f => f.ToString() ) ) + ")..." );
#endif // SliceFinalizerDebug
			sliceWrapper.Dispose();
		}
	}
}
