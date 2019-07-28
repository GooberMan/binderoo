/*
Binderoo
Copyright (c) 2016-2017, Remedy Entertainment
Copyright (c) 2018-2019, Ethan Watson
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

module binderoo.binding.boundobject;
//----------------------------------------------------------------------------

public import binderoo.slice;
public import binderoo.typedescriptor;

import binderoo.binding.serialise;
import binderoo.binding.attributes;
//----------------------------------------------------------------------------

//version = BoundSerialise;
//----------------------------------------------------------------------------

alias BoundObjectAllocator		= extern( C ) void* function( size_t uCount );
alias BoundObjectDeallocator	= extern( C ) void function( void* pObject );
alias BoundObjectThunk			= extern( C ) void* function( void* );
alias BoundObjectSerialise		= extern( C ) const( char )* function( void* );
alias BoundObjectDeserialise	= extern( C ) void function( void*, const( char )* );
alias BoundObjectQueryStringArr	= string[] function();
alias BoundObjectQueryBool		= bool function();
alias BoundObjectQueryString	= string function();

@CTypeName( "binderoo::BoundObject", "binderoo/boundobject.h" )
struct BoundObject
{
	@CTypeName( "binderoo::BoundObject::Type", "binderoo/boundobject.h" )
	enum Type : int
	{
		Undefined		= 0b00000000,
		Value			= 0b00000001,
		Reference		= 0b00000010,

		InstancedType	= 0b00000100,
	}

	DString						strFullyQualifiedName;
	ulong						uFullyQualifiedNameHash;

	BoundObjectAllocator		alloc;
	BoundObjectDeallocator		free;
	BoundObjectThunk			thunk;
	BoundObjectSerialise		serialise;
	BoundObjectDeserialise		deserialise;

	Type						eType = Type.Undefined;

	BoundObjectQueryStringArr	generateCSharpVariables;
	BoundObjectQueryStringArr	generateCSharpTypeDecl;
	BoundObjectQueryBool		hasBaseType;
	BoundObjectQueryString		getBaseTypeNameCSharp;
}
//----------------------------------------------------------------------------

pragma( inline, true ) bool Is( BoundObject.Type Type )( BoundObject.Type val )
{
	return cast(int)( val & Type ) != 0;
}
//----------------------------------------------------------------------------

pragma( inline, true ) bool Is( BoundObject.Type Type )( ref BoundObject obj )
{
	return obj.eType.Is!Type;
}
//----------------------------------------------------------------------------

pragma( inline, true ) bool Is( BoundObject.Type Type )( BoundObject* obj )
{
	return obj !is null ? Is!Type( *obj ) : false;
}
//----------------------------------------------------------------------------

struct BoundObjectFunctions( Type )
{
	enum Instanced = IsTemplatedType!Type ? BoundObject.Type.InstancedType : BoundObject.Type.Undefined;

	static if( is( Type == struct ) )
	{
		enum TypeVal = cast( BoundObject.Type )( BoundObject.Type.Value | Instanced );
		enum TypeSize = Type.sizeof;
		alias BaseType = void;
		alias CastType = Type*;
	}
	else
	{
		enum TypeVal = cast( BoundObject.Type )( BoundObject.Type.Reference | Instanced );
		enum TypeSize = __traits( classInstanceSize, Type );
		static if ( is( Type BT == super ) && BT.length > 0 && !is( BT[ 0 ] == Object ) )
		{
			alias BaseType = BT[ 0 ];
		}
		else
		{
			alias BaseType = void;
		}
		alias CastType = Type;
	}


	static extern( C ) void* thunkObj( void* pObj )
	{
		static if( is( Type == class ) )
		{
			return cast( void* )cast( CastType )cast( Object )pObj;
		}
		else
		{
			// TODO: Work out how to thunk value types
			return pObj;
		}
	}

	static extern( C ) void* allocObj( size_t uCount = 1 )
	in
	{
		assert( uCount == 1, "Only one object supported for allocation right now!" );
	}
	body
	{
		import std.conv : emplace;
		import core.stdc.stdlib : malloc;
		import core.memory : GC;

		// TODO: Provide allocator API
		auto mem = malloc( TypeSize )[ 0 .. TypeSize ];

		GC.addRange( mem.ptr, TypeSize );

		static if( is( Type == struct ) )
		{
			Type* pVal = cast(Type*)mem;

			import binderoo.binding.inheritance : constructObject;

			constructObject( *pVal );

			return cast(void*)mem;
		}
		else
		{
			//static assert( false, "Classes aren't ready to be used like this. Please stick to structs." );

			emplace!( Type )( mem[ 0 .. TypeSize ] );

			return cast(void*)mem;
		}
	}

	static extern( C ) void deallocObj( void* pObj )
	in
	{
		assert( thunkObj( pObj ) !is null, "Calling " ~ Type.stringof ~ " deallocator on a different type!" );
	}
	body
	{
		import core.stdc.stdlib : free;
		import core.memory : GC;

		// TODO: Provide allocator API
		auto val = cast( CastType )thunkObj( pObj );

		static if( is( Type == struct ) )
		{
			import binderoo.binding.inheritance : destructObject;
			
			destructObject( *val );
		}
		else
		{
			destroy( val );
		}

		GC.removeRange( pObj );
		free( pObj );
	}

	static extern( C ) const( char )* serialiseObj( void* pObj )
	in
	{
		assert( thunkObj( pObj ) !is null, "Calling " ~ Type.stringof ~ " serialiser on a different type!" );
	}
	body
	{
		version( BoundSerialise )
		{
			import std.json;
			import std.string : toStringz;

			static if( is( Type == struct ) )
			{
				CastType actualObj = cast( CastType )pObj;
				JSONValue serialised = serialise( *actualObj );
			}
			else
			{
				CastType actualObj = cast( CastType )pObj;
				JSONValue serialised = serialise( actualObj );
			}

			string strJSON = toJSON( &serialised );

			return strJSON.toStringz;
		}
		else
		{
			return null;
		}
	}

	static extern( C ) void deserialiseObj( void* pObj, const( char )* pData )
	in
	{
		assert( thunkObj( pObj ) !is null, "Calling " ~ Type.stringof ~ " deserialiser on a different type!" );
	}
	body
	{
		version( BoundSerialise )
		{
			import std.json;
			import core.stdc.string : strlen;

			const( char )[] strJSONSource = pData[ 0 .. strlen( pData ) ];
			JSONValue json = parseJSON( strJSONSource );

			CastType castObj = cast( CastType )pObj;
			static if( is( Type == struct ) )
			{
				deserialise( *castObj, json );
			}
			else
			{
				deserialise( castObj, json );
			}
		}
		else
		{

		}
	}

	static string[] generateCSharpTypeDecl()
	{
		import std.conv : to;
		import binderoo.objectprivacy;
		import binderoo.typedescriptor;

		string[] strOutput;
		static if( TypeVal.Is!( BoundObject.Type.Value ) )
		{
			strOutput ~= "[ StructLayout( LayoutKind.Explicit, Pack = " ~ Type.alignof.to!string ~ " ) ]";
			strOutput ~= "public struct " ~ CSharpTypeString!Type;
		}
		else static if( TypeVal.Is!( BoundObject.Type.Reference ) )
		{
			static if( !is( BaseType == void ) )
			{
				enum Base = CSharpFullTypeString!( BaseType );
			}
			else
			{
				enum Base = "IDisposable";
			}
			strOutput ~= "public class " ~ CSharpTypeString!Type ~ " : " ~ Base;
		}
		return strOutput;
	}

	static string[] generateCSharpVariables()
	{
		string[] strVariables;
		string[] strProperties;

		import std.conv : to;
		import std.string : replace;
		import binderoo.objectprivacy;
		import binderoo.typedescriptor;

		static foreach( var; Type.tupleof )
		{
			static if( is( Type == struct ) )
			{
				static if( IsStaticArray!( typeof( var ) ) )
				{
					strVariables ~= "\t//Ridiculous fixed array preservation code for " ~ __traits( identifier, var ) ~ " of length " ~ StaticArrayLength!( typeof( var ) ).to!string;
					static foreach( iIndex; 0 .. StaticArrayLength!( typeof( var ) ) )
					{
						strVariables ~=	"\t[ FieldOffset( " ~ ( var.offsetof + iIndex * ArrayValueType!( typeof( var ) ).sizeof ).to!string ~ " ) ] "
										~ "private "
										~ ( is( ArrayValueType!( typeof( var ) ) == class ) ? "IntPtr" : CSharpFullTypeString!( ArrayValueType!( typeof( var ) ), MarshallingStage.Marshalled ) )
										~ " "
										~ ( HasUDA!( var, InheritanceBase ) ? "_baseobj" : "var_" ~ __traits( identifier, var ) )
										//~ ( var.init != typeof( var ).init ? " = " ~ var.init.stringof : "" )
										~ "_elem" ~ iIndex.to!string
										~ ";";
					}
				}
				else
				{
					strVariables ~=	"\t[ FieldOffset( " ~ var.offsetof.to!string ~ " ) ] "
									~ "private "
									~ ( is( typeof( var ) == class ) ? "IntPtr" : CSharpFullTypeString!( typeof( var ), MarshallingStage.Marshalled ) )
									~ " "
									~ ( HasUDA!( var, InheritanceBase ) ? "_baseobj" : "var_" ~ __traits( identifier, var ) )
									//~ ( var.init != typeof( var ).init ? " = " ~ var.init.stringof : "" )
									~ ";";

					static if( IsExternallyAccessible!var )
					{
						strProperties ~= "\tpublic " ~ CSharpFullTypeString!( typeof( var ) ) ~ " " ~ __traits( identifier, var );
						strProperties ~= "\t{";
						static if( is( typeof( var ) == string ) || is( typeof( var ) == const(char)[] ) )
						{
							strProperties ~= "\t\tget { return new SliceString( var_" ~ __traits( identifier, var ) ~ " ).Data; }";
							static if( IsMutable!( typeof( var ) ) ) strProperties ~= "\t\t// Setter coming soon...";
						}
						else static if( IsNonAssociativeArray!( typeof( var ) ) )
						{
							strProperties ~= "\t\tget { return new Slice< " ~ CSharpFullTypeString!( ArrayValueType!( typeof( var ) ) ) ~ " >( var_" ~ __traits( identifier, var ) ~ " ).Data; }";
							static if( IsMutable!( typeof( var ) ) ) strProperties ~= "\t\t// Setter coming soon...";
						}
						else
						{
							strProperties ~= "\t\tget { return var_" ~ __traits( identifier, var ) ~ "; }";
							static if( IsMutable!( typeof( var ) ) )
							{
								strProperties ~= "\t\tset { var_" ~ __traits( identifier, var ) ~ " = value; }";
							}
						}
						strProperties ~= "\t}";
						strProperties ~= "";
					}
				}
			}
			else static if( is( Type == class ) && PrivacyOf!var == PrivacyLevel.Public )
			{
				{
					enum bool GenerateSliceWrapper = IsNonAssociativeArray!( typeof( var ) ) || is( typeof( var ) == string );

					strProperties ~= "\tpublic " ~ CSharpFullTypeString!( typeof( var ) ) ~ " " ~ __traits( identifier, var );
					strProperties ~= "\t{";
					strProperties ~= "\t\tget { return " ~ ( GenerateSliceWrapper ? "new " ~ CSharpFullTypeString!( typeof( var ), MarshallingStage.Intermediary ) ~ "( " : "" ) ~ "binderoointernal.FP." ~ FullTypeName!( Type ).replace( ".", "_" ) ~ "_" ~ __traits( identifier, var ) ~ "_Getter( pObj.Ptr )" ~ ( GenerateSliceWrapper ? " ).Data" : "" ) ~ "; }";
					static if( IsMutable!( typeof( var ) ) )
					{
						strProperties ~= "\t\tset { binderoointernal.FP." ~ FullTypeName!( Type ).replace( ".", "_" ) ~ "_" ~ __traits( identifier, var ) ~ "_Setter( pObj.Ptr, " ~ ( GenerateSliceWrapper ? "new " ~ CSharpFullTypeString!( typeof( var ), MarshallingStage.Intermediary ) ~ "( value ).SliceData" : "value" ) ~ " ); }";
					}
					strProperties ~= "\t}";
					strProperties ~= "";
				}
			}
		}
		
		return strProperties ~ strVariables;
	}

	bool hasBaseType()
	{
		return !is( BaseType == void );
	}

	string getBaseTypeNameCSharp()
	{
		return CSharpFullTypeString!( BaseType );
	}
}
//----------------------------------------------------------------------------

//============================================================================
