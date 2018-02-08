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

module binderoo.typedescriptor;
//----------------------------------------------------------------------------

public import binderoo.traits;
public import std.traits;
public import std.typetuple;
//----------------------------------------------------------------------------

struct TypeDescriptor( T, bool bIsRef = false )
{
	template HasUDA( Attr )
	{
		static if( is( T == void ) )
		{
			enum HasUDA = false;
		}
		else
		{
			enum HasUDA = binderoo.traits.HasUDA!( T, Attr );
		}
	}
	//-------------------------------------------------------------------------

	template GetUDA( Attr )
	{
		static if( is( T == void ) )
		{
			alias GetUDA = void;
		}
		else
		{
			alias GetUDA = binderoo.traits.GetUDA!( T, Attr );
		}
	}
	//-------------------------------------------------------------------------

	static if( __traits( compiles, __traits( getAttributes, T ) ) )
	{
		alias		UDAs							= TypeTuple!( __traits( getAttributes, T ) );
	}
	else
	{
		alias		UDAS							= TypeTuple!();
	}
	//------------------------------------------------------------------------

	alias			Type							= T;
	enum			Name							= T.stringof;

	alias			UnqualifiedType					= Unqualified!( T );

	enum			Size							= T.sizeof;

	enum			IsScalarType					= std.traits.isScalarType!( T );
	enum			IsBasicType						= std.traits.isBasicType!( T );
	enum			IsAggregateType					= std.traits.isAggregateType!( T );
	enum			IsUserType						= binderoo.traits.IsUserType!( T );

	enum			IsVoid							= is( T == void );
	enum			IsEnum							= is( T == enum );
	enum			IsStruct						= is( T == struct );
	enum			IsClass							= is( T == class );
	enum			IsInterface						= is( T == interface );

	enum			IsConst							= binderoo.traits.IsConst!( T );
	enum			IsImmutable						= binderoo.traits.IsImmutable!( T );
	enum			IsRef							= bIsRef;
	enum			IsPointer						= binderoo.traits.IsPointer!( T );

	// Arrays are a bit special. We handle associative arrays, arrays, and not-an-array separately.
	static if( binderoo.traits.IsAssociativeArray!( T ) )
	{
		// We define IsElementAssociativeArray to true AND IsElementArray to false here. This provides a complete descriptor regardless of the type.
		enum		IsSomeArray						= true;
		enum		IsAssociativeArray				= true;
		enum		IsArray							= false;
		alias 		ArrayValueType					= TypeDescriptor!( binderoo.traits.ArrayValueType!( T ) );
		alias		ArrayKeyType					= TypeDescriptor!( binderoo.traits.ArrayKeyType!( T ) );
	}
	else static if( binderoo.traits.IsPlainArray!( T ) )
	{
		// The key type is aliased to void here as we don't actually have a key type.
		enum		IsSomeArray						= true;
		enum		IsAssociativeArray				= false;
		enum		IsArray							= true;
		alias		ArrayValueType					= TypeDescriptor!( binderoo.traits.ArrayValueType!( T ) );
		alias		ArrayKeyType					= TypeDescriptor!( void );
	}
	else
	{
		// And the array types and identifying enumerations are set to void and false here.
		enum		IsSomeArray						= false;
		enum		IsAssociativeArray				= false;
		enum		IsArray							= false;
		alias		ArrayValueType					= TypeDescriptor!( void );
		alias		ArrayKeyType					= TypeDescriptor!( void );
	}

}
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------

// Mark up your user types with this
struct CTypeName
{
	string name;
	string header;
	ulong hash;

	@disable this();

	this( string n )
	{
		import binderoo.hash;
		name = n;
		hash = fnv1a_64( name );
	}

	this( string n, string h )
	{
		import binderoo.hash;
		name = n;
		header = h;
		hash = fnv1a_64( name );
	}
}
//----------------------------------------------------------------------------

struct CSharpTypeName
{
	string name;
	string namespace;
	ulong hash;

	@disable this();

	this( string n )
	{
		import binderoo.hash;
		name = n;
		hash = fnv1a_64( name );
	}

	this( string n, string ns )
	{
		import binderoo.hash;
		name = n;
		namespace = ns;
		hash = fnv1a_64( name );
	}
}
//----------------------------------------------------------------------------

// Dealing with a native type or library type? Stick this somewhere visible.
enum TypeNameUndefined						= "undefined";
enum CTypeNameOverride( T )					= TypeNameUndefined;
enum CSharpTypeNameOverride( T )			= TypeNameUndefined;
//----------------------------------------------------------------------------

// Default overrides
enum CTypeNameOverride( T : byte )			= "int8_t";
enum CTypeNameOverride( T : ubyte )			= "uint8_t";
enum CTypeNameOverride( T : short )			= "int16_t";
enum CTypeNameOverride( T : ushort )		= "uint16_t";
enum CTypeNameOverride( T : int )			= "int32_t";
enum CTypeNameOverride( T : uint )			= "uint32_t";
enum CTypeNameOverride( T : long )			= "int64_t";
enum CTypeNameOverride( T : ulong )			= "uint64_t";
enum CTypeNameOverride( T : wchar )			= "wchar_t";
enum CTypeNameOverride( T : char )			= "char"; // char will cast to a byte if you don't redefine here
enum CTypeNameOverride( T : bool )			= "bool"; // Returns char otherwise when parsing functions? o_O
//----------------------------------------------------------------------------

enum CSharpTypeNameOverride( T : char )		= "byte";
enum CSharpTypeNameOverride( T : wchar ) 	= "char";
//----------------------------------------------------------------------------

template CTypeString( T )
{
	template TemplateParameters( uint iIndex, T... )
	{
		static if( iIndex < T.length )
		{
			enum TemplateParameters = CTypeString!( T[ iIndex ] ) ~ ( iIndex < T.length - 1 ? ", " ~ TemplateParameters!( iIndex + 1, T ) : "" );
		}
		else
		{
			enum TemplateParameters = "";
		}
	}

	template TemplateParametersString( T )
	{
		static if( IsTemplatedType!( T ) )
		{
			enum TemplateParametersString = "< " ~ TemplateParameters!( 0u, TemplateParametersOf!( T ) ) ~ " >";
		}
		else
		{
			enum TemplateParametersString = "";
		}
	}

	template TemplateTypeString( T : Obj!( Params ), alias Obj, Params... )
	{
		enum TemplateTypeString = Obj.stringof;
	}

	template TemplateTypeString( T )
	{
		enum TemplateTypeString = T.stringof;
	}

	static if( isPointer!( T ) )
	{
		enum CTypeString = CTypeString!( binderoo.traits.PointerTarget!( T ) ) ~ "*";
	}
	else static if( IsConst!( T ) || IsImmutable!( T ) )
	{
		enum CTypeString = "const " ~ CTypeString!( Unqual!( T ) );
	}
	else static if( CTypeNameOverride!( T ) != TypeNameUndefined )
	{
		enum CTypeString = CTypeNameOverride!( T );
	}
	else static if( IsUserType!( T ) )
	{
		static if( HasUDA!( T, CTypeName ) )
		{
			enum CTypeString = GetUDA!( T, CTypeName ).name ~ TemplateParametersString!( T );
		}
		else
		{
			enum CTypeString = TemplateTypeString!( T ) ~ TemplateParametersString!( T );
		}
	}
	else
	{
		enum CTypeString = T.stringof;
	}
}
//----------------------------------------------------------------------------

// Value writing is required for template parameters
template CTypeString( long val )
{
	import std.conv : to; enum CTypeString = to!string( val );
}
//----------------------------------------------------------------------------

template CTypeString( ulong val )
{
	import std.conv : to; enum CTypeString = to!string( val );
}
//----------------------------------------------------------------------------

template CSharpTypeString( T )
{
	static if( IsConst!( T ) || IsImmutable!( T ) )
	{
		enum CSharpTypeString = "const " ~ CSharpTypeString!( Unqual!( T ) );
	}
	else static if( CSharpTypeNameOverride!( T ) != TypeNameUndefined )
	{
		enum CSharpTypeString = CSharpTypeNameOverride!( T );
	}
	else static if( IsUserType!( T ) )
	{
		static if( HasUDA!( T, CSharpTypeName ) )
		{
			enum CSharpTypeString = GetUDA!( T, CSharpTypeName ).name;
		}
		else
		{
			enum CSharpTypeString = T.stringof;
		}
	}
	else
	{
		enum CSharpTypeString = T.stringof;
	}
}

struct TypeString( T, bool bIsRef = false )
{
	static private string typeStringD( string TypeName )
	{
		return ( bIsRef ? "ref " : "" ) ~ TypeName;
	}

	static private string typeStringC( string TypeName )
	{
		return TypeName ~ ( bIsRef ? "&" : "" );
	}

	static private string unqualC( string TypeName )
	{
		import std.string;

		auto found = TypeName.lastIndexOf( ':' );

		if( found > 0 && TypeName[ found - 1 ] == ':' )
		{
			return TypeName[ found + 1 .. $ ];
		}
		return TypeName;
	}

	static private string typeStringCSharp( )
	{
		static if( ( IsConst!( T ) || IsImmutable!( T ) )
					&& bIsRef )
		{
			return "in " ~ CSharpTypeString!( Unqual!( T ) );
		}
		else
		{
			return ( bIsRef ? "ref " : "" ) ~ CSharpTypeString!( T );
		}
	}
	//------------------------------------------------------------------------

	enum			FullyQualifiedDDecl				= typeStringD( fullyQualifiedName!( T ) );
	enum			DDecl							= typeStringD( T.stringof );
	enum			CDecl							= typeStringC( CTypeString!( T ) );
	enum			UnqualifiedCDecl				= unqualC( typeStringC( CTypeString!( T ) ) );
	enum			CSharpDecl						= typeStringCSharp( );
}
//----------------------------------------------------------------------------

template TypeString( Desc : TypeDescriptor!( T, bIsRef ), T, bool bIsRef )
{
	alias TypeString = TypeString!( T, bIsRef );
}
//----------------------------------------------------------------------------

//============================================================================
