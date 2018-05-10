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

module binderoo.traits;
//----------------------------------------------------------------------------

private import std.traits;
//----------------------------------------------------------------------------

// Get the instance of an attribute bound to a symbol.
template GetUDA( alias symbol, Attribute )
{
	template Impl( A... )
	{
		static if( A.length == 0 )
		{
			alias void Impl;
		}
		else static if( is( A[ 0 ] ) )
		{
			static if( is( A[ 0 ] == Attribute ) )
			{
				enum Attribute Impl = Attribute();
			}
			else
			{
				alias Impl!( A[1..$] ) Impl;
			}
		}
		else static if( is( typeof( A[ 0 ] ) == Attribute ) )
		{
			enum Impl = A[ 0 ];
		}
		else
		{
			alias Impl!( A[ 1..$ ] ) Impl;
		}
	}

	alias GetUDA = Impl!( __traits( getAttributes, symbol ) );
}
//----------------------------------------------------------------------------

template HasUDA( alias symbol, Attribute )
{
	enum HasUDA = !is( GetUDA!( symbol, Attribute ) == void );
}
//----------------------------------------------------------------------------

// I don't know why, but the compiler didn't like using pointers in alias...
template HasUDA( T : A*, Attribute, A )
{
	enum HasUDA = !is( GetUDA!( T, Attribute ) == void );
}
//----------------------------------------------------------------------------

template IsConst( T )
{
	enum IsConst = is( T == const( BaseT ), BaseT )
				|| is( T == const( BaseT )*, BaseT );
}
//----------------------------------------------------------------------------

template IsImmutable( T )
{
	enum IsImmutable = is( T == immutable( BaseT ), BaseT );
}
//----------------------------------------------------------------------------

template IsInOut( T )
{
	enum IsInOut = is( T == inout( BaseT ), BaseT );
}
//----------------------------------------------------------------------------

template IsUserType( T )
{
	enum IsUserType = is( T == struct ) || is( T == class ) || is( T == enum ) || is( T == interface ) || is( T == union );
}
//----------------------------------------------------------------------------

template IsUserType( T : A*, A )
{
	enum IsUserType = IsUserType!A;
}
//----------------------------------------------------------------------------

template IsUserTypeButNotEnum( T )
{
	enum IsUserTypeButNotEnum = is( T == struct ) || is( T == class ) || is( T == interface ) || is( T == union );
}
//----------------------------------------------------------------------------

template IsUserTypeButNotEnum( alias T )
{
	enum IsUserTypeButNotEnum = false;
}
//----------------------------------------------------------------------------

template IsEnum( T )
{
	enum IsEnum = is( T == enum );
}
//----------------------------------------------------------------------------

alias IsAggregateType( T ) = IsUserTypeButNotEnum!T;
alias IsAggregateType( alias T ) = IsUserTypeButNotEnum!T;
//----------------------------------------------------------------------------

template IsIntegral( T )
{
	enum IsIntegral = is( T == byte )
					|| is( T == ubyte )
					|| is( T == short )
					|| is( T == ushort )
					|| is( T == int )
					|| is( T == uint )
					|| is( T == long )
					|| is( T == ulong );
}
//----------------------------------------------------------------------------

template IntegralTypeOf( T )
{
	static if( IsIntegral!T )
	{
		alias IntegralTypeOf = T;
	}
	else static if( is( T == bool ) )
	{
		alias IntegralTypeOf = byte;
	}
}
//----------------------------------------------------------------------------

template IsScalar( T )
{
	enum IsScalar = is( T == float )
					|| is( T == double )
					|| is( T == real );
}
//----------------------------------------------------------------------------

template IsSIMD( T )
{
	enum IsSIMD = is( T : __vector( ET ), ET );
}
//----------------------------------------------------------------------------

template IsUnsigned( T )
{
	enum IsUnsigned = is( T == ubyte )
					|| is( T == ushort )
					|| is( T == uint )
					|| is( T == ulong );
}
//----------------------------------------------------------------------------

template IsSigned( T )
{
	enum IsSigned = is( T == byte )
					|| is( T == short )
					|| is( T == int )
					|| is( T == long )
					|| is( T == float )
					|| is( T == double )
					|| is( T == real );
}
//----------------------------------------------------------------------------

template IsStaticMember( T, alias Member )
{
	static if( __traits( compiles, __traits( getMember, T, Member ) ) )
	{
		enum IsStaticMember = IsVariable!( __traits( getMember, T, Member ) )
							&& !__traits( compiles, __traits( getMember, T, Member ).offsetof );
	}
	else
	{
		enum IsStaticMember = false;
	}
}
//----------------------------------------------------------------------------

template IsPointer( T )
{
	enum IsPointer = false;
}
//----------------------------------------------------------------------------

template IsPointer( T : T* )
{
	enum IsPointer = true;
}
//----------------------------------------------------------------------------

template ArrayValueType( A : T[], T )
{
	alias ArrayValueType = T;
}
//----------------------------------------------------------------------------

template ArrayValueType( A : T[ E ], T, E )
{
	alias ArrayValueType = T;
}
//----------------------------------------------------------------------------

template ArrayKeyType( A : T[ E ], T, E )
{
	alias ArrayKeyType = E;
}
//----------------------------------------------------------------------------

template IsSomeArray( T )
{
	enum IsSomeArray = false;
}
//----------------------------------------------------------------------------

template IsSomeArray( A : T[], T )
{
	enum IsSomeArray = true;
}
//----------------------------------------------------------------------------

template IsSomeArray( A : T[ E ], T, E )
{
	enum IsSomeArray = true;
}
//----------------------------------------------------------------------------

template IsPlainArray( T )
{
	enum IsPlainArray = false;
}
//----------------------------------------------------------------------------

template IsPlainArray( A : T[], T )
{
	enum IsPlainArray = true;
}
//----------------------------------------------------------------------------

template IsStaticArray( T )
{
	enum IsStaticArray = false;
}
//----------------------------------------------------------------------------

template IsStaticArray( A : T[], T )
{
	enum IsStaticArray = false;
}
//----------------------------------------------------------------------------

template IsStaticArray( A : T[ L ], T, size_t L )
{
	enum IsStaticArray = true;
}
//----------------------------------------------------------------------------

template IsAssociativeArray( T )
{
	enum IsAssociativeArray = false;
}
//----------------------------------------------------------------------------

template IsAssociativeArray( A : T[ E ], T, E )
{
	enum IsAssociativeArray = true;
}
//----------------------------------------------------------------------------

template IsSomeType( T )
{
	import std.traits : isFunctionPointer;
	enum IsSomeType = !isFunctionPointer!( T );
}
//----------------------------------------------------------------------------

template IsSomeType( T... )
{
	enum IsSomeType = false;
}
//----------------------------------------------------------------------------

template IsSomeType( T : void )
{
	enum IsSomeType = false;
}
//----------------------------------------------------------------------------

template IsSomeType( alias symbol )
{
	import std.traits : isFunctionPointer;
	enum IsSomeType = is( symbol ) && !isFunctionPointer!( symbol );
}
//----------------------------------------------------------------------------

template IsVariable( X... ) if ( X.length == 1 )
{
	static if( is( X[ 0 ] == void ) || is( typeof( X[ 0 ] ) == void ) )
	{
		enum IsVariable = false;
	}
	else static if ( !IsSomeType!( X[ 0 ] ) )
	{
		static if( isSomeFunction!( X[ 0 ] ) )
		{
			enum IsVariable = isFunctionPointer!( X[ 0 ] ) || isDelegate!( X[ 0 ] );
		}
		else
		{
			enum IsVariable = is( typeof( X [ 0 ] ) )
							&& !is( typeof( X [ 0 ] ) == void )
							&& IsMutable!( typeof( X[ 0 ] ) );
		}
	}
	else
	{
		enum IsVariable = false;
	}
}
//----------------------------------------------------------------------------

template IsMemberVariable( X... ) if( X.length == 1 )
{
	static if( IsVariable!( X[ 0 ] ) )
	{
		enum IsMemberVariable = is( typeof( { enum OffsetOf = X[ 0 ].offsetof; } ) );
	}
	else
	{
		enum IsMemberVariable = false;
	}
}
//----------------------------------------------------------------------------

template PointerOf( T )
{
	alias PointerOf = T*;
}
//----------------------------------------------------------------------------

template PointerTarget( T )
{
	alias PointerTarget = T;
}
//----------------------------------------------------------------------------

template PointerTarget( T : A*, A )
{
	alias PointerTarget = A;
}
//----------------------------------------------------------------------------

template IsMutable( T )
{
	enum IsMutable = !( is( T == const )
						|| is( T == immutable )
						|| is( T == inout ) );
}
//----------------------------------------------------------------------------

template Unqualified( T )
{
	static if(		is( T A == immutable A )
				||	is( T A == shared inout const A )
				||	is( T A == shared inout A )
				||	is( T A == inout const A )
				||	is( T A == inout A )
				||	is( T A == shared const A )
				||	is( T A == shared A )
				||	is( T A == const A ) )
	{
		alias Unqualified = A;
	}
	else
	{
		alias Unqualified = T;
	}
}
//----------------------------------------------------------------------------

template TemplateParametersOf( T )
{
	static if( !IsMutable!( T ) )
	{
		alias UnqualifiedType = Unqualified!( T );
		alias TemplateParametersOf = TemplateParametersOf!( UnqualifiedType );
	}
	else
	{
		private import std.typetuple;
		alias TemplateParametersOf = TypeTuple!( );
	}
}
//----------------------------------------------------------------------------

template TemplateParametersOf( T : U!( Params ), alias U, Params... )
{
	private import std.typetuple;

	alias TemplateParametersOf = TypeTuple!( Params );
}
//----------------------------------------------------------------------------

template TemplateParametersOf( T : __vector( Type[ Length ] ), Type, size_t Length )
{
	private import std.typetuple;

	alias TemplateParametersOf = TypeTuple!( Type[ Length ] );
}
//----------------------------------------------------------------------------

template IsTemplatedType( T )
{
	static if( IsMutable!( T ) )
	{
		enum IsTemplatedType = false;
	}
	else
	{
		alias IsTemplatedType = IsTemplatedType!( Unqualified!( T ) );
	}
}
//----------------------------------------------------------------------------

template IsTemplatedType( T : U[], U )
{
	enum IsTemplatedType = IsTemplatedType!( U );
}
//----------------------------------------------------------------------------

template IsTemplatedType( alias T : U!( Params ), alias U, Params... )
{
	enum IsTemplatedType = true;
}
//----------------------------------------------------------------------------

template IsTemplatedType( T : U!( Params ), alias U, Params )
{
	enum IsTemplatedType = true;
}
//----------------------------------------------------------------------------

template TemplateOf( T )
{
	alias TemplateOf = void;
}
//----------------------------------------------------------------------------

template TemplateOf( alias T : U!( Params ), alias U, Params... )
{
	alias TemplateOf = U;
}
//----------------------------------------------------------------------------

template TemplateOf( T : U!( Params ), alias U, Params... )
{
	alias TemplateOf = U;
}
//----------------------------------------------------------------------------

template IsBaseTemplate( alias Base, alias Instantiated : U!( Params ), alias U, Params... )
{
	static if( __traits( compiles, Base!Params ) )
	{
		// HORRIBLE HACK THANKS TO TEMPLATES THAT ARE EXACTLY THE SAME NOT MATCHING
		enum IsBaseTemplate = Instantiated.stringof == Base!( Params ).stringof;
	}
	else
	{
		enum IsBaseTemplate = false;
	}
}
//----------------------------------------------------------------------------

template SupportsAppend( T : U[], U )
{
	enum SupportsAppend = true;
}
//----------------------------------------------------------------------------

template ZeroValue( T )
{
	static if( IsScalar!T )
	{
		enum ZeroValue = cast( T )0;
	}
	else
	{
		alias ZeroValue = T.init;
	}
}
//----------------------------------------------------------------------------

template Alias( alias A )
{
	alias Alias = A;
}

/+template TypeTuple( T... ) if( T.length > 0 )
{
	alias TypeTuple = T;
}+/
//----------------------------------------------------------------------------

string FullTypeName( Symbol )()
{
	static if( is( Symbol A == const A ) )
	{
		return "const(" ~ FullTypeName!( A ) ~ ")";
	}
	else static if( IsTemplatedType!Symbol )
	{
		return FullTypeName!( __traits( parent, TemplateOf!( Symbol ) ) ) ~ "." ~ Symbol.stringof;
	}
	else static if( __traits( compiles, __traits( parent, Symbol ) ) )
	{
		return FullTypeName!( __traits( parent, Symbol ) ) ~ "." ~ Symbol.stringof;
	}
	else
	{
		return Symbol.stringof;
	}
}
//----------------------------------------------------------------------------

string FullTypeName( alias Symbol )()
{
	import std.algorithm.searching : startsWith;
	import std.traits : isSomeFunction;

	static if( isSomeFunction!Symbol )
	{
		enum SymbolString = __traits( identifier, Symbol );
	}
	else static if( Symbol.stringof.startsWith( "module " ) )
	{
		enum SymbolString = Symbol.stringof[ 7 .. $ ];
	}
	else static if( Symbol.stringof.startsWith( "package " ) )
	{
		enum SymbolString = Symbol.stringof[ 8 .. $ ];
	}
	else
	{
		enum SymbolString = Symbol.stringof;
	}

	static if( __traits( compiles, __traits( parent, Symbol ) ) )
	{
		return FullTypeName!( __traits( parent, Symbol ) ) ~ "." ~ SymbolString;
	}
	else
	{
		return SymbolString;
	}
}
//----------------------------------------------------------------------------

string ModuleLocalTypeName( alias Symbol )()
{
	import std.algorithm.searching : startsWith;

	static if( Symbol.stringof.startsWith( "module " )
			|| Symbol.stringof.startsWith( "package " ) )
	{
		return "";
	}
	else
	{
		static if( is( Symbol A == const A ) )
		{
			return "const(" ~ ModuleLocalTypeName!( A ) ~ ")";
		}
		else static if( is( Symbol ) && IsTemplatedType!Symbol )
		{
			string strParent = ModuleLocalTypeName!( __traits( parent, TemplateOf!( Symbol ) ) );
			if( strParent.length > 0 )
			{
				strParent ~= ".";
			}
			return strParent ~ Symbol.stringof;
		}
		else static if( __traits( compiles, __traits( parent, Symbol ) ) )
		{
			static if( __traits( parent, Symbol ).stringof.startsWith( "module " )
					|| __traits( parent, Symbol ).stringof.startsWith( "package " ) )
			{
				return Symbol.stringof;
			}
			else
			{
				return ModuleLocalTypeName!( __traits( parent, Symbol ) ) ~ "." ~ Symbol.stringof;
			}
		}
		else
		{
			return "";
		}
	}
}
//----------------------------------------------------------------------------

string ModuleName( alias Symbol )()
{
	import std.algorithm.searching : startsWith;

	static if( __traits( compiles, __traits( parent, Symbol ) ) )
	{
		alias Parent = Alias!( __traits( parent, Symbol ) );

		static if( is( Symbol ) && IsTemplatedType!Symbol )
		{
			static if( __traits( parent, TemplateOf!( Symbol ) ).stringof.startsWith( "module " ) 
				|| __traits( parent, TemplateOf!( Symbol ) ).stringof.startsWith( "package " ) )
			{
				return FullTypeName!( __traits( parent, TemplateOf!( Symbol ) ) );
			}
			else
			{
				return ModuleName!( __traits( parent, TemplateOf!( Symbol ) ) );
			}
				
		}
		else static if( Parent.stringof.startsWith( "module " ) 
				|| Parent.stringof.startsWith( "package " ) )
		{
			return FullTypeName!( __traits( parent, Symbol ) );
		}
		else
		{
			return ModuleName!( __traits( parent, Symbol ) );
		}
	}

	return "";
}
//----------------------------------------------------------------------------

string[] gatherImports( T )()
{
	// Not complete, does not parse member types correctly
	string[] modules;

	void addModule( string name )
	{
		import std.algorithm;
		if( !modules.canFind( name ) )
		{
			modules ~= "import " ~ name ~ ";";
		}
	}

	static if( IsUserType!( T ) )
	{
		import std.traits;
		import binderoo.objectprivacy;

		addModule( moduleName!( T ) );

		static if( IsTemplatedType!( T ) )
		{
			foreach( Parameter; TemplateParametersOf!( T ) )
			{
				static if ( is( Parameter ) )
				{
					modules ~= gatherImports!( T );
				}
			}
		}

		foreach( member; __traits( allMembers, T ) )
		{
			static if( IsAccessible!( T, member ) && IsMemberVariable!( __traits( getMember, T, member ) ) && IsUserType!( typeof( __traits( getMember, T, member ) ) ) )
			{
				addModule( moduleName!( typeof( __traits( getMember, T, member ) ) ) );
			}
		}
	}

	return modules;
}
//----------------------------------------------------------------------------

auto joinWith( alias Pred = ( ref val ) => val, CT, T )( CT[] values, T joiner ) pure @safe nothrow if( SupportsAppend!( T ) )
{
	Unqualified!T output;

	foreach( ref value; values )
	{
		if( output.length > 0 )
		{
			output ~= joiner;
		}
		output ~= Pred( value );
	}

	return output;
}
//----------------------------------------------------------------------------

auto joinWithReverse( alias Pred = ( ref val ) => val, CT, T )( CT[] values, T joiner ) pure @safe nothrow if( SupportsAppend!( T ) )
{
	Unqualified!T output;

	foreach_reverse( ref value; values )
	{
		if( output.length > 0 )
		{
			output ~= joiner;
		}
		output ~= Pred( value );
	}

	return output;
}
//----------------------------------------------------------------------------

auto joinWith( T )( T[] values, T preJoiner, T postJoiner, T separator ) pure @safe nothrow if( SupportsAppend!( T ) )
{
	T output;

	foreach( ref value; values )
	{
		if( output.length > 0 )
		{
			output ~= postJoiner ~ separator;
		}
		output ~= preJoiner ~ value;
	}

	if( output.length > 0 )
	{
		return output ~ postJoiner;
	}

	return output;
}
//----------------------------------------------------------------------------

auto joinWithReverse( T )( T[] values, T joiner ) pure @safe nothrow if( SupportsAppend!( T ) )
{
	T output;

	foreach_reverse( ref value; values )
	{
		if( output.length > 0 )
		{
			output ~= joiner;
		}
		output ~= value;
	}

	return output;
}
//----------------------------------------------------------------------------

//============================================================================
