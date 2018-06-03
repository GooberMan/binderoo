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
	static if( IsPointer!T )
	{
		enum IsUserType = IsUserType!( PointerTarget!T );
	}
	else
	{
		enum IsUserType = is( T == struct ) || is( T == class ) || is( T == enum ) || is( T == interface ) || is( T == union );
	}
}
//----------------------------------------------------------------------------

template IsUserType( T : ET[], ET )
{
	enum IsUserType = IsUserType!ET;
}
//----------------------------------------------------------------------------

template IsUserType( alias T : U!( Params ), alias U, Params... )
{
	enum IsUserType = is( T == struct ) || is( T == class ) || is( T == enum ) || is( T == interface ) || is( T == union );
}
//----------------------------------------------------------------------------

template IsUserType( T : U!( Params ), alias U, Params... )
{
	enum IsUserType = is( T == struct ) || is( T == class ) || is( T == enum ) || is( T == interface ) || is( T == union );
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
	else static if( is( T U == enum ) )
	{
		alias IntegralTypeOf = IntegralTypeOf!U;
	}
	else static if( is( T == bool ) )
	{
		alias IntegralTypeOf = byte;
	}
	else
	{
		alias IntegralTypeOf = void;
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

template IsPlainArray( A : T[ L ], T, size_t L )
{
	enum IsPlainArray = false;
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

template StaticArrayLength( A : T[ L ], T, size_t L )
{
	enum StaticArrayLength = L;
}
//----------------------------------------------------------------------------

template IsNonAssociativeArray( T )
{
	enum IsNonAssociativeArray = IsPlainArray!T || IsStaticArray!T;
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
	static if( IsAggregateType!T )
	{
		mixin( "import " ~ ModuleName!T ~ ";" );
	}
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

template IsSomeType( alias Symbol )
{
	static if( IsAggregateType!Symbol )
	{
		mixin( "import " ~ ModuleName!Symbol ~ ";" );
	}
	import std.traits : isFunctionPointer;
	enum IsSomeType = is( Symbol ) && !isFunctionPointer!( Symbol );
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

template IsModule( alias Symbol )
{
	static if( isSomeFunction!Symbol )
	{
		enum IsModule = false;
	}
	else
	{
		import std.algorithm.searching : startsWith;
		enum IsModule = Symbol.stringof.startsWith( "module " );
	}
}
//----------------------------------------------------------------------------

template IsPackage( alias Symbol )
{
	static if( isSomeFunction!Symbol )
	{
		enum IsPackage = false;
	}
	else
	{
		import std.algorithm.searching : startsWith;
		enum IsPackage = Symbol.stringof.startsWith( "package " );
	}
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

template IsTemplate( alias T )
{
	enum IsTemplate = __traits( isTemplate, T );
}
//----------------------------------------------------------------------------

template IsTemplate( T )
{
	enum IsTemplate = __traits( isTemplate, T );
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

template IsTemplatedType( T : U!( Params ), alias U, Params )
{
	enum IsTemplatedType = true;
}
//----------------------------------------------------------------------------

template IsTemplatedType( alias T : U!( Params ), alias U, Params... )
{
	enum IsTemplatedType = true;
}
//----------------------------------------------------------------------------

template IsTemplatedType( T : U!( Params )[], alias U, Params )
{
	enum IsTemplatedType = true;
}
//----------------------------------------------------------------------------

template IsTemplatedType( alias T : U!( Params )[], alias U, Params... )
{
	enum IsTemplatedType = true;
}
//----------------------------------------------------------------------------

template IsTemplatedType( alias T )
{
	enum IsTemplatedType = false;
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

template TemplateOf( alias T : U!( Params )[], alias U, Params... )
{
	alias TemplateOf = U;
}
//----------------------------------------------------------------------------

template TemplateOf( T : U!( Params )[], alias U, Params... )
{
	alias TemplateOf = U;
}
//----------------------------------------------------------------------------

template TemplateParamsOf( alias T : U!( Params ), alias U, Params... )
{
	alias TemplateParamsOf = binderoo.traits.AliasSeq!( Params );
}
//----------------------------------------------------------------------------

template TemplateParamsOf( T: U!( Params ), alias U, Params... )
{
	alias TemplateParamsOf = binderoo.traits.AliasSeq!( Params );
}
//----------------------------------------------------------------------------

template TemplateParamsOf( alias T : U!( Params )[], alias U, Params... )
{
	alias TemplateParamsOf = binderoo.traits.AliasSeq!( Params );
}
//----------------------------------------------------------------------------

template TemplateParamsOf( T: U!( Params )[], alias U, Params... )
{
	alias TemplateParamsOf = binderoo.traits.AliasSeq!( Params );
}
//----------------------------------------------------------------------------

template TemplateParamsOf( T : __vector( Type[ Length ] ), Type, size_t Length )
{
	alias TemplateParamsOf = binderoo.traits.AliasSeq!( Type[ Length ] );
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
//----------------------------------------------------------------------------

template AliasSeq( A... )
{
	alias AliasSeq = A;
}
//----------------------------------------------------------------------------

// TestTemplate must be instantiable, take one parameter, and alias to a boolean enum.
template Filter( alias TestTemplate, Symbols... ) if( __traits( compiles, "TestTemplate!Symbols[ 0 ]" ) )
{
	string generate()
	{
		import std.conv : to;

		string[] passedSymbols;
		static foreach( iIndex, Symbol; Symbols )
		{
			static if( TestTemplate!Symbol )
			{
				passedSymbols ~= "Symbols[ " ~ iIndex.to!string ~ " ]";
			}
		}

		return "alias Filter = binderoo.traits.AliasSeq!( " ~ passedSymbols.joinWith( ", " ) ~ " );";
	}

	mixin( generate() );
}
//----------------------------------------------------------------------------

template ModuleOf( alias Symbol )
{
	import std.algorithm.searching : startsWith;

	static if( IsModule!Symbol || IsPackage!Symbol )
	{
		alias ModuleOf = Symbol;
	}
	else
	{
		alias ModuleOf = ModuleOf!( __traits( parent, Symbol ) );
	}
}
//----------------------------------------------------------------------------

template ModuleFromName( string strModuleName )
{
	mixin( "import " ~ strModuleName ~ "; alias ModuleFromName = binderoo.traits.Alias!( " ~ strModuleName ~ " );" );
}
//----------------------------------------------------------------------------

template AliasFromName( string strName, string strModuleName = "" )
{
	static if( strModuleName.length )
	{
		mixin( "import " ~ strModuleName ~ ";" );
	}
	mixin( "alias AliasFromName = binderoo.traits.Alias!( " ~ strName ~ ");" );
}
//----------------------------------------------------------------------------

template IsAlias( alias Parent, string SymbolName )
{
	static bool impl()
	{
		mixin( "static import " ~ ModuleName!Parent ~ ";" );
		static if( __traits( compiles, "alias ThisSymbol = binderoo.traits.Alias!( " ~ FullTypeName!Parent ~ "." ~ SymbolName ~ " );" ) )
		{
			mixin( "alias ThisSymbol = binderoo.traits.Alias!( " ~ FullTypeName!Parent ~ "." ~ SymbolName ~ " );" );
			return SymbolName != ThisSymbol.stringof; //__traits( identifier, ThisSymbol );
		}
		else
		{
			return false;
		}
	}

	enum IsAlias = impl();
}
//----------------------------------------------------------------------------

struct SymbolOverride
{
	bool Overrides;
	string Name;
}
//----------------------------------------------------------------------------

struct DSymbolToStringProvider
{
	enum String( T ) = T.stringof;
	enum String( alias T ) = T.stringof;
	enum SymbolOverride OverrideString( T ) = { false, T.stringof };
	enum TemplateOpen = "!(";
	enum TemplateClose = ")";
	enum ConstOpen = "const( ";
	enum ConstClose = " )";
	enum NamespaceSeparator = ".";
}
//----------------------------------------------------------------------------

struct CSymbolToStringProvider
{
	enum String( T ) = T.stringof;
	enum String( alias T ) = T.stringof;
	enum SymbolOverride OverrideString( T ) = { false, T.stringof };
	enum TemplateOpen = "<";
	enum TemplateClose = ">";
	enum ConstOpen = "const ";
	enum ConstClose = "";
	enum NamespaceSeparator = "::";
}
//----------------------------------------------------------------------------

string FullTypeName( Symbol, alias StringProvider = DSymbolToStringProvider )()
{
	static if( StringProvider.OverrideString!Symbol.Overrides )
	{
		return StringProvider.OverrideString!Symbol.Name;
	}
	else static if( IsNonAssociativeArray!Symbol )
	{
		static if( IsStaticArray!Symbol )
		{
			import std.conv : to;
			enum ArrayString = "[" ~ StaticArrayLength!Symbol.to!string ~ "]";
		}
		else
		{
			enum ArrayString = "[]";
		}
		return FullTypeName!( ArrayValueType!Symbol, StringProvider ) ~ ArrayString;
	}
	else static if( is( Symbol A == const A ) )
	{
		return StringProvider.ConstOpen ~ FullTypeName!( A, StringProvider ) ~ StringProvider.ConstClose;
	}
	else static if( IsTemplatedType!Symbol )
	{
		string[] strTemplatedTypes;
		static foreach( Param; TemplateParamsOf!Symbol )
		{
			static if( is( Param ) )
			{
				strTemplatedTypes ~= FullTypeName!( Param, StringProvider );
			}
			else
			{
				strTemplatedTypes ~= __traits( identifier, Param );
			}
		}
		return FullTypeName!( __traits( parent, TemplateOf!( Symbol ) ), StringProvider ) ~ StringProvider.NamespaceSeparator ~ __traits( identifier, TemplateOf!( Symbol ) ) ~ StringProvider.TemplateOpen ~ strTemplatedTypes.joinWith( "," ) ~ StringProvider.TemplateClose;
	}
	else static if( __traits( compiles, __traits( parent, Symbol ) ) )
	{
		return FullTypeName!( __traits( parent, Symbol ), StringProvider ) ~ StringProvider.NamespaceSeparator ~ StringProvider.String!Symbol;
	}
	else
	{
		return StringProvider.String!Symbol;
	}
}
//----------------------------------------------------------------------------

string FullTypeName( alias Symbol, alias StringProvider = DSymbolToStringProvider )()
{
	import std.algorithm.searching : startsWith;
	import std.traits : isSomeFunction;

	static if( isSomeFunction!Symbol )
	{
		enum SymbolString = FullTypeName!( __traits( parent, Symbol ), StringProvider ) ~ __traits( identifier, Symbol );
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
		enum SymbolString = StringProvider.String!Symbol;
	}

	static if( __traits( compiles, __traits( parent, Symbol ) ) )
	{
		return FullTypeName!( __traits( parent, Symbol ), StringProvider ) ~ StringProvider.NamespaceSeparator ~ SymbolString;
	}
	else
	{
		return SymbolString;
	}
}
//----------------------------------------------------------------------------

string ModuleLocalTypeName( Symbol )() if( !IsUserType!Symbol )
{
	return Symbol.stringof;
}
//----------------------------------------------------------------------------

string ModuleLocalTypeName( alias Symbol )()
{
	import std.algorithm.searching : startsWith;

	static if( IsModule!Symbol || IsPackage!Symbol )
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
			static if( IsModule!( __traits( parent, Symbol ) )
					|| IsPackage!( __traits( parent, Symbol ) ) )
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

string ModuleName( T )() if( IsTemplatedType!T )
{
	return ModuleName!( TemplateOf!T );
}
//----------------------------------------------------------------------------

string ModuleName( T : PT*, PT )() if( IsTemplatedType!PT )
{
	return ModuleName!( TemplateOf!PT );
}
//----------------------------------------------------------------------------

string ModuleName( T : AT[], AT )()
{
	static if( IsTemplatedType!AT )
	{
		return ModuleName!( TemplateOf!AT );
	}
	else
	{
		return ModuleName!AT;
	}
}
//----------------------------------------------------------------------------

string ModuleName( alias Symbol )()
{
	static if( is( Symbol : KT[], KT ) )
	{
		return ModuleName!KT;
	}
	else static if( is( Symbol : PT*, PT ) )
	{
		return ModuleName!PT;
	}
	else
	{
		import std.algorithm.searching : startsWith;

		static if( IsModule!Symbol || IsPackage!Symbol )
		{
			return FullTypeName!Symbol;
		}
		else static if( __traits( compiles, __traits( parent, Symbol ) ) )
		{
			alias Parent = Alias!( __traits( parent, Symbol ) );

			static if( is( Symbol ) && IsTemplatedType!Symbol )
			{
				static if( IsModule!( __traits( parent, TemplateOf!( Symbol ) ) )
					|| IsPackage!( __traits( parent, TemplateOf!( Symbol ) ) ) )
				{
					return FullTypeName!( __traits( parent, TemplateOf!( Symbol ) ) );
				}
				else
				{
					return ModuleName!( __traits( parent, TemplateOf!( Symbol ) ) );
				}
				
			}
			else static if( IsModule!Parent || IsPackage!Parent )
			{
				return FullTypeName!( __traits( parent, Symbol ) );
			}
			else
			{
				return ModuleName!( __traits( parent, Symbol ) );
			}
		}
	}
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

	static if( IsNonAssociativeArray!T )
	{
		alias UT = Unqualified!( ArrayValueType!T );
	}
	else
	{
		alias UT = Unqualified!T;
	}

	import std.conv : to;
	static if( IsUserType!UT )
	{
		import std.traits;
		import binderoo.objectprivacy;

		addModule( ModuleName!UT );

		static if( IsTemplatedType!UT )
		{
			addModule( ModuleName!( TemplateOf!UT ) );

			foreach( Parameter; TemplateParamsOf!UT )
			{
				static if ( is( Parameter ) )
				{
					modules ~= gatherImports!Parameter;
				}
			}
		}

		foreach( member; __traits( allMembers, UT ) )
		{
			static if( IsAccessible!( UT, member ) && IsMemberVariable!( __traits( getMember, UT, member ) ) && IsUserType!( typeof( __traits( getMember, UT, member ) ) ) )
			{
				addModule( ModuleName!( typeof( __traits( getMember, UT, member ) ) ) );
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
