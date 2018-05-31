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

module binderoo.util.atomic;
//----------------------------------------------------------------------------

// Atomic is directly analagous to std::atomic in the C++ Standard Library and
// adheres to the standard functionality defined with it.

import binderoo.traits;
//----------------------------------------------------------------------------

template AtomicStorageOf( T )
{
	static if( is( T U == enum ) )
	{
		alias IntegralType = IntegralTypeOf!U;
	}
	else
	{
		alias IntegralType = IntegralTypeOf!T;
	}

	static assert( !is( IntegralType == void ), T.stringof ~ " cannot be used as an atomic variable. Please use basic types only." );
	static assert( IntegralType.sizeof <= 8, T.stringof ~ " cannot be used as an atomic variable. Please use basic types only." );

	static if( IntegralType.sizeof < 4 )
	{
		static if( IsUnsigned!IntegralType )
		{
			alias AtomicStorageOf = uint;
		}
		else
		{
			alias AtomicStorageOf = int;
		}
	}
	else
	{
		alias AtomicStorageOf = IntegralType;
	}
}
//----------------------------------------------------------------------------

struct Atomic( T ) if( IsIntegral!( IntegralTypeOf!T ) )
{
	import core.atomic;

	this( T primer )
	{
		m_val = primer;
	}
	//------------------------------------------------------------------------

	// C++ std::atomic API

	pragma( inline ) T exchange( const( T ) val )									{ T prev = load(); store( val ); return prev; }
	pragma( inline ) T load( ) const												{ return cast( T )atomicLoad( m_val ); }
	pragma( inline ) void store( const( T ) val )									{ atomicStore( m_val, cast( Storage )val ); }
	//------------------------------------------------------------------------

	pragma( inline ) bool compare_exchange_strong( const( T ) val )					{ return cas( &m_val, cast( Storage )load(), cast( Storage )val ); }
	//------------------------------------------------------------------------

	alias fetch_add = fetch!"+";
	alias fetch_sub = fetch!"-";
	alias fetch_and = fetch!"&";
	alias fetch_or = fetch!"|";
	alias fetch_xor = fetch!"^";
	//------------------------------------------------------------------------

	// Convenience accessors
	pragma( inline ) T opCast( CastType : T )()										{ return load(); }
	pragma( inline ) T opAssign( const( T ) val )									{ store( val ); return val; }
	//------------------------------------------------------------------------

	pragma( inline ) T opOpAssign( string op )( const( T ) val ) if( op == "+" )	{ return fetch_add( val ) + val; }
	pragma( inline ) T opOpAssign( string op )( const( T ) val ) if( op == "-" )	{ return fetch_sub( val ) - val; }
	pragma( inline ) T opOpAssign( string op )( const( T ) val ) if( op == "&" )	{ return fetch_and( val ) & val; }
	pragma( inline ) T opOpAssign( string op )( const( T ) val ) if( op == "|" )	{ return fetch_or( val ) | val; }
	pragma( inline ) T opOpAssign( string op )( const( T ) val ) if( op == "^" )	{ return fetch_xor( val ) ^ val; }
	//------------------------------------------------------------------------

	pragma( inline ) T opUnary( string op )( ) if( op == "++" )						{ return fetch_add( 1 ) + 1; }
	pragma( inline ) T opUnary( string op )( ) if( op == "--" )						{ return fetch_sub( 1 ) - 1; }

	pragma( inline ) T fetch( string op )( const( T ) val )							{ return cast( T )atomicOp!op( m_val, cast( Storage )val ); }
	//------------------------------------------------------------------------

	alias Storage = AtomicStorageOf!T;
	private shared Storage m_val;

}
//----------------------------------------------------------------------------

//============================================================================
