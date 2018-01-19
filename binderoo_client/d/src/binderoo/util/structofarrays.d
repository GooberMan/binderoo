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

module binderoo.util.structofarrays;
//----------------------------------------------------------------------------

import binderoo.traits;
//----------------------------------------------------------------------------

// The SOA template will rewrite the provided type to a new struct, with each
// member variable of the Descriptor represented by a dynamic array of the
// same type.
//----------------------------------------------------------------------------

struct SOA( alias Descriptor )
{
	mixin( GenerateSOAImplementation!( Descriptor )() );
}
//----------------------------------------------------------------------------

// Instead of writing a proper struct, you can also just plain provide variables
// as a string, ie SOA!( "int foo; int bar;" )
//----------------------------------------------------------------------------

template SOA( string variables )
{
	struct Impl
	{
		mixin( variables );
	}

	alias SOA = SOA!( Impl );
}
//----------------------------------------------------------------------------

// This ByMember iterator functionality will iterate over your array by member
// regardless of if it's a traditional array of structs, or one of our templated
// struct of arrays defined by the SOA template. As such, if you want your code
// to switch over one way or the other, using this ByMember iterator on all your
// things will allow you to switch implementations at will.
//
// For things that are already arrays, we need to define a new iterator for it.
//----------------------------------------------------------------------------

auto ByMember( string Member, T )( ref T[] obj )
{
	static if( IsUserType!T )
	{
		mixin( "import " ~ ModuleName!( T ) ~ ";" );
		mixin( "alias MemberType = typeof( T." ~ Member ~ " );" );
	}

	// Currently a full iterator that preserves original range.
	// Probably worth making it non-preservational.
	struct Iterator
	{
		private T[] range;

		@property save() const							{ return Iterator( range, iFront, iBack ); }
		@property length() const						{ return range.length; }
		@property bool empty() const					{ return range.length == 0; }

		void popFront()									{ ++iFront; }
		@property ref MemberType front() const			{ mixin( "return range[ 0 ]." ~ Member ~ ";" ); }

		void popBack()									{ --iBack; }
		@property ref MemberType back() const			{ mixin( "return range[ 0 ]." ~ Member ~ ";" ); }

		ref MemberType opIndex( size_t uIndex ) const	{ mixin( "return range[ uIndex ]." ~ Member ~ ";" ); }
		size_t opDollar() const							{ return range.length; }
		Iterator opSlice( size_t uBegin, size_t uEnd )	{ return Iterator( range[ uBegin .. uEnd ] ); }
	}

	return Iterator( obj );
}
//----------------------------------------------------------------------------

// Since the SOA template already has elements represented by slices, we simply
// return the member slice. Simple.
//----------------------------------------------------------------------------

auto ByMember( string Member, T : SOA!( U ), U )( ref T obj )
{
	mixin( "return obj." ~ Member ~ ";" );
}
//----------------------------------------------------------------------------

// IMPLEMENTATION FOLLOWS
//----------------------------------------------------------------------------

private string GenerateSOAImplementation( alias Descriptor )()
{
	string generateIterator()
	{
		string[] strOutput;
		strOutput ~= "private:";
		strOutput ~= "import " ~ ModuleName!( Descriptor ) ~ ";";
		strOutput ~= "alias StructType = " ~ ModuleLocalTypeName!( Descriptor ) ~ ";";
		strOutput ~= "alias SOAType = typeof( this );";
		strOutput ~= "struct Iterator";
		strOutput ~= "{";
		strOutput ~= "\tpackage SOAType* pBase = null;";
		strOutput ~= "\tpackage size_t iIndex = size_t.max;";
		strOutput ~= "";
		foreach( iIndex, member; Descriptor.init.tupleof )
		{
			static if( IsUserType!( typeof( Descriptor.tupleof[ iIndex ] ) ) )
			{
				enum ThisMemberType = ModuleLocalTypeName!( typeof( Descriptor.tupleof[ iIndex ] ) );
			}
			else
			{
				enum ThisMemberType = typeof( Descriptor.tupleof[ iIndex ] ).stringof;
			}
			enum ThisMemberName = __traits( identifier, Descriptor.tupleof[ iIndex ] );
			enum ThisMemberAccessor = "pBase." ~ ThisMemberName ~ "[ iIndex ]";

			strOutput ~= "\t@property ref " ~ ThisMemberType ~ " " ~ ThisMemberName ~ "() { return " ~ ThisMemberAccessor ~ "; }";
			strOutput ~= "\t@property ref " ~ ThisMemberType ~ " " ~ ThisMemberName ~ "( " ~ ThisMemberType ~ " val ) { " ~ ThisMemberAccessor ~ " = val; return " ~ ThisMemberAccessor ~ "; }";
			strOutput ~= "";
		}
		strOutput ~= "}";

		return strOutput.joinWith( "\t", "", "\n" );
	}

	string generateLengthProperties()
	{
		return "\t@property size_t length() const { return 0; }\n\t@property size_t length( size_t uNewLength ) { return uNewLength; }";
	}

	string generateOperators()
	{
		string[] strOutput;

		strOutput ~= "Iterator opIndex( size_t uIndex ) { return Iterator( &this, uIndex ); }";

		return strOutput.joinWith( "\t", "", "\n" );
	}

	string generateVariables()
	{
		string[] strOutput;

		foreach( iIndex, member; Descriptor.init.tupleof )
		{
			string[] strAllDecls;

			foreach( uda; __traits( getAttributes, Descriptor.tupleof[ iIndex ] ) )
			{
				strUDAs ~= uda.stringof;
			}

			static if( IsUserType!( typeof( Descriptor.tupleof[ iIndex ] ) ) )
			{
				enum TypeString = ModuleLocalTypeName!( typeof( Descriptor.tupleof[ iIndex ] ) );
			}
			else
			{
				enum TypeString = typeof( Descriptor.tupleof[ iIndex ] ).stringof;
			}

			strAllDecls ~= TypeString ~ "[]";
			strAllDecls ~= __traits( identifier, Descriptor.tupleof[ iIndex ] );

			strOutput ~= strAllDecls.joinWith( " " );
		}

		return strOutput.joinWith( "\t", ";", "\n" );
	}

	string generateAllInternals()
	{
		return generateIterator()
				~ "\n\npublic:\n"
				~ generateVariables()
				~ "\n\n"
				~ generateLengthProperties()
				~ "\n\n"
				~ generateOperators();
	}

	//pragma( msg, generateAllInternals() );

	return generateAllInternals();
}
//----------------------------------------------------------------------------


//============================================================================
