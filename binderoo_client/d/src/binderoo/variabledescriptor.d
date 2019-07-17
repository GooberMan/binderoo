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

module binderoo.variabledescriptor;
//----------------------------------------------------------------------------

public import binderoo.descriptorsearch;
public import binderoo.objectprivacy;
public import binderoo.traits;
public import binderoo.typedescriptor;
public import binderoo.objectprivacy;
public import std.typetuple;
//----------------------------------------------------------------------------

// This wrapper is designed to be used with Type.tupleof iterators
struct VariableDescriptor( alias Variable )
{
	template HasUDA( Attr )
	{
		static if( is( ET == void ) )
		{
			enum HasUDA = false;
		}
		else
		{
			enum HasUDA = binderoo.traits.HasUDA!( Variable, Attr );
		}
	}
	//------------------------------------------------------------------------

	template GetUDA( Attr )
	{
		static if( is( ET == void ) )
		{
			alias GetUDA = void;
		}
		else
		{
			alias GetUDA = binderoo.traits.GetUDA!( Variable, Attr );
		}
	}
	//------------------------------------------------------------------------

	private alias T = binderoo.traits.Alias!( __traits( parent, Variable ) );
	private alias ET = typeof( Variable );
	private enum objectName = __traits( identifier, Variable );

	alias			UDAs							= TypeTuple!( __traits( getAttributes, Variable ) );
	//------------------------------------------------------------------------

	// The struct/class that contains the element we're interested in.
	alias			BaseType						= TypeDescriptor!( T, false );

	// The type of the element. This is what we're actually going to serialize, so we need deep knowledge of it.
	alias			ElementType						= TypeDescriptor!( ET, false );

	// If you pass in an object name, we can inspect BaseType using the name.
	enum string		ElementName						= objectName;

	// Caching the size of the element type
	enum			ElementSize						= ElementType.Size;
	//------------------------------------------------------------------------

	alias			Type							= ElementType;
	alias			Name							= ElementName;
	//------------------------------------------------------------------------

	enum			ModuleName						= binderoo.traits.ModuleName!( T );
	enum			FullyQualifiedName				= binderoo.traits.FullTypeName!( T ) ~ "." ~ ElementName;
	enum			BindingFullName					= binderoo.binding.attributes.BindingFullName!T ~ "." ~ ElementName;
	//------------------------------------------------------------------------

	enum			IsStatic						= binderoo.traits.IsStaticMember!( T, Name );
	enum			IsMutable						= binderoo.traits.IsMutable!( T );
	enum			PrivacyLevel					= PrivacyOf!Variable;
	//------------------------------------------------------------------------

	// This class has also been designed to allow you to use it without a containing type, so if you want to know if a type can be serialized at all you use VariableDescriptor!( void, <your type>, null )
	static if( __traits( compiles, Variable.offsetof ) )
	{
		// If we have a valid element name, then we'll go ahead and get some useful information from it.
		enum		IsElementNameVoid				= false;

		// The offset of this element in the struct/class. Can be useful (especially for OSP work)
		enum		ElementOffset					= Variable.offsetof;

		static foreach( iIndex, thisVar; T.tupleof )
		{
			static if( thisVar.offsetof == Variable.offsetof )
			{
				enum TupleIndex						= iIndex;
			}
		}
	}
	else
	{
		// Defaults if we don't have an element name. Notice the offset is -1. This is your biggest clue that it's not a member variable.
		enum		IsElementNameVoid				= true;
		enum		ElementOffset					= -1;
		enum		TupleIndex						= -1;
	}

	// And finally, by using an instance of this struct you can pass a ref to the base type and get the actual member you're after. But only if we have a base type.
	static if( !IsElementNameVoid )
	{
		static ref auto get( ref T obj )
		{
			return obj.tupleof[ TupleIndex ];
		}

		static if( IsMutable )
		{
			static void set( ref T obj, ref ET val )
			{
				obj.tupleof[ TupleIndex ] = val;
			}
		}
	}
}
//----------------------------------------------------------------------------

version( NotDeprecated ) :

template VariableDescriptors( T )
{
	public import std.traits;
	public import std.typetuple;
	alias Tuple = std.typetuple.TypeTuple;

	string gatherImportsForMixin()
	{
		string strOutput;

		static if( IsUserType!( T ) )
		{
			import std.algorithm;

			string[] modules;

			modules ~= binderoo.traits.ModuleName!( T );

			foreach( iIndex, member; T.init.tupleof )
			{
				alias ThisType = typeof( T.tupleof[ iIndex ] );
				static if( IsUserType!( ThisType ) )
				{
					enum ThisModuleName = binderoo.traits.ModuleName!( binderoo.traits.PointerTarget!ThisType );
					if( !modules.canFind( ThisModuleName ) )
					{
						modules ~= ThisModuleName;
					}
				}
			}

			strOutput = modules.joinWith( "import ", ";", "\n" );
		}
		return strOutput;
	}

	string gatherMembersForMixin()
	{
		string[] strOutputs;

		static if( IsUserType!( T ) )
		{
			import std.traits;

			foreach( iIndex, member; T.init.tupleof )
			{
				alias ThisType = typeof( T.tupleof[ iIndex ] );
				strOutputs ~= "VariableDescriptor!( T, " ~ ThisType.stringof ~ ", \"" ~ __traits( identifier, T.tupleof[ iIndex ] ) ~ "\" )";
			}
		}

		return strOutputs.joinWith( ",\n" );
	}

	static if( __traits( compiles, __traits( allMembers, T ) ) )
	{
		enum ImportDeclarations = gatherImportsForMixin();
		enum VariableDescriptorTypes = gatherMembersForMixin();

		mixin( ImportDeclarations );
		mixin( "alias VariableDescriptors = Tuple!( " ~ VariableDescriptorTypes ~ " );" );
	}
	else
	{
		alias VariableDescriptors = Tuple!( );
	}
}
//----------------------------------------------------------------------------

template VariableDescriptorsByUDA( T, UDAs... )
{
	alias VariableDescriptorsByUDA = DescriptorsByUDA!( T, VariableDescriptors, UDAs );
}
//----------------------------------------------------------------------------

//============================================================================
