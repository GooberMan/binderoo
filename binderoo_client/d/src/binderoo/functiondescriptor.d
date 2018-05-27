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

module binderoo.functiondescriptor;

public import std.traits;
public import std.typetuple;

public import binderoo.descriptorsearch;
public import binderoo.objectprivacy;
public import binderoo.traits;
public import binderoo.typedescriptor;
//----------------------------------------------------------------------------

version = FunctionDescriptorMSVC;

enum CollectionOrder
{
	Normal,
	OverridesReversed,
	AllReversed,
}

version( FunctionDescriptorMSVC ) { enum DefaultCollectionOrder = CollectionOrder.OverridesReversed; }
else { enum DefaultCollectionOrder = CollectionOrder.Normal; }

//version = FunctionDescriptorOutputDebug;
//----------------------------------------------------------------------------

struct FunctionDescriptor( alias symbol, size_t iOverloadIndex = 0 )
{
	mixin( "import " ~ binderoo.traits.ModuleName!symbol ~ ";" );

	template HasUDA( Attr )
	{
		static if( is( typeof( symbol ) == void ) )
		{
			enum HasUDA = false;
		}
		else
		{
			enum bool HasUDA = binderoo.traits.HasUDA!( symbol, Attr );
		}
	}
	//------------------------------------------------------------------------

	template GetUDA( Attr )
	{
		static if( HasUDA!( Attr ) )
		{
			enum GetUDA = binderoo.traits.GetUDA!( symbol, Attr );
		}
		else
		{
			alias GetUDA = void;
		}
	}
	//------------------------------------------------------------------------

	alias			UDAs							= binderoo.traits.AliasSeq!( __traits( getAttributes, symbol ) );
	//------------------------------------------------------------------------

	enum FunctionAttribute : uint
	{
		Invalid				= 0b0000000000000000,
		Final				= 0b0000000000000001,
		Virtual				= 0b0000000000000010,
		Abstract			= 0b0000000000000100,
		Override			= 0b0000000000001000,
		Static				= 0b0000000000010000,
		Const				= 0b0000000000100000,
		Immutable			= 0b0000000001000000,
		Property			= 0b0000000010000000,
		ReturnsRef			= 0b0000000100000000,
	}

	enum 					FunctionAttributes		= ( __traits( isFinalFunction, symbol )														? FunctionAttribute.Final		: FunctionAttribute.Invalid )
													| ( __traits( isVirtualMethod, symbol )														? FunctionAttribute.Virtual		: FunctionAttribute.Invalid )
													| ( __traits( isAbstractFunction, symbol )													? FunctionAttribute.Abstract	: FunctionAttribute.Invalid )
													| ( __traits( isOverrideFunction, symbol )													? FunctionAttribute.Override	: FunctionAttribute.Invalid )
													| ( __traits( isStaticFunction, symbol )													? FunctionAttribute.Static		: FunctionAttribute.Invalid )
													| ( ( std.traits.functionAttributes!( symbol ) & std.traits.FunctionAttribute.const_ )		? FunctionAttribute.Const		: FunctionAttribute.Invalid )
													| ( ( std.traits.functionAttributes!( symbol ) & std.traits.FunctionAttribute.immutable_ )	? FunctionAttribute.Immutable	: FunctionAttribute.Invalid )
													| ( ( std.traits.functionAttributes!( symbol ) & std.traits.FunctionAttribute.property )	? FunctionAttribute.Property	: FunctionAttribute.Invalid )
													| ( ( std.traits.functionAttributes!( symbol ) & std.traits.FunctionAttribute.ref_ )		? FunctionAttribute.ReturnsRef	: FunctionAttribute.Invalid )
													;

	enum					IsFinal					= ( FunctionAttributes & FunctionAttribute.Final ) != FunctionAttribute.Invalid;
	enum					IsVirtual				= ( FunctionAttributes & FunctionAttribute.Virtual ) != FunctionAttribute.Invalid;
	enum					IsAbstract				= ( FunctionAttributes & FunctionAttribute.Abstract ) != FunctionAttribute.Invalid;
	enum					IsOverride				= ( FunctionAttributes & FunctionAttribute.Override ) != FunctionAttribute.Invalid;
	enum					IsStatic				= ( FunctionAttributes & FunctionAttribute.Static ) != FunctionAttribute.Invalid;
	enum					IsConst					= ( FunctionAttributes & FunctionAttribute.Const ) != FunctionAttribute.Invalid;
	enum					IsImmutable				= ( FunctionAttributes & FunctionAttribute.Immutable ) != FunctionAttribute.Invalid;
	enum					IsProperty				= ( FunctionAttributes & FunctionAttribute.Property ) != FunctionAttribute.Invalid;
	enum					ReturnsRef				= ( FunctionAttributes & FunctionAttribute.ReturnsRef ) != FunctionAttribute.Invalid;
	//------------------------------------------------------------------------

	enum LinkageType : string
	{
		D					= "D",
		C					= "C",
		CPlusPlus			= "C++",
		Windows				= "Windows",
		Pascal				= "Pascal",
	}

	enum					Linkage					= cast( LinkageType )std.traits.functionLinkage!( symbol );

	enum					IsDFunction				= Linkage == LinkageType.D;
	enum					IsCFunction				= Linkage == LinkageType.C;
	enum					IsCPlusPlusFunction		= Linkage == LinkageType.CPlusPlus;
	enum					IsWindowsFunction		= Linkage == LinkageType.Windows;
	enum					IsPascalFunction		= Linkage == LinkageType.Pascal;
	//------------------------------------------------------------------------

	// The struct/class that contains the element we're interested in.
	alias 					ObjectType				= void;
	alias					Parent					= binderoo.traits.Alias!( __traits( parent, symbol ) );
	//------------------------------------------------------------------------

	enum					IsMemberFunction		= false;
	enum					IsImplementedInType		= true;

	// The type of the function.
	alias					FunctionType			= FunctionTypeOf!( symbol );
	alias					FunctionPointerType		= PointerOf!( FunctionType );

	// Direct representation of the function itself.
	enum					FunctionMangle			= symbol.mangleof;

	// The name of the function. We can use this to call the thing.
	enum string				FunctionName			= __traits( identifier, symbol );

	enum					OverloadIndex			= iOverloadIndex;
	//------------------------------------------------------------------------

	alias					Type					= FunctionType;
	alias					Name					= FunctionName;
	alias					Symbol					= binderoo.traits.Alias!( symbol );
	//------------------------------------------------------------------------

	enum					ModuleName				= binderoo.traits.ModuleName!( symbol );
	enum					FullyQualifiedName		= FullTypeName!( __traits( parent, symbol ) ) ~ "." ~ Name; //FullTypeName!( symbol );
	//------------------------------------------------------------------------

	private alias			ReturnDescriptor		= TypeDescriptor!( std.traits.ReturnType!( symbol ), ReturnsRef );

	// Function details. First, the return type.
	alias					ReturnType				= ReturnDescriptor.Type;

	// The raw type if you're in to that kind of thing.
	alias					UnqualifiedReturnType	= ReturnDescriptor.UnqualifiedType;

	// Easy bool check. Oh my.
	enum					HasReturnType			= !is( UnqualifiedReturnType == void );
	//------------------------------------------------------------------------

	// Before you go poking in to parameters, check if the count is greater than 0.
	enum uint				ParameterCount			= std.traits.Parameters!( symbol ).length;

	// And now, a templated parameter accessor.
	struct Parameter( uint iIndex )
	{
		static assert( iIndex < ParameterCount, "Parameter index out of bounds." );

		private alias Identifiers = std.traits.ParameterIdentifierTuple!( symbol );
		private alias Types = std.traits.Parameters!( symbol );
		private alias Storages = std.traits.ParameterStorageClassTuple!( symbol );

		alias				Descriptor				= TypeDescriptor!( Types[ iIndex ], Storages[ iIndex ] == ParameterStorageClass.ref_ );

		enum uint			Index					= iIndex;
		enum string			Name					= Identifiers[ iIndex ];
		alias 				Type					= Descriptor.Type;
		alias				UnqualifiedType			= Descriptor.UnqualifiedType;
		enum				IsConst					= Descriptor.IsConst;
		enum				IsRef					= Descriptor.IsRef;
		enum				IsPointer				= Descriptor.IsPointer;
		enum				IsArray					= Descriptor.IsArray;
	}
	//------------------------------------------------------------------------

	private template ParameterTuple( uint uIndex )
	{
		alias Tuple = std.typetuple.TypeTuple;

		static if( uIndex < ParameterCount )
		{
			alias ParameterTuple = Tuple!( Parameter!( uIndex ), ParameterTuple!( uIndex + 1 ) );
		}
		else
		{
			alias ParameterTuple = Tuple!( );
		}
	}
	//------------------------------------------------------------------------

	alias					ParametersAsTuple		= ParameterTuple!( 0 );
	//------------------------------------------------------------------------

	enum					PrivacyLevel			= PrivacyOf!( symbol );

	enum					IsPublic				= PrivacyLevel == PrivacyLevel.Public;
	enum					IsPrivate				= PrivacyLevel == PrivacyLevel.Private;
	enum					IsProtected				= PrivacyLevel == PrivacyLevel.Protected;
	enum					IsExport				= PrivacyLevel == PrivacyLevel.Export;
	enum					IsPackage				= PrivacyLevel == PrivacyLevel.Package;

	enum					CanCallExternally		= IsExternallyAccessible!( symbol );
	//------------------------------------------------------------------------

	static ReturnDescriptor.Type invoke( Params... )( Params params )
	{
		static assert( Params.length == ParameterCount, "Invalid parameter count for " ~ T.stringof ~ "." ~ FunctionName );

		return FunctionSymbol( params );
	}
}
//----------------------------------------------------------------------------

struct FunctionDescriptor( T, string symbolName, size_t iSymbolIndex )
{
	mixin( "import " ~ binderoo.traits.ModuleName!T ~ ";" );

	template HasUDA( Attr )
	{
		static if( is( typeof( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] ) == void ) )
		{
			enum HasUDA = false;
		}
		else
		{
			enum bool HasUDA = binderoo.traits.HasUDA!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ], Attr );
		}
	}
	//------------------------------------------------------------------------

	template GetUDA( Attr )
	{
		static if( HasUDA!( Attr ) )
		{
			enum GetUDA = binderoo.traits.GetUDA!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ], Attr );
		}
		else
		{
			alias GetUDA = void;
		}
	}
	//------------------------------------------------------------------------

	alias			UDAs							= binderoo.traits.AliasSeq!( __traits( getAttributes, __traits( getOverloads, T, symbolName )[ iSymbolIndex ] ) );
	//------------------------------------------------------------------------

	enum FunctionAttribute : uint
	{
		Invalid				= 0b0000000000000000,
		Final				= 0b0000000000000001,
		Virtual				= 0b0000000000000010,
		Abstract			= 0b0000000000000100,
		Override			= 0b0000000000001000,
		Static				= 0b0000000000010000,
		Const				= 0b0000000000100000,
		Immutable			= 0b0000000001000000,
		Property			= 0b0000000010000000,
		ReturnsRef			= 0b0000000100000000,
	}

	enum 					FunctionAttributes		= ( __traits( isFinalFunction, __traits( getOverloads, T, symbolName )[ iSymbolIndex ] )														? FunctionAttribute.Final		: FunctionAttribute.Invalid )
													| ( __traits( isVirtualMethod, __traits( getOverloads, T, symbolName )[ iSymbolIndex ] )														? FunctionAttribute.Virtual		: FunctionAttribute.Invalid )
													| ( __traits( isAbstractFunction, __traits( getOverloads, T, symbolName )[ iSymbolIndex ] )													? FunctionAttribute.Abstract	: FunctionAttribute.Invalid )
													| ( __traits( isOverrideFunction, __traits( getOverloads, T, symbolName )[ iSymbolIndex ] )													? FunctionAttribute.Override	: FunctionAttribute.Invalid )
													| ( __traits( isStaticFunction, __traits( getOverloads, T, symbolName )[ iSymbolIndex ] )														? FunctionAttribute.Static		: FunctionAttribute.Invalid )
													| ( ( std.traits.functionAttributes!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] ) & std.traits.FunctionAttribute.const_ )		? FunctionAttribute.Const		: FunctionAttribute.Invalid )
													| ( ( std.traits.functionAttributes!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] ) & std.traits.FunctionAttribute.immutable_ )	? FunctionAttribute.Immutable	: FunctionAttribute.Invalid )
													| ( ( std.traits.functionAttributes!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] ) & std.traits.FunctionAttribute.property )		? FunctionAttribute.Property	: FunctionAttribute.Invalid )
													| ( ( std.traits.functionAttributes!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] ) & std.traits.FunctionAttribute.ref_ )			? FunctionAttribute.ReturnsRef	: FunctionAttribute.Invalid )
													;

	enum					IsFinal					= ( FunctionAttributes & FunctionAttribute.Final ) != FunctionAttribute.Invalid;
	enum					IsVirtual				= ( FunctionAttributes & FunctionAttribute.Virtual ) != FunctionAttribute.Invalid;
	enum					IsAbstract				= ( FunctionAttributes & FunctionAttribute.Abstract ) != FunctionAttribute.Invalid;
	enum					IsOverride				= ( FunctionAttributes & FunctionAttribute.Override ) != FunctionAttribute.Invalid;
	enum					IsStatic				= ( FunctionAttributes & FunctionAttribute.Static ) != FunctionAttribute.Invalid;
	enum					IsConst					= ( FunctionAttributes & FunctionAttribute.Const ) != FunctionAttribute.Invalid;
	enum					IsImmutable				= ( FunctionAttributes & FunctionAttribute.Immutable ) != FunctionAttribute.Invalid;
	enum					IsProperty				= ( FunctionAttributes & FunctionAttribute.Property ) != FunctionAttribute.Invalid;
	enum					ReturnsRef				= ( FunctionAttributes & FunctionAttribute.ReturnsRef ) != FunctionAttribute.Invalid;
	//------------------------------------------------------------------------

	enum LinkageType : string
	{
		D					= "D",
		C					= "C",
		CPlusPlus			= "C++",
		Windows				= "Windows",
		Pascal				= "Pascal",
	}

	enum					Linkage					= cast( LinkageType )std.traits.functionLinkage!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] );

	enum					IsDFunction				= Linkage == LinkageType.D;
	enum					IsCFunction				= Linkage == LinkageType.C;
	enum					IsCPlusPlusFunction		= Linkage == LinkageType.CPlusPlus;
	enum					IsWindowsFunction		= Linkage == LinkageType.Windows;
	enum					IsPascalFunction		= Linkage == LinkageType.Pascal;
	//------------------------------------------------------------------------

	// The struct/class that contains the element we're interested in.
	alias 					ObjectType				= T;
	alias					Parent					= binderoo.traits.Alias!( __traits( parent, __traits( getOverloads, T, symbolName )[ iSymbolIndex ] ) );
	//------------------------------------------------------------------------

	enum					IsMemberFunction		= !is( T == void );
	static if( IsMemberFunction )
	{
		enum				IsImplementedInType		= is( Parent == T );
	}
	else
	{
		enum				IsImplementedInType		= false;
	}

	// The type of the function.
	alias					FunctionType			= FunctionTypeOf!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] );
	alias					FunctionPointerType		= PointerOf!( FunctionType );

	// Direct representation of the function itself.
	enum					FunctionMangle			= __traits( getMember, T, symbolName ).mangleof;

	// The name of the function. We can use this to call the thing.
	enum string				FunctionName			= symbolName;

	enum					OverloadIndex			= iSymbolIndex;
	//------------------------------------------------------------------------

	alias					Type					= FunctionType;
	alias					Name					= FunctionName;
	alias					Symbol					= binderoo.traits.Alias!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] );
	//------------------------------------------------------------------------

	enum					ModuleName				= binderoo.traits.ModuleName!( T );
	enum					FullyQualifiedName		= FullTypeName!( T ) ~ "." ~ Name; //FullTypeName!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] );
	//------------------------------------------------------------------------

	private alias			ReturnDescriptor		= TypeDescriptor!( std.traits.ReturnType!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] ), ReturnsRef );

	// Function details. First, the return type.
	alias					ReturnType				= ReturnDescriptor.Type;

	// The raw type if you're in to that kind of thing.
	alias					UnqualifiedReturnType	= ReturnDescriptor.UnqualifiedType;

	// Easy bool check. Oh my.
	enum					HasReturnType			= !is( UnqualifiedReturnType == void );
	//------------------------------------------------------------------------

	// Before you go poking in to parameters, check if the count is greater than 0.
	enum uint				ParameterCount			= std.traits.Parameters!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] ).length;

	// And now, a templated parameter accessor.
	struct Parameter( uint iIndex )
	{
		static assert( iIndex < ParameterCount, "Parameter index out of bounds." );

		private alias Identifiers = std.traits.ParameterIdentifierTuple!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] );
		private alias Types = std.traits.Parameters!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] );
		private alias Storages = std.traits.ParameterStorageClassTuple!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] );

		alias				Descriptor				= TypeDescriptor!( Types[ iIndex ], Storages[ iIndex ] == ParameterStorageClass.ref_ );

		enum uint			Index					= iIndex;
		enum string			Name					= Identifiers[ iIndex ];
		alias 				Type					= Descriptor.Type;
		alias				UnqualifiedType			= Descriptor.UnqualifiedType;
		enum				IsConst					= Descriptor.IsConst;
		enum				IsRef					= Descriptor.IsRef;
		enum				IsPointer				= Descriptor.IsPointer;
		enum				IsArray					= Descriptor.IsArray;

	}
	//------------------------------------------------------------------------

	private template ParameterTuple( uint uIndex )
	{
		alias Tuple = std.typetuple.TypeTuple;

		static if( uIndex < ParameterCount )
		{
			alias ParameterTuple = Tuple!( Parameter!( uIndex ), ParameterTuple!( uIndex + 1 ) );
		}
		else
		{
			alias ParameterTuple = Tuple!( );
		}
	}
	//------------------------------------------------------------------------

	alias					ParametersAsTuple		= ParameterTuple!( 0 );
	//------------------------------------------------------------------------

	enum					PrivacyLevel			= PrivacyOf!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] );

	enum					IsPublic				= PrivacyLevel == PrivacyLevel.Public;
	enum					IsPrivate				= PrivacyLevel == PrivacyLevel.Private;
	enum					IsProtected				= PrivacyLevel == PrivacyLevel.Protected;
	enum					IsExport				= PrivacyLevel == PrivacyLevel.Export;
	enum					IsPackage				= PrivacyLevel == PrivacyLevel.Package;

	enum					CanCallExternally		= IsExternallyAccessible!( __traits( getOverloads, T, symbolName )[ iSymbolIndex ] );
	//------------------------------------------------------------------------

	static ReturnDescriptor.Type invoke( Params... )( ref T obj, Params params )
	{
		static assert( Params.length == ParameterCount, "Invalid parameter count for " ~ T.stringof ~ "." ~ FunctionName );

		static if( HasReturnType )
		{
			mixin( "return obj." ~ FunctionName ~ "( params );");
		}
		else
		{
			mixin( "obj." ~ FunctionName ~ "( params );" );
		}
	}
}
//----------------------------------------------------------------------------

template FunctionDescriptors( T, CollectionOrder Order = DefaultCollectionOrder )
{
	alias Tuple = std.typetuple.TypeTuple;

	private template isMemberFunction( T, alias name )
	{
		static if( IsAccessible!( T, name ) )
		{
			enum isMemberFunction = __traits( getOverloads, T, name ).length > 0;
		}
		else
		{
			enum isMemberFunction = false;
		}
	}
	//------------------------------------------------------------------------

	string gatherImportsForMixin( Type )()
	{
		static if( IsUserType!( Type ) )
		{
			import std.traits;

			string output = "import " ~ moduleName!( Type ) ~ ";\n";
			
			static if( IsTemplatedType!( Type ) )
			{
				foreach( NewType; TemplateParametersOf!( Type ) )
				{
					static if ( is( NewType ) )
					{
						output ~= gatherImportsForMixin!( NewType )();
					}
				}
			}
			return output;
		}
		else
		{
			return "";
		}
	}
	//------------------------------------------------------------------------

	string generateDescriptorsForMixin( alias name )()
	{
		import std.conv;

		alias Overloads = Tuple!( __traits( getOverloads, T, name ) );

		string[] strOutputs;

		static if( Order == CollectionOrder.OverridesReversed || Order == CollectionOrder.AllReversed )
		{
			foreach_reverse( iIndex, Overload; Overloads )
			{
				strOutputs ~= "FunctionDescriptor!( " ~ FullyQualifiedName!( T ) ~ ", \"" ~ name ~ "\", " ~ to!string( iIndex ) ~ " )";
			}
		}
		else
		{
			foreach( iIndex, Overload; Overloads )
			{
				strOutputs ~= "FunctionDescriptor!( " ~ FullyQualifiedName!( T ) ~ ", \"" ~ name ~ "\", " ~ to!string( iIndex ) ~ " )";
			}
		}

		return strOutputs.joinWith( ", " );
	}
	//------------------------------------------------------------------------

	string gatherForMixin()
	{
		string[] strOutputs;

		static if( IsUserType!( T ) )
		{
			static if( Order == CollectionOrder.AllReversed )
			{
				foreach_reverse( Member; __traits( allMembers, T ) )
				{
					static if( isMemberFunction!( T, Member ) )
					{
						strOutputs ~= generateDescriptorsForMixin!( Member )();
					}
				}
			}
			else
			{
				foreach( Member; __traits( allMembers, T ) )
				{
					static if( isMemberFunction!( T, Member ) )
					{
						strOutputs ~= generateDescriptorsForMixin!( Member )();
					}
				}
			}
		}

		return strOutputs.joinWith( ", " );
	}
	//------------------------------------------------------------------------


	static if( __traits( compiles, __traits( allMembers, T ) ) )
	{
		enum DescriptorsString = "alias FunctionDescriptors = Tuple!( " ~ gatherForMixin() ~ " );";
		enum ModuleString = gatherImportsForMixin!( T )();
//		pragma( msg, ModuleString );
//		pragma( msg, DescriptorsString );

		mixin( ModuleString );
		mixin( DescriptorsString );

		version( FunctionDescriptorOutputDebug ) pragma( msg, "Function descriptors: found " ~ FunctionDescriptors.length.stringof ~ " functions in " ~ T.stringof ~ "." );
	}
	else
	{
		alias FunctionDescriptors = Tuple!( );
	}
}
//----------------------------------------------------------------------------

template FunctionDescriptorsByUDA( T, UDAs... )
{
	alias FunctionDescriptorsByUDA = DescriptorsByUDA!( T, FunctionDescriptors, UDAs );
}
//----------------------------------------------------------------------------

bool IsFunctionDescriptor( T )() { return false; }
bool IsFunctionDescriptor( Desc : FunctionDescriptor!( T, symbolName, iIndex ), T, string symbolName, uint iIndex )() { return true; }
bool IsFunctionDescriptor( Desc : FunctionDescriptor!( symbolName, iIndex ), alias symbolName, uint iIndex )() { return true; }
//----------------------------------------------------------------------------

struct FunctionString( Desc ) if( IsFunctionDescriptor!( Desc )() )
{
	alias TypeDescriptor								= Desc;
	alias ReturnDescriptor								= Desc.ReturnDescriptor;

	// Signature that we can parse, aw yis.
	enum string				DSignature					= TypeDescriptor.FunctionType.stringof;
	enum string				CSignature					= generateCSignature();

	enum string				ParameterNames				= generateDParameterString( ParameterInfo.Names );
	enum string				ParameterDeclarations		= generateDParameterString( ParameterInfo.Names | ParameterInfo.Types );
	enum string				ParameterTypes				= generateDParameterString( ParameterInfo.Types );

	private enum string		Linkage						= !Desc.IsDFunction ? "extern( " ~ Desc.Linkage ~ " ) " : "";

	enum string				DDeclNoLinkage				= generateFullyQualifiedName!( "FullyQualifiedDDecl" )();
	enum string				DDeclNoLinkageRename( alias to ) = generateFullyQualifiedName!( "FullyQualifiedDDecl", to )();
	enum string				DDeclNoLinkagePrependName( alias pre ) = generateFullyQualifiedName!( "FullyQualifiedDDecl", pre ~ TypeDescriptor.FunctionName )();

	enum string				DDecl						= Linkage ~ DDeclNoLinkage;
	enum string				CDecl						= generateFullyQualifiedName!( "CDecl" )();
	enum string				CSharpDecl					= generateFullyQualifiedName!( "CSharpDecl" )();
	enum string				CSharpMarshalledDecl		= generateFullyQualifiedName!( "CSharpMarshalledDecl" )();
	enum string				DCall						= generateDCall();
	//------------------------------------------------------------------------

	struct Parameter( uint iIndex )
	{
		alias				BaseParam					= TypeDescriptor.Parameter!( iIndex );

		alias				Name						= BaseParam.Name;

		package enum		DDecl						= TypeString!( BaseParam.Descriptor ).DDecl ~ " " ~ BaseParam.Name;
		package enum		FullyQualifiedDDecl			= TypeString!( BaseParam.Descriptor ).FullyQualifiedDDecl ~ " " ~ BaseParam.Name;
		package enum		CDecl						= TypeString!( BaseParam.Descriptor ).CDecl ~ ( BaseParam.Descriptor.IsClass ? "* " : " " ) ~ BaseParam.Name;
		package enum		CSharpDecl					= TypeString!( BaseParam.Descriptor ).CSharpDecl ~ " " ~ BaseParam.Name;
		package enum		CSharpMarshalledDecl		= ( BaseParam.Descriptor.IsClass ? "IntPtr" : BaseParam.Descriptor.IsArray ? "SliceData" : TypeString!( BaseParam.Descriptor ).CSharpDecl ) ~ " " ~ BaseParam.Name;
	}
	//------------------------------------------------------------------------

	private static string generateCSignature()
	{
		enum ConstString = Desc.IsConst ? " const" : "";
		string generateParameterString()
		{
			string[] parameterTypes;
			foreach( Parameter; TypeDescriptor.ParametersAsTuple )
			{
				parameterTypes ~= TypeString!( Parameter.Descriptor ).CDecl ~ ( Parameter.Descriptor.IsClass ? "*" : "" );
			}
			return parameterTypes.joinWith( ", " );
		}

		static if( Desc.IsStatic || !Desc.IsMemberFunction )
		{
			string strPtrType = "(*)";
		}
		else
		{
			string strPtrType = "(" ~ TypeString!( Desc.ObjectType ).CDecl ~ "::*)";
		}

		return TypeString!( ReturnDescriptor ).CDecl ~ strPtrType ~ "(" ~ generateParameterString() ~ ")" ~ ConstString;
	}
	//------------------------------------------------------------------------

	private static string generateFullyQualifiedName( alias stringSymbol, alias renameString = TypeDescriptor.FunctionName )()
	{
		enum ConstString = Desc.IsConst ? " const" : "";
		enum StaticString = Desc.IsStatic ? "static " : "";

		string generateParameterString( uint iIndex )()
		{
			static if( iIndex >= TypeDescriptor.ParameterCount )
			{
				return "";
			}
			else
			{
				return mixin( "Parameter!( iIndex )." ~ stringSymbol ) ~ ( iIndex < TypeDescriptor.ParameterCount - 1 ? ", " : " " ) ~ generateParameterString!( iIndex + 1 )();
			}
		}

		static if( stringSymbol == "CDecl" )
		{
			enum pointerString = ( ReturnDescriptor.IsClass || ReturnDescriptor.IsInterface ) ? "* " : " ";
		}
		else
		{
			enum pointerString = " ";
			enum namespaceString = "";
		}

		return StaticString ~ mixin( "TypeString!( ReturnDescriptor )." ~ stringSymbol ) ~ pointerString ~ renameString ~ "( " ~ generateParameterString!( 0 )() ~ ")" ~ ConstString;
	};
	//------------------------------------------------------------------------

	enum ParameterInfo
	{
		Names = 0x1,
		Types = 0x2,
	}

	private static string generateDParameterString( ParameterInfo eInfo )
	{
		string[] parameters;

		foreach( Parameter; TypeDescriptor.ParametersAsTuple )
		{
			if( ( eInfo & ParameterInfo.Types ) && ( eInfo & ParameterInfo.Names ) )
			{
				parameters ~= TypeString!( Parameter.Descriptor ).DDecl ~ " " ~ Parameter.Name;
			}
			else if( eInfo & ParameterInfo.Types )
			{
				parameters ~= TypeString!( Parameter.Descriptor ).DDecl;
			}
			else if( eInfo & ParameterInfo.Names )
			{
				parameters ~= Parameter.Name;
			}
		}

		return parameters.joinWith( ", " );
	}

	private static string generateDCall()
	{
		return TypeDescriptor.FunctionName ~ "( " ~ ParameterNames ~ " )";
	}
}
//----------------------------------------------------------------------------

/+template FunctionString( alias symbol )
{
	// Signature that we can parse, aw yis.
	static string			DSignature()()				{ return typeof( symbol ).stringof; }
	static string			CSignature()()				{ return generateCSignature(); }

	static string			ParameterNames()()			{ return generateDParameterString( ParameterInfo.Names ); }
	static string			ParameterDeclarations()()	{ return generateDParameterString( ParameterInfo.Names | ParameterInfo.Types ); }
	static string			ParameterTypes()()			{ return generateDParameterString( ParameterInfo.Types ); }

	static string			DDeclNoLinkage()()							{ return generateFullyQualifiedName!( "FullyQualifiedDDecl" )(); }
	static string			DDeclNoLinkageRename( alias to )()			{ return generateFullyQualifiedName!( "FullyQualifiedDDecl", to )(); }
	static string			DDeclNoLinkagePrependName( alias pre )()	{ return generateFullyQualifiedName!( "FullyQualifiedDDecl", pre ~ __traits( identifier, symbol ) )(); }

	static string			DDecl()()					{ return Linkage() ~ DDeclNoLinkage(); }
	static string			CDecl()()					{ return generateFullyQualifiedName!( "CDecl" )(); }

	static string			DCall()()					{ return generateDCall(); }
	//------------------------------------------------------------------------

	private static string	Linkage()()
	{
		enum LinkageOfT = std.traits.functionLinkage!( symbol );
		static if( LinkageOfT == "D" ) return "";
		else return "extern( " ~ Desc.Linkage ~ " ) ";
	}

	struct Parameter( uint iIndex )
	{
		alias				BaseParam					= TypeDescriptor.Parameter!( iIndex );

		alias				Name						= BaseParam.Name;

		package enum		DDecl						= TypeString!( BaseParam.Descriptor ).DDecl ~ " " ~ BaseParam.Name;
		package enum		FullyQualifiedDDecl			= TypeString!( BaseParam.Descriptor ).FullyQualifiedDDecl ~ " " ~ BaseParam.Name;
		package enum		CDecl						= TypeString!( BaseParam.Descriptor ).CDecl ~ ( BaseParam.Descriptor.IsClass ? "* " : " " ) ~ BaseParam.Name;
	}
	//------------------------------------------------------------------------

	private static string generateCSignature()
	{
		enum ConstString = ( std.traits.functionAttributes!( symbol ) & std.traits.FunctionAttribute.const_ ) ? " const" : "";
		alias ReturnType = std.traits.ReturnType!( symbol );
		enum IsRef = ( std.traits.functionAttributes!( symbol ) & std.traits.FunctionAttribute.ref_ ) != 0;

		string generateParameterString()
		{
			string[] parameterTypes;
			foreach( Parameter; TypeDescriptor.ParametersAsTuple )
			{
				parameterTypes ~= TypeString!( Parameter.Descriptor ).CDecl;
			}
			return parameterTypes.joinWith( ", " );
		}

		return TypeString!( ReturnType, IsRef ).CDecl ~ "(" ~ generateParameterString() ~ ")" ~ ConstString;
	}
	//------------------------------------------------------------------------

	private static string generateFullyQualifiedName( alias stringSymbol, alias renameString = __traits( identifier, symbol ) )()
	{
		enum ConstString = ( std.traits.functionAttributes!( symbol ) & std.traits.FunctionAttribute.const_ ) ? " const" : "";
		alias ReturnType = std.traits.ReturnType!( symbol );
		enum IsRef = ( std.traits.functionAttributes!( symbol ) & std.traits.FunctionAttribute.ref_ ) != 0;
		enum StaticString = __traits( isStaticFunction, symbol ) ? "static " : "";
		enum MakePointer = is( ReturnType == class ) || is( ReturnType == interface );

		string generateParameterString( uint iIndex )()
		{
			static if( iIndex >= TypeDescriptor.ParameterCount )
			{
				return "";
			}
			else
			{
				return mixin( "Parameter!( iIndex )." ~ stringSymbol ) ~ ( iIndex < TypeDescriptor.ParameterCount - 1 ? ", " : " " ) ~ generateParameterString!( iIndex + 1 )();
			}
		}

		static if( stringSymbol == "CDecl" )
		{
			enum pointerString = MakePointer ? "* " : " ";
		}
		else
		{
			enum pointerString = " ";
		}

		return StaticString ~ mixin( "TypeString!( ReturnType, IsRef )." ~ stringSymbol ) ~ pointerString ~ renameString ~ "( " ~ generateParameterString!( 0 )() ~ ")" ~ ConstString;
	};
	//------------------------------------------------------------------------

	enum ParameterInfo
	{
		Names = 0x1,
		Types = 0x2,
	}

	private static string generateDParameterString( ParameterInfo eInfo )
	{
		string[] parameters;

		foreach( Parameter; TypeDescriptor.ParametersAsTuple )
		{
			if( ( eInfo & ParameterInfo.Types ) && ( eInfo & ParameterInfo.Names ) )
			{
				parameters ~= TypeString!( Parameter.Descriptor ).DDecl ~ " " ~ Parameter.Name;
			}
			else if( eInfo & ParameterInfo.Types )
			{
				parameters ~= TypeString!( Parameter.Descriptor ).DDecl;
			}
			else if( eInfo & ParameterInfo.Names )
			{
				parameters ~= Parameter.Name;
			}
		}

		return parameters.joinWith( ", " );
	}

	private static string generateDCall()
	{
		return TypeDescriptor.FunctionName ~ "( " ~ ParameterNames ~ " )";
	}
}
//----------------------------------------------------------------------------
+/

//============================================================================
