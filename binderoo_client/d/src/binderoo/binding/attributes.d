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

module binderoo.binding.attributes;
//----------------------------------------------------------------------------

struct BindIgnore						{ }
struct BindNoExportObject				{ }
//----------------------------------------------------------------------------

// Entire object or single variable not suitable for serialisation? Then
// mark it up with this UDA and Binderoo will ignore it
struct BindNoSerialise
{
}
//----------------------------------------------------------------------------

// A function marked with BindDisallow will not define the wrapper function
// in cases where you still need to know about it.
struct BindDisallow
{
}
//----------------------------------------------------------------------------

// A type marked with BindAbstract will indicate that the corresponding C++
// type cannot instantiate thanks to the existence of pure virtual methods.
// These types will not attempt to auto-generate C++ bindings.
// TODO: Add more restrictions for instantiation etc
struct BindAbstract
{
}
//----------------------------------------------------------------------------

// Marking your object with BindVersion is used for narrowing down bound types
// under certain search circumstances. Not using a BindVersion means it is
// always available. If you don't construct with an array, the constructor
// acceps a variable amount of string arguments and converts to an array
// automagically.
struct BindVersion
{
	string[]	strVersions;

	this( string[] versions... )
	{
		strVersions = versions;
	}
}
//----------------------------------------------------------------------------

// Conversely, you can exclude from specific versions with BindExcludeVersion.
struct BindExcludeVersion
{
	string[]	strVersions;

	this( string[] versions... )
	{
		strVersions = versions;
	}
}
//----------------------------------------------------------------------------

struct BindUFCS
{
}
//----------------------------------------------------------------------------

struct BindInstancesOf
{
	enum ShouldUseName
	{
		No,				// Not used at all
		TemplateName,	// Name of the template
		AttributeName,	// BindInstanceName attribute on object
	}

	enum InstantiateIn
	{
		TemplateModule,
		FirstParamModule,
	}

	this( Params... )( Params params ) if( Params.length > 0 )
	{
		import binderoo.traits : PartialRight, IsTypeMatch;

		alias IsShouldUseName = PartialRight!( IsTypeMatch, ShouldUseName );
		alias IsInstantiateIn = PartialRight!( IsTypeMatch, InstantiateIn );

		static foreach( iIndex, Type; Params )
		{
			static if( IsShouldUseName!Type ) UseName = params[ iIndex ];
			static if( IsInstantiateIn!Type ) Instantiate = params[ iIndex ];
		}
	}

	ShouldUseName	UseName			= ShouldUseName.AttributeName;
	InstantiateIn	Instantiate		= InstantiateIn.TemplateModule;
}
//----------------------------------------------------------------------------

struct BindInstanceName
{
	string Name;
}
//----------------------------------------------------------------------------

struct BindInstanceParamLookup
{
	int[] ParamIndex;

	this( int[] params... ) { ParamIndex = params; }

}
//----------------------------------------------------------------------------

struct BindVirtual
{
	int		iIntroducedVersion		= -1;
	int		iMaxVersion				= -1;
}
//----------------------------------------------------------------------------

struct BindMethod
{
	int		iIntroducedVersion		= -1;
	int		iMaxVersion				= -1;
}
//----------------------------------------------------------------------------

struct BindConstructor
{
	int		iIntroducedVersion		= -1;
	int		iMaxVersion				= -1;
}
//----------------------------------------------------------------------------

struct BindDestructor
{
	int		iIntroducedVersion		= -1;
	int		iMaxVersion				= -1;
}
//----------------------------------------------------------------------------

struct BindVirtualDestructor
{
	int		iIntroducedVersion		= -1;
	int		iMaxVersion				= -1;
}
//----------------------------------------------------------------------------

struct BindExport
{
	int		iIntroducedVersion		= -1;
	int		iMaxVersion				= -1;
}
//----------------------------------------------------------------------------

struct BindGetter
{
	string	strPropertyName;
}
//----------------------------------------------------------------------------

struct BindSetter
{
	string strPropertyName;
}
//----------------------------------------------------------------------------

struct BindBinaryMatch
{
}
//----------------------------------------------------------------------------

template BindingData( alias Type )
{
	import binderoo.traits	: IsTemplatedType
							, TemplateOf
							, TemplateParamsOf
							, HasUDA
							, GetUDA
							, ParentOf
							, FullTypeName
							, ModuleName
							, IsNonAssociativeArray
							, ArrayValueType
							, IsConst
							, IsImmutable
							, IsInOut
							, IsPointer
							, PointerTarget
							, Unqualified;
	static if( IsPointer!Type )
	{
		enum Name = BindingData!( PointerTarget!Type ).Name ~ "*";
		enum Parent = BindingData!( PointerTarget!Type ).Parent;
	}
	else static if( IsConst!Type || IsImmutable!Type || IsInOut!Type )
	{
		enum Name = BindingData!( Unqualified!Type ).Name;
		enum Parent = BindingData!( Unqualified!Type ).Parent;
	}
	else static if( IsNonAssociativeArray!Type )
	{
		enum Name = BindingData!( ArrayValueType!Type ).Name ~ "[]";
		enum Parent = BindingData!( ArrayValueType!Type ).Parent;
	}
	else static if( IsTemplatedType!Type )
	{
		static if( HasUDA!( Type, BindInstancesOf ) )
		{
			enum InstanceInfo = GetUDA!( Type, BindInstancesOf );
			alias InstanceParams = TemplateParamsOf!( Type );

			static assert( !is( InstanceInfo == void ), "Instance " ~ FullTypeName!Type ~ " made it here without a @BindInstancesOf declaration" );

			static if( HasUDA!( Type, BindInstanceParamLookup ) )
			{
				enum ParamIndex = GetUDA!( Type, BindInstanceParamLookup ).ParamIndex;
			}
			else
			{
				import std.range : iota;
				import std.array : array;

				enum ParamIndex = iota( 0, InstanceParams.length ).array;
			}

			static assert( ParamIndex.length > 0, "Binderoo can't deal with a template of zero parameters" );

			string generateName()
			{
				string output;
				static if( InstanceInfo.UseName == BindInstancesOf.UseName.TemplateName ) // TODO: Resolve to alias identifier
				{
					static assert( false, "BindingName doesn't support TemplateName yet" );
				}
				else
				{
					static if( InstanceInfo.UseName == BindInstancesOf.UseName.AttributeName )
					{
						output ~= GetUDA!( Type, BindInstanceName ).Name;
					}
				}

				static foreach( Index; ParamIndex )
				{
					static if( HasUDA!( InstanceParams[ Index ], BindInstanceName ) )
					{
						output ~= GetUDA!( InstanceParams[ Index ], BindInstanceName ).Name;
					}
					else
					{
						output ~= InstanceParams[ Index ].stringof;
					}
				}

				return output;
			}

			string generateModule()
			{
				static if( InstanceInfo.Instantiate == BindInstancesOf.InstantiateIn.TemplateModule )
				{
					return ModuleName!( ParentOf!( TemplateOf!Type ) );
				}
				else static if( InstanceInfo.Instantiate == BindInstancesOf.InstantiateIn.FirstParamModule )
				{
					return ModuleName!( InstanceParams[ 0 ] );
				}
			}

			enum Name = generateName();
			enum Parent = generateModule();
		}
		else
		{
			static assert( FullTypeName!Type.length != FullTypeName!( ParentOf!( TemplateOf!Type ) ).length, FullTypeName!Type ~ " fails at bind naming with parent" ~ FullTypeName!( ParentOf!( TemplateOf!Type ) ) );

			enum Name = FullTypeName!Type[ Parent.length + 1 .. $ ];
			enum Parent = FullTypeName!( ParentOf!( TemplateOf!Type ) );
		}
	}
	else
	{
		import std.traits : isSomeFunction;
		static if( isSomeFunction!Type )
		{
			enum Name = __traits( identifier, Type );
			enum Parent = BindingData!( ParentOf!Type ).Parent ~ "." ~ BindingData!( ParentOf!Type ).Name;
		}
		else
		{
			static if( __traits( compiles, { alias Parent = ParentOf!Type; } ) )
			{
				enum Name = FullTypeName!Type[ Parent.length + 1 .. $ ];
				enum Parent = FullTypeName!( ParentOf!Type );
			}
			else
			{
				enum Name = FullTypeName!Type;
				enum Parent = "";
			}
		}
	}
}
//----------------------------------------------------------------------------

template BindingData( Type ) // TODO: UPGRADE DMD AND KILL THIS THING
{
	import binderoo.traits	: IsTemplatedType
							, TemplateOf
							, TemplateParamsOf
							, HasUDA
							, GetUDA
							, ParentOf
							, FullTypeName
							, ModuleName
							, IsNonAssociativeArray
							, ArrayValueType
							, IsConst
							, IsImmutable
							, IsInOut
							, IsPointer
							, PointerTarget
							, Unqualified;
	static if( IsPointer!Type )
	{
		enum Name = BindingData!( PointerTarget!Type ).Name ~ "*";
		enum Parent = BindingData!( PointerTarget!Type ).Parent;
	}
	else static if( IsConst!Type || IsImmutable!Type || IsInOut!Type )
	{
		enum Name = BindingData!( Unqualified!Type ).Name;
		enum Parent = BindingData!( Unqualified!Type ).Parent;
	}
	else static if( IsNonAssociativeArray!Type )
	{
		enum Name = BindingData!( ArrayValueType!Type ).Name ~ "[]";
		enum Parent = BindingData!( ArrayValueType!Type ).Parent;
	}
	else static if( IsTemplatedType!Type )
	{
		static if( HasUDA!( Type, BindInstancesOf ) )
		{
			enum InstanceInfo = GetUDA!( Type, BindInstancesOf );
			alias InstanceParams = TemplateParamsOf!( Type );

			static assert( !is( InstanceInfo == void ), "Instance " ~ FullTypeName!Type ~ " made it here without a @BindInstancesOf declaration" );

			static if( HasUDA!( Type, BindInstanceParamLookup ) )
			{
				enum ParamIndex = GetUDA!( Type, BindInstanceParamLookup ).ParamIndex;
			}
			else
			{
				import std.range : iota;
				import std.array : array;

				enum ParamIndex = iota( 0, InstanceParams.length ).array;
			}

			static assert( ParamIndex.length > 0, "Binderoo can't deal with a template of zero parameters" );

			string generateName()
			{
				string output;
				static if( InstanceInfo.UseName == BindInstancesOf.UseName.TemplateName ) // TODO: Resolve to alias identifier
				{
					static assert( false, "BindingName doesn't support TemplateName yet" );
				}
				else
				{
					static if( InstanceInfo.UseName == BindInstancesOf.UseName.AttributeName )
					{
						output ~= GetUDA!( Type, BindInstanceName ).Name;
					}
				}

				static foreach( Index; ParamIndex )
				{
					static if( HasUDA!( InstanceParams[ Index ], BindInstanceName ) )
					{
						output ~= GetUDA!( InstanceParams[ Index ], BindInstanceName ).Name;
					}
					else
					{
						output ~= InstanceParams[ Index ].stringof;
					}
				}

				return output;
			}

			string generateModule()
			{
				static if( InstanceInfo.Instantiate == BindInstancesOf.InstantiateIn.TemplateModule )
				{
					return ModuleName!( ParentOf!( TemplateOf!Type ) );
				}
				else static if( InstanceInfo.Instantiate == BindInstancesOf.InstantiateIn.FirstParamModule )
				{
					return ModuleName!( InstanceParams[ 0 ] );
				}
			}

			enum Name = generateName();
			enum Parent = generateModule();
		}
		else
		{
			static assert( FullTypeName!Type.length != FullTypeName!( ParentOf!( TemplateOf!Type ) ).length, FullTypeName!Type ~ " fails at bind naming with parent" ~ FullTypeName!( ParentOf!( TemplateOf!Type ) ) );

			enum Name = FullTypeName!Type[ Parent.length + 1 .. $ ];
			enum Parent = FullTypeName!( ParentOf!( TemplateOf!Type ) );
		}
	}
	else
	{
		import std.traits : isSomeFunction;
		static if( isSomeFunction!Type )
		{
			static if( __traits( compiles, { enum Name = __traits( identifier, Type ); } ) )
			{
				enum Name = __traits( identifier, Type );
				enum Parent = BindingData!( ParentOf!Type ).Parent ~ "." ~ BindingData!( ParentOf!Type ).Name;
			}
			else
			{
				static if( __traits( compiles, { alias ParentType = ParentOf!Type; } ) )
				{
					enum Name = FullTypeName!Type[ Parent.length + 1 .. $ ];
					enum Parent = FullTypeName!( ParentOf!Type );
				}
				else
				{
					//pragma( msg, "Weird type " ~ Name );
					enum Name = FullTypeName!Type;
					enum Parent = "";
				}
			}
		}
		else
		{
			static if( __traits( compiles, { alias Parent = ParentOf!Type; } ) )
			{
				enum Name = FullTypeName!Type[ Parent.length + 1 .. $ ];
				enum Parent = FullTypeName!( ParentOf!Type );
			}
			else
			{
				enum Name = FullTypeName!Type;
				enum Parent = "";
			}
		}
	}
}
//----------------------------------------------------------------------------

private import binderoo.traits : FullTypeName;

enum BindingName( alias Type )			= BindingData!Type.Name;
enum BindingName( Type )				= BindingData!Type.Name;

enum BindingParentName( alias Type )	= BindingData!Type.Parent;
enum BindingParentName( Type )			= BindingData!Type.Parent;

enum BindingFullName( alias Type )		= BindingData!Type.Parent ~ "." ~ BindingData!Type.Name;
enum BindingFullName( Type )			= BindingData!Type.Parent ~ ( BindingData!Type.Parent.length > 0 ? "." : "" ) ~ BindingData!Type.Name;
//----------------------------------------------------------------------------

package:

// Used internally. Avoid using yourself.
struct BindRawImport
{
	enum FunctionKind : short
	{
		Invalid = -1,
		Static,
		Method,
		Virtual,
		Constructor,
		Destructor,
		VirtualDestructor,
	}

	string			strCName;
	string			strCSignature;
	string[]		strIncludeVersions;
	string[]		strExcludeVersions;
	FunctionKind	eKind = FunctionKind.Invalid;
	bool			bIsConst;
	bool			bOwnerIsAbstract;
	ulong			uNameHash;
	ulong			uSignatureHash;
	int				iOrderInTable			= 0;
	int				iIntroducedVersion		= -1;
	int				iMaxVersion				= -1;

	this( string name, string signature, string[] includeVersions, string[] excludeVersions, FunctionKind kind, int orderInTable, bool isConst, bool ownerIsAbstract, int introducedVersion = -1, int maxVersion = -1 )
	{
		import binderoo.hash;

		strCName						= name;
		strCSignature					= signature;
		strIncludeVersions				= includeVersions;
		strExcludeVersions				= excludeVersions;
		eKind							= kind;
		bIsConst						= isConst;
		bOwnerIsAbstract				= ownerIsAbstract;
		uNameHash						= fnv1a_64( name );
		uSignatureHash					= fnv1a_64( signature );
		iOrderInTable					= orderInTable;
		iIntroducedVersion				= introducedVersion;
		iMaxVersion						= maxVersion;
	}

	this( string name, string signature, string[] includeVersions, string[] excludeVersions, FunctionKind kind, bool isConst, bool ownerIsAbstract, ulong nameHash, ulong signatureHash, int orderInTable, int introducedVersion, int maxVersion )
	{
		strCName						= name;
		strCSignature					= signature;
		strIncludeVersions				= includeVersions;
		strExcludeVersions				= excludeVersions;
		eKind							= kind;
		bIsConst						= isConst;
		bOwnerIsAbstract				= ownerIsAbstract;
		uNameHash						= nameHash;
		uSignatureHash					= signatureHash;
		iOrderInTable					= orderInTable;
		iIntroducedVersion				= introducedVersion;
		iMaxVersion						= maxVersion;
	}

	string toUDAString()
	{
		import std.conv : to;
		return "@BindRawImport(\""	~ strCName ~ "\", \"" ~ strCSignature ~ "\", "
									~ "cast(string[])" ~ strIncludeVersions.to!string ~ ", "
									~ "cast(string[])" ~ strExcludeVersions.to!string ~ ", "
									~ "BindRawImport.FunctionKind." ~ eKind.to!string ~ ", "
									~ bIsConst.to!string ~ ", "
									~ bOwnerIsAbstract.to!string ~ ", "
									~ uNameHash.to!string ~ "UL, "
									~ uSignatureHash.to!string ~ "UL, "
									~ iOrderInTable.to!string ~ ", "
									~ iIntroducedVersion.to!string ~ ", "
									~ iMaxVersion.to!string ~ ")";
	}
}
//----------------------------------------------------------------------------

// Used internally. Avoid using yourself.
struct BindOverrides
{
	string strFunctionName;
}
//----------------------------------------------------------------------------

// Used internally. Avoid using yourself.
struct InheritanceBase
{
}
//----------------------------------------------------------------------------

struct Documentation
{
	struct ParameterDocumentation
	{
		string						strParamName;
		string						strParamDescription;
	}

	string							strFuncDescription;
	string							strReturnDescription;
	ParameterDocumentation[]		vecTemplateParameterDescriptions;
	ParameterDocumentation[]		vecParameterDescriptions;

}
//----------------------------------------------------------------------------

//============================================================================
