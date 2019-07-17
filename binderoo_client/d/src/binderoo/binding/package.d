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

module binderoo.binding;
//----------------------------------------------------------------------------

public import binderoo.binding.attributes;
public import binderoo.binding.cppfunctiongenerator;
public import binderoo.binding.functionstub;
public import binderoo.binding.boundenum;
public import binderoo.binding.boundfunction;
public import binderoo.binding.boundobject;
public import binderoo.binding.boundmodule;

public import binderoo.functiondescriptor;
public import binderoo.variabledescriptor;
public import binderoo.hash;
public import binderoo.objectprivacy;
public import binderoo.slice;

public import binderoo.util.enumoptions;

private import std.traits;
//----------------------------------------------------------------------------

enum BindOption
{
	ExportUntagged,
	IgnoreBindNoExportObject,
}
//----------------------------------------------------------------------------

mixin template BindVersionDeclaration( int iCurrentVersion )
{
	enum			BoundVersion		= iCurrentVersion;
	__gshared int	ImportedVersion		= -1;
}
//----------------------------------------------------------------------------

mixin template BindOnly( Type, int iCurrentVersion = 0, Options... )
{
	import std.traits : isSomeFunction;
	mixin BindModuleStaticSetup!( binderoo.traits.Filter!( isSomeFunction, Options ) );
	
	static void initialiseModuleBinding()
	{
		enum BindOptions = OptionsOf!( BindOption, Options )();
		alias CurrModule = ModuleOf!theseModules;

		auto currModule = generateModuleInfo!CurrModule();
		auto theseFunctionsToImport = generateFunctionImports!( CurrModule, Type )();
		auto theseFunctionsToExport = generateFunctionExports!( BindOptions, CurrModule, Type )();
		auto theseObjectsToExport = generateObjectExports!( CurrModule, Type )();
		auto theseEnumsToExport = generateEnumExports!( CurrModule, Type )();

		registerModule( currModule );
		registerImportFunctions( theseFunctionsToImport );
		registerExportedFunctions( theseFunctionsToExport );
		registerExportedObjects( theseObjectsToExport );
		registerExportedEnums( theseEnumsToExport );

		theseModules = [ currModule ];
		functionsToImport = theseFunctionsToImport;
		functionsToExport = theseFunctionsToExport;
		objectsToExport = theseObjectsToExport;
		enumsToExport = theseEnumsToExport;
	}
	//------------------------------------------------------------------------

}
//----------------------------------------------------------------------------

mixin template BindModule( int iCurrentVersion = 0, Options... )
{
	import std.traits : isSomeFunction;
	mixin BindModuleStaticSetup!( binderoo.traits.Filter!( isSomeFunction, Options ) );

	void initialiseModuleBinding()
	{
		enum BindOptions = OptionsOf!( BindOption, Options )();
		alias CurrModule = ModuleOf!theseModules;

		//import std.stdio : writeln;
		//pragma( msg, "Initialising " ~ CurrModule.stringof ~ " bindings..." );

		auto currModule					= generateModuleInfo!CurrModule();
		auto theseFunctionsToImport		= generateFunctionImports!CurrModule();
		auto theseFunctionsToExport		= generateFunctionExports!( BindOptions, CurrModule )();
		auto theseObjectsToExport		= generateObjectExports!CurrModule();
		auto theseEnumsToExport			= generateEnumExports!CurrModule();

		registerModule( currModule );
		registerImportFunctions( theseFunctionsToImport );
		registerExportedFunctions( theseFunctionsToExport );
		registerExportedObjects( theseObjectsToExport );
		registerExportedEnums( theseEnumsToExport );

		theseModules = [ currModule ];
		functionsToImport = theseFunctionsToImport;
		functionsToExport = theseFunctionsToExport;
		objectsToExport = theseObjectsToExport;
		enumsToExport = theseEnumsToExport;
	}
	//------------------------------------------------------------------------

}
//----------------------------------------------------------------------------

mixin template BindModules( int iCurrentVersion, Options... )
{
	import std.traits : isSomeFunction;
	mixin BindModuleStaticSetup!( binderoo.traits.Filter!( isSomeFunction, Options ) );

	void initialiseModuleBinding()
	{
		enum Modules = ExtractAllOf!( string, Options )();
		enum BindOptions = OptionsOf!( BindOption, Options )();
		alias SpecificTypes = binderoo.traits.Filter!( IsSomeType, Options );

		BoundModule[] 		allModules;
		BoundObject[]		allObjectsToExport;
		BoundFunction[]		allFunctionsToImport;
		BoundFunction[]		allFunctionsToExport;
		BoundEnum[]			allEnumsToExport;

		static foreach( ModuleName; Modules )
		{
			{
				alias CurrModule = ModuleFromName!ModuleName;

				auto currModule					= generateModuleInfo!CurrModule();
				auto theseFunctionsToImport		= generateFunctionImports!CurrModule();
				auto theseFunctionsToExport		= generateFunctionExports!( BindOptions, CurrModule )();
				auto theseObjectsToExport		= generateObjectExports!CurrModule();
				auto theseEnumsToExport			= generateEnumExports!CurrModule();

				registerModule( currModule );
				registerImportFunctions( theseFunctionsToImport );
				registerExportedFunctions( theseFunctionsToExport );
				registerExportedObjects( theseObjectsToExport );
				registerExportedEnums( theseEnumsToExport );

				allModules						~= currModule;
				allObjectsToExport				~= theseObjectsToExport;
				allFunctionsToImport			~= theseFunctionsToImport;
				allFunctionsToExport			~= theseFunctionsToExport;
				allEnumsToExport				~= theseEnumsToExport;
			}
		}

		static foreach( Type; SpecificTypes )
		{
			{
				alias CurrModule = ModuleOf!Type;

				auto currModule					= generateModuleInfo!CurrModule();
				auto theseFunctionsToImport		= generateFunctionImports!( CurrModule, Type )();
				auto theseFunctionsToExport		= generateFunctionExports!( BindOptions, CurrModule, Type )();
				auto theseObjectsToExport		= generateObjectExports!( CurrModule, Type )();
				auto theseEnumsToExport			= generateEnumExports!( CurrModule, Type )();

				registerModule( currModule );
				registerImportFunctions( theseFunctionsToImport );
				registerExportedFunctions( theseFunctionsToExport );
				registerExportedObjects( theseObjectsToExport );
				registerExportedEnums( theseEnumsToExport );

				allModules						~= currModule;
				allObjectsToExport				~= theseObjectsToExport;
				allFunctionsToImport			~= theseFunctionsToImport;
				allFunctionsToExport			~= theseFunctionsToExport;
				allEnumsToExport				~= theseEnumsToExport;
			}
		}

		theseModules = allModules;
		functionsToImport = allFunctionsToImport;
		functionsToExport = allFunctionsToExport;
		objectsToExport = allObjectsToExport;
		enumsToExport = allEnumsToExport;
	}
}
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------

// Module internals
//----------------------------------------------------------------------------
mixin template BindModuleStaticSetup( AdditionalStaticThisCalls... )
{
	shared static this()
	{
		initialiseModuleBinding();

		foreach( newCall; AdditionalStaticThisCalls )
		{
			newCall();
		}
	}
	//------------------------------------------------------------------------

	public:

	__gshared BoundModule[] 						theseModules;
	__gshared BoundObject[]							objectsToExport;
	__gshared BoundFunction[]						functionsToImport;
	__gshared BoundFunction[]						functionsToExport;
	__gshared BoundEnum[]							enumsToExport;
	//------------------------------------------------------------------------
}
//----------------------------------------------------------------------------

template ModuleTypeDescriptors( alias ParentClass, Aliases... )
{
	import std.typetuple;
	mixin( "import " ~ ModuleName!ParentClass ~ ";" );

	string makeATuple() // TODO: Resolve to alias identifier
	{
		import std.conv : to;
		import binderoo.traits : IsAggregateType, IsSomeType, joinWith;
		import binderoo.binding.attributes : BindInstancesOf;

		string[] indices;
		foreach( Alias; Aliases )
		{
			enum AliasString = FullTypeName!( ParentClass ) ~ "." ~ Alias;

			static if( mixin( "__traits( compiles, " ~ AliasString ~ " ) && IsSomeType!( " ~ AliasString ~ " )" ) )
			{
				mixin( "alias Type = " ~ AliasString ~ ";" );
				static if( IsAggregateType!( Type )
					&& ( Alias == Type.stringof || HasUDA!( Type, BindInstancesOf ) ) )
				{
					mixin( "import " ~ ModuleName!( Type ) ~ ";" );
					indices ~= AliasString;
				}
			}
		}

		return "alias ModuleTypeDescriptors = TypeTuple!( " ~ indices.joinWith( ", " ) ~ " );";
	}


	mixin( makeATuple() );
}
//----------------------------------------------------------------------------

template ModuleEnumDescriptors( alias ParentClass, Aliases... )
{
	import std.conv : to;
	import std.typetuple;
	mixin( "import " ~ ModuleName!ParentClass ~ ";" );

	string[] getEnums( alias Parent, Symbols... )()
	{
		import binderoo.traits : IsAggregateType, IsSomeType, joinWith;

		string[] indices;
		foreach( Symbol; Symbols )
		{
			enum AliasString = FullTypeName!( Parent ) ~ "." ~ Symbol;

			static if( mixin( "__traits( compiles, " ~ AliasString ~ " ) && IsSomeType!( " ~ AliasString ~ " )" ) )
			{
				mixin( "alias Type = " ~ AliasString ~ ";" );
				static if( Symbol == Type.stringof )
				{
					static if( IsEnum!( Type ) && IsIntegral!( OriginalType!Type ) )
					{
						mixin( "import " ~ ModuleName!( Type ) ~ ";" );
						indices ~= AliasString;
					}
					else static if( IsAggregateType!Type )
					{
						mixin( "import " ~ ModuleName!( Type ) ~ ";" );
						indices ~= getEnums!( Type, __traits( allMembers, Type ) )();
					}
				}
			}
		}

		return indices;
	}

	mixin( "alias ModuleEnumDescriptors = TypeTuple!( " ~ getEnums!( ParentClass, Aliases )().joinWith( ", " ) ~ " );" );
}
//----------------------------------------------------------------------------

template ModuleTemplateInstances( alias Parent, Symbols... )
{
	mixin( "import " ~ ModuleName!Parent ~ ";" );
	
	string[] getTemplates()
	{
		import std.conv : to;
		import binderoo.traits : IsAggregateType, IsSomeType, IsAlias, joinWith;

		string[] output;

		foreach( Symbol; Symbols )
		{
			enum AliasString = FullTypeName!Parent ~ "." ~ Symbol;
			static if( mixin( "__traits( compiles, " ~ AliasString ~ " )" ) )
			{
				mixin( "alias Type = " ~ AliasString ~ ";" );
				static if( IsTemplate!Type )
				{
					output ~= AliasString;
				}
			}
		}

		return output;
	}

	mixin( "alias ModuleTemplateInstances = binderoo.traits.AliasSeq!( " ~ getTemplates().joinWith( ", " ) ~ " );" );
}
//----------------------------------------------------------------------------

BoundModule generateModuleInfo( alias Module, ObjectTypes... )()
{
	mixin( "import " ~ ModuleName!Module ~ ";" );

	DString[] gatherFor( Types... )()
	{
		DString[] interfaceStrings;

		static foreach( CurrType; Types )
		{
			static if( __traits( hasMember, CurrType, "DInferfaceText" ) )
			{
				interfaceStrings ~= DString( CurrType.DInferfaceText );
			}
		}

		return interfaceStrings;
	}

	string thisModuleName = ModuleName!Module;

	BoundModule currModule = { thisModuleName, cast(DString[])( [ ] ), thisModuleName.fnv1a_64() };

	static if( ObjectTypes.length == 0 )
	{
		alias ModuleTypes = ModuleTypeDescriptors!( Module, __traits( allMembers, Module ) );
	}
	else
	{
		alias ModuleTypes = ModuleTypeDescriptors!( Module, ObjectTypes[ 0 ].stringof );
	}

	currModule.strDInterfaceDecls = Slice!DString( gatherFor!ModuleTypes );

	return currModule;
}
//----------------------------------------------------------------------------

BoundObject[] generateObjectExports( alias Parent, ObjectTypes... )()
{
	mixin( "import " ~ ModuleName!Parent ~ ";" );

	BoundObject[] gatherFor( bool bRecursive, Types... )()
	{
		BoundObject[] objects;
		foreach( Type; Types )
		{
			static if ( Type.stringof == "Monitor" || Type.stringof == "Object" )
			{
				//pragma( msg, "Not grabbing object " ~ Type.stringof );
			}
			else
			{
				//pragma( msg, "Grabbing object " ~ Type.stringof );
				/+static if( is( Type == class ) )
				{
					foreach( m; __traits( allMembers, Type ) )
					{
						pragma( msg, m );
					}
				}+/
				
				enum FullName = BindingName!Type;

				static if( ( is( Type == struct ) || is( Type == class ) )
							&& !HasUDA!( Type, BindNoExportObject )
							&& !HasUDA!( Type, BindIgnore ) )
				{
					alias TheseFunctions = BoundObjectFunctions!( Type );
					objects ~= BoundObject( DString( FullName )
											, fnv1a_64( FullName )
											, &TheseFunctions.allocObj
											, &TheseFunctions.deallocObj
											, &TheseFunctions.thunkObj
											, &TheseFunctions.serialiseObj
											, &TheseFunctions.deserialiseObj
											, TheseFunctions.TypeVal
											, &TheseFunctions.generateCSharpVariables
											, &TheseFunctions.generateCSharpTypeDecl
											, &TheseFunctions.hasBaseType
											, &TheseFunctions.getBaseTypeNameCSharp );
				}

				static if( bRecursive )
				{
					alias AllSubTypes = ModuleTypeDescriptors!( Type, __traits( allMembers, Type ) );

					objects ~= gatherFor!( bRecursive, AllSubTypes )();
				}
			}
		}

		return objects;
	}

	static if( ObjectTypes.length == 0 )
	{
		alias ModuleTypes = ModuleTypeDescriptors!( Parent, __traits( allMembers, Parent ) );
		alias ModuleTemplates = ModuleTemplateInstances!( Parent, __traits( allMembers, Parent ) );
		return gatherFor!( true, ModuleTypes )();
	}
	else
	{
		alias ModuleTypes = ModuleTypeDescriptors!( __traits( parent, ObjectTypes[ 0 ] ), ObjectTypes[ 0 ].stringof );
		alias ModuleTemplates = ModuleTemplateInstances!( Parent, __traits( allMembers, Parent ) );
		return gatherFor!( false, ModuleTypes )();
	}
}
//----------------------------------------------------------------------------

BoundEnum[] generateEnumExports( alias Parent, ExportTypes... )()
{
	mixin( "import " ~ ModuleName!Parent ~ ";" );
	//pragma( msg, "Looking for enums in " ~ ModuleName!Parent ~ " from symbols " ~ __traits( allMembers, Parent ).stringof );

	BoundEnum[] gatherFor( Types... )()
	{
		BoundEnum[] enums;

		foreach( Type; Types )
		{
			import std.traits : OriginalType;
			import std.conv : to;

			alias ThisTypeString = TypeString!( OriginalType!Type );

			//pragma( msg, "Enum " ~ FullTypeName!Type );
			//pragma( msg, " -> D type: " ~ ThisTypeString.DDecl );
			//pragma( msg, " -> C type: " ~ ThisTypeString.CDecl );
			//pragma( msg, " -> C# type: " ~ ThisTypeString.CSharpDecl );

			BoundEnum newEnum;
			newEnum.strName = DString( FullTypeName!Type );
			newEnum.strUnderlyingTypeD = DString( ThisTypeString.DDecl );
			newEnum.strUnderlyingTypeC = DString( ThisTypeString.CDecl );
			newEnum.strUnderlyingTypeCSharp = DString( ThisTypeString.CSharpDecl );

			EnumValueInfo[] values;

			foreach( EnumValue; __traits( allMembers, Type ) )
			{
				mixin( "enum Val = cast( OriginalType!Type )" ~ FullTypeName!Type ~ "." ~ EnumValue ~ ";" );
				//pragma( msg, " -> " ~ EnumValue ~ " = " ~ Val.to!string );

				EnumValueInfo newInfo;
				newInfo.strIdentifier = DString( EnumValue );
				newInfo.strValue = DString( Val.to!string );

				values ~= newInfo;
			}

			newEnum.vecAllValues = Slice!EnumValueInfo( values );

			enums ~= newEnum;
		}

		return enums;
	}

	static if( ExportTypes.length == 0 )
	{
		alias ModuleEnums = ModuleEnumDescriptors!( Parent, __traits( allMembers, Parent ) );
	}
	else
	{
		alias ModuleEnums = ModuleEnumDescriptors!( Parent, ExportTypes[ 0 ].stringof );
	}

	return gatherFor!ModuleEnums;
}
//----------------------------------------------------------------------------

template BoundFunctionReturnTypeKind( T )
{
	BoundFunction.FunctionKind generate( CurrType )()
	{
		BoundFunction.FunctionKind eKind;

		static if( is( CurrType : PT*, PT ) )
		{
			eKind |= BoundFunction.FunctionKind.ReturnsPointer | generate!PT;
		}
		else static if( is( CurrType : AT[], AT ) )
		{
			eKind |= BoundFunction.FunctionKind.ReturnsArray | generate!AT;
		}
		else static if( is( CurrType == class ) || is( CurrType == interface ) )
		{
			eKind |= BoundFunction.FunctionKind.ReturnsClass;
		}
		else static if( is( CurrType == struct ) )
		{
			eKind |= BoundFunction.FunctionKind.ReturnsStruct;
		}
		else static if( is( CurrType == union ) )
		{
			eKind |= BoundFunction.FunctionKind.ReturnsUnion;
		}
		else static if( is( CurrType == void ) )
		{
			eKind |= BoundFunction.FunctionKind.ReturnsVoid;
		}
		else
		{
			eKind |= BoundFunction.FunctionKind.ReturnsBasicType;
		}

		return eKind;
	}

	enum BoundFunctionReturnTypeKind = generate!T();
}
//----------------------------------------------------------------------------

BoundFunction[] generateFunctionImports( alias Parent, ImportTypes... )()
{
	mixin( "import " ~ ModuleName!Parent ~ ";" );

	BoundFunction.FunctionKind convert( BindRawImport.FunctionKind eKind )
	{
		final switch( eKind ) with( BindRawImport.FunctionKind )
		{
		case Invalid:
			return BoundFunction.FunctionKind.Undefined;
		case Static:
			return BoundFunction.FunctionKind.Static;
		case Method:
			return BoundFunction.FunctionKind.Method;
		case Virtual:
			return BoundFunction.FunctionKind.Virtual;
		case Constructor:
			return BoundFunction.FunctionKind.Constructor;
		case Destructor:
			return BoundFunction.FunctionKind.Destructor;
		case VirtualDestructor:
			return BoundFunction.FunctionKind.VirtualDestructor;
		}
	}

	BoundFunction[] TableGrabber( Type, string TableStaticMember )()
	{
		BoundFunction[] imports;

		static if( IsStaticMember!( Type, TableStaticMember ) )
		{
			alias TableType = typeof( __traits( getMember, Type, TableStaticMember ) );

			static if( Type.StructType == InheritanceStructType.CPP )
			{
				alias CTypeData = GetUDA!( Type, CTypeName );
			}
			else
			{
				alias CTypeData = GetUDA!( typeof( Type.base ), CTypeName );
			}

			foreach( iIndex, TableMember; TableType.init.tupleof )
			{
				static if( HasUDA!( TableType.tupleof[ iIndex ], BindRawImport ) )
				{
					enum MemberName = __traits( identifier, TableType.tupleof[ iIndex ] );
					alias ImportData = GetUDA!( TableType.tupleof[ iIndex ], BindRawImport );

					enum AbstractFlag = ImportData.bOwnerIsAbstract ? BoundFunction.Flags.OwnerIsAbstract : BoundFunction.Flags.None;
					enum ConstFlag = ImportData.bIsConst ? BoundFunction.Flags.Const : BoundFunction.Flags.None;

					enum FoundFlags = cast( BoundFunction.Flags )( AbstractFlag | ConstFlag );

					//pragma( msg, cast(string)ImportData.strCName ~ ", " ~ cast(string)ImportData.strCSignature );

/*						enum NameHash = ImportData.uNameHash;
					enum SignatureHash = ImportData.uSignatureHash;
					pragma( msg, tableMember ~ " imports " ~ ImportData.strCName ~ " ( " ~ NameHash.stringof ~ " ), " ~ ImportData.strCSignature ~ " ( " ~ SignatureHash.stringof ~ " )" );*/

					imports ~= BoundFunction(	DString( ImportData.strCName )
												, DString( ImportData.strCSignature )
												, DString( CTypeData.name )
												, DString( CTypeData.header )
												, ImportData.strIncludeVersions.toSliceRecursive
												, ImportData.strExcludeVersions.toSliceRecursive
												, BoundFunction.Hashes( ImportData.uNameHash, ImportData.uSignatureHash )
												, mixin( "cast(void*) &" ~ FullTypeName!( Type ) ~ "." ~ TableStaticMember ~ "." ~ MemberName )
												, ImportData.iIntroducedVersion
												, ImportData.iOrderInTable
												, BoundFunction.Resolution.WaitingForImport
												, BoundFunction.CallingConvention.CPP
												, convert( ImportData.eKind )
												, FoundFlags
												, null
												, null
												, null
												, null
												, null
												, null
												, null
												, null
												, null
												, null
											);
				}
			}
		}

		return imports;
	}

	BoundFunction[] gatherFor( bool bRecursive, Types... )()
	{
		BoundFunction[] imports;
		foreach( Type; Types )
		{
			alias Members = TypeTuple!( __traits( allMembers, Type ) );

			imports ~= TableGrabber!( Type, "_vtableData" )();
			imports ~= TableGrabber!( Type, "_methodtableData" )();

			static if( bRecursive )
			{
				imports ~= gatherFor!( bRecursive, ModuleTypeDescriptors!( Type, Members ) )();
			}
		}

		return imports;
	}

	static if( ImportTypes.length == 0 )
	{
		alias Types = ModuleTypeDescriptors!( Parent , __traits( allMembers, Parent ) );
		return gatherFor!( true, Types )();
	}
	else
	{
		alias Types = ModuleTypeDescriptors!( __traits( parent, ImportTypes[ 0 ] ), ImportTypes[ 0 ].stringof );
		return gatherFor!( false, Types )();
	}
}
	
BoundFunction[] generateFunctionExports( alias Options, alias Parent, ExportTypes... )()
{
	mixin( "import " ~ ModuleName!Parent ~ ";" );

	BoundFunction[] functionGrabber( int IntroducedVersion, alias Scope, Symbols... )()
	{
		static if( HasUDA!( Scope, BindIgnore )
					|| HasUDA!( Scope, BindNoExportObject ) )
		{
			//pragma( msg, "Skipping functions for " ~ Scope.stringof ~ "..." );
			return [];
		}

		//pragma( msg, "Processing " ~ Scope.stringof ~ " for exports" );

		enum ExportAllFound = IntroducedVersion != BindExport.iIntroducedVersion.init;
		enum ScopeIsAggregate = IsAggregateType!Scope;

		BoundFunction[] foundExports;

		void handleFunction( alias Descriptor )( int iIntroducedVersion )
		{
			static if( Descriptor.HasUDA!BindIgnore || Descriptor.IsOperator )
			{
				return;
			}
			else static if( Descriptor.IsImplementedInType )
			{
				void* pFunctionCDecl;
				void* pFunctionCPPDecl;

				alias NewFunc = CPPFunctionGenerator!Descriptor;

				static if( Descriptor.IsCPlusPlusFunction )
				{
					pFunctionCDecl = &NewFunc.FuncCDecl;
					mixin( "pFunctionCPPDecl = &" ~ Descriptor.FullyQualifiedName ~ ";" );
				}
				else static if( Descriptor.IsCFunction )
				{
					mixin( "pFunctionCDecl = &" ~ Descriptor.FullyQualifiedName ~ ";" );
					pFunctionCPPDecl = &NewFunc.FuncCPPDecl;
				}
				else
				{
					//mixin( "alias Descriptor = FunctionDescriptor!( NewFunc." ~ OriginalDescriptor.Name ~ ", 0 );" );
					pFunctionCDecl = &NewFunc.FuncCDecl;
					pFunctionCPPDecl = &NewFunc.FuncCPPDecl;
				}

				enum FullName		= Descriptor.FullyQualifiedName;
				enum Signature		= FunctionString!( Descriptor ).CSignature;
				enum DDecl			= FunctionString!( Descriptor ).DDecl;
				enum CDecl			= FunctionString!( Descriptor ).CDecl;
				enum CSharpDecl		= FunctionString!( Descriptor ).CSharpDecl;
				enum ParameterNames	= FunctionString!( Descriptor ).ParameterNames;

				static if( Descriptor.IsStatic )
				{
					static if( Descriptor.IsMemberFunction )
					{
						enum MethodFlag = BoundFunction.FunctionKind.Method;
					}
					else
					{
						enum MethodFlag = BoundFunction.FunctionKind.Undefined;
					}

					static if( Descriptor.IsProperty )
					{
						enum FunctionKind = cast( BoundFunction.FunctionKind )( BoundFunction.FunctionKind.Static | BoundFunction.FunctionKind.Property | MethodFlag );
					}
					else
					{
						enum FunctionKind = cast( BoundFunction.FunctionKind )( BoundFunction.FunctionKind.Static | MethodFlag );
					}
				}
				else
				{
					static if( Descriptor.IsProperty )
					{
						enum FunctionKind = cast( BoundFunction.FunctionKind )( BoundFunction.FunctionKind.Method | BoundFunction.FunctionKind.Property );
					}
					else static if( Descriptor.IsConstructor )
					{
						enum FunctionKind = cast( BoundFunction.FunctionKind )( BoundFunction.FunctionKind.Method | BoundFunction.FunctionKind.Constructor );
					}
					else static if( Descriptor.IsDestructor )
					{
						enum FunctionKind = cast( BoundFunction.FunctionKind )( BoundFunction.FunctionKind.Method | BoundFunction.FunctionKind.Destructor );
					}
					else
					{
						enum FunctionKind = BoundFunction.FunctionKind.Method;
					}
				}

				//pragma( msg, "Exporting " ~ FullName ~ ": " ~ Signature );
				//pragma( msg, " -> Parameter names: " ~ BoundFunctionFunctions!( Descriptor ).ParameterNames().joinWith( ", " ) );
				//pragma( msg, " -> C Declaration: " ~ BoundFunctionFunctions!( Descriptor ).CPrototype() );
				//pragma( msg, " -> Parameter types (C): " ~ BoundFunctionFunctions!( Descriptor ).CParameterTypes().joinWith( ", " ) );
				//pragma( msg, " -> D Declaration: " ~ BoundFunctionFunctions!( Descriptor ).DPrototype() );
				//pragma( msg, " -> Parameter types (D): " ~ BoundFunctionFunctions!( Descriptor ).ParameterNames().joinWith( ", " ) );
				//pragma( msg, " -> C# Declaration: " ~ BoundFunctionFunctions!( Descriptor ).CSharpPrototype() );
				//pragma( msg, " -> C# Marshalled declaration: " ~ BoundFunctionFunctions!( Descriptor ).CSharpMarshalledPrototype() );
				//pragma( msg, " -> Parameter types (C#): " ~ BoundFunctionFunctions!( Descriptor ).CSharpParameterTypes().joinWith( ", " ) );
				//pragma( msg, " -> Parameter call (C#): " ~ BoundFunctionFunctions!( Descriptor ).CSharpParameterNamesWithQualifiers().joinWith( ", " ) );

				foundExports ~= BoundFunction( DString( FullName )
												, DString( Signature )
												, DString( "" )
												, DString( "" )
												, Slice!DString.init
												, Slice!DString.init
												, BoundFunction.Hashes( fnv1a_64( FullName ), fnv1a_64( Signature ) )
												, pFunctionCDecl
												, pFunctionCPPDecl
												, iIntroducedVersion
												, 0
												, BoundFunction.Resolution.Exported
												, BoundFunction.CallingConvention.CPP
												, cast( BoundFunction.FunctionKind )( FunctionKind | BoundFunctionReturnTypeKind!( Descriptor.ReturnType ) )
												, Descriptor.IsOverride ? BoundFunction.Flags.IsOverride : BoundFunction.Flags.None
												, &BoundFunctionFunctions!( Descriptor ).CPrototype
												, &BoundFunctionFunctions!( Descriptor ).DPrototype
												, &BoundFunctionFunctions!( Descriptor ).CSharpPrototype
												, &BoundFunctionFunctions!( Descriptor ).CSharpMarshalledPrototype
												, &BoundFunctionFunctions!( Descriptor ).ParameterNames
												, &BoundFunctionFunctions!( Descriptor ).CParameterTypes
												, &BoundFunctionFunctions!( Descriptor ).DParameterTypes
												, &BoundFunctionFunctions!( Descriptor ).CSharpParameterTypes
												, &BoundFunctionFunctions!( Descriptor ).CSharpReturnType
												, &BoundFunctionFunctions!( Descriptor ).CSharpMarshalledParameterTypes
												, &BoundFunctionFunctions!( Descriptor ).CSharpMarshalledReturnType
												, &BoundFunctionFunctions!( Descriptor ).CSharpParameterNamesWithQualifiers
											);
			}
		}

		void handleVariable( Type, size_t TupleIndex )()
		{
			alias VariableDesc = VariableDescriptor!( Type.tupleof[ TupleIndex ] );
			alias GeneratedFuncs = CPPFunctionGenerator!VariableDesc;
			alias GetterDesc = FunctionDescriptor!( GeneratedFuncs.GetterCPPDecl );
			alias SetterDesc = FunctionDescriptor!( GeneratedFuncs.SetterCPPDecl );

			enum FullNameGetter = VariableDesc.FullyQualifiedName ~ "_Getter";
			enum SignatureGetter	= FunctionString!( GetterDesc ).CSignature;

			enum FullNameSetter = VariableDesc.FullyQualifiedName ~ "_Setter";
			enum SignatureSetter	= FunctionString!( SetterDesc ).CSignature;

			enum FunctionKind = cast(BoundFunction.FunctionKind)( BoundFunction.FunctionKind.CodeGenerated | BoundFunction.FunctionKind.Static | BoundFunction.FunctionKind.Property );

			//pragma( msg, "Exporting " ~ VariableDesc.FullyQualifiedName ~ " Properties: " );
			//pragma( msg, " -> Getter: " ~ FullNameGetter ~ " => " ~ SignatureGetter );
			//pragma( msg, " -> Setter: " ~ FullNameSetter ~ " => " ~ SignatureSetter );
			//pragma( msg, " -> C# Getter Declaration: " ~ BoundFunctionFunctions!( GetterDesc ).CSharpPrototype() );
			//pragma( msg, " -> C# Setter Declaration: " ~ BoundFunctionFunctions!( SetterDesc ).CSharpPrototype() );

			foundExports ~= BoundFunction( DString( FullNameGetter )
											, DString( SignatureGetter )
											, DString( "" )
											, DString( "" )
											, Slice!DString.init
											, Slice!DString.init
											, BoundFunction.Hashes( fnv1a_64( FullNameGetter ), fnv1a_64( SignatureGetter ) )
											, &GeneratedFuncs.GetterCDecl
											, &GeneratedFuncs.GetterCPPDecl
											, 1 // HORRIBLE HACK
											, 0
											, BoundFunction.Resolution.Exported
											, BoundFunction.CallingConvention.CPP
											, cast( BoundFunction.FunctionKind )( FunctionKind | BoundFunctionReturnTypeKind!( GetterDesc.ReturnType ) )
											, BoundFunction.Flags.None
											, &BoundFunctionFunctions!( GetterDesc ).CPrototype
											, &BoundFunctionFunctions!( GetterDesc ).DPrototype
											, &BoundFunctionFunctions!( GetterDesc ).CSharpPrototype
											, &BoundFunctionFunctions!( GetterDesc ).CSharpMarshalledPrototype
											, &BoundFunctionFunctions!( GetterDesc ).ParameterNames
											, &BoundFunctionFunctions!( GetterDesc ).CParameterTypes
											, &BoundFunctionFunctions!( GetterDesc ).DParameterTypes
											, &BoundFunctionFunctions!( GetterDesc ).CSharpParameterTypes
											, &BoundFunctionFunctions!( GetterDesc ).CSharpReturnType
											, &BoundFunctionFunctions!( GetterDesc ).CSharpMarshalledParameterTypes
											, &BoundFunctionFunctions!( GetterDesc ).CSharpMarshalledReturnType
											, &BoundFunctionFunctions!( GetterDesc ).CSharpParameterNamesWithQualifiers
										);

			foundExports ~= BoundFunction( DString( FullNameSetter )
											, DString( SignatureSetter )
											, DString( "" )
											, DString( "" )
											, Slice!DString.init
											, Slice!DString.init
											, BoundFunction.Hashes( fnv1a_64( FullNameSetter ), fnv1a_64( SignatureSetter ) )
											, &GeneratedFuncs.SetterCDecl
											, &GeneratedFuncs.SetterCPPDecl
											, 1 // HORRIBLE HACK
											, 0
											, BoundFunction.Resolution.Exported
											, BoundFunction.CallingConvention.CPP
											, cast( BoundFunction.FunctionKind )( FunctionKind | BoundFunctionReturnTypeKind!( SetterDesc.ReturnType ) )
											, BoundFunction.Flags.None
											, &BoundFunctionFunctions!( SetterDesc ).CPrototype
											, &BoundFunctionFunctions!( SetterDesc ).DPrototype
											, &BoundFunctionFunctions!( SetterDesc ).CSharpPrototype
											, &BoundFunctionFunctions!( SetterDesc ).CSharpMarshalledPrototype
											, &BoundFunctionFunctions!( SetterDesc ).ParameterNames
											, &BoundFunctionFunctions!( SetterDesc ).CParameterTypes
											, &BoundFunctionFunctions!( SetterDesc ).DParameterTypes
											, &BoundFunctionFunctions!( SetterDesc ).CSharpParameterTypes
											, &BoundFunctionFunctions!( SetterDesc ).CSharpReturnType
											, &BoundFunctionFunctions!( SetterDesc ).CSharpMarshalledParameterTypes
											, &BoundFunctionFunctions!( SetterDesc ).CSharpMarshalledReturnType
											, &BoundFunctionFunctions!( SetterDesc ).CSharpParameterNamesWithQualifiers
										);
		}

		void handleVariables( Type )()
		{
			static foreach( iIndex; 0 .. Type.tupleof.length )
			{
				static if( VariableDescriptor!( Type.tupleof[ iIndex ] ).PrivacyLevel == PrivacyLevel.Public )
				{
					pragma( msg, "Handling variable " ~ __traits( identifier, Type.tupleof[ iIndex ] ) );
					handleVariable!( Type, iIndex );
				}
			}
		}

		foreach( SymbolName; Symbols )
		{
			mixin( "enum CompilesGetOverloads = __traits( compiles, __traits( getOverloads, Scope, \"" ~ SymbolName ~ "\" ) );" );
			mixin( "enum CompilesIsAggregate = __traits( compiles, IsAggregateType!( " ~ FullTypeName!Scope ~ "." ~ SymbolName ~ " ) );" );

			static if( CompilesGetOverloads )
			{
				mixin( "enum NumOverloads = __traits( getOverloads, Scope, \"" ~ SymbolName ~ "\" ).length;" );
			}
			else
			{
				enum NumOverloads = 0;
			}

			static if( CompilesIsAggregate )
			{
				mixin( "enum IsAggregate = IsAggregateType!( " ~ FullTypeName!Scope ~ "." ~ SymbolName ~ " );" );
			}
			else
			{
				enum IsAggregate = false;
			}

			static if( SymbolName == "Monitor"
						|| SymbolName == "Object" )
			{
				//pragma( msg, "Skipping " ~ Scope.stringof ~ "." ~ SymbolName ~ "..." );
			}
			else
			{
				static if( NumOverloads > 0 )
				{
					static if( ExportAllFound )
					{
						enum UDA = BindExport( IntroducedVersion, -1 );
					}
					else
					{
						alias UDA = GetUDA!( __traits( getOverloads, Scope, SymbolName )[ 0 ], BindExport );
					}

					static if( is( UDA : void ) && Options.ExportUntagged )
					{
						enum ExportData = BindExport( 1, -1 );
					}
					else
					{
						alias ExportData = UDA;
					}

					static if( !is( ExportData : void ) )
					{
						static foreach( iIndex, CurrFuncSymbol; __traits( getOverloads, Scope, SymbolName ) )
						{
							static if( IsExternallyAccessible!CurrFuncSymbol )
							{
								static if( !ScopeIsAggregate )
								{
									handleFunction!( FunctionDescriptor!( CurrFuncSymbol, iIndex ) )( ExportData.iIntroducedVersion );
								}
								else
								{
									handleFunction!( FunctionDescriptor!( Scope, SymbolName, iIndex ) )( ExportData.iIntroducedVersion );
								}
							}
						}
					}
				}
				else static if( IsAggregate && !IsAlias!( Scope, SymbolName ) )
				{
					mixin( "alias Symbol = " ~ FullTypeName!Scope ~ "." ~ SymbolName ~ ";" );

					static if( ExportAllFound )
					{
						enum UDA = BindExport( IntroducedVersion, -1 );
					}
					else
					{
						alias FoundUDA = GetUDA!( Symbol, BindExport );

						static if( is( FoundUDA : void ) && Options.ExportUntagged )
						{
							enum UDA = BindExport( 1, -1 );
						}
						else
						{
							alias UDA = FoundUDA;
						}
					}

					static if( !is( UDA : void ) )
					{
						enum NewIntroducedVersion = UDA.iIntroducedVersion;
						foundExports ~= functionGrabber!( NewIntroducedVersion, Symbol, __traits( allMembers, Symbol ) )();
						static if( is( Symbol == class ) )
						{
							handleVariables!( Symbol )();
						}
					}
				}
			}
		}

		return foundExports;
	}

	static if( ExportTypes.length == 0 )
	{
		return functionGrabber!( BindExport.iIntroducedVersion.init, Parent, __traits( allMembers, Parent ) );
	}
	else
	{
		return functionGrabber!( BindExport.iIntroducedVersion.init, Parent, ExportTypes[ 0 ].stringof );
	}
}
//----------------------------------------------------------------------------

public void registerModule( ref BoundModule thisModule )
{
	registeredModules ~= thisModule;
}
//----------------------------------------------------------------------------

public void registerImportFunctions( BoundFunction[] imports )
{
	foreach( iCurrIndex, ref forImport; imports )
	{
		importFunctions[ forImport.functionHashes ] ~= forImport;
	}
}
//----------------------------------------------------------------------------

public void registerExportedFunctions( BoundFunction[] exports )
{
	exportFunctions.reserve( exportFunctions.length + exports.length );

	foreach( ref forExport; exports )
	{
		auto found = ( forExport.functionHashes in exportFunctionIndices );
		if( found !is null )
		{
			assert( found is null, "Hash collision with exported function! \"" ~ cast(string)forExport.strFunctionName ~ "\" with signature \"" ~ cast(string)forExport.strFunctionSignature ~ "\" <-> \"" ~ cast(string)exportFunctions[ *found ].strFunctionName ~ "\" with signature \"" ~ cast(string)exportFunctions[ *found ].strFunctionSignature ~ "\"" );
		}

		size_t uIndex = exportFunctions.length;
		exportFunctions ~= forExport;
		exportFunctionIndices[ forExport.functionHashes ] = uIndex;
	}
}
//----------------------------------------------------------------------------

public void registerExportedObjects( BoundObject[] exports )
{
	exportObjects.reserve( exportObjects.length + exports.length );

	foreach( ref forExport; exports )
	{
		auto found = ( forExport.uFullyQualifiedNameHash in exportObjectIndices );
		if( found !is null )
		{
			assert( found is null, "Hash collision with exported object! \"" ~ cast(string)forExport.strFullyQualifiedName ~ "\" <-> \"" ~ cast(string)exportObjects[ *found ].strFullyQualifiedName ~ "\"" );
		}

		size_t uIndex = exportObjects.length;
		exportObjects ~= forExport;
		exportObjectIndices[ forExport.uFullyQualifiedNameHash ] = uIndex;
	}
}
//----------------------------------------------------------------------------

public void registerExportedEnums( BoundEnum[] exports )
{
	exportEnums ~= exports;
}
//----------------------------------------------------------------------------

public string[] generateCPPStyleExportDeclaration( BoundFunction[] functions )
{
	import std.string;
	import std.algorithm;
	import std.conv;

	string[] splitSignature( string signature )
	{
		auto result = signature.replace( "()", "" )
			.replace( "(", ", " )
			.replace( ")", "" )
			.split( ", " );

		return result;
	}

	string[] outputs;

	string[] includes;

/*	outputs ~=	"#ifdef private\n"
				"  #undef private\n"
				"#endif\n"
				"\n"
				"#ifdef protected\n"
				"  #undef protected\n"
				"#endif\n"
				"\n"
				"// Dirty hack\n"
				"#define _ALLOW_KEYWORD_MACROS\n"
				"#define private public\n"
				"#define protected public";*/

	outputs ~=	"#include \"binderoo/defs.h\"\n"
				~ "#include \"binderoo/exports.h\"";

	foreach( ref boundFunction; functions )
	{
		if( boundFunction.strRequiredInclude.Length > 0 )
		{
			string fullName = cast( string )boundFunction.strRequiredInclude;

			if( !includes.canFind( fullName ) )
			{
				includes ~= fullName;
			}
		}
	}

	outputs ~= includes.joinWith( "#include \"", "\"", "\n" );

	size_t[][ string ] functionsByClass;

	foreach( iIndex, ref boundFunction; functions )
	{
		if( ( boundFunction.eFlags & BoundFunction.Flags.OwnerIsAbstract ) != BoundFunction.Flags.OwnerIsAbstract )
		{
			string className = cast( string )boundFunction.strOwningClass;

			functionsByClass[ className ] ~= iIndex;
		}
	}

	foreach( foundClass; functionsByClass.byKeyValue )
	{
		string strClass = foundClass.key;
		string strClassWithoutNamespaces = strClass.replace( "::", "_" );

		string strClassUnqualifiedName = strClass;
		auto found = strClassUnqualifiedName.lastIndexOf( ':' );
		if( found > 0 && strClassUnqualifiedName[ found - 1 ] == ':' )
		{
			strClassUnqualifiedName = strClass[ found + 1 .. $ ];
		}

		string[] definitionLines;

		string strExportVersion = "iExportVersion_" ~ strClassWithoutNamespaces;
		string strNumExportedMethods = "numExportedMethods_" ~ strClassWithoutNamespaces;
		string strVTableOf = "vtableOf_" ~ strClassWithoutNamespaces;
		string strExportedMethods = "exportedMethods_" ~ strClassWithoutNamespaces;
		string strExportedClass = "exportedClass_" ~ strClassWithoutNamespaces;

		definitionLines ~= "static const int " ~ strExportVersion ~ " = 1; // VERSION NUMBER IS A HACK";
		definitionLines ~= "static size_t " ~ strNumExportedMethods ~ " = " ~ to!string( foundClass.value.length ) ~ ";";

		bool bFoundVirtuals = false;

		if( foundClass.value.length == 0 )
		{
			definitionLines ~= "static binderoo::ExportedMethod* " ~ strExportedMethods ~ " = nullptr;";
		}
		else
		{
			string[] exportedMethods;

			foreach( iIndex; foundClass.value )
			{
				BoundFunction func = functions[ iIndex ];

				if( ( func.eFunctionKind & BoundFunction.FunctionKind.Virtual ) && !( func.eFunctionKind & BoundFunction.FunctionKind.Destructor ) )
				{
					bFoundVirtuals = true;
					exportedMethods ~= "\tbinderoo::ExportedMethod( \"" ~ cast( string )func.strFunctionName ~ "\", \"" ~ cast( string )func.strFunctionSignature ~ "\", " ~ strVTableOf ~ "[ " ~ to!string( func.iOrderInTable ) ~ " ] ),";
				}
				else
				{
/+					auto strOriginalSig = cast( string )func.strFunctionSignature;
					auto foundOpenBrackets = strOriginalSig.indexOf( '(' );
					auto strReturnType = strOriginalSig[ 0 .. foundOpenBrackets ];
					auto foundCloseBrackets = strOriginalSig.indexOf( ')' );
					auto strParameters = strOriginalSig[ foundOpenBrackets .. foundCloseBrackets + 1 ];

					string strTypeCast;
					if( func.eFunctionKind & BoundFunction.FunctionKind.Static )
					{
						strTypeCast = "( " ~ strReturnType ~ "(*)" ~ strParameters ~ " )";
					}
					else
					{
						strTypeCast = "( " ~ strReturnType ~ "(" ~ strClass ~ "::*)" ~ strParameters;
						if( func.eFlags & BoundFunction.Flags.Const )
						{
							strTypeCast ~= " const";
						}
						strTypeCast ~= " )";
					}+/

					string strTypeCast = "(" ~ cast( string )func.strFunctionSignature ~ ")";

					if( func.eFunctionKind & BoundFunction.FunctionKind.Constructor )
					{
						definitionLines ~= "static void constructor_" ~ strClassWithoutNamespaces ~ "( " ~ strClass ~ "* pObj ) { new( pObj ) " ~ strClass ~ "; }";
						exportedMethods ~= "\tbinderoo::ExportedMethod( \"" ~ cast( string )func.strFunctionName ~ "\", \"" ~ cast( string )func.strFunctionSignature ~ "\", ( void(*)( " ~ strClass ~ "* ) )&constructor_" ~ strClassWithoutNamespaces ~ " ),";
					}
					else if( func.eFunctionKind & BoundFunction.FunctionKind.Destructor )
					{
						definitionLines ~= "static void destructor_" ~ strClassWithoutNamespaces ~ "( " ~ strClass ~ "* pObj ) { pObj->~" ~ strClassUnqualifiedName ~ "(); }";
						exportedMethods ~= "\tbinderoo::ExportedMethod( \"" ~ cast( string )func.strFunctionName ~ "\", \"" ~ cast( string )func.strFunctionSignature ~ "\", ( void(*)( " ~ strClass ~ "* ) )&destructor_" ~ strClassWithoutNamespaces ~ " ),";
					}
					else
					{
						exportedMethods ~= "\tbinderoo::ExportedMethod( \"" ~ cast( string )func.strFunctionName ~ "\", \"" ~ cast( string )func.strFunctionSignature ~ "\", " ~ strTypeCast ~ "&" ~ cast(string)func.strFunctionName ~ " ),";
					}
				}
			}

			if( bFoundVirtuals )
			{
				definitionLines ~= "static void** getVTable_" ~ strClassWithoutNamespaces ~ "() { " ~ foundClass.key ~ " thisInstance; return *(void***)&thisInstance; }";
				definitionLines ~= "static void** " ~ strVTableOf ~ " = getVTable_" ~ strClassWithoutNamespaces ~ "();";
			}

			definitionLines ~= "static binderoo::ExportedMethod " ~ strExportedMethods ~ "[] =";
			definitionLines ~= "{";
			definitionLines ~= exportedMethods;
			definitionLines ~= "};";
		}

		definitionLines ~= "static binderoo::ExportedClass " ~ strExportedClass ~ "( " ~ strExportVersion ~ ", binderoo::DString( \"" ~ foundClass.key ~ "\" ), binderoo::DString( \"<unknown>\" ), binderoo::ExportedMethods( " ~ strExportedMethods ~ ", " ~ strNumExportedMethods ~ " ) );";

		outputs ~= definitionLines.joinWith( "\n" );
	}

	return outputs;
}
//----------------------------------------------------------------------------

public string generateCPPStyleExportDeclarationsForAllObjects( string strVersion )
{
	BoundFunction[] functions;

	foreach( currFunctions; importFunctions.byValue )
	{
		functions ~= currFunctions[ 0 ];
	}

	import std.algorithm : sort, filter, map, canFind;
	import std.array : array;

	if( strVersion.length > 0 )
	{
		functions = functions.filter!( a => ( a.strIncludeVersions.Length == 0 || a.strIncludeVersions.toSlice.map!( a => a.toSlice ).array.canFind( strVersion ) )
											&& ( a.strExcludeVersions.Length == 0 || !a.strExcludeVersions.toSlice.map!( a => a.toSlice ).array.canFind( strVersion ) ) )
					.array;
	}

	functions.sort!( ( a, b ) => a.iOrderInTable < b.iOrderInTable )();

	string[] declarations = generateCPPStyleExportDeclaration( functions );

	import std.stdio;
	foreach( decl; declarations )
	{
		writeln( decl, "\n//----------------------------------------------------------------------------\n" );
	}

	return declarations.joinWith( "\n//----------------------------------------------------------------------------\n" );
}
//----------------------------------------------------------------------------

public string generateCSharpStyleImportDeclarationsForAllObjects( string strVersion )
{
	import std.stdio : writeln;
	import std.array : split, replace;
	import std.conv : to;
	import std.string : startsWith, endsWith;

	class CSharpObject
	{
		enum Type : int
		{
			Root = -1,
			Namespace = 0,
			StaticClass = 1,
			Struct = 2,
			Class = 3,
			Enum = 4,
		}

		this( Type t, string n = "root", string f = "root", CSharpObject p = null )
		{
			eType = t;
			strName = n;
			strFullName = f;
			parent = p;
		}

		Type						eType;
		string 						strName;
		string						strFullName;
		string						strInheritsFrom;
		string[]					strTypeDecl;
		string[]					strVariables;
		BoundFunction*[]			arrFunctions;
		BoundEnum*					pThisEnum;
		CSharpObject[ string ]		subTypes;
		CSharpObject				parent;
	}

	struct PropertyFunc
	{
		string			Name;
		BoundFunction*	Getter;
		size_t			GetterIndex;
		BoundFunction*	Setter;
		size_t			SetterIndex;
	}

	enum Separator = "//----------------------------------------------------------------------------";
	enum Blank = "";

	string generateTabs( int depth )
	{
		string strTabs = "";
		foreach ( thisDepth; 0 .. depth )
		{
			strTabs ~= '\t';
		}

		return strTabs;
	}

	string getFunctionPointerName( BoundFunction* func, size_t iIndex )
	{
		string nonIndexed = "F_" ~ ( cast(string)func.strFunctionName ).replace( ".", "_" );
		if( ( func.eFunctionKind & BoundFunction.FunctionKind.CodeGenerated ) == 0 )
		{
			return nonIndexed ~ iIndex.to!string;
		}
		return nonIndexed;
	}

	string getDelegateTypeName( BoundFunction* func, size_t iIndex )
	{
		string nonIndexed = "D_" ~ ( cast(string)func.strFunctionName ).replace( ".", "" );
		if( ( func.eFunctionKind & BoundFunction.FunctionKind.CodeGenerated ) == 0 )
		{
			return nonIndexed ~ iIndex.to!string;
		}
		return nonIndexed;
	}

	string getPropertyName( BoundFunction* func, size_t iIndex )
	{
		string nonIndexed = ( cast(string)func.strFunctionName ).replace( ".", "_" );
		if( ( func.eFunctionKind & BoundFunction.FunctionKind.CodeGenerated ) == 0 )
		{
			return nonIndexed ~ iIndex.to!string;
		}
		return nonIndexed;
	}

	string[] generateCSharpFunctionPointer( CSharpObject thisObj, BoundFunction* func, size_t iIndex, int depth )
	{
		bool bMemberFunc = cast(bool)( func.eFunctionKind & BoundFunction.FunctionKind.Method );
		bool bStaticFunc = cast(bool)( func.eFunctionKind & BoundFunction.FunctionKind.Static );
		string[] strParameterNames = func.CSharpParameterNamesWithQualifiers();

		string strObjTabs = generateTabs( depth );
		string strContentTabs = strObjTabs ~ '\t';

		string strImportedFuncVarName = getFunctionPointerName( func, iIndex );
		string strDelegateType = getDelegateTypeName( func, iIndex );
		string strPropertyName = getPropertyName( func, iIndex );

		string strMarshalledPrototype = func.CSharpMarshalledPrototype();
		if( strMarshalledPrototype.startsWith( "static " ) )
		{
			strMarshalledPrototype = strMarshalledPrototype[ 7 .. $ ];
		}

		string strReturnType = func.CSharpMarshalledReturnType();
		string[] strSplitApart = strMarshalledPrototype.split( '(' );

		string strParameters = "(";
		if( bMemberFunc && !bStaticFunc )
		{
			if( thisObj.eType == CSharpObject.Type.Struct )
			{
				strParameters ~= " ref " ~ thisObj.strFullName ~ " pThis" ~ ( strParameterNames.length > 0 ? "," : "" );
			}
			else
			{
				strParameters ~= " IntPtr pThis" ~ ( strParameterNames.length > 0 ? "," : "" );
			}
		}
		strParameters ~= strSplitApart[ 1 ];

		string[] lines;

		lines ~= strObjTabs ~ "// Function " ~ cast(string)func.strFunctionName;
		//lines ~= strObjTabs ~ "// Signature " ~ cast(string)func.strFunctionSignature;
		//lines ~= strObjTabs ~ "// Base prototype " ~ func.CSharpPrototype();
		//lines ~= strObjTabs ~ "// Marshalled prototype " ~ func.CSharpMarshalledPrototype();
		//lines ~= strObjTabs ~ "// Parameter types " ~ func.CSharpParameterTypes().joinWith( ", " );
		//lines ~= strObjTabs ~ "// Marshalled parameter types " ~ func.CSharpMarshalledParameterTypes().joinWith( ", " );
		lines ~= strObjTabs ~ "public static ImportedFunction " ~ strImportedFuncVarName ~ ";";
		lines ~= strObjTabs ~ "public delegate " ~ strReturnType ~ " " ~ strDelegateType ~ strParameters ~ ";";
		lines ~= strObjTabs ~ "public static " ~ strDelegateType ~ " " ~ strPropertyName;
		lines ~= strObjTabs ~ "{ get {";
		lines ~= strContentTabs ~ "if( " ~ strImportedFuncVarName ~ " == null ) " ~ strImportedFuncVarName ~ " = new ImportedFunction( \"" ~ cast(string)func.strFunctionName ~ "\", \"" ~ cast(string)func.strFunctionSignature ~ "\" );";
		lines ~= strContentTabs ~ "return (" ~ strDelegateType ~ ")Marshal.GetDelegateForFunctionPointer( " ~ strImportedFuncVarName ~ ".FuncPtr, typeof( " ~ strDelegateType ~ " ) );";
		lines ~= strObjTabs ~ "} }";

		return lines;
	}

	string generateCSharpFunctionCallForFunction( BoundFunction* func, size_t iIndex, string[] strParameterNames, bool bIgnoreReturn = false )
	{
		string strPropertyName = getPropertyName( func, iIndex );
		string strReturnType = func.CSharpReturnType();

		if( bIgnoreReturn || strReturnType == "void" || func.eFunctionKind & BoundFunction.FunctionKind.Constructor )
		{
			return "binderoointernal.FP." ~ strPropertyName ~ "( " ~ strParameterNames.joinWith( ", " ) ~ " );";
		}
		else if( strReturnType == "string" )
		{
			return "return new SliceString( binderoointernal.FP." ~ strPropertyName ~ "( " ~ strParameterNames.joinWith( ", " ) ~ " ) ).Data;";
		}
		else if( func.eFunctionKind & BoundFunction.FunctionKind.ReturnsClass )
		{
			return "return new " ~ strReturnType ~ "( binderoointernal.FP." ~ strPropertyName ~ "( " ~ strParameterNames.joinWith( ", " ) ~ " ) );";
		}
		else if( strReturnType.endsWith( "[]" ) )
		{
			return "return new Slice< " ~ strReturnType[ 0 .. $ - 2 ] ~ " >( binderoointernal.FP." ~ strPropertyName ~ "( " ~ strParameterNames.joinWith( ", " ) ~ " ) ).Data;";
		}
		else
		{
			return "return binderoointernal.FP." ~ strPropertyName ~ "( " ~ strParameterNames.joinWith( ", " ) ~ " );";
		}
	}

	string[] generateCSharpForFunction( CSharpObject thisObj, BoundFunction* func, size_t iIndex, int depth )
	{
		bool bMemberFunc = cast(bool)( func.eFunctionKind & BoundFunction.FunctionKind.Method );
		bool bStaticFunc = cast(bool)( func.eFunctionKind & BoundFunction.FunctionKind.Static );
		string[] strParameterNames = func.CSharpParameterNamesWithQualifiers();
		ulong originalParamCount = strParameterNames.length;

		string[] lines;

		string strObjTabs = generateTabs( depth );
		string strContentTabs = strObjTabs ~ '\t';

		string strPrototype = func.CSharpPrototype();
		//if( strPrototype.startsWith( "static " ) )
		//{
		//	strPrototype = strPrototype[ 7 .. $ ];
		//}

		if( bMemberFunc && !bStaticFunc )
		{
			if( thisObj.eType == CSharpObject.Type.Struct )
			{
				strParameterNames = [ "ref this" ] ~ strParameterNames;
			}
			else
			{
				strParameterNames = [ "pObj.Ptr" ] ~ strParameterNames;
			}
		}

		bool isProperty = ( func.eFunctionKind & BoundFunction.FunctionKind.Property ) != 0;

		enum strAccessibility = "public ";

		lines ~= strObjTabs ~ strAccessibility ~ strPrototype;
		lines ~= strObjTabs ~ "{";
		if( isProperty )
		{
			lines ~= strContentTabs ~ "// TODO: THIS IS A PROPERTY";
			lines ~= strContentTabs ~ "// " ~ cast(string)func.strFunctionName;
		}

		lines ~= strContentTabs ~ generateCSharpFunctionCallForFunction( func, iIndex, strParameterNames );

		if( isProperty )
		{
			lines ~= strContentTabs ~ "// TODO: THIS IS A PROPERTY";
		}

		lines ~= strObjTabs ~ "}";
		lines ~= strObjTabs ~ Separator[ 0 .. $ - depth * 4 ];
		lines ~= Blank;
		return lines;
	}

	string[] generateCSharpForProperty( CSharpObject thisObj, ref PropertyFunc property, int depth )
	{
		string[] strParameterNames;
		if( thisObj.eType == CSharpObject.Type.Struct )
		{
			strParameterNames = [ "ref this" ];
		}
		else
		{
			strParameterNames = [ "pObj.Ptr" ];
		}

		string strPrototype;
		if( property.Getter !is null )
		{
			strPrototype = property.Getter.CSharpPrototype();
		}
		else
		{
			strPrototype = property.Setter.CSharpPrototype();
		}

		import std.string : lastIndexOf;
		ptrdiff_t FoundName = strPrototype.lastIndexOf( property.Name );
		strPrototype = strPrototype[ 0 .. FoundName + property.Name.length ];

		string[] lines;

		string strObjTabs = generateTabs( depth );
		string strContentTabs = strObjTabs ~ '\t';

		enum strAccessibility = "public ";

		lines ~= strObjTabs ~ strAccessibility ~ strPrototype;
		lines ~= strObjTabs ~ "{";

		if( property.Getter !is null )
		{
			lines ~= strContentTabs ~ "get { " ~ generateCSharpFunctionCallForFunction( property.Getter, property.GetterIndex, strParameterNames ) ~ " }";
		}

		if( property.Setter !is null )
		{
			strParameterNames ~= "value";
			lines ~= strContentTabs ~ "set { " ~ generateCSharpFunctionCallForFunction( property.Setter, property.SetterIndex, strParameterNames, true ) ~ " }";
		}

		lines ~= strObjTabs ~ "}";
		lines ~= strObjTabs ~ Separator[ 0 .. $ - depth * 4 ];
		lines ~= Blank;
		return lines;
	}

	string[] generateCSharpConstructorsAndDestructors( CSharpObject obj, int depth = 0 )
	{
		string strObjTabs = generateTabs( depth );

		string strSeparator = strObjTabs ~ Separator[ 0 .. $ - depth * 4 ];

		string[] lines;

		lines ~= "#region InternalMagic";
		if( obj.strInheritsFrom.length == 0 )
		{
			lines ~= strObjTabs ~ "public " ~ obj.strName ~ "()";
			lines ~= strObjTabs ~ "{";
			lines ~= strObjTabs ~ "\tpObj = new ImportedClass( \"" ~ obj.strFullName ~ "\" );";
			lines ~= strObjTabs ~ "}";
			lines ~= strSeparator;
			lines ~= Blank;
			lines ~= strObjTabs ~ "public " ~ obj.strName ~ "( IntPtr pInstance )";
			lines ~= strObjTabs ~ "{";
			lines ~= strObjTabs ~ "\tpObj = new ImportedClass( pInstance, \"" ~ obj.strFullName ~ "\" );";
			lines ~= strObjTabs ~ "}";
			lines ~= strSeparator;
			lines ~= Blank;
			lines ~= strObjTabs ~ "protected " ~ obj.strName ~ "( string strClass )";
			lines ~= strObjTabs ~ "{";
			lines ~= strObjTabs ~ "\tpObj = new ImportedClass( strClass );";
			lines ~= strObjTabs ~ "}";
			lines ~= strSeparator;
			lines ~= Blank;
			lines ~= strObjTabs ~ "~" ~ obj.strName ~ "()";
			lines ~= strObjTabs ~ "{";
			lines ~= strObjTabs ~ "\tDispose();";
			lines ~= strObjTabs ~ "}";
			lines ~= strSeparator;
			lines ~= Blank;
			lines ~= strObjTabs ~ "public void Dispose()";
			lines ~= strObjTabs ~ "{";
			lines ~= strObjTabs ~ "\tif( pObj != null )";
			lines ~= strObjTabs ~ "\t{";
			lines ~= strObjTabs ~ "\t\tpObj.Dispose();";
			lines ~= strObjTabs ~ "\t\tpObj = null;";
			lines ~= strObjTabs ~ "\t}";
			lines ~= strObjTabs ~ "}";
			lines ~= strSeparator;
		}
		else
		{
			lines ~= strObjTabs ~ "public " ~ obj.strName ~ "()";
			lines ~= strObjTabs ~ "\t: base( \"" ~ obj.strFullName ~ "\" )";
			lines ~= strObjTabs ~ "{";
			lines ~= strObjTabs ~ "}";
			lines ~= strSeparator;
			lines ~= Blank;
			lines ~= strObjTabs ~ "protected " ~ obj.strName ~ "( string strClass )";
			lines ~= strObjTabs ~ "\t: base( strClass )";
			lines ~= strObjTabs ~ "{";
			lines ~= strObjTabs ~ "}";
			lines ~= strSeparator;
			lines ~= Blank;
		}
		lines ~= "#endregion";

		return lines;
	}

	string[] generateCSharpForObject( CSharpObject obj, int depth = 0 )
	{
		string[] lines;

		string strObjTabs = generateTabs( depth );
		string strSeparatorTabs = strObjTabs ~ "\t" ~ Separator[ 0 .. $ - ( depth + 1 ) * 4 ];

		final switch( obj.eType ) with( CSharpObject.Type )
		{
			case Root:
				lines ~= "using System;";
				lines ~= "using System.Runtime.InteropServices;";
				lines ~= "using binderoo;";
				lines ~= Separator;
				lines ~= Blank;

				foreach( subObj; obj.subTypes.byValue )
				{
					lines ~= generateCSharpForObject( subObj, depth );
				}
				break;

			case Namespace:
				if( depth == 0)
				{
					lines ~= "namespace " ~ obj.strName;
				}
				else
				{
					lines ~= strObjTabs ~ "public static class " ~ obj.strName;
				}
				lines ~= strObjTabs ~ "{";
				foreach( subObj; obj.subTypes.byValue )
				{
					lines ~= generateCSharpForObject( subObj, depth + 1 );
				}
				foreach( iIndex, func; obj.arrFunctions )
				{
					lines ~= generateCSharpForFunction( obj, func, iIndex, depth + 1 );
				}
				lines ~= strObjTabs ~ "}";
				lines ~= strObjTabs ~ Separator[ 0 .. $ - depth * 4 ];
				lines ~= Blank;
				break;

			case StaticClass:
				lines ~= strObjTabs ~ "public static class " ~ obj.strName;
				lines ~= strObjTabs ~ "{";
				foreach( subObj; obj.subTypes.byValue )
				{
					lines ~= generateCSharpForObject( subObj, depth + 1 );
				}
				lines ~= strSeparatorTabs;
				lines ~= Blank;
				foreach( iIndex, func; obj.arrFunctions )
				{
					lines ~= generateCSharpForFunction( obj, func, iIndex, depth + 1 );
				}
				lines ~= strObjTabs ~ "}";
				lines ~= strObjTabs ~ Separator[ 0 .. $ - depth * 4 ];
				lines ~= Blank;
				break;

			case Enum:
				lines ~= strObjTabs ~ "public enum " ~ obj.strName ~ " : " ~ cast(string)obj.pThisEnum.strUnderlyingTypeCSharp;
				lines ~= strObjTabs ~ "{";
				foreach( ref enumVal; obj.pThisEnum.vecAllValues.toSlice )
				{
					lines ~= strObjTabs ~ "\t" ~ cast(string)enumVal.strIdentifier ~ " = " ~ cast(string)enumVal.strValue ~ ",";
				}
				lines ~= strObjTabs ~ "}";
				lines ~= strObjTabs ~ Separator[ 0 .. $ - depth * 4 ];
				lines ~= Blank;
				break;

			case Class:
				goto case Struct;
			
			case Struct:
				foreach( strTok; obj.strTypeDecl )
				{
					lines ~= strObjTabs ~ strTok;
				}
				lines ~= strObjTabs ~ "{";
				foreach( subObj; obj.subTypes.byValue )
				{
					lines ~= generateCSharpForObject( subObj, depth + 1 );
				}
				lines ~= strSeparatorTabs;
				lines ~= Blank;

				PropertyFunc[ string ] FoundProperties;
				
				lines ~= strObjTabs ~ "\t// Methods";
				lines ~= strSeparatorTabs;
				lines ~= Blank;

				foreach( iIndex, func; obj.arrFunctions )
				{
					if( ( func.eFunctionKind & BoundFunction.FunctionKind.CodeGenerated ) == 0 )
					{
						bool isProperty = ( func.eFunctionKind & BoundFunction.FunctionKind.Property ) != 0;
						if( isProperty )
						{
							string strName = cast(string)func.strFunctionName;
							bool bGetter = func.ParameterNames().length == 0;
							PropertyFunc* found = strName in FoundProperties;
							if( found !is null )
							{
								bGetter ? found.Getter = func : found.Setter = func;
								bGetter ? found.GetterIndex = iIndex : found.SetterIndex = iIndex;
							}
							else
							{
								import std.string : lastIndexOf;

								PropertyFunc newProperty;
								newProperty.Name = strName[ strName.lastIndexOf( '.' ) + 1 .. $ ];
								bGetter ? newProperty.Getter = func : newProperty.Setter = func;
								bGetter ? newProperty.GetterIndex = iIndex : newProperty.SetterIndex = iIndex;
								FoundProperties[ strName ] = newProperty;
							}
						}
						else
						{
							lines ~= generateCSharpForFunction( obj, func, iIndex, depth + 1 );
						}
					}
				}
				lines ~= strSeparatorTabs;
				lines ~= Blank;

				PropertyFunc[] AllProperties = FoundProperties.values;
				if( AllProperties.length )
				{
					lines ~= strObjTabs ~ "\t// Properties";
					lines ~= strSeparatorTabs;
					lines ~= Blank;
					foreach( ref Property; AllProperties )
					{
						lines ~= generateCSharpForProperty( obj, Property, depth + 1 );
					}
				}

				foreach( strTok; obj.strVariables )
				{
					lines ~= strObjTabs ~ strTok;
				}
				lines ~= strSeparatorTabs;
				lines ~= Blank;
				if( obj.eType == CSharpObject.Type.Class )
				{
					lines ~= generateCSharpConstructorsAndDestructors( obj, depth + 1 );
				}
				lines ~= strObjTabs ~ "}";
				lines ~= strObjTabs ~ Separator[ 0 .. $ - depth * 4 ];
				lines ~= Blank;
				break;
		}

		return lines;
	}

	string[] generateCSharpFunctionPointerSingleton( CSharpObject root )
	{
		string[] handleObject( CSharpObject obj )
		{
			string[] lines;

			foreach( iIndex, func; obj.arrFunctions )
			{
				lines ~= generateCSharpFunctionPointer( obj, func, iIndex, 2 );
				lines ~= Blank;
			}

			foreach( subObj; obj.subTypes.byValue )
			{
				lines ~= handleObject( subObj );
			}

			return lines;
		}

		string[] lines;

		lines ~= Blank;
		lines ~= Separator;
		lines ~= Separator;
		lines ~= "#region InternalMagic";
		lines ~= "namespace binderoointernal";
		lines ~= "{";
		lines ~= "\tpublic static class FP";
		lines ~= "\t{";
		lines ~= handleObject( root );
		lines ~= "\t}";
		lines ~= "}";
		lines ~= "#endregion";
		lines ~= Separator;
		lines ~= Separator;

		return lines;
	}

/*	CSharpObject resolveObject( CSharpObject currObj, string[] strObjNameSplit )
	{
		while( strObjNameSplit.length > 0 )
		{
			const( CSharpObject* ) thisObj = strObjNameSplit[ 0 ] in currObj.subTypes;
			if( thisObj is null )
			{
				CSharpObject newObj = new CSharpObject ( CSharpObject.Type.Namespace, strObjNameSplit[ 0 ], strObjFullName, currObj );
				currObj.subTypes[ newObj.strName ] = newObj;
			}

			currObj = currObj.subTypes[ strObjNameSplit[ 0 ] ];
			strObjNameSplit = strObjNameSplit[ 1 .. $ ];
		}

		return currObj;
	}*/

	CSharpObject root = new CSharpObject( CSharpObject.Type.Root );

	foreach( ref currExportedObj; exportObjects )
	{
		string strObjFullName = cast(string)currExportedObj.strFullyQualifiedName;
		string[] strObjNameSplit = strObjFullName.split( '.' );

		if( strObjNameSplit.length > 0 && strObjNameSplit[ 0 ] == "binderoo" )
		{
			continue;
		}

		CSharpObject currObj = root;

		while( strObjNameSplit.length > 1 )
		{
			const( CSharpObject* ) thisObj = strObjNameSplit[ 0 ] in currObj.subTypes;
			if( thisObj is null )
			{
				CSharpObject newObj = new CSharpObject ( CSharpObject.Type.Namespace, strObjNameSplit[ 0 ], strObjFullName, currObj );
				currObj.subTypes[ newObj.strName ] = newObj;
			}

			currObj = currObj.subTypes[ strObjNameSplit[ 0 ] ];
			strObjNameSplit = strObjNameSplit[ 1 .. $ ];
		}

		CSharpObject.Type eNewType = ( currExportedObj.eType == BoundObject.Type.Value ? CSharpObject.Type.Struct : CSharpObject.Type.Class );
		CSharpObject thisObj = new CSharpObject( eNewType, strObjNameSplit[ 0 ], strObjFullName, currObj );
		currObj.subTypes[ thisObj.strName ] = thisObj;

		thisObj.strVariables = currExportedObj.generateCSharpVariables();
		thisObj.strTypeDecl = currExportedObj.generateCSharpTypeDecl();
		if( currExportedObj.hasBaseType() )
		{
			thisObj.strInheritsFrom = currExportedObj.getBaseTypeNameCSharp();
		}
	}

	foreach( ref currFunction; exportFunctions )
	{
		// TODO: Collect UFCS functions
		if( ( currFunction.eFunctionKind & ( BoundFunction.FunctionKind.Constructor | BoundFunction.FunctionKind.Destructor ) ) != BoundFunction.FunctionKind.Undefined )
		{
			continue;
		}
		string strFuncFullName = cast(string)currFunction.strFunctionName;
		string[] strFuncSplitNames = strFuncFullName.split( '.' );

		if( strFuncSplitNames.length > 0 && strFuncSplitNames[ 0 ] == "binderoo" )
		{
			continue;
		}

		CSharpObject currObj = root;
		while( strFuncSplitNames.length > 2 )
		{
			const( CSharpObject* ) thisObj = strFuncSplitNames[ 0 ] in currObj.subTypes;
			if( thisObj is null )
			{
				CSharpObject newObj = new CSharpObject ( CSharpObject.Type.Namespace, strFuncSplitNames[ 0 ], strFuncFullName, currObj );
				currObj.subTypes[ newObj.strName ] = newObj;
			}

			currObj = currObj.subTypes[ strFuncSplitNames[ 0 ] ];
			strFuncSplitNames = strFuncSplitNames[ 1 .. $ ];
		}

		const( CSharpObject* ) thisClass = strFuncSplitNames[ 0 ] in currObj.subTypes;
		if( thisClass is null )
		{
			currObj.subTypes[ strFuncSplitNames[ 0 ] ] = new CSharpObject( CSharpObject.Type.StaticClass, strFuncSplitNames[ 0 ], strFuncFullName, currObj );
		}
		currObj.subTypes[ strFuncSplitNames[ 0 ] ].arrFunctions ~= &currFunction;
	}

	foreach( ref currEnum; exportEnums )
	{
		string strEnumFullName = cast(string)currEnum.strName;
		string[] strEnumNameSplit = strEnumFullName.split( '.' );

		if( strEnumNameSplit.length > 0 && strEnumNameSplit[ 0 ] == "binderoo" )
		{
			continue;
		}

		CSharpObject currObj = root;

		while( strEnumNameSplit.length > 1 )
		{
			const( CSharpObject* ) thisObj = strEnumNameSplit[ 0 ] in currObj.subTypes;
			if( thisObj is null )
			{
				CSharpObject newObj = new CSharpObject ( CSharpObject.Type.Namespace, strEnumNameSplit[ 0 ], strEnumFullName, currObj );
				currObj.subTypes[ newObj.strName ] = newObj;
			}

			currObj = currObj.subTypes[ strEnumNameSplit[ 0 ] ];
			strEnumNameSplit = strEnumNameSplit[ 1 .. $ ];
		}

		CSharpObject thisObj = new CSharpObject( CSharpObject.Type.Enum, strEnumNameSplit[ 0 ], strEnumFullName, currObj );
		currObj.subTypes[ thisObj.strName ] = thisObj;
		
		thisObj.pThisEnum = &currEnum;
	}

	string[] lines 	= [ "// Binderoo generated file - DO NOT MODIFY!",
					"// Regenerate the file if a change is required.",
					Separator,
					Blank ]
					~ generateCSharpForObject( root, 0 )
					~ generateCSharpFunctionPointerSingleton( root );

	foreach( currLine; lines )
	{
		writeln( currLine );
	}

	return lines.joinWith( "\n" );
}
//----------------------------------------------------------------------------

public void generateDInterfaceFiles( string strOutputFolder )
{
	import std.string : replace, split, endsWith;
	import std.file : exists, mkdir;
	import std.stdio : writeln, File;

	enum Separator = "//----------------------------------------------------------------------------";
	enum BlankLine = "";

	string strPath =  strOutputFolder.replace( "\\", "/" );
	if( !strPath.endsWith( '/' ) )
	{
		strPath ~= '/';
	}

	writeln( "Outputting to ", strPath );

	if( !strPath.exists )
	{
		strPath.mkdir();
	}

	foreach( ref thisModule; registeredModules )
	{
		string[] modulesAndTypeName = thisModule.Name.split( '.' );

		string outputPath = strPath;
		foreach( iIndex; 0 .. modulesAndTypeName.length - 1 )
		{
			outputPath ~= modulesAndTypeName[ iIndex ];
			if( !outputPath.exists )
			{
				outputPath.mkdir();
			}
			outputPath ~= "/";
		}

		string outputFileName = outputPath ~ modulesAndTypeName[ $ - 1 ] ~ ".di";

		string[] lines;
		lines ~= "// Binderoo generated interface. DO NOT MODIFY";
		lines ~= BlankLine;
		lines ~= "module " ~ thisModule.Name ~ ";";
		lines ~= Separator;
		lines ~= BlankLine;

		foreach( interfaceText; thisModule.strDInterfaceDecls )
		{
			lines ~= cast(string)interfaceText;
			lines ~= Separator;
			lines ~= BlankLine;
		}

		File outputFile = File( outputFileName, "wb" );
		outputFile.rawWrite( lines.joinWith( "\n" ) );
		outputFile.close();

		writeln( "Generated interface for module ", thisModule.Name, " (", outputFileName, ")" );
	}

}
//----------------------------------------------------------------------------

public auto getAllImportedFunctions()
{
	return importFunctions;
}
//----------------------------------------------------------------------------

public const(BoundFunction)[] getAllExportedFunctions()
{
	return exportFunctions;
}
//----------------------------------------------------------------------------

public const(BoundObject)[] getAllExportedObjects()
{
	return exportObjects;
}
//----------------------------------------------------------------------------

// Module external interface
//----------------------------------------------------------------------------

export extern( C ) void importFunctionsFrom( binderoo.slice.Slice!( BoundFunction ) exports )
/*in
{
	foreach( ref exportedFunction; exports )
	{
		assert( exportedFunction.eResolution == BoundFunction.Resolution.Exported, "Function " ~ cast(string) exportedFunction.strFunctionName ~ " is not marked as exported!" );
		auto foundIndex = ( exportedFunction.functionHashes in importFunctionIndices );
		if( foundIndex !is null )
		{
			assert( importFunctions[ *foundIndex ].eResolution == BoundFunction.Resolution.WaitingForImport, "Function " ~ cast(string) exportedFunction.strFunctionName ~ " is not waiting for import!" );
		}
	}
}
out
{
	foreach( ref importedFunction; importFunctions )
	{
		assert( importedFunction.eResolution == BoundFunction.Resolution.WaitingForImport, "Function " ~ cast(string) importedFunction.strFunctionName ~ " is still waiting for import!" );
	}
}
body*/
{
	import std.stdio;
	import std.conv;

	foreach( ref exportedFunction; exports )
	{
		auto foundArray = ( exportedFunction.functionHashes in importFunctions );
		if( foundArray !is null )
		{
			foreach( ref targetImport; *foundArray )
			{
				void** pTarget = cast(void**)targetImport.pFunctionCPPDecl;
				*pTarget = exportedFunction.pFunctionCPPDecl;
				targetImport.eResolution = BoundFunction.Resolution.Imported;
			}
		}
	}
}
//----------------------------------------------------------------------------

export extern( C ) void getExportedObjects( binderoo.slice.Slice!( BoundObject )* pOutput )
{
	*pOutput = binderoo.slice.Slice!( BoundObject )( exportObjects );
}
//----------------------------------------------------------------------------

export extern( C ) void getExportedFunctions( binderoo.slice.Slice!( BoundFunction )* pOutput )
{
	*pOutput = binderoo.slice.Slice!( BoundFunction )( exportFunctions );
}
//----------------------------------------------------------------------------

export extern( C ) void* createObjectByName( DString name )
{
	return createObjectByHash( fnv1a_64( cast( string ) name ) );
}
//----------------------------------------------------------------------------

export extern( C ) void* createObjectByHash( ulong objectHash )
{
	auto foundIndex = ( objectHash in exportObjectIndices );
	if( foundIndex !is null )
	{
		void* pObj = exportObjects[ *foundIndex ].alloc( 1 );
		return pObj;
	}

	return null;
}
//----------------------------------------------------------------------------

export extern( C ) void destroyObjectByName( DString name, void* pObj )
{
	return destroyObjectByHash( fnv1a_64( cast( string ) name ), pObj );
}
//----------------------------------------------------------------------------

export extern( C ) void destroyObjectByHash( ulong objectHash, void* pObj )
{
	auto foundIndex = ( objectHash in exportObjectIndices );
	if( foundIndex !is null )
	{
		exportObjects[ *foundIndex ].free( pObj );
	}
}
//----------------------------------------------------------------------------

alias BindingRawAllocator = extern( C ) void* function( size_t size );

export extern( C ) const( char )* generateCPPStyleExportDeclarationsForAllObjects( BindingRawAllocator allocator, const( char )* pVersion )
{
	import core.stdc.string;

	string strVersion;
	if( pVersion )
	{
		strVersion = cast(string)pVersion[ 0 .. strlen( pVersion ) ];
	}

	string fullDeclaration = generateCPPStyleExportDeclarationsForAllObjects( strVersion );

	size_t outputSize = fullDeclaration.length + 1;
	char* pOutput = cast(char*)allocator( outputSize );

	if( fullDeclaration.length > 0 )
	{
		memcpy( pOutput, fullDeclaration.ptr, outputSize );
	}

	pOutput[ fullDeclaration.length ] = 0;

	return pOutput;
}
//----------------------------------------------------------------------------

export extern( C ) const( char )* generateCSharpStyleImportDeclarationsForAllObjects( BindingRawAllocator allocator, const( char )* pVersion )
{
	import core.stdc.string;

	string strVersion;
	if( pVersion )
	{
		strVersion = cast(string)pVersion[ 0 .. strlen( pVersion ) ];
	}

	string fullDeclaration = generateCSharpStyleImportDeclarationsForAllObjects( strVersion );

	size_t outputSize = fullDeclaration.length + 1;
	char* pOutput = cast(char*)allocator( outputSize );

	if( fullDeclaration.length > 0 )
	{
		memcpy( pOutput, fullDeclaration.ptr, outputSize );
	}

	pOutput[ fullDeclaration.length ] = 0;

	return pOutput;
}
//----------------------------------------------------------------------------

export extern( C ) void generateDInterfaceFiles( const( char )* pOutputFolder )
{
	import core.stdc.string : strlen;
	generateDInterfaceFiles( cast(string)pOutputFolder[ 0 .. strlen( pOutputFolder ) ] );
}
//----------------------------------------------------------------------------

private:

__gshared BoundModule[]								registeredModules;
__gshared BoundFunction[][ BoundFunction.Hashes ]	importFunctions;
__gshared BoundFunction[]							exportFunctions;
__gshared size_t[ BoundFunction.Hashes ]			exportFunctionIndices;
__gshared BoundObject[]								exportObjects;
__gshared size_t[ ulong ]							exportObjectIndices;
__gshared BoundEnum[]								exportEnums;

//============================================================================
