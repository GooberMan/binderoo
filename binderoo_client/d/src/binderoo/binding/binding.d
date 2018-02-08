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

module binderoo.binding.binding;
//----------------------------------------------------------------------------

public import binderoo.binding.attributes;
public import binderoo.binding.functionstub;
public import binderoo.binding.boundfunction;
public import binderoo.binding.boundobject;
public import binderoo.binding.boundmodule;

public import binderoo.functiondescriptor;
public import binderoo.variabledescriptor;
public import binderoo.hash;
public import binderoo.objectprivacy;
public import binderoo.slice;

private import std.traits;
//----------------------------------------------------------------------------

mixin template BindVersionDeclaration( int iCurrentVersion )
{
	enum			BoundVersion		= iCurrentVersion;
	__gshared int	ImportedVersion		= -1;
}
//----------------------------------------------------------------------------

mixin template BindOnly( Type, int iCurrentVersion = 0, AdditionalStaticThisCalls... )
{
	mixin BindModuleImplementation!( iCurrentVersion, AdditionalStaticThisCalls );

	static void initialiseModuleBinding()
	{
		string thisModuleName = ModuleName!initialiseModuleBinding;

		auto currModule = generateModuleInfo();
		auto theseFunctionsToImport = generateImports!( Type )();
		auto theseFunctionsToExport = generateExports!( Type )();
		auto theseObjectsToExport = generateObjects!( Type )();

		registerModule( currModule );
		registerImportFunctions( theseFunctionsToImport );
		registerExportedFunctions( theseFunctionsToExport );
		registerExportedObjects( theseObjectsToExport );

		thisModule = currModule;
		functionsToImport = theseFunctionsToImport;
		functionsToExport = theseFunctionsToExport;
		objectsToExport = theseObjectsToExport;
	}
	//------------------------------------------------------------------------

}
//----------------------------------------------------------------------------

mixin template BindModule( int iCurrentVersion = 0, AdditionalStaticThisCalls... )
{
	mixin BindModuleImplementation!( iCurrentVersion, AdditionalStaticThisCalls );

	void initialiseModuleBinding()
	{
//		import std.stdio : writeln;
//		writeln( "Initialising ", ThisModuleName, " bindings..." );

		auto currModule = generateModuleInfo();
		auto theseFunctionsToImport = generateImports();
		auto theseFunctionsToExport = generateExports();
		auto theseObjectsToExport = generateObjects();

		registerModule( currModule );
		registerImportFunctions( theseFunctionsToImport );
		registerExportedFunctions( theseFunctionsToExport );
		registerExportedObjects( theseObjectsToExport );

		thisModule = currModule;
		functionsToImport = theseFunctionsToImport;
		functionsToExport = theseFunctionsToExport;
		objectsToExport = theseObjectsToExport;
	}
	//------------------------------------------------------------------------

}
//----------------------------------------------------------------------------

// Module internals
//----------------------------------------------------------------------------
mixin template BindModuleImplementation( int iCurrentVersion = 0, AdditionalStaticThisCalls... )
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

	private:

	__gshared BoundModule 							thisModule;
	__gshared BoundObject[]							objectsToExport;
	__gshared BoundFunction[]						functionsToImport;
	__gshared BoundFunction[]						functionsToExport;
	//------------------------------------------------------------------------

	template ModuleTypeDescriptors( ParentClass, Aliases... )
	{
		import std.typetuple;

		string makeATuple()
		{
			import std.conv : to;
			import binderoo.traits : IsUserType, IsSomeType, joinWith;

			string[] indices;
			foreach( Alias; Aliases )
			{
				static if( !is( ParentClass == void ) )
				{
					enum AliasString = FullTypeName!( ParentClass ) ~ "." ~ Alias;
				}
				else
				{
					enum AliasString = Alias;
				}

				static if( mixin( "__traits( compiles, " ~ AliasString ~ " ) && IsSomeType!( " ~ AliasString ~ " )" ) )
				{
					mixin( "alias Type = " ~ AliasString ~ ";" );
					mixin( "import " ~ ModuleName!( Type ) ~ ";" );

					static if( IsUserTypeButNotEnum!( Type ) && Alias == Type.stringof )
					{
						indices ~= AliasString;
					}
				}
			}

			return "alias ModuleTypeDescriptors = TypeTuple!( " ~ indices.joinWith( ", " ) ~ " );";
		}

		mixin( makeATuple() );
	}
	//------------------------------------------------------------------------

	BoundModule generateModuleInfo( ObjectTypes... )()
	{
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

		string thisModuleName = ModuleName!functionsToImport;

		BoundModule currModule = { thisModuleName, cast(DString[])( [ ] ), thisModuleName.fnv1a_64() };

		static if( ObjectTypes.length == 0 )
		{
			alias ModuleTypes = ModuleTypeDescriptors!( void, __traits( allMembers, mixin( ModuleName!( functionsToImport ) ) ) );
		}
		else
		{
			alias ModuleTypes = ModuleTypeDescriptors!( void, ObjectTypes[ 0 ].stringof );
		}

		currModule.strDInterfaceDecls = Slice!DString( gatherFor!ModuleTypes );

		return currModule;
	}
	//------------------------------------------------------------------------

	BoundObject[] generateObjects( ObjectTypes... )()
	{
		BoundObject[] gatherFor( bool bRecursive, Types... )()
		{
			BoundObject[] objects;
			foreach( Type; Types )
			{
				static if( ( is( Type == struct ) || is( Type == class ) )
							&& !HasUDA!( Type, BindNoExportObject ) )
				{
					objects ~= BoundObject( DString( FullTypeName!( Type ) ),
											fnv1a_64( FullTypeName!( Type ) ),
											&BoundObjectFunctions!( Type ).allocObj,
											&BoundObjectFunctions!( Type ).deallocObj,
											&BoundObjectFunctions!( Type ).thunkObj,
											&BoundObjectFunctions!( Type ).serialiseObj,
											&BoundObjectFunctions!( Type ).deserialiseObj,
											BoundObjectFunctions!( Type ).TypeVal );
				}

				static if( bRecursive )
				{
					alias AllSubTypes = ModuleTypeDescriptors!( Type, __traits( allMembers, Type ) );

					objects ~= gatherFor!( bRecursive, AllSubTypes )();
				}
			}

			return objects;
		}

		static if( ObjectTypes.length == 0 )
		{
			alias ModuleTypes = ModuleTypeDescriptors!( void, __traits( allMembers, mixin( ModuleName!( functionsToImport ) ) ) );
			return gatherFor!( true, ModuleTypes )();
		}
		else
		{
			alias ModuleTypes = ModuleTypeDescriptors!( void, ObjectTypes[ 0 ].stringof );
			return gatherFor!( false, ModuleTypes )();
		}
	}
	//------------------------------------------------------------------------

	BoundFunction[] generateImports( ImportTypes... )()
	{
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
													, DString( "" )
													, DString( "" )
													, DString( "" )
													, DString( "" )
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
													, FoundFlags );
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
			alias Types = ModuleTypeDescriptors!( void, __traits( allMembers, mixin( ModuleName!( functionsToImport ) ) ) );
			return gatherFor!( true, Types )();
		}
		else
		{
			alias Types = ModuleTypeDescriptors!( void, ImportTypes[ 0 ].stringof );
			return gatherFor!( false, Types )();
		}
	}

	BoundFunction[] generateExports( ExportTypes... )()
	{
		BoundFunction[] functionGrabber( Type, Symbols... )()
		{
			BoundFunction[] foundExports;

			foreach( SymbolName; Symbols )
			{
				static if( mixin( "__traits( compiles, " ~ SymbolName ~ " )" ) )
				{
					mixin( "alias Symbol = " ~ SymbolName  ~ ";" );

					static if( isSomeFunction!( Symbol ) && HasUDA!( Symbol, BindExport ) )
					{
						alias Descriptor = FunctionDescriptor!( Symbol );
						alias ExportData = Descriptor.GetUDA!( BindExport );

						static assert( Descriptor.IsCPlusPlusFunction, FullTypeName!( Symbol ) ~ " can only be exported as extern( C++ ) for now." );

						enum FullName = FullTypeName!( Symbol );
						enum Signature = FunctionString!( Descriptor ).CSignature;
						enum DDecl = FunctionString!( Descriptor ).DDecl;
						enum CDecl = FunctionString!( Descriptor ).CDecl;
						enum CSharpDecl = FunctionString!( Descriptor ).CSharpDecl;
						enum ParamNames = FunctionString!( Descriptor ).ParameterNames;

						// pragma( msg, "Exporting " ~ FullName ~ ": " ~ Signature );
						// pragma( msg, " -> D Declaration: " ~ DDecl );
						// pragma( msg, " -> C Declaration: " ~ CDecl );
						// pragma( msg, " -> C# Declaration: " ~ CSharpDecl );
						// pragma( msg, " -> Parameter names: " ~ ParamNames );

						foundExports ~= BoundFunction( DString( FullName )
														, DString( Signature )
														, DString( ParamNames )
														, DString( CDecl )
														, DString( DDecl )
														, DString( CSharpDecl )
														, DString( "" )
														, DString( "" )
														, Slice!DString.init
														, Slice!DString.init
														, BoundFunction.Hashes( fnv1a_64( FullName ), fnv1a_64( Signature ) )
														, mixin( "&" ~ FullTypeName!( Symbol ) )
														, ExportData.iIntroducedVersion
														, 0
														, BoundFunction.Resolution.Exported
														, BoundFunction.CallingConvention.CPP
														, BoundFunction.FunctionKind.Static
														, BoundFunction.Flags.None );
					}
				}
			}

			return foundExports;
		}

		static if( ExportTypes.length == 0 )
		{
			return functionGrabber!( void, __traits( allMembers, mixin( ModuleName!( functionsToImport ) ) ) );
		}
		else
		{
			return functionGrabber!( void, ExportTypes[ 0 ].stringof );
		}
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
					if( func.eFunctionKind == BoundFunction.FunctionKind.Static )
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
	import std.array : split;

	class CSharpObject
	{
		enum Type : int
		{
			Root = -1,
			Namespace = 0,
			StaticClass = 1,
		}

		this( Type t, string n = "root", CSharpObject r = null )
		{
			eType = t;
			strName = n;
			root = r;
		}

		Type						eType;
		string 						strName;
		BoundFunction*[]			arrFunctions;
		CSharpObject[ string ]		subTypes;
		CSharpObject				root;
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

	string[] generateCSharpForFunction( BoundFunction* func, int depth )
	{
		string[] lines;

		string strObjTabs = generateTabs( depth );
		string strContentTabs = strObjTabs ~ '\t';
		string strFuncName = ( cast(string)func.strFunctionName ).split( "." )[ $ - 1 ];
		string strImportedFuncVarName = "func_" ~ strFuncName;
		string strDelegateType = "Delegate" ~ strFuncName;

		string[] strSplitApart = (cast(string)func.strFunctionSignature).split( '(' );
		string strReturnType = strSplitApart[ 0 ];

		strSplitApart = (cast(string)func.strCSharpPrototype).split( '(' );
		string strParameters = "(" ~ strSplitApart[ 1 ];

		lines ~= strObjTabs ~ "// Function " ~ cast(string)func.strFunctionName;
		lines ~= strObjTabs ~ "private static ImportedFunction " ~ strImportedFuncVarName ~ ";";
		lines ~= strObjTabs ~ "private delegate " ~ strReturnType ~ " " ~ strDelegateType ~ strParameters ~ ";";
		lines ~= strObjTabs ~ "public " ~ cast(string)func.strCSharpPrototype;
		lines ~= strObjTabs ~ "{";
		lines ~= strContentTabs ~ "if( " ~ strImportedFuncVarName ~ " == null ) " ~ strImportedFuncVarName ~ " = new ImportedFunction( \"" ~ cast(string)func.strFunctionName ~ "\", \"" ~ cast(string)func.strFunctionSignature ~ "\" );";
		lines ~= strContentTabs ~ strDelegateType ~ " call = (" ~ strDelegateType ~ ")Marshal.GetDelegateForFunctionPointer( " ~ strImportedFuncVarName ~ ".FuncPtr, typeof( " ~ strDelegateType ~ " ) );";
		if( strReturnType == "void" )
		{
			lines ~= strContentTabs ~ "call( " ~ cast(string)func.strParameterNames ~ " );";
		}
		else
		{
			lines ~= strContentTabs ~ "return call(" ~ cast(string)func.strParameterNames ~ " );";
		}
		lines ~= strObjTabs ~ "}";
		lines ~= strObjTabs ~ Separator[ 0 .. $ - depth * 4 ];
		lines ~= Blank;
		return lines;
	}

	string[] generateCSharpForObject( CSharpObject obj, int depth = 0 )
	{
		string[] lines;

		string strObjTabs = generateTabs( depth );

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
				lines ~= strObjTabs ~ "namespace " ~ obj.strName;
				lines ~= strObjTabs ~ "{";
				foreach( subObj; obj.subTypes.byValue )
				{
					lines ~= generateCSharpForObject( subObj, depth + 1 );
				}
				lines ~= strObjTabs ~ "}";
				lines ~= strObjTabs ~ Separator[ 0 .. $ - depth * 4 ];
				lines ~= Blank;
				break;

			case StaticClass:
				lines ~= strObjTabs ~ "public static class " ~ obj.strName;
				lines ~= strObjTabs ~ "{";
				foreach( func; obj.arrFunctions )
				{
					lines ~= generateCSharpForFunction( func, depth + 1 );
				}
				lines ~= strObjTabs ~ "}";
				lines ~= strObjTabs ~ Separator[ 0 .. $ - depth * 4 ];
				lines ~= Blank;
				break;
		}

		return lines;
	}

	CSharpObject root = new CSharpObject( CSharpObject.Type.Root );

	foreach( ref currFunction; exportFunctions )
	{
		string strFuncFullName = cast(string)currFunction.strFunctionName;

		string[] strFuncSplitNames = strFuncFullName.split( '.' );

		CSharpObject currObj = root;

		while( strFuncSplitNames.length > 2 )
		{
			const( CSharpObject* ) thisObj = strFuncSplitNames[ 0 ] in currObj.subTypes;
			if( thisObj is null )
			{
				CSharpObject newObj = new CSharpObject ( CSharpObject.Type.Namespace, strFuncSplitNames[ 0 ], currObj );
				currObj.subTypes[ newObj.strName ] = newObj;
			}

			currObj = currObj.subTypes[ strFuncSplitNames[ 0 ] ];
			strFuncSplitNames = strFuncSplitNames[ 1 .. $ ];
		}

		const( CSharpObject* ) thisClass = strFuncSplitNames[ 0 ] in currObj.subTypes;
		if( thisClass is null )
		{
			currObj.subTypes[ strFuncSplitNames[ 0 ] ] = new CSharpObject( CSharpObject.Type.StaticClass, strFuncSplitNames[ 0 ], currObj );
		}
		currObj.subTypes[ strFuncSplitNames[ 0 ] ].arrFunctions ~= &currFunction;
	}

	string[] lines = generateCSharpForObject( root, 0 );

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
				void** pTarget = cast(void**)targetImport.pFunction;
				*pTarget = exportedFunction.pFunction;
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

//============================================================================
