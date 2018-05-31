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

module binderoo.binding.cheaderparsing;

public import binderoo.binding;
public import binderoo.binding.inheritance;
public import binderoo.binding.cheaderobjects;
import binderoo.traits;

/+version( RegexFinallyWorksAtCompileTimeWhoEverThoughtThatWouldntBeUseful )
{
	enum CRegexStatements : string // Standalone searches are in comments
	{
		HashInclude  					= "(#include\\s*[\"<](.*)[\">])",						// /(#include\s*["<](.*)[">])/
		HashIf 							= "(#if\\b(.*))",										// /(#if\b(.*))/
		HashElif 						= "(#elif\\b(.*))",										// /(#elif\b(.*))/
		HashEndif 						= "(#endif\\b(.*))",									// /(#endif\b(.*))/
		HashIfndef						= "(#ifndef\\b(.*))",									// /(#ifndef\b(.*))/
		HashDefine 						= "(#define\\b(.*))",									// /(#define\b(.*))/
		HashPragma 						= "(#pragma\\b(.*))",									// /(#pragma\b(.*))/
		HashError 						= "(#error\\b(.*))",									// /(#error\b(.*))/

		SingleLineComment				= "\\/\\/(.*)",											// /\/\/(.*)/
		MultiLineComment				= "\\/\\*((?s).*)\\*\\//",								// /\/\*((?s).*)\*\///

		StringLiteral					= "\"([^\"]*)\"",										// /"([^"]*)"/
		IntegralLiteral					= "(0x[0-9a-fA-F]+)|[+-]?([0-9]+)(?!0[xX])",			// Standard: [+-]?([0-9]+)(?!0[xX]) Hex: (0x(?i)[0-9a-f]+)
		FloatingPointLiteral			= "([+-]?\\d*\\.\\d+[f]?)|([+-]?\\d+\\.\\d*[f]?)",		// /([+-]?\d*\.\d+[f]?)|([+-]?\d+\.\d*[f]?)/

		AllComments						= SingleLineComment ~ "|" ~ MultiLineComment,
	}

	import std.regex;

	// Embedding in a struct since I can't do this with an enum directly...
	struct CRegex
	{
		static foreach( thisMember; __traits( allMembers, CRegexStatements ) )
		{
			mixin( "\tenum " ~ thisMember ~ " = regex( cast(string)CRegexStatements." ~ thisMember ~ " );" );
		}
	}

	import std.string;

	string GenerateDObjectsFromHeader( string strHeaderContents, string strHeaderLocation )
	{
		string[] output;

		// Strip comments
		
		string strReplaced = strHeaderLocation.replaceAll!( x => "" )( CRegex.AllComments );

		return strReplaced;
		return output.joinWith( "\n" );
	}

	mixin template ImportCHeader( string strHeaderLocation )
	{
		pragma( msg, GenerateDObjectsFromHeader( import( strHeaderLocation ), strHeaderLocation ) );
	}
}
else :+/
// The awful implementation

//version = CHeaderReport;
//version = CHeaderReportIncludes;
//version = CHeaderReportDefines;
//version = CHeaderReportAggregates;
//version = CHeaderReportAggregateBases;
//version = CHeaderReportAggregateTemplateParams;
//version = CHeaderReportFunctions;
//version = CHeaderReportFunctionParameters;
//version = CHeaderReportEnums;
//version = CHeaderReportEnumValues;
//version = CHeaderReportVariables;
//version = CHeaderReportRemainingLines;
//version = BinderooCodeReport;

/+mixin template ImportCHeader( string strHeaderLocation, CDefine[] predefines, int iVersion = 0 )
{
	mixin( "enum CHeader " ~ HeaderID!strHeaderLocation
			~ " = GenerateHeader( strHeaderLocation, import( strHeaderLocation ), predefines, false );" );

	version( CHeaderReport )
	{
		mixin( "static foreach( strLine; GenerateHeaderReport( " ~ HeaderID!strHeaderLocation ~ " ) ) { pragma( msg, strLine ); }" );
	}

	version( BinderooCodeReport )
	{
		pragma( msg, "Binderoo code for " ~ strHeaderLocation ~ ":" );
		mixin( "pragma( msg, GenerateBinderooCodeFromHeader( " ~ HeaderID!strHeaderLocation ~ " ) );" );
	}
}+/

CHeader GenerateHeader( string strHeaderLocation, string strHeaderContents, CDefine[ string ] predefines, bool bPreprocessorOnly = false, string[] strSearchPaths = [] )
{
	import std.string : splitLines;

	CHeader header = 	( strHeaderContents ~ "\n" )
						.StandardiseLineEndings
						.StripComments
						.StripExcessWhiteSpace
						.splitLines
						.CleanupLines
						.HandleCleanHeader( predefines, bPreprocessorOnly, strSearchPaths );

	header.strLocation = strHeaderLocation;
	return header;
}

string[] GenerateHeaderReport( CHeader header )
{
	import std.conv : to;

	string[] strOutput;
	strOutput ~= "C Header report: " ~ header.strLocation;

	version( CHeaderReportIncludes ) foreach( ref strIncludes; header.strIncludes )
	{
		strOutput ~= "#include " ~ strIncludes;
	}

	version( CHeaderReportDefines ) foreach( ref currDefine; header.defines.byValue )
	{
		strOutput ~= ( currDefine.Type == CDefine.DefineType.PreDefined ? "Pre-" : "" ) ~ "Define " ~ currDefine.ID ~ ( currDefine.HasParameters > 0 ? " (" ~ currDefine.Parameters.joinWith( ", " ) ~ ")" : "" ) ~ ( currDefine.Value.length ?  " -> " ~ currDefine.Value : "" );
	}

	void handleAggregate( ref CAggregate aggregate )
	{
		version( CHeaderReportAggregates )
		{
			strOutput ~= aggregate.eType.to!string ~ " " ~ aggregate.strIdentifier;
			version( CHeaderReportAggregateTemplateParams ) foreach( ref param; aggregate.templateParams )
			{
				strOutput ~= " -> Template param " ~ param.eType ~ ( param.eType == CTemplateParam.Type.Variable ? " " ~ param.strVariableType ~ " " : " " ) ~ param.strIdentifier ~ ( param.strDefaultValue.length > 0 ? " (default: " ~ param.strDefaultValue ~ ")" : "" );
			}

			version( CHeaderReportAggregateBases ) foreach( ref base; aggregate.baseAggregates )
			{
				strOutput ~= " -> Derived from" ~ base.eVisibility.to!string ~ " " ~ base.strIdentifier;
			}

		}

		version( CHeaderReportEnums ) foreach( ref currEnum; aggregate.enums )
		{
			strOutput ~= " -> " ~ currEnum.eVisibility.to!string ~ " Enum " ~ ( currEnum.bIsClass ? "Class " : "" ) ~ ( currEnum.strIdentifier.length > 0 ? currEnum.strIdentifier : "(anonymous)" ) ~ " (type " ~ currEnum.strUnderlyingType ~ ")";
			version( CHeaderReportEnumValues ) foreach( ref val; currEnum.allValues )
			{
				strOutput ~= " --> " ~ val.strIdentifier ~ ( val.strDefault.length > 0 ? " (value: " ~ val.strDefault ~ ")" : "" );
			}
		}

		version( CHeaderReportFunctions ) foreach( ref func; aggregate.functions )
		{
			strOutput ~= " -> " ~ func.eVisibility.to!string ~ " " ~ func.eType.to!string ~ " " ~ func.strIdentifier;
			if( func.CanReturnValue )
			{
				strOutput ~= " --> Returns " ~ func.strReturnType;
			}

			version( CHeaderReportFunctionParameters ) foreach( ref param; func.parameters )
			{
				strOutput ~= " --> Param (" ~ param.strType ~ ") " ~ param.strIdentifier ~ ( param.strDefault.length ? " (default " ~ param.strDefault ~ ")" : "" );
			}
		}

		version( CHeaderReportVariables ) foreach( ref var; aggregate.variables )
		{
			strOutput ~= " -> Var " ~ var.eVisibility.to!string ~ " (" ~ var.strType ~ ") " ~ var.strIdentifier ~ ( var.strDefault.length ? " (default " ~ var.strDefault ~ ")" : "" ) ~ ( var.strBitfieldSize.length ? " (bitfield size: " ~ var.strBitfieldSize ~ ") " : "" );
		}

		version( CHeaderReportRemainingLines ) foreach( strRemaining; aggregate.strRemainingLines )
		{
			strOutput ~= " -> Remaining line: \"" ~ strRemaining ~ "\"";
		}
		
		foreach( ref nextAggregate; aggregate.types )
		{
			handleAggregate( nextAggregate );
		}
	}

	handleAggregate( header.global );

	foreach( strRemaining; header.strRemainingLines )
	{
		strOutput ~= "-> Global remaining line: \"" ~ strRemaining ~ "\"";
	}

	return strOutput;
}

string GenerateBinderooCodeFromHeader( CHeader header )
{
	import std.stdio : writeln;

	version( CHeaderReport ) header.GenerateHeaderReport.joinWith( "\n" ).writeln;

	enum BreakLine =	"//----------------------------------------------------------------------------\n";
	enum EmptyLine =	"\n";

	enum TopContent			= BreakLine
							~ "// Binderoo generated interface for C++ header.\n"
							~ "// DO NOT MODIFY YOURSELF! Regnerate this file by invoking the function\n"
							~ "// \"binderoo.binding.cheaderparsing.ParseAndGenerateBinderooCodeFromHeader\"\n"
							~ BreakLine;

	enum Imports			= "public import binderoo.binding;\n"
							~ "public import binderoo.binding.inheritance;\n"
							~ "public import binderoo.traits;\n"
							~ BreakLine;

	enum BottomContent		= BreakLine
							~ "mixin BindModule!1;\n";
	

	string[] strOutput;

	strOutput ~= TopContent;
	strOutput ~= "module " ~ header.strLocation.GetModuleNameFromHeader ~ ";\n";
	strOutput ~= Imports;

	foreach( strInclude; header.strIncludes )
	{
		strOutput ~= "public import " ~ strInclude.GetModuleNameFromHeader ~ ";";
	}

	if( header.strIncludes.length > 0 )
	{
		strOutput ~= BreakLine;
	}

	bool bAddedDefine;
	foreach( ref currDefine; header.defines )
	{
		if( currDefine.Type == CDefine.DefineType.CodeDefined
			&& currDefine.IsLiteral )
		{
			strOutput ~= "enum " ~ currDefine.ID ~ " = " ~ currDefine.Value ~ ";";
			bAddedDefine = true;
		}
	}

	if( bAddedDefine )
	{
		strOutput ~= BreakLine;
	}

	void handle( ref CAggregate currAggregate, string strDefTabs = "", string strContentTabs = "\t" )
	{
		if( currAggregate.eType != CAggregate.Type.Namespace )
		{
			string strType = ( currAggregate.eType == CAggregate.Type.Class ? "struct" : cast(string)currAggregate.eType );

			strOutput ~= strDefTabs ~ "@CTypeName( \"" ~ currAggregate.strFullyQualifiedName ~ "\", \"" ~ header.strLocation ~ "\" )";
			if( currAggregate.IsAbstract )
			{
				strOutput ~= strDefTabs ~ "@BindAbstract";
			}
			strOutput ~= strDefTabs ~ strType ~ " " ~ currAggregate.strIdentifier;
			strOutput ~= strDefTabs ~ "{";

			if( currAggregate.baseAggregates.length > 0 )
			{
				string[] strBaseNames;

				foreach( ref thisBase; currAggregate.baseAggregates )
				{
					strBaseNames ~= thisBase.strIdentifier;
				}

				strOutput ~= strContentTabs ~ "mixin CStructInherits!( " ~ strBaseNames.joinWith( ", " ) ~ " );";
			}
			else
			{
				strOutput ~= strContentTabs ~ "mixin CStructBase;";
			}
			strOutput ~= strContentTabs ~ "mixin( GenerateBindings!( typeof( this ) ) );";
			strOutput ~= BreakLine;
		}
		else
		{
			import std.algorithm : max;
			if( strDefTabs.length > 0 ) strDefTabs = strDefTabs[ 0 .. max( 0, $ - 1 ) ];
			strContentTabs = strContentTabs[ 0 .. max( 0, $ - 1 ) ];
		}

		foreach( ref currTypedef; currAggregate.typedefs )
		{
			strOutput ~= strContentTabs ~ "alias " ~ currTypedef.strIdentifier ~ " = " ~ currTypedef.strDDecl ~ ";";
		}
		if( currAggregate.typedefs.length > 0 )
		{
			strOutput ~= BreakLine;
		}

		foreach( ref currEnum; currAggregate.enums )
		{
			if( currEnum.strIdentifier.length > 0 )
			{
				strOutput ~= strContentTabs ~ "@CTypeName( \"" ~ currEnum.strFullyQualifiedName ~ "\", \"" ~ header.strLocation ~ "\" )";
				strOutput ~= strContentTabs ~ "enum " ~ currEnum.strIdentifier ~ " : " ~ currEnum.strUnderlyingType;
			}
			else
			{
				strOutput ~= strContentTabs ~ "enum : " ~ currEnum.strUnderlyingType;
			}
			strOutput ~= strContentTabs ~ "{";
			foreach( ref currVal; currEnum.allValues )
			{
				strOutput ~= strContentTabs ~ "\t" ~ currVal.strIdentifier ~ ( currVal.strDefault.length > 0 ? " = " ~ currVal.strDefault : "" ) ~ ",";
			}
			strOutput ~= strContentTabs ~ "}";
			strOutput ~= BreakLine;
		}

		string strNextDefTabs = strContentTabs;
		string strNextContentTabs = strNextDefTabs ~ "\t";

		foreach( ref nextAggregate; currAggregate.types )
		{
			handle( nextAggregate, strNextDefTabs, strNextContentTabs );
		}

		if( currAggregate.types.length > 0 )
		{
			strOutput ~= BreakLine;
		}

		alias PredType = bool function( ref CFunction );
		void handleFunctionType( PredType Pred, string strBeginningComment )
		{
			string[] strFunctionDefs;

			strFunctionDefs ~= strContentTabs ~ "// " ~ strBeginningComment;
			strFunctionDefs ~= EmptyLine;
			int iNumAdded;

			foreach( ref currFunction; currAggregate.functions )
			{
				if( !Pred( currFunction ) )
				{
					continue;
				}

				if( currFunction.eVisibility == CObjectVisibility.Private && !currFunction.IsVirtual )
				{
					continue;
				}

				bool bShouldAdd;
				string strDecl = strContentTabs;
				string strIdentifier = currFunction.strIdentifier;

				final switch( currFunction.eType ) with( CFunction.Type )
				{
					case Function:
					case Static:
					case NonVirtual:
						strDecl ~= "@BindMethod( 1 ) ";
						bShouldAdd = ( currFunction.eVisibility != CObjectVisibility.Private );
						break;
					case Virtual:
						strDecl ~= "@BindVirtual( 1 ) ";
						bShouldAdd = true;
						break;
					case Constructor:
						strDecl ~= "@BindConstructor( 1 ) ";
						strIdentifier = "_cppconstructor";
						bShouldAdd = ( currFunction.eVisibility != CObjectVisibility.Private );
						break;
					case Destructor:
						strDecl ~= "@BindDestructor( 1 ) ";
						strIdentifier = "_cppdestructor";
						bShouldAdd = ( currFunction.eVisibility != CObjectVisibility.Private );
						break;
					case VirtualDestructor:
						strDecl ~= "@BindVirtualDestructor( 1 ) ";
						strIdentifier = "_cppvirtualdestructor";
						bShouldAdd = true;
						break;
				}

				if( currFunction.bIsAbstract )
				{
					strDecl ~= "@BindAbstract ";
				}

				strDecl ~= "\n" ~ strContentTabs ~ cast(string)currFunction.eVisibility ~ " " ~ currFunction.strReturnType ~ " " ~ strIdentifier ~ "( ";

				string[] strParams;
				foreach( ref param; currFunction.parameters )
				{
					strParams ~= param.FullDeclCPP;
				}

				strDecl ~= strParams.joinWith( ", " ) ~ " )";

				if( currFunction.bIsConst )
				{
					strDecl ~= " const";
				}

				if( bShouldAdd )
				{
					strFunctionDefs ~= strDecl ~ ";";
					++iNumAdded;
				}
			}

			if( iNumAdded )
			{
				strFunctionDefs ~= BreakLine;
				strOutput ~= strFunctionDefs;
			}
		}

		handleFunctionType( ( ref a ) => a.eType == CFunction.Type.Function, "Plain old ordinary functions" );
		handleFunctionType( ( ref a ) => a.eType == CFunction.Type.Static, "Static methods" );
		handleFunctionType( ( ref a ) => a.eType == CFunction.Type.Constructor, "Constructors" );
		handleFunctionType( ( ref a ) => a.eType == CFunction.Type.Destructor, "Destructors" );
		handleFunctionType( ( ref a ) => a.IsVirtual, "Virtual methods" );
		handleFunctionType( ( ref a ) => a.eType == CFunction.Type.NonVirtual, "Non-virtual methods" );

		foreach( ref currVariable; currAggregate.variables )
		{
			import std.conv : to;

			strOutput ~= strContentTabs ~ currVariable.eVisibility.to!string ~ " " ~ currVariable.strType ~ " " ~ currVariable.strIdentifier ~ ";";
		}
		
		if( currAggregate.variables.length > 0 )
		{
			strOutput ~= BreakLine;
		}

		if( currAggregate.eType != CAggregate.Type.Namespace )
		{
			strOutput ~= strDefTabs ~ "}";
			strOutput ~= BreakLine;
		}
	}

	handle( header.global );

	strOutput ~= BottomContent;

	return strOutput.joinWith( "\n" );
}

@BindExport( 1 )
extern( C++ ) void ListAllIncludes( const(char)* pHeaderBasePaths, const(char)* pHeaderFilename, const(char)* pPredefines )
{
	import core.stdc.string : strlen;
	import std.file : read, exists;
	import std.stdio : writeln;
	import std.string : split;

	string strHeaderBasePath = cast( string )pHeaderBasePaths[ 0 .. strlen( pHeaderBasePaths ) ];
	string strHeaderFilename = cast( string )pHeaderFilename[ 0 .. strlen( pHeaderFilename ) ];
	string strPredefine = cast( string )pPredefines[ 0 .. strlen( pPredefines ) ];

	string[] strHeaderBasePaths = strHeaderBasePath.split( ',' );
	string[] strPredefines = strPredefine.split( ',' );

	string strFullFilename;
	
	foreach( strBasePath; strHeaderBasePaths )
	{
		string strWorkingPath = strBasePath ~ "/" ~ strHeaderFilename;
		if( strWorkingPath.exists )
		{
			strFullFilename = strWorkingPath;
			break;
		}
	}

	string strHeaderContents = cast(string)cast( const( char )[] )read( strFullFilename );

	CDefine[ string ] Predefines = BasicPredefines();

	foreach( predefine; strPredefines )
	{
		string[] allSplitUp = predefine.split( '=' );
		if( allSplitUp.length > 1 )
		{
			Predefines[ allSplitUp[ 0 ] ] = CDefine( allSplitUp[ 0 ], allSplitUp[ 1 ] );
		}
		else
		{
			Predefines[ allSplitUp[ 0 ] ] = CDefine( allSplitUp[ 0 ] );
		}
	}

	CHeader header = GenerateHeader( strHeaderFilename, strHeaderContents, Predefines, false, strHeaderBasePaths );

	foreach( strInclude; header.strIncludes ~ header.strAdditionalIncludes )
	{
		writeln( strInclude );
	}
}

@BindExport( 1 )
extern( C++ ) void ParseAndGenerateBinderooCodeFromHeader( const(char)* pHeaderBasePaths, const(char)* pHeaderFilename, const(char)* pPredefines )
{
	import core.stdc.string : strlen;
	import std.file : read, exists, write, mkdirRecurse;
	import std.stdio : writeln;
	import std.string : split;

	string strHeaderBasePath = cast( string )pHeaderBasePaths[ 0 .. strlen( pHeaderBasePaths ) ];
	string strHeaderFilename = cast( string )pHeaderFilename[ 0 .. strlen( pHeaderFilename ) ];
	string strPredefine = cast( string )pPredefines[ 0 .. strlen( pPredefines ) ];

	string[] strHeaderBasePaths = strHeaderBasePath.split( ',' );
	string[] strPredefines = strPredefine.split( ',' );

	string strFullFilename;
	
	foreach( strBasePath; strHeaderBasePaths )
	{
		string strWorkingPath = strBasePath ~ "/" ~ strHeaderFilename;
		if( strWorkingPath.exists )
		{
			strFullFilename = strWorkingPath;
			break;
		}
	}

	string strHeaderContents = cast(string)cast( const( char )[] )read( strFullFilename );

	CDefine[ string ] Predefines = BasicPredefines();

	foreach( predefine; strPredefines )
	{
		string[] allSplitUp = predefine.split( '=' );
		if( allSplitUp.length > 1 )
		{
			Predefines[ allSplitUp[ 0 ] ] = CDefine( allSplitUp[ 0 ], allSplitUp[ 1 ] );
		}
		else
		{
			Predefines[ allSplitUp[ 0 ] ] = CDefine( allSplitUp[ 0 ] );
		}
	}

	writeln( "Processing ", strHeaderFilename, "..." );

	string strCode =	GenerateHeader( strHeaderFilename, strHeaderContents, Predefines, false, strHeaderBasePaths )
						.GenerateBinderooCodeFromHeader;

	string strOutputFolder = strHeaderFilename.GetModuleFolderFromHeader;
	if( strOutputFolder != "." )
		strOutputFolder.mkdirRecurse;

	string strOutputFile = strHeaderFilename.GetModuleFilenameFromHeader;
	
	strOutputFile.write( strCode );

	writeln( "Processed ", strHeaderFilename, " -> ", strOutputFile );
}

package:

string StandardiseLineEndings( string strSource )
{
	import std.string;
	return strSource.replace( MSLineEnd, StandardisedLineEnd );
}

string StripCommentType( string Begin, string End, string AppendAfterEnd = "" )( string strSource )
{
	import std.algorithm : countUntil, max, min;

	string strOutput = Reserve!string( strSource.length );

	ptrdiff_t iIndex = -1;
	while( ( iIndex = strSource.countUntil( Begin ) ) != -1 )
	{
		strOutput ~= strSource[ 0 .. iIndex ];
		strSource = strSource[ iIndex .. $ ];
		iIndex = max( 0, strSource.countUntil( End ) );
		strSource = AppendAfterEnd ~ strSource[ min( iIndex + End.length, $ ) .. $ ];
	}

	strOutput ~= strSource;

	return strOutput;
}

string StripComments( string strSource )
{
	return strSource.StripCommentType!( "//", StandardisedLineEnd, StandardisedLineEnd )()
					.StripCommentType!( "/*", "*/" )();
}

string[] CleanupLines( string[] strSource )
{
	import std.algorithm : startsWith, endsWith, max;
	import std.string : count;

	string[] strOutput = Reserve!( string[] )( strSource.length );

	string strWorking;
	foreach( ref strLine; strSource )
	{
		if( strLine.endsWith( "\\" ) )
		{
			strWorking ~= strLine[ 0 .. $ - 1 ];
		}
		else if( strWorking.length > 0 )
		{
			if( strLine.endsWith( "\\" ) )
			{
				strWorking ~= " " ~ strLine[ 0 .. max( 0, $ - 1 ) ];
			}
			else
			{
				if( strLine.length > 0 )
				{
					strWorking ~= " " ~ strLine;
				}
				strOutput ~= strWorking;
				strWorking.length = 0;
			}
		}
		else if( strLine.length > 0 )
		{
			strOutput ~= strLine;
		}
	}

	if( strWorking.length > 0 )
	{
		strOutput ~= " " ~ strWorking;
	}

	strSource = strOutput;
	strOutput = Reserve!( string[] )( strSource.length );

	ptrdiff_t iOpenBracketCount = 0;
	strWorking.length = 0;

	foreach( ref strLine; strSource )
	{
		strWorking ~= strLine;

		iOpenBracketCount += cast(ptrdiff_t)strLine.count( "(" );
		iOpenBracketCount -= cast(ptrdiff_t)strLine.count( ")" );

		if( iOpenBracketCount == 0 )
		{
			strOutput ~= strWorking;
			strWorking.length = 0;
		}
		else
		{
			strWorking ~= " ";
		}
	}

	if( strWorking.length > 0 )
	{
		strOutput ~= " " ~ strWorking;
	}

	return strOutput;
}

auto pushBack( T )( ref T[] container, T newVal )
{
	container ~= newVal;
	return container;
}

auto popBack( T )( ref T[] container )
{
	import std.algorithm : max;
	container = container[ 0 .. max( 0, $ - 1 ) ];
	return container;
}

ref auto back( T )( T[] container )
{
	import std.algorithm : max;
	return container[ max( 0, $ - 1 ) ];
}

// Index 0 is a string from the start including the last instance of ignoreGroupLast
// Index 1 is the remainder of the string; or the entire string if there is not a matching
// count of ignoreGroupFirst to ignoreGroupLast
string[] SplitDefinition( string container, char ignoreGroupFirst, char ignoreGroupLast )
{
	import std.algorithm : min;

	string[] strOutput = Reserve!( string[] )( 2 );

	ptrdiff_t iEnd = -1;

	ptrdiff_t iIgnoreGroupCount = 0;

	foreach( iIndex, ref currVal; container )
	{
		if( currVal == ignoreGroupFirst )
		{
			++iIgnoreGroupCount;
		}
		else if( currVal == ignoreGroupLast )
		{
			if( --iIgnoreGroupCount == 0 )
			{
				iEnd = min( iIndex + 1, container.length );
				break;
			}
		}
	}

	if( iEnd >= 0 && iEnd <= container.length )
	{
		strOutput ~= container[ 0 .. iEnd ];
		strOutput ~= container[ iEnd .. $ ];
	}
	else
	{
		strOutput ~= "";
		strOutput ~= container;
	}

	return strOutput;
}

string[] BuildStatements( string[] strLines )
{
	import std.string : indexOfAny, startsWith;

	string[] strOutput;
	string strWorking;

	ptrdiff_t iOpenBraceCount = 0;

	foreach( strLine; strLines )
	{
		ptrdiff_t iIndex = -1;
		while( strLine.length > 0 && ( iIndex = strLine.indexOfAny( ";{} " ) ) != -1 )
		{
			switch( strLine[ iIndex ] )
			{
			case ';':
				if( iOpenBraceCount > 0 )
				{
					goto default;
				}
				strWorking ~= strLine[ 0 .. iIndex ];
				if( strWorking.length > 0 )
				{
					strOutput ~= strWorking;
					strWorking.length = 0;
				}
				break;

			case '{':
				strWorking ~= strLine[ 0 .. iIndex  + 1 ];
				++iOpenBraceCount;
				break;

			case '}':
				strWorking ~= strLine[ 0 .. iIndex  + 1 ];
				if( --iOpenBraceCount == 0 )
				{
					bool bHackIt = strWorking.startsWith( "typedef enum" )
									|| strWorking.startsWith( "typedef struct" )
									|| strWorking.startsWith( "typedef class" );

					if( strWorking.length > 0 && !bHackIt )
					{
						strOutput ~= strWorking ~ " ";
						strWorking.length = 0;
					}
					else if( bHackIt )
					{
						strWorking ~= " ";
					}
				}
				break;

			case ' ':
				if( iIndex > 0 )
				{
					strWorking ~= strLine[ 0 .. iIndex  + 1 ];
				}
				break;

			default:
				strWorking ~= strLine[ 0 .. iIndex  + 1 ];
				break;
			}
			strLine = strLine[ iIndex + 1 .. $ ];
		}

		if( strLine.length > 0 || strWorking.length > 0 )
		{
			strWorking ~= strLine ~ " ";
		}
	}

	if( strWorking.length > 0 )
	{
		strOutput ~= strWorking;
	}

	return strOutput;
}

string CTypeNameToDTypeName( string strFullType, bool bRetainIdentifier )
{
	string[] strTokens = strFullType.SplitTokens( CWhitespace, "*&" );

	return CTypeNameToDTypeName( strTokens, bRetainIdentifier );
}

string CTypeNameToDTypeName( string[] strTokens, bool bRetainIdentifier )
{
	struct DType
	{
		string strType;
		string strPointers;
		string strIdentifier;
		bool bConst;
		bool bRef;
		bool bUnsigned;

		@property ToString() const
		{
			if( bConst )
			{
				return ( bRef ? "ref " : "" ) ~ "const( " ~ strType ~ " )" ~ strPointers;
			}
			return ( bRef ? "ref " : "" ) ~ strType ~ strPointers;
		}

		@property Valid() const
		{
			return strType.length > 0;
		}
	}

	DType[] foundTypes;
	DType working;

	int iLongCount;
	int iShortCount;
	int iIntCount;
	bool bIgnore;
	bool bSignedUnsigned;

	void pushWorking()
	{
		if( iShortCount == 1 )
		{
			working.strType ~= "short";
		}
		else if( iLongCount == 1 )
		{
			version( X86_64 ) working.strType = "long";
			else working.strType ~= "int";
		}
		else if( iLongCount == 2 )
		{
			working.strType ~= "long";
		}
		else if( iIntCount == 1 )
		{
			working.strType ~= "int";
		}

		if( working.bUnsigned )
		{
			working.strType = "u" ~ working.strType;
		}

		if( working.strType == "uchar" )
		{
			working.strType = "ubyte";
		}

		if( working.Valid )
		{
			foundTypes ~= working;
		}
		else if( bSignedUnsigned )
		{
			working.strType ~= "int";
			foundTypes ~= working;
		}

		iLongCount = 0;
		iShortCount = 0;
		iIntCount = 0;
		bIgnore = false;
		bSignedUnsigned = false;
		working = DType.init;
	}

	foreach( tok; strTokens )
	{
		if( tok == "," )
		{
			pushWorking();
		}
		else if( tok == "=" )
		{
			bIgnore = true;
		}
		else if( !bIgnore )
		{
			if( tok == "const" )
			{
				working.bConst = true;
			}
			else if( tok == "unsigned" )
			{
				working.bUnsigned = true;
			}
			else if( tok == "signed" )
			{
				working.bUnsigned = false;
			}
			else if( tok == "long" )
			{
				++iLongCount;
			}
			else if( tok == "short" )
			{
				++iShortCount;
			}
			else if( tok == "int" )
			{
				++iIntCount;
			}
			else if( tok == "wchar_t" || tok == "__WCHAR_TYPE__" )
			{
				version( Windows ) working.strType ~= "wchar";
				else working.strType ~= "dchar";
			}
			else if( tok == "&" )
			{
				working.bRef = true;
			}
			else if( tok == "*" )
			{
				working.strPointers ~= tok;
			}
			else
			{
				if( working.strType.length == 0 )
				{
					working.strType ~= tok;
				}
				else if( bRetainIdentifier )
				{
					working.strIdentifier = tok;
				}
			}
		}
	}

	pushWorking();

	return foundTypes.joinWith!( ( ref a ) => a.ToString )( ", " );
}

ref CTemplateParam[] HandleTemplateParametersStatement( ref CTemplateParam[] output, string[] strParameters )
{
	import std.string : indexOf;
	import std.algorithm : countUntil;

	foreach( param; strParameters )
	{
		CTemplateParam newParam;
		string[] strTokens = param.SplitApart( ' ', '<', '>' );
		ptrdiff_t iEqualsIndex = strTokens.countUntil!( ( a, b ) => a.indexOf( b ) != -1 )( '=' );
		switch( strTokens[ 0 ] )
		{
		case "typename":
		case "class":
		case "struct":
			newParam.eType = cast(CTemplateParam.Type)strTokens[ 0 ];
			newParam.strIdentifier = strTokens[ 1 ];
			break;
		default:
			newParam.eType = CTemplateParam.Type.Variable;
			newParam.strVariableType = strTokens[ 0 ];
			newParam.strIdentifier = strTokens[ 1 ];
			break;
		}

		if( iEqualsIndex != -1 )
		{
			ptrdiff_t iEqualsChar = strTokens[ iEqualsIndex ].indexOf( '=' );
			newParam.strDefaultValue ~= strTokens[ iEqualsIndex ][ iEqualsChar + 1 .. $ ];
			foreach( tok; strTokens[ iEqualsIndex + 1 .. $ ] )
			{
				newParam.strDefaultValue ~= tok;
			}
		}

		output ~= newParam;
	}
	return output;
}

ref CTemplateParam[] HandleTemplateParametersStatement( ref CTemplateParam[] output, string strStatement )
{
	return output.HandleTemplateParametersStatement( strStatement.SplitContents( '<', '>' ).SplitApart( ',', IgnoreGroupTemplatesAndFunctions ) );
}

ref CAggregate HandleAggregateStatement( ref CAggregate output, ref CObjectVisibility eVisibility, CAggregate.Type eType, string[] strStatementTokens, string strExtendedStatement )
{
	if( strExtendedStatement.length > 0 )
	{
		import std.string : endsWith, indexOf, indexOfAny;
		import std.math : abs;
		import std.algorithm : min;

		CAggregate newAggregate;

		newAggregate.eType = eType;
		newAggregate.eVisibility = eVisibility;
		newAggregate.strParentAggregate = output.strIdentifier;

		bool bLookingForAggregateName = true;

		string strPotentialName;
		ptrdiff_t iAggregateTypeDefnToken = 0;
		CObjectVisibility ePotentialVisibility;
		
		bool bHandlingTemplate = false;
		string strTemplateDecl;

		foreach( iTokenIndex, tok; strStatementTokens )
		{
			if( bLookingForAggregateName )
			{
				ptrdiff_t iSearchUpTo = tok.indexOfAny( CTokenSymbols );
				if( iSearchUpTo == -1 )
				{
					iSearchUpTo = tok.length;
				}

				switch( tok [ 0 .. iSearchUpTo ] )
				{
				case "class":
				case "struct":
				case "union":
				case "namespace":
					iAggregateTypeDefnToken = iTokenIndex;
					break;

				case "template":
					bHandlingTemplate = true;
					goto default;

				default:
					if( bHandlingTemplate )
					{
						strTemplateDecl ~= tok;
						if( strTemplateDecl.indexOf( '<' ) != -1 )
						{
							newAggregate.templateParams.HandleTemplateParametersStatement( strTemplateDecl );
							bHandlingTemplate = false;
						}
						break;
					}

					if( tok.endsWith( ":" ) )
					{
						strPotentialName ~= tok[ 0 .. $ - 1 ];
						newAggregate.strIdentifier = strPotentialName;
						strPotentialName.length = 0;
						bLookingForAggregateName = false;
					}
					else
					{
						strPotentialName ~= tok;
					}
					break;
				}
			}
			else
			{
				switch( tok )
				{
				case "public":
					ePotentialVisibility = CObjectVisibility.Public;
					break;
				case "private":
					ePotentialVisibility = CObjectVisibility.Private;
					break;
				case "protected":
					ePotentialVisibility = CObjectVisibility.Protected;
					break;
				default:
					if( tok.endsWith( "," ) )
					{
						strPotentialName ~= tok[ 0 .. $ - 1 ];
						newAggregate.baseAggregates ~= CAggregate.Derived( strPotentialName, ePotentialVisibility );
						strPotentialName.length = 0;
						ePotentialVisibility = CObjectVisibility.Public;
					}
					else
					{
						strPotentialName ~= tok;
					}
					break;
				}
			}
		}

		if( strPotentialName.length > 0 )
		{
			if( bLookingForAggregateName )
			{
				newAggregate.strIdentifier = strPotentialName;
			}
			else
			{
				newAggregate.baseAggregates ~= CAggregate.Derived( strPotentialName, ePotentialVisibility );
			}
		}
		
		newAggregate.strFullyQualifiedName = [ output.strFullyQualifiedName, newAggregate.strIdentifier ].joinWith( "::" );

		CObjectVisibility eCurrStatementVisibility = eType.DefaultVisibility;
		string[] strStatements = [ strExtendedStatement ].BuildStatements;

		//if( output.strIdentifier.length > 0 )
		//{
		//	newAggregate.strRemainingLines ~= strStatements;
		//}

		foreach( ref statement; strStatements )
		{
			HandleStatement( newAggregate, eCurrStatementVisibility, statement );
		}

		output.types ~= newAggregate;
	}

	return output;
}

ref CAggregate HandleTypedefStatement( ref CAggregate output, ref CObjectVisibility eVisibility, string strFullStatement )
{
	import std.string : indexOf;
	import std.algorithm: countUntil;
	import std.range : retro;
	import std.stdio : writeln;

	//writeln( strFullStatement );

	string[] strTokens = strFullStatement.SplitTokens( CWhitespace, CSymbols );
	if( strTokens[ 0 ] == "typedef" )
	{
		bool bAlreadyHandled = false;
		if( strTokens[ 1 ] == "enum" || strTokens[ 1 ] == "class" || strTokens[ 1 ] == "struct" )
		{
			ptrdiff_t iOpenBraceIndex = strTokens.countUntil!( ( a, b ) => a == b )( "{" );
			if( iOpenBraceIndex != -1 )
			{
				ptrdiff_t iCloseBraceIndexPlusOne = strTokens.length - strTokens.retro.countUntil!( ( a, b ) => a == b )( "}" );
				ptrdiff_t iNameIndex = 2;
				if( strTokens[ 2 ] == "{" )
				{
					iNameIndex = iCloseBraceIndexPlusOne;
				}
				string[] strNewTokens = [ strTokens[ 1 ], strTokens[ iNameIndex ] ];
				
				string strExtendedStatement = strTokens[ iOpenBraceIndex + 1 .. iCloseBraceIndexPlusOne - 1 ].joinWith( " " );

				if( strTokens[ 1 ] == "enum" )
				{
					output.HandleEnumStatement( eVisibility, strNewTokens, strExtendedStatement );
				}
				else
				{
					output.HandleAggregateStatement( eVisibility, strTokens[ 1 ] == "struct" ? CAggregate.Type.Struct : CAggregate.Type.Class, strNewTokens, strExtendedStatement );
				}

				bAlreadyHandled = true;
			}
			else
			{
				strTokens = [ strTokens[ 0 ] ] ~ strTokens[ 2 .. $ ];
			}
		}

		if( !bAlreadyHandled )
		{
			CTypeDef newTypedef;
			newTypedef.eVisibility = eVisibility;
			if( strFullStatement.indexOf( '(' ) != -1 )
			{
				// Function pointer typedef
				newTypedef.eType = CTypeDef.Type.FunctionAlias;

				string[] strReturnType;
				string strAggregateType;
				strTokens.Consume; // typedef
				while( strTokens.Peek != "(" )
				{
					strReturnType ~= strTokens.Consume;
				}
				strTokens.Consume; // (
				while( strTokens.Peek != "*" && strTokens.Peek != "::*" )
				{
					strAggregateType = strTokens.Consume;
				}
				strTokens.Consume; // *
				newTypedef.strIdentifier = strTokens.Consume;
				strTokens.Consume; // )

				string[] strParams;
				if( strAggregateType.length > 0 )
				{
					strParams = [ strAggregateType, "*", "pThis" ];
				}
				string[] strRemainingParams = strTokens[ 1 .. $ - 1 ];
				if( strRemainingParams.length > 0 )
				{
					strParams ~= ",";
					strParams ~= strRemainingParams;
				}

				newTypedef.strActualType = strReturnType.joinWith( " " ) ~ "(*)( " ~ strParams.joinWith( " " ) ~ " )";
				newTypedef.strDDecl = "extern( C++ ) " ~ strReturnType.joinWith( " " ) ~ " function( " ~ strParams.CTypeNameToDTypeName( false ) ~ " )";

				//writeln( strFullStatement, " ==>\nalias ", newTypedef.strIdentifier, " = ", newTypedef.strDDecl );
			}
			else
			{
				newTypedef.eType = CTypeDef.Type.TypeAlias;
				newTypedef.strIdentifier = strTokens[ $ - 1 ];
				newTypedef.strActualType = strTokens[ 1 .. $ - 1 ].joinWith( " " );
				newTypedef.strDDecl = strTokens[ 1 .. $ - 1 ].CTypeNameToDTypeName( false );

				//writeln( strFullStatement, " ==> ", newTypedef.strDDecl );
			}
			output.typedefs ~= newTypedef;
		}
	}
	return output;
}

ref CAggregate HandleUsingStatement( ref CAggregate output, ref CObjectVisibility eVisibility, string[] strStatementTokens )
{
	return output;
}

ref CAggregate HandleEnumStatement( ref CAggregate output, ref CObjectVisibility eVisibility, string[] strStatementTokens, string strExtendedStatement )
{
	import std.string : indexOf;
	import std.algorithm : min;

	import std.stdio : writeln;
//	writeln( strExtendedStatement );

	CEnum newEnum;
	newEnum.eVisibility = eVisibility;

	bool bSearchingForUnderlyingType;
	string strWorking;

	foreach( tok; strStatementTokens )
	{
		if( tok == "enum" )
		{
			continue;
		}
		if( tok == "class" )
		{
			newEnum.bIsClass = true;
			continue;
		}
		if( !bSearchingForUnderlyingType )
		{
			ptrdiff_t iFoundColon = tok.indexOf( ':' );
			if( iFoundColon >= 0 )
			{
				strWorking ~= tok[ 0 .. iFoundColon ];
				newEnum.strIdentifier = strWorking;
				strWorking.length = 0;
				tok = tok[ min( iFoundColon + 1, $ ) .. $ ];
				bSearchingForUnderlyingType = true;
			}
			else
			{
				strWorking ~= tok;
			}
		}

		if( bSearchingForUnderlyingType )
		{
			strWorking ~= tok;
		}

	}

	if( !bSearchingForUnderlyingType )
	{
		newEnum.strIdentifier = strWorking;
	}
	else
	{
		newEnum.strUnderlyingType = strWorking;
	}

	newEnum.strFullyQualifiedName = [ output.strFullyQualifiedName, newEnum.strIdentifier ].joinWith( "::" );

	string[] strValues = strExtendedStatement.SplitApart( ',', '(', ')' );

	foreach( val; strValues )
	{
		CVariable enumVal;
		enumVal.eVisibility = CObjectVisibility.Public;
		enumVal.strType = newEnum.strUnderlyingType;

		if( val.indexOf( '=' ) >= 0 )
		{
			string[] strSplit = val.SplitApart( '=', '(', ')' );
			enumVal.strIdentifier = strSplit[ 0 ].StripExcessWhiteSpace;
			enumVal.strDefault = strSplit[ 1 ].StripExcessWhiteSpace;
		}
		else
		{
			enumVal.strIdentifier = val.StripExcessWhiteSpace;
		}

		newEnum.allValues ~= enumVal;
	}

	output.enums ~= newEnum;

	return output;
}

ref CVariable[] HandleVariableStatement( ref CVariable[] output, ref CObjectVisibility eVisibility, string[] strStatementTokens )
{
	import std.algorithm : countUntil, max;
	import std.string : indexOf, indexOfAny, lastIndexOfAny;

	CVariable[] newVariables;
	string strBaseType;

	string[] strAllDecls = strStatementTokens.joinWith( " " ).SplitApart( ',', '(', ')' );

	foreach( iDeclIndex, decl; strAllDecls )
	{
		string[] strCurrTokens = decl.SplitApart( ' ', '(', ')' );
		
		CVariable newVariable;
		newVariable.eVisibility = eVisibility;

		ptrdiff_t iDefaultValIndex = strCurrTokens.countUntil!( ( a, b ) => a.indexOf( b ) != -1 )( '=' );

		if( iDefaultValIndex != -1 )
		{
			ptrdiff_t iEqualsIndex = strCurrTokens[ iDefaultValIndex ].indexOf( '=' );
			string strWorking = strCurrTokens[ iDefaultValIndex ][ iEqualsIndex + 1 .. $ ];

			foreach( tok; strCurrTokens[ iDefaultValIndex + 1 .. strCurrTokens.length ] )
			{
				strWorking ~= " " ~ tok;
			}

			strCurrTokens[ iDefaultValIndex ] = strCurrTokens[ iDefaultValIndex ][ 0 .. iEqualsIndex ];
			if( strCurrTokens[ iDefaultValIndex ].length == 0 )
			{
				strCurrTokens = strCurrTokens[ 0 .. iDefaultValIndex ];
			}
			else
			{
				strCurrTokens = strCurrTokens[ 0 .. iDefaultValIndex + 1 ];
			}

			newVariable.strDefault = strWorking.StripExcessWhiteSpace;
		}

		ptrdiff_t iBitfieldIndex = strCurrTokens.countUntil!( ( a, b ) => a.indexOf( b ) != -1 )( ':' );
		if( iBitfieldIndex != -1 )
		{
			ptrdiff_t iColonIndex = strCurrTokens[ iBitfieldIndex ].indexOf( ':' );
			string strWorking = strCurrTokens[ iBitfieldIndex ][ iColonIndex + 1 .. $ ];
			foreach( tok; strCurrTokens[ iBitfieldIndex + 1 .. strCurrTokens.length ] )
			{
				strWorking ~= tok;
			}

			strCurrTokens[ iBitfieldIndex ] = strCurrTokens[ iBitfieldIndex ][ 0 .. iColonIndex ];
			if( strCurrTokens[ iBitfieldIndex ].length == 0 )
			{
				strCurrTokens = strCurrTokens[ 0 .. iBitfieldIndex ];
			}
			else
			{
				strCurrTokens = strCurrTokens[ 0 .. iBitfieldIndex + 1 ];
			}

			newVariable.strBitfieldSize = strWorking;
		}

		newVariable.strIdentifier = strCurrTokens[ max( 0, $ - 1 ) ];

		if( iDeclIndex == 0 )
		{
			strBaseType = strCurrTokens[ 0 .. max( 0, $ - 1 ) ].joinWith( " " );

			ptrdiff_t iModifierIndex = strBaseType.length;
			ptrdiff_t iFoundIndex = -1;
			while( ( iFoundIndex = strBaseType[ 0 .. iModifierIndex ].lastIndexOfAny( "*&" ) ) != -1 )
			{
				iModifierIndex = iFoundIndex;
			}

			if( iModifierIndex != strBaseType.length )
			{
				newVariable.strIdentifier = strBaseType[ iModifierIndex .. $ ] ~ newVariable.strIdentifier;
				strBaseType = strBaseType[ 0 .. iModifierIndex ];
			}
		}

		newVariable.strType = strBaseType;
		
		ptrdiff_t iModifierIndex = -1;
		while( ( iModifierIndex = newVariable.strIdentifier.indexOfAny( "*&" ) ) != -1 )
		{
			newVariable.strType ~= newVariable.strIdentifier[ 0 .. iModifierIndex + 1 ];
			newVariable.strIdentifier = newVariable.strIdentifier[ iModifierIndex + 1 .. $ ];
		}

		newVariables ~= newVariable;
	}

	output ~= newVariables;
	return output;
}

ref CAggregate HandleVariableStatement( ref CAggregate output, ref CObjectVisibility eVisibility, string[] strStatementTokens )
{
	HandleVariableStatement( output.variables, eVisibility, strStatementTokens );
	return output;
}

ref CAggregate HandleFunctionStatement( ref CAggregate output, ref CObjectVisibility eVisibility, string[] strStatementTokens, string strExtendedStatement )
{
	import std.string : indexOf, indexOfAny;
	import std.algorithm : find;
	import std.algorithm : min;

	import std.stdio : writeln;
	//writeln( strStatementTokens.joinWith( " <===> " ) );

	bool bShouldAdd = true;
	CFunction newFunc;

	newFunc.eVisibility = eVisibility;

	if( output.eType == CAggregate.Type.Struct || output.eType == CAggregate.Type.Class )
	{
		newFunc.eType = CFunction.Type.NonVirtual;
	}

	string[] strTypeQualifiers;
	bool bFoundDefinition;

	tokenparse: foreach( tok; strStatementTokens )
	{
		switch( tok )
		{
		case "static":
			newFunc.eType = CFunction.Type.Static;
			break;

		case "virtual":
			newFunc.eType = CFunction.Type.Virtual;
			break;

		default:
			if( !bFoundDefinition )
			{
				string[] strDefinition = tok.SplitDefinition( '(', ')' );
				if( strDefinition[ 0 ].length > 0 )
				{
					bFoundDefinition = true;

					string strName = strDefinition[ 0 ];

					ptrdiff_t iFoundIndex = -1;
					while( ( iFoundIndex = strName.indexOfAny( "*&" ) ) == 0 )
					{
						strTypeQualifiers ~= strName[ 0 .. iFoundIndex ];
						strName = strName[ min( iFoundIndex + 1, $ ) .. $ ];
					}

					newFunc.strIdentifier = strName[ 0 .. strName.indexOf( '(' ) ];

					if( newFunc.strIdentifier.find( "::" ).length > 0 )
					{
						bShouldAdd = false;
						break tokenparse;
					}

					//import std.stdio:writeln;
					//writeln( "Function ", newFunc.strIdentifier );
					//writeln( " --> tokens: ", strStatementTokens.joinWith( " - " ) );

					newFunc.strFullyQualifiedName = [ output.strFullyQualifiedName, newFunc.strIdentifier ].joinWith( "::" );

					if( newFunc.strIdentifier == output.strIdentifier )
					{
						newFunc.eType = CFunction.Type.Constructor;
					}
					else if( newFunc.strIdentifier == "~" ~ output.strIdentifier )
					{
						newFunc.eType = ( newFunc.eType == CFunction.Type.Virtual ? CFunction.Type.VirtualDestructor : CFunction.Type.Destructor );
					}

					newFunc.strReturnType = strTypeQualifiers.joinWith( " " ).StripExcessWhiteSpace;

					if( newFunc.strReturnType.indexOf( '&' ) != -1 )
					{
						newFunc.bReturnsRef = true;
					}

					string[] strFuncParameters = strName.SplitContents( '(', ')' ).SplitApart( ',', '(', ')' );
					CObjectVisibility eParamVisibility = CObjectVisibility.Public;

					foreach( strVar; strFuncParameters )
					{
						newFunc.parameters.HandleVariableStatement( eParamVisibility, strVar.SplitApart( ' ', '(', ')' ) );
					}
				}
				else
				{
					strTypeQualifiers ~= tok;
				}
			}
			break;
		}
	}

	if( !bFoundDefinition )
	{
		{
			import std.stdio : writeln;
			writeln( "COULDN'T FIND A DEFINITION??? Statement tokens ", strStatementTokens.joinWith( " -@- " ) );
		}
	}

	if( bShouldAdd )
	{
		output.functions ~= newFunc;
	}
	return output;
}

ref CAggregate HandleStatement( ref CAggregate output, ref CObjectVisibility eVisibility, string strFullStatement )
{
	import std.string : indexOf;

	enum StatementType : string
	{
		None				= "none",
		VariableDecl		= "var",
		FunctionDecl		= "func",
		ExternDecl			= "extern",
		AccessibilityDecl	= "access",
		TypedefDecl			= "typedef",
		UsingDecl			= "using",
		EnumDecl			= "enum",
		NamespaceDecl		= "namespace",
		ClassDecl			= "class",
		StructDecl			= "struct",
		UnionDecl			= "union",
	}

	string strExtendedStatement = strFullStatement.StripExcessWhiteSpace.SplitContents( '{', '}' );

	string[] strStatementTokens;
	ptrdiff_t iIndex = strFullStatement.length;

	if( strExtendedStatement.length > 0 )
	{
		iIndex = strFullStatement.indexOf( '{' );
	}
	strStatementTokens = strFullStatement[ 0 .. iIndex ].SplitApart!true( ' ', IgnoreGroupTemplatesAndFunctions );

	tokenparse:
	StatementType eType = StatementType.None;

	ptrdiff_t iPrivacyToken = -1;
	ptrdiff_t iEqualsToken = -1;
	ptrdiff_t iBracketToken = -1;

	tokenloop: foreach( iTokenIndex, tok; strStatementTokens )
	{
		switch( tok ) with( StatementType )
		{
		case ExternDecl:
			eType = cast(StatementType)tok;
			break tokenloop;

		case EnumDecl:
			eType = cast(StatementType)tok;
			HandleEnumStatement( output, eVisibility, strStatementTokens, strExtendedStatement );
			break tokenloop;

		case TypedefDecl:
			eType = cast(StatementType)tok;
			HandleTypedefStatement( output, eVisibility, strFullStatement );
			break tokenloop;

		case UsingDecl:
			eType = cast(StatementType)tok;
			HandleUsingStatement( output, eVisibility, strStatementTokens );
			break tokenloop;

		case NamespaceDecl:
			eType = cast(StatementType)tok;
			HandleAggregateStatement( output, eVisibility, CAggregate.Type.Namespace, strStatementTokens, strExtendedStatement );
			break tokenloop;

		case ClassDecl:
			eType = cast(StatementType)tok;
			HandleAggregateStatement( output, eVisibility, CAggregate.Type.Class, strStatementTokens, strExtendedStatement );
			break tokenloop;

		case StructDecl:
			eType = cast(StatementType)tok;
			HandleAggregateStatement( output, eVisibility, CAggregate.Type.Struct, strStatementTokens, strExtendedStatement );
			break tokenloop;

		case UnionDecl:
			eType = cast(StatementType)tok;
			HandleAggregateStatement( output, eVisibility, CAggregate.Type.Union, strStatementTokens, strExtendedStatement );
			break tokenloop;

		case "public:":
			eType = AccessibilityDecl;
			eVisibility = CObjectVisibility.Public;
			iPrivacyToken = iTokenIndex;
			break tokenloop;

		case "private:":
			eType = AccessibilityDecl;
			eVisibility = CObjectVisibility.Private;
			iPrivacyToken = iTokenIndex;
			break tokenloop;

		case "protected:":
			eType = AccessibilityDecl;
			eVisibility = CObjectVisibility.Protected;
			iPrivacyToken = iTokenIndex;
			break tokenloop;

		default:
			ptrdiff_t iFoundBracket = tok.indexOf( '(' );
			if( iFoundBracket != -1 )
			{
				eType = FunctionDecl;
				iBracketToken = iTokenIndex;
				import std.stdio : writeln;
				//writeln( strFullStatement );
				HandleFunctionStatement( output, eVisibility, strStatementTokens, strExtendedStatement );
				break tokenloop;
			}
			break;
		}
	}

	if( eType == StatementType.AccessibilityDecl )
	{
		import std.algorithm : min;
		strStatementTokens = strStatementTokens[ 0 .. iPrivacyToken ] ~ strStatementTokens[ min( iPrivacyToken + 1, $ ) .. $ ];

		goto tokenparse; // YES I'M USING A GOTO HAHAHA
	}

	if( eType == StatementType.None && strStatementTokens.length > 0 )
	{
		// Assume variable
		output.HandleVariableStatement( eVisibility, strStatementTokens );
	}
	//import std.conv : to;
	//output.strRemainingLines ~= eVisibility.to!string ~ " " ~ strStatementTokens.joinWith( " - " );

	return output;
}

ref CHeader ScrapeForDefines( ref CHeader header, string strFilename, string[] strSearchPaths )
{
	import std.file : read, exists;
	import std.string : splitLines;
	import std.conv : to;
	import std.stdio : writeln;

	string *strHeaderContents = null;
	string strActualFileLocation;
	
	foreach( strPath; strSearchPaths )
	{
		strActualFileLocation = strPath ~ "/" ~ strFilename;
		if( strActualFileLocation.exists )
		{
			strHeaderContents = strActualFileLocation in header.strIncludeContents;
			if( strHeaderContents is null )
			{
				//writeln( "Loading: " ~ strActualFileLocation );
				string strContents = cast(string)cast( const( char )[] )read( strActualFileLocation ) ~ "\n";
				header.strIncludeContents[ strActualFileLocation ] = strContents;
				strHeaderContents = strActualFileLocation in header.strIncludeContents;
			}
			break;
		}
	}

	//writeln( "Scraping: ", strActualFileLocation );

	string[] strLines =	( *strHeaderContents )
						.StandardiseLineEndings
						.StripComments
						.StripExcessWhiteSpace
						.splitLines
						.CleanupLines;

/*	foreach( ref define; header.defines )
	{
		writeln( define.Type.to!string,
				" ",
				define.ID,
				( define.HasParameters > 0 ? " (" ~ define.Parameters.joinWith( ", " ) ~ ")" : "" ),
				( define.Value.length ?  " -> " ~ define.Value : "" ) );
	}*/

	header.HandlePreprocessor( strLines, CDefine.DefineType.Included, strSearchPaths );
	//writeln( "Scraping done: ", strActualFileLocation );

	return header;
}

string ReplaceWord( string strSource, string strSearchWord, string strReplaceWord )
{
	import std.string : indexOfAny;
	import std.algorithm : min;

	string strOutput;
	while( strSource.length > 0 )
	{
		ptrdiff_t iFound = strSource.indexOfAny( CTokenSymbols );
		if( iFound == -1 )
		{
			iFound = strSource.length;
		}

		string strTok = strSource[ 0 .. iFound ];
		if( strTok == strSearchWord )
		{
			strOutput ~= strReplaceWord;
		}
		else
		{
			strOutput ~= strTok;
		}

		if( iFound != strSource.length )
		{
			strOutput ~= strSource[ iFound ];
		}

		strSource = strSource[ min( iFound + 1, $ ) .. $ ];
	}

	return strOutput;
}

enum LineState { Include, Skip, SkipPendingNextCheck }

bool IncludeLine( LineState eState )
{
	return eState == LineState.Include;
}

bool RequiresRetesting( LineState eState )
{
	return eState == LineState.SkipPendingNextCheck;
}

string Consume( ref string[] strTokens )
{
	string tok = strTokens[ 0 ];
	strTokens = strTokens[ 1 .. $ ];
	return tok;
}

string Peek( ref string[] strTokens )
{
	if( strTokens.length > 0 )
	{
		return strTokens[ 0 ];
	}
	return "";
}

string ExpandPreprocessor( ref CHeader header, string strStatement )
{
	import std.stdio : writeln;

	string[] strOutput;
	string[] strTokens = strStatement.SplitTokens( CWhitespace, CSymbols );

	bool bExpanded = false;

	while( strTokens.length > 0 )
	{
		string strTok = strTokens.Consume;
		CDefine* foundDefine = strTok in header.defines;
		if( foundDefine !is null )
		{
			bExpanded = true;
			if( !foundDefine.HasParameters )
			{
				if( foundDefine.Value.length > 0 )
				{
					strTokens = foundDefine.ValueTokens ~ strTokens;
				}
			}
			else if( foundDefine.Parameters.length == 0 )
			{
				strTokens.Consume; // (
				strTokens.Consume; // )
				if( foundDefine.Value.length > 0 )
				{
					strTokens = foundDefine.ValueTokens ~ strTokens;
				}
			}
			else
			{
				string[][] strParams;
				string[] strWorking;
				strTokens.Consume; // (
				int iOpenBrackets = 1;
				while( iOpenBrackets > 0 )
				{
					string strDefTok = strTokens.Consume;
					if( strDefTok == "(" )
					{
						++iOpenBrackets;
						strWorking ~= strDefTok;
					}
					else if( strDefTok == ")" )
					{
						if( --iOpenBrackets > 0 )
						{
							strWorking ~= strDefTok;
						}
					}
					else if( iOpenBrackets == 1 && strDefTok == "," )
					{
						strParams ~= strWorking;
						strWorking.length = 0;
					}
					else
					{
						strWorking ~= strDefTok;
					}
				}

				if( foundDefine.Value.length > 0 )
				{
					if( strWorking.length > 0 )
					{
						strParams ~= strWorking;
						strWorking.length = 0;
					}

					const( string[] ) strValueParamNames = foundDefine.Parameters;
					string[] strSourceValue = cast(string[])foundDefine.ValueTokens;
					string[] strTargetValue;

					while( strSourceValue.length > 0 )
					{
						string strSourceTok = strSourceValue.Consume;
						bool bHandled = false;

						foreach( iIndex, ref strParamName; strValueParamNames )
						{
							if( strSourceTok == strParamName )
							{
								strTargetValue ~= strParams[ iIndex ];
								bHandled = true;
								break;
							}
						}

						if( !bHandled )
						{
							strTargetValue ~= strSourceTok;
						}
					}

					strTokens = strTargetValue ~ strTokens;
				}
			}
		}
		else
		{
			strOutput ~= strTok;
		}
	}

	string[] strOutputValue = strOutput;
	strOutput.length = 0;
	while( strOutputValue.length > 0 )
	{
		string strTargetTok = strOutputValue.Consume;
		if( strTargetTok == "##" )
		{
			strOutput.back ~= strOutputValue.Consume;
		}
		else if( strTargetTok == "#" )
		{
			strOutput ~= "\"" ~ strOutputValue.Consume ~ "\"";
		}
		else
		{
			strOutput ~= strTargetTok;
		}
	}

	string strFinal = strOutput.joinWith( " " );

/+	if( bExpanded )
	{
		writeln( "Expanded:" );
		writeln( " -> From: ", strStatement );
		writeln( " -> To:   ", strFinal );
	}+/

	return strFinal;

/+	bool bReExpand = true;

	while( bReExpand == true )
	{
		import std.string : indexOfAny, replace;
		import std.algorithm : min;

		bReExpand = false;

		string strPassed;
		string strRemaining = strStatement;

		while( strRemaining.length > 0 )
		{
			ptrdiff_t iFoundIndex = strRemaining.indexOfAny( CTokenSymbols );
			if( iFoundIndex == -1 )
			{
				iFoundIndex = strRemaining.length;
			}

			string strTok = strRemaining[ 0 .. iFoundIndex ];

			CDefine* foundDefine = strTok in header.defines;
			if( foundDefine !is null )
			{
				bReExpand = true;

				if( !foundDefine.HasParameters )
				{
					import std.stdio : writeln;

					//writeln( "Expanding ", foundDefine.ID, ": ", strStatement, " -> ", foundDefine.Value );
					strPassed ~= foundDefine.Value;
					strRemaining = strRemaining[ iFoundIndex .. $ ];
				}
				else
				{
					import std.stdio : writeln;

					string[] strSplitApart = strRemaining.SplitDefinition( '(', ')' );
					string strWorking = foundDefine.Value;
					if( strSplitApart[ 0 ].length > 0 )
					{
						string[] strParams = strSplitApart[ 0 ]
											.SplitContents( '(', ')' )
											.SplitApart( ',', '(', ')' );

						foreach( iIndex; 0 .. foundDefine.Parameters.length )
						{
							if( iIndex >= strParams.length )
							{
								break;
							}
							else if( strWorking.length > 0 )
							{
								strWorking = strWorking.ReplaceWord( foundDefine.Parameters[ iIndex ], strParams[ iIndex ] );
							}
						}

						strWorking = strWorking.replace( " ## ", "" ).replace( " # ", "" ).replace( "## ", "" ).replace( "# ", "" ).replace( " ##", "" ).replace( " #", "" ).replace( "##", "" ).replace( "#", "" );
						//writeln( "Expanding ", foundDefine.ID, "(", foundDefine.Parameters.joinWith( "," ), ")", /+": ", strStatement,+/ " -> ", foundDefine.Value, " ==> ", strWorking );
					}
					else
					{
						strSplitApart[ 1 ] = strSplitApart[ 1 ][ foundDefine.ID.length .. $ ];
						writeln( "Encountered ", foundDefine.ID, " but could not expand" );
					}

					strPassed ~= strWorking;
					strRemaining = strSplitApart[ 1 ];
				}
			}
			else
			{
				strPassed ~= strRemaining[ 0 .. min( iFoundIndex + 1, $ ) ];
				strRemaining = strRemaining[ min( iFoundIndex + 1, $ ) .. $ ];
			}
		}

		/*{
			import std.stdio : writeln;
			writeln( "Expanded:" );
			writeln( "\t From -> ", strStatement );
			writeln( "\t To -> ", strPassed );
		}*/

		strStatement = strPassed;
	}

	return strStatement;+/
}

enum Assign			= 0b0000000000000;
enum Equals			= 0b0000000000001;
enum GreaterThan	= 0b0000000000010;
enum LessThan		= 0b0000000000100;
enum And			= 0b0000000001000;
enum Or				= 0b0000000010000;
enum Xor			= 0b0000000100000;
enum Logical		= 0b0000001000000;
enum Not			= 0b0000010000000;
enum Addition		= 0b0000100000000;
enum Subtraction	= 0b0001000000000;
enum Multiply		= 0b0010000000000;
enum Divide			= 0b0100000000000;
enum Modulus		= 0b1000000000000;

enum EQ				= Equals;
enum NEQ			= Not | Equals;
enum GT				= GreaterThan;
enum GTE			= GreaterThan | Equals;
enum LT				= LessThan;
enum LTE			= LessThan | Equals;
enum BWA			= And;
enum BWO			= Or;
enum BWX			= Xor;
enum LGA			= Logical | And;
enum LGO			= Logical | Or;
enum ADD			= Addition;
enum SUB			= Subtraction;
enum MUL			= Multiply;
enum DIV			= Divide;
enum MOD			= Modulus;

string OpString( int op )
{
	switch( op )
	{
	case Assign: return "AVL";
	case Not: return "NVL";
	case EQ: return "EQ";
	case NEQ: return "NEQ";
	case GT: return "GT";
	case GTE: return "GTE";
	case LT: return "LT";
	case LTE: return "LTE";
	case BWA: return "BWA";
	case BWO: return "BWO";
	case BWX: return "BWX";
	case LGA: return "LGA";
	case LGO: return "LGO";
	case ADD: return "ADD";
	case SUB: return "SUB";
	case MUL: return "MUL";
	case DIV: return "DIV";
	case MOD: return "MOD";
	default: return "UDF";
	}
}

@BindNoExportObject
struct Operation
{
	int 		op = Assign;
	string 		strVal = "0";
}

auto CPPNumericTo( T )( string strVal )
{
	import std.ascii : toLower;
	import std.conv : to;

	while( strVal.back.toLower == 'u' || strVal.back.toLower == 'l' )
	{
		strVal.popBack;
	}

	return strVal.to!T;
}

int ResolveOperations( ref CHeader header, Operation[] pendingOps )
{
	int iResolution = 0;
	import std.string : isNumeric;
	import std.conv : to;
	import std.stdio : writeln;

	foreach( ref op; pendingOps )
	{
		import std.stdio : writeln;
		//writeln( "op ", op.op.OpString, " - ", op.strVal );
		int iNew = ( op.strVal.isNumeric ? op.strVal.CPPNumericTo!int : header.ExpandPreprocessor( op.strVal ).CPPNumericTo!int );
		switch( op.op )
		{
		case Assign:
			iResolution = iNew;
			break;
		case Not:
			iResolution = !iNew;
			break;
		case EQ:
			iResolution = !!( iResolution == iNew );
			break;
		case NEQ:
			iResolution = !!( iResolution != iNew );
			break;
		case GT:
			iResolution = !!( iResolution > iNew );
			break;
		case GTE:
			iResolution = !!( iResolution >= iNew );
			break;
		case LT:
			iResolution = !!( iResolution < iNew );
			break;
		case LTE:
			iResolution = !!( iResolution <= iNew );
			break;
		case BWA:
			iResolution &= iNew;
			break;
		case BWO:
			iResolution |= iNew;
			break;
		case BWX:
			iResolution ^= iNew;
			break;
		case LGA:
			iResolution = !!( iResolution && iNew );
			break;
		case LGO:
			iResolution = !!( iResolution || iNew );
			break;
		case ADD:
			iResolution += iNew;
			break;
		case SUB:
			iResolution -= iNew;
			break;
		case MUL:
			iResolution *= iNew;
			break;
		case DIV:
			iResolution /= iNew;
			break;
		case MOD:
			iResolution %= iNew;
			break;
		default:
			break;
		}
	}

	pendingOps = [ Operation.init ];
	return iResolution;
}

int ResolveExpression( ref CHeader header, ref string[] strTokens )
{
	import std.stdio : writeln;
	import std.conv : to;

	string[] strOrigTokens = strTokens;
	//writeln( "EXPRESSION: ", strTokens.joinWith( " <-> " ) );

	Operation[] pendingOps = [ Operation.init ];

	tokenparse: while( strTokens.length > 0 )
	{
		string strTok = strTokens.Consume;
		switch( strTok )
		{
		case "!":
			pendingOps.back.op ^= Not;
			break;

		case "defined":
			string strSearchFor;
			int iPopCloseBrackets = 0;
			while( strTokens.Peek == "(" )
			{
				strTokens.Consume; // (
				++iPopCloseBrackets;
			}
			strSearchFor = strTokens.Consume;
			while( iPopCloseBrackets > 0 )
			{
				strTokens.Consume; // )
				--iPopCloseBrackets;
			}
			
			CDefine* pFoundDefine = strSearchFor in header.defines;
			pendingOps.back.strVal = ( pFoundDefine !is null ) ? "1" : "0";
			break;

		case "==":
			Operation newOp = Operation( EQ, "" );
			pendingOps.pushBack( newOp );
			break;
		case "!=":
			Operation newOp = Operation( NEQ, "" );
			pendingOps.pushBack( newOp );
			break;

		case ">":
			Operation newOp = Operation( GT, "" );
			pendingOps.pushBack( newOp );
			break;
		case ">=":
			Operation newOp = Operation( GTE, "" );
			pendingOps.pushBack( newOp );
			break;

		case "<":
			Operation newOp = Operation( LT, "" );
			pendingOps.pushBack( newOp );
			break;
		case "<=":
			Operation newOp = Operation( LTE, "" );
			pendingOps.pushBack( newOp );
			break;

		case "+":
			Operation newOp = Operation( ADD, "" );
			pendingOps.pushBack( newOp );
			break;

		case "-":
			Operation newOp = Operation( SUB, "" );
			pendingOps.pushBack( newOp );
			break;

		case "*":
			Operation newOp = Operation( MUL, "" );
			pendingOps.pushBack( newOp );
			break;

		case "/":
			Operation newOp = Operation( DIV, "" );
			pendingOps.pushBack( newOp );
			break;

		case "%":
			Operation newOp = Operation( MOD, "" );
			pendingOps.pushBack( newOp );
			break;

		case "&&":
			int iResolvedSuccessfully = header.ResolveOperations( pendingOps );
			if( iResolvedSuccessfully )
			{
				pendingOps = [ Operation( Assign, header.ResolveExpression( strTokens ).to!string ) ];
				break tokenparse;
			}
			else
			{
				pendingOps = [ Operation.init ];
				break tokenparse;
			}

		case "||":
			int iResolvedSuccessfully = header.ResolveOperations( pendingOps );
			if( !iResolvedSuccessfully )
			{
				pendingOps = [ Operation( Assign, header.ResolveExpression( strTokens ).to!string ) ];
				break tokenparse;
			}
			else
			{
				pendingOps = [ Operation.init ];
				break tokenparse;
			}

		case "(":
			string[] strNewTokens;
			int iBracketCount = 1;
			while( iBracketCount > 0 )
			{
				string strNewTok = strTokens.Consume;
				if( strNewTok == "(" )
				{
					++iBracketCount;
				}
				else if( strNewTok == ")" )
				{
					--iBracketCount;
				}

				if( iBracketCount > 0 )
				{
					strNewTokens ~= strNewTok;
				}
			}
			pendingOps.back.strVal = header.ResolveExpression( strNewTokens ).to!string;
			break;

		default:
			string strDefn = strTok;
			int iPopCloseBrackets = 0;
			if( strTokens.Peek == "(" )
			{
				int iBracketCount = 0;
				do
				{
					string strNewTok = strTokens.Consume;
					strDefn ~= strNewTok;
					if( strNewTok == "(" )
					{
						++iBracketCount;
					}
					else if( strNewTok == ")" )
					{
						--iBracketCount;
					}

				} while( iBracketCount > 0 );
			}

			string strExpanded = header.ExpandPreprocessor( strDefn );
			if( strExpanded == strDefn )
			{
				pendingOps.back.strVal = strExpanded;
			}
			else
			{
				strTokens = strExpanded.SplitTokens( CWhitespace, CSymbols ) ~ strTokens;
			}
			break;
		}
	}

	int iResolution = header.ResolveOperations( pendingOps );
	//writeln( "EXPRESSION RESULT: ", strOrigTokens.joinWith( " <-> " ), " ===> ", iResolution );
	return iResolution;
}

ref CHeader HandlePreprocessor( ref CHeader output, string[] strLines, CDefine.DefineType eDefineType, string[] strSearchPaths )
{
	import std.string : startsWith, split, replace;
	import std.algorithm : countUntil, min;
	import std.string : indexOfAny;

	LineState[] currLineState = [ LineState.Include ];

	foreach( strLine; strLines )
	{
		import std.stdio : writeln;
		import std.conv : to;

		string[] strExpTokens = strLine.SplitTokens( CWhitespace, CSymbols );
		//writeln( currLineState.back.to!string, ": ", strExpTokens.joinWith( " <-> " ) );

		if( strExpTokens.Consume == "#" )
		{
			switch( strExpTokens.Consume )
			{
			case "define":
				if( currLineState.back.IncludeLine )
				{
					CDefine currDefine;

					string strIdentifier = strExpTokens.Consume;
					currDefine.ID = strIdentifier;

					if( strExpTokens.length > 0 )
					{
						currDefine.Value = strExpTokens.joinWith( " " );
					}
					currDefine.Type = eDefineType;

					output.defines[ currDefine.ID ] = currDefine;

					//writeln( strLine ~ " (" ~ strIdentifier ~ ") <==> " ~ currDefine.ID ~ " => " ~ currDefine.Value );
				}
				break;
			case "undef":
				if( currLineState.back.IncludeLine )
				{
					CDefine* foundDefine = strExpTokens.Consume in output.defines;
					if( foundDefine !is null )
					{
						foundDefine.Undefined = true;
					}
				}
				break;

			case "include":
				if( currLineState.back.IncludeLine )
				{
					string strFilename;
					if( strExpTokens.Peek != "<" )
					{
						strFilename = strExpTokens.Consume.replace( "\"", "" );

						if( eDefineType != CDefine.DefineType.Included && output.strIncludes.countUntil( strFilename ) == -1 )
						{
							output.strIncludes ~= strFilename;
						}
						else if( output.strAdditionalIncludes.countUntil( strFilename ) == -1 )
						{
							output.strAdditionalIncludes ~= strFilename;
						}
						output.ScrapeForDefines( strFilename, strSearchPaths );
					}
					else
					{
						strFilename = strExpTokens[ 1 .. $ - 1 ].joinWith( " " );
						output.strSystemIncludes ~= strFilename;
					}
				}
				break;

			case "if":
				if( currLineState.back.IncludeLine )
				{
					currLineState.pushBack( output.ResolveExpression( strExpTokens ) ? LineState.Include : LineState.SkipPendingNextCheck );
				}
				else
				{
					currLineState.pushBack( LineState.Skip );
				}
				break;

			case "ifdef":
				if( currLineState.back.IncludeLine )
				{
					string[] strNewTokens = [ "defined", "(", strExpTokens.Consume, ")" ];
					currLineState.pushBack( output.ResolveExpression( strNewTokens ) ? LineState.Include : LineState.SkipPendingNextCheck );
				}
				else
				{
					currLineState.pushBack( LineState.Skip );
				}
				break;

			case "ifndef":
				if( currLineState.back.IncludeLine )
				{
					string[] strNewTokens = [ "!", "defined", "(", strExpTokens.Consume, ")" ];
					currLineState.pushBack( output.ResolveExpression( strNewTokens ) ? LineState.Include : LineState.SkipPendingNextCheck );
				}
				else
				{
					currLineState.pushBack( LineState.Skip );
				}
				break;
			
			case "elif":
				if( currLineState.back.RequiresRetesting )
				{
					currLineState.back = ( output.ResolveExpression( strExpTokens ) ? LineState.Include : LineState.SkipPendingNextCheck );
				}
				else
				{
					currLineState.back = LineState.Skip;
				}
				break;

			case "else":
				if( currLineState.back.RequiresRetesting )
				{
					currLineState.back = LineState.Include;
				}
				else
				{
					currLineState.back = LineState.Skip;
				}
				break;
			
			case "endif":
				currLineState.popBack;
				break;
			case "pragma":
				break;
			case "error":
				break;
			default:
				break;
			}
		}
		else if( currLineState.back.IncludeLine && eDefineType != CDefine.DefineType.Included )
		{
			string strNewLine = output.ExpandPreprocessor( strLine );
			output.strRemainingLines ~= strNewLine;
		}
	}
	
	return output;
}

CHeader HandleCleanHeader( string[] strLines, CDefine[ string ] predefines, bool bPreprocessorOnly, string[] strSearchPaths )
{
	ref CHeader HandleStatements( ref CHeader output )
	{
		string[] strStatements = output.strRemainingLines.BuildStatements;
		output.strRemainingLines.length = 0; // = strStatements;

		foreach( ref statement; strStatements )
		{
			CObjectVisibility eVisibility = CObjectVisibility.Public;
			HandleStatement( output.global, eVisibility, statement );
		}

		return output;
	}

	CHeader header;
	header.defines = predefines;

	header.strRemainingLines = Reserve!( string[] )( strLines.length );

	HandlePreprocessor( header, strLines, CDefine.DefineType.CodeDefined, strSearchPaths );

	if( !bPreprocessorOnly )
	{
		HandleStatements( header );
	}
	
	return header;
}

mixin BindModule!1;
