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

module binderoo.binding.cheaderobjects;

import binderoo.traits;

CDefine[ string ] NoPredefines() { CDefine[ string ] nothing; return nothing; }
CDefine[ string ] BasicPredefines()
{
	CDefine[ string ] defines;

	void add( string ID, string Val )
	{
		defines[ ID ] = CDefine( ID, Val );
	}

	add( "__cplusplus", 		"201103L" );
	add( "__STDC_VERSION__", 	"201112L" );
	add( "__STDC__", 			"1" );

	version( X86_64 )
	{
		add( "__CHAR16_TYPE__", "unsigned short" );
		add( "__CHAR32_TYPE__", "unsigned int" );
		add( "__INT16_TYPE__", "short" );
		add( "__INT32_TYPE__", "int" );
		add( "__INT64_TYPE__", "long int" );
		add( "__INT8_TYPE__", "signed char" );
		add( "__INTMAX_TYPE__", "long int" );
		add( "__INTPTR_TYPE__", "long int" );
		add( "__INT_FAST16_TYPE__", "short" );
		add( "__INT_FAST32_TYPE__", "int" );
		add( "__INT_FAST64_TYPE__", "long int" );
		add( "__INT_FAST8_TYPE__", "signed char" );
		add( "__INT_LEAST16_TYPE__", "short" );
		add( "__INT_LEAST32_TYPE__", "int" );
		add( "__INT_LEAST64_TYPE__", "long int" );
		add( "__INT_LEAST8_TYPE__", "signed char" );
		add( "__PTRDIFF_TYPE__", "long int" );
		add( "__SIZE_TYPE__", "long unsigned int" );
		add( "__UINT16_TYPE__", "unsigned short" );
		add( "__UINT32_TYPE__", "unsigned int" );
		add( "__UINT64_TYPE__", "long unsigned int" );
		add( "__UINT8_TYPE__", "unsigned char" );
		add( "__UINTMAX_TYPE__", "long unsigned int" );
		add( "__UINTPTR_TYPE__", "long unsigned int" );
		add( "__UINT_FAST16_TYPE__", "unsigned short" );
		add( "__UINT_FAST32_TYPE__", "unsigned int" );
		add( "__UINT_FAST64_TYPE__", "long unsigned int" );
		add( "__UINT_FAST8_TYPE__", "unsigned char" );
		add( "__UINT_LEAST16_TYPE__", "unsigned short" );
		add( "__UINT_LEAST32_TYPE__", "unsigned int" );
		add( "__UINT_LEAST64_TYPE__", "long unsigned int" );
		add( "__UINT_LEAST8_TYPE__", "unsigned char" );
		version( Windows ) add( "__WCHAR_TYPE__", "short" );
		else add( "__WCHAR_TYPE__", "int" );
		add( "__WINT_TYPE__", "unsigned int" );
	}
	return defines;
}

template HeaderID( string strHeaderLocation )
{
	import std.string : replace;
	enum HeaderID = strHeaderLocation.replace( "/", "_" ).replace( "\\", "_" ).replace( ".", "_" );
}


string GetModuleNameFromHeader( string strHeaderLocation )
{
	import std.string : split, replace;
	strHeaderLocation = strHeaderLocation	.replace( ".h", "" )
											.replace( ".hpp", "" )
											.replace( ".inl", "" )
											.replace( "\\", "/" );

	string[] strPotentialModules = strHeaderLocation.split( "/" );

	if( strPotentialModules.length > 1 && strPotentialModules[ $ - 2 ] == strPotentialModules[ $ - 1 ] )
	{
		strPotentialModules = strPotentialModules[ 0 .. $ - 1 ];
	}

	return strPotentialModules.joinWith( "." );
}

string GetModuleFolderFromHeader( string strHeaderLocation )
{
	import std.string : split, replace;
	strHeaderLocation = strHeaderLocation	.replace( ".h", "" )
											.replace( ".hpp", "" )
											.replace( ".inl", "" )
											.replace( "\\", "/" );

	string[] strPotentialModules = strHeaderLocation.split( "/" );

	if( strPotentialModules.length == 1 )
	{
		return ".";
	}

	return strPotentialModules[ 0 .. $ - 1 ].joinWith( "/" );
}

string GetModuleFilenameFromHeader( string strHeaderLocation )
{
	import std.string : split, replace;
	strHeaderLocation = strHeaderLocation	.replace( ".h", "" )
											.replace( ".hpp", "" )
											.replace( ".inl", "" )
											.replace( "\\", "/" );

	string[] strPotentialModules = strHeaderLocation.split( "/" );

	if( strPotentialModules.length > 1 && strPotentialModules[ $ - 2 ] == strPotentialModules[ $ - 1 ] )
	{
		strPotentialModules[ $ - 1 ] = "package";
	}

	return strPotentialModules.joinWith( "/" ) ~ ".d";
}

template HeaderModule( string strHeaderLocation )
{
	enum HeaderModule = strHeaderLocation.GetModuleNameFromHeader();
}

template HeaderModuleFilename( string strHeaderLocation )
{
	enum HeaderModuleFilename = strHeaderLocation.GetModuleFilenameFromHeader();
}

template HeaderData( string strHeaderLocation )
{
	mixin( "import " ~ HeaderModule!strHeaderLocation ~ ";" );
	mixin( "alias HeaderData = " ~ HeaderID!strHeaderLocation ~ ";" );
}

struct CDefine
{
	enum DefineType
	{
		PreDefined,
		Included,
		CodeDefined,
	};

	enum ValueType
	{
		None,
		Expression,
		Integral,
		String,
	}

	private string		strIdentifier;
	private bool		bHasParameters;
	private bool		bUndefined;
	private string[]	strParameters;
	private string		strValue;
	private string[]	strValueTokens;
	private DefineType	eType;
	private ValueType	eValueType;

	this( string id )
	{
		ID = id;
	}

	this( string id, string val )
	{
		ID = id;
		Value = val;
	}

	@property ID() const			{ return strIdentifier; }
	@property Value() const			{ return strValue; }
	@property ValueTokens() const	{ return strValueTokens; }
	@property Type() const			{ return eType; }
	@property HasParameters() const	{ return bHasParameters; }
	@property Parameters() const	{ return strParameters; }
	@property Valid() const			{ return !bUndefined; }

	@property IsLiteral() const 	{ return eValueType == ValueType.Integral || eValueType == ValueType.String; }

	package @property ID( string id )
	{
		import std.algorithm : countUntil;
		
		ptrdiff_t iIndex = id.countUntil( '(' );
		if( iIndex >= 0 )
		{
			strIdentifier = id[ 0 .. iIndex ];
			bHasParameters = true;
			strParameters = id.SplitContents( '(', ')' ).SplitApart( ',', '(', ')' );

			foreach( ref param; strParameters )
			{
				param = param.StripExcessWhiteSpace;
			}
		}
		else
		{
			strIdentifier = id;
		}
	}

	package @property Value( string val )
	{
		strValue = val;

		import std.string : isNumeric, startsWith, endsWith;

		if( val.isNumeric || val.startsWith( "0x" ) )
		{
			eValueType = ValueType.Integral;
		}
		else if( val.startsWith( "\"" ) && val.endsWith( "\"" ) )
		{
			eValueType = ValueType.String;
		}
		else
		{
			eValueType = ValueType.Expression;
		}

		strValueTokens = val.SplitTokens( CWhitespace, CSymbols );
	}

	package @property Type( DefineType type )
	{
		eType = type;
	}

	package @property Undefined( bool b )
	{
		bUndefined = b;
	}
}

enum CObjectVisibility
{
	Public = "public",
	Private = "private",
	Protected = "protected",
}

CObjectVisibility DefaultVisibility( CAggregate.Type eType )
{
	final switch( eType ) with( CAggregate.Type ) with( CObjectVisibility )
	{
		case Namespace:
		case Union:
		case Struct:
			return Public;
		case Class:
			return Private;
	}
}

struct CTypeDef
{
	enum Type
	{
		TypeAlias,
		FunctionAlias
	}

	string				strIdentifier;
	string				strActualType;
	Type				eType;

	string				strDDecl;

	CObjectVisibility	eVisibility;
}

struct CVariable
{
	string				strIdentifier;
	string				strType;
	string				strDefault;
	string				strBitfieldSize;

	@property FullDeclCPP() const
	{
		return strType ~ " " ~ strIdentifier ~ ( strBitfieldSize.length > 0 ? " : " ~ strBitfieldSize : strDefault.length > 0 ? " = " ~ strDefault : "" );
	}

	CObjectVisibility	eVisibility;
}

struct CFunction
{
	enum Type
	{
		Function,
		Static,
		Virtual,
		NonVirtual,
		Constructor,
		Destructor,
		VirtualDestructor,
	}

	@property CanReturnValue() const
	{
		switch( eType ) with( Type )
		{
		case Constructor:
		case Destructor:
		case VirtualDestructor:
			return false;
		default:
			return true; // strReturnType != "void";
		}
	}

	@property IsVirtual() const { return eType == Type.Virtual || eType == Type.VirtualDestructor; }

	string 				strIdentifier;
	string				strFullyQualifiedName;
	Type 				eType = Type.Function;

	string 				strReturnType;

	CVariable[] 		parameters;

	bool				bReturnsRef;
	bool				bIsConst;
	bool				bIsAbstract;

	CObjectVisibility	eVisibility;
}

struct CTemplateParam
{
	enum Type
	{
		Typename		= "typename",
		Struct			= "struct",
		Class			= "class",
		Variable		= "variable",
	}

	string				strIdentifier;
	string				strVariableType;
	string				strDefaultValue;

	Type				eType;

	CObjectVisibility	eVisibility;
}

struct CAggregate
{
	enum Type
	{
		Namespace = "namespace",
		Struct = "struct",
		Class = "class",
		Union = "union",
	}

	struct Derived
	{
		string				strIdentifier;
		CObjectVisibility	eVisibility;
	}

	@property IsAbstract() const
	{
		bool bAbstract = false;
		foreach( ref func; functions )
		{
			bAbstract |= func.bIsAbstract;
		}
		return bAbstract;
	}

	string 				strIdentifier;
	Type				eType;

	string				strParentAggregate;
	string				strFullyQualifiedName;

	Derived[]			baseAggregates;
	CTemplateParam[]	templateParams;

	CVariable[]			variables;
	CFunction[]			functions;
	CAggregate[]		types;
	CEnum[]				enums;
	CTypeDef[]			typedefs;

	CObjectVisibility	eVisibility;

	string[]			strRemainingLines;
}

struct CEnum
{
	string				strIdentifier;
	string				strFullyQualifiedName;
	string				strUnderlyingType = "int";
	bool				bIsClass;
	CVariable[] 		allValues;

	CObjectVisibility 	eVisibility;
}

struct CHeader
{
	string				strLocation;
	
	string[]			strIncludes;
	string[]			strAdditionalIncludes;
	string[ string ]	strIncludeContents;
	string[]			strSystemIncludes;

	CDefine[ string ]	defines;

	CAggregate			global;

	string[]			strRemainingLines;
}

package:

enum StandardisedLineEnd = "\x0A";
enum MSLineEnd = "\x0D\x0A";
enum CSymbols = "()<>~!^&|~%+-*/=.,#:;?\\\"";
enum CWhitespace = "\t\x20\xA0";
enum CTokenSymbols = CSymbols ~ CWhitespace;
enum IgnoreGroupTemplatesAndFunctions = [ IgnoreGroup!(char)( '(', ')' ), IgnoreGroup!(char)( '<', '>' ) ];

string[] SplitTokens( string strSource, string strHardBreaks, string strInclusiveBreaks )
{
	import std.string : indexOf;
	string[] strOutput;

	string strWorking;
	bool bFoundDefine = false;
	bool bHitInclusive = false;
	bool bHitQuotationMark = false;
	bool bHitEscape = false;
	int iBracketCount = 0;

	void pushWorking()
	{
		if( strWorking.length > 0 )
		{
			strOutput ~= strWorking;
			bFoundDefine = ( strWorking == "define" );
			strWorking.length = 0;
		}
	}

	foreach( iIndex, ref thisChar; strSource )
	{
		ptrdiff_t iInclusiveIndex = strInclusiveBreaks.indexOf( thisChar );
		ptrdiff_t iHardIndex = strHardBreaks.indexOf( thisChar );

		// Egregious hack
		if( bFoundDefine )
		{
			if( thisChar == '(' )
			{
				strWorking ~= thisChar;
				++iBracketCount;
			}
			else if( thisChar == ')' )
			{
				strWorking ~= thisChar;
				if( --iBracketCount == 0 )
				{
					pushWorking();
				}
			}
			else if( iBracketCount == 0 && iHardIndex != -1 )
			{
				pushWorking();
			}
			else
			{
				strWorking ~= thisChar;
			}
			continue;
		}

		if( thisChar == '(' || thisChar == ')' || thisChar == ',' )
		{
			pushWorking();
			strWorking ~= thisChar;
			pushWorking();
		}
		else if( iHardIndex != -1 )
		{
			pushWorking();
		}
		else if( !bHitInclusive && iInclusiveIndex != -1 )
		{
			if( bHitQuotationMark && thisChar == '\\' )
			{
				bHitEscape = true;
			}
			else if( bHitEscape )
			{
				bHitEscape = false;
			}
			else if( !bHitEscape && thisChar == '\"' )
			{
				bHitQuotationMark = !bHitQuotationMark;
			}
			else if( !bHitQuotationMark )
			{
				bHitInclusive = true;
				pushWorking();
			}
			strWorking ~= thisChar;
		}
		else if( !bHitQuotationMark && bHitInclusive )
		{
			if( iInclusiveIndex == -1 )
			{
				bHitInclusive = false;
				pushWorking();
			}
			strWorking ~= thisChar;
		}
		else
		{
			strWorking ~= thisChar;
		}
	}

	if( strWorking.length > 0 )
	{
		strOutput ~= strWorking;
	}

	return strOutput;
}

struct IgnoreGroup( T )
{
	T countFirst;
	T countLast;
}

auto SplitApart( bool bDebug = false )( string container, char delimiter, char ignoreGroupFirst, char ignoreGroupLast )
{
	string[] output;
	ptrdiff_t iBegin = 0;
	ptrdiff_t iEnd = 0;

	ptrdiff_t iIgnoreGroupCount = 0;

	foreach( iIndex, ref currVal; container )
	{
		if( currVal == delimiter && iIgnoreGroupCount == 0 )
		{
			if( iBegin != iEnd )
			{
				static if( bDebug )
				{
					import std.stdio : writeln;
					writeln( "New split ", container[ iBegin .. iEnd ] );
				}
				output ~= container[ iBegin .. iEnd ];
			}
			iBegin = iEnd = iIndex + 1;
		}
		else if( currVal == ignoreGroupLast )
		{
			--iIgnoreGroupCount;
			++iEnd;
		}
		else if( currVal == ignoreGroupFirst )
		{
			++iIgnoreGroupCount;
			++iEnd;
		}
		else
		{
			++iEnd;
		}
	}

	if( iBegin != iEnd )
	{
		static if( bDebug )
		{
			import std.stdio : writeln;
			writeln( "New split ", container[ iBegin .. iEnd ] );
		}
		output ~= container[ iBegin .. iEnd ];
	}

	return output;
}

auto SplitApart( bool bDebug = false )( string container, char delimiter, IgnoreGroup!(char)[] ignoreGroups )
{
	import std.string : indexOf;

	string[] output;
	ptrdiff_t iBegin = 0;
	ptrdiff_t iEnd = 0;

	ptrdiff_t iIgnoreGroupCount = 0;

	string strIgnoreGroupFirst = Reserve!string( ignoreGroups.length );
	string strIgnoreGroupLast = Reserve!string( ignoreGroups.length );

	foreach( ref group; ignoreGroups )
	{
		strIgnoreGroupFirst ~= group.countFirst;
		strIgnoreGroupLast ~= group.countLast;
	}

	char ignoreGroupLast = 0x1A; // EOF

	foreach( iIndex, ref currVal; container )
	{
		ptrdiff_t iFoundIndex = -1;

		if( iIgnoreGroupCount == 0 && currVal == delimiter )
		{
			if( iBegin != iEnd )
			{
				output ~= container[ iBegin .. iEnd ];
			}
			iBegin = iEnd = iIndex + 1;
		}
		else if( ( iFoundIndex = strIgnoreGroupFirst.indexOf( currVal ) ) != -1 )
		{
			ignoreGroupLast = strIgnoreGroupLast[ iFoundIndex ];
			++iIgnoreGroupCount;
			++iEnd;
		}
		else if( currVal == ignoreGroupLast )
		{
			--iIgnoreGroupCount;
			++iEnd;
		}
		else
		{
			++iEnd;
		}
	}

	if( iBegin != iEnd )
	{
		output ~= container[ iBegin .. iEnd ];
	}

	return output;
}

auto SplitContents( string container, char ignoreGroupFirst, char ignoreGroupLast )
{
	string strOutput;

	ptrdiff_t iBegin = -1;
	ptrdiff_t iEnd = -1;

	ptrdiff_t iIgnoreGroupCount = 0;

	foreach( iIndex, ref currVal; container )
	{
		if( currVal == ignoreGroupFirst )
		{
			if( iIgnoreGroupCount++ == 0 )
			{
				iBegin = iIndex + 1;
			}
		}
		else if( currVal == ignoreGroupLast )
		{
			if( --iIgnoreGroupCount == 0 )
			{
				iEnd = iIndex;
			}
		}
	}

	if( iBegin >= 0 && iEnd >= iBegin )
	{
		strOutput = container[ iBegin .. iEnd ]; //.StripExcessWhiteSpace;
	}

	return strOutput;
}

string StripExcessWhiteSpace( string strSource )
{
	import std.string : indexOfAny;
	import std.algorithm : min, endsWith;

	string strOutput = Reserve!string( strSource.length );

	ptrdiff_t iIndex = -1;
	while( ( iIndex = strSource.indexOfAny( StandardisedLineEnd ~ CWhitespace ) ) != -1 ) // Normal space and NBSP is accounted for...
	{
		if( iIndex != 0 )
		{
			if( strOutput.length > 0 && !strOutput.endsWith( StandardisedLineEnd ) )
			{
				strOutput ~= "\x20";
			}
			strOutput ~= strSource[ 0 .. iIndex ];
		}
		if( strSource[ iIndex ] == StandardisedLineEnd[ 0 ] )
		{
			strOutput ~= StandardisedLineEnd;
		}
		strSource = strSource[ min( iIndex + 1, $ ) .. $ ];
	}

	strOutput ~= strSource;

	return strOutput;
}

auto Reserve( T )( size_t length )
{
	T reserved;
	reserved.length = length;
	reserved.length = 0;
	return reserved;
}

