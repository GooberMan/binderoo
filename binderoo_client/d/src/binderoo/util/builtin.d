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

module binderoo.util.builtin;

import binderoo.binding.binding;
import binderoo.binding.inheritance;
import binderoo.traits;

@BindExport( 1 )
extern( C++ ) void GenerateUtilFnPtrMap( int MaxParamCount )
{
	import std.stdio : writeln;
	import std.conv : to;

	enum TopContents =		"/*\n"
							~ "Binderoo\n"
							~ "Copyright (c) 2016, Remedy Entertainment\n"
							~ "All rights reserved.\n"
							~ "\n"
							~ "Redistribution and use in source and binary forms, with or without\n"
							~ "modification, are permitted provided that the following conditions are met:\n"
							~ "    * Redistributions of source code must retain the above copyright\n"
							~ "      notice, this list of conditions and the following disclaimer.\n"
							~ "    * Redistributions in binary form must reproduce the above copyright\n"
							~ "      notice, this list of conditions and the following disclaimer in the\n"
							~ "      documentation and/or other materials provided with the distribution.\n"
							~ "    * Neither the name of the copyright holder (Remedy Entertainment) nor the\n"
							~ "      names of its contributors may be used to endorse or promote products\n"
							~ "      derived from this software without specific prior written permission.\n"
							~ "\n"
							~ "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\" AND\n"
							~ "ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED\n"
							~ "WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE\n"
							~ "DISCLAIMED. IN NO EVENT SHALL REMEDY ENTERTAINMENT BE LIABLE FOR ANY\n"
							~ "DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES\n"
							~ "(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;\n"
							~ "LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND\n"
							~ "ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n"
							~ "(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\n"
							~ "SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"
							~ "*/\n"
							~ "//----------------------------------------------------------------------------\n"
							~ "\n"
							~ "// THIS FILE IS AUTOGENERATED - DO NOT MODIFY!\n"
							~ "// Run binderoo.util.builtin.GenerateUtilFnPtrMap for an updated version\n"
							~ "//----------------------------------------------------------------------------\n"
							~ "\n"
							~ "#include \"binderoo/defs.h\"\n"
							~ "#include \"binderoo/functiontraits.h\"\n"
							~ "#include \"binderoo/imports.h\"\n"
							~ "#include \"paramhandler.h\"\n"
							~ "\n"
							~ "#include <map>\n"
							~ "//----------------------------------------------------------------------------\n"
							~ "\n";
	
	enum BottomContents =	"void handleFunction( const char* pFunctionName, ParamHandler& parameters )\n"
							~ "{\n"
							~ "\tconst binderoo::BoundFunction* pFunction = binderoo::Host::getActiveHost()->getImportedFunctionDetails( pFunctionName );\n"
							~ "\tif( pFunction )\n"
							~ "\t{\n"
							~ "\t\tauto found = mapSignaturesToHandlers.find( pFunction->functionHashes.uFunctionSignatureHash );\n"
							~ "\t\tif( found != mapSignaturesToHandlers.end() )\n"
							~ "\t\t{\n"
							~ "\t\t\tfound->second( pFunctionName, parameters );\n"
							~ "\t\t}\n"
							~ "\t\telse\n"
							~ "\t\t{\n"
							~ "\t\t\tparameters.setReturn( \"Function cannot be called!\" );\n"
							~ "\t\t}\n"
							~ "\t}\n"
							~ "\telse\n"
							~ "\t{\n"
							~ "\t\tparameters.setReturn( \"Function does not exist!\" );\n"
							~ "\t}\n"
							~ "}\n";

	string[] GenerateTypeNames()
	{
		alias Types = TypeTuple!( void, const(char*), bool, char, short, int, long, float );
		string[] strOutput;
		static foreach( Type; Types )
		{
			strOutput ~= CTypeString!Type;
		}
		return strOutput;
	}

	enum SupportedTypes = GenerateTypeNames();
	enum ParamTypes = SupportedTypes[ 1 .. $ ];
	//enum MaxParamCount = 4;

	struct Permutation
	{
		string strReturnType;
		string strParamTypes;
		string strTemplateTypes;
		string strSignature;
		string strHash;
		ulong uHash;
		ulong iParamCount;
	}

	string[][] CreateParamPermutations( int iRemainingPermutations, string[] strBase = [] )
	{
		string[][] strOutput;
		foreach( CurrType; ParamTypes )
		{
			string[] strNewBase = strBase ~ CurrType;
			if( iRemainingPermutations > 0 )
			{
				strOutput ~= CreateParamPermutations( iRemainingPermutations - 1, strNewBase );
			}
			else
			{
				strOutput ~= strNewBase;
			}
		}

		return strOutput;
	}

	string[][] CreateAllParamPermutations()
	{
		string[][] strOutput;
		strOutput ~= [ "" ];
		foreach( iCurrParamCount; 0 .. MaxParamCount )
		{
			strOutput ~= CreateParamPermutations( iCurrParamCount );
		}

		return strOutput;
	}

	Permutation[] GeneratePermutations( int iMaxParamCount )
	{
		import std.math : pow;
		import std.algorithm : count;

		Permutation[] output; // = Reserve!( Permutation[] )( SupportedTypes.length + pow( SupportedTypes.length - 1, iMaxParamCount ) );

		string[][] strParamPermutations = CreateAllParamPermutations();

		foreach( Type; SupportedTypes )
		{
			foreach( params; strParamPermutations )
			{
				Permutation p;
				p.strReturnType = Type;
				p.strParamTypes = params.joinWith( ", " );
				string[] templateTypes;
				templateTypes ~= Type;
				if( params[ 0 ].length > 0 )
				{
					templateTypes ~= params;
				}
				p.strTemplateTypes = templateTypes.joinWith( ", " );
				p.strSignature = p.strReturnType ~ "(*)(" ~ p.strParamTypes ~ ")";
				p.uHash = p.strSignature.fnv1a_64;
				p.strHash = p.uHash.to!string ~ "ull";

				output ~= p;
			}
		}

		return output;
	}

	Permutation[] permutations = GeneratePermutations( MaxParamCount );

	writeln( TopContents );

	writeln( "static std::map< uint64_t, void(*)(const char*, ParamHandler&) > mapSignaturesToHandlers =\n{" );

	//{ 14174537562445187860ull, &invoker_0 },		//void()
	foreach( iCurrCount, ref permutation; permutations )
	{
		writeln( "\t{ ", permutation.strHash ~ ", &Invoker< " ~ permutation.strTemplateTypes ~ " >::call }," );
	}

	writeln( "};\n" );

	writeln( BottomContents );
}
//----------------------------------------------------------------------------

version( BinderooUtilSuite ) mixin BindModule!1;