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

module binderoo.binding.cppfunctiongenerator;
//----------------------------------------------------------------------------

public import binderoo.binding.attributes;
public import binderoo.functiondescriptor;
public import binderoo.variabledescriptor;

// CPPFunctionGenerator!FunctionDescriptor has the following members:
// * Func - a CPP wrappper to the specified function
//
// CPPFunctionGenerator!VariableDescriptor has the following members:
// * Getter - a CPP getter (thiscall compliant for aggregate members)
// * Setter - a CPP setter (thiscall compliant for aggregate members)
//
// These functions are directly callable using the required parameters, and you
// can obtain the address of the function just as easily

template CPPFunctionGenerator( alias Desc )
{
	static if( IsTemplatedType!Desc && is( Desc == FunctionDescriptor!( TemplateParametersOf!( Desc ) ) ) )
	{
		mixin( "import " ~ Desc.ModuleName ~ ";" );
		string generate()
		{
			import std.string : replace;
			import std.conv : to;
			import std.format : format;
			// WHY DO I HAVE TO KEEP DOING THIS...
			enum FnName = "Func"; //Desc.FunctionName;

			enum MangleBase = "pragma( mangle, \"%s_wrapper_" ~ Desc.FullyQualifiedName.replace( ".", "_" ) ~ Desc.OverloadIndex.to!string ~ "\" )\nextern( %s ) ";
			enum ExportDeclC = MangleBase.format( "c", "C" );
			enum ExportDeclCPP = MangleBase.format( "cpp", "C++" );

			string strCOutput;
			string strCPPOutput;

			void appendToBoth( string val )
			{
				strCOutput ~= val;
				strCPPOutput ~= val;
			}

			string[] strParameters;
			string[] strParameterNames;
			static if( Desc.IsMemberFunction )
			{
				static if( is( Desc.ObjectType == class ) )
				{
					strParameters ~= Desc.ObjectType.stringof ~ " pThis";
				}
				else
				{
					strParameters ~= Desc.ObjectType.stringof ~ "* pThis";
				}
				strParameterNames ~= "pThis";
			}
			static foreach( Param; Desc.ParametersAsTuple )
			{
				strParameters ~= TypeString!( Param.Descriptor ).FullyQualifiedDDecl ~ " " ~ Param.Name;
				strParameterNames ~= Param.Name;
			}

			strCOutput = ExportDeclC;
			strCPPOutput = ExportDeclCPP;
			static if( Desc.ReturnsRef )
			{
				strCPPOutput ~= "ref ";
			}
			appendToBoth( Desc.ReturnType.stringof );
			static if( Desc.ReturnsRef )
			{
				strCOutput ~= "*";
			}

			appendToBoth( " " ~ FnName );
			strCOutput ~= "CDecl";
			strCPPOutput ~= "CPPDecl";
			appendToBoth( "( " ~ strParameters.joinWith( ", " ) ~ " ) { " );
			static if( Desc.HasReturnType )
			{
				appendToBoth( "return " );
			}

			static if( Desc.ReturnsRef )
			{
				strCOutput ~= "&";
			}

			static if( Desc.IsMemberFunction )
			{
				enum ParamNameBaseIndex = 1;
				appendToBoth( "pThis." ~ Desc.FunctionName );
			}
			else
			{
				enum ParamNameBaseIndex = 0;
				appendToBoth( Desc.FullyQualifiedName );
			}
				
			appendToBoth( "( " ~ strParameterNames[ ParamNameBaseIndex .. $ ].joinWith( ", " ) ~ " ); }" );

			return strCOutput ~ "\n" ~ strCPPOutput;
		}

		enum Defn		= generate();
	}
	else static if( IsTemplatedType!Desc && is( Desc == VariableDescriptor!( TemplateParametersOf!( Desc ) ) ) )
	{
		pragma( msg, Desc.stringof );
		enum Defn = "alias Func = void function();";
	}

	pragma( msg, Defn );
	mixin( Defn );
}
