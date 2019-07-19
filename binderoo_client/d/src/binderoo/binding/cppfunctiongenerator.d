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

module binderoo.binding.cppfunctiongenerator;
//----------------------------------------------------------------------------

public import binderoo.binding.attributes;
public import binderoo.functiondescriptor;
public import binderoo.variabledescriptor;
public import binderoo.slice;

// CPPFunctionGenerator!FunctionDescriptor has the following members:
// * FuncCDecl - a C wrappper to the specified function
// * FuncCPPDecl - a C++ wrappper to the specified function
//
// CPPFunctionGenerator!VariableDescriptor has the following members:
// * GetterCDecl - a C getter, first parameter is aggregate object if applicable
// * SetterCDecl - a C setter, first parameter is aggregate object if applicable
// * GetterCPPDecl - a C++ getter (thiscall compliant for aggregate members)
// * SetterCPPDecl - a C++ setter (thiscall compliant for aggregate members)
//
// These functions are directly callable using the required parameters, and you
// can obtain the address of the function just as easily

struct CPPFunctionGenerator( alias Desc ) if( IsTemplatedType!Desc )
{
	import std.string : replace, toLower;
	import std.conv : to;
	import std.format : format;

	static if( IsBaseTemplate!( FunctionDescriptor, Desc ) )
	{
		static string collectModules()
		{
			import std.algorithm : canFind;

			string[] output = [ "import " ~ Desc.ModuleName ~ ";" ];

			static if( Desc.IsMemberFunction )
			{
				output ~= gatherImports!( Desc.ObjectType );
			}
			static if( Desc.HasReturnType )
			{
				output ~= gatherImports!( Desc.UnqualifiedReturnType );
			}

			static foreach( Param; Desc.ParametersAsTuple )
			{
				output ~= gatherImports!( Param.UnqualifiedType );
			}

			return output.joinWith!( x => "static " ~ x )( "\n" ) ~ "\nstatic import std.traits;";
		}

		enum FnName = Desc.FunctionName;

		static string generate()
		{
			enum Modules = collectModules();
			mixin( Modules );
			// WHY DO I HAVE TO KEEP DOING THIS...
			enum MangleBase = "pragma( mangle, \"%s_wrapper_" ~ Desc.FullyQualifiedName.replace( ".", "_" ).replace( "!", "_" ).replace( "(", "_" ).replace( ",", "_" ).replace( ")", "_" ) ~ Desc.OverloadIndex.to!string ~ "\" )\nexport extern( %s ) static ";

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
			string[] strWrapperDeclarators;

			static if( Desc.IsMemberFunction )
			{
				static if( is( Desc.ObjectType == class ) )
				{
					strParameters ~= TypeString!( Desc.ObjectType ).FullyQualifiedDDecl ~ " pThis";
				}
				else
				{
					strParameters ~= TypeString!( Desc.ObjectType ).FullyQualifiedDDecl ~ "* pThis";
				}
				strParameterNames ~= "pThis";
			}
			static foreach( iIndex, Param; Desc.ParametersAsTuple )
			{
				static if( Param.Name.length == 0 )
				{
					pragma( msg, "Zero length parameter encountered of type " ~ TypeString!( Param.Descriptor ).FullyQualifiedDDecl );
				}
				static if( is( Param.Type == delegate ) || is( Param.Type == function ) )
				{
					pragma( msg, "Delegate encountered: " ~ TypeString!( Param.Descriptor ).FullyQualifiedDDecl ~ " ===> " ~ TypeString!( TypeDescriptor!( SetFunctionAttributes!( Param.Type, "D", functionAttributes!( Param.Type ) ) ) ).FullyQualifiedDDecl );

					strWrapperDeclarators ~= "auto _wrapdel_" ~ iIndex.to!string ~ " = cast( SetFunctionAttributes!( " ~ TypeString!( Param.Descriptor ).FullyQualifiedDDecl ~ ", \"D\", functionAttributes!( " ~ ( Param.Name.length ? Param.Name : "_unnamed_param_" ~ iIndex.to!string ) ~ " ) ) ) " ~ ( Param.Name.length ? Param.Name : "_unnamed_param_" ~ iIndex.to!string ) ~ ";";
					strParameters ~= TypeString!( Param.Descriptor ).FullyQualifiedDDeclNoRef ~ " " ~ ( Param.Name.length ? Param.Name : "_unnamed_param_" ~ iIndex.to!string );
					strParameterNames ~= ( Param.Name.length ? Param.Name : "_wrapdel_" ~ iIndex.to!string );
				}
				else static if( Param.IsArray )
				{
					static if( IsStaticArray!( Param.Type ) )
					{
						//pragma( msg, "Static array encountered:" ~ TypeString!( Param.Descriptor ).FullyQualifiedDDecl );

						strWrapperDeclarators ~= "import std.algorithm : copy; " ~ TypeString!( TypeDescriptor!( Unqualified!( Param.Type ) ) ).FullyQualifiedDDeclNoRef ~ " _arr_p" ~ iIndex.to!string ~ "; " ~ ( Param.Name.length ? Param.Name : "_unnamed_param_" ~ iIndex.to!string ) ~ ".toSlice.copy( _arr_p" ~ iIndex.to!string ~ "[ 0 .. " ~ StaticArrayLength!( Param.Type ).to!string ~ " ] );";
						strParameters ~= FullTypeName!( SliceOf!( Param.Type ) ) ~ " " ~ ( Param.Name.length ? Param.Name : "_unnamed_param_" ~ iIndex.to!string );
						strParameterNames ~= "_arr_p" ~ iIndex.to!string;
					}
					else
					{
						//pragma( msg, "Dynamic array encountered:" ~ TypeString!( Param.Descriptor ).FullyQualifiedDDecl );

						strWrapperDeclarators ~= TypeString!( Param.Descriptor ).FullyQualifiedDDeclNoRef ~ "* _arr_p" ~ iIndex.to!string ~ " = cast( " ~ TypeString!( Param.Descriptor ).FullyQualifiedDDeclNoRef ~ "*)&" ~ ( Param.Name.length ? Param.Name : "_unnamed_param_" ~ iIndex.to!string ) ~ ";";
						strParameters ~= FullTypeName!( SliceOf!( Param.Type ) ) ~ " " ~ ( Param.Name.length ? Param.Name : "_unnamed_param_" ~ iIndex.to!string );
						strParameterNames ~= "*_arr_p" ~ iIndex.to!string;
					}
				}
				else
				{
					strParameters ~= TypeString!( Param.Descriptor ).FullyQualifiedDDeclNoRef ~ " " ~ ( Param.Name.length ? Param.Name : "_unnamed_param_" ~ iIndex.to!string );
					strParameterNames ~= ( Param.Name.length ? Param.Name : "_unnamed_param_" ~ iIndex.to!string );
				}
			}

			strCOutput = ExportDeclC;
			strCPPOutput = ExportDeclCPP;
			static if( Desc.ReturnsRef )
			{
				strCPPOutput ~= "ref ";
			}
			static if( IsNonAssociativeArray!( Desc.ReturnType ) )
			{
				appendToBoth( FullTypeName!( SliceOf!( Desc.ReturnType ) ) );
			}
			else
			{
				appendToBoth( TypeString!( Desc.ReturnType ).FullyQualifiedDDecl );
			}
			static if( Desc.ReturnsRef )
			{
				strCOutput ~= "*";
			}

			appendToBoth( " " ~ FnName );
			strCOutput ~= "CDecl";
			strCPPOutput ~= "CPPDecl";
			appendToBoth( "( " ~ strParameters.joinWith( ", " ) ~ " ) { " ~ strWrapperDeclarators.joinWith( " " ) ~ " " );
			static if( Desc.HasReturnType )
			{
				static if( !Desc.IsConstructor )
				{
					static if( Desc.ReturnsRef )
					{
						appendToBoth( FullTypeName!( Desc.ReturnType ) ~ "* retval = &" );
					}
					else
					{
						appendToBoth( FullTypeName!( Desc.ReturnType ) ~ " retval = " );
					}
				}
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
				
			appendToBoth( "( " ~ strParameterNames[ ParamNameBaseIndex .. $ ].joinWith( ", " ) ~ " )" );
			
			static if( Desc.HasReturnType )
			{
				appendToBoth( "; return " );
				static if( Desc.ReturnsRef )
				{
					strCPPOutput ~= "*";
				}

				static if( Desc.IsConstructor )
				{
					appendToBoth( "pThis" );
				}
				else
				{
					static if( IsNonAssociativeArray!( Desc.ReturnType ) )
					{
						appendToBoth( "*( cast( " ~ FullTypeName!( SliceOf!( Desc.ReturnType ) ) ~ "* )&" );
					}
					appendToBoth( "retval" );
					static if( IsNonAssociativeArray!( Desc.ReturnType ) )
					{
						appendToBoth( " )" );
					}
				}
			}

			appendToBoth( "; }" );

			return strCOutput ~ "\n" ~ strCPPOutput ~ "\nalias FuncCDecl = " ~ FnName ~ "CDecl;\nalias FuncCPPDecl = " ~ FnName ~ "CPPDecl;";
		}

		enum Defn = collectModules() ~ "\n\n" ~ generate() ~ "\n//----------------------------------------------------------------------------\n";
		//pragma( msg, Defn );
		mixin( Defn );
	}
	else static if( IsBaseTemplate!( VariableDescriptor, Desc ) )
	{
		static string collectModules()
		{
			string[] strOutput;
			strOutput ~= gatherImports!( Desc.ElementType.Type );
			strOutput ~= gatherImports!( Desc.BaseType.Type );
			return strOutput.joinWith!( x => "static " ~ x )( "\n" );
		}

		static string generate( string AccessorType )()
		{
			mixin( collectModules() );
			// WHY DO I HAVE TO KEEP DOING THIS...
			enum MangleBase = "pragma( mangle, \"%s_wrapper_" ~ Desc.FullyQualifiedName.replace( ".", "_" ).replace( "!", "_" ).replace( "(", "_" ).replace( ")", "_" ) ~ "_%s\" )\nexport extern( %s ) static ";
			enum ExportDeclC = MangleBase.format( "c", AccessorType, "C" );
			enum ExportDeclCPP = MangleBase.format( "cpp", AccessorType, "C++" );

			string strCOutput;
			string strCPPOutput;

			void appendToBoth( string val )
			{
				strCOutput ~= val;
				strCPPOutput ~= val;
			}

			string[] strParameters;
			string[] strParameterNames;
			static if( Desc.BaseType.IsClass )
			{
				strParameters ~= Desc.BaseType.FullyQualifiedName ~ " pThisObj";
			}
			else
			{
				strParameters ~= Desc.BaseType.FullyQualifiedName ~ "* pThisObj";
			}
			strParameterNames ~= "pThisObj";

			// TODO: This needs slice support
			static if( AccessorType == "Setter" )
			{
				static if( Desc.ElementType.IsStruct )
				{
					strParameters ~= Desc.ElementType.FullyQualifiedName ~ "* val";
				}
				else
				{
					strParameters ~= Desc.ElementType.FullyQualifiedName ~ " val";
				}
				strParameterNames ~= "val";
				strCOutput = ExportDeclC;
				strCPPOutput = ExportDeclCPP;
				appendToBoth( "void" );
			}
			else static if( AccessorType == "Getter" )
			{
				strCOutput = ExportDeclC;
				strCPPOutput = ExportDeclCPP;
				static if( Desc.ElementType.IsStruct ) strCPPOutput ~= "ref ";
				appendToBoth( fullyQualifiedName!( Desc.ElementType.Type ) );
				static if( Desc.ElementType.IsStruct ) strCOutput ~= "*";
			}

			appendToBoth( " " ~ Desc.Name ~ "_" ~ AccessorType );
			strCOutput ~= "CDecl";
			strCPPOutput ~= "CPPDecl";
			appendToBoth( "( " ~ strParameters.joinWith( ", " ) ~ " ) { " );

			static if( AccessorType == "Setter" )
			{
				enum Dereference = Desc.ElementType.IsStruct ? "*" : "";
				appendToBoth( "pThisObj.tupleof[ " ~ Desc.TupleIndex.to!string ~ " ] = " ~ Dereference ~ "val;" );
			}
			else static if( AccessorType == "Getter" )
			{
				enum AddressOf = Desc.ElementType.IsStruct ? "&" : "";
				strCOutput ~= "return " ~ AddressOf ~ "pThisObj.tupleof[ " ~ Desc.TupleIndex.to!string ~ " ];";
				strCPPOutput ~= "return pThisObj.tupleof[ " ~ Desc.TupleIndex.to!string ~ " ];";
			}
				
			appendToBoth( " }" );

			return strCOutput ~ "\n" ~ strCPPOutput;
		}
		
		enum Defn = collectModules() ~ "\n\n"
					~ generate!"Getter" ~ "\n" ~ generate!"Setter"
					~ "\nalias GetterCDecl = " ~ Desc.Name ~ "_GetterCDecl;"
					~ "\nalias SetterCDecl = " ~ Desc.Name ~ "_SetterCDecl;"
					~ "\nalias GetterCPPDecl = " ~ Desc.Name ~ "_GetterCPPDecl;"
					~ "\nalias SetterCPPDecl = " ~ Desc.Name ~ "_SetterCPPDecl;"
					~ "\n//----------------------------------------------------------------------------\n";
		//pragma( msg, Defn );
		mixin( Defn );
	}

}
