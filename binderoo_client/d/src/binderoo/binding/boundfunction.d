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

module binderoo.binding.boundfunction;
//----------------------------------------------------------------------------

public import binderoo.slice;
public import binderoo.typedescriptor;
//----------------------------------------------------------------------------

public import binderoo.functiondescriptor;

alias StringGen = string function();
alias StringArrayGen = string[] function();

struct BoundFunctionFunctions( Descriptor )
{
	static string CPrototype()
	{
		return FunctionString!( Descriptor ).CDecl;
	}

	static string DPrototype()
	{
		return FunctionString!( Descriptor ).DDecl;
	}

	static string CSharpPrototype()
	{
		return FunctionString!( Descriptor ).CSharpDecl;
	}

	static string CSharpMarshalledPrototype()
	{
		return FunctionString!( Descriptor ).CSharpMarshalledDecl;
	}

	static string[] ParameterNames()
	{
		string[] generate()
		{
			string[] output;
			static foreach( param; Descriptor.ParametersAsTuple )
			{
				output ~= param.Name;
			}
			return output;
		}

		enum Names = generate();

		return Names;
	}

	static string[] CParameterTypes()
	{
		string[] generate()
		{
			string[] output;
			static foreach( param; Descriptor.ParametersAsTuple )
			{
				output ~= TypeString!( param.Type ).CDecl;
			}
			return output;
		}

		enum Types = generate();

		return Types;
	}

	static string[] DParameterTypes()
	{
		string[] generate()
		{
			string[] output;
			static foreach( param; Descriptor.ParametersAsTuple )
			{
				output ~= TypeString!( param.Type ).DDecl;
			}
			return output;
		}

		enum Types = generate();

		return Types;
	}

	static string[] CSharpParameterTypes()
	{
		string[] generate()
		{
			string[] output;
			static foreach( param; Descriptor.ParametersAsTuple )
			{
				output ~= TypeString!( param.Type ).CSharpDecl;
			}
			return output;
		}

		enum Types = generate();

		return Types;
	}

	static string[] CSharpMarshalledParameterTypes()
	{
		string[] generate()
		{
			string[] output;

			static foreach( param; Descriptor.ParametersAsTuple )
			{
				output ~= TypeString!( param.Type ).CSharpMarshalledDecl;
			}
			return output;
		}

		enum Types = generate();

		return Types;
	}

	static string[] CSharpParameterNamesWithQualifiers()
	{
		string[] generate()
		{
			string[] output;
			static foreach( param; Descriptor.ParametersAsTuple )
			{
				static if( param.IsArray )
				{
					output ~= "new " ~ CSharpTypeString!( param.Type, MarshallingStage.Intermediary ) ~ "( " ~ param.Name ~ " ).SliceData";
				}
				else
				{
					output ~= ( param.IsRef || ( param.IsPointer && param.Descriptor.IsUserType ) ? "ref " : "" ) ~ param.Name;
				}
			}
			return output;
		}

		enum Names = generate();

		return Names;
	}
}

@CTypeName( "binderoo::BoundFunction", "binderoo/boundfunction.h" )
align( 16 )
struct BoundFunction
{
	@CTypeName( "binderoo::BoundFunction::Resolution", "binderoo/boundfunction.h" )
	enum Resolution : char
	{
		Unresolved,
		WaitingForImport,
		Imported,
		Exported,
	}

	@CTypeName( "binderoo::BoundFunction::CallingConvention", "binderoo/boundfunction.h" )
	enum CallingConvention : char
	{
		Undefined,
		C,
		CPP,
		D
	}

	@CTypeName( "binderoo::BoundFunction::FunctionKind", "binderoo/boundfunction.h" )
	enum FunctionKind : char
	{
		Undefined			= 0,
		Static				= 0x1,
		Method				= 0x2,
		Virtual				= 0x4,
		Abstract			= 0x8,
		Constructor			= 0x10,
		Destructor			= 0x20,
		Property			= 0x40,
		CodeGenerated		= 0x80,
		VirtualDestructor	= Virtual | Destructor,
	}

	@CTypeName( "binderoo::BoundFunction::Flags", "binderoo/boundfunction.h" )
	enum Flags : char
	{
		None			= 0,
		OwnerIsAbstract	= 0x1,
		Const			= 0x2,
	}

	@CTypeName( "binderoo::BoundFunction::Hashes", "binderoo/boundfunction.h" )
	struct Hashes
	{
		ulong				uFunctionNameHash;
		ulong				uFunctionSignatureHash;
	}

	DString					strFunctionName;
	DString					strFunctionSignature;
	DString					strOwningClass;
	DString					strRequiredInclude;
	Slice!( DString )		strIncludeVersions;
	Slice!( DString )		strExcludeVersions;
	Hashes					functionHashes;
	void*					pFunctionCDecl;
	void*					pFunctionCPPDecl;
	int						iMinimumVersion;
	int						iOrderInTable;
	Resolution				eResolution;
	CallingConvention		eCallingConvention;
	FunctionKind			eFunctionKind;
	Flags					eFlags;

	StringGen				CPrototype;
	StringGen				DPrototype;
	StringGen				CSharpPrototype;
	StringGen				CSharpMarshalledPrototype;
	StringArrayGen			ParameterNames;
	StringArrayGen			CParameterTypes;
	StringArrayGen			DParameterTypes;
	StringArrayGen			CSharpParameterTypes;
	StringArrayGen			CSharpMarshalledParameterTypes;
	StringArrayGen			CSharpParameterNamesWithQualifiers;
}
//----------------------------------------------------------------------------

//============================================================================
