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

#pragma once

#if !defined( _BINDEROO_BOUNDFUNCTION_H_ )
#define _BINDEROO_BOUNDFUNCTION_H_

#include "binderoo/defs.h"
#include "binderoo/slice.h"
//----------------------------------------------------------------------------

namespace binderoo
{
	typedef void*			BoundFunctionDLangCall;

	struct BIND_ALIGN( 16 ) BoundFunction
	{
		enum class Resolution : unsigned short
		{
			Unresolved,
			WaitingForImport,
			Imported,
			Exported,

			Max,
			Min = Unresolved,
		};
		//--------------------------------------------------------------------

		enum class CallingConvention : unsigned short
		{
			Undefined,
			C,
			CPP,

			Max,
			Min = Undefined,
		};
		//--------------------------------------------------------------------

		enum class FunctionKind : unsigned short
		{
			Undefined			= 0,
			Static				= 0x0001,
			Method				= 0x0002,
			Virtual				= 0x0004,
			Abstract			= 0x0008,
			Constructor			= 0x0010,
			Destructor			= 0x0020,
			Property			= 0x0040,
			CodeGenerated		= 0x0080,

			ReturnsVoid			= 0x0100,
			ReturnsBasicType	= 0x0200,
			ReturnsStruct		= 0x0400,
			ReturnsUnion		= 0x0800,
			ReturnsClass		= 0x1000,
			ReturnsArray		= 0x2000,
			ReturnsPointer		= 0x4000,
			ReturnsReference	= 0x8000,

			VirtualDestructor	= Virtual | Destructor,
		};
		//--------------------------------------------------------------------

		enum class Flags : unsigned short
		{
			None				= 0,
			OwnerIsAbstract		= 0x1,
			Const				= 0x2,
		};
		//--------------------------------------------------------------------

		struct Hashes
		{
			uint64_t			uFunctionNameHash;
			uint64_t			uFunctionSignatureHash;

			BIND_INLINE bool	operator==( const Hashes& rhs ) const			{ return uFunctionNameHash == rhs.uFunctionNameHash && uFunctionSignatureHash == rhs.uFunctionSignatureHash; }
			BIND_INLINE bool	operator<( const Hashes& rhs ) const			{ return uFunctionNameHash < rhs.uFunctionNameHash || uFunctionSignatureHash < rhs.uFunctionSignatureHash; }
		};
		//--------------------------------------------------------------------

		DString					strFunctionName;
		DString					strFunctionSignature;
		DString					strOwningClass;
		DString					strRequiredInclude;
		Slice< DString >		strIncludeVersions;
		Slice< DString >		strExcludeVersions;
		Hashes					functionHashes;
		void*					pFunctionCDecl;
		void*					pFunctionCPPDecl;
		int						iMinimumVersion;
		int						iOrderInTable;
		Resolution				eResolution;
		CallingConvention		eCallingConvention;
		FunctionKind			eFunctionKind;
		Flags					eFlags;

	private:
		BoundFunctionDLangCall	CPrototype;
		BoundFunctionDLangCall	DPrototype;
		BoundFunctionDLangCall	CSharpPrototype;
		BoundFunctionDLangCall	CSharpMarshalledPrototype;
		BoundFunctionDLangCall	ParameterNames;
		BoundFunctionDLangCall	CParameterTypes;
		BoundFunctionDLangCall	DParameterTypes;
		BoundFunctionDLangCall	CSharpParameterTypes;
		BoundFunctionDLangCall	CSharpReturnType;
		BoundFunctionDLangCall	CSharpMarshalledParameterTypes;
		BoundFunctionDLangCall	CSharpMarshalledReturnType;
		BoundFunctionDLangCall	CSharpParameterNamesWithQualifiers;
	};
	//------------------------------------------------------------------------
}
//----------------------------------------------------------------------------

#endif // !defined( _BINDEROO_BOUNDFUNCTION_H_ )

//============================================================================
