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

module binderoo.binding.mangler;
//----------------------------------------------------------------------------

public import binderoo.binding.attributes;
public import binderoo.functiondescriptor;

import binderoo.variabledescriptor;
import binderoo.typedescriptor;
import std.string : replace;
import std.conv : to;

template CMangler( alias Func ) if( IsTemplatedType!Func && IsBaseTemplate!( FunctionDescriptor, Func ) )
{
	enum OverloadString = Func.OverloadIndex > 0 ? Func.OverloadIndex.to!string : "";
	version( Windows )
	{
		version( X86_64 ) enum Leading = "";
		else enum Leading = "_";

		enum CMangler = Leading ~ Func.FullyQualifiedName.replace( ".", "_" ) ~ OverloadString;
	}
	else version( linux )
	{
		version( X86_64 ) enum Leading = "";
		else enum Leading = "_";

		enum CMangler = Leading ~ Func.FullyQualifiedName.replace( ".", "_" ) ~ OverloadString;
	}
	else
	{
		static assert( false, "CMangler does not support this platform! Need to implement if it's different to the above..." );
	}
}

template CPPMangler( alias Func ) if( IsTemplatedType!Func && IsBaseTemplate!( FunctionDescriptor, Func ) )
{
	private struct NameMangler
	{
		private string[] allSymbolNames;
		private string[] duplicatedSymbolNames;

		private string[] mangleAll( alias Symbol )() const
		{
			static if( !__traits( compiles, __traits( parent, Symbol ) ) )
			{
				return [ __traits( identifier, Symbol ) ~ "@" ];
			}
			else
			{
				return [ __traits( identifier, Symbol ) ~ "@" ] ~ mangleAll!( __traits( parent, Symbol ) );
			}
		}

		string[] UncollapsedMangle( alias Symbol )() const
		{
			return mangleAll!Symbol;
		}

		string CollapseMangle( string[] mangled )
		{
			import std.array : join;
			import std.algorithm : countUntil;
			import std.conv : to;

			foreach( ref thisMangle; mangled )
			{
				ptrdiff_t iIndex = allSymbolNames.countUntil( thisMangle );
				if( iIndex >= 0 )
				{
					iIndex = duplicatedSymbolNames.countUntil( thisMangle );
					if( iIndex < 0 )
					{
						if( duplicatedSymbolNames.length < 9 )
						{
							duplicatedSymbolNames ~= thisMangle;
							thisMangle = ( duplicatedSymbolNames.length ).to!string;
						}
					}
					else
					{
						thisMangle = ( iIndex + 1 ).to!string;
					}
				}
				else
				{
					allSymbolNames ~= thisMangle;
				}
			}

			return mangled.join ~ "@";
		}

		string Mangle( alias Symbol, bool bCollapse = true )( )
		{
			import std.array : join;
			static if( bCollapse ) return CollapseMangle( UncollapsedMangle!( Symbol ) );
			else return UncollapsedMangle!( Symbol ).join;
		}

		string TypeIdentifier( alias Type, bool bCollapse = true )() if( IsTemplatedType!Type && IsBaseTemplate!( TypeDescriptor, Type ) )
		{
			version( Windows )
			{
				static if( Type.IsRef )
				{
					return "AEA" ~ TypeIdentifier!( TypeDescriptor!( Type.Type, false ) );
				}
				else static if( Type.IsConst && Type.IsPointer )
				{
					return "PEB" ~ TypeIdentifier!( TypeDescriptor!( Unqualified!( binderoo.traits.PointerTarget!( Type.Type ) ), false ) );
				}
				else static if( Type.IsPointer )
				{
					return "PEA" ~ TypeIdentifier!( TypeDescriptor!( binderoo.traits.PointerTarget!( Type.Type ), false ) );
				}
				else static if( Type.IsConst || Type.IsImmutable )
				{
					return TypeIdentifier!( TypeDescriptor!( Unqualified!( Type.Type ), false ) );
				}
				else static if( is( Type.Type == void ) )
				{
					return "X";
				}
				else static if( is( Type.Type == bool ) )
				{
					return "_N";
				}
				else static if( is( Type.Type == char ) )
				{
					return "D";
				}
				else static if( is( Type.Type == byte ) )
				{
					return "C";
				}
				else static if( is( Type.Type == ubyte ) )
				{
					return "E";
				}
				else static if( is( Type.Type == short ) )
				{
					return "F";
				}
				else static if( is( Type.Type == ushort ) )
				{
					return "G";
				}
				else static if( is( Type.Type == int ) )
				{
					return "H";
				}
				else static if( is( Type.Type == uint ) )
				{
					return "I";
				}
				else static if( is( Type.Type == long ) )
				{
					return "_J";
				}
				else static if( is( Type.Type == ulong ) )
				{
					return "_K";
				}
				else static if( is( Type.Type == wchar ) )
				{
					return "L";
				}
				else static if( is( Type.Type == float ) )
				{
					return "M";
				}
				else static if( is( Type.Type == double ) )
				{
					return "N";
				}
				else static if( is( Type == string ) )
				{
					return Mangle!( const(char)*, bCollapse );
				}
				else static if( Type.IsUserType )
				{
					static if( Type.IsUnion )
					{
						enum TypeID = "T";
					}
					else static if( Type.IsStruct )
					{
						enum TypeID = "U";
					}
					else static if( Type.IsClass || Type.IsInterface )
					{
						enum TypeID = "V";
					}
					else static if( Type.IsEnum )
					{
						enum TypeID = "W4";
					}

					return TypeID ~ Mangle!( Type.Type, bCollapse );
				}
			}
		}

		string CallingConvention( alias Func )() if( IsTemplatedType!Func && IsBaseTemplate!( FunctionDescriptor, Func ) )
		{
			version( Windows )
			{
				version( X86_64 ) return  "A";
				else
				{
					enum Ret = Func.IsCFunction ? "A"
						: Func.IsPascalFunction ? "C"
						: Func.IsWindowsFunction ? "G"
						: Func.IsCPlusPlusFunction && Func.IsMemberFunction ? "E"
						: "I"; // __fastcall is the last remaining one here

					return Ret;
				}
			}
		}

		string Storage( alias Func )() if( IsTemplatedType!Func && IsBaseTemplate!( FunctionDescriptor, Func ) )
		{
			version( Windows )
			{
				enum IsConst = IsConst!( Func.ReturnType ) || IsImmutable!( Func.ReturnType );
				enum IsVolatile = false;
				enum AlwaysInclude = IsUserType!( Func.ReturnType );

				return ( IsConst && IsVolatile ) ? "D"
					: IsVolatile ? "C"
					: IsConst ? "B"
					: AlwaysInclude ? "A"
					: "";
			}
		}

		string Storage( alias Type )() if( IsTemplatedType!Type && IsBaseTemplate!( TypeDescriptor, Type ) )
		{
			version( Windows )
			{
				enum IsConst = Type.IsConst || Type.IsImmutable;
				enum IsVolatile = false;
				enum AlwaysInclude = Type.IsUserType;

				return ( IsConst && IsVolatile ) ? "D"
					: IsVolatile ? "C"
					: IsConst ? "B"
					: AlwaysInclude ? "A"
					: "";
			}
		}

		string Modifier( alias Func )() if( IsTemplatedType!Func && IsBaseTemplate!( FunctionDescriptor, Func ) )
		{
			version( Windows )
			{
				static if( Func.IsMemberFunction )
				{
					string strOutput;
					static if( Func.IsPublic || Func.IsExport )
					{
						strOutput ~= Func.IsStatic ? "S"
									: Func.IsVirtual ? "U"
									: "Q";
					}
					else static if( Func.IsProtected )
					{
						strOutput ~= Func.IsStatic ? "K"
									: Func.IsVirtual ? "M"
									: "I";
					}
					else
					{
						strOutput ~= Func.IsStatic ? "C"
									: Func.IsVirtual ? "E"
									: "A";
					}

					static if( !Func.IsStatic )
					{
						enum PointerBase = "E";
						strOutput ~= PointerBase;

						static if( Func.IsConst )
						{
							strOutput ~= "B";
						}
						else
						{
							strOutput ~= "A";
						}
					}

					return strOutput;
				}
				else
				{
					return "Y"; // Near. Far is unsupported on 64 bit
				}
			}
		}
	}

	string generate()
	{
		version( Windows )
		{
			NameMangler mangler;

			string strOutput;
			strOutput ~= "?";

			strOutput ~= mangler.Mangle!( Func.Symbol );

			strOutput ~= mangler.Modifier!Func ~ mangler.CallingConvention!Func;
			strOutput ~= ( IsUserType!( Func.ReturnType ) ? "?" : "" ) ~ mangler.Storage!Func ~ mangler.TypeIdentifier!( TypeDescriptor!( Func.ReturnType, Func.ReturnsRef ) );

			static if( Func.ParameterCount > 0 )
			{
				import std.array : join;
				import std.algorithm : countUntil;
				import std.conv : to;

				string[] strUncollapsedParams;
				foreach( Parameter; Func.ParametersAsTuple )
				{
					string strUncollapsedParam = mangler.TypeIdentifier!( Parameter.Descriptor, false );
					string strThisParam = strUncollapsedParam;
					if( strThisParam.length > 1 )
					{
						ptrdiff_t iIndex = strUncollapsedParams.countUntil( strThisParam );
						if( iIndex >= 0 )
						{
							strThisParam = iIndex.to!string;
						}
						else
						{
							strThisParam = mangler.TypeIdentifier!( Parameter.Descriptor, true );
							strUncollapsedParams ~= strUncollapsedParam;
						}
					}
					strOutput ~= strThisParam;
				}

				strOutput ~= "@Z";
			}
			else
			{
				strOutput ~= "XZ";
			}

			return strOutput;
		}
	}

	enum CPPMangler = generate();
}