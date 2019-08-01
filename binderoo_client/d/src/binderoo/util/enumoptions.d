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

// EnumOptions is, at its core, a bitfield. This bitfield, however, creates an
// entry for each member of an enumeration. These entries are single bits
// representing an on or off state. Thus, your enum field becomes a rather
// user friendly struct and an enum bitfield at the same time.
//
// Primarily designed to make deep metaprogramming handy, its power really
// comes from passing any arbitrary parameters to a template and scraping for
// the values you care about.
//
// With the following example:
//
// enum SomeOptions
// {
//   SomethingCool,
//   SomethingNotSoCool,
//   SomethingRidiculous
// }
//
// And invoking it via:
//
// EnumOptions!SomeOptions options;
//
// It can be used as if you went to all the effort of writing that struct out
// manually:
//
// options.SomethingCool = !options.SomethingRidiculous;
// writeln( "Is there something cool? ", options.SomethingCool ? "YES!" : "No." );
//
// There are also manual setters and getters:
//
// options.Set( SomeOptions.SomethingRidiculous, true );
// options.Set( options.SomethingCool, !options.Get( Options.SomethingRidiculous ) );
//
// Creation of your options is fairly straightforward. With an array of values:
//
// auto options = OptionsFrom( [ Options.SomethingCool, Options.SomethingRidiculous ] );
//
// And with a tuple:
//
// enum options = OptionsOf!( SomeOptions, Options.SomethingCool, NotOptions.SomethingElse );
//
// Especially useful in templates:
//
// template Collector( Options... )
// {
//   enum options = OptionsOf!( SomeOptions, Options );
//   enum notoptions = OptionsOf!( NotOptions, Options );
//
//   static if( options.SomethingCool )
//     enum Collector = "Aw yiss! Something cool!";
//   else static if( notoptions.SomethingElse )
//     enum Collector = "Naw, it's something else.";
//   else
//     enum Collector = "Nothing I care about at all.";
// }
//
//----------------------------------------------------------------------------

module binderoo.util.enumoptions;
//----------------------------------------------------------------------------

import binderoo.traits : ModuleName;
import binderoo.util.bitpacking;
import std.traits : OriginalType;
//----------------------------------------------------------------------------

struct EnumOptions( alias Enum ) if( is( Enum == enum ) )
{
	mixin( "import " ~ ModuleName!Enum ~ ";" );

	alias AllOptions = Enum;

	this( AllOptions[] values )
	{
		foreach( val; values )
		{
			Set( val, true );
		}
	}

	mixin( generateEnumOptionsContent!AllOptions );
}
//----------------------------------------------------------------------------

auto OptionsFrom( EnumArray : Enum[], Enum )( EnumArray values )
{
	return EnumOptions!Enum( values );
}
//----------------------------------------------------------------------------

auto OptionsOf( EnumType, Symbols... )()
{
	return OptionsFrom( ExtractAllOf!( EnumType, Symbols ) );
}
//----------------------------------------------------------------------------

auto ExtractAllOf( Type, Symbols... )()
{
	Type[] output;

	import std.conv : to;
	static foreach( Symbol; Symbols )
	{
		static if( __traits( compiles, "Type val = Symbol" ) )
		{
			static if( is( typeof( Symbol ) == Type ) )
			{
				output ~= Symbol;
			}
		}
	}

	return output;
}
//----------------------------------------------------------------------------

package string generateEnumOptionsContent( alias Enum )()
{
	mixin( "import " ~ ModuleName!Enum ~ ";" );

	enum AllValues = __traits( allMembers, Enum );

	string[] bitpackEntries;
	string[] getEntries;
	string[] setEntries;

	getEntries ~= "bool Get( AllOptions val )";
	getEntries ~= "{";
	getEntries ~= "\tfinal switch( val ) with( AllOptions )";
	getEntries ~= "\t{";

	setEntries ~= "bool Set( AllOptions val, bool bSet )";
	setEntries ~= "{";
	setEntries ~= "\tfinal switch( val ) with( AllOptions )";
	setEntries ~= "\t{";

	static foreach( Value; AllValues )
	{
		bitpackEntries ~= "@PackSize( 1 ) bool " ~ Value ~ ";";
		getEntries ~= "\t\tcase " ~ Value ~ ": return this." ~ Value ~ ";";
		setEntries ~= "\t\tcase " ~ Value ~ ": this." ~ Value ~ " = bSet; return bSet;";
	}

	getEntries ~= "\t}";
	getEntries ~= "}";

	setEntries ~= "\t}";
	setEntries ~= "}";

	return "mixin BitPack!( \"" ~ bitpackEntries.joinWith( " " ) ~ "\");\n\n"
			~ getEntries.joinWith( "\n" ) ~ "\n\n"
			~ setEntries.joinWith( "\n" ) ~ "\n\n";
}
//----------------------------------------------------------------------------

//============================================================================
