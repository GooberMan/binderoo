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

#include <tuple>

namespace binderoo
{
namespace functiontraits
{
	template< typename _args, typename _rawArgs, bool bIsConst >
	struct Common
	{
		typedef _args										arg_tuple;
		typedef _rawArgs									raw_arg_tuple;

		static const size_t									num_args = std::tuple_size< _args >::value;
		static const size_t									num_raw_args = std::tuple_size< _rawArgs >::value;

		static const bool									is_member_method = ( num_args != num_raw_args );
		static const bool									is_const_method = is_member_method && bIsConst;

		template < size_t index >
		struct arg
		{
			typedef typename std::tuple_element< index, arg_tuple >::type type;
		};

		template < size_t index >
		struct rawarg
		{
			typedef typename std::tuple_element< index, raw_arg_tuple >::type type;
		};

	};
	//------------------------------------------------------------------------

	template< typename _ty >
	struct Implementation
	{
		Implementation()
		{
			static_assert( sizeof( _ty ) == 0, "Non-function passed to function traits!" );
		}
	};
	//------------------------------------------------------------------------

	template< typename _retType, typename ..._args >
	struct Implementation< _retType( _args... ) > : public Common< std::tuple< _args... >, std::tuple< _args... >, false >
	{
		typedef _retType									return_type;
		typedef void										object_type;
		typedef return_type( * signature )( _args... );
		typedef signature									raw_signature;
	};
	//------------------------------------------------------------------------

	template< typename _retType, typename ..._args >
	struct Implementation< _retType( * )( _args... ) > : public Common< std::tuple< _args... >, std::tuple< _args... >, false >
	{
		typedef _retType									return_type;
		typedef void										object_type;
		typedef return_type( * signature )( _args... );
		typedef signature									raw_signature;
	};
	//------------------------------------------------------------------------

	template< typename _retType, typename _objType, typename ..._args >
	struct Implementation< _retType( _objType::* )( _args... ) > : public Common< std::tuple< _args... >, std::tuple< _objType* const, _args... >, false >
	{
		typedef _retType									return_type;
		typedef _objType									object_type;
		typedef return_type( object_type::* signature )( _args... );
		typedef return_type( * raw_signature )( object_type* const, _args... );
	};
	//------------------------------------------------------------------------

	template< typename _retType, typename _objType, typename ..._args >
	struct Implementation< _retType( _objType::* )( _args... ) const > : public Common< std::tuple< _args... >, std::tuple< _objType* const, _args... >, true >
	{
		typedef _retType									return_type;
		typedef _objType									object_type;
		typedef return_type( object_type::* signature )( _args... ) const;
		typedef return_type( * raw_signature )( object_type* const, _args... );
	};
	//------------------------------------------------------------------------
}
}
//----------------------------------------------------------------------------

//============================================================================