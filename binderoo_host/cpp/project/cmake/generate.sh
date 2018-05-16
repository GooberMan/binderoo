
if [ "$1" == "" ] || [ "${1,,}" == "release" ]; then
	Build="Release"
elif [ "${1,,}" == "debug" ]; then
	Build="Debug"
else
	echo "Invalid option \"$1\", must be either \"debug\" or \"release\"."
	exit 1
fi

if [ "$2" == "" ]; then
	Target="all"
else
	Target="$2"
fi

echo "Building target $Target in $Build configuration..."
cmake -G 'Unix Makefiles' -DCMAKE_BUILD_TYPE=$Build -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++
make $Target