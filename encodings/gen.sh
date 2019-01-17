DKCHECK="dkcheck -nl -e"

BUILD=_build


rm -rf $BUILD
mkdir $BUILD


if [ "$1" = "original" ]; then
	cat interfaces/original.dk private/original.dk theories/original.dk > $BUILD/Coq.dk
elif [ "$1" = "original_cast" ]; then
	cat interfaces/original_cast.dk private/original_cast.dk theories/original.dk > $BUILD/Coq.dk
elif [ "$1" = "predicates" ]; then
	cat interfaces/predicates.dk private/predicates.dk theories/predicates.dk > $BUILD/Coq.dk
elif [ "$1" = "functionnal" ]; then
	cat interfaces/functionnal.dk private/functionnal.dk theories/functionnal.dk > $BUILD/Coq.dk
elif [ "$1" = "constructors" ]; then
	cat interfaces/constructors.dk private/constructors.dk theories/constructors.dk > $BUILD/Coq.dk
elif [ "$1" = "full_constructors" ]; then
	cat interfaces/full_constructors.dk private/full_constructors.dk theories/full_constructors.dk > $BUILD/Coq.dk
elif [ "$1" = "clean" ]; then
	rm -rf $BUILD
fi


if [ "$2" = "short" ]; then
	rm -f $BUILD/C.dk
	cp $BUILD/Coq.dk $BUILD/C.dk
	sed -i 's/Sort/S/g' $BUILD/C.dk
	sed -i 's/Term/T/g' $BUILD/C.dk
	sed -i 's/Univ/U/g' $BUILD/C.dk
	sed -i 's/univ/u/g' $BUILD/C.dk
fi


if [ "$2" = "check" ]; then
	$DKCHECK $BUILD/Coq.dk
elif [ "$3" = "check" ]; then
	$DKCHECK $BUILD/Coq.dk
	$DKCHECK $BUILD/C.dk
fi

