DKCHECK="dkcheck -nl -e"

CURDIR=`dirname $0`
BUILD=$CURDIR/_build

rm -rf $BUILD
mkdir $BUILD

if [ "$1" = "original" ]; then
	cat $CURDIR/interfaces/original.dk $CURDIR/private/original.dk $CURDIR/theories/original.dk > $BUILD/Coq.dk
elif [ "$1" = "original_cast" ]; then
	cat $CURDIR/interfaces/original_cast.dk $CURDIR/private/original_cast.dk $CURDIR/theories/original.dk > $BUILD/Coq.dk
elif [ "$1" = "predicates" ]; then
	cat $CURDIR/interfaces/predicates.dk $CURDIR/private/predicates.dk $CURDIR/theories/predicates.dk > $BUILD/Coq.dk
elif [ "$1" = "functionnal" ]; then
	cat $CURDIR/interfaces/functionnal.dk $CURDIR/private/functionnal.dk $CURDIR/theories/functionnal.dk > $BUILD/Coq.dk
elif [ "$1" = "constructors" ]; then
	cat $CURDIR/interfaces/constructors.dk $CURDIR/private/functionnal.dk $CURDIR/theories/functionnal.dk > $BUILD/Coq.dk
elif [ "$1" = "full_constructors" ]; then
	cat $CURDIR/interfaces/full_constructors.dk $CURDIR/private/full_constructors.dk $CURDIR/theories/full_constructors.dk > $BUILD/Coq.dk
elif [ "$1" = "fullcodes" ]; then
	cat $CURDIR/interfaces/predicates.dk $CURDIR/private/fullcodes.dk $CURDIR/theories/predicates.dk > $BUILD/Coq.dk
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

