DKCHECK="dkcheck -nl -e"

CURDIR=`dirname $0`
BUILD=$CURDIR/_build

rm -rf $BUILD
mkdir $BUILD

if [ "$1" = "original" ]; then
	cat $CURDIR/interfaces/original.dk \
		$CURDIR/private/original.dk \
		$CURDIR/theories/original.dk > $BUILD/Coq.dk
elif [ "$1" = "original_cast" ]; then
	cat $CURDIR/interfaces/original_cast.dk \
		$CURDIR/private/original_cast.dk \
		$CURDIR/theories/original.dk > $BUILD/Coq.dk
elif [ "$1" = "predicates" ]; then
	cat $CURDIR/interfaces/predicates.dk \
		$CURDIR/private/predicates.dk \
		$CURDIR/theories/predicates.dk > $BUILD/Coq.dk
elif [ "$1" = "functionnal" ]; then
	cat $CURDIR/interfaces/functionnal.dk \
		$CURDIR/private/functionnal.dk \
		$CURDIR/theories/functionnal.dk > $BUILD/Coq.dk
elif [ "$1" = "constructors" ]; then
	cat $CURDIR/interfaces/constructors.dk \
		$CURDIR/private/functionnal.dk \
		$CURDIR/theories/functionnal.dk > $BUILD/Coq.dk
elif [ "$1" = "full_constructors" ]; then
	cat $CURDIR/interfaces/full_constructors.dk \
		$CURDIR/private/full_constructors.dk \
		$CURDIR/theories/full_constructors.dk > $BUILD/Coq.dk
elif [ "$1" = "fullcodes" ]; then
	cat $CURDIR/interfaces/predicates.dk \
		$CURDIR/private/fullcodes.dk \
		$CURDIR/theories/predicates.dk > $BUILD/Coq.dk
elif [ "$1" = "deepcodes" ]; then
	cat $CURDIR/interfaces/predicates.dk \
		$CURDIR/private/deepcodes.dk \
		$CURDIR/theories/predicates.dk > $BUILD/Coq.dk
elif [ "$1" = "test1" ]; then
	cat $CURDIR/interfaces/predicates.dk \
		$CURDIR/private/deepcodes.dk \
		$CURDIR/theories/test.dk > $BUILD/Coq.dk
elif [ "$1" = "test2" ]; then
	cat $CURDIR/interfaces/predicates.dk \
		$CURDIR/private/deepcodes.dk \
		$CURDIR/theories/predicates.dk \
		$CURDIR/theories/matita.dk > $BUILD/Coq.dk
else
	echo "Unknown encoding: $1"
fi

shift
while [ ! -z $1 ]
do
	if [ "$1" = "test" ]; then
		cp $BUILD/Coq.dk $BUILD/Test.dk
		cat $CURDIR/tests/test.dk >> $BUILD/Test.dk
		$DKCHECK $BUILD/Test.dk
		rm $BUILD/Test.dk
	elif [ "$1" = "clean" ]; then
		rm -rf $BUILD
	elif [ "$1" = "short" ]; then
		rm -f $BUILD/C.dk
		cp $BUILD/Coq.dk $BUILD/C.dk
		sed -i 's/Sort/S/g' $BUILD/C.dk
		sed -i 's/Term/T/g' $BUILD/C.dk
		sed -i 's/Univ/U/g' $BUILD/C.dk
		sed -i 's/univ/u/g' $BUILD/C.dk
	elif [ "$1" = "check" ]; then
		$DKCHECK $BUILD/Coq.dk
		$DKCHECK $BUILD/C.dk
	else
		echo "Unknown command: $1"
	fi
	shift
done

