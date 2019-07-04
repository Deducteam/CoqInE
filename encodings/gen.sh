DKCHECK="dkcheck -e"

CURDIR=`dirname $0`
BUILD=$CURDIR/_build

rm -rf $BUILD
mkdir $BUILD

exportdk () {
	cat $CURDIR/interfaces/$1.dk $CURDIR/private/$2.dk $CURDIR/theories/$3.dk > $BUILD/Coq.dk
}

if [ "$1" = "original" ]; then
	exportdk original original original
elif [ "$1" = "original_cast" ]; then
	exportdk original_cast original_cast original
elif [ "$1" = "predicates" ]; then
	exportdk predicates predicates predicates
elif [ "$1" = "lift_predicates" ]; then
	exportdk lift-predicates lift-predicates lift-predicates
elif [ "$1" = "functionnal" ]; then
	exportdk functionnal functionnal functionnal
elif [ "$1" = "constructors" ]; then
	exportdk constructors functionnal functionnal
elif [ "$1" = "full_constructors" ]; then
	exportdk full_constructors functionnal functionnal
elif [ "$1" = "fullcodes" ]; then
	exportdk predicates fullcodes predicates
elif [ "$1" = "fullcodes_test1" ]; then
	exportdk predicates fullcodes test1
elif [ "$1" = "fullcodes_test2" ]; then
	exportdk predicates fullcodes predicates
	cat $CURDIR/theories/test2.dk >> $BUILD/Coq.dk
elif [ "$1" = "fullcodes_test3" ]; then
	exportdk predicates fullcodes predicates
	cat $CURDIR/theories/test3.dk >> $BUILD/Coq.dk
elif [ "$1" = "deepcodes" ]; then
	exportdk predicates deepcodes predicates
elif [ "$1" = "deepcodes_test1" ]; then
	exportdk predicates deepcodes test1
elif [ "$1" = "deepcodes_test2" ]; then
	exportdk predicates deepcodes predicates
	cat $CURDIR/theories/test2.dk >> $BUILD/Coq.dk
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
		$DKCHECK $BUILD/*.dk
	else
		echo "Unknown command: $1"
	fi
	shift
done
