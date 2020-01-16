DKPATH=run/upoly_logipedia/out
COQPATH=~/git/coq/v8.8/theories/
SUMMARY=summary.csv

rm -f $SUMMARY
echo "File,.v size,.vo size,.dk size,.dko size,.dk.tar.gz size" > $SUMMARY

for dkfile in $(find $DKPATH -maxdepth 1 -regex '.*Coq__[A-Z].*\.dk')
do
    vsize="?"
    vosize="?"
    dksize="?"
    dkosize="?"

    coqfile=$(echo $dkfile | sed  -E "s/(.+)Coq__//g; s/\.dk//g; s/__([A-Z])/\/\1/g")
    echo $coqfile

    if test -f "$dkfile"; then
        dksize=$(stat -c "%s" $dkfile)
    fi
    dkofile=$(echo $dkfile | sed  -E "s/\.dk/\.dko/g")
    if test -f "$dkofile"; then
        dkosize=$(stat -c "%s" $dkofile)
    fi
    vfile=$COQPATH/$coqfile.v
    if test -f "$vfile"; then
        vsize=$(stat -c "%s" $vfile)
    fi
    vofile=$COQPATH/$coqfile.vo
    if test -f "$vofile"; then
        vosize=$(stat -c "%s" $vofile)
    fi

    rm -f deleteme.tar.gz
    tar zcf deleteme.tar.gz $dkfile
    tarsize=$(stat -c "%s" deleteme.tar.gz)
    rm -f deleteme.tar.gz

    echo "$coqfile,$vsize,$vosize,$dksize,$dkosize,$tarsize" >> $SUMMARY
done
