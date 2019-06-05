#!/bin/bash

CURDIR=`dirname $0`

ELEMENTS_FILES=(
Axioms/euclidean_axioms.v
Elements/OriginalProofs/euclidean_defs.v
Elements/OriginalProofs/general_tactics.v
Elements/OriginalProofs/euclidean_tactics.v
Elements/OriginalProofs/lemma_congruencesymmetric.v
Elements/OriginalProofs/lemma_congruencetransitive.v
Elements/OriginalProofs/lemma_congruenceflip.v
Elements/OriginalProofs/lemma_equalitysymmetric.v
Elements/OriginalProofs/lemma_inequalitysymmetric.v
Elements/OriginalProofs/lemma_3_6a.v
Elements/OriginalProofs/lemma_betweennotequal.v
Elements/OriginalProofs/lemma_localextension.v
Elements/OriginalProofs/lemma_extensionunique.v
Elements/OriginalProofs/lemma_3_7a.v
Elements/OriginalProofs/lemma_3_7b.v
Elements/OriginalProofs/lemma_partnotequalwhole.v
Elements/OriginalProofs/proposition_01.v
Elements/OriginalProofs/lemma_NCdistinct.v
Elements/OriginalProofs/lemma_doublereverse.v
Elements/OriginalProofs/lemma_differenceofparts.v
Elements/OriginalProofs/proposition_02.v
Elements/OriginalProofs/lemma_extension.v
Elements/OriginalProofs/lemma_betweennesspreserved.v
Elements/OriginalProofs/lemma_lessthancongruence.v
Elements/OriginalProofs/proposition_03.v
Elements/OriginalProofs/lemma_ray2.v
Elements/OriginalProofs/lemma_collinear2.v
Elements/OriginalProofs/lemma_collinear1.v
Elements/OriginalProofs/lemma_collinearorder.v
Elements/OriginalProofs/lemma_3_5b.v
Elements/OriginalProofs/lemma_3_6b.v
Elements/OriginalProofs/lemma_outerconnectivity.v
Elements/OriginalProofs/lemma_collinear4.v
Elements/OriginalProofs/lemma_rayimpliescollinear.v
Elements/OriginalProofs/lemma_collinearitypreserved.v
Elements/OriginalProofs/lemma_raystrict.v
Elements/OriginalProofs/lemma_equalanglesNC.v
Elements/OriginalProofs/lemma_ray.v
Elements/OriginalProofs/lemma_ray1.v
Elements/OriginalProofs/lemma_interior5.v
Elements/OriginalProofs/lemma_layoffunique.v
Elements/OriginalProofs/lemma_ray4.v
Elements/OriginalProofs/lemma_ray5.v
Elements/OriginalProofs/lemma_ray3.v
Elements/OriginalProofs/proposition_04.v
Elements/OriginalProofs/lemma_ABCequalsCBA.v
Elements/OriginalProofs/proposition_05.v
Elements/OriginalProofs/lemma_supplements.v
Elements/OriginalProofs/proposition_05b.v
Elements/OriginalProofs/lemma_equalanglessymmetric.v
Elements/OriginalProofs/lemma_angledistinct.v
Elements/OriginalProofs/lemma_layoff.v
Elements/OriginalProofs/lemma_equalangleshelper.v
Elements/OriginalProofs/lemma_equalanglestransitive.v
Elements/OriginalProofs/lemma_angleorderrespectscongruence2.v
Elements/OriginalProofs/lemma_equalanglesreflexive.v
Elements/OriginalProofs/lemma_NCorder.v
Elements/OriginalProofs/lemma_angleorderrespectscongruence.v
Elements/OriginalProofs/lemma_crossbar.v
Elements/OriginalProofs/lemma_angleordertransitive.v
Elements/OriginalProofs/lemma_9_5b.v
Elements/OriginalProofs/lemma_9_5a.v
Elements/OriginalProofs/lemma_sameside2.v
Elements/OriginalProofs/lemma_twolines.v
Elements/OriginalProofs/proposition_10.v
Elements/OriginalProofs/proposition_12.v
Elements/OriginalProofs/lemma_samesidesymmetric.v
Elements/OriginalProofs/lemma_twolines2.v
Elements/OriginalProofs/lemma_collinear5.v
Elements/OriginalProofs/lemma_planeseparation.v
Elements/OriginalProofs/lemma_lessthantransitive.v
Elements/OriginalProofs/lemma_midpointunique.v
Elements/OriginalProofs/lemma_rightangleNC.v
Elements/OriginalProofs/lemma_8_2.v
Elements/OriginalProofs/lemma_8_3.v
Elements/OriginalProofs/lemma_collinearright.v
Elements/OriginalProofs/lemma_rightreverse.v
Elements/OriginalProofs/lemma_altitudebisectsbase.v
Elements/OriginalProofs/lemma_droppedperpendicularunique.v
Elements/OriginalProofs/lemma_fiveline.v
Elements/OriginalProofs/proposition_07.v
Elements/OriginalProofs/lemma_angletrichotomy.v
Elements/OriginalProofs/proposition_06a.v
Elements/OriginalProofs/lemma_trichotomy1.v
Elements/OriginalProofs/proposition_06.v
Elements/OriginalProofs/proposition_08.v
Elements/OriginalProofs/proposition_09.v
Elements/OriginalProofs/lemma_samesidereflexive.v
Elements/OriginalProofs/lemma_8_7.v
Elements/OriginalProofs/lemma_notperp.v
Elements/OriginalProofs/lemma_lessthancongruence2.v
Elements/OriginalProofs/proposition_15.v
Elements/OriginalProofs/lemma_pointreflectionisometry.v
Elements/OriginalProofs/lemma_oppositesidesymmetric.v
Elements/OriginalProofs/proposition_11B.v
Elements/OriginalProofs/proposition_11.v
Elements/OriginalProofs/lemma_NChelper.v
Elements/OriginalProofs/proposition_13.v
Elements/OriginalProofs/proposition_14.v
Elements/OriginalProofs/proposition_16.v
Elements/OriginalProofs/proposition_17.v
Elements/OriginalProofs/proposition_18.v
Elements/OriginalProofs/proposition_19.v
Elements/OriginalProofs/proposition_20.v
Elements/OriginalProofs/lemma_lessthanbetween.v
Elements/OriginalProofs/lemma_trichotomy2.v
Elements/OriginalProofs/lemma_lessthanadditive.v
Elements/OriginalProofs/lemma_21helper.v
Elements/OriginalProofs/lemma_TGsymmetric.v
Elements/OriginalProofs/lemma_TTorder.v
Elements/OriginalProofs/lemma_TGflip.v
Elements/OriginalProofs/lemma_TTflip.v
Elements/OriginalProofs/lemma_TTtransitive.v
Elements/OriginalProofs/lemma_TTflip2.v
Elements/OriginalProofs/proposition_21.v
Elements/OriginalProofs/lemma_lessthannotequal.v
Elements/OriginalProofs/lemma_together.v
Elements/OriginalProofs/lemma_subtractequals.v
Elements/OriginalProofs/lemma_ondiameter.v
Elements/OriginalProofs/proposition_22.v
Elements/OriginalProofs/proposition_23.v
Elements/OriginalProofs/lemma_linereflectionisometry.v
Elements/OriginalProofs/lemma_10_12.v
Elements/OriginalProofs/lemma_Euclid4.v
Elements/OriginalProofs/lemma_equalanglesflip.v
Elements/OriginalProofs/lemma_9_5.v
Elements/OriginalProofs/proposition_23B.v
Elements/OriginalProofs/proposition_23C.v
Elements/OriginalProofs/proposition_24.v
Elements/OriginalProofs/lemma_angletrichotomy2.v
Elements/OriginalProofs/proposition_25.v
Elements/OriginalProofs/proposition_26A.v
Elements/OriginalProofs/lemma_26helper.v
Elements/OriginalProofs/proposition_26B.v
Elements/OriginalProofs/lemma_collinearbetween.v
Elements/OriginalProofs/proposition_27.v
Elements/OriginalProofs/lemma_collinearparallel.v
Elements/OriginalProofs/lemma_parallelsymmetric.v
Elements/OriginalProofs/lemma_parallelflip.v
Elements/OriginalProofs/proposition_27B.v
Elements/OriginalProofs/proposition_28A.v
Elements/OriginalProofs/lemma_supplementsymmetric.v
Elements/OriginalProofs/proposition_28B.v
Elements/OriginalProofs/proposition_28C.v
Elements/OriginalProofs/proposition_31.v
Elements/OriginalProofs/lemma_crossbar2.v
Elements/OriginalProofs/lemma_supplementinequality.v
Elements/OriginalProofs/proposition_29.v
Elements/OriginalProofs/proposition_29B.v
Elements/OriginalProofs/lemma_parallelNC.v
Elements/OriginalProofs/proposition_29C.v
Elements/OriginalProofs/proposition_30A.v
Elements/OriginalProofs/proposition_30B.v
Elements/OriginalProofs/lemma_parallelcollinear1.v
Elements/OriginalProofs/lemma_tarskiparallelflip.v
Elements/OriginalProofs/lemma_parallelcollinear2.v
Elements/OriginalProofs/lemma_parallelcollinear.v
Elements/OriginalProofs/lemma_paralleldef2B.v
Elements/OriginalProofs/lemma_crisscross.v
Elements/OriginalProofs/lemma_30helper.v
Elements/OriginalProofs/lemma_crossimpliesopposite.v
Elements/OriginalProofs/proposition_30.v
Elements/OriginalProofs/proposition_31short.v
Elements/OriginalProofs/proposition_32.v
Elements/OriginalProofs/lemma_samenotopposite.v
Elements/OriginalProofs/proposition_33.v
Elements/OriginalProofs/proposition_33B.v
Elements/OriginalProofs/lemma_diagonalsmeet.v
Elements/OriginalProofs/proposition_34.v
Elements/OriginalProofs/lemma_35helper.v
Elements/OriginalProofs/lemma_TCreflexive.v
Elements/OriginalProofs/lemma_ETreflexive.v
Elements/OriginalProofs/lemma_PGrotate.v
Elements/OriginalProofs/lemma_PGsymmetric.v
Elements/OriginalProofs/lemma_PGflip.v
Elements/OriginalProofs/lemma_diagonalsbisect.v
Elements/OriginalProofs/lemma_trapezoiddiagonals.v
Elements/OriginalProofs/lemma_EFreflexive.v
Elements/OriginalProofs/proposition_35A.v
Elements/OriginalProofs/lemma_parallelPasch.v
Elements/OriginalProofs/proposition_35.v
Elements/OriginalProofs/lemma_collinearparallel2.v
Elements/OriginalProofs/proposition_36.v
Elements/OriginalProofs/proposition_36A.v
Elements/OriginalProofs/lemma_samesidetransitive.v
Elements/OriginalProofs/lemma_Playfairhelper.v
Elements/OriginalProofs/lemma_Playfairhelper2.v
Elements/OriginalProofs/lemma_Playfair.v
Elements/OriginalProofs/lemma_triangletoparallelogram.v
Elements/OriginalProofs/proposition_37.v
Elements/OriginalProofs/proposition_38.v
Elements/OriginalProofs/proposition_39A.v
Elements/OriginalProofs/lemma_samesideflip.v
Elements/OriginalProofs/proposition_39.v
Elements/OriginalProofs/proposition_40.v
Elements/OriginalProofs/proposition_41.v
Elements/OriginalProofs/lemma_samesidecollinear.v
Elements/OriginalProofs/proposition_42.v
Elements/OriginalProofs/proposition_42B.v
Elements/OriginalProofs/lemma_supplements2.v
Elements/OriginalProofs/proposition_28D.v
Elements/OriginalProofs/proposition_43B.v
Elements/OriginalProofs/proposition_43.v
Elements/OriginalProofs/lemma_parallelbetween.v
Elements/OriginalProofs/proposition_44A.v
Elements/OriginalProofs/proposition_44.v
Elements/OriginalProofs/lemma_RTcongruence.v
Elements/OriginalProofs/lemma_RTsymmetric.v
Elements/OriginalProofs/proposition_45.v
Elements/OriginalProofs/lemma_equaltorightisright.v
Elements/OriginalProofs/proposition_46.v
Elements/OriginalProofs/lemma_oppositesideflip.v
Elements/OriginalProofs/lemma_righttogether.v
Elements/OriginalProofs/lemma_squareparallelogram.v
Elements/OriginalProofs/lemma_erectedperpendicularunique.v
Elements/OriginalProofs/lemma_twoperpsparallel.v
Elements/OriginalProofs/lemma_legsmallerhypotenuse.v
Elements/OriginalProofs/lemma_tworays.v
Elements/OriginalProofs/lemma_altitudeofrighttriangle.v
Elements/OriginalProofs/proposition_47A.v
Elements/OriginalProofs/lemma_angleaddition.v
Elements/OriginalProofs/proposition_47B.v
Elements/OriginalProofs/lemma_supplementofright.v
Elements/OriginalProofs/lemma_PGrectangle.v
Elements/OriginalProofs/lemma_squarerectangle.v
Elements/OriginalProofs/proposition_48A.v
Elements/OriginalProofs/lemma_squareflip.v
Elements/OriginalProofs/proposition_47.v
Elements/OriginalProofs/lemma_squaresequal.v
Elements/OriginalProofs/lemma_rectanglerotate.v
Elements/OriginalProofs/lemma_rectangleparallelogram.v
Elements/OriginalProofs/lemma_paste5.v
Elements/OriginalProofs/proposition_48.v
Elements/OriginalProofs/book1.v
)

echo "${GEOCOQ_DIR}"

# Check that the GEOCOQ_DIR variable is set
if [ -z "${GEOCOQ_DIR}" ]; then
	echo "Environment variable GEOCOQ_DIR is not set."
	echo "Please set it to the path of your GeoCoq library."
	echo "Failing..."
	exit 1
fi

# Remove previous GeoCoq dependencies and the lists in Make / import.v
rm -f $CURDIR/Make
rm -f $CURDIR/import.v
rm -rf $CURDIR/GeoCoq

# Create them again from the GEOCOQ_FILES variable
mkdir  $CURDIR/GeoCoq
echo "import.v" >> $CURDIR/Make
for i in "${ELEMENTS_FILES[@]}"; do
	mkdir -p $CURDIR/GeoCoq/$(dirname "$i")
	cp ${GEOCOQ_DIR}/$i $CURDIR/GeoCoq/$i
	echo "GeoCoq/$i" >> $CURDIR/Make
	echo "Require Import GeoCoq.$i" | sed -e "s/\.v/\./" | sed -e "s/\//\./g" >> $CURDIR/import.v
done
