#!/bin/bash
for tissue in {"forebrain","neuraltube","brain","heart","limb","hindbrain","midbrain"}
#for tissue in {"forebrain","midbrain","heart"}
do
	grep "${tissue}" /home/si14w/nearline/enhancer_predictions/mouse_collapsed_2.bed > /home/si14w/nearline/enhancer_predictions/mouse_${tissue}_positive.2.bed
	grep -v "${tissue}" /home/si14w/nearline/enhancer_predictions/mouse_collapsed_2.bed | sed 's/positive/negative/g' > /home/si14w/nearline/enhancer_predictions/mouse_${tissue}_negative.2.bed
	cat /home/si14w/nearline/enhancer_predictions/mouse_${tissue}_positive.2.bed /home/si14w/nearline/enhancer_predictions/mouse_${tissue}_negative.2.bed | sort -k1,1 -k2,2n > /home/si14w/nearline/enhancer_predictions/mouse_${tissue}_positive_and_negative_sorted.bed
	echo chr        start   end     id      label   species tissues > ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.bed
	awk '{ print $4"_"$6"\t"$0 }' /home/si14w/nearline/enhancer_predictions/mouse_${tissue}_positive_and_negative_sorted.bed | sort -k1,1V | awk '{ print $2"\t"$3"\t"$4"\t"$5"\t"$6"\t"$7"\t"$8}' | sed 's/negative/0/' | sed 's/positive/1/' >> ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.bed
	#for age in {"embryonic_11.5day","embryonic_14.5day","postnatal_0day"}
	age="embryonic_11.5day"
#	do
		echo "ls ~/nearline/enhancer_predictions/new_encode_data/*_mm_${age}_*${tissue}*_VS_*_mm_${age}_*${tissue}*_ChIP-seq_Control.macs2_output_peaks.narrowPeak"
		for peakFile in `ls ~/nearline/enhancer_predictions/new_encode_data/*_mm_${age}_*${tissue}*_VS_*_mm_${age}_*${tissue}*_ChIP-seq_Control.macs2_output_peaks.narrowPeak`
		do
			featurename=`basename $peakFile | cut -d"_" -f2,3,4,5,7`
			bwFile=`basename $peakFile | sed 's/output_peaks\.narrowPeak/output_logLR\.bw/'`
			bedtools intersect -b ~/nearline/enhancer_predictions/mouse_${tissue}_positive_and_negative_sorted.bed -a $peakFile -wb  | awk '{ print $1"\t"$2"\t"$3"\t"$7"\t"$11"\t"$12"\t"$13"\t"$14"\t"$15"\t"$16 }' > ~/nearline/enhancer_predictions/new_encode_data/${featurename}_intersect_mouse_${tissue}_training.bed
	
			Rscript getWeightedScores.R $HOME/nearline/enhancer_predictions/new_encode_data/${featurename}_intersect_mouse_${tissue}_training.bed ${featurename} /home/si14w/nearline/enhancer_predictions/new_encode_data/${featurename}.weightedscore
	
			bedtools intersect -a ~/nearline/enhancer_predictions/mouse_${tissue}_positive_and_negative_sorted.bed -b $peakFile -v | awk '{ print $1"\t"$2"\t"$3"\t"$4"_"$6}' >  ~/nearline/enhancer_predictions/new_encode_data/${featurename}_no_intersect_mouse_${tissue}_training.bed
			bigWigAverageOverBed ~/nearline/enhancer_predictions/new_encode_data/${bwFile} ~/nearline/enhancer_predictions/new_encode_data/${featurename}_no_intersect_mouse_${tissue}_training.bed stdout | cut -f1,6 | sed 's/_/\t/' > ~/nearline/enhancer_predictions/new_encode_data/${featurename}_no_intersect_mouse_${tissue}_training.bed.score
			echo ${featurename} > ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores_${featurename}.bed
			cat ~/nearline/enhancer_predictions/new_encode_data/${featurename}_no_intersect_mouse_${tissue}_training.bed.score ~/nearline/enhancer_predictions/new_encode_data/${featurename}.weightedscore | sort -k1,1V | awk '{ print $3}' >> ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores_${featurename}.bed
			paste ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.bed ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores_${featurename}.bed > ~/nearline/enhancer_predictions/new_encode_data/temp
			mv ~/nearline/enhancer_predictions/new_encode_data/temp ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.bed
		done
#	done
	echo done master for ${tissue}
	awk '{ printf("%s_%s\t",$4,$6); for (e = 8; e < NF; e ++) { printf("%s\t", $e) } printf ("%s\n", $5) }' ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.bed >  ~/nearline/enhancer_predictions/new_encode_data/${tissue}_master_table_training.tab
	echo glm	svm	gbm	nb	glm_noreg > ~/my/auc_${tissue}
	for i in {1..50}
	do
		echo $i of 50
		Rscript $HOME/my/super.R $HOME/nearline/enhancer_predictions/new_encode_data/${tissue}_master_table_training.tab ${tissue}
	done
done
