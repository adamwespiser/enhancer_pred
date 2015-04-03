for accession in `awk -F"\t" '{ if (NR > 1 && $8 != "Control" && $8 != "control") print $1 }' mouse_chipseq_2014_release.tsv | sort | uniq`
#for accession in `awk -F"\t" '{ if ($8 != "Control" && $8 != "control") print $1 }' testawk | sort | uniq`
do
        bamFilesTreat=`ls -ltr $HOME/nearline/enhancer_predictions/new_encode_data/${accession}*.bam | awk '{ print $NF}'`
        row=`grep -m 1 -P "^${accession}" mouse_chipseq_2014_release.tsv`
        controlAccession=`echo "$row" | awk -F"\t" '{ print substr($(NF-1), index($(NF-1),"@")+1, index($(NF-1), ";")-index($(NF-1), "@")-1)}'`
        bamFilesControl=`ls -ltr $HOME/nearline/enhancer_predictions/new_encode_data/${controlAccession}*.bam | awk '{ print $NF}'`
        firstFileNameTreat=`echo $bamFilesTreat | awk '{ print $1}'`
        prefix_treat=`basename $firstFileNameTreat | cut -d"_" -f1-7`

        firstFileNameControl=`echo $bamFilesControl | awk '{ print $1}'`
        prefix_control=`basename $firstFileNameControl | cut -d"_" -f1-7`
        echo """
        macs2 callpeak -t "$bamFilesTreat"  -c "$bamFilesControl"  -f BAM -B --nomodel --SPMR -g mm -n $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output
        macs2 bdgcmp -t  $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output_treat_pileup.bdg -c $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output_control_lambda.bdg -o $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output_logLR.bdg -m logLR -p 0.00001
        macs2 bdgcmp -t  $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output_treat_pileup.bdg -c $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output_control_lambda.bdg -o $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output_FE.bdg -m FE -p 0.00001
        macs2 bdgcmp -t  $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output_treat_pileup.bdg -c $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output_control_lambda.bdg -o $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output_qpois.bdg -m qpois -p 0.00001

        $HOME/TR/bdg2bw $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output_logLR.bdg mm9.len
        $HOME/TR/bdg2bw $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output_FE.bdg mm9.len
        $HOME/TR/bdg2bw $HOME/nearline/enhancer_predictions/new_encode_data/${prefix_treat}_VS_${prefix_control}.macs2_output_qpois.bdg mm9.len

        """ > bsubFiles/macs2_$accession.bsub
done
