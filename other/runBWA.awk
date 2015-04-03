BEGIN {FS="\t"}
{
        fileCheck=sprintf("if [ -f $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq.gz ]\n\tthen rm $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq.gz\nfi",$4,$4)
        wget=sprintf("wget %s -O $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq.gz",$2,$4)
        zcat=sprintf("zcat $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq.gz > $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq",$4,$4)
        md5check1=sprintf("md5=`md5sum $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq.gz  | cut -d\" \" -f1`",$4)
        md5check2=sprintf("if [ \"$md5\" != \"%s\" ]\n\tthen echo \"Error in md5 in %s\" >&2\n\texit\nfi",$3,$4)
        moduleLoad1=sprintf("module load bwa/0.7.5a")
        moduleLoad2=sprintf("module load samtools/0.0.19")
        bwaaln=sprintf("bwa aln -t 8 -f $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq.sai /share/data/umw_biocore/genome_data/mouse/mm9/mm9.fa $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq",$4,$4)
        bwasame=sprintf("bwa samse -f $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq.bwa_out.sam /share/data/umw_biocore/genome_data/mouse/mm9/mm9.fa $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq.sai $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq", $4,$4,$4,$4,$4)
        samtoolsview=sprintf("samtools view -bS -o $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq.bwa_out.bam $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq.bwa_out.sam",$4,$4,$4,$4)
        rm=sprintf("rm $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq.sai $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq.bwa_out.sam $HOME/nearline/enhancer_predictions/new_encode_data/%s.fastq" ,$4,$4,$4)
        print fileCheck > "bsubFiles/bwa_new_encode_data_"$4".bsub"
        print wget >> "bsubFiles/bwa_new_encode_data_"$4".bsub"
        print md5check1 >> "bsubFiles/bwa_new_encode_data_"$4".bsub"
        print md5check2 >> "bsubFiles/bwa_new_encode_data_"$4".bsub"
        print zcat >> "bsubFiles/bwa_new_encode_data_"$4".bsub"
        print moduleLoad1 >> "bsubFiles/bwa_new_encode_data_"$4".bsub"
        print moduleLoad2 >> "bsubFiles/bwa_new_encode_data_"$4".bsub"
        print bwaaln >> "bsubFiles/bwa_new_encode_data_"$4".bsub"
        print bwasame >> "bsubFiles/bwa_new_encode_data_"$4".bsub"
        print samtoolsview >> "bsubFiles/bwa_new_encode_data_"$4".bsub"
        print rm >> "bsubFiles/bwa_new_encode_data_"$4".bsub"
}
