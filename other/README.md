awk -F"\t" '{ print $12"\t"$13"\t"$14"\t"$NF}' mouse_chipseq_2014_release.tsv > somefile.tab <br>
Remove blank spaces from somefile.tab<br>
awk -f runBWA.awk somefile.tab<br>
runMACS2.sh<br>
