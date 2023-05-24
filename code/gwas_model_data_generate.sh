#plink --bfile  raw-GWA-data --keep ../ukb_cal_chr1_v2_all.fam.sample --pheno  ../ukb_cal_chr1_v2_all.fam.sample --pheno-col-nums 6  --extract ori_rsid_sig.txt --export A  -out model_data.txt

####   extract sig and tag snp
#plink --bfile  raw-GWA-data --keep ukb_snp_chr_v2.fam.sample --pheno ukb_snp_chr_v2.fam.sample --mpheno 4  --extract ori_rsid_sig.txt --export A  -out model_data.txt
#plink --bfile raw-GWA-data --rm-dup force-first --extract ori_rsid_sig.txt --indep-pairwise 100 10 0.8 -out ori_rsid_sig_uniq