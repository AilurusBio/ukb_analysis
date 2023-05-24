cd gwas
### 数据链接

chr=1
for chr in $(seq 1 22)
do
ln -s /exdata/server/public_database/ukb/call/ukb_cal_chr$chr\_v2.bed raw-GWA-data.bed
ln -s /exdata/server/public_database/ukb/call/ukb51671_cal_chr$chr\_v2_s488264.fam raw-GWA-data.fam
ln -s /exdata/server/public_database/ukb/call/ukb_snp_chr$chr\_v2.bim raw-GWA-data.bim

plink --bfile raw-GWA-data --make-bed --out raw-GWA-data
../software/plink2 --bfile raw-GWA-data --maf 0.01 --geno 0.05 --hwe 0.00001 --make-bed --out clean-GWA-data

../software/plink2 --bfile clean-GWA-data --keep ../ukb_cal_chr1_v2.fam.sample --pheno ../ukb_cal_chr1_v2.fam.sample  --pheno-col-nums 6 --glm no-firth no-x-sex --covar ../P_cov.txt --covar-col-nums 3-4 -out clean-GWA-data

cp clean-GWA-data.PHENO1.glm.logistic clean-GWA-data_GWAS.chr${chr}

grep ADD clean-GWA-data.PHENO1.glm.logistic | cut -f 1-5,9,12  > res.glm.logistic.clean
cp res.glm.logistic.clean res.glm.logistic.clean.chr${chr}
#cat res.glm.logistic.clean  | awk '{if($7<0.0000001) print $0 }' > res.glm.logistic.clean.sig
#cat res.glm.logistic.clean.sig | cut -f 3 >> ori_rsid_sig.txt
rm *.bed *.bim *.fam
done

####
#plink --bfile  raw-GWA-data --keep ../ukb_cal_chr1_v2_all.fam.sample --pheno  ../ukb_cal_chr1_v2_all.fam.sample --pheno-col-nums 6  --extract ori_rsid_sig.txt --export A  -out model_data.txt

####   extract sig and tag snp
#plink --bfile  raw-GWA-data --keep ukb_snp_chr_v2.fam.sample --pheno ukb_snp_chr_v2.fam.sample --mpheno 4  --extract ori_rsid_sig.txt --export A  -out model_data.txt
#plink --bfile raw-GWA-data --rm-dup force-first --extract ori_rsid_sig.txt --indep-pairwise 100 10 0.8 -out ori_rsid_sig_uniq

