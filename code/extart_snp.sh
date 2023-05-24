cd gwas
### 数据链接

chr=1
for chr in $(seq 1 22)
do
ln -s /exdata/server/public_database/ukb/call/ukb_cal_chr$chr\_v2.bed raw-GWA-data.bed
ln -s /exdata/server/public_database/ukb/call/ukb51671_cal_chr$chr\_v2_s488264.fam raw-GWA-data.fam
ln -s /exdata/server/public_database/ukb/call/ukb_snp_chr$chr\_v2.bim raw-GWA-data.bim

../software/plink2 --bfile  raw-GWA-data --keep ../ukb_cal_chr1_v2_all.fam.sample --pheno  ../ukb_cal_chr1_v2_all.fam.sample --pheno-col-nums 6  --extract ori_rsid_sig.txt --export A  -out model_data.txt

mv model_data.txt.raw model_data.txt.chr${chr}

#cat res.glm.logistic.clean  | awk '{if($7<0.0000001) print $0 }' > res.glm.logistic.clean.sig
#cat res.glm.logistic.clean.sig | cut -f 3 >> ori_rsid_sig.txt
rm *.bed *.bim *.fam
rm model_data.txt.log
done

####
