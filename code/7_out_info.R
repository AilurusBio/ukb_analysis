tmp <- system('wc -l gwas/clean-GWA-data_GWAS.chr* | tail -n 1', intern = TRUE)
snp_num <- round(as.numeric(gsub("[^[:digit:]]", "", tmp))/4)
snp_num_sig <- ncol(lm_res$train_data)-1


best_feature
best_model_name 
best_model_performance$byClass['Specificity']
prs_performance$byClass['Specificity']

best_model_performance_AUC
patients <- length(c(case_eid,control_eid))

Preliminary_patients_data <- length(first_confirmed_age) #length(unique(c(unlist(AD_label_group))))
Patients_data <- length(which(!is.na(P_AD$first_confirmed_age[unique(unlist(AD_label_group))])))
Case_data <- length(case_eid)
Control_data <- length(control_eid)

as.character(knnImp_res[1:3,1])

rs_AD_anno <- read.csv('./GWAS_plot/Top40_gwas.anno.csv',stringsAsFactors = F)
rs_AD_anno$gene_name[1]
rs_AD_anno$ID[1]