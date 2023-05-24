load('S1_sample_id.RData')
set.seed(1234)
control_eid_11 <- sample(control_eid,length(case_eid))
case_eid_test <- sample(case_eid,round(length(case_eid)*0.2))
control_eid_11test <- sample(control_eid_11,length(case_eid_test))

case_eid_trainK <- setdiff(case_eid,case_eid_test)
control_eid_trainK <- setdiff(control_eid_11,control_eid_11test)
control_eid_trainK_f <- sample(setdiff(control_eid,control_eid_11test),
                               min(length(case_eid_trainK)*50,length(setdiff(control_eid,control_eid_11test))),replace = F)
control_eid_trainK_f <- unique(c(control_eid_trainK_f,control_eid_trainK))

fam_file <- data.frame(V1=c(case_eid_trainK,control_eid_trainK),
                       V2=c(case_eid_trainK,control_eid_trainK),
                       V3=0,
                       V4=0,
                       V5=0,
                       V6=c(rep(2,length(case_eid_trainK)),rep(1,length(control_eid_trainK))))
write.table(fam_file,file = paste0('','ukb_cal_chr1_v2.fam','.sample'),sep = ' ',quote = F,col.names = F,row.names = F)

fam_file <- data.frame(V1=c(case_eid_trainK,control_eid_trainK_f),
                       V2=c(case_eid_trainK,control_eid_trainK_f),
                       V3=0,
                       V4=0,
                       V5=0,
                       V6=c(rep(2,length(case_eid_trainK)),rep(1,length(control_eid_trainK_f))))
write.table(fam_file,file = paste0('','ukb_cal_chr1_v2_f.fam','.sample'),sep = ' ',quote = F,col.names = F,row.names = F)

fam_file_all <- data.frame(V1=c(case_eid,control_eid_11),
                       V2=c(case_eid,control_eid_11),
                       V3=0,
                       V4=0,
                       V5=0,
                       V6=c(rep(2,length(case_eid)),rep(1,length(control_eid_11))))
write.table(fam_file_all,file = paste0('','ukb_cal_chr1_v2_all.fam','.sample'),sep = ' ',quote = F,col.names = F,row.names = F)

save(case_eid_trainK,control_eid_trainK,case_eid,control_eid,control_eid_11,file = 'S2_traintest_id.RData')
