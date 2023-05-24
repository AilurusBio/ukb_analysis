library(stringr)
library(readr)
print('Generate case data...')
ukb_1K <- read.csv(paste0(rawdata_path,'./ukb44440/ukb44440_1ktest.csv'),stringsAsFactors = F)
ukb_Data_Dictionary <- read.csv(paste0(rawdata_path,'./Data_Dictionary_Showcase.csv'),stringsAsFactors = F)

case_FieldID <- ukb_Data_Dictionary$FieldID[grep(paste0('Date ',icd10_case),ukb_Data_Dictionary$Field)]
if (is.na(case_FieldID)) {
  print('None icd10 case match')
}

input_file <- paste0(rawdata_path ,"./ukb44440_2/ukb44678.tab")
output_file <- "./P_CaseDate.tsv"
lines <- read_lines(input_file, n_max = 1)
fields <- str_split(lines, "\t", simplify = TRUE)[1,]


case_FieldID_col <- paste0('f.',case_FieldID,'.0.0')

command <- paste0('cut -f 1,',grep(paste0('f.',case_FieldID,'.0.0'),fields),' ',input_file,' > ',output_file)
system(command)
pdf("user_chart/dataclean.pdf")

print('Load case data...')
rel_table <- read.table(paste0(rawdata_path,'./ukb_rel_a51671_s488249.dat'),sep = ' ',stringsAsFactors = F,header = T)
P_152 <- read.delim(paste0(rawdata_path,'./ukb44440/P_152info_877col.tsv'))
P_54 <- read.delim(paste0(rawdata_path,'./ukb44440_2/P_54info_156col.tsv'))
P_fa <- read.delim(paste0(rawdata_path,'./ukb44440/P_father_age.tsv'))
P_ma <- read.delim(paste0(rawdata_path,'./ukb44440/P_mather_age.tsv'))
P_J44 <- read.delim(paste0('./P_CaseDate.tsv'))

P_AD_ori <- cbind(P_J44[,],
                  P_152[match(P_54$f.eid,P_152$eid),-1],
                  P_54[,-1],
                  P_fa[match(P_54$f.eid,P_152$eid),],
                  P_ma[match(P_54$f.eid,P_152$eid),])
colnames(P_AD_ori)[1] <- 'eid'
dim(P_AD_ori) #502492   

related_id <- unique(unlist(rel_table[rel_table$HetHet >= 0.05,1:2]))
length(related_id)
502492 - length(which(P_AD_ori$X31.0.0 == P_AD_ori$X22001.0.0)) # 14614 
# 502492 - length(which(P_AD_ori$X1647.0.0 == 1 )) # 274226
502492 - length(which(P_AD_ori$X22004.0.0 < 0.1951)) # 16104
502492 - length(which(P_AD_ori$X22005.0.0 < 0.02)) # 21594
502492 - length(which(P_AD_ori$X22006.0.0 == 1)) # 92888
502492 - length(which(P_AD_ori$X22021.0.0 == 0 )) # 162917
502492 - length(which(P_AD_ori$X21000.0.0==1001)) # 59919


# as.numeric(str_split(P_J44[[case_FieldID_col]],'-',simplify = T)[,1]) - 
#   P_AD_ori$X34.0.0


hist(as.numeric(str_split(P_J44[[case_FieldID_col]],'-',simplify = T)[,1]) - 
       P_AD_ori$X34.0.0)

first_confirmed_age <- as.numeric(str_split(P_J44[[case_FieldID_col]],'-',simplify = T)[,1]) - P_AD_ori$X34.0.0
first_confirmed_age <-  na.omit(first_confirmed_age[first_confirmed_age > 0])

P_AD_ori$first_confirmed_age <- as.numeric(str_split(P_J44[[case_FieldID_col]],'-',simplify = T)[,1]) - P_AD_ori$X34.0.0

plot(density(na.omit(first_confirmed_age)),main = '') #main = 'Age distribution of the confirmed population'
abline(v = case_age_min, lty = 2)
abline(v = case_age_max, lty = 2)

confirmed_age_den <- c(length(which(na.omit(first_confirmed_age)<case_age_min)),
                       length(which(na.omit(first_confirmed_age)>=case_age_min & (na.omit(first_confirmed_age)<=case_age_max))),
                       length(which(na.omit(first_confirmed_age)>case_age_max)))
label <- c(paste0('<',case_age_min),paste0('>=',case_age_min,',','>',case_age_max),paste0('>',case_age_max))
label <- paste0(label,':   ',round(confirmed_age_den/sum(confirmed_age_den),3)*100,'%')
pie(confirmed_age_den,label)

###################

###########################
print('Data clean...')


P_AD <- P_AD_ori
P_AD <- P_AD[which(P_AD$X31.0.0 == P_AD$X22001.0.0),]
dim(P_AD) #487878   
# P_AD <- P_AD[which(P_AD$X1647.0.0 == 1 ),]
# dim(P_AD)
P_AD <- P_AD[which(P_AD$X22004.0.0 < 0.1951),]
dim(P_AD)
P_AD <- P_AD[which(P_AD$X22005.0.0 < 0.02),]
dim(P_AD)
P_AD <- P_AD[which(P_AD$X22006.0.0 == 1),]
dim(P_AD)
P_AD <- P_AD[-na.omit(match(related_id,P_AD$eid)),]
# P_AD <- P_AD[which(P_AD$X22021.0.0 == 0 ),]
dim(P_AD)
P_AD <- P_AD[which(P_AD$X21000.0.0==1001),]
dim(P_AD) # 343558   

P_AD$age <- as.numeric(str_split(P_AD[,c('X53.0.0')],'-',simplify = T)[,1])-(P_AD[,c('X34.0.0')])
  # 31	Sex
# 22001	Genetic sex
# 1647	Country of birth (UK/elsewhere)
# 22004	Heterozygosity, PCA corrected 0.1951
# 22005	Missingness 0.02
# 22006	Genetic ethnic grouping  #Caucasian
# 22021 Genetic kinship to other participants # No kinship found
# 21000 Ethnic background # British


# P_152_J44 <- read.delim('../ukb44440/P_152info_877col_J44.tsv')
# P_152_F00 <- read.delim('../ukb44440/P_152info_877col_F00.tsv')

#### 131036	Date J44 first reported (alzheimer's disease) 这个数据是缺失的 
#### add Non-cancer illness code, self-reported  20002
####  vn 40002，41202，X41204
Causes_Underlying_death <- apply(P_AD[,grep('40001',names(P_AD))], 1, function(x) {
  paste(na.omit(x),collapse = ';' )
})
Causes_Contributory_death <- apply(P_AD[,grep('40002',names(P_AD))], 1, function(x) {
  paste(na.omit(x),collapse = ';' )
})
Causes_main_ICD10 <- apply(P_AD[,grep('41202',names(P_AD))], 1, function(x) {
  paste(na.omit(x),collapse = ';' )
})
Causes_secondary_ICD10 <- apply(P_AD[,grep('41204',names(P_AD))], 1, function(x) {
  paste(na.omit(x),collapse = ';' )
})

AD_label_group <- list(Underlying_death=
                         (grep(icd10_case,Causes_Underlying_death) ),
                       Contributory_death=
                         (grep(icd10_case,Causes_Contributory_death) ),
                       main_ICD10=
                         (grep(icd10_case,Causes_main_ICD10) ),
                       secondary_ICD10=
                         (grep(icd10_case,Causes_secondary_ICD10) ))
P_AD$first_confirmed_age[unique(unlist(AD_label_group))]

table(is.na(P_AD$first_confirmed_age[unique(unlist(AD_label_group))]))
aaa <- P_AD$first_confirmed_age[unique(unlist(AD_label_group))]
plot(density(na.omit(aaa)))
abline(v = case_age_min, lty = 2)
abline(v = case_age_max, lty = 2)

aaa <- Causes_secondary_ICD10[!is.na(P_AD$first_confirmed_age)]
### 
sort(table(unlist(str_split(aaa,';'))),decreasing = T)
#################### 
print('Data case and control...')

case_id_all <- intersect(unique(unlist(AD_label_group)),
                         which(!is.na(P_AD$first_confirmed_age)))
out_case_id_all <- union(unique(unlist(AD_label_group)),
                         which(!is.na(P_AD$first_confirmed_age)))
control_id_all <- setdiff(1:nrow(P_AD),out_case_id_all)

first_confirmed_age_2050 <- which(P_AD$first_confirmed_age >=case_age_min & P_AD$first_confirmed_age < case_age_max)
id_age <- as.numeric(str_split(P_AD[,c('X53.0.0')],'-',simplify = T)[,1])-(P_AD[,c('X34.0.0')])
attending_assessment_centre_2050 <- which(id_age >=case_age_min & id_age < case_age_max)


case_eid <- P_AD$eid[intersect(case_id_all,first_confirmed_age_2050)]
control_eid <- P_AD$eid[(control_id_all)]

save(case_eid,control_eid,P_AD,file = 'S1_sample_id.RData')

###############  control sample 

table(P_AD[P_AD$eid %in% case_eid,'X31.0.0'])


##############


P_cov <- data.frame(P_AD_ori$eid,
                    P_AD_ori$eid,
                    P_AD_ori$X31.0.0, # 性别
                    as.numeric(str_split(P_AD_ori[,c('X53.0.0')],'-',simplify = T)[,1])-
                      (P_AD_ori[,c('X34.0.0')]), #年龄
                    #                    P_AD_ori$X26414.0.0, # 教育评分
                    #                    P_AD_ori$X26415.0.0, # 房屋评分
                    P_AD_ori$X26411.0.0 # 收入评分
                    #                    P_AD_ori$X20116.0.0, # 犯罪评分
                    #                    P_AD_ori$X20117.0.0) # 居住环境评分
)
write.table(P_cov,file = 'P_cov.txt',sep = ' ',quote = F,col.names = F,row.names = F)

dev.off()