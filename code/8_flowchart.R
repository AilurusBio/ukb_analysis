#install.packages("flowchart", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
library(DiagrammeR)
library(DiagrammeRsvg)
library(magrittr)
library(rsvg)
library(
  caTools)
data_a1 <- list(a1=502492,
             a2=343558,
             a3=Preliminary_patients_data,
             a4=Patients_data,
             a5=Case_data,
             a6=Control_data)


code <- "digraph {
  node [shape=rectangle]
  A [label='@@1']
  B [label='@@2']
  C [label='@@3']
  D [label='@@4']
  E [label='@@5']
  F [label='@@6']
  G [label='Training set (80%)']
  H [label='Genetic + Questionnaire + Blood biochemical feature']
  I [label='Modeling']
  J [label='Validation set (20%)']
  K [label='Model evaluation']
  A -> B [label='Preliminary data filtering']
  B -> C [label='Filter based on diagnosis']
  C -> D [label='The medical record contains the time of first diagnosis']
  D -> E [label='case_age_min < First diagnosis age <= case_age_max']
  B -> F [label='No history of the disease']
  F -> G
  E -> G
  F -> J
  E -> J
  G -> H [label='GWAS']
  H -> I 
  I -> K
  J -> K
}
"

code <- paste0(code,
               "[1]:  paste0('Raw data (n = ', data_a1$a1, ')')",'\n',
               "[2]:  paste0('Preliminary research data (n = ', data_a1$a2, ')')",'\n',
               "[3]:  paste0('Preliminary patients data (n = ', data_a1$a3, ')')",'\n',
               "[4]:  paste0('Patients data (n = ', data_a1$a4, ')')",'\n',
               "[5]:  paste0('Case data (n = ', data_a1$a5, ')')",'\n',
               "[6]:  paste0('Control data (n = ', data_a1$a6, ')')",'\n'
               )

code <- sub('case_age_min',case_age_min,code)
code <- sub('case_age_max',case_age_max,code)

# Render the flowchart
########### P 1
library(DiagrammeR)
grViz(code)%>%
  export_svg %>% charToRaw %>% rsvg_png("paper_chart/P1_diagram.png")
######   P 2
png("paper_chart/P2_age.png",width = 600,height = 400)
plot(density(na.omit(first_confirmed_age)),main = '') #main = 'Age distribution of the confirmed population'
abline(v = case_age_min, lty = 2)
abline(v = case_age_max, lty = 2)
dev.off()
######  ST1
file.copy('S3_label_select.csv','paper_chart/ST1_label_select.csv',overwrite = T)
###### T1
# load('S2_traintest_id.RData')
# case_eid_trainK,control_eid_trainK,case_eid,control_eid,control_eid_11
# length(case_eid_trainK)
# length(case_eid)

###### T1
sampleqc <- read.delim('feature_info/sampleQC.tsv')
write.csv(sampleqc,file = 'paper_chart/T1_sampleQC.csv')
###### P3
file.copy('GWAS_plot/manhatom.png','paper_chart/P3_manhatom.png',overwrite = T)
###### T2
out_rs_AD_anno <- rs_AD_anno[rs_AD_anno$P < significance,]
colnames(out_rs_AD_anno) <- toupper(colnames(out_rs_AD_anno))
out_rs_AD_anno <- out_rs_AD_anno[,c(1:8,11)]
write.csv(out_rs_AD_anno,file = 'paper_chart/T2_GWAS_ANNO.csv')
###### T3
out_auc_res <- auc_res
colnames(out_auc_res) <- c("ANN" , "SVM-Linear", "SVM-Radial", "KNN" ,  "RandomForest" , "AdaBoost" , "Bayes-glm", "C5.0")
out_auc_res <- cbind(Model_features=c('Genetic','Questionnaire',
                                      'Questionnaire + \nBlood biochemical feature',
                                      'Genetic + \nQuestionnaire + \nBlood biochemical feature') ,
                     round(out_auc_res,5)
                     )
write.csv(out_auc_res,file = 'paper_chart/T3_AUC.csv',row.names = T)
###### P4
png("paper_chart/P4_AUC.png", width = 1000, height = 1000, units="px", pointsize=30)
#colAUC(OB_res,test_data$PHENOTYPE,plotROC = T)
library(pROC)
colors <- rainbow(ncol(OB_res))
linetypes <- 1:ncol(OB_res)

# 初始化一个空白的绘图区域
plot(0, 0, type = "n", xlim = c(1, 0), ylim = c(0, 1), main = "ROC Curves", xlab = "False Positive Rate", ylab = "True Positive Rate")

# 使用二进制标签（0和1）替换原始标签（case和control）
binary_labels <- ifelse(test_data$PHENOTYPE == "case", 1, 0)

# 遍历每个模型，计算并绘制ROC曲线
for (i in 1:ncol(OB_res)) {
  model_scores <- OB_res[, i]
  roc_obj <- roc(binary_labels, model_scores)
  lines(roc_obj, col = colors[i], lty = linetypes[i])
}

# 添加图例
legend("bottomright", legend = colnames(OB_res), col = colors, lty = linetypes)

dev.off()
###### P5
png("paper_chart/P5_Imp.png", width = 600, height = 400, units="px", pointsize=30)
caret::dotPlot(knnImp)
dev.off()

