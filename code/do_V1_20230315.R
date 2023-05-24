rawdata_path <- '~/ukb_sj_data/'
anno_gtf_path <- paste0(rawdata_path,'/genome/Homo_sapiens.GRCh37.87.gtf')
case_age_max <- 50
case_age_min <- 20
icd10_case <- 'J44'
case_full_name <- 'early-onset chronic obstructive pulmonary'
case_name <- 'COPD'
significance <- 1e-5


dir.create('user_chart')
dir.create('paper_chart')



source('code/1_data_clean.R')
source('code/2_data_gwas.R')

dir.create('gwas')
command_gwas <- 'bash ./code/independent_2.sh ./ukb_cal_chr1_v2_f.fam.sample  ./P_cov.txt ./gwas/ ./ukb_cal_chr1_v2_all.fam.sample'
system(command_gwas)
system('rm clean-*')
system('rm res.glm*')
source('code/3_sigSNP_anno.R')

source('code/4_lm_model.R')
source('code/5_feature_ml_V2.R')
source('code/6_val.R')
source('code/7_out_info.R')

##########
source('GPT_code/paper_promt.R')

source('code/9_paper_promt.R')


# command_rmd <- "/usr/lib/rstudio-server/bin/quarto/bin/pandoc +RTS -K512m -RTS test.knit.md --to latex --from markdown+autolink_bare_uris+tex_math_single_backslash --output test.tex --lua-filter /home1/songjie/R/x86_64-pc-linux-gnu-library/3.6/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home1/songjie/R/x86_64-pc-linux-gnu-library/3.6/rmarkdown/rmarkdown/lua/latex-div.lua --self-contained --highlight-style tango --pdf-engine pdflatex --variable graphics --include-in-header /tmp/Rtmptq2GeE/rmarkdown-str31516754a38948.html --variable 'geometry:margin=1in' --include-in-header /tmp/Rtmptq2GeE/rmarkdown-str3151677ec5417a.html "
# system(command_rmd)
