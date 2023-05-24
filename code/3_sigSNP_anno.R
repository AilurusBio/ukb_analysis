source('code/plot_manhatom_V2.r')

plot_manhatton(save_path ='GWAS_plot/',
               gwas_res_path = 'gwas/',significance=significance)


# Depends BiocGenerics (>= 0.1.0), S4Vectors (>= 0.17.29), IRanges (>=
# 2.13.23), GenomeInfoDb (>= 1.25.7), GenomicRanges (>= 1.31.17),
# AnnotationDbi (>= 1.41.4)
# Imports methods, utils, stats, tools, DBI, RSQLite (>= 2.0), RCurl,
# XVector (>= 0.19.7), Biostrings (>= 2.47.6), rtracklayer (>=
# 1.39.7), biomaRt (>= 2.17.1), Biobase (>= 2.15.1)

# library(biomaRt)
require(GenomicRanges)
# require(GenomicFeatures)
require(rtracklayer)
require(ape)

# BiocManager::install('biomaRt')
# install.packages('biomaRt')
ref_gtf <- import.gff(anno_gtf_path)
head(ref_gtf)
ref_gtf_gene <- ref_gtf[ref_gtf$type %in% c('exon','gene','five_prime_utr', 'three_prime_utr')]


annopeak <- function(focus_table,ref_range){
  if (class(focus_table) == 'GRanges') {
    focus_range <- focus_table
    rs_AD <- as.data.frame(focus_range)
  }else{
    rs_AD <- focus_table
    focus_range <- GRanges(seqnames = rs_AD$CHROM,ranges = rs_AD$POS)
  }
  ref_range <- ref_range
  nearest_ref <- ref_range[nearest(focus_range,ref_range)]
  nearest_ref_dis <- distanceToNearest(focus_range,ref_range)
  
  rs_AD$gene_name <- nearest_ref@elementMetadata@listData$gene_name
  rs_AD$gene_id <- nearest_ref@elementMetadata@listData$gene_id 
  rs_AD$gene_pos <- as.character(nearest_ref)
  rs_AD$type <- nearest_ref@elementMetadata@listData$type 
  rs_AD$type[rs_AD$type=='gene'] <- NA
  
  rs_AD$distance <- nearest_ref_dis@elementMetadata@listData$distance
  
  rs_AD$distance_direction <- NA
  strand_negative <- which(as.character(nearest_ref@strand)== '-') 
  strand_positive <- which(as.character(nearest_ref@strand)== '+') 
  rs_AD$distance_direction[nearest_ref@ranges@start > focus_range@ranges@start +focus_range@ranges@width ] <- -1
  rs_AD$distance_direction[nearest_ref@ranges@start + nearest_ref@ranges@width <
                             focus_range@ranges@start] <- 1
  rs_AD$distance_direction[strand_negative] <- 
    -rs_AD$distance_direction[strand_negative]
  rs_AD$distance_direction[rs_AD$distance_direction == -1] <- 'upstream'
  rs_AD$distance_direction[rs_AD$distance_direction == 1] <- 'downstream'
  
  rs_AD
}

rs_AD <- read.csv('./GWAS_plot/Top40_gwas.csv')
rs_AD_anno <- annopeak(focus_table = rs_AD,ref_range = ref_gtf_gene)
write.csv(rs_AD_anno[,c("CHROM","POS","ID","REF","ALT","OR", "P","gene_name",'type','gene_pos','distance','distance_direction')],
          file = './GWAS_plot/Top40_gwas.anno.csv',row.names = F,quote = F)

write.table(rs_AD_anno$ID[rs_AD_anno$P < significance],file = './gwas/ori_rsid_sig00001.txt',quote =F,row.names = F,col.names = F)


# AD_range_flank <- flank(GRanges(seqnames = rs_AD$CHROM,ranges = rs_AD$POS)[rs_AD$sig],
#                         width = 10000)
# prxoy_range_flank <- flank(GRanges(seqnames = rs_prxoy$CHROM,ranges = rs_prxoy$POS)[rs_prxoy$sig],
#                            width = 10000)
# rs_AD_union <- intersect(AD_range_flank,prxoy_range_flank)
# rs_AD_intersect_annopeak <- annopeak(focus_table = rs_AD_union,ref_range = ref_gtf_gene)
# 
# count_rs1 <- findOverlaps(rs_AD_union,AD_range_flank)
# count_rs2 <- findOverlaps(rs_AD_union,prxoy_range_flank)
# rs_AD_intersect_annopeak$range_rs_id <- NA
# for (i in 1:nrow(rs_AD_intersect_annopeak)) {
#   rs_AD_intersect_annopeak$range_rs_id[i] <- paste0(unique(c(rs_AD$ID[count_rs1@to[count_rs1@from==i]],
#                                                              rs_prxoy$ID[count_rs2@to[count_rs2@from==i]])),collapse = ';')
#   print( rs_AD_intersect_annopeak$range_rs_id[i])
# }
# write.csv(rs_AD_intersect_annopeak,file = './GWAS_plot/ADandAD-proxy_intersect_anno.csv',row.names = F,quote = F)
# 

