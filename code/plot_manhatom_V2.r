#!/usr/bin/Rscript
#example : Rscript plot_manhatom.r XXX.assoc XXX.pdf
#define the function to plot the manhatton and quantitle-quantitle plot
plot_manhatton<-function(save_path,gwas_res_path,updown = 10000,significance=1e-7){
  save_file <- save_path
  res_file <- gwas_res_path
  data <- c()
  for(file in grep('clean-GWA-data_GWAS.chr',dir(res_file,full.names = T),value = T)){
    data_tmp <- read.table(file,header=F,stringsAsFactors = F)
    data_tmp <- data_tmp[which(data_tmp[,7]=="ADD"),]
    dim(data_tmp)
    data <- rbind(data,data_tmp)
    print(file)
  }
  print(dim(data))
  significance=significance#1e-4 #1e-7 #-log10(0.05/length(data[,1]))
  
  dir.create(save_path)
  #setwd('GWAS_plot')
  data_view <- data[order(data$V12),]
  colnames(data_view) <- c('CHROM' ,'POS' ,'ID', 'ALT',  'REF','A1',
                           'TEST' ,'OBS_CT' ,
                           'OR' ,'LOG(OR)_SE', 'Z_STAT', 'P', 'ERRCODE')
  data_view$sig <- data_view$P < significance #0.05/length(data[,1])
  data_view <- data_view[,!colnames(data_view) %in% c('A1','TEST','LOG(OR)_SE', 'Z_STAT' , 'ERRCODE')]
  min_num <- length(which(data_view$sig))
  if (min_num<40) {
    min_num <- 40
  }
  write.csv(data_view[1:min_num,],file = paste0(save_path,'/Top',min_num,'_',sub('/','',res_file),'.csv'),row.names = F)
  set.seed(1234)
  data_ori <- data
  which(is.na(data_ori[,2]))
  data <- rbind(data_ori[which(data_ori$V12 < 0.05),],
                data_ori[sample(which(data_ori$V12 >= 0.05 ),min(length(which(data_ori$V12 >= 0.05)),10000),replace = F),])
  #which(is.na(data_ori[which(data_ori$V12 < 0.05),][,2]))

  logp=-log10(data$V12) #value
  chr=data[,1] #chr
  position<-data[,2] #position
  #       xaxis=0
  sig=numeric()
  lab=numeric()
  flogp=numeric();
  the_col=c("darkblue","gray"); #chr class
  whole_col=c()
  xlabel=numeric();
  length_add=0
  label=numeric();
  for(i in 1:22){
    position[chr==i]->chr_pos
    chr_pos<-chr_pos-chr_pos[1]+17000000
    chr_num=length(chr_pos)
    cat("For chrosome",i,",Num of SNPs:",chr_num,"\n")
    if(length(chr_pos)==0){
      next
    }
    flogp=c(flogp,logp[chr==i])
    label=c(label,i)
    whole_col=c(whole_col,rep(the_col[i%%2+1],chr_num))
    chr_pos=length_add+chr_pos
    xlabel=c(xlabel,chr_pos)
    length_add=sort(chr_pos,decreasing=T)[1]
    lab=c(lab,(chr_pos[1]+length_add)/2)
  }
  print('plot...')
  png(paste0(save_file,'/manhatom.png'),height=500,width=1800)
  par(mar=c(5,6,4,2))
  plot(xlabel,flogp,col=whole_col,axes=F,xlab="",ylab="",ylim=c(0,12),pch=20,cex=0.5,cex.lab=1.2,cex.axis=1.4)#ylim=c(0,6)
  
  sig_pos<-xlabel[which(flogp> -log10(significance))]
  sig <- c()
  for(i in 1:length(sig_pos)){
    #  sig<-c(sig,which(xlabel>(sig_pos[i]-updown) & xlabel<(sig_pos[i]+updown)))
    sig<-c(sig,which(xlabel==sig_pos[i]))
  }
  sig<-unique(sig)
  cat("significant signal:",sig,"\n")
  points(xlabel[sig],flogp[sig],col="red",pch=20,cex=0.5)
  title(xlab="Chromosome",ylab=expression(-log[10]*(p)),cex.lab=1.4)
  #   title(xlab="Chromosome",ylab="Score",cex.lab=1.8)
  #       title("GWAS scan for Hair curl", cex.main=2.5)
  #       yaxis=seq(0,1,by=0.1)
  #       axis(2,yaxis,yaxis,las=1,cex.axis=1.5,line=-2)
  axis(2,las=2,cex.axis=1.4)
  #las can change the directory of the axis character
  #las=0 always parallel to the axis
  #las=2 always horizontal
  
  for(i in 1:22){
    mtext(label[i],side=1,at=lab[i],las=0,font=1,cex=0.8)
    #cex magnified relative to the to the default
  }
  #    mtext("X",side=1,at=x[23],las=0,font=1,cex=1.4)
  #    mtext("Y",side=1,at=x[24],las=0,font=1,cex=1.4)
  
  #       axis(1,x,as.character(label),tick=TRUE,font=1)
  par(lty=2)
  abline(h=-log10(significance),cex=1.5,col="red")
  dev.off()
  
  #plot the QQ plot
  png(paste0(save_file,'/QQ_sig.png'),height=400,width=600)
  #par(fig=c(0.58,0.92,0.43,0.95),new=TRUE)
  observed <- sort(data[,12])
  logp=-log10(observed)
  expected <- c(1:length(observed))
  lexp <- -(log10(expected / (length(expected)+1)))
  plot(x=lexp,y=logp,pch=19,cex=0.6, xlab="Expected (-logP)", ylab="Observed (-logP)",cex.lab=1.2,cex.axis=1.2)
  abline(0,1,col="red",lwd=2,lty=1)
  dev.off()
}
# plot_manhatton(save_file = 'proxy.png',
#                res_file = 'proxy_res'
#                )
# plot_manhatton(save_file = 'MCI.png',
#                res_file = 'MCI_res'
# )
# plot_manhatton(save_file = 'AD.png',
#                res_file = 'AD_res'
# )
