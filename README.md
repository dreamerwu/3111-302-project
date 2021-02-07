# 3111-302-project


####Predict PD rate
install.packages ("survival")
library("survival")
data=read.delim("Binghao/input/BGB-3111_302/PFS_31AUG2020.txt",head=T,sep="\t")
data=read.delim("Binghao/input/BGB-3111_302/PFS_31Mar2020.txt",head=T,sep="\t")

ls(data)
surv_object=Surv(time=data$futime,event=data$fustat)
survdiff(Surv(time=data$futime,event=data$fustat)~Arms,data) #test pvalue (log-rank test)
fit=survfit(surv_object~Arms,data=data)
fit # show median survival time
summary(fit, times=12)#estimate survival at specific timepoint
plot(fit,mark=3,col=c("green","red","blue"),lty=c(1),lwd=1,conf.int=FALSE,ylab="PFS (investigator, 31AUG2020 cutoff)",xlab="months",cex.axis=1,cex.lab=1)
legend(2, .3, c("Cohort1 Zanu Arm (n=102)","Cohort1 Ibru Arm (n=99)","Cohort2 (n=28)","p=0.005"),col=c("green","red","blue"),lty=c(1),cex=0.8)
abline (v=10, col="gray")# add line


#calculate and plot Odds ratio for MRR/VGPR
data=read.delim("Binghao/input/BGB_3111_302/Oddsratio_MYD88WT.txt",head=T,sep="\t")
output=matrix(ncol=3,nrow=nrow(data)/2)
j=1
for (i in seq(1,nrow(data),by=2)) {
  #input=matrix(c(data[i,2],data[i+1,2],data[i,3],data[i+1,3]),nrow=2,byrow=F) #for MRR
  input=matrix(c(data[i,4],data[i+1,4],data[i,5],data[i+1,5]),nrow=2,byrow=F) #for VGPR
Oddsratio=((input[1,1])/(input[2,1]))/((input[1,2])/(input[2,2])) #calculate oddratio
pvalue=fisher.test(input) #calculate fisher exact test
output[j,1]=pvalue$p.value
output[j,2]=Oddsratio
j=j+1
}
write.csv(output,"Binghao/output.csv")







