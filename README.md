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








