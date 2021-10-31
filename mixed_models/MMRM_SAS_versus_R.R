##########################################################################################
### Load packages
##########################################################################################
library(haven)
library(nlme)
library(lme4)
library(emmeans)
library(pbkrtest)
library(lmerTest)

##########################################################################################
### Import data and subset
##########################################################################################
path<-"D:/working group/"
adlbh<-read_xpt(paste0(path,"adlbh.xpt"))
adlbh<-as.data.frame(adlbh)
adlbh<-subset(adlbh,!is.na(BASE) & !(AVISITN %in% c(NA,0,99)))
adlbh$AVISITN<-as.factor(adlbh$AVISITN)

##########################################################################################
### Run MMRM analyses
##########################################################################################
all_param<-unique(adlbh$PARAMCD)
### Unstructured
k<-1
for(i in 1:length(all_param)){
  adlbh_1<-subset(adlbh,PARAMCD==all_param[i])
  mod1<-tryCatch(gls(model = CHG ~ TRTP + AVISITN + TRTP:AVISITN + AVISITN + BASE,
                   data = adlbh_1,
                   correlation = corSymm(form = ~1|SUBJID),
                   weights = varIdent(form = ~1|AVISITN),
                   control = glsControl(opt = "optim"),
                   method = "REML",
                   na.action = "na.omit"),
                   error = function(x) return(NULL))
  if(!(is.null(mod1))){
    mod1<-emmeans(mod1, ~TRTP|AVISITN, mode="df.error")
    mod1<-emmeans::contrast(mod1, method="pairwise", adjust=NULL)
    mod1<-as.data.frame(confint(mod1))
    mod1$PARAMCD<-all_param[i]
    mod1$corr<-"corSymm"
    if(k==1){final<-mod1}
    if(k>1){final<-rbind(final,mod1)}
    k<-k+1
  }
  print(i)
}

### Compound symmetry
for(i in 1:length(all_param)){
  adlbh_1<-subset(adlbh,PARAMCD==all_param[i])
  mod1<-tryCatch(gls(model = CHG ~ TRTP + AVISITN + TRTP:AVISITN + AVISITN + BASE,
                   data = adlbh_1,
                   correlation = corCompSymm(form= ~ 1 | SUBJID),
                   weights = varIdent(form = ~1|AVISITN),
                   control = glsControl(opt = "optim"),
                   method = "REML",
                   na.action = "na.omit"),
                   error = function(x) return(NULL))
  if(!(is.null(mod1))){
    mod1<-emmeans(mod1, ~TRTP|AVISITN, mode="df.error")
    mod1<-emmeans::contrast(mod1, method="pairwise", adjust=NULL)
    mod1<-as.data.frame(confint(mod1))
    mod1$PARAMCD<-all_param[i]
    mod1$corr<-"corCompSymm"
    final<-rbind(final,mod1)
  }
  print(i)
}

### AR(1)
for(i in 1:length(all_param)){
  adlbh_1<-subset(adlbh,PARAMCD==all_param[i])
  mod1<-tryCatch(gls(model = CHG ~ TRTP + AVISITN + TRTP:AVISITN + AVISITN + BASE,
                   data = adlbh_1,
                   correlation = corCAR1(form= ~ 1 | SUBJID),
                   weights = varIdent(form = ~1|AVISITN),
                   control = glsControl(opt = "optim"),
                   method = "REML",
                   na.action = "na.omit"),
                   error = function(x) return(NULL))
  if(!(is.null(mod1))){
    mod1<-emmeans(mod1, ~TRTP|AVISITN, mode="df.error")
    mod1<-emmeans::contrast(mod1, method="pairwise", adjust=NULL)
    mod1<-as.data.frame(confint(mod1))
    mod1$PARAMCD<-all_param[i]
    mod1$corr<-"corCAR1"
    final<-rbind(final,mod1)
  }
  print(i)
}


write.csv(final, paste0(path,"pair_diff.csv"), row.names = FALSE)

##########################################################################################
### Compare results from SAS and R
##########################################################################################
path1<-"D:/working group/results_first/"
from_R<-final
from_SAS<-read.csv(paste0(path1,"LSMeansDifferences_ANCOVAwithRepeatedMeasures_KR_ADLBH_GithubData.csv"))

### Data manipulation of the SAS output 
from_SAS$contrast<-paste(from_SAS$reftrtp,"-",from_SAS$TRTP)
from_SAS$contrast[from_SAS$contrast=="Xanomeline High Dose - Xanomeline Low Dose"]<-"Xanomeline Low Dose - Xanomeline High Dose"
from_SAS$VISIT<-toupper(from_SAS$AVISIT)
from_SAS$corr<-from_SAS$covariatestructure
from_SAS$corr[from_SAS$corr=="Heterogeneous First Order Autoregressive"]<-"corCAR1"
from_SAS$corr[from_SAS$corr=="Compound Symmetry"]<-"corCompSymm"
from_SAS$corr[from_SAS$corr=="Unstructured"]<-"corSymm"
from_SAS_1<-subset(from_SAS,corr %in% c("corCAR1","corCompSymm","corSymm"),select=c("PARAMCD","corr","contrast","VISIT","lsmeans_difference","lowerci95","upperci95"))

### Data manipulation of the R output 
from_R$VISIT<-paste("WEEK",from_R$AVISITN)
from_R$upper<-from_R$upper.CL
from_R$lower<-from_R$lower.CL
for(i in 1:dim(from_R)[1]){
  if(grepl("Placebo",from_R$contrast[i])){
    from_R$estimate[i]<-(-1)*from_R$estimate[i]
    from_R$lower[i]<-(-1)*from_R$upper.CL[i]
    from_R$upper[i]<-(-1)*from_R$lower.CL[i]
  }
}

from_R_1<-subset(from_R,select=c("PARAMCD","corr","contrast","VISIT","estimate","lower","upper"))

### Merge with output from R (by paramcd, visit, correlation structure and contrast)
temp01<-merge(from_SAS_1,from_R_1,by=c("PARAMCD","corr","contrast","VISIT"))

### Compute some summary statistics for estimates of treatment effect
summary(temp01$lsmeans_difference)
summary(temp01$estimate)
aggregate(lsmeans_difference~corr,data=temp01,FUN=mean)
aggregate(estimate~corr,data=temp01,FUN=mean)
aggregate(lsmeans_difference~VISIT,data=temp01,FUN=mean)
aggregate(estimate~VISIT,data=temp01,FUN=mean)

### Plot estimates against each other
par(cex=1.3)
plot(temp01$lsmeans_difference,temp01$estimate,xlab="From SAS",ylab="From R",main="Estimates of treatment effect")
lines(c(-10,15),c(-10,15),lty=2)
points_to_plot<-subset(temp01,corr=="corCAR1")
points(points_to_plot$lsmeans_difference,points_to_plot$estimate,pch=19,col="red")
points_to_plot<-subset(temp01,corr=="corCompSymm")
points(points_to_plot$lsmeans_difference,points_to_plot$estimate,pch=19,col="green")
points_to_plot<-subset(temp01,corr=="corSymm")
points(points_to_plot$lsmeans_difference,points_to_plot$estimate,pch=19,col="blue")
legend("bottomright",c("AR(1)","Compound Symmetry","Unstructured"),col=c("red","green","blue"),pch=19)

### Find width of 95% CI and compute summary statistics
temp01$width_CI_SAS<-temp01$upperci95-temp01$lowerci95
temp01$width_CI_R<-temp01$upper-temp01$lower
summary(temp01$width_CI_SAS)
summary(temp01$width_CI_R)
aggregate(width_CI_SAS~corr,data=temp01,FUN=mean)
aggregate(width_CI_R~corr,data=temp01,FUN=mean)
aggregate(width_CI_SAS~VISIT,data=temp01,FUN=mean)
aggregate(width_CI_R~VISIT,data=temp01,FUN=mean)

### Plot width of 95% CI against each other
par(cex=1.3)
plot(c(0,50),c(0,50),type="l",lty=2,xlab="From SAS",ylab="From R",main="Width of 95% CI")
points(temp01$width_CI_SAS,temp01$width_CI_R)
points_to_plot<-subset(temp01,corr=="corCAR1")
points(points_to_plot$width_CI_SAS,points_to_plot$width_CI_R,pch=19,col="red")
points_to_plot<-subset(temp01,corr=="corCompSymm")
points(points_to_plot$width_CI_SAS,points_to_plot$width_CI_R,pch=19,col="green")
points_to_plot<-subset(temp01,corr=="corSymm")
points(points_to_plot$width_CI_SAS,points_to_plot$width_CI_R,pch=19,col="blue")
legend("bottomright",c("AR(1)","Compound Symmetry","Unstructured"),col=c("red","green","blue"),pch=19)

#################################################################################################
### Try the approach suggested by Roche 
### See https://www.linkedin.com/pulse/mmrm-r-presented-rpharma-daniel-saban%C3%A9s-bov%C3%A9/
#################################################################################################

### Loop through all lab parameters
all_param<-unique(from_R$PARAMCD)
for(i in 1:length(all_param)){
  adlbh_1<-subset(adlbh,PARAMCD==all_param[i])
  mod1<-lmer(CHG ~ TRTP + AVISITN + TRTP:AVISITN + BASE + (0 + AVISITN | SUBJID),data=adlbh_1, control=lmerControl(check.nobs.vs.nRE="ignore"), na.action=na.omit)
  mod2<-emmeans(mod1, ~TRTP|AVISITN ,lmer.df = "asymptotic")
  mod3<-emmeans::contrast(mod2, method="pairwise", adjust=NULL)
  mod4<-as.data.frame(confint(mod3))
  mod4$PARAMCD<-all_param[i]
  if(i==1){final<-mod4}
  if(i>1){final<-rbind(final,mod4)}
  print(i)
}

### Do a bit of data manipulation
final$upper<-final$asymp.UCL
final$lower<-final$asymp.LCL
for(i in 1:dim(final)[1]){
  final$estimate[i]<-(-1)*final$estimate[i]
  final$lower[i]<-(-1)*final$asymp.UCL[i]
  final$upper[i]<-(-1)*final$asymp.LCL[i]
}

final$VISIT<-paste("WEEK",final$AVISITN)

final$contrast<-as.character(final$contrast)
final$contrast[final$contrast=="Xanomeline High Dose - Xanomeline Low Dose"]<-"Xanomeline Low Dose - Xanomeline High Dose"

final_1<-subset(final,select=c("PARAMCD","contrast","VISIT","estimate","lower","upper"))

### Export data
write.csv(final_1, paste0(path,"roche_diff.csv"), row.names = FALSE)


### Compare with SAS
from_SAS_2<-subset(from_SAS_1,corr=="corSymm")
temp02<-merge(from_SAS_2,final_1,by=c("PARAMCD","contrast","VISIT"))

### Compute some summary statistics for estimates of treatment effect
summary(temp02$lsmeans_difference)
summary(temp02$estimate)

### Plot estimates against each other
par(cex=1.3)
plot(temp02$lsmeans_difference,temp02$estimate,xlab="From SAS",ylab="From R (Roche approach)",main="Estimates of treatment effect")
lines(c(-10,15),c(-10,15),lty=2)

### Find width of 95% CI and compute summary statistics
temp02$width_CI_SAS<-temp02$upperci95-temp02$lowerci95
temp02$width_CI_R<-temp02$upper-temp02$lower
summary(temp02$width_CI_SAS)
summary(temp02$width_CI_R)

### Plot width of 95% CI against each other
par(cex=1.3)
plot(c(0,40),c(0,40),type="l",lty=2,xlab="From SAS",ylab="From R (Roche approach)",main="Width of 95% CI")
points(temp02$width_CI_SAS,temp02$width_CI_R)



