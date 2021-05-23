library(survival)
data(ovarian)
#~1 is a required grouping parameter. if no groups, should be set to 1
res_surv<-survfit(Surv(time = ovarian$futime, event = ovarian$fustat)~1) 
summary(res_surv)
# basic kaplan-meier
plot(res_surv)
#with censoring events and no dotted lines
plot(res_surv, mark.time=T, conf.int=F) 

# set groups to treatment
res_surv_2<-survfit(Surv(ovarian$futime, ovarian$fustat)~ovarian$rx)
plot(res_surv_2, mark.time=T, conf.int=F)
# colored by treatment
plot(res_surv_2, mark.time=T, conf.int=F, col=c(1,2), lty=c(1,2))
# colored with legend
legend("bottomleft",legend=c("rx = 1","rx = 2"),col=c(1,2),text.col=c(1,2),lty=c(1,2),cex=2)
 
# Nelson Aaelen estimator
h_hat<-res_surv$n.event/res_surv$n.risk
H_hat<-cumsum(h_hat)
res_NA=cbind(res_surv$time , res_surv$n.event , res_surv$n.risk , 
             h_hat, H_hat , res_surv$surv , exp(-H_hat) )
colnames(res_NA)=c("time","N_event","N_risk",
                   "hazard","Cumulative Hazard","KM Estimator","NA Estimator")
res_NA[res_surv$n.event==1,]

# log-rank test
res_log_rank_1=survdiff(Surv(ovarian$futime,ovarian$fustat) ~ ovarian$rx)
names(res_log_rank_1)
res_log_rank_1
1-pchisq(res_log_rank_1$chisq,1)

# cox model
install.packages("carData")
library(carData)
data_surv=Rossi[,c(1:10)]
# dot means you're putting all the covariates in the model
res_Cox=coxph(Surv(week , arrest) ~ .,data=data_surv)
res_Cox
summary(res_Cox)

