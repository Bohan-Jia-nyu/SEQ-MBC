library(tidyverse)
library(TraMineR)
#create data considering linear term
data(mvad)
mvad[1:10,15:86]
sapply(mvad[1:10,15:86],as.numeric)
table(sapply(mvad[,15:86],as.numeric))
dat<-mvad[,-c(2:14)]
dat<-sapply(dat,as.numeric)
dat<-data.frame(dat)
names(dat)<-c("id",c(1:72))
outcome<-dat[,c(1,62:73)]
dat<-dat[,-c(50:73)]
dat1 <- reshape::melt(dat,id=c("id"),variable_name="time")
dat1$time <- as.integer(dat1$time) 
colnames(dat1)[3] <- "coRes"
newdat1 <- dat1[with(dat1, order(id, time)), ]
test_dat1<-newdat1
table(test_dat1$coRes)

outcome1 <- reshape::melt(outcome,id=c("id"),variable_name="time")
outcome1$time <- as.integer(outcome1$time) 
colnames(outcome1)[3] <- "coRes"
outcome2 <- outcome1[with(outcome1, order(id, time)), ]
outcome2<-outcome2

outcome3<-outcome2%>%
  mutate(working=ifelse(coRes==3,1,0))%>%
  group_by(id)%>%
  summarise(total_work=sum(working))%>%
  mutate(work_more=ifelse(total_work>6,1,0))

load("id_training_201.rdata")
test_pred_dat1<-test_dat1%>%
  filter(id %in% id_training)
test_pred_dat2<-test_dat1%>%
  filter(!(id %in% id_training))
outcome3_pred_dat1<-outcome3%>%
  filter(id %in% id_training)
outcome3_pred_dat2<-outcome3%>%
  filter(!(id %in% id_training))


load("32_hypo1.rdata")
dat3<-broom.mixed::tidyMCMC(fit_3classes,rhat = T)
dat3%>%
  filter(rhat>1.1)
log_lik<-loo::extract_log_lik(fit_3classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)
bayesplot::mcmc_trace(fit_3classes,"log_lik[103]")
bayesplot::mcmc_trace(fit_3classes,"mean2")
dat3new<-dat3[-c(1:615),]
C<-cbind(estimate=dat3new$estimate,
         t=c(rep(c(1:48),18)),
         c=rep(c(1,2,3),each=48*6),
         state=rep(rep(c(1:6),each=48),3))
C<-data.frame(C)
C$c<-as.factor(C$c)
C$state<-as.factor(C$state)
C<-C%>%
  mutate(State=case_when(state=="1"~"school",
                         state=="2"~"FE",
                         state=="3"~"employment",
                         state=="4"~"training",
                         state=="5"~"jobless",
                         state=="6"~"HE"))%>%
  mutate(c=case_when(c==1~ "Class1",
                     c==2~ "Class2",
                     c==3~ "Class3",
                     c==4~ "Class4"))
p<-ggplot(data = C,aes(x=t,y=estimate,color=State,group=State))+
  geom_point()+geom_smooth()+facet_wrap(~c,ncol=2)
p


######new plot
list_draw<-rstan::extract(fit_3classes)
draws<-rep(0,nrow(C))
for (i in 1961:2000) {
  one_draw<-rbind(matrix(as.vector(list_draw$invMNlogit_c1[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c2[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c3[i,,]),ncol = 1))
  draws<-cbind(draws,one_draw)
  
}
draws<-draws[,-1]
draws<-data.frame(draws)
C<-cbind(draws,
         t=c(rep(c(1:48),18)),
         c=rep(c(1,2,3),each=48*6),
         state=rep(rep(c(1:6),each=48),3))

C$c<-as.factor(C$c)
C$state<-as.factor(C$state)

C<-C%>%
  mutate(State=case_when(state=="1"~"school",
                         state=="2"~"FE",
                         state=="3"~"employment",
                         state=="4"~"training",
                         state=="5"~"jobless",
                         state=="6"~"HE"))
#semi
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat3[dat3$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat3[dat3$term=="p[1]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat3[dat3$term=="p_class1[1]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_class1[2]","estimate"],2)),
                                  '); o=',as.numeric(round(dat3[dat3$term=="mean1","estimate"]*dat3[dat3$term=="p_class1[1]","estimate"]+
                                                             dat3[dat3$term=="mean2","estimate"]*dat3[dat3$term=="p_class1[2]","estimate"]
                                                           ,1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat3[dat3$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat3[dat3$term=="p[2]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat3[dat3$term=="p_class2[1]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_class2[2]","estimate"],2)),
                                  '); o=',as.numeric(round(dat3[dat3$term=="mean1","estimate"]*dat3[dat3$term=="p_class2[1]","estimate"]+
                                                             dat3[dat3$term=="mean2","estimate"]*dat3[dat3$term=="p_class2[2]","estimate"]
                                                           ,1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat3[dat3$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat3[dat3$term=="p[3]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat3[dat3$term=="p_class3[1]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_class3[2]","estimate"],2)),
                                  '); o=',as.numeric(round(dat3[dat3$term=="mean1","estimate"]*dat3[dat3$term=="p_class3[1]","estimate"]+
                                                             dat3[dat3$term=="mean2","estimate"]*dat3[dat3$term=="p_class3[2]","estimate"]
                                                           ,1)))))


class.plot<-unique(C$c)
C.plot.1<-C%>%filter(c==class.plot[1])
p1<-ggplot(data=C.plot.1,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p1<-p1+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p1<-p1+ggtitle(paste0(class.plot[1]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p1

C.plot.2<-C%>%filter(c==class.plot[2])
p2<-ggplot(data=C.plot.2,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p2<-p2+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p2<-p2+ggtitle(paste0(class.plot[2]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p2

C.plot.3<-C%>%filter(c==class.plot[3])
p3<-ggplot(data=C.plot.3,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p3<-p3+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p3<-p3+ggtitle(paste0(class.plot[3]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p3


data_outcome.plot<-dat3[37:38,2]
data_outcome.plot<-dat3[c(37,39,38),2]
data_outcome.plot<-data_outcome.plot[order(data_outcome.plot$estimate),]
data_outcome.plot<-cbind(data_outcome.plot,outcome=c('mu1','mu2'))
p_outcome<-ggplot(data = data_outcome.plot)+geom_point(aes(x=outcome,y=estimate))+
  geom_segment(aes(x=outcome,y=0,yend=estimate))+geom_text(aes(x=outcome,y=estimate,
                                                               label=round(estimate,2)),
                                                           hjust= +1.2, size = 3)+
  ylab("")
p_outcome
ggpubr::ggarrange(p1,p3,p2,p_outcome,
                  common.legend = TRUE,
                  ncol = 2, nrow = 2,legend = "bottom"
)


#####
load("33_hypo27.rdata")
dat3<-broom.mixed::tidyMCMC(fit_3classes,rhat = T)
dat3%>%
  filter(rhat>1.1)

log_lik<-loo::extract_log_lik(fit_3classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)
bayesplot::mcmc_trace(fit_3classes,"log_lik[103]")
bayesplot::mcmc_trace(fit_3classes,"mean2")
dat3new<-dat3[-c(1:610),]
C<-cbind(estimate=dat3new$estimate,
         t=c(rep(c(1:48),18)),
         c=rep(c(1,3,2),each=48*6),
         state=rep(rep(c(1:6),each=48),3))
C<-data.frame(C)
C$c<-as.factor(C$c)
C$state<-as.factor(C$state)
C<-C%>%
  mutate(State=case_when(state=="1"~"school",
                         state=="2"~"FE",
                         state=="3"~"employment",
                         state=="4"~"training",
                         state=="5"~"jobless",
                         state=="6"~"HE"))%>%
  mutate(c=case_when(c==1~ "Class1",
                     c==2~ "Class2",
                     c==3~ "Class3",
                     c==4~ "Class4"))
p<-ggplot(data = C,aes(x=t,y=estimate,color=State,group=State))+
  geom_point()+geom_smooth()+facet_wrap(~c,ncol=2)
p


#############new plot
#############new plot
list_draw<-rstan::extract(fit_3classes)
draws<-rep(0,nrow(C))
for (i in 1961:2000) {
  one_draw<-rbind(matrix(as.vector(list_draw$invMNlogit_c1[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c2[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c3[i,,]),ncol = 1))
  draws<-cbind(draws,one_draw)
  
}
draws<-draws[,-1]
draws<-data.frame(draws)
C<-cbind(draws,
         t=c(rep(c(1:48),18)),
         c=rep(c(1,3,2),each=48*6),
         state=rep(rep(c(1:6),each=48),3))

C$c<-as.factor(C$c)
C$state<-as.factor(C$state)

C<-C%>%
  mutate(State=case_when(state=="1"~"school",
                         state=="2"~"FE",
                         state=="3"~"employment",
                         state=="4"~"training",
                         state=="5"~"jobless",
                         state=="6"~"HE"))
#semi
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat3[dat3$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat3[dat3$term=="p[1]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat3[dat3$term=="p_class1[2]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_class1[1]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_class1[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat3[dat3$term=="mean1","estimate"]*dat3[dat3$term=="p_class1[1]","estimate"]+
                                                             dat3[dat3$term=="mean2","estimate"]*dat3[dat3$term=="p_class1[2]","estimate"]+
                                                             dat3[dat3$term=="mean3","estimate"]*dat3[dat3$term=="p_class1[3]","estimate"]
                                                           ,1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat3[dat3$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat3[dat3$term=="p[3]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat3[dat3$term=="p_class3[2]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_class3[1]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_class3[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat3[dat3$term=="mean1","estimate"]*dat3[dat3$term=="p_class3[1]","estimate"]+
                                                             dat3[dat3$term=="mean2","estimate"]*dat3[dat3$term=="p_class3[2]","estimate"]+
                                                             dat3[dat3$term=="mean3","estimate"]*dat3[dat3$term=="p_class3[3]","estimate"]
                                                           ,1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat3[dat3$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat3[dat3$term=="p[2]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat3[dat3$term=="p_class2[2]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_class2[1]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_class2[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat3[dat3$term=="mean1","estimate"]*dat3[dat3$term=="p_class2[1]","estimate"]+
                                                             dat3[dat3$term=="mean2","estimate"]*dat3[dat3$term=="p_class2[2]","estimate"]+
                                                             dat3[dat3$term=="mean3","estimate"]*dat3[dat3$term=="p_class2[3]","estimate"]
                                                           ,1)))))




# concord
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat3[dat3$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat3[dat3$term=="p[1]","estimate"],2)),
                                  '\n p.g=c(1, 0, 0); o=',as.numeric(round(dat3[dat3$term=="mean1","estimate"],1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat3[dat3$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat3[dat3$term=="p[3]","estimate"],2)),
                                  '\n p.g=c(0, 1, 0); o=',as.numeric(round(dat3[dat3$term=="mean3","estimate"],1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat3[dat3$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat3[dat3$term=="p[2]","estimate"],2)),
                                  '\n p.g=c(0, 0,1); o=',as.numeric(round(dat3[dat3$term=="mean2","estimate"],1)))))
# indep
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat3[dat3$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat3[dat3$term=="p[1]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat3[dat3$term=="p_outcome[1]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_outcome[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat3[dat3$term=="mean1","estimate"]*dat3[dat3$term=="p_outcome[1]","estimate"]+
                                                             dat3[dat3$term=="mean2","estimate"]*dat3[dat3$term=="p_outcome[2]","estimate"]+
                                                             dat3[dat3$term=="mean3","estimate"]*dat3[dat3$term=="p_outcome[3]","estimate"]
                                                           ,1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat3[dat3$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat3[dat3$term=="p[2]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat3[dat3$term=="p_outcome[1]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_outcome[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat3[dat3$term=="mean1","estimate"]*dat3[dat3$term=="p_outcome[1]","estimate"]+
                                                             dat3[dat3$term=="mean2","estimate"]*dat3[dat3$term=="p_outcome[2]","estimate"]+
                                                             dat3[dat3$term=="mean3","estimate"]*dat3[dat3$term=="p_outcome[3]","estimate"]
                                                           ,1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat3[dat3$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat3[dat3$term=="p[3]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat3[dat3$term=="p_outcome[1]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat3[dat3$term=="p_outcome[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat3[dat3$term=="mean1","estimate"]*dat3[dat3$term=="p_outcome[1]","estimate"]+
                                                             dat3[dat3$term=="mean2","estimate"]*dat3[dat3$term=="p_outcome[2]","estimate"]+
                                                             dat3[dat3$term=="mean3","estimate"]*dat3[dat3$term=="p_outcome[3]","estimate"]
                                                           ,1)))))

p<-ggplot(data = C,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+facet_wrap(~c,ncol=2)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p<-p+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p
class.plot<-unique(C$c)
C.plot.1<-C%>%filter(c==class.plot[1])
p1<-ggplot(data=C.plot.1,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p1<-p1+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p1<-p1+ggtitle(paste0(class.plot[1]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p1

C.plot.2<-C%>%filter(c==class.plot[2])
p2<-ggplot(data=C.plot.2,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p2<-p2+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p2<-p2+ggtitle(paste0(class.plot[2]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p2

C.plot.3<-C%>%filter(c==class.plot[3])
p3<-ggplot(data=C.plot.3,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p3<-p3+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p3<-p3+ggtitle(paste0(class.plot[3]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p3


data_outcome.plot<-dat3[37:39,2]
data_outcome.plot<-dat3[c(37,39,38),2]
data_outcome.plot<-data_outcome.plot[order(data_outcome.plot$estimate),]
data_outcome.plot<-cbind(data_outcome.plot,outcome=c('mu1','mu2','mu3'))
p_outcome<-ggplot(data = data_outcome.plot)+geom_point(aes(x=outcome,y=estimate))+
  geom_segment(aes(x=outcome,y=0,yend=estimate))+geom_text(aes(x=outcome,y=estimate,
                                                               label=round(estimate,2)),
                                                           hjust= +1.2, size = 3)+
  ylab("")
p_outcome
ggpubr::ggarrange(p1,p3,p2,p_outcome,
                  common.legend = TRUE,
                  ncol = 2, nrow = 2,legend = "bottom"
)


#####
load("43_hypo1.rdata")
dat4<-broom.mixed::tidyMCMC(fit_4classes,rhat = T)
dat4%>%
  filter(rhat>1.1)
bayesplot::mcmc_trace(fit_4classes,"mean2")
log_lik<-loo::extract_log_lik(fit_4classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)

dat4new<-dat4[-c(1:634),]
C<-cbind(estimate=dat4new$estimate,
         t=c(rep(c(1:48),24)),
         c=rep(c(1,3,4,2),each=48*6),
         state=rep(rep(c(1:6),each=48),4))
C<-data.frame(C)
C$c<-as.factor(C$c)
C$state<-as.factor(C$state)
C<-C%>%
  mutate(State=case_when(state=="1"~"school",
                         state=="2"~"FE",
                         state=="3"~"employment",
                         state=="4"~"training",
                         state=="5"~"jobless",
                         state=="6"~"HE"))%>%
  mutate(c=case_when(c==1~ "Class1",
                     c==2~ "Class2",
                     c==3~ "Class3",
                     c==4~ "Class4"))
p<-ggplot(data = C,aes(x=t,y=estimate,color=State,group=State))+
  geom_point()+geom_smooth()+facet_wrap(~c,ncol=2)
p


#############new plot
list_draw<-rstan::extract(fit_4classes)
draws<-rep(0,nrow(C))
for (i in 1961:2000) {
  one_draw<-rbind(matrix(as.vector(list_draw$invMNlogit_c1[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c2[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c3[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c4[i,,]),ncol = 1))
  draws<-cbind(draws,one_draw)
  
}
draws<-draws[,-1]
draws<-data.frame(draws)
C<-cbind(draws,
         t=c(rep(c(1:48),24)),
         c=rep(c(1,3,4,2),each=48*6),
         state=rep(rep(c(1:6),each=48),4))

C$c<-as.factor(C$c)
C$state<-as.factor(C$state)

C<-C%>%
  mutate(State=case_when(state=="1"~"school",
                         state=="2"~"FE",
                         state=="3"~"employment",
                         state=="4"~"training",
                         state=="5"~"jobless",
                         state=="6"~"HE"))
#semi
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[1]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat4[dat4$term=="p_class1[2]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class1[3]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class1[1]","estimate"],2)),
                                  '); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"]*dat4[dat4$term=="p_class1[1]","estimate"]+
                                                             dat4[dat4$term=="mean2","estimate"]*dat4[dat4$term=="p_class1[2]","estimate"]+
                                                             dat4[dat4$term=="mean3","estimate"]*dat4[dat4$term=="p_class1[3]","estimate"]
                                                           ,1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[4]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[4]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat4[dat4$term=="p_class4[2]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class4[3]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class4[1]","estimate"],2)),
                                  '); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"]*dat4[dat4$term=="p_class4[1]","estimate"]+
                                                             dat4[dat4$term=="mean2","estimate"]*dat4[dat4$term=="p_class4[2]","estimate"]+
                                                             dat4[dat4$term=="mean3","estimate"]*dat4[dat4$term=="p_class4[3]","estimate"]
                                                           ,1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[2]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat4[dat4$term=="p_class2[2]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class2[3]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class2[1]","estimate"],2)),
                                  '); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"]*dat4[dat4$term=="p_class2[1]","estimate"]+
                                                             dat4[dat4$term=="mean2","estimate"]*dat4[dat4$term=="p_class2[2]","estimate"]+
                                                             dat4[dat4$term=="mean3","estimate"]*dat4[dat4$term=="p_class2[3]","estimate"]
                                                           ,1))),
                     c==4~ paste0('Class 4: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[3]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat4[dat4$term=="p_class3[2]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class3[3]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class3[1]","estimate"],2)),
                                  '); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"]*dat4[dat4$term=="p_class3[1]","estimate"]+
                                                             dat4[dat4$term=="mean2","estimate"]*dat4[dat4$term=="p_class3[2]","estimate"]+
                                                             dat4[dat4$term=="mean3","estimate"]*dat4[dat4$term=="p_class3[3]","estimate"]
                                                           ,1)))))





class.plot<-unique(C$c)
C.plot.1<-C%>%filter(c==class.plot[1])
p1<-ggplot(data=C.plot.1,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p1<-p1+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p1<-p1+ggtitle(paste0(class.plot[1]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p1

C.plot.2<-C%>%filter(c==class.plot[2])
p2<-ggplot(data=C.plot.2,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p2<-p2+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p2<-p2+ggtitle(paste0(class.plot[2]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p2

C.plot.3<-C%>%filter(c==class.plot[3])
p3<-ggplot(data=C.plot.3,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p3<-p3+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p3<-p3+ggtitle(paste0(class.plot[3]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p3

C.plot.4<-C%>%filter(c==class.plot[4])
p4<-ggplot(data=C.plot.4,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p4<-p4+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p4<-p4+ggtitle(paste0(class.plot[4]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p4


data_outcome.plot<-dat4[49:51,2]
data_outcome.plot<-dat4[c(50,49,51,52),2]
data_outcome.plot<-data_outcome.plot[order(data_outcome.plot$estimate),]
data_outcome.plot<-cbind(data_outcome.plot,outcome=c('mu1','mu2','mu3'))
p_outcome<-ggplot(data = data_outcome.plot)+geom_point(aes(x=outcome,y=estimate))+
  geom_segment(aes(x=outcome,y=0,yend=estimate))+geom_text(aes(x=outcome,y=estimate,
                                                               label=round(estimate,2)),
                                                           hjust= +1.2, size = 3)+
  ylab("")
p_outcome
ggpubr::ggarrange(p1,p4,p2,p3,p_outcome,
                  common.legend = TRUE,
                  ncol = 2, nrow = 3,legend = "bottom"
)

#####
load("44_hypo21.rdata")
dat4<-broom.mixed::tidyMCMC(fit_4classes,rhat = T)
dat4%>%
  filter(rhat>1.1)
bayesplot::mcmc_trace(fit_4classes,"mean2")
log_lik<-loo::extract_log_lik(fit_4classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)

dat4new<-dat4[-c(1:623),]
C<-cbind(estimate=dat4new$estimate,
         t=c(rep(c(1:48),24)),
         c=rep(c(1,2,3,4),each=48*6),
         state=rep(rep(c(1:6),each=48),4))
C<-data.frame(C)
C$c<-as.factor(C$c)
C$state<-as.factor(C$state)
C<-C%>%
  mutate(State=case_when(state=="1"~"school",
                         state=="2"~"FE",
                         state=="3"~"employment",
                         state=="4"~"training",
                         state=="5"~"jobless",
                         state=="6"~"HE"))%>%
  mutate(c=case_when(c==1~ "Class1",
                     c==2~ "Class2",
                     c==3~ "Class3",
                     c==4~ "Class4"))
p<-ggplot(data = C,aes(x=t,y=estimate,color=State,group=State))+
  geom_point()+geom_smooth()+facet_wrap(~c,ncol=2)
p


#############new plot
list_draw<-rstan::extract(fit_4classes)
draws<-rep(0,nrow(C))
for (i in 1961:2000) {
  one_draw<-rbind(matrix(as.vector(list_draw$invMNlogit_c1[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c2[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c3[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c4[i,,]),ncol = 1))
  draws<-cbind(draws,one_draw)
  
}
draws<-draws[,-1]
draws<-data.frame(draws)
C<-cbind(draws,
         t=c(rep(c(1:48),24)),
         c=rep(c(2,1,3,4),each=48*6),
         state=rep(rep(c(1:6),each=48),4))

C$c<-as.factor(C$c)
C$state<-as.factor(C$state)

C<-C%>%
  mutate(State=case_when(state=="1"~"school",
                         state=="2"~"FE",
                         state=="3"~"employment",
                         state=="4"~"training",
                         state=="5"~"jobless",
                         state=="6"~"HE"))
#semi
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[1]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat4[dat4$term=="p_class1[4]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class1[2]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class1[1]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class1[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"]*dat4[dat4$term=="p_class1[1]","estimate"]+
                                                             dat4[dat4$term=="mean2","estimate"]*dat4[dat4$term=="p_class1[2]","estimate"]+
                                                             dat4[dat4$term=="mean3","estimate"]*dat4[dat4$term=="p_class1[3]","estimate"]+
                                                             dat4[dat4$term=="mean4","estimate"]*dat4[dat4$term=="p_class1[4]","estimate"]
                                                           ,1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[2]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat4[dat4$term=="p_class2[4]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class2[2]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class2[1]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class2[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"]*dat4[dat4$term=="p_class2[1]","estimate"]+
                                                             dat4[dat4$term=="mean2","estimate"]*dat4[dat4$term=="p_class2[2]","estimate"]+
                                                             dat4[dat4$term=="mean3","estimate"]*dat4[dat4$term=="p_class2[3]","estimate"]+
                                                             dat4[dat4$term=="mean4","estimate"]*dat4[dat4$term=="p_class2[4]","estimate"]
                                                           ,1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[3]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat4[dat4$term=="p_class3[4]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class3[2]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class3[1]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class3[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"]*dat4[dat4$term=="p_class3[1]","estimate"]+
                                                             dat4[dat4$term=="mean2","estimate"]*dat4[dat4$term=="p_class3[2]","estimate"]+
                                                             dat4[dat4$term=="mean3","estimate"]*dat4[dat4$term=="p_class3[3]","estimate"]+
                                                             dat4[dat4$term=="mean4","estimate"]*dat4[dat4$term=="p_class3[4]","estimate"]
                                                           ,1))),
                     c==4~ paste0('Class 4: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[4]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[4]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat4[dat4$term=="p_class4[4]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class4[2]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class4[1]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_class4[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"]*dat4[dat4$term=="p_class4[1]","estimate"]+
                                                             dat4[dat4$term=="mean2","estimate"]*dat4[dat4$term=="p_class4[2]","estimate"]+
                                                             dat4[dat4$term=="mean3","estimate"]*dat4[dat4$term=="p_class4[3]","estimate"]+
                                                             dat4[dat4$term=="mean4","estimate"]*dat4[dat4$term=="p_class4[4]","estimate"]
                                                           ,1)))))




# concord
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[2]","estimate"],2)),
                                  '\n p.g=c(1, 0, 0, 0); o=',as.numeric(round(dat4[dat4$term=="mean2","estimate"],1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[1]","estimate"],2)),
                                  '\n p.g=c(0, 1, 0, 0); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"],1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[3]","estimate"],2)),
                                  '\n p.g=c(0, 0, 1, 0); o=',as.numeric(round(dat4[dat4$term=="mean3","estimate"],1))),
                     c==4~ paste0('Class 4: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[4]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[4]","estimate"],2)),
                                  '\n p.g=c(0, 0, 0, 1); o=',as.numeric(round(dat4[dat4$term=="mean4","estimate"],1)))))
# indep
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[1]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat4[dat4$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_outcome[1]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_outcome[3]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_outcome[4]","estimate"],2)),
                                  '); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"]*dat4[dat4$term=="p_outcome[1]","estimate"]+
                                                             dat4[dat4$term=="mean2","estimate"]*dat4[dat4$term=="p_outcome[2]","estimate"]+
                                                             dat4[dat4$term=="mean3","estimate"]*dat4[dat4$term=="p_outcome[3]","estimate"]+
                                                             dat4[dat4$term=="mean4","estimate"]*dat4[dat4$term=="p_outcome[4]","estimate"]
                                                           ,1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[2]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat4[dat4$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_outcome[1]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_outcome[3]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_outcome[4]","estimate"],2)),
                                  '); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"]*dat4[dat4$term=="p_outcome[1]","estimate"]+
                                                             dat4[dat4$term=="mean2","estimate"]*dat4[dat4$term=="p_outcome[2]","estimate"]+
                                                             dat4[dat4$term=="mean3","estimate"]*dat4[dat4$term=="p_outcome[3]","estimate"]+
                                                             dat4[dat4$term=="mean4","estimate"]*dat4[dat4$term=="p_outcome[4]","estimate"]
                                                           ,1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[3]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat4[dat4$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_outcome[1]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_outcome[3]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_outcome[4]","estimate"],2)),
                                  '); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"]*dat4[dat4$term=="p_outcome[1]","estimate"]+
                                                             dat4[dat4$term=="mean2","estimate"]*dat4[dat4$term=="p_outcome[2]","estimate"]+
                                                             dat4[dat4$term=="mean3","estimate"]*dat4[dat4$term=="p_outcome[3]","estimate"]+
                                                             dat4[dat4$term=="mean4","estimate"]*dat4[dat4$term=="p_outcome[4]","estimate"]
                                                           ,1))),
                     c==4~ paste0('Class 4: ',' (rho=',as.numeric(round(dat4[dat4$term=="rho[4]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat4[dat4$term=="p[4]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat4[dat4$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_outcome[1]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_outcome[3]","estimate"],2)),", ",
                                  as.numeric(round(dat4[dat4$term=="p_outcome[4]","estimate"],2)),
                                  '); o=',as.numeric(round(dat4[dat4$term=="mean1","estimate"]*dat4[dat4$term=="p_outcome[1]","estimate"]+
                                                             dat4[dat4$term=="mean2","estimate"]*dat4[dat4$term=="p_outcome[2]","estimate"]+
                                                             dat4[dat4$term=="mean3","estimate"]*dat4[dat4$term=="p_outcome[3]","estimate"]+
                                                             dat4[dat4$term=="mean4","estimate"]*dat4[dat4$term=="p_outcome[4]","estimate"]
                                                           ,1)))))

p<-ggplot(data = C,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+facet_wrap(~c,ncol=2)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p<-p+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p
class.plot<-unique(C$c)
C.plot.1<-C%>%filter(c==class.plot[1])
p1<-ggplot(data=C.plot.1,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p1<-p1+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p1<-p1+ggtitle(paste0(class.plot[1]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p1

C.plot.2<-C%>%filter(c==class.plot[2])
p2<-ggplot(data=C.plot.2,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p2<-p2+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p2<-p2+ggtitle(paste0(class.plot[2]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p2

C.plot.3<-C%>%filter(c==class.plot[3])
p3<-ggplot(data=C.plot.3,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p3<-p3+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p3<-p3+ggtitle(paste0(class.plot[3]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p3

C.plot.4<-C%>%filter(c==class.plot[4])
p4<-ggplot(data=C.plot.4,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p4<-p4+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p4<-p4+ggtitle(paste0(class.plot[4]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p4


data_outcome.plot<-dat4[49:52,2]
data_outcome.plot<-dat4[c(50,49,51,52),2]
data_outcome.plot<-data_outcome.plot[order(data_outcome.plot$estimate),]
data_outcome.plot<-cbind(data_outcome.plot,outcome=c('mu1','mu2','mu3',
                                                     'mu4'))
p_outcome<-ggplot(data = data_outcome.plot)+geom_point(aes(x=outcome,y=estimate))+
  geom_segment(aes(x=outcome,y=0,yend=estimate))+geom_text(aes(x=outcome,y=estimate,
                                                               label=round(estimate,2)),
                                                           hjust= +1.2, size = 3)+
  ylab("")
p_outcome
ggpubr::ggarrange(p2,p1,p3,p4,p_outcome,
                  common.legend = TRUE,
                  ncol = 2, nrow = 3,legend = "bottom"
)
#####
for (i in 31:45) {
  load( paste0("55_hypo",i,".rdata"))
  dat5<-broom.mixed::tidyMCMC(fit_5classes,rhat = T)
  a<-dat5[1:66,]%>%
    filter(rhat>1.05)
  if(nrow(a)==0){
  print(i)
  log_lik<-loo::extract_log_lik(fit_5classes, parameter_name = "log_lik", merge_chains = TRUE)
  b<-loo::waic(log_lik)
  print(b[["estimates"]][3,1])}
}
load("55_hypo45.rdata")
dat5<-broom.mixed::tidyMCMC(fit_5classes,rhat = T)
dat5%>%
  filter(rhat>1.1)
bayesplot::mcmc_trace(fit_5classes, pars = "beta1[1,4]", size = 0.5)
log_lik<-loo::extract_log_lik(fit_5classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)
dat5new<-dat5[-c(1:661),]
C<-cbind(estimate=dat5new$estimate,
         t=c(rep(c(1:48),30)),
         c=rep(c(2,3,1,5,4),each=48*6),
         state=rep(rep(c(1:6),each=48),5))
C<-data.frame(C)
C$c<-as.factor(C$c)
C$state<-as.factor(C$state)
C<-C%>%
  mutate(State=case_when(state=="1"~"school",
                         state=="2"~"FE",
                         state=="3"~"employment",
                         state=="4"~"training",
                         state=="5"~"jobless",
                         state=="6"~"HE"))%>%
  mutate(c=case_when(c==1~ "Class1",
                     c==2~ "Class2",
                     c==3~ "Class3",
                     c==4~ "Class4",
                     c==5~ "Class5"))
p<-ggplot(data = C,aes(x=t,y=estimate,color=State,group=State))+
  geom_point()+geom_smooth()+facet_wrap(~c,ncol=2)
p

#############new plot
list_draw<-rstan::extract(fit_5classes)
draws<-rep(0,nrow(C))
for (i in 1961:2000) {
  one_draw<-rbind(matrix(as.vector(list_draw$invMNlogit_c1[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c2[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c3[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c4[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c5[i,,]),ncol = 1))
  draws<-cbind(draws,one_draw)
  
}
draws<-draws[,-1]
draws<-data.frame(draws)
C<-cbind(draws,
         t=c(rep(c(1:48),30)),
         c=rep(c(2,3,1,5,4),each=48*6),
         state=rep(rep(c(1:6),each=48),5))

C$c<-as.factor(C$c)
C$state<-as.factor(C$state)

C<-C%>%
  mutate(State=case_when(state=="1"~"school",
                         state=="2"~"FE",
                         state=="3"~"employment",
                         state=="4"~"training",
                         state=="5"~"jobless",
                         state=="6"~"HE"))
#semi
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[3]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_class3[5]","estimate"],2)),", ",
                                          as.numeric(round(dat5[dat5$term=="p_class3[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class3[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class3[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class3[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_class3[1]","estimate"]+
                                                           dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_class3[2]","estimate"]+
                                                           dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_class3[3]","estimate"]+
                                                           dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_class3[4]","estimate"]+
                                                           dat5[dat5$term=="mean5","estimate"]*dat5[dat5$term=="p_class3[5]","estimate"]
                                                           ,1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[1]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_class1[5]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class1[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class1[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class1[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class1[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_class1[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_class1[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_class1[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_class1[4]","estimate"]+
                                                             dat5[dat5$term=="mean5","estimate"]*dat5[dat5$term=="p_class1[5]","estimate"]
                                                           ,1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[2]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_class2[5]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class2[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class2[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class2[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class2[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_class2[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_class2[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_class2[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_class2[4]","estimate"]+
                                                             dat5[dat5$term=="mean5","estimate"]*dat5[dat5$term=="p_class2[5]","estimate"]
                                                           ,1))),
                     c==4~ paste0('Class 4: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[5]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[5]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_class5[5]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class5[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class5[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class5[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class5[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_class5[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_class5[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_class5[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_class5[4]","estimate"]+
                                                             dat5[dat5$term=="mean5","estimate"]*dat5[dat5$term=="p_class5[5]","estimate"]
                                                           ,1))),
                     c==5~ paste0('Class 5: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[4]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[4]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_class4[5]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class4[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class4[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class4[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class4[3]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_class4[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_class4[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_class4[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_class4[4]","estimate"]+
                                                             dat5[dat5$term=="mean5","estimate"]*dat5[dat5$term=="p_class4[5]","estimate"]
                                                           ,1)))))




# concord
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[5]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[5]","estimate"],2)),
                                  '\n p.g=c(1, 0, 0, 0, 0); o=',as.numeric(round(dat5[dat5$term=="mean5","estimate"],1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[1]","estimate"],2)),
                                  '\n p.g=c(0, 1, 0, 0, 0); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"],1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[3]","estimate"],2)),
                                  '\n p.g=c(0, 0, 1, 0, 0); o=',as.numeric(round(dat5[dat5$term=="mean3","estimate"],1))),
                     c==4~ paste0('Class 4: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[4]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[4]","estimate"],2)),
                                  '\n p.g=c(0, 0, 0, 1, 0); o=',as.numeric(round(dat5[dat5$term=="mean4","estimate"],1))),
                     c==5~ paste0('Class 5: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[2]","estimate"],2)),
                                  '\n p.g=c(0, 0, 0, 0, 1); o=',as.numeric(round(dat5[dat5$term=="mean2","estimate"],1)))))
# indep
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[1]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_outcome[5]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[4]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_outcome[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_outcome[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_outcome[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_outcome[4]","estimate"]+
                                                             dat5[dat5$term=="mean5","estimate"]*dat5[dat5$term=="p_outcome[5]","estimate"]
                                                           ,1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[2]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_outcome[5]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[4]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_outcome[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_outcome[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_outcome[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_outcome[4]","estimate"]+
                                                             dat5[dat5$term=="mean5","estimate"]*dat5[dat5$term=="p_outcome[5]","estimate"]
                                                           ,1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[3]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_outcome[5]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[4]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_outcome[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_outcome[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_outcome[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_outcome[4]","estimate"]+
                                                             dat5[dat5$term=="mean5","estimate"]*dat5[dat5$term=="p_outcome[5]","estimate"]
                                                           ,1))),
                     c==4~ paste0('Class 4: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[4]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[4]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_outcome[5]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[4]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_outcome[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_outcome[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_outcome[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_outcome[4]","estimate"]+
                                                             dat5[dat5$term=="mean5","estimate"]*dat5[dat5$term=="p_outcome[5]","estimate"]
                                                           ,1))),
                     c==5~paste0('Class 5: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[5]","estimate"],2)),
                                 '); p.f=',as.numeric(round(dat5[dat5$term=="p[5]","estimate"],2)),
                                 '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_outcome[5]","estimate"],2)),", ",
                                 as.numeric(round(dat5[dat5$term=="p_outcome[1]","estimate"],2)),", ",
                                 as.numeric(round(dat5[dat5$term=="p_outcome[3]","estimate"],2)),", ",
                                 as.numeric(round(dat5[dat5$term=="p_outcome[2]","estimate"],2)),", ",
                                 as.numeric(round(dat5[dat5$term=="p_outcome[4]","estimate"],2)),
                                 '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_outcome[1]","estimate"]+
                                                            dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_outcome[2]","estimate"]+
                                                            dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_outcome[3]","estimate"]+
                                                            dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_outcome[4]","estimate"]+
                                                            dat5[dat5$term=="mean5","estimate"]*dat5[dat5$term=="p_outcome[5]","estimate"]
                                                          ,1)))))

p<-ggplot(data = C,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+facet_wrap(~c,ncol=2)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p<-p+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p
class.plot<-unique(C$c)
C.plot.1<-C%>%filter(c==class.plot[1])
p1<-ggplot(data=C.plot.1,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p1<-p1+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p1<-p1+ggtitle(paste0(class.plot[1]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p1

C.plot.2<-C%>%filter(c==class.plot[2])
p2<-ggplot(data=C.plot.2,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p2<-p2+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p2<-p2+ggtitle(paste0(class.plot[2]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p2

C.plot.3<-C%>%filter(c==class.plot[3])
p3<-ggplot(data=C.plot.3,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p3<-p3+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p3<-p3+ggtitle(paste0(class.plot[3]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p3

C.plot.4<-C%>%filter(c==class.plot[4])
p4<-ggplot(data=C.plot.4,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p4<-p4+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p4<-p4+ggtitle(paste0(class.plot[4]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p4

C.plot.5<-C%>%filter(c==class.plot[5])
p5<-ggplot(data=C.plot.5,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p5<-p5+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p5<-p5+ggtitle(paste0(class.plot[5]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p5

data_outcome.plot<-dat5[61:65,2]
data_outcome.plot<-dat5[c(65,61,63,64,62),2]
data_outcome.plot<-data_outcome.plot[order(data_outcome.plot$estimate),]
data_outcome.plot<-cbind(data_outcome.plot,outcome=c('mu1','mu2','mu3',
                                                     'mu4','mu5'))
p_outcome<-ggplot(data = data_outcome.plot)+geom_point(aes(x=outcome,y=estimate))+
  geom_segment(aes(x=outcome,y=0,yend=estimate))+geom_text(aes(x=outcome,y=estimate,
                                                               label=round(estimate,2)),
                                                           hjust= +1.2, size = 3)+
  ylab("")
p_outcome
ggpubr::ggarrange(p3,p1,p2,p5,p4,p_outcome,
          common.legend = TRUE,
          ncol = 2, nrow = 3,legend = "bottom"
          )
##### 53 semi
load("54_hypo41.rdata")
dat5<-broom.mixed::tidyMCMC(fit_5classes,rhat = T)
dat5%>%
  filter(rhat>1.1)
bayesplot::mcmc_trace(fit_5classes, pars = "beta4[2,3]", size = 0.5)
log_lik<-loo::extract_log_lik(fit_5classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)
dat5new<-dat5[-c(1:635),]
C<-cbind(estimate=dat5new$estimate,
         t=c(rep(c(1:48),30)),
         c=rep(c(2,5,3,1,4),each=48*6),
         state=rep(rep(c(1:6),each=48),5))
C<-data.frame(C)
C$c<-as.factor(C$c)
C$state<-as.factor(C$state)
C<-C%>%
  mutate(State=case_when(state=="1"~"school",
                         state=="2"~"FE",
                         state=="3"~"employment",
                         state=="4"~"training",
                         state=="5"~"jobless",
                         state=="6"~"HE"))%>%
  mutate(c=case_when(c==1~ "Class1",
                     c==2~ "Class2",
                     c==3~ "Class3",
                     c==4~ "Class4",
                     c==5~ "Class5"))
p<-ggplot(data = C,aes(x=t,y=estimate,color=State,group=State))+
  geom_point()+geom_smooth()+facet_wrap(~c,ncol=2)
p

#############new plot
list_draw<-rstan::extract(fit_5classes)
draws<-rep(0,nrow(C))
for (i in 1961:2000) {
  one_draw<-rbind(matrix(as.vector(list_draw$invMNlogit_c1[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c2[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c3[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c4[i,,]),ncol = 1),
                  matrix(as.vector(list_draw$invMNlogit_c5[i,,]),ncol = 1))
  draws<-cbind(draws,one_draw)
  
}
draws<-draws[,-1]
draws<-data.frame(draws)
C<-cbind(draws,
         t=c(rep(c(1:48),30)),
         c=rep(c(2,5,3,1,4),each=48*6),
         state=rep(rep(c(1:6),each=48),5))

C$c<-as.factor(C$c)
C$state<-as.factor(C$state)

C<-C%>%
  mutate(State=case_when(state=="1"~"school",
                         state=="2"~"FE",
                         state=="3"~"employment",
                         state=="4"~"training",
                         state=="5"~"jobless",
                         state=="6"~"HE"))
#semi
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[5]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[5]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_class5[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class5[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class5[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class5[2]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_class5[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_class5[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_class5[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_class5[4]","estimate"]
                                                           ,1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[3]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_class3[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class3[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class3[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class3[2]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_class3[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_class3[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_class3[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_class3[4]","estimate"]
                                                           ,1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[4]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[4]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_class4[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class4[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class4[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class4[2]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_class4[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_class4[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_class4[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_class4[4]","estimate"]
                                                           ,1))),
                     c==4~ paste0('Class 4: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[2]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_class2[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class2[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class2[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class2[2]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_class2[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_class2[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_class2[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_class2[4]","estimate"]
                                                           ,1))),
                     c==5~ paste0('Class 5: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[2]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_class2[1]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class1[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class1[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_class1[2]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_class1[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_class1[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_class1[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_class1[4]","estimate"]
                                                           ,1)))))




# concord
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[4]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[4]","estimate"],2)),
                                  '\n p.g=c(1, 0, 0, 0); o=',as.numeric(round(dat5[dat5$term=="mean4","estimate"],1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[1]","estimate"],2)),
                                  '\n p.g=c(0, 1, 0, 0); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"],1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[3]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[3]","estimate"],2)),
                                  '\n p.g=c(0, 0, 1, 0); o=',as.numeric(round(dat5[dat5$term=="mean3","estimate"],1))),
                     c==4~ paste0('Class 4: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[5]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[5]","estimate"],2)),
                                  '\n p.g=c(1, 0, 0, 0); o=',as.numeric(round(dat5[dat5$term=="mean4","estimate"],1))),
                     c==5~ paste0('Class 5: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[2]","estimate"],2)),
                                  '\n p.g=c(0, 0, 0, 1); o=',as.numeric(round(dat5[dat5$term=="mean2","estimate"],1)))))
# indep
C<-C%>%
  mutate(c=case_when(c==1~ paste0('Class 1: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[5]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[5]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_outcome[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[1]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_outcome[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_outcome[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_outcome[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_outcome[4]","estimate"]
                                                           ,1))),
                     c==2~ paste0('Class 2: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[2]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[2]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_outcome[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[1]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_outcome[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_outcome[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_outcome[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_outcome[4]","estimate"]
                                                           ,1))),
                     c==3~ paste0('Class 3: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[1]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[1]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_outcome[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[1]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_outcome[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_outcome[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_outcome[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_outcome[4]","estimate"]
                                                           ,1))),
                     c==4~ paste0('Class 4: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[4]","estimate"],2)),
                                  '); p.f=',as.numeric(round(dat5[dat5$term=="p[4]","estimate"],2)),
                                  '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_outcome[4]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[2]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[3]","estimate"],2)),", ",
                                  as.numeric(round(dat5[dat5$term=="p_outcome[1]","estimate"],2)),
                                  '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_outcome[1]","estimate"]+
                                                             dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_outcome[2]","estimate"]+
                                                             dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_outcome[3]","estimate"]+
                                                             dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_outcome[4]","estimate"]
                                                           ,1))),
                     c==5~paste0('Class 5: ',' (rho=',as.numeric(round(dat5[dat5$term=="rho[3]","estimate"],2)),
                                 '); p.f=',as.numeric(round(dat5[dat5$term=="p[3]","estimate"],2)),
                                 '\n p.g=(',as.numeric(round(dat5[dat5$term=="p_outcome[4]","estimate"],2)),", ",
                                 as.numeric(round(dat5[dat5$term=="p_outcome[2]","estimate"],2)),", ",
                                 as.numeric(round(dat5[dat5$term=="p_outcome[3]","estimate"],2)),", ",
                                 as.numeric(round(dat5[dat5$term=="p_outcome[1]","estimate"],2)),
                                 '); o=',as.numeric(round(dat5[dat5$term=="mean1","estimate"]*dat5[dat5$term=="p_outcome[1]","estimate"]+
                                                            dat5[dat5$term=="mean2","estimate"]*dat5[dat5$term=="p_outcome[2]","estimate"]+
                                                            dat5[dat5$term=="mean3","estimate"]*dat5[dat5$term=="p_outcome[3]","estimate"]+
                                                            dat5[dat5$term=="mean4","estimate"]*dat5[dat5$term=="p_outcome[4]","estimate"]
                                                          ,1)))))

p<-ggplot(data = C,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+facet_wrap(~c,ncol=2)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p<-p+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p
class.plot<-unique(C$c)
C.plot.1<-C%>%filter(c==class.plot[1])
p1<-ggplot(data=C.plot.1,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p1<-p1+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p1<-p1+ggtitle(paste0(class.plot[1]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p1

C.plot.2<-C%>%filter(c==class.plot[2])
p2<-ggplot(data=C.plot.2,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p2<-p2+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p2<-p2+ggtitle(paste0(class.plot[2]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p2

C.plot.3<-C%>%filter(c==class.plot[3])
p3<-ggplot(data=C.plot.3,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p3<-p3+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p3<-p3+ggtitle(paste0(class.plot[3]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p3

C.plot.4<-C%>%filter(c==class.plot[4])
p4<-ggplot(data=C.plot.4,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p4<-p4+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p4<-p4+ggtitle(paste0(class.plot[4]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p4

C.plot.5<-C%>%filter(c==class.plot[5])
p5<-ggplot(data=C.plot.5,aes(x=t,y=V1,color=State,group=State))+geom_smooth(se=F)+ylab("Prob")+xlab("Time")+theme(legend.position = "bottom")+guides(color = guide_legend(nrow = 1))+
  theme(legend.direction = "horizontal")

for (i in 2:40) {
  p5<-p5+geom_smooth(aes_string(x="t",y=paste0(c("V"),i),color="State",group="State"),se=F)
}

p5<-p5+ggtitle(paste0(class.plot[5]))+
  theme(plot.title = element_text(hjust = 0.5,size = 10))
p5

data_outcome.plot<-dat5[61:64,2]
data_outcome.plot<-dat5[c(64,61,63,64,62),2]
data_outcome.plot<-data_outcome.plot[order(data_outcome.plot$estimate),]
data_outcome.plot<-cbind(data_outcome.plot,outcome=c('mu1','mu2','mu3', "mu1",
                                                     'mu4'))
p_outcome<-ggplot(data = data_outcome.plot)+geom_point(aes(x=outcome,y=estimate))+
  geom_segment(aes(x=outcome,y=0,yend=estimate))+geom_text(aes(x=outcome,y=estimate,
                                                               label=round(estimate,2)),
                                                           hjust= +1.2, size = 3)+
  ylab("")
p_outcome
ggpubr::ggarrange(p4,p1,p3,p5,p2,p_outcome,
                  common.legend = TRUE,
                  ncol = 2, nrow = 3,legend = "bottom"
)
