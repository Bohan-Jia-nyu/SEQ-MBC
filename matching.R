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

### 3-class

load("33_hypo18.rdata")
log_lik<-loo::extract_log_lik(fit_3classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)
datPred1<-broom.mixed::tidyMCMC(fit_3classes, rhat =T)
datPred1%>%filter(rhat>=1.01)
bayesplot::mcmc_trace(fit_3classes, pars = "beta1[1,4]", size = 0.5)
bayesplot::mcmc_trace(fit_3classes, pars = "mean1", size = 0.5)
dat_pred_par<-datPred1[-c(1:613),]
dat_pred_par2<-datPred1[c(1:43),]
log_prob<-data.frame(matrix(rep(0,570*3),nrow = 3,byrow = T))
for (i in 1:570) {
  c1p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1,"estimate"])
  c2p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*6,"estimate"])
  c3p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*12,"estimate"])
  
  for (j in (2+(i-1)*48):(i*48)) {
    
    if(test_pred_dat1[j,"coRes"]==test_pred_dat1[j-1,"coRes"]){
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"]+dat_pred_par2$estimate["rho[1]"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"]+dat_pred_par2$estimate["rho[2]"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"]+dat_pred_par2$estimate["rho[3]"])
      
    }
    else{
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"])
      
    }  
  }
  
  
  log_prob[1,i]<-log(dat_pred_par2$estimate["p[1]"])+c1p1
  log_prob[2,i]<-log(dat_pred_par2$estimate["p[2]"])+c2p1
  log_prob[3,i]<-log(dat_pred_par2$estimate["p[3]"])+c3p1
}

log_prob<-exp(log_prob)

col_sum_likelihood<-colSums(log_prob)
prob<-data.frame(matrix(rep(0,570*3),nrow = 3,byrow = T))
for(i in 1:570){
  for (j in 1:3) {
    prob[j,i]<-log_prob[j,i]/col_sum_likelihood[i]
  }
}

dat_classMember_prob_train33<-cbind(data.frame(dat%>%filter(id %in% id_training)),
                                    t(prob))
colnames(dat_classMember_prob_train33)<-c("id",c(1:48),"class1","class2","class3")
save(dat_classMember_prob_train33,file = "class_membership_train33.rdata")


load("33_hypo27.rdata")
log_lik<-loo::extract_log_lik(fit_3classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)
datPred1<-broom.mixed::tidyMCMC(fit_3classes, rhat =T)
datPred1%>%filter(rhat>=1.1)
dat_pred_par<-datPred1[-c(1:610),]
dat_pred_par2<-datPred1[c(1:40),]
log_prob_o<-data.frame(matrix(rep(0,570*3),nrow = 3,byrow = T))
for (i in 1:570) {
  c1p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1,"estimate"])
  c2p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*6,"estimate"])
  c3p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*12,"estimate"])
  
  for (j in (2+(i-1)*48):(i*48)) {
    
    if(test_pred_dat1[j,"coRes"]==test_pred_dat1[j-1,"coRes"]){
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"]+dat_pred_par2$estimate["rho[1]"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"]+dat_pred_par2$estimate["rho[2]"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"]+dat_pred_par2$estimate["rho[3]"])
      
    }
    else{
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"])
      
    }  
  }
  
  
  log_prob_o[1,i]<-log(dat_pred_par2$estimate["p[1]"])+c1p1
  log_prob_o[2,i]<-log(dat_pred_par2$estimate["p[2]"])+c2p1
  log_prob_o[3,i]<-log(dat_pred_par2$estimate["p[3]"])+c3p1}

log_prob_o<-exp(log_prob_o)

col_sum_likelihood_o<-colSums(log_prob_o)
prob_o<-data.frame(matrix(rep(0,570*3),nrow = 3,byrow = T))
for(i in 1:570){
  for (j in 1:3) {
    prob_o[j,i]<-log_prob_o[j,i]/col_sum_likelihood_o[i]
  }
}

prob<-t(prob)
prob<-data.frame(prob)
colnames(prob)<-c("c1","c2","c3")
prob<-prob%>%mutate(id=c(1:570))
prob<-prob%>%
  group_by(id)%>%
  mutate(class=case_when(c1==max(c1,c2,c3) ~ 1,
                         c2==max(c1,c2,c3) ~ 2,
                         c3==max(c1,c2,c3) ~ 3))%>%
    ungroup()%>%
  select(-c1,-c2,-c3)

prob_o<-t(prob_o)
prob_o<-data.frame(prob_o)
colnames(prob_o)<-c("c1","c2","c3")
prob_o<-prob_o%>%mutate(id=c(1:570))
prob_o<-prob_o%>%
  group_by(id)%>%
  mutate(class=case_when(c1==max(c1,c2,c3) ~ 1,
                         c2==max(c1,c2,c3) ~ 2,
                         c3==max(c1,c2,c3) ~ 3))%>%
  ungroup()%>%
  select(-c1,-c2,-c3)
table(prob_o$class)
table(prob_o$class,prob$class)
prob_o$class<-factor(prob_o$class,levels = c(1,3,2))
table(prob_o$class,prob$class)
prob_o$class<-factor(prob_o$class,levels = c(3,1,2))
table(prob_o$class,prob$class)
prob_o$class<-factor(prob_o$class,levels = c(1,3,2))
table(prob_o$class,prob$class)
prob_o$class<-factor(prob_o$class,levels = c(2,3,1))
table(prob_o$class,prob$class)
prob_o$class<-factor(prob_o$class,levels = c(2,1,3))
table(prob_o$class,prob$class)


#### 4

load("44_hypo19.rdata")
log_lik<-loo::extract_log_lik(fit_4classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)
datPred1<-broom.mixed::tidyMCMC(fit_4classes, rhat =T)
dat_pred_par<-datPred1[-c(1:627),]
dat_pred_par2<-datPred1[c(1:57),]
log_prob<-data.frame(matrix(rep(0,570*4),nrow = 4,byrow = T))
for (i in 1:570) {
  c1p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1,"estimate"])
  c2p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*6,"estimate"])
  c3p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*12,"estimate"])
  c4p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*18,"estimate"])
  
  for (j in (2+(i-1)*48):(i*48)) {
    
    if(test_pred_dat1[j,"coRes"]==test_pred_dat1[j-1,"coRes"]){
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"]+dat_pred_par2$estimate["rho[1]"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"]+dat_pred_par2$estimate["rho[2]"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"]+dat_pred_par2$estimate["rho[3]"])
      c4p1<-c4p1+
        log((1-dat_pred_par2$estimate["rho[4]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*18,"estimate"]+dat_pred_par2$estimate["rho[4]"])
      
    }
    else{
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"])
      c4p1<-c4p1+
        log((1-dat_pred_par2$estimate["rho[4]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*18,"estimate"])
      
    }  
  }
  
  
  log_prob[1,i]<-log(dat_pred_par2$estimate["p[1]"])+c1p1
  log_prob[2,i]<-log(dat_pred_par2$estimate["p[2]"])+c2p1
  log_prob[3,i]<-log(dat_pred_par2$estimate["p[3]"])+c3p1
  log_prob[4,i]<-log(dat_pred_par2$estimate["p[4]"])+c4p1
}

log_prob<-exp(log_prob)

col_sum_likelihood<-colSums(log_prob)
prob<-data.frame(matrix(rep(0,570*4),nrow = 4,byrow = T))
for(i in 1:570){
  for (j in 1:4) {
    prob[j,i]<-log_prob[j,i]/col_sum_likelihood[i]
  }
}

dat_classMember_prob_train44<-cbind(data.frame(dat%>%filter(id %in% id_training)),
                                    t(prob))
colnames(dat_classMember_prob_train44)<-c("id",c(1:48),"class1","class2","class3","class4")
save(dat_classMember_prob_train44,file = "class_membership_train44.rdata")

load("44_hypo21.rdata")
log_lik<-loo::extract_log_lik(fit_4classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)
datPred1<-broom.mixed::tidyMCMC(fit_4classes, rhat =T)
dat_pred_par<-datPred1[-c(1:623),]
dat_pred_par2<-datPred1[c(1:53),]
log_prob_o<-data.frame(matrix(rep(0,570*4),nrow = 4,byrow = T))
for (i in 1:570) {
  c1p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1,"estimate"])
  c2p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*6,"estimate"])
  c3p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*12,"estimate"])
  c4p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*18,"estimate"])
  
  for (j in (2+(i-1)*48):(i*48)) {
    
    if(test_pred_dat1[j,"coRes"]==test_pred_dat1[j-1,"coRes"]){
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"]+dat_pred_par2$estimate["rho[1]"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"]+dat_pred_par2$estimate["rho[2]"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"]+dat_pred_par2$estimate["rho[3]"])
      c4p1<-c4p1+
        log((1-dat_pred_par2$estimate["rho[4]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*18,"estimate"]+dat_pred_par2$estimate["rho[4]"])
      
    }
    else{
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"])
      c4p1<-c4p1+
        log((1-dat_pred_par2$estimate["rho[4]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*18,"estimate"])
      
    }  
  }
  
  
  log_prob_o[1,i]<-log(dat_pred_par2$estimate["p[1]"])+c1p1
  log_prob_o[2,i]<-log(dat_pred_par2$estimate["p[2]"])+c2p1
  log_prob_o[3,i]<-log(dat_pred_par2$estimate["p[3]"])+c3p1
  log_prob_o[4,i]<-log(dat_pred_par2$estimate["p[4]"])+c4p1
  }

log_prob_o<-exp(log_prob_o)

col_sum_likelihood_o<-colSums(log_prob_o)
prob_o<-data.frame(matrix(rep(0,570*4),nrow = 4,byrow = T))
for(i in 1:570){
  for (j in 1:4) {
    prob_o[j,i]<-log_prob_o[j,i]/col_sum_likelihood_o[i]
  }
}

prob<-t(prob)
prob<-data.frame(prob)
colnames(prob)<-c("c1","c2","c3","c4")
prob<-prob%>%mutate(id=c(1:570))
prob<-prob%>%
  group_by(id)%>%
  mutate(class=case_when(c1==max(c1,c2,c3,c4) ~ 1,
                         c2==max(c1,c2,c3,c4) ~ 2,
                         c3==max(c1,c2,c3,c4) ~ 3,
                         c4==max(c1,c2,c3,c4) ~ 4))%>%
  ungroup()%>%
  select(-c1,-c2,-c3,-c4)

prob_o<-t(prob_o)
prob_o<-data.frame(prob_o)
colnames(prob_o)<-c("c1","c2","c3","c4")
prob_o<-prob_o%>%mutate(id=c(1:570))
prob_o<-prob_o%>%
  group_by(id)%>%
  mutate(class=case_when(c1==max(c1,c2,c3,c4) ~ 1,
                         c2==max(c1,c2,c3,c4) ~ 2,
                         c3==max(c1,c2,c3,c4) ~ 3,
                         c4==max(c1,c2,c3,c4) ~ 4))%>%
  ungroup()%>%
  select(-c1,-c2,-c3,-c4)


table(prob_o$class,prob$class)
prob_o$class<-factor(prob_o$class,levels = c(2,1,3,4))
table(prob_o$class,prob$class)

###### 5
load("53_hypo16.rdata")
log_lik<-loo::extract_log_lik(fit_5classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)
datPred1<-broom.mixed::tidyMCMC(fit_5classes, rhat =T)
dat_pred_par<-datPred1[-c(1:637),]
dat_pred_par2<-datPred1[c(1:697),]
log_prob<-data.frame(matrix(rep(0,570*5),nrow = 5,byrow = T))
for (i in 1:570) {
  c1p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1,"estimate"])
  c2p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*6,"estimate"])
  c3p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*12,"estimate"])
  c4p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*18,"estimate"])
  c5p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*24,"estimate"])
  
  for (j in (2+(i-1)*48):(i*48)) {
    
    if(test_pred_dat1[j,"coRes"]==test_pred_dat1[j-1,"coRes"]){
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"]+dat_pred_par2$estimate["rho[1]"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"]+dat_pred_par2$estimate["rho[2]"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"]+dat_pred_par2$estimate["rho[3]"])
      c4p1<-c4p1+
        log((1-dat_pred_par2$estimate["rho[4]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*18,"estimate"]+dat_pred_par2$estimate["rho[4]"])
      c5p1<-c5p1+
        log((1-dat_pred_par2$estimate["rho[5]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*24,"estimate"]+dat_pred_par2$estimate["rho[5]"])
      
    }
    else{
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"])
      c4p1<-c4p1+
        log((1-dat_pred_par2$estimate["rho[4]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*18,"estimate"])
      c5p1<-c5p1+
        log((1-dat_pred_par2$estimate["rho[5]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*24,"estimate"])
      
    }  
  }
  
  
  log_prob[1,i]<-log(dat_pred_par2$estimate["p[1]"])+c1p1
  log_prob[2,i]<-log(dat_pred_par2$estimate["p[2]"])+c2p1
  log_prob[3,i]<-log(dat_pred_par2$estimate["p[3]"])+c3p1
  log_prob[4,i]<-log(dat_pred_par2$estimate["p[4]"])+c4p1
  log_prob[5,i]<-log(dat_pred_par2$estimate["p[5]"])+c5p1
}

log_prob<-exp(log_prob)

col_sum_likelihood<-colSums(log_prob)
prob<-data.frame(matrix(rep(0,570*5),nrow = 5,byrow = T))
for(i in 1:570){
  for (j in 1:5) {
    prob[j,i]<-log_prob[j,i]/col_sum_likelihood[i]
  }
}

dat_classMember_prob_train53<-cbind(data.frame(dat%>%filter(id %in% id_training)),
                            t(prob))
colnames(dat_classMember_prob_train53)<-c("id",c(1:48),"class1","class2","class3","class4","class5")
save(dat_classMember_prob_train53,file = "class_membership_train53.rdata")


load("54_hypo41.rdata")
log_lik<-loo::extract_log_lik(fit_5classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)
datPred1<-broom.mixed::tidyMCMC(fit_5classes, rhat =T)
dat_pred_par<-datPred1[-c(1:635),]
dat_pred_par2<-datPred1[c(1:65),]
log_prob_o<-data.frame(matrix(rep(0,570*5),nrow = 5,byrow = T))
for (i in 1:570) {
  c1p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1,"estimate"])
  c2p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*6,"estimate"])
  c3p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*12,"estimate"])
  c4p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*18,"estimate"])
  c5p1<-log(dat_pred_par[(test_pred_dat1[1+(i-1)*48,"coRes"]-1)*48+1+48*24,"estimate"])
  
  for (j in (2+(i-1)*48):(i*48)) {
    
    if(test_pred_dat1[j,"coRes"]==test_pred_dat1[j-1,"coRes"]){
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"]+dat_pred_par2$estimate["rho[1]"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"]+dat_pred_par2$estimate["rho[2]"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"]+dat_pred_par2$estimate["rho[3]"])
      c4p1<-c4p1+
        log((1-dat_pred_par2$estimate["rho[4]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*18,"estimate"]+dat_pred_par2$estimate["rho[4]"])
      c5p1<-c5p1+
        log((1-dat_pred_par2$estimate["rho[5]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*24,"estimate"]+dat_pred_par2$estimate["rho[5]"])
      
    }
    else{
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"])
      c4p1<-c4p1+
        log((1-dat_pred_par2$estimate["rho[4]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*18,"estimate"])
      c5p1<-c5p1+
        log((1-dat_pred_par2$estimate["rho[5]"])*dat_pred_par[(test_pred_dat1[j,"coRes"]-1)*48+(j-(i-1)*48)+48*24,"estimate"])
      
    }  
  }
  
  
  log_prob_o[1,i]<-log(dat_pred_par2$estimate["p[1]"])+c1p1
  log_prob_o[2,i]<-log(dat_pred_par2$estimate["p[2]"])+c2p1
  log_prob_o[3,i]<-log(dat_pred_par2$estimate["p[3]"])+c3p1
  log_prob_o[4,i]<-log(dat_pred_par2$estimate["p[4]"])+c4p1
  log_prob_o[5,i]<-log(dat_pred_par2$estimate["p[5]"])+c5p1
  
}

log_prob_o<-exp(log_prob_o)

col_sum_likelihood_o<-colSums(log_prob_o)
prob_o<-data.frame(matrix(rep(0,570*5),nrow = 5,byrow = T))
for(i in 1:570){
  for (j in 1:5) {
    prob_o[j,i]<-log_prob_o[j,i]/col_sum_likelihood_o[i]
  }
}

prob<-t(prob)
prob<-data.frame(prob)
colnames(prob)<-c("c1","c2","c3","c4","c5")
prob<-prob%>%mutate(id=c(1:570))
prob<-prob%>%
  group_by(id)%>%
  mutate(class=case_when(c1==max(c1,c2,c3,c4,c5) ~ 1,
                         c2==max(c1,c2,c3,c4,c5) ~ 2,
                         c3==max(c1,c2,c3,c4,c5) ~ 3,
                         c4==max(c1,c2,c3,c4,c5) ~ 4,
                         c5==max(c1,c2,c3,c4,c5) ~ 5))%>%
  ungroup()%>%
  select(-c1,-c2,-c3,-c4,-c5)

prob_o<-t(prob_o)
prob_o<-data.frame(prob_o)
colnames(prob_o)<-c("c1","c2","c3","c4","c5")
prob_o<-prob_o%>%mutate(id=c(1:570))
prob_o<-prob_o%>%
  group_by(id)%>%
  mutate(class=case_when(c1==max(c1,c2,c3,c4,c5) ~ 1,
                         c2==max(c1,c2,c3,c4,c5) ~ 2,
                         c3==max(c1,c2,c3,c4,c5) ~ 3,
                         c4==max(c1,c2,c3,c4,c5) ~ 4,
                         c5==max(c1,c2,c3,c4,c5) ~ 5))%>%
  ungroup()%>%
  select(-c1,-c2,-c3,-c4,-c5)

table(prob_o$class)
prob_o$class<-factor(prob_o$class,levels = c(1,2,3,4,5))
table(prob_o$class,prob$class)
prob_o$class<-factor(prob_o$class,levels = c(4,1,3,5,2))
table(prob_o$class,prob$class)
