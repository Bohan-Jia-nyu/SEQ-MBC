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
####1phase3class
load("33_hypo27.rdata")
log_lik<-loo::extract_log_lik(fit_3classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)
datPred1<-broom.mixed::tidyMCMC(fit_3classes, rhat =T)
datPred1%>%
  filter(rhat>1.1)
bayesplot::mcmc_trace(fit_3classes, pars = "p[2]", size = 0.5)
dat_pred_par<-datPred1[-c(1:610),]
dat_pred_par2<-datPred1[c(1:40),]
###
log_prob<-data.frame(matrix(rep(0,142*3),nrow = 3,byrow = T))
for (i in 1:142) {
  c1p1<-log(dat_pred_par[(test_pred_dat2[1+(i-1)*48,"coRes"]-1)*48+1,"estimate"])
  c2p1<-log(dat_pred_par[(test_pred_dat2[1+(i-1)*48,"coRes"]-1)*48+1+48*6,"estimate"])
  c3p1<-log(dat_pred_par[(test_pred_dat2[1+(i-1)*48,"coRes"]-1)*48+1+48*12,"estimate"])
  
  for (j in (2+(i-1)*48):(i*48)) {
    
    if(test_pred_dat2[j,"coRes"]==test_pred_dat2[j-1,"coRes"]){
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"]+dat_pred_par2$estimate["rho[1]"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"]+dat_pred_par2$estimate["rho[2]"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"]+dat_pred_par2$estimate["rho[3]"])
      
    }
    else{
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"])
      
    }  
  }
  
  
  log_prob[1,i]<-log(dat_pred_par2$estimate["p[1]"])+c1p1
  log_prob[2,i]<-log(dat_pred_par2$estimate["p[2]"])+c2p1
  log_prob[3,i]<-log(dat_pred_par2$estimate["p[3]"])+c3p1
}

log_prob<-exp(log_prob)

col_sum_likelihood<-colSums(log_prob)
prob<-data.frame(matrix(rep(0,142*3),nrow = 3,byrow = T))
for(i in 1:142){
  for (j in 1:3) {
    prob[j,i]<-log_prob[j,i]/col_sum_likelihood[i]
  }
}


##rmse
estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"],
      dat_pred_par2$estimate["mean3"]))%*%
  as.matrix(prob)

estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"]))%*%
  matrix(c(dat_pred_par2$estimate["p_class1[1]"],dat_pred_par2$estimate["p_class1[2]"],
           dat_pred_par2$estimate["p_class2[1]"],dat_pred_par2$estimate["p_class2[2]"],
           dat_pred_par2$estimate["p_class3[1]"],dat_pred_par2$estimate["p_class3[2]"]),
         byrow = FALSE,ncol=3)%*%
  as.matrix(prob)

estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"],
    dat_pred_par2$estimate["mean3"]))%*%
  matrix(c(dat_pred_par2$estimate["p_class1[1]"],dat_pred_par2$estimate["p_class1[2]"],dat_pred_par2$estimate["p_class1[3]"],
           dat_pred_par2$estimate["p_class2[1]"],dat_pred_par2$estimate["p_class2[2]"],dat_pred_par2$estimate["p_class2[3]"],
           dat_pred_par2$estimate["p_class3[1]"],dat_pred_par2$estimate["p_class3[2]"],dat_pred_par2$estimate["p_class3[3]"]),
         byrow = FALSE,ncol=3)%*%
  as.matrix(prob)

estimated_total_work<-
  c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"],
      dat_pred_par2$estimate["mean3"])%*%
  matrix(c(dat_pred_par2$estimate["p_outcome[1]"],dat_pred_par2$estimate["p_outcome[2]"],dat_pred_par2$estimate["p_outcome[3]"]),
         nrow=3)

sqrt(sum((rep(estimated_total_work,142) - outcome3_pred_dat2$total_work)^2)/142)

####indep weighted RMSE

sqrt(( sum((rep(dat_pred_par2$estimate["mean1"],142)-outcome3_pred_dat2$total_work)^2)*
  dat_pred_par2$estimate["p_outcome[1]"]+
sum((rep(dat_pred_par2$estimate["mean2"],142)-outcome3_pred_dat2$total_work)^2)*
  dat_pred_par2$estimate["p_outcome[2]"]+
sum((rep(dat_pred_par2$estimate["mean3"],142)-outcome3_pred_dat2$total_work)^2)*
  dat_pred_par2$estimate["p_outcome[3]"] )/142)

sqrt(sum((estimated_total_work - outcome3_pred_dat2$total_work)^2)/142)
####1phase4class
load("44_hypo21.rdata")
log_lik<-loo::extract_log_lik(fit_4classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)

datPred1<-broom.mixed::tidyMCMC(fit_4classes, rhat =T)
dat_pred_par<-datPred1[-c(1:623),]
dat_pred_par2<-datPred1[c(1:53),]
###
log_prob<-data.frame(matrix(rep(0,142*4),nrow = 4,byrow = T))
for (i in 1:142) {
  c1p1<-log(dat_pred_par[(test_pred_dat2[1+(i-1)*48,"coRes"]-1)*48+1,"estimate"])
  c2p1<-log(dat_pred_par[(test_pred_dat2[1+(i-1)*48,"coRes"]-1)*48+1+48*6,"estimate"])
  c3p1<-log(dat_pred_par[(test_pred_dat2[1+(i-1)*48,"coRes"]-1)*48+1+48*12,"estimate"])
  c4p1<-log(dat_pred_par[(test_pred_dat2[1+(i-1)*48,"coRes"]-1)*48+1+48*18,"estimate"])
  
  for (j in (2+(i-1)*48):(i*48)) {
    
    if(test_pred_dat2[j,"coRes"]==test_pred_dat2[j-1,"coRes"]){
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"]+dat_pred_par2$estimate["rho[1]"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"]+dat_pred_par2$estimate["rho[2]"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"]+dat_pred_par2$estimate["rho[3]"])
      c4p1<-c4p1+
        log((1-dat_pred_par2$estimate["rho[4]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*18,"estimate"]+dat_pred_par2$estimate["rho[4]"])
      
    }
    else{
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"])
      c4p1<-c4p1+
        log((1-dat_pred_par2$estimate["rho[4]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*18,"estimate"])
      
    }  
  }
  
  
  log_prob[1,i]<-log(dat_pred_par2$estimate["p[1]"])+c1p1
  log_prob[2,i]<-log(dat_pred_par2$estimate["p[2]"])+c2p1
  log_prob[3,i]<-log(dat_pred_par2$estimate["p[3]"])+c3p1
  log_prob[4,i]<-log(dat_pred_par2$estimate["p[4]"])+c4p1
}

log_prob<-exp(log_prob)

col_sum_likelihood<-colSums(log_prob)
prob<-data.frame(matrix(rep(0,142*4),nrow = 4,byrow = T))
for(i in 1:142){
  for (j in 1:4) {
    prob[j,i]<-log_prob[j,i]/col_sum_likelihood[i]
  }
}


##rmse
estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"],
      dat_pred_par2$estimate["mean3"],
      dat_pred_par2$estimate["mean4"]))%*%
  as.matrix(prob)

estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"],
      dat_pred_par2$estimate["mean3"]))%*%
  matrix(c(dat_pred_par2$estimate["p_class1[1]"],dat_pred_par2$estimate["p_class1[2]"],dat_pred_par2$estimate["p_class1[3]"],
           dat_pred_par2$estimate["p_class2[1]"],dat_pred_par2$estimate["p_class2[2]"],dat_pred_par2$estimate["p_class2[3]"],
           dat_pred_par2$estimate["p_class3[1]"],dat_pred_par2$estimate["p_class3[2]"],dat_pred_par2$estimate["p_class3[3]"],
           dat_pred_par2$estimate["p_class4[1]"],dat_pred_par2$estimate["p_class4[2]"],dat_pred_par2$estimate["p_class4[3]"]),
         byrow = FALSE,ncol=4)%*%
  as.matrix(prob)


estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"],
      dat_pred_par2$estimate["mean3"],
      dat_pred_par2$estimate["mean4"]))%*%
  matrix(c(dat_pred_par2$estimate["p_class1[1]"],dat_pred_par2$estimate["p_class1[2]"],dat_pred_par2$estimate["p_class1[3]"],dat_pred_par2$estimate["p_class1[4]"],
           dat_pred_par2$estimate["p_class2[1]"],dat_pred_par2$estimate["p_class2[2]"],dat_pred_par2$estimate["p_class2[3]"],dat_pred_par2$estimate["p_class2[4]"],
           dat_pred_par2$estimate["p_class3[1]"],dat_pred_par2$estimate["p_class3[2]"],dat_pred_par2$estimate["p_class3[3]"],dat_pred_par2$estimate["p_class3[4]"],
           dat_pred_par2$estimate["p_class4[1]"],dat_pred_par2$estimate["p_class4[2]"],dat_pred_par2$estimate["p_class4[3]"],dat_pred_par2$estimate["p_class4[4]"]),
         byrow = FALSE,ncol=4)%*%
  as.matrix(prob)

estimated_total_work<-
  c(dat_pred_par2$estimate["mean1"],
    dat_pred_par2$estimate["mean2"],
    dat_pred_par2$estimate["mean3"],
    dat_pred_par2$estimate["mean4"])%*%
  matrix(c(dat_pred_par2$estimate["p_outcome[1]"],dat_pred_par2$estimate["p_outcome[2]"],
           dat_pred_par2$estimate["p_outcome[3]"],dat_pred_par2$estimate["p_outcome[4]"]),
         nrow=4)

sqrt(sum((estimated_total_work - outcome3_pred_dat2$total_work)^2)/142)
sqrt(sum((rep(estimated_total_work,142) - outcome3_pred_dat2$total_work)^2)/142)

# indep 
sqrt(( sum((rep(dat_pred_par2$estimate["mean1"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[1]"]+
         sum((rep(dat_pred_par2$estimate["mean2"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[2]"]+
         sum((rep(dat_pred_par2$estimate["mean3"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[3]"]+
         sum((rep(dat_pred_par2$estimate["mean4"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[4]"])/142)

####1phase5class
load("54_hypo41.rdata")
log_lik<-loo::extract_log_lik(fit_5classes, parameter_name = "log_lik", merge_chains = TRUE)
loo::waic(log_lik)
datPred1<-broom.mixed::tidyMCMC(fit_5classes, rhat =T)
datPred1%>%
  filter(rhat>1.1)
bayesplot::mcmc_trace(fit_5classes,"beta2[1,5]")
dat_pred_par<-datPred1[-c(1:635),]
dat_pred_par2<-datPred1[c(1:65),]
###
log_prob<-data.frame(matrix(rep(0,142*5),nrow = 5,byrow = T))
for (i in 1:142) {
  c1p1<-log(dat_pred_par[(test_pred_dat2[1+(i-1)*48,"coRes"]-1)*48+1,"estimate"])
  c2p1<-log(dat_pred_par[(test_pred_dat2[1+(i-1)*48,"coRes"]-1)*48+1+48*6,"estimate"])
  c3p1<-log(dat_pred_par[(test_pred_dat2[1+(i-1)*48,"coRes"]-1)*48+1+48*12,"estimate"])
  c4p1<-log(dat_pred_par[(test_pred_dat2[1+(i-1)*48,"coRes"]-1)*48+1+48*18,"estimate"])
  c5p1<-log(dat_pred_par[(test_pred_dat2[1+(i-1)*48,"coRes"]-1)*48+1+48*24,"estimate"])
  
  for (j in (2+(i-1)*48):(i*48)) {
    
    if(test_pred_dat2[j,"coRes"]==test_pred_dat2[j-1,"coRes"]){
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"]+dat_pred_par2$estimate["rho[1]"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"]+dat_pred_par2$estimate["rho[2]"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"]+dat_pred_par2$estimate["rho[3]"])
      c4p1<-c4p1+
        log((1-dat_pred_par2$estimate["rho[4]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*18,"estimate"]+dat_pred_par2$estimate["rho[4]"])
      c5p1<-c5p1+
        log((1-dat_pred_par2$estimate["rho[5]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*24,"estimate"]+dat_pred_par2$estimate["rho[5]"])
      
    }
    else{
      c1p1<-c1p1+
        log((1-dat_pred_par2$estimate["rho[1]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48),"estimate"])
      c2p1<-c2p1+
        log((1-dat_pred_par2$estimate["rho[2]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*6,"estimate"])
      c3p1<-c3p1+
        log((1-dat_pred_par2$estimate["rho[3]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*12,"estimate"])
      c4p1<-c4p1+
        log((1-dat_pred_par2$estimate["rho[4]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*18,"estimate"])
      c5p1<-c5p1+
        log((1-dat_pred_par2$estimate["rho[5]"])*dat_pred_par[(test_pred_dat2[j,"coRes"]-1)*48+(j-(i-1)*48)+48*24,"estimate"])
      
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
prob<-data.frame(matrix(rep(0,142*5),nrow = 5,byrow = T))
for(i in 1:142){
  for (j in 1:5) {
    prob[j,i]<-log_prob[j,i]/col_sum_likelihood[i]
  }
}

##rmse
estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"],
      dat_pred_par2$estimate["mean3"],
      dat_pred_par2$estimate["mean4"],
      dat_pred_par2$estimate["mean4"]))%*%
  as.matrix(prob)

estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"]))%*%
  matrix(c(dat_pred_par2$estimate["p_class1[1]"],dat_pred_par2$estimate["p_class1[2]"],
           dat_pred_par2$estimate["p_class2[1]"],dat_pred_par2$estimate["p_class2[2]"],
           dat_pred_par2$estimate["p_class3[1]"],dat_pred_par2$estimate["p_class3[2]"],
           dat_pred_par2$estimate["p_class4[1]"],dat_pred_par2$estimate["p_class4[2]"],
           dat_pred_par2$estimate["p_class5[1]"],dat_pred_par2$estimate["p_class5[2]"]),
         byrow = FALSE,ncol=5)%*%
  as.matrix(prob)

estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"],
      dat_pred_par2$estimate["mean3"]))%*%
  matrix(c(dat_pred_par2$estimate["p_outcome[1]"],dat_pred_par2$estimate["p_outcome[2]"],dat_pred_par2$estimate["p_outcome[3]"]),
         byrow = FALSE)

estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"],
      dat_pred_par2$estimate["mean3"]))%*%
  matrix(c(dat_pred_par2$estimate["p_class1[1]"],dat_pred_par2$estimate["p_class1[2]"],dat_pred_par2$estimate["p_class1[3]"],
           dat_pred_par2$estimate["p_class2[1]"],dat_pred_par2$estimate["p_class2[2]"],dat_pred_par2$estimate["p_class2[3]"],
           dat_pred_par2$estimate["p_class3[1]"],dat_pred_par2$estimate["p_class3[2]"],dat_pred_par2$estimate["p_class3[3]"],
           dat_pred_par2$estimate["p_class4[1]"],dat_pred_par2$estimate["p_class4[2]"],dat_pred_par2$estimate["p_class4[3]"],
           dat_pred_par2$estimate["p_class5[1]"],dat_pred_par2$estimate["p_class5[2]"],dat_pred_par2$estimate["p_class5[3]"]),
         byrow = FALSE,ncol=5)%*%
  as.matrix(prob)

estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"],
      dat_pred_par2$estimate["mean3"],
      dat_pred_par2$estimate["mean4"]))%*%
  matrix(c(dat_pred_par2$estimate["p_class1[1]"],dat_pred_par2$estimate["p_class1[2]"],dat_pred_par2$estimate["p_class1[3]"],dat_pred_par2$estimate["p_class1[4]"],
           dat_pred_par2$estimate["p_class2[1]"],dat_pred_par2$estimate["p_class2[2]"],dat_pred_par2$estimate["p_class2[3]"],dat_pred_par2$estimate["p_class2[4]"],
           dat_pred_par2$estimate["p_class3[1]"],dat_pred_par2$estimate["p_class3[2]"],dat_pred_par2$estimate["p_class3[3]"],dat_pred_par2$estimate["p_class3[4]"],
           dat_pred_par2$estimate["p_class4[1]"],dat_pred_par2$estimate["p_class4[2]"],dat_pred_par2$estimate["p_class4[3]"],dat_pred_par2$estimate["p_class4[4]"],
           dat_pred_par2$estimate["p_class5[1]"],dat_pred_par2$estimate["p_class5[2]"],dat_pred_par2$estimate["p_class5[3]"],dat_pred_par2$estimate["p_class5[4]"]),
         byrow = FALSE,ncol=5)%*%
  as.matrix(prob)

estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
      dat_pred_par2$estimate["mean2"],
      dat_pred_par2$estimate["mean3"],
      dat_pred_par2$estimate["mean4"],
      dat_pred_par2$estimate["mean5"]))%*%
  matrix(c(dat_pred_par2$estimate["p_class1[1]"],dat_pred_par2$estimate["p_class1[2]"],dat_pred_par2$estimate["p_class1[3]"],dat_pred_par2$estimate["p_class1[4]"],dat_pred_par2$estimate["p_class1[5]"],
           dat_pred_par2$estimate["p_class2[1]"],dat_pred_par2$estimate["p_class2[2]"],dat_pred_par2$estimate["p_class2[3]"],dat_pred_par2$estimate["p_class2[4]"],dat_pred_par2$estimate["p_class1[5]"],
           dat_pred_par2$estimate["p_class3[1]"],dat_pred_par2$estimate["p_class3[2]"],dat_pred_par2$estimate["p_class3[3]"],dat_pred_par2$estimate["p_class3[4]"],dat_pred_par2$estimate["p_class1[5]"],
           dat_pred_par2$estimate["p_class4[1]"],dat_pred_par2$estimate["p_class4[2]"],dat_pred_par2$estimate["p_class4[3]"],dat_pred_par2$estimate["p_class4[4]"],dat_pred_par2$estimate["p_class1[5]"],
           dat_pred_par2$estimate["p_class5[1]"],dat_pred_par2$estimate["p_class5[2]"],dat_pred_par2$estimate["p_class5[3]"],dat_pred_par2$estimate["p_class5[4]"],dat_pred_par2$estimate["p_class1[5]"]),
         byrow = FALSE,ncol=5)%*%
  as.matrix(prob)

estimated_total_work<-
  c(dat_pred_par2$estimate["mean1"],
    dat_pred_par2$estimate["mean2"],
    dat_pred_par2$estimate["mean3"],
    dat_pred_par2$estimate["mean4"],
    dat_pred_par2$estimate["mean5"])%*%
  matrix(c(dat_pred_par2$estimate["p_outcome[1]"],dat_pred_par2$estimate["p_outcome[2]"],
           dat_pred_par2$estimate["p_outcome[3]"],dat_pred_par2$estimate["p_outcome[4]"],
           dat_pred_par2$estimate["p_outcome[5]"]),
         nrow=5)
estimated_total_work<-
  t(c(dat_pred_par2$estimate["mean1"],
    dat_pred_par2$estimate["mean2"],
    dat_pred_par2$estimate["mean3"],
    dat_pred_par2$estimate["mean4"]))%*%
  matrix(c(dat_pred_par2$estimate["p_outcome[1]"],dat_pred_par2$estimate["p_outcome[2]"],
           dat_pred_par2$estimate["p_outcome[3]"],dat_pred_par2$estimate["p_outcome[4]"]),byrow = FALSE)

sqrt(sum((estimated_total_work - outcome3_pred_dat2$total_work)^2)/142)
sqrt(sum((rep(estimated_total_work,142) - outcome3_pred_dat2$total_work)^2)/142)

#indep
sqrt(( sum((rep(dat_pred_par2$estimate["mean1"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[1]"]+
         sum((rep(dat_pred_par2$estimate["mean2"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[2]"]+
         sum((rep(dat_pred_par2$estimate["mean3"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[3]"] )/142)

sqrt(( sum((rep(dat_pred_par2$estimate["mean1"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[1]"]+
         sum((rep(dat_pred_par2$estimate["mean2"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[2]"]+
         sum((rep(dat_pred_par2$estimate["mean3"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[3]"]+
         sum((rep(dat_pred_par2$estimate["mean4"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[4]"])/142)


sqrt(( sum((rep(dat_pred_par2$estimate["mean1"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[1]"]+
         sum((rep(dat_pred_par2$estimate["mean2"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[2]"]+
         sum((rep(dat_pred_par2$estimate["mean3"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[3]"]+
         sum((rep(dat_pred_par2$estimate["mean4"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[4]"]+
         sum((rep(dat_pred_par2$estimate["mean5"],142)-outcome3_pred_dat2$total_work)^2)*
         dat_pred_par2$estimate["p_outcome[5]"])/142)
