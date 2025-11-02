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


entropy_testing<-c()
#
log_prob_o<-data.frame(matrix(rep(0,142*3),nrow = 3,byrow = T))
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
  
  
  log_prob_o[1,i]<-log(dat_pred_par2$estimate["p[1]"])+c1p1
  log_prob_o[2,i]<-log(dat_pred_par2$estimate["p[2]"])+c2p1
  log_prob_o[3,i]<-log(dat_pred_par2$estimate["p[3]"])+c3p1
}

log_prob_o<-exp(log_prob_o)

col_sum_likelihood_o<-colSums(log_prob_o)
prob_o<-data.frame(matrix(rep(0,142*3),nrow = 3,byrow = T))
for(i in 1:142){
  for (j in 1:3) {
    prob_o[j,i]<-log_prob_o[j,i]/col_sum_likelihood_o[i]
  }
}

dat_classMember_prob_test33<-cbind(data.frame(dat%>%filter(!id %in% id_training)),
                                   t(prob_o))

colnames(dat_classMember_prob_test33)<-c("id",c(1:48),"class1","class2","class3")
save(dat_classMember_prob_test33,file = "class_membership_test33.rdata")


entropy<-0
for (i in 1:142) {
  for (k in 1:nrow(prob_o)) {
    entropy<-entropy-(prob_o[k,i]*log2(prob_o[k,i]))
  }
}
entropy/142

#### 4

log_prob_o<-data.frame(matrix(rep(0,142*4),nrow = 4,byrow = T))
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
  
  
  log_prob_o[1,i]<-log(dat_pred_par2$estimate["p[1]"])+c1p1
  log_prob_o[2,i]<-log(dat_pred_par2$estimate["p[2]"])+c2p1
  log_prob_o[3,i]<-log(dat_pred_par2$estimate["p[3]"])+c3p1
  log_prob_o[4,i]<-log(dat_pred_par2$estimate["p[4]"])+c4p1
}

log_prob_o<-exp(log_prob_o)

col_sum_likelihood_o<-colSums(log_prob_o)
prob_o<-data.frame(matrix(rep(0,142*4),nrow = 4,byrow = T))
for(i in 1:142){
  for (j in 1:4) {
    prob_o[j,i]<-log_prob_o[j,i]/col_sum_likelihood_o[i]
  }
}

dat_classMember_prob_test44<-cbind(data.frame(dat%>%filter(!id %in% id_training)),
                                   t(prob_o))

colnames(dat_classMember_prob_test44)<-c("id",c(1:48),"class1","class2","class3","class4")
save(dat_classMember_prob_test44,file = "class_membership_test44.rdata")


entropy<-0
for (i in 1:142) {
  for (k in 1:nrow(prob_o)) {
    entropy<-entropy-(prob_o[k,i]*log2(prob_o[k,i]))
  }
}
entropy/142

###### 5

log_prob_o<-data.frame(matrix(rep(0,142*5),nrow = 5,byrow = T))
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
  
  
  log_prob_o[1,i]<-log(dat_pred_par2$estimate["p[1]"])+c1p1
  log_prob_o[2,i]<-log(dat_pred_par2$estimate["p[2]"])+c2p1
  log_prob_o[3,i]<-log(dat_pred_par2$estimate["p[3]"])+c3p1
  log_prob_o[4,i]<-log(dat_pred_par2$estimate["p[4]"])+c4p1
  log_prob_o[5,i]<-log(dat_pred_par2$estimate["p[5]"])+c5p1
}

log_prob_o<-exp(log_prob_o)

col_sum_likelihood_o<-colSums(log_prob_o)
prob_o<-data.frame(matrix(rep(0,142*5),nrow = 5,byrow = T))
for(i in 1:142){
  for (j in 1:5) {
    prob_o[j,i]<-log_prob_o[j,i]/col_sum_likelihood_o[i]
  }
}


dat_classMember_prob_test53<-cbind(data.frame(dat%>%filter(!id %in% id_training)),
                            t(prob_o))

colnames(dat_classMember_prob_test53)<-c("id",c(1:48),"class1","class2","class3","class4","class5")
save(dat_classMember_prob_test53,file = "class_membership_test53.rdata")


entropy<-0
for (i in 1:142) {
  for (k in 1:nrow(prob_o)) {
    entropy<-entropy-(log2(prob_o[k,i]^(prob_o[k,i])))
  }
}
entropy/142

