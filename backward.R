library(tidyverse)
library(TraMineR)
#create datas considering linear term
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
outcome3<-cbind(outcome3,male=mvad$male,fmpr=mvad$fmpr)
outcome3<-outcome3%>%
  mutate(male=as.numeric(male)-1,
         fmpr=as.numeric(fmpr)-1)
load("id_training_201.rdata")
test_pred_dat1<-test_dat1%>%
  filter(id %in% id_training)
test_pred_dat2<-test_dat1%>%
  filter(!(id %in% id_training))
outcome3_pred_dat1<-outcome3%>%
  filter(id %in% id_training)
outcome3_pred_dat2<-outcome3%>%
  filter(!(id %in% id_training))


###### semi
load("55_hypo5.rdata")
datPred1<-broom.mixed::tidyMCMC(fit_5classes, rhat =T)
dat_pred_par<-datPred1[-c(1:661),]
dat_pred_par2<-datPred1[c(1:91),]
post_prob<-data.frame(matrix(rep(0,25*5),nrow = 5,byrow = T))
JoPrPa1<-matrix(0,nrow = 5,ncol = 5)
JoPrPa2<-matrix(0,nrow = 5,ncol = 5)
JoPrPa3<-matrix(0,nrow = 5,ncol = 5)
JoPrPa4<-matrix(0,nrow = 5,ncol = 5)
JoPrPa5<-matrix(0,nrow = 5,ncol = 5)
####p(path|Y)
y<-c(0,3,6,9,12)
ordered_class<-c(3,1,2,5,4)
ordered_mu<-c(5,1,2,4,3)
for (k.1 in 1:5){
  for (k.2 in 1:5) {
    
    JoPrPa1[k.1,k.2]<-dat_pred_par2$estimate[paste0("p[",ordered_class[k.1],"]")]*
      dat_pred_par2$estimate[paste0("p_class",ordered_class[k.1],"[",ordered_mu[k.2],"]")]*
      dnorm(y[1],dat_pred_par2$estimate[paste0("mean",ordered_mu[k.2])],
            dat_pred_par2$estimate["sigma"])
    JoPrPa2[k.1,k.2]<-dat_pred_par2$estimate[paste0("p[",ordered_class[k.1],"]")]*
      dat_pred_par2$estimate[paste0("p_class",ordered_class[k.1],"[",ordered_mu[k.2],"]")]*
      dnorm(y[2], dat_pred_par2$estimate[paste0("mean",ordered_mu[k.2])],
                            dat_pred_par2$estimate["sigma"])
    JoPrPa3[k.1,k.2]<-dat_pred_par2$estimate[paste0("p[",ordered_class[k.1],"]")]*
      dat_pred_par2$estimate[paste0("p_class",ordered_class[k.1],"[",ordered_mu[k.2],"]")]*
      dnorm(y[3],dat_pred_par2$estimate[paste0("mean",ordered_mu[k.2])],
                            dat_pred_par2$estimate["sigma"])
    JoPrPa4[k.1,k.2]<-dat_pred_par2$estimate[paste0("p[",ordered_class[k.1],"]")]*
      dat_pred_par2$estimate[paste0("p_class",ordered_class[k.1],"[",ordered_mu[k.2],"]")]*
      dnorm(y[4],dat_pred_par2$estimate[paste0("mean",ordered_mu[k.2])],
                            dat_pred_par2$estimate["sigma"])
    JoPrPa5[k.1,k.2]<-dat_pred_par2$estimate[paste0("p[",ordered_class[k.1],"]")]*
      dat_pred_par2$estimate[paste0("p_class",ordered_class[k.1],"[",ordered_mu[k.2],"]")]*
      dnorm(y[5],dat_pred_par2$estimate[paste0("mean",ordered_mu[k.2])],
            dat_pred_par2$estimate["sigma"])
  }
}
for (k.1 in 1:5){
  for (k.2 in 1:5) {
    post_prob[1,(k.1-1)*5+k.2]<-JoPrPa1[k.1,k.2]/sum(JoPrPa1)
    post_prob[2,(k.1-1)*5+k.2]<-JoPrPa2[k.1,k.2]/sum(JoPrPa2)
    post_prob[3,(k.1-1)*5+k.2]<-JoPrPa3[k.1,k.2]/sum(JoPrPa3)
    post_prob[4,(k.1-1)*5+k.2]<-JoPrPa4[k.1,k.2]/sum(JoPrPa4)
    post_prob[5,(k.1-1)*5+k.2]<-JoPrPa5[k.1,k.2]/sum(JoPrPa5)
  }
}
sum(post_prob[1,])
sum(post_prob[2,])
sum(post_prob[3,])
sum(post_prob[4,])
sum(post_prob[5,])
for (i in 1:25) {
  for(j in 1:5){post_prob[j,i]<-round(as.numeric(post_prob[j,i]),10)}
}
post_prob2<-post_prob
post_prob[6,]<-as.factor(c("1-1","1-2","1-3","1-4","1-5",
                           "2-1","2-2","2-3","2-4","2-5",
                           "3-1","3-2","4-3","3-4","3-5",
                           "4-1","4-2","4-3","4-4","4-5",
                           "5-1","5-2","5-3","5-4","5-5"))
rownames(post_prob)<-c("o0","o3","o6","o9","o12","path")
dat<-data.frame(t(post_prob))
dat<-data.frame(post_prob)
colnames(dat)<-c("1-1","1-2","1-3","1-4","1-5",
                 "2-1","2-2","2-3","2-4","2-5",
                 "3-1","3-2","4-3","3-4","3-5",
                 "4-1","4-2","4-3","4-4","4-5",
                 "5-1","5-2","5-3","5-4","5-5")
dat<-dat[-6,]
dat_semi_post_prob<-dat
save(dat_semi_post_prob,file = "dat_semi_post_prob.rdata")
ggplot(data = dat)+geom_point(aes(x=path,y=o0,color="outcome0",group=1))+
  geom_line(aes(x=path,y=o0,color="outcome0",group=1))+
  geom_point(data = dat,aes(x=path,y=o3,color="outcome3",group = 1))+
  geom_line(aes(x=path,y=o3,color="outcome3",group=1))+
  geom_point(data = dat,aes(x=path,y=o6,color="outcome6",group = 1))+
  geom_line(aes(x=path,y=o6,color="outcome6",group=1))+
  geom_point(data = dat,aes(x=path,y=o9,color="outcome9",group = 1))+
  geom_line(aes(x=path,y=o9,color="outcome9",group=1))+
  geom_point(data = dat,aes(x=path,y=o12,color="outcome12",group = 1))+
  geom_line(aes(x=path,y=o12,color="outcome12",group=1))+
  ylab("Posterior Prob of Path|Y")+
  scale_color_manual(values = c(outcome0 = "blue", outcome3 = "green",
                                outcome6 = "red",
                                outcome9 = "black",
                                outcome12="yellow"))
#####binary
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
estimated_binary<- as.matrix(prob_o)%*%(as.matrix(outcome3_pred_dat1[,4]))#m
estimated_binary<- as.matrix(prob_o)%*%(as.matrix(outcome3_pred_dat1[,5]))#f
estimated_binary<-estimated_binary/c(sum(prob_o[1,]),sum(prob_o[2,]),sum(prob_o[3,]),
                                       sum(prob_o[4,]),
                                       sum(prob_o[5,]))
PoPrPa1<-c(sum(post_prob2[1,1:5]),sum(post_prob2[2,1:5]),
           sum(post_prob2[3,1:5]),sum(post_prob2[4,1:5]),sum(post_prob2[5,1:5]))
PoPrPa2<-c(sum(post_prob2[1,6:10]),sum(post_prob2[2,6:10]),
           sum(post_prob2[3,6:10]),sum(post_prob2[4,6:10]),sum(post_prob2[5,6:10]))
PoPrPa3<-c(sum(post_prob2[1,11:15]),sum(post_prob2[2,11:15]),
           sum(post_prob2[3,11:15]),sum(post_prob2[4,11:15]),sum(post_prob2[5,11:15]))
PoPrPa4<-c(sum(post_prob2[1,16:20]),sum(post_prob2[2,16:20]),
           sum(post_prob2[3,16:20]),sum(post_prob2[4,16:20]),sum(post_prob2[5,16:20]))
PoPrPa5<-c(sum(post_prob2[1,21:25]),sum(post_prob2[2,21:25]),
           sum(post_prob2[3,21:25]),sum(post_prob2[4,21:25]),sum(post_prob2[5,21:25]))
data.frame(p_Z_given_Y=PoPrPa1*estimated_binary[1]+PoPrPa2*estimated_binary[2]+PoPrPa3*estimated_binary[3]+
             PoPrPa4*estimated_binary[4]+PoPrPa5*estimated_binary[5],
           y=y)%>%
  ggplot()+geom_point(aes(x=y,y=p_Z_given_Y))+geom_line(aes(x=y,y=p_Z_given_Y))
fmpr_semi<-data.frame(p_Z_given_Y=PoPrPa1*estimated_binary[1]+PoPrPa2*estimated_binary[2]+PoPrPa3*estimated_binary[3]+
                           PoPrPa4*estimated_binary[4]+PoPrPa5*estimated_binary[5],
                         y=y)
male_semi<-data.frame(p_Z_given_Y=PoPrPa1*estimated_binary[1]+PoPrPa2*estimated_binary[2]+PoPrPa3*estimated_binary[3]+
                           PoPrPa4*estimated_binary[4]+PoPrPa5*estimated_binary[5],
                         y=y)
save(fmpr_semi,file=paste0("55_semi_fmpr.rdata"))
save(male_semi,file=paste0("55_semi_male.rdata"))
save(post_prob,file=paste0("55_semi_PostProb.rdata"))
################################
###### concord
load("55_hypo35.rdata")
datPred1<-broom.mixed::tidyMCMC(fit_5classes, rhat =T)
dat_pred_par<-datPred1[-c(1:636),]
dat_pred_par2<-datPred1[c(1:66),]
post_prob<-data.frame(matrix(rep(0,25*5),nrow = 5,byrow = T))
JoPrPa1<-matrix(0,nrow = 5,ncol = 5)
JoPrPa2<-matrix(0,nrow = 5,ncol = 5)
JoPrPa3<-matrix(0,nrow = 5,ncol = 5)
JoPrPa4<-matrix(0,nrow = 5,ncol = 5)
JoPrPa5<-matrix(0,nrow = 5,ncol = 5)
concord_prob<-diag(1,nrow = 5)
####p(path|Y)
y<-c(0,3,6,9,12)
ordered_class<-c(5,1,3,4,2)
ordered_mu<-c(5,1,3,4,2)
for (k.1 in 1:5){
  for (k.2 in 1:5) {
    
    JoPrPa1[k.1,k.2]<-dat_pred_par2$estimate[paste0("p[",ordered_class[k.1],"]")]*
      concord_prob[ordered_class[k.1],ordered_mu[k.2]]*
      dnorm(y[1],dat_pred_par2$estimate[paste0("mean",ordered_mu[k.2])],
            dat_pred_par2$estimate["sigma"])
    JoPrPa2[k.1,k.2]<-dat_pred_par2$estimate[paste0("p[",ordered_class[k.1],"]")]*
      concord_prob[ordered_class[k.1],ordered_mu[k.2]]*
      dnorm(y[2], dat_pred_par2$estimate[paste0("mean",ordered_mu[k.2])],
            dat_pred_par2$estimate["sigma"])
    JoPrPa3[k.1,k.2]<-dat_pred_par2$estimate[paste0("p[",ordered_class[k.1],"]")]*
      concord_prob[ordered_class[k.1],ordered_mu[k.2]]*
      dnorm(y[3],dat_pred_par2$estimate[paste0("mean",ordered_mu[k.2])],
            dat_pred_par2$estimate["sigma"])
    JoPrPa4[k.1,k.2]<-dat_pred_par2$estimate[paste0("p[",ordered_class[k.1],"]")]*
      concord_prob[ordered_class[k.1],ordered_mu[k.2]]*
      dnorm(y[4],dat_pred_par2$estimate[paste0("mean",ordered_mu[k.2])],
            dat_pred_par2$estimate["sigma"])
    JoPrPa5[k.1,k.2]<-dat_pred_par2$estimate[paste0("p[",ordered_class[k.1],"]")]*
      concord_prob[ordered_class[k.1],ordered_mu[k.2]]*
      dnorm(y[5],dat_pred_par2$estimate[paste0("mean",ordered_mu[k.2])],
            dat_pred_par2$estimate["sigma"])
  }
}
for (k.1 in 1:5){
  for (k.2 in 1:5) {
    post_prob[1,(k.1-1)*5+k.2]<-JoPrPa1[k.1,k.2]/sum(JoPrPa1)
    post_prob[2,(k.1-1)*5+k.2]<-JoPrPa2[k.1,k.2]/sum(JoPrPa2)
    post_prob[3,(k.1-1)*5+k.2]<-JoPrPa3[k.1,k.2]/sum(JoPrPa3)
    post_prob[4,(k.1-1)*5+k.2]<-JoPrPa4[k.1,k.2]/sum(JoPrPa4)
    post_prob[5,(k.1-1)*5+k.2]<-JoPrPa5[k.1,k.2]/sum(JoPrPa5)
  }
}
sum(post_prob[1,])
sum(post_prob[2,])
sum(post_prob[3,])
sum(post_prob[4,])
sum(post_prob[5,])
for (i in 1:25) {
  for(j in 1:5){post_prob[j,i]<-round(as.numeric(post_prob[j,i]),10)}
}
post_prob2<-post_prob
post_prob[6,]<-as.factor(c("1-1","1-2","1-3","1-4","1-5",
                           "2-1","2-2","2-3","2-4","2-5",
                           "3-1","3-2","4-3","3-4","3-5",
                           "4-1","4-2","4-3","4-4","4-5",
                           "5-1","5-2","5-3","5-4","5-5"))
rownames(post_prob)<-c("o0","o3","o6","o9","o12","path")
dat<-data.frame(t(post_prob))
dat<-data.frame(post_prob)
colnames(dat)<-c("1-1","1-2","1-3","1-4","1-5",
                 "2-1","2-2","2-3","2-4","2-5",
                 "3-1","3-2","4-3","3-4","3-5",
                 "4-1","4-2","4-3","4-4","4-5",
                 "5-1","5-2","5-3","5-4","5-5")
dat<-dat[-6,]
dat_full_post_prob<-dat
save(dat_full_post_prob,file = "dat_full_post_prob.rdata")

ggplot(data = dat)+geom_point(aes(x=path,y=o0,color="outcome0",group=1))+
  geom_line(aes(x=path,y=o0,color="outcome0",group=1))+
  geom_point(data = dat,aes(x=path,y=o3,color="outcome3",group = 1))+
  geom_line(aes(x=path,y=o3,color="outcome3",group=1))+
  geom_point(data = dat,aes(x=path,y=o6,color="outcome6",group = 1))+
  geom_line(aes(x=path,y=o6,color="outcome6",group=1))+
  geom_point(data = dat,aes(x=path,y=o9,color="outcome9",group = 1))+
  geom_line(aes(x=path,y=o9,color="outcome9",group=1))+
  geom_point(data = dat,aes(x=path,y=o12,color="outcome12",group = 1))+
  geom_line(aes(x=path,y=o12,color="outcome12",group=1))+
  ylab("Posterior Prob of Path|Y")+
  scale_color_manual(values = c(outcome0 = "blue", outcome3 = "green",
                                outcome6 = "grey",
                                outcome9 = "black",
                                outcome12="yellow"))
#####binary
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
estimated_binary<- as.matrix(prob_o)%*%(as.matrix(outcome3_pred_dat1[,4]))#m
estimated_binary<- as.matrix(prob_o)%*%(as.matrix(outcome3_pred_dat1[,5]))#f
estimated_binary<-estimated_binary/c(sum(prob_o[1,]),sum(prob_o[2,]),sum(prob_o[3,]),
                                     sum(prob_o[4,]),
                                     sum(prob_o[5,]))
PoPrPa1<-c(sum(post_prob2[1,1:5]),sum(post_prob2[2,1:5]),
           sum(post_prob2[3,1:5]),sum(post_prob2[4,1:5]),sum(post_prob2[5,1:5]))
PoPrPa2<-c(sum(post_prob2[1,6:10]),sum(post_prob2[2,6:10]),
           sum(post_prob2[3,6:10]),sum(post_prob2[4,6:10]),sum(post_prob2[5,6:10]))
PoPrPa3<-c(sum(post_prob2[1,11:15]),sum(post_prob2[2,11:15]),
           sum(post_prob2[3,11:15]),sum(post_prob2[4,11:15]),sum(post_prob2[5,11:15]))
PoPrPa4<-c(sum(post_prob2[1,16:20]),sum(post_prob2[2,16:20]),
           sum(post_prob2[3,16:20]),sum(post_prob2[4,16:20]),sum(post_prob2[5,16:20]))
PoPrPa5<-c(sum(post_prob2[1,21:25]),sum(post_prob2[2,21:25]),
           sum(post_prob2[3,21:25]),sum(post_prob2[4,21:25]),sum(post_prob2[5,21:25]))
data.frame(p_Z_given_Y=PoPrPa1*estimated_binary[1]+PoPrPa2*estimated_binary[2]+PoPrPa3*estimated_binary[3]+
             PoPrPa4*estimated_binary[4]+PoPrPa5*estimated_binary[5],
           y=y)%>%
  ggplot()+geom_point(aes(x=y,y=p_Z_given_Y))+geom_line(aes(x=y,y=p_Z_given_Y))
fmpr_concord<-data.frame(p_Z_given_Y=PoPrPa1*estimated_binary[1]+PoPrPa2*estimated_binary[2]+PoPrPa3*estimated_binary[3]+
                   PoPrPa4*estimated_binary[4]+PoPrPa5*estimated_binary[5],
                 y=y)
male_concord<-data.frame(p_Z_given_Y=PoPrPa1*estimated_binary[1]+PoPrPa2*estimated_binary[2]+PoPrPa3*estimated_binary[3]+
                           PoPrPa4*estimated_binary[4]+PoPrPa5*estimated_binary[5],
                         y=y)
save(fmpr_concord,file=paste0("55_concord_fmpr.rdata"))
save(male_concord,file=paste0("55_concord_male.rdata"))
save(post_prob,file=paste0("55_concord_PostProb.rdata"))
