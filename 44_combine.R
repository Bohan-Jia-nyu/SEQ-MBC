
SEED.START = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

library(rstan)
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

load("id_training_201.rdata")
test_pred_dat1<-test_dat1%>%
  filter(id %in% id_training)
test_pred_dat2<-test_dat1%>%
  filter(!(id %in% id_training))
outcome3_pred_dat1<-outcome3%>%
  filter(id %in% id_training)
outcome3_pred_dat2<-outcome3%>%
  filter(!(id %in% id_training))

time_matrix_test<-as.matrix(cbind(1,test_pred_dat1$time))
time_matrix_test2<-as.matrix(cbind(1,c(1:48)))

train_dat<-list(
  n=27360,
  N=48,
  M=6,
  D=1,
  N_individual=570,
  cores=c(as.numeric(test_pred_dat1$coRes)),
  time_matrix=time_matrix_test,
  time_matrix_2=time_matrix_test2,
  outcome=outcome3_pred_dat1$total_work
)

if (SEED.START%in%c(1:10)) {
model<-"
data {
  int<lower=0> n;//N*N_individual
  int<lower=0> N;//times
  int<lower=0> M;//states
  int<lower=0> D;//power
  int<lower=0> N_individual;//people
  int cores[n];//outcome
  matrix[n,1+D] time_matrix;
  matrix[N,1+D] time_matrix_2;
  int outcome[N_individual];
}


parameters {
  real<lower=0,upper=1> rho[4];
  simplex [4] p;
  real beta1 [D+1,M-1];
  real beta2 [D+1,M-1];
  real beta3 [D+1,M-1];
  real beta4 [D+1,M-1];
  //outcome
  real<lower=0> mean1;
  real<lower=0> mean2;
  real<lower=0> mean3;
  real<lower=0> mean4;
  real<lower=0> sigma;//homogeneous standard error
  simplex [4] p_class1; //class 1
  simplex [4] p_class2; 
  simplex [4] p_class3; 
  simplex [4] p_class4; 
  
}

transformed parameters{
  real log_lik[N_individual];
  {
  ///////tranform
  real lp[4];
  real multi1[N_individual];
  real multi2[N_individual];
  real multi3[N_individual];
  real multi4[N_individual];
  matrix[n,M] exp_omega1 ;
  matrix[n,M] exp_omega2 ;
  matrix[n,M] exp_omega3 ;
  matrix[n,M] exp_omega4 ;
  vector[n] x=rep_vector(1,n);
  vector[M] row_sum=rep_vector(1,M);
  vector[n] row_sum_1;
  vector[n] row_sum_2;
  vector[n] row_sum_3;
  vector[n] row_sum_4;
  //phase 2
  real lp_ph2_parent1[4];
  real lp_ph2_parent2[4];
  real lp_ph2_parent3[4];
  real lp_ph2_parent4[4];
  real ph2_likelihood_parent1[N_individual];
  real ph2_likelihood_parent2[N_individual];
  real ph2_likelihood_parent3[N_individual];
  real ph2_likelihood_parent4[N_individual];
  
  exp_omega1=append_col(exp(time_matrix*to_matrix(beta1)),x); 
  exp_omega2=append_col(exp(time_matrix*to_matrix(beta2)),x);
  exp_omega3=append_col(exp(time_matrix*to_matrix(beta3)),x); 
  exp_omega4=append_col(exp(time_matrix*to_matrix(beta4)),x);
  row_sum_1=exp_omega1*row_sum;
  row_sum_2=exp_omega2*row_sum;
  row_sum_3=exp_omega3*row_sum;
  row_sum_4=exp_omega4*row_sum;
  for(i in 1:N_individual){
      multi1[i]=log(exp_omega1[(i-1)*N+1,cores[(i-1)*N+1]]/row_sum_1[(i-1)*N+1]);
      multi2[i]=log(exp_omega2[(i-1)*N+1,cores[(i-1)*N+1]]/row_sum_2[(i-1)*N+1]);
      multi3[i]=log(exp_omega3[(i-1)*N+1,cores[(i-1)*N+1]]/row_sum_3[(i-1)*N+1]);
      multi4[i]=log(exp_omega4[(i-1)*N+1,cores[(i-1)*N+1]]/row_sum_4[(i-1)*N+1]);
      for (j in (i-1)*N+2:i*N){
        if(cores[j]==cores[j-1]){
      multi1[i]=multi1[i]+log(
      (exp_omega1[j,cores[j]]/row_sum_1[j])*(1-rho[1])+rho[1]);
      multi2[i]=multi2[i]+log(
      (exp_omega2[j,cores[j]]/row_sum_2[j])*(1-rho[2])+rho[2]);
      multi3[i]=multi3[i]+log(
      (exp_omega3[j,cores[j]]/row_sum_3[j])*(1-rho[3])+rho[3]);
      multi4[i]=multi4[i]+log(
      (exp_omega4[j,cores[j]]/row_sum_4[j])*(1-rho[4])+rho[4]);
      }
        else{
      multi1[i]=multi1[i]+log(
      (exp_omega1[j,cores[j]]/row_sum_1[j])*(1-rho[1]));
      multi2[i]=multi2[i]+log(
      (exp_omega2[j,cores[j]]/row_sum_2[j])*(1-rho[2]));
      multi3[i]=multi3[i]+log(
      (exp_omega3[j,cores[j]]/row_sum_3[j])*(1-rho[3]));
      multi4[i]=multi4[i]+log(
      (exp_omega4[j,cores[j]]/row_sum_4[j])*(1-rho[4]));
      }
      
    }
    
    lp_ph2_parent1[1]=log(p_class1[1])+normal_lpdf(outcome[i]|mean1,sigma);
    lp_ph2_parent1[2]=log(p_class1[2])+normal_lpdf(outcome[i]|mean2,sigma);
    lp_ph2_parent1[3]=log(p_class1[3])+normal_lpdf(outcome[i]|mean3,sigma);
    lp_ph2_parent1[4]=log(p_class1[4])+normal_lpdf(outcome[i]|mean4,sigma);
    
    lp_ph2_parent2[1]=log(p_class2[1])+normal_lpdf(outcome[i]|mean1,sigma);
    lp_ph2_parent2[2]=log(p_class2[2])+normal_lpdf(outcome[i]|mean2,sigma);
    lp_ph2_parent2[3]=log(p_class2[3])+normal_lpdf(outcome[i]|mean3,sigma);
    lp_ph2_parent2[4]=log(p_class2[4])+normal_lpdf(outcome[i]|mean4,sigma);
    
    lp_ph2_parent3[1]=log(p_class3[1])+normal_lpdf(outcome[i]|mean1,sigma);
    lp_ph2_parent3[2]=log(p_class3[2])+normal_lpdf(outcome[i]|mean2,sigma);
    lp_ph2_parent3[3]=log(p_class3[3])+normal_lpdf(outcome[i]|mean3,sigma);
    lp_ph2_parent3[4]=log(p_class3[4])+normal_lpdf(outcome[i]|mean4,sigma);
    
    lp_ph2_parent4[1]=log(p_class4[1])+normal_lpdf(outcome[i]|mean1,sigma);
    lp_ph2_parent4[2]=log(p_class4[2])+normal_lpdf(outcome[i]|mean2,sigma);
    lp_ph2_parent4[3]=log(p_class4[3])+normal_lpdf(outcome[i]|mean3,sigma);
    lp_ph2_parent4[4]=log(p_class4[4])+normal_lpdf(outcome[i]|mean4,sigma);
    
    ph2_likelihood_parent1[i]=log_sum_exp(lp_ph2_parent1);
    ph2_likelihood_parent2[i]=log_sum_exp(lp_ph2_parent2);
    ph2_likelihood_parent3[i]=log_sum_exp(lp_ph2_parent3);
    ph2_likelihood_parent4[i]=log_sum_exp(lp_ph2_parent4);
    
    lp[1]=multi1[i]+log(p[1])+ph2_likelihood_parent1[i];
    lp[2]=multi2[i]+log(p[2])+ph2_likelihood_parent2[i];
    lp[3]=multi3[i]+log(p[3])+ph2_likelihood_parent3[i];
    lp[4]=multi4[i]+log(p[4])+ph2_likelihood_parent4[i];
    log_lik[i]=log_sum_exp(lp);
  }
///////end of transform
}
}

model{
target+=log_lik;
to_vector(to_matrix(beta1))~ normal(0, 4); 
to_vector(to_matrix(beta2))~ normal(0, 4); 
to_vector(to_matrix(beta3))~ normal(0, 4); 
to_vector(to_matrix(beta4))~ normal(0, 4); // weak-moderate prior
p ~ dirichlet(to_vector([2.5,2.5,2.5,2.5]));
mean1~normal(0,15);
mean2~normal(0,15);
mean3~normal(0,15);
mean4~normal(0,15);
sigma~inv_gamma(8,3);
p_class1 ~ dirichlet(to_vector([2.5,2.5,2.5,2.5]));
p_class2 ~ dirichlet(to_vector([2.5,2.5,2.5,2.5]));
p_class3 ~ dirichlet(to_vector([2.5,2.5,2.5,2.5]));
p_class4 ~ dirichlet(to_vector([2.5,2.5,2.5,2.5]));
}

generated quantities{
  matrix[N,M] invMNlogit_c1;
  matrix[N,M] invMNlogit_c2;
  matrix[N,M] invMNlogit_c3;
  matrix[N,M] invMNlogit_c4;
  {
  vector[N] row_sum_c1;
  vector[N] row_sum_c2;
  vector[N] row_sum_c3;
  vector[N] row_sum_c4;
  row_sum_c1=append_col(exp(time_matrix_2*to_matrix(beta1)),rep_vector(1,N))*rep_vector(1,M);
  row_sum_c2=append_col(exp(time_matrix_2*to_matrix(beta2)),rep_vector(1,N))*rep_vector(1,M);
  row_sum_c3=append_col(exp(time_matrix_2*to_matrix(beta3)),rep_vector(1,N))*rep_vector(1,M);
  row_sum_c4=append_col(exp(time_matrix_2*to_matrix(beta4)),rep_vector(1,N))*rep_vector(1,M);
  
  for (i in 1:N){
    invMNlogit_c1[i,]=(append_col(exp(time_matrix_2*to_matrix(beta1)),rep_vector(1,N)))[i,]/row_sum_c1[i];
    invMNlogit_c2[i,]=(append_col(exp(time_matrix_2*to_matrix(beta2)),rep_vector(1,N)))[i,]/row_sum_c2[i];
    invMNlogit_c3[i,]=(append_col(exp(time_matrix_2*to_matrix(beta3)),rep_vector(1,N)))[i,]/row_sum_c3[i];
    invMNlogit_c4[i,]=(append_col(exp(time_matrix_2*to_matrix(beta4)),rep_vector(1,N)))[i,]/row_sum_c4[i];
  }
  }
}

"

###############model 4 classes
fit_4classes<-stan(model_code = model,data=train_dat,iter=5000,warmup = 3000,seed = 100+SEED.START,
                   chains = 1,
                   init = list(
                     list(
                       rho=c(0.6,0.6,0.6,0.6),
                       p=c(0.25,0.25,0.25,0.25),
                       beta1=t(cbind(rep(0.1,5),rep(0.001,5))),
                       beta2=t(cbind(rep(0.1,5),rep(0.001,5))),
                       beta3=t(cbind(rep(0.1,5),rep(0.001,5))),
                       beta4=t(cbind(rep(0.1,5),rep(0.001,5))),
                       mean1=6,
                       mean2=6,
                       mean3=6,
                       mean4=6,
                       p_class1=c(0.25,0.25,0.25,0.25),
                       p_class2=c(0.25,0.25,0.25,0.25),
                       p_class3=c(0.25,0.25,0.25,0.25),
                       p_class4=c(0.25,0.25,0.25,0.25),
                       sigma=1
                     )
                   ))}else if (SEED.START%in%c(11:20)){
                     
model<-"
data {
  int<lower=0> n;//N*N_individual
  int<lower=0> N;//times
  int<lower=0> M;//states
  int<lower=0> D;//power
  int<lower=0> N_individual;//people
  int cores[n];//outcome
  matrix[n,1+D] time_matrix;
  matrix[N,1+D] time_matrix_2;
  int outcome[N_individual];
}


parameters {
  real<lower=0,upper=1> rho[4];
  simplex [4] p;
  real beta1 [D+1,M-1];
  real beta2 [D+1,M-1];
  real beta3 [D+1,M-1];
  real beta4 [D+1,M-1];
  //outcome
  real<lower=0> mean1;
  real<lower=0> mean2;
  real<lower=0> mean3;
  real<lower=0> mean4;
  real<lower=0> sigma;//homogeneous standard error
  simplex [4] p_outcome;
}

transformed parameters{
  real log_lik[N_individual];
  {
  ///////tranform
  real lp[4];
  real multi1[N_individual];
  real multi2[N_individual];
  real multi3[N_individual];
  real multi4[N_individual];
  matrix[n,M] exp_omega1 ;
  matrix[n,M] exp_omega2 ;
  matrix[n,M] exp_omega3 ;
  matrix[n,M] exp_omega4 ;
  vector[n] x=rep_vector(1,n);
  vector[M] row_sum=rep_vector(1,M);
  vector[n] row_sum_1;
  vector[n] row_sum_2;
  vector[n] row_sum_3;
  vector[n] row_sum_4;
  //phase 2
  real lp_ph2[4];
  
  exp_omega1=append_col(exp(time_matrix*to_matrix(beta1)),x); 
  exp_omega2=append_col(exp(time_matrix*to_matrix(beta2)),x);
  exp_omega3=append_col(exp(time_matrix*to_matrix(beta3)),x); 
  exp_omega4=append_col(exp(time_matrix*to_matrix(beta4)),x);
  row_sum_1=exp_omega1*row_sum;
  row_sum_2=exp_omega2*row_sum;
  row_sum_3=exp_omega3*row_sum;
  row_sum_4=exp_omega4*row_sum;
  for(i in 1:N_individual){
      multi1[i]=log(exp_omega1[(i-1)*N+1,cores[(i-1)*N+1]]/row_sum_1[(i-1)*N+1]);
      multi2[i]=log(exp_omega2[(i-1)*N+1,cores[(i-1)*N+1]]/row_sum_2[(i-1)*N+1]);
      multi3[i]=log(exp_omega3[(i-1)*N+1,cores[(i-1)*N+1]]/row_sum_3[(i-1)*N+1]);
      multi4[i]=log(exp_omega4[(i-1)*N+1,cores[(i-1)*N+1]]/row_sum_4[(i-1)*N+1]);
      for (j in (i-1)*N+2:i*N){
        if(cores[j]==cores[j-1]){
      multi1[i]=multi1[i]+log(
      (exp_omega1[j,cores[j]]/row_sum_1[j])*(1-rho[1])+rho[1]);
      multi2[i]=multi2[i]+log(
      (exp_omega2[j,cores[j]]/row_sum_2[j])*(1-rho[2])+rho[2]);
      multi3[i]=multi3[i]+log(
      (exp_omega3[j,cores[j]]/row_sum_3[j])*(1-rho[3])+rho[3]);
      multi4[i]=multi4[i]+log(
      (exp_omega4[j,cores[j]]/row_sum_4[j])*(1-rho[4])+rho[4]);
      }
        else{
      multi1[i]=multi1[i]+log(
      (exp_omega1[j,cores[j]]/row_sum_1[j])*(1-rho[1]));
      multi2[i]=multi2[i]+log(
      (exp_omega2[j,cores[j]]/row_sum_2[j])*(1-rho[2]));
      multi3[i]=multi3[i]+log(
      (exp_omega3[j,cores[j]]/row_sum_3[j])*(1-rho[3]));
      multi4[i]=multi4[i]+log(
      (exp_omega4[j,cores[j]]/row_sum_4[j])*(1-rho[4]));
      }
      
    }
    
    lp_ph2[1]=log(p_outcome[1])+normal_lpdf(outcome[i]|mean1,sigma);
    lp_ph2[2]=log(p_outcome[2])+normal_lpdf(outcome[i]|mean2,sigma);
    lp_ph2[3]=log(p_outcome[3])+normal_lpdf(outcome[i]|mean3,sigma);
    lp_ph2[4]=log(p_outcome[4])+normal_lpdf(outcome[i]|mean4,sigma);
    
    lp[1]=multi1[i]+log(p[1]);
    lp[2]=multi2[i]+log(p[2]);
    lp[3]=multi3[i]+log(p[3]);
    lp[4]=multi4[i]+log(p[4]);
    log_lik[i]=log_sum_exp(lp)+log_sum_exp(lp_ph2);
  }
///////end of transform
}
}

model{
target+=log_lik;
to_vector(to_matrix(beta1))~ normal(0, 4); 
to_vector(to_matrix(beta2))~ normal(0, 4); 
to_vector(to_matrix(beta3))~ normal(0, 4); 
to_vector(to_matrix(beta4))~ normal(0, 4); // weak-moderate prior
p ~ dirichlet(to_vector([2.5,2.5,2.5,2.5]));
mean1~normal(0,15);
mean2~normal(0,15);
mean3~normal(0,15);
mean4~normal(0,15);
sigma~inv_gamma(8,3);
p_outcome ~ dirichlet(to_vector([2.5,2.5,2.5,2.5]));
}

generated quantities{
  matrix[N,M] invMNlogit_c1;
  matrix[N,M] invMNlogit_c2;
  matrix[N,M] invMNlogit_c3;
  matrix[N,M] invMNlogit_c4;
  {
  vector[N] row_sum_c1;
  vector[N] row_sum_c2;
  vector[N] row_sum_c3;
  vector[N] row_sum_c4;
  row_sum_c1=append_col(exp(time_matrix_2*to_matrix(beta1)),rep_vector(1,N))*rep_vector(1,M);
  row_sum_c2=append_col(exp(time_matrix_2*to_matrix(beta2)),rep_vector(1,N))*rep_vector(1,M);
  row_sum_c3=append_col(exp(time_matrix_2*to_matrix(beta3)),rep_vector(1,N))*rep_vector(1,M);
  row_sum_c4=append_col(exp(time_matrix_2*to_matrix(beta4)),rep_vector(1,N))*rep_vector(1,M);
  
  for (i in 1:N){
    invMNlogit_c1[i,]=(append_col(exp(time_matrix_2*to_matrix(beta1)),rep_vector(1,N)))[i,]/row_sum_c1[i];
    invMNlogit_c2[i,]=(append_col(exp(time_matrix_2*to_matrix(beta2)),rep_vector(1,N)))[i,]/row_sum_c2[i];
    invMNlogit_c3[i,]=(append_col(exp(time_matrix_2*to_matrix(beta3)),rep_vector(1,N)))[i,]/row_sum_c3[i];
    invMNlogit_c4[i,]=(append_col(exp(time_matrix_2*to_matrix(beta4)),rep_vector(1,N)))[i,]/row_sum_c4[i];
  }
  }
}

"

###############model 4 classes
fit_4classes<-stan(model_code = model,data=train_dat,iter=5000,warmup = 3000,
                   seed =SEED.START*200+SEED.START,
                   chains = 1,
                   init = list(
                     list(
                       rho=c(0.6,0.6,0.6,0.6),
                       p=c(0.25,0.25,0.25,0.25),
                       beta1=t(cbind(rep(0.1,5),rep(0.001,5))),
                       beta2=t(cbind(rep(0.1,5),rep(0.001,5))),
                       beta3=t(cbind(rep(0.1,5),rep(0.001,5))),
                       beta4=t(cbind(rep(0.1,5),rep(0.001,5))),
                       mean1=6,
                       mean2=6,
                       mean3=6,
                       mean4=6,
                       p_outcome=c(0.25,0.25,0.25,0.25),
                       sigma=1
                     )
                   ))}else{
                     
model<-"
data {
  int<lower=0> n;//N*N_individual
  int<lower=0> N;//times
  int<lower=0> M;//states
  int<lower=0> D;//power
  int<lower=0> N_individual;//people
  int cores[n];//outcome
  matrix[n,1+D] time_matrix;
  matrix[N,1+D] time_matrix_2;
  int outcome[N_individual];
}


parameters {
  real<lower=0,upper=1> rho[4];
  simplex [4] p;
  real beta1 [D+1,M-1];
  real beta2 [D+1,M-1];
  real beta3 [D+1,M-1];
  real beta4 [D+1,M-1];
  //outcome
  real<lower=0> mean1;
  real<lower=0> mean2;
  real<lower=0> mean3;
  real<lower=0> mean4;
  real<lower=0> sigma;//homogeneous standard error
  
}

transformed parameters{
  real log_lik[N_individual];
  {
  ///////tranform
  real lp[4];
  real multi1[N_individual];
  real multi2[N_individual];
  real multi3[N_individual];
  real multi4[N_individual];
  matrix[n,M] exp_omega1 ;
  matrix[n,M] exp_omega2 ;
  matrix[n,M] exp_omega3 ;
  matrix[n,M] exp_omega4 ;
  vector[n] x=rep_vector(1,n);
  vector[M] row_sum=rep_vector(1,M);
  vector[n] row_sum_1;
  vector[n] row_sum_2;
  vector[n] row_sum_3;
  vector[n] row_sum_4;
  exp_omega1=append_col(exp(time_matrix*to_matrix(beta1)),x); 
  exp_omega2=append_col(exp(time_matrix*to_matrix(beta2)),x);
  exp_omega3=append_col(exp(time_matrix*to_matrix(beta3)),x); 
  exp_omega4=append_col(exp(time_matrix*to_matrix(beta4)),x);
  row_sum_1=exp_omega1*row_sum;
  row_sum_2=exp_omega2*row_sum;
  row_sum_3=exp_omega3*row_sum;
  row_sum_4=exp_omega4*row_sum;
  for(i in 1:N_individual){
      multi1[i]=log(exp_omega1[(i-1)*N+1,cores[(i-1)*N+1]]/row_sum_1[(i-1)*N+1]);
      multi2[i]=log(exp_omega2[(i-1)*N+1,cores[(i-1)*N+1]]/row_sum_2[(i-1)*N+1]);
      multi3[i]=log(exp_omega3[(i-1)*N+1,cores[(i-1)*N+1]]/row_sum_3[(i-1)*N+1]);
      multi4[i]=log(exp_omega4[(i-1)*N+1,cores[(i-1)*N+1]]/row_sum_4[(i-1)*N+1]);
      for (j in (i-1)*N+2:i*N){
        if(cores[j]==cores[j-1]){
      multi1[i]=multi1[i]+log(
      (exp_omega1[j,cores[j]]/row_sum_1[j])*(1-rho[1])+rho[1]);
      multi2[i]=multi2[i]+log(
      (exp_omega2[j,cores[j]]/row_sum_2[j])*(1-rho[2])+rho[2]);
      multi3[i]=multi3[i]+log(
      (exp_omega3[j,cores[j]]/row_sum_3[j])*(1-rho[3])+rho[3]);
      multi4[i]=multi4[i]+log(
      (exp_omega4[j,cores[j]]/row_sum_4[j])*(1-rho[4])+rho[4]);
      }
        else{
      multi1[i]=multi1[i]+log(
      (exp_omega1[j,cores[j]]/row_sum_1[j])*(1-rho[1]));
      multi2[i]=multi2[i]+log(
      (exp_omega2[j,cores[j]]/row_sum_2[j])*(1-rho[2]));
      multi3[i]=multi3[i]+log(
      (exp_omega3[j,cores[j]]/row_sum_3[j])*(1-rho[3]));
      multi4[i]=multi4[i]+log(
      (exp_omega4[j,cores[j]]/row_sum_4[j])*(1-rho[4]));
      }
      
    }
    lp[1]=multi1[i]+log(p[1])+normal_lpdf(outcome[i]|mean1,sigma);
    lp[2]=multi2[i]+log(p[2])+normal_lpdf(outcome[i]|mean2,sigma);
    lp[3]=multi3[i]+log(p[3])+normal_lpdf(outcome[i]|mean3,sigma);
    lp[4]=multi4[i]+log(p[4])+normal_lpdf(outcome[i]|mean4,sigma);
    log_lik[i]=log_sum_exp(lp);
  }
///////end of transform
}
}

model{
target+=log_lik;
to_vector(to_matrix(beta1))~ normal(0, 4); 
to_vector(to_matrix(beta2))~ normal(0, 4); 
to_vector(to_matrix(beta3))~ normal(0, 4); 
to_vector(to_matrix(beta4))~ normal(0, 4); // weak-moderate prior
p ~ dirichlet(to_vector([2.5,2.5,2.5,2.5]));
mean1~normal(0,15);
mean2~normal(0,15);
mean3~normal(0,15);
mean4~normal(0,15);
sigma~inv_gamma(8,3);
}

generated quantities{
  matrix[N,M] invMNlogit_c1;
  matrix[N,M] invMNlogit_c2;
  matrix[N,M] invMNlogit_c3;
  matrix[N,M] invMNlogit_c4;
  {
  vector[N] row_sum_c1;
  vector[N] row_sum_c2;
  vector[N] row_sum_c3;
  vector[N] row_sum_c4;
  row_sum_c1=append_col(exp(time_matrix_2*to_matrix(beta1)),rep_vector(1,N))*rep_vector(1,M);
  row_sum_c2=append_col(exp(time_matrix_2*to_matrix(beta2)),rep_vector(1,N))*rep_vector(1,M);
  row_sum_c3=append_col(exp(time_matrix_2*to_matrix(beta3)),rep_vector(1,N))*rep_vector(1,M);
  row_sum_c4=append_col(exp(time_matrix_2*to_matrix(beta4)),rep_vector(1,N))*rep_vector(1,M);
  
  for (i in 1:N){
    invMNlogit_c1[i,]=(append_col(exp(time_matrix_2*to_matrix(beta1)),rep_vector(1,N)))[i,]/row_sum_c1[i];
    invMNlogit_c2[i,]=(append_col(exp(time_matrix_2*to_matrix(beta2)),rep_vector(1,N)))[i,]/row_sum_c2[i];
    invMNlogit_c3[i,]=(append_col(exp(time_matrix_2*to_matrix(beta3)),rep_vector(1,N)))[i,]/row_sum_c3[i];
    invMNlogit_c4[i,]=(append_col(exp(time_matrix_2*to_matrix(beta4)),rep_vector(1,N)))[i,]/row_sum_c4[i];
  }
  }
}

"

###############model 4 classes
fit_4classes<-stan(model_code = model,data=train_dat,iter=5000,warmup = 3000,seed = 100+SEED.START,
                   chains = 1,
                   init = list(
                     list(
                       rho=c(0.6,0.6,0.6,0.6),
                       p=c(0.25,0.25,0.25,0.25),
                       beta1=t(cbind(rep(0.1,5),rep(0.001,5))),
                       beta2=t(cbind(rep(0.1,5),rep(0.001,5))),
                       beta3=t(cbind(rep(0.1,5),rep(0.001,5))),
                       beta4=t(cbind(rep(0.1,5),rep(0.001,5))),
                       mean1=6,
                       mean2=6,
                       mean3=6,
                       mean4=6,
                       sigma=1
                     )
                   ))

                   }

save(fit_4classes, file = paste0("44_hypo", SEED.START, ".rdata"))

