# Loop through Easy, Medium, then Hard
# Crossed with a range of 2-phase versions of Indep, Semi, Fully (Concord)
library(TraMineR)
library(flexmix)
library(tidyr)
library(ggseqplot)
library(Hmisc)

setwd("~/Dropbox/WGMBC/Mixture-Sequences-Bohan/Marc_Code")

#some of initial code from chatGPT - 2Jul25
#14Sep26: inclreased to 100 trials & saved 'truth'
# Parameters
nTrials <- 100           # simulations per condition (extra allows for non-convergent)
n <- 600                # number of sequences
##seq_length <- 12         # length of each sequence (there are 2 phases, of seq_length each) - run1
seq_length <- 24         # length of each sequence (there are 2 phases, of seq_length each) - run2
#
K1 <- 3                 # number of latent sequence classes - phase 1
K2 <- 3                 # number of latent sequence classes - phase 2
K3 <- 3                 # number of latent outcome classes
rho <- c(.6,.6,.6)
states <- c("A", "B", "C", "D", "E", "F") # 1st three dominate first phase; last 3 dominate 2nd phase
seqTypes <- c("E","M","H") #easy, medium hard sequences to detect
modChoices <- list(c("I","I"),c("I","Sa"),c("I","Sb"),c("S","Sa"),c("S","Sb"),c("S","Bal"),c("S","Pl1"),c("S","Pl2"),c("C","C")) #indep, semi-concord, concord

datName <- paste0("simDat",as.character(seq_length),"_") #originally nothing was added as a suffix (for length 12)

#part I: data gen
modCode <- "333"

nChoices <- length(modChoices)
init_class_prob_list <- vector("list",nChoices)
for (i in 1:nChoices) init_class_prob_list[[i]] <-rep(1/3,3)
# Class-specific multinomial probabilities
# Each row corresponds to class 1, 2, or 3

seq_probs_list <- list(
  E = list(
    Phase_1 = matrix(
      c(0.80, 0.07, 0.07, 0.02, 0.02, 0.02,  # Class 1
      0.07, 0.80, 0.07, 0.02, 0.02, 0.02,  # Class 2
      0.07, 0.07, 0.80, 0.02, 0.02, 0.02),   # Class 3
    3,6,byrow=T),
    Phase_2 = matrix(
      c(0.02, 0.02, 0.02, 0.80, 0.07, 0.07,  # Class 1
      0.02, 0.02, 0.02, 0.07, 0.80, 0.07,  # Class 2
      0.02, 0.02, 0.02, 0.07, 0.07, 0.80),  # Class 3
    3,6,byrow=T)
  ),
  M = list(
    Phase_1 = matrix(
      c(0.11, 0.40, 0.40, 0.03, 0.03, 0.03,  # Class 1
      0.40, 0.11, 0.40, 0.03, 0.03, 0.03,  # Class 2
      0.40, 0.40, 0.11, 0.03, 0.03, 0.03),   # Class 3
    3,6,byrow=T),
    Phase_2 = matrix(
      c(0.03, 0.03, 0.03, 0.11, 0.40, 0.40,  # Class 1
      0.03, 0.03, 0.03, 0.40, 0.11, 0.40,  # Class 2
      0.03, 0.03, 0.03, 0.40, 0.40, 0.11),   # Class 3
    3,6,byrow=T)
  ),
  H = list(
    Phase_1 = matrix(
      c(0.20, 0.34, 0.34, 0.04, 0.04, 0.04,  # Class 1
      0.34, 0.20, 0.34, 0.04, 0.04, 0.04,  # Class 2
      0.34, 0.34, 0.20, 0.04, 0.04, 0.04),   # Class 3
    3,6,byrow=T),
    Phase_2 = matrix(
      c(0.04, 0.04, 0.04, 0.20, 0.34, 0.34,  # Class 1
      0.04, 0.04, 0.04, 0.34, 0.20, 0.34,  # Class 2
      0.04, 0.04, 0.04, 0.34, 0.34, 0.20),   # Class 3
    3,6,byrow=T)
# ORIGINAL HARD level (too hard):
#Phase_1 = matrix(
#  c(0.24, 0.32, 0.32, 0.04, 0.04, 0.04,  # Class 1
#  0.32, 0.24, 0.32, 0.04, 0.04, 0.04,  # Class 2
#  0.32, 0.32, 0.24, 0.04, 0.04, 0.04),   # Class 3
#3,6,byrow=T),
#Phase_2 = matrix(
#  c(0.04, 0.04, 0.04, 0.24, 0.32, 0.32,  # Class 1
#  0.04, 0.04, 0.04, 0.32, 0.24, 0.32,  # Class 2
#  0.04, 0.04, 0.04, 0.32, 0.32, 0.24),   # Class 3
#3,6,byrow=T)
  )
)


#transition probabilities
cond_probs_list <- list(
I=matrix(
c(0.38,0.33,0.29,  # Indep of class; unequal probs
  0.38,0.33,0.29,  #
  0.38,0.33,0.29),3,3,byrow=T),  #
S=matrix(
c(0.04, 0.48, 0.48,  # Class 1 leads to NOT mu1
  0.48, 0.04, 0.48,  # Class 2 leads to NOT mu2
  0.48, 0.48, 0.04),3,3,byrow=T),   # Class 3 leads to NOT mu3
C=matrix(
c(1, 0, 0,  # Class 1 leads to mu1
  0, 1, 0,  # Class 2 leads to mu2
  0, 0, 1),3,3,byrow=T)
)

dep_phase2 <- function(mat,new.names=c("K2_1","K2_2","K2_3")) {
    #convert to take single col of each mat and 'repeat' as a row - so no longer deps on phase 1.
    # ABC
    # DEF
    # GHI
    # becomes:
    # ABC   DEF  GHI
    # ABC   DEF  GHI
    # ABC   DEF  GHI
    ####
    dims <- dim(mat)
    matList <- vector("list",dims[1])
    names(matList) <- new.names
    for (i in 1:dims[1]) {
      matList[[i]] <- matrix(mat[,i],dims[1],dims[2],byrow=T)
    }
    matList
}

#this needs to be thought through a bit more, but it's a start (basically this is the $\pi'''_{k_3|k_2,k_1}$ matrix
outMat1 <- matrix(c(0.80, 0.10, 0.10,  # Class 1 leads to mu1
                    0.10, 0.80, 0.10,  # Class 2 leads to mu2
                    0.10, 0.10, 0.80),3,3,byrow=T) # Class 3 leads to mu3
matrix_out_probs <- list(
  I = list(
    k2_1 = cond_probs_list$I,
    k2_2 = cond_probs_list$I,
    k2_3 = cond_probs_list$I
  ),
  Sa = list(
    k2_1 = outMat1,
    k2_2 = outMat1,
    k2_3 = outMat1
  ),
  Sb = dep_phase2(outMat1),
  Bal = list(
    k2_1 = matrix(
      c(0.80, 0.10, 0.10,  # k1 = 1
      0.45, 0.45, 0.10,  # k1 = 2
      0.45, 0.10, 0.45),   # k1 = 3
    3,3,byrow=T),
    k2_2 = matrix(
      c(0.45, 0.45, 0.10,  # k1 = 1
      0.10, 0.80, 0.10,  # k1 = 2
      0.10, 0.45, 0.45),   # k1 = 3
    3,3,byrow=T),
    k2_3 = matrix(
      c(0.45, 0.10, 0.45,  # k1 = 1
      0.10, 0.45, 0.45,  # k1 = 2
      0.10, 0.10, 0.80),   # k1 = 3
    3,3,byrow=T)
  ),
  Pl1 = list(
    k2_1 = matrix(
      c(0.80, 0.10, 0.10,  # k1 = 1
      0.30, 0.60, 0.10,  # k1 = 2
      0.30, 0.10, 0.60),   # k1 = 3
     3,3,byrow=T),
    k2_2 = matrix(
      c(0.60, 0.30, 0.10,  # k1 = 1
      0.10, 0.80, 0.10,  # k1 = 2
      0.10, 0.30, 0.60),   # k1 = 3
    3,3,byrow=T),
    k2_3 = matrix(
      c(0.60, 0.10, 0.30,  # k1 = 1
      0.10, 0.60, 0.30,  # k1 = 2
      0.10, 0.10, 0.80),   # k1 = 3
    3,3,byrow=T)
  ),
  Pl2 = list(
    k2_1 = matrix(
      c(0.80, 0.10, 0.10,  # k1 = 1
      0.60, 0.30, 0.10,  # k1 = 2
      0.60, 0.10, 0.30),   # k1 = 3
    3,3,byrow=T),
    k2_2 = matrix(
      c(0.30, 0.60, 0.10,  # k1 = 1
      0.10, 0.80, 0.10,  # k1 = 2
      0.10, 0.60, 0.30),   # k1 = 3
    3,3,byrow=T),
    k2_3 = matrix(
      c(0.30, 0.10, 0.60,  # k1 = 1
      0.10, 0.30, 0.60,  # k1 = 2
      0.10, 0.10, 0.80),   # k1 = 3
    3,3,byrow=T)
  ),
  C = dep_phase2(cond_probs_list$C)
)
#outcomes
mu <- c(-1,0,1) #keeps rmse on scale similar to r.sq
sd <- .4

##start.seed <- 1000000
start.seed <- 2000000
df_long_list <- vector("list",nTrials)
train_dat_list <- vector("list",nTrials)
K <- c(K1,K2,K3) #for indexing later

nTypes <- length(seqTypes)
seqList <- vector("list",nTypes)
names(seqList) <- seqTypes #easy, medium, etc.
for (i in 1:nTypes) {
    seqList[[i]] <- vector("list",nChoices) #storage of all runs
    names(seqList[[i]]) <- sapply(modChoices,paste0,collapse="")
}
plot.it <- T
par(mfrow=c(1,1),ask=F)
for (seqType in seq_along(seqTypes)) {
  seq_probs <- seq_probs_list[[seqType]] #a list of size 2, one per phase
  for (modCh in seq_along(modChoices)) {
    seqList[[seqType]][[modCh]] <- vector("list",nTrials)
    #set up the output probs:
    out_probs <- simplify2array(matrix_out_probs[[modChoices[[modCh]][2]]])
    #choose cond_probs
    for (m in 1:nTrials) {
      set.seed(start.seed+seqType*100000+modCh*10000+m)
      # Class membership probabilities (sequences)
      #initial state
      Z1 <- Z <- sample(1:K1, n, replace = TRUE, prob = init_class_prob_list[[modCh]]) # class in phase 1
      #loop through phases
      for (ph in 1:2) {
        sequences <- matrix(NA, nrow = n, ncol = seq_length) #baseline one phase sequence
        #browser()
        seq_prob <- seq_probs[[ph]] #token probs
        # Simulate sequences & outcomes
        for (i in 1:n) {
          sequences[i, ] <- sample(states, seq_length, replace = TRUE, prob = seq_prob[Z[i],])
          stayers <- rbinom(seq_length, 1 ,prob = rho[Z[i]])
          for(j in 2:seq_length) {
            if (stayers[j]==1) {
              sequences[i,j] <- sequences[i,j-1]
            }
          }
          #setup for phase 2 - uses class from phase 1 and overwrites it
          Z[i] <- sample(1:K2, 1, prob = cond_probs_list[[modChoices[[modCh]][1]]][Z1[i],]) # class in phase 1 (overwriting OK, since done with i at this point) -- not set up for more than 2 phases. The [[1]] here indicates phase 1->2 xition type, such as 'I'
        }
        #Store this portion of the sequence
        if (ph==1) {
            seqList[[seqType]][[modCh]][[m]]$seq <- sequences
            Z2 <- Z #true classes, phase 2 (since it's the end of first phase)
        } else {
            seqList[[seqType]][[modCh]][[m]]$seq <- cbind(seqList[[seqType]][[modCh]][[m]]$seq,sequences)
            idx_mat <- rbind(
              cbind(Z1, 1, Z2),  # Coordinates for col 1 of out_probs
              cbind(Z1, 2, Z2),  # Coordinates for col 2 of out_probs
              cbind(Z1, 3, Z2)   # Coordinates for col 3 of out_probs
            )
            extracted_values <- out_probs[idx_mat]
            result_matrix <- matrix(extracted_values, nrow = n, ncol = K3)
            Z3 <- Hmisc::rMultinom(probs = result_matrix, m = 1)[,1] #make it a vector
            seqList[[seqType]][[modCh]][[m]]$class <- cbind(Z1,Z2,Z3)
            colnames(seqList[[seqType]][[modCh]][[m]]$class) <- c("Phase1","Phase2","Outcome")
            #assign outcomes
            seqList[[seqType]][[modCh]][[m]]$y <- rnorm(n,mean=mu[Z3],sd=sd)
        }
      }
      if (plot.it) { #remove after more testing
          s1 <- seqdef(seqList[[seqType]][[modCh]][[m]]$seq) #take first one
          idx<-order(s1[,12],s1[,13])
          #idx <- order(Z2,Z1)
          seqIplot(s1[idx,],sortv=NULL)
      }
    }
  }
}

require(cfda)
require(TraMineR)
require(cluster)
#####
for (seqType in seq_along(seqTypes)) {
    for (modCh in seq_along(modChoices)) {
        true_clust_list <- train_dat_list <- df_long_list <- vector("list",nTrials)
        for (m in 1:nTrials) {
            # Convert to data frame
            df <- as.data.frame(seqList[[seqType]][[modCh]][[m]]$seq)
            df$id <- 1:n
            df_long <- tidyr::pivot_longer(df,cols = starts_with("V"),names_to = "time",values_to = "state")
            df_long$time <- rep(0:(seq_length*2-1),times=n)
            df_long$y <- rep(seqList[[seqType]][[modCh]][[m]]$y,each=2*seq_length) #useful in other contexts
            time_matrix <-as.matrix(cbind(1,as.numeric(as.factor(df_long$time))))
            train_dat <- list(
            K1=K1, K2=K2, K3=K3, n=n*seq_length*2, N=seq_length*2, M=6, D=0,
            N_individual=n,
            Ys=as.numeric(factor(df_long$state)),
            X=time_matrix[,1,drop=F],
            Yo=seqList[[seqType]][[modCh]][[m]]$y) #just bring in outcomes (1 per seq) here.
            #save in list
            df_long_list[[m]] <- df_long
            train_dat_list[[m]] <- train_dat
            true_clust_list[[m]] <- seqList[[seqType]][[modCh]][[m]]$class
        }
        #FIX
        true_params <- list(init_probs=init_class_prob_list[[modCh]], seq_probs=seq_probs_list[[seqType]], phase_trans_probs= cond_probs_list[[modChoices[[modCh]][1]]], rho=rho,out_probs=matrix_out_probs[[modChoices[[modCh]][2]]],mu=mu,sd=sd)
        #added 2x to filename for 24 length phases.
        save(df_long_list, train_dat_list, true_clust_list, nTrials, true_params, file=paste0( datName,modCode,"_",seqTypes[seqType],"_",paste0(modChoices[modCh][[1]],collapse=""),".rdata"))
    }
}



#minimum needed to run from here:
plot.it <- T
basis <- create.bspline.basis(c(0, seq_length*2-1), nbasis = 7, norder = 4)
#####

if (plot.it) par(mfrow=c(3,3))
####
runList <- vector("list",nTypes)
names(runList) <- seqTypes #easy, medium, etc.
for (i in 1:nTypes) {
    runList[[i]] <- vector("list",nChoices) #storage of all runs
    names(runList[[i]]) <- sapply(modChoices,paste0,collapse="")
}

#### add a function on all data, divide into train... test; pass a formula
rmse <- function(data,fmla=y~.,cfda=F,basis=NULL,eigen.cutoff=1) {
    n <- length(unique(data$id)) #assume in order 1,2,...,n
    train_df <- data[data$id <= n/2,]
    test_df <- data[data$id > n/2,]
    if (!cfda) {
        fit.train <- lm(fmla,data=train_df)
        dat.train <- train_df #same structure
        dat.test <- test_df
        yhat.train <- predict(fit.train,newdata=train_df)
        yhat.test <- predict(fit.train,newdata=test_df)
    } else {
        fmca <- compute_optimal_encoding(train_df, basis=basis, nCores = 7,verbose=F) # a bit kludgey
        cut.pt <- sum(fmca$eigen>=eigen.cutoff)
        dat.train <- data.frame(fmca$pc[,1:cut.pt])
        dat.train$y <- train_df$y[!duplicated(train_df$id)] #need to remove repetitions
        fit.train <- lm(fmla,data=dat.train)
        dat.test <- data.frame(predict(fmca,newdata=test_df, nCores = 7,verbose=F)[,1:cut.pt])
        dat.test$y <- test_df$y[!duplicated(test_df$id)]  #need to remove repetitions
        yhat.train <- predict(fit.train,newdata=dat.train)
        yhat.test <- predict(fit.train,newdata=dat.test)
    }
    rmse <- function(actual, predicted) {
      sqrt(mean((actual - predicted)^2))
    }
    train_rmse <- rmse(dat.train$y, yhat.train)
    test_rmse <- rmse(dat.test$y, yhat.test)
    list(train.rmse=train_rmse,test.rmse=test_rmse)
}
####

runName <- paste0("RunList",as.character(seq_length),"_")

##rerun w.o fmca:

eigen.cutoff <- 1 #cfda may be sensitive to this choice.
###run all models + oracles. Note: needs to be split into train/test and use rmse not r.sq
for (seqType in seq_along(seqTypes)) {
    for (modCh in seq_along(modChoices)) {
        load(file=paste0( datName,modCode,"_",seqTypes[seqType],"_",paste0(modChoices[modCh][[1]],collapse=""),".rdata"))
        
##        fit_list <- vector("list",nTrials)
##        rmse_list <- vector("list",nTrials)
        for (m in 1:nTrials) {
            cat(".")
            #### Here is where a split into train/test with n/2 data points each could be made
            df <- df_long_list[[m]]
            ####cfda
  ##          fmca <- compute_optimal_encoding(df, basis, nCores = 7,verbose=F)
  ##          cut.pt <- sum(fmca$eigen>=eigen.cutoff)
            cut.pt <- sum(fit_list[[m]]$eigen>=eigen.cutoff)
  ##          dat <- data.frame(fmca$pc[,1:cut.pt])
            dat <- data.frame(fit_list[[m]]$pc[,1:cut.pt])
            dat$y <- df_long_list[[m]]$y[df_long_list[[m]]$time==0]
  ##          fit.cfda <- lm(y~.,data=dat)  #train... test
            ####counts
            df$state <- factor(df$state,levels=states)
            df1 <- df[df$time <seq_length,] #ph1, given seq_length is for one phase.
            df2 <- df[df$time >=seq_length,] #ph2
            counts_ph1 <- table(df1$id, df1$state)
            counts_ph2 <- table(df2$id, df2$state)
            ids <- unique(df1$id)
            dat2 <- data.frame(id=ids,y=dat$y,cbind(counts_ph1,counts_ph2))
            fit.counts <- lm(y~.,data=dat2[,-1]) #train... test
            ####oracle: best clustering
            clusts <- true_clust_list[[m]]
            dat3 <- data.frame(clusts[,1:2])
            dat3[] <- lapply(dat3,factor)
            dat3$y <- dat$y
            dat3$id <- ids
            fit.orac1 <- lm(y~.*.,data=dat3[,-1]) #best if we can interact the two sets of clusters! #train... test
            ####oracle: knows the outcome bin
            dat4 <- data.frame(id=ids,y=dat$y,clust=factor(clusts[,3]))
            fit.orac2 <- lm(y~.,data=dat4[,-1]) #train... test
            ####cluster - under constr. better with wide version, and that's a pain here.

##            fit_list[[m]] <- list(pc=fmca$pc,eigen=fmca$eigen,fit.cfda=fit.cfda,fit.counts=fit.counts, fit.orac1=fit.orac1, fit.orac2=fit.orac2)
            fit_list[[m]] <- list(pc=fit_list[[m]]$pc,eigen=fit_list[[m]]$eigen,fit.cfda=fit_list[[m]]$fit.cfda,fit.counts=fit.counts, fit.orac1=fit.orac1, fit.orac2=fit.orac2)
            #### now just rmse train/test:
            rmse.cfda <- rmse(df,cfda=T,basis=basis)
            rmse.counts <- rmse(dat2,y~.)
            rmse.orac1 <- rmse(dat3,y~.*.)
            rmse.orac2 <- rmse(dat4,y~.)
            rmse.all <- sapply(list(rmse.cfda,rmse.counts,rmse.orac1,rmse.orac2),identity) #matrix
            rmse_list[[m]] <- rmse.all
        }
        cat("\n")
        #### SAVE
        rmse_arr <- simplify2array(rmse_list)
        dimnames(rmse_arr) <- list(c("train","test"),c("cfda","counts","orac1","orac2"),NULL)
        runList[[seqType]][[modCh]] <- list(fit_list=fit_list,rmse_train=t(rmse_arr[1,,]),rmse_test=t(rmse_arr[2,,]))
        rsq_mat <- sapply(fit_list,function(trial) {
            fit_names <- grep("fit", names(trial), value = TRUE)
            # Extract R-squared for just those four models
            sapply(trial[fit_names], function(fit) {
                summary.lm(fit)$r.squared
            })
        })
        rsq_df <- as.data.frame(t(rsq_mat))
        rsq_df$trial_id <- 1:nrow(rsq_df)
        if (plot.it) {
            #matplot(rsq_df[,-5], type='l',lwd=1,col=c(2:4,1), lty=1,ylim=c(0,1),xlab='Trial',main=paste0( datName,modCode,"_",seqTypes[seqType],"_",paste0(modChoices[modCh][[1]],collapse="")))
            matplot(t(rmse_arr[1,,]), type='l',lwd=1,col=c(2:4,1),lty=1,ylim=c(0.3,1.2),xlab='Trial',ylab='RMSE',main=paste0("simDat",modCode,"_",seqTypes[seqType],"_",paste0(modChoices[modCh][[1]],collapse="")))
            matlines(t(rmse_arr[2,,]), type='b',lwd=2,col=c(2:4,1),lty=5)
        }
    }
}
save(runList,file=paste0(runName,modCode,"_all.rdata"))


#STOP POINT for cut/paste approach

#minimum needed to run from here:
seqTypes <- c("E","M","H") #easy, medium hard sequences to detect
modChoices <- list(c("I","I"),c("I","Sa"),c("I","Sb"),c("S","Sa"),c("S","Sb"),c("S","Bal"),c("S","Pl1"),c("S","Pl2"),c("C","C")) #indep, semi-concord, concord
plot.it <- T
basis <- create.bspline.basis(c(0, seq_length*2-1), nbasis = 7, norder = 4)
load(paste0(runName,modCode,"_all.rdata"))
#####

#plot all 3 types, one after the other:
par(mfrow=c(3,3),ask = TRUE)
#par(mfrow=c(3,3),ask = FALSE)
for (seqType in seq_along(seqTypes)) {
    for (modCh in seq_along(modChoices)) {
        fit_list <- runList[[seqType]][[modCh]]$fit_list
        rmse_train <-runList[[seqType]][[modCh]]$rmse_train
        rmse_test <-runList[[seqType]][[modCh]]$rmse_test
        rsq_mat <- sapply(fit_list,function(trial) {
            fit_names <- grep("fit", names(trial), value = TRUE)
            # Extract R-squared for just those four models
            sapply(trial[fit_names], function(fit) {
                summary.lm(fit)$r.squared
            })
        })
        rsq_df <- as.data.frame(t(rsq_mat))
        rsq_df$trial_id <- 1:nrow(rsq_df)
        if (plot.it) {
            #matplot(rsq_df[,-5],type='b',lwd=c(2,3,2,3),col=c(2:4,1),lty=c(1,4,1,2),xlab='Trial',main=paste0("simDat",modCode,"_",seqTypes[seqType],"_",paste0(modChoices[modCh][[1]],collapse="")),ylim=c(0,1))
            matplot(rmse_test, type='b',lwd=2,col=c(2:4,1),lty=5,ylim=c(0.3,1.2),xlab='Trial',ylab='RMSE',main=paste0("simDat",modCode,"_",seqTypes[seqType],"_",paste0(modChoices[modCh][[1]],collapse="")))
            #matlines(rmse_train, type='l',lwd=1,col=c(2:4,1),lty=1)
            #s1 <- seqdef(seqList[[seqType]][[modCh]][[1]]$seq) #take first trial
            #idx<-order(seqList[[seqType]][[modCh]][[1]]$class[,2])
            #seqIplot(s1[idx,],sortv=NULL,main=paste0("simDat",modCode,"_",seqTypes[seqType],"_",paste0(modChoices[modCh][[1]],collapse="")))
        }
    }
}


###TESTS#####
seqType<-1; modCh <- 3
tbl12 <- tbl13 <- tbl23 <- matrix(0,3,3)
pv12 <- pv13 <- pv23 <- vector("numeric",nTrials)
for (m in 1:nTrials) {
  tbl12 <- tbl12 + (tt12 <- table(seqList[[seqType]][[modCh]][[m]]$class[,1],seqList[[seqType]][[modCh]][[m]]$class[,2]))
  pv12[m] <- chisq.test(tt12)$p.value
  tbl13 <- tbl13 + (tt13<-table(seqList[[seqType]][[modCh]][[m]]$class[,1],seqList[[seqType]][[modCh]][[m]]$class[,3]))
  pv13[m] <- chisq.test(tt13)$p.value
  tbl23 <- tbl23 + (tt23<-table(seqList[[seqType]][[modCh]][[m]]$class[,2],seqList[[seqType]][[modCh]][[m]]$class[,3]))
  pv23[m] <- chisq.test(tt23)$p.value
}
round(prop.table(tbl12),2)
mean(pv12<.05)
round(prop.table(tbl13),2)
mean(pv13<.05)
round(prop.table(tbl23),2)
mean(pv23<.05)





