setwd("C:/Users/Romain/Code/Signal et langue/SL_Digit_Recognition/src")
source(file.path(getwd(),"hmmDigitR.r"))
source(file.path(getwd(),"usefullTools.r"))
source(file.path(getwd(),"newHMM.r"))
library("nnet")



#Count the number of dot by cell 
create_image_from_data <- function(data, h, w){
  m<-matrix(0,dim(data)[1],h*w)
  for(j in 1 : dim(data)[1]){
    x<-rep_len(0, h*w)
    ligne<-data[j,]
    for(i in 1:length(ligne)){
      x[ligne[i]] <- x[ligne[i]]+1
      #x[m[i]] <- 1}
    }
    m[j,]<-x
  }
  return(m)
}

create_black_image_from_data <- function(data, h, w){
  m<-matrix(0,dim(data)[1],h*w)
  for(j in 1 : dim(data)[1]){
    x<-rep_len(0, h*w)
    ligne<-data[j,]
    for(i in 1:length(ligne)){
      x[ligne[i]] <- -1
      #x[m[i]] <- 1}
    }
    m[j,]<-x
  }
  return(m)
}
#compute the MSE
test.mse <- function(true, pred) {
  diff <- true - pred
  sqred <- diff * diff
  return (sum(sqred) / length(sqred))
}

#return for each line and column the closest cell containing a dot
launch_probe <-function(m){
  probes<-c()
  for(i in 1:dim(m)[1]){
    r=0
    l=0
    for( j in 1:dim(m)[2]){
      if(m[i,j]!= 0 & l==0){
        l<-j
      }
      if(m[i,dim(m)[2]-j+1]!= 0 & r==0){
        r<-dim(m)[2]-j+1
      }
      if(r!=0 & l!=0){
        break
      }
    }
    probes<- c(probes, l,r)
  }
  for( i in 1:dim(m)[2]){
    u=0
    d=0
    for(j in 1:dim(m)[1]){
      if(m[j,i]!= 0 & u==0){
        u<-j
      }
      if(m[dim(m)[1]-j+1,i]!= 0 & d==0){
        d<-dim(m)[1]-j+1
      }
      if(u!=0 & d!=0){
        break
      }
    }
    probes<- c(probes, u,d)
  }
  return(probes)
}

#Construct the features needed by the NN from the data$obs
construct_Ft <-function(data, nbr, nbc){
  m<-create_image_from_data(data,nbr,nbc)
  dataFt <-matrix(data=0, nrow = dim(m)[1], ncol=(nbr*3+nbc*3))
  
  for(j in 1:dim(m)[1]){
    x <- matrix(m[j,],nrow=nbr,ncol=nbc, byrow=T)
    probes <- launch_probe(x)
    h<-rowSums(x, T, 1)
    w<-colSums(x, T, 1)
    dataFt[j,] <- c(probes,h,w)
  }
  return(dataFt)
}

construct_all_Ft <-function(data, nbr, nbc){
  m<-create_image_from_data(data,nbr,nbc)
  dataFt <-matrix(data=0, nrow = dim(m)[1], ncol=(nbr*3+nbc*3)+dim(m)[2])
  
  for(j in 1:dim(m)[1]){
    x <- matrix(m[j,],nrow=nbr,ncol=nbc, byrow=T)
    probes <- launch_probe(x)
    h<-rowSums(x, T, 1)
    w<-colSums(x, T, 1)
    dataFt[j,] <- c(m[j,],probes,h,w)
  }
  return(dataFt)
}

construct_trigo_Ft <-function(m,size){
  dataFt <-matrix(data=0, nrow = dim(m)[1], ncol=size*2)
  
  image <-create_image_from_data( m,size,1)
  for(j in 1:dim(m)[1]){
    variations <- matrix(data=0, nrow= 1, ncol= dim(m)[2]-1)
    for (i in 1:dim(m)[2]-1){
      variations[1,i] <- ((m[j,i] - m[j,i+1])%%size)+1
    }
    count_variations <- create_image_from_data( variations,size,1)
    
    dataFt[j,] <- c(image[j,], count_variations)
  }
  return(dataFt)
}

crossVal <-function(dataFt, dataTarg, fold, nbN, nbLoop, step)
{
  mseT = 0
  mseV = 0
  scoreT = 0
  scoreV = 0
  bestRate = 0
  best_nn = NULL
  select <- sample(1:dim(dataFt)[1],dim(dataFt)[1])
  indice <- 1:dim(dataFt)[1]
  for(i in 1:fold){
    nbE <- floor(dim(dataFt)[1]/fold)
    valId <- (nbE * (i-1) +1) : (nbE * i)
    cross_res <- learnVal(dataFt, dataTarg,select[-valId], select[valId], nbN, nbLoop, step)
    if(cross_res$bestRate > bestRate){
      bestRate <- cross_res$bestRate
      best_nn <- cross_res$best_nn
    }
    
    mseT <- mseT +cross_res$mseT
    mseV <- mseV + cross_res$mseV
    scoreV <- scoreV + cross_res$scoreVal
    scoreT <- scoreT + cross_res$scoreTrain
    
    
#     par(fg = "black")
#     plot(cross_res$it, cross_res$scoreTrain, type = "l")
#     par(fg = "red")
#     lines(cross_res$it, cross_res$scoreVal, type = "l")
#     
#     par(fg = "black")
#     plot(cross_res$it, cross_res$mseT, type = "l")
#     par(fg = "red")
#     lines(cross_res$it, cross_res$mseV, type = "l")
  }
  mseT <- mseT /5
  mseV <- mseV /5
  scoreV <- scoreV /5
  scoreT <- scoreT /5
  #   cat("min ", min(scores_res), "\n")#error
  return(list(best_nn=best_nn, mseT=mseT, mseV=mseV, scoreTrain=scoreT, scoreValid = scoreV))
}

learnVal <- function (dataFt, dataTarg,trainId, valId, nbN, nbLoop, pas=1e-4){
  #init a new random MLP
  print(nbN)
  new_nn <- nnet(dataFt[trainId,],dataTarg[trainId,], size=nbN, maxit=0, decay=pas,rang = 1)
  best_nn = new_nn
  curr_w <- new_nn$wts
  # compute initial rates / mse and save them
  currTrRate <- test.reco(dataTarg[trainId,], predict(new_nn,dataFt[trainId,]))
  currTrRateVal <- test.reco(dataTarg[valId,], predict(new_nn,dataFt[valId,]))
  currTrMSE <- test.mse(dataTarg[trainId,], predict(new_nn,dataFt[trainId,]))
  currTrMSEVal <- test.mse(dataTarg[valId,], predict(new_nn,dataFt[valId,]))
  scoresT <- c(currTrRate/length(trainId))
  scoresV <- c(currTrRateVal/length(valId))
  mseT <- c(currTrMSE)
  mseV <- c(currTrMSEVal)
  iterations <- c(0)
  bestRate <- 0  
  bestIt <- 0
  cat("Starting Reco rate = ",currTrRate,"\n")
  for(i in 1:nbLoop){
    cat(i," / ", nbLoop, "\n")
    #continue the training
    new_nn <- nnet(dataFt[trainId,],dataTarg[trainId,], size=nbN, maxit=50, decay=pas,rang = 1, Wts=curr_w)
    curr_w <- new_nn$wts
    #compute the rates/MSE
    currTrRate <- test.reco(dataTarg[trainId,], predict(new_nn,dataFt[trainId,]))
    currTrRateVal <- test.reco(dataTarg[valId,], predict(new_nn,dataFt[valId,]))
    currTrMSE <- test.mse(dataTarg[trainId,], predict(new_nn,dataFt[trainId,]))
    currTrMSEVal <- test.mse(dataTarg[valId,], predict(new_nn,dataFt[valId,]))
    
    #save values to plot
    scoresT <- c(scoresT,currTrRate/length(trainId))
    scoresV <- c(scoresV,currTrRateVal/length(valId))
    mseT <- c(mseT,currTrMSE)
    mseV <- c(mseV,currTrMSEVal)
    iterations <- c(iterations, i * 50)
    #save if best
    if(currTrRateVal > bestRate){
      bestRate <- currTrRateVal
      best_nn <- new_nn
      bestIt <- 50 * i
    }
  }
  return (list(nn = best_nn, nbIt=bestIt, bestRate=bestRate, scoreTrain = scoresT, scoreVal = scoresV, it=iterations, mseT=mseT,mseV=mseV))
}

evalNbN<-function(dataFt, dataTarg, fold, step, nbNSet){
  nbN <- NULL
  mseT <- NULL
  mseV <- NULL
  scoreT <-NULL
  scoreV <- NULL
  for(i in nbNSet){
    cat("NbN =============>", i)
    res<-crossVal(dataFt, dataTarg, fold , i, 10, step)
    mseT<-c(mseT, min(res$mseT))
    mseV<-c(mseV, min(res$mseV))
    scoreT <- c(scoreT, max(res$scoreTrain))
    scoreV <- c(scoreV, max(res$scoreValid))
    nbN <- c(nbN, i)
  }
  
  return(list(mseT= mseT, mseV=mseV, scoreT=scoreT, scoreV=scoreV))
}

evalStep<-function(dataFt, dataTarg, fold, step, nbN){
  bstep <- NULL
  mseT <- NULL
  mseV <- NULL
  scoreT <-NULL
  scoreV <- NULL
  for(i in step){
    cat("step =============>", i)
    res<-crossVal(dataFt, dataTarg, fold , nbN, 10, i)
    mseT<-c(mseT, min(res$mseT))
    mseV<-c(mseV, min(res$mseV))
    scoreT <- c(scoreT, max(res$scoreTrain))
    scoreV <- c(scoreV, max(res$scoreValid))
    bstep <- c(bstep, i)
  }
  
  return(list(mseT= mseT, mseV=mseV, scoreT=scoreT, scoreV=scoreV))
}

neural_fusion_data<-function(nn, hmm, data, dataFt, dataTarg){
  print("############# Calcul NN #############")
  res_nn<-predict(nn, dataFt)
  print("############# Calcul HMM #############")
  res_hmm<-exp(classifyDigit(hmm, data$obs))
  return(cbind(res_nn, res_hmm))
}


# data<-loadAll(file.path(getwd(),"data/Test_compute_symbol_5_4Digit"))
# # # m<-create_image_from_data(data$obs,5,3)
# dataFt<-construct_all_Ft(data$obs, 5, 4)


# data<-loadAll("C:/Users/Romain/Code/Signal et langue/data_Digits_HMM/dir8/Train_compute_symbol_dir_8Digit")
# dataFt<-construct_trigo_Ft(data$obs,8)




# data<-Load_Obs("C:/Users/Romain/Code/Signal et langue/data_Digits_HMM/Data7x5/Train_compute_symbol_7_5Digit9.txt")
# m<-create_black_image_from_data(data,7,5)
# image(matrix(m[179,],5,7, byrow=F),)
# dataFt<-construct_Ft(m, 7, 5)

# digitCl<-data$cl
# 
# select <- ((digitCl == 1) | (digitCl == 2)| (digitCl == 3)| (digitCl == 4)| (digitCl == 5)| (digitCl == 6)| (digitCl == 7)| (digitCl == 8)| (digitCl == 9)| (digitCl == 0))
# 
# dataTarg <- class.ind(digitCl[select])

# 
# load(file.path(getwd(),'Results/3_states/matrice_HMM_8.rdata'))
# load(file.path(getwd(),"Results/neural_network/nnet8.rda"))
# dataFt <- neural_fusion_data(nnet8,matrice_HMM_8,data, dataFt, dataTarg)


# res<-crossVal(dataFt, dataTarg, 5 , 21, 20, 1e-1)
#  res<-evalStep(dataFt, dataTarg, 5, list(1e-3,5e-3,1e-2,5e-2,1e-1,5e-1,1), nbN= 21)
# 
# nbnS=1:30
#  res<-evalNbN(dataFt, dataTarg, 5, 1e-1, nbNSet= nbnS)
# 
# 
# 
# par(fg = "blue")
# plot(nbnS, res$mseT, type = "l")
# par(fg = "red")
# lines(nbnS, res$mseV, type = "l")
# 
# 
# par(fg = "blue")
# plot(nbnS, res$scoreT, type = "l")
# par(fg = "red")
# lines(nbnS, res$scoreV, type = "l")

# nn_fusion_8 <- nnet(dataFt,dataTarg, size=25, maxit=500, decay=1e-1,rang = 1)
# cat(predict(new_nn,dataFt[300,]), dataTarg[300,])
# save(nn_fusion_8, file=file.path(getwd(),"Results/neural_network/nn_fusion_8.rda"))


# load(file.path(getwd(),"Results/neural_network/nnet5x4.rda"))
# load(file.path(getwd(),"Results/3_states/matrice_HMM_5X4.rdata"))
# # 
# res<-test.reco(dataTarg, predict(nnet_5_4, dataFt))
# print(res/dim(dataTarg)[1])
# 
# reco <- test.reco(dataTarg_5_4, exp(classifyDigit(matrice_HMM_5X4,data_5_4$obs)))
# print("HMM_5_4")
# print(reco/dim(dataTarg_5_4)[1])