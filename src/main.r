setwd("C:/Users/Romain/Code/Signal et langue/SL_Digit_Recognition/src")
source(file.path(getwd(),"hmmDigitR.r"))
source(file.path(getwd(),"nnDigitR.r"))
source(file.path(getwd(),"usefullTools.r"))
source(file.path(getwd(),"newHMM.r"))


### HMM
load(file.path(getwd(),'Results/3_states/matrice_HMM_5X3.rdata'))
load(file.path(getwd(),'Results/3_states/matrice_HMM_5X4.rdata'))
load(file.path(getwd(),'Results/3_states/matrice_HMM_8.rdata'))
load(file.path(getwd(),'Results/3_states/matrice_HMM_16.rdata'))

### NN
load(file.path(getwd(),"Results/neural_network/nnet5x3.rda"))
load(file.path(getwd(),"Results/neural_network/nnet5x4.rda"))
load(file.path(getwd(),"Results/neural_network/nnet7x5.rda"))
load(file.path(getwd(),"Results/neural_network/nnet8.rda"))
load(file.path(getwd(),"Results/neural_network/nnet16.rda"))

### Fusion
load(file.path(getwd(),"Results/neural_network/nn_fusion_5_3.rda"))
load(file.path(getwd(),"Results/neural_network/nn_fusion_5_4.rda"))
load(file.path(getwd(),"Results/neural_network/nn_fusion_8.rda"))
load(file.path(getwd(),"Results/neural_network/nn_fusion_16.rda"))

### Data
data_5_3<-loadAll(file.path(getwd(),"data/Test_compute_symbol_5_3Digit"))
data_5_4<-loadAll(file.path(getwd(),"data/Test_compute_symbol_5_4Digit"))
data_7_5<-loadAll(file.path(getwd(),"data/Test_compute_symbol_7_5Digit"))
data_8<-loadAll(file.path(getwd(),"data/Test_compute_symbol_dir_8Digit"))
data_16<-loadAll(file.path(getwd(),"data/Test_compute_symbol_dir_16Digit"))

### Creation des features pour les NN
dataFt_5_3<-construct_all_Ft(data_5_3$obs, 5 , 3)
dataFt_5_4<-construct_all_Ft(data_5_4$obs, 5 , 4)
dataFt_7_5<-construct_all_Ft(data_7_5$obs, 7 , 5)
dataFt_8<-construct_trigo_Ft(data_8$obs,8)
dataFt_16<-construct_trigo_Ft(data_16$obs,16)

### Creation des objectifs
digitCl <- data_5_3$cl
select <- ((digitCl == 1) | (digitCl == 2)| (digitCl == 3)| (digitCl == 4)| (digitCl == 5)| (digitCl == 6)| (digitCl == 7)| (digitCl == 8)| (digitCl == 9)| (digitCl == 0))
dataTarg_5_3 <- class.ind(digitCl[select])
digitCl <- data_5_4$cl
select <- ((digitCl == 1) | (digitCl == 2)| (digitCl == 3)| (digitCl == 4)| (digitCl == 5)| (digitCl == 6)| (digitCl == 7)| (digitCl == 8)| (digitCl == 9)| (digitCl == 0))
dataTarg_5_4 <- class.ind(digitCl[select])
digitCl <- data_7_5$cl
select <- ((digitCl == 1) | (digitCl == 2)| (digitCl == 3)| (digitCl == 4)| (digitCl == 5)| (digitCl == 6)| (digitCl == 7)| (digitCl == 8)| (digitCl == 9)| (digitCl == 0))
dataTarg_7_5 <- class.ind(digitCl[select])
digitCl <- data_8$cl
select <- ((digitCl == 1) | (digitCl == 2)| (digitCl == 3)| (digitCl == 4)| (digitCl == 5)| (digitCl == 6)| (digitCl == 7)| (digitCl == 8)| (digitCl == 9)| (digitCl == 0))
dataTarg_8 <- class.ind(digitCl[select])
digitCl <- data_16$cl
select <- ((digitCl == 1) | (digitCl == 2)| (digitCl == 3)| (digitCl == 4)| (digitCl == 5)| (digitCl == 6)| (digitCl == 7)| (digitCl == 8)| (digitCl == 9)| (digitCl == 0))
dataTarg_16 <- class.ind(digitCl[select])

### Score HMM

reco <- test.reco(dataTarg_5_3, exp(classifyDigit(matrice_HMM_5X3,data_5_3$obs)))
print("HMM_5_3")
print(reco/dim(dataTarg_5_3)[1])

reco <- test.reco(dataTarg_5_4, exp(classifyDigit(matrice_HMM_5X4,data_5_4$obs)))
print("HMM_5_4")
print(reco/dim(dataTarg_5_4)[1])

reco <- test.reco(dataTarg_8, exp(classifyDigit(matrice_HMM_8,data_8$obs)))
print("HMM_8")
print(reco/dim(dataTarg_8)[1])

reco <- test.reco(dataTarg_16, exp(classifyDigit(matrice_HMM_16,data_16$obs)))
print("HMM_16")
print(reco/dim(dataTarg_16)[1])


###Score NN
print("NN_5_3")
reco<-test.reco(dataTarg_5_3, predict(nnet_5_3, dataFt_5_3))
print(reco/dim(dataTarg_5_3)[1])

print("NN_5_4")
reco<-test.reco(dataTarg_5_4, predict(nnet_5_4, dataFt_5_4))
print(reco/dim(dataTarg_5_4)[1])

print("NN_7_5")
reco<-test.reco(dataTarg_7_5, predict(nnet_7_5, dataFt_7_5))
print(reco/dim(dataTarg_7_5)[1])

print("NN_8")
reco<-test.reco(dataTarg_8, predict(nnet8, dataFt_8))
print(reco/dim(dataTarg_8)[1])

print("NN_16")
reco<-test.reco(dataTarg_16, predict(nnet16, dataFt_16))
print(reco/dim(dataTarg_16)[1])

### Data fusion
dataFt_fusion_5_3 <- neural_fusion_data(nnet_5_3,matrice_HMM_5X3,data_5_3, dataFt_5_3, dataTarg_5_3)
dataFt_fusion_5_4 <- neural_fusion_data(nnet_5_4,matrice_HMM_5X4,data_5_4, dataFt_5_4, dataTarg_5_4)
dataFt_fusion_8 <- neural_fusion_data(nnet8,matrice_HMM_8,data_8, dataFt_8, dataTarg_8)
dataFt_fusion_16 <- neural_fusion_data(nnet16,matrice_HMM_16,data_16, dataFt_16, dataTarg_16)

###Score Fusion

print("Fusion_5_3")
reco<-test.reco(dataTarg_5_3, predict(nn_fusion_5_3, dataFt_fusion_5_3))
print(reco/dim(dataTarg_5_3)[1])

print("Fusion_5_4")
reco<-test.reco(dataTarg_5_4, predict(nn_fusion_5_4, dataFt_fusion_5_4))
print(reco/dim(dataTarg_5_4)[1])


print("Fusion_8")
reco<-test.reco(dataTarg_8, predict(nn_fusion_8, dataFt_fusion_8))
print(reco/dim(dataTarg_8)[1])

print("Fusion_16")
reco<-test.reco(dataTarg_16, predict(nn_fusion_16, dataFt_fusion_16))
print(reco/dim(dataTarg_16)[1])