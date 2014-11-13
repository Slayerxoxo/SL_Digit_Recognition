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
data_5_3<-loadAll(file.path(getwd(),"dataset/Data5x3/Test_compute_symbol_5_3Digit"))
data_5_4<-loadAll(file.path(getwd(),"dataset/Data5x4/Test_compute_symbol_5_4Digit"))
data_7_5<-loadAll(file.path(getwd(),"dataset/Data7x5/Test_compute_symbol_7_5Digit"))
data_8<-loadAll(file.path(getwd(),"dataset/Dir8/Test_compute_symbol_dir_8Digit"))
data_16<-loadAll(file.path(getwd(),"dataset/Dir16/Test_compute_symbol_dir_16Digit"))

### Creation des features pour les NN
dataFt_5_3<-construct_all_Ft(data_5_3$obs, 5 , 3)
dataFt_5_4<-construct_all_Ft(data_5_4$obs, 5 , 4)
dataFt_7_5<-construct_all_Ft(data_7_5$obs, 7 , 5)
dataFt_8<-construct_trigo_Ft(data_8$obs,8)
dataFt_16<-construct_trigo_Ft(data_16$obs,16)

### Creation des objectifs
select <- ((digitCl == 1) | (digitCl == 2)| (digitCl == 3)| (digitCl == 4)| (digitCl == 5)| (digitCl == 6)| (digitCl == 7)| (digitCl == 8)| (digitCl == 9)| (digitCl == 0))
dataTarg_5_3 <- class.ind(data_5_3$cl[select])
dataTarg_5_4 <- class.ind(data_5_4$cl[select])
dataTarg_7_5 <- class.ind(data_7_5$cl[select])
dataTarg_8 <- class.ind(data_8$cl[select])
dataTarg_16 <- class.ind(data_16$cl[select])

### Score HMM
reco <- test.reco(dataTarg_5_3, exp(classifyDigit(matrice_HMM_5X3,data_5_3$obs)))
print("HMM_5_3")
print(reco/dim(dataTarg_5_3)[1])