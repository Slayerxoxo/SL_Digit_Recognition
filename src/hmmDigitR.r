#########################################################
###                  FUNCTIONS                        ###
#########################################################

initHMMDigit <- function(nbrEtats, nbrObsv){
  etats <- c()
  obsv <- c()
  probInit <- c()
  transProbs <- c()
  emissionProbs <- c()
  
  # Création de la liste d'états
  for (i in 1:nbrEtats){
    etats <- c(etats, as.character(i))
  }
  
  # Création de la liste d'observation
  for (i in 1:nbrObsv){
    obsv <- c(obsv, as.character(i))
  }
  
  # Mise en place des proba initiales
  for (i in 1:nbrEtats){
    if (i == 1){
      probInit <- c(probInit, 1)
    }
    else{
      probInit <- c(probInit, 0)
    }
  }
  
  # Mise en place des proba de transition ppour le HMM optimal
  for (i in 1:nbrEtats){
    for (j in 1:nbrEtats){
      if (i == j){
        transProbs <- c(transProbs, 0.9)
      }
      else if (j == i+1){
        transProbs <- c(transProbs, 0.1)
      }
      else{
        transProbs <- c(transProbs, 0)
      }
    }
  }
  matriceInit <- matrix(transProbs, nrow = nbrEtats, ncol = nbrEtats, byrow = T)
  #print(matriceInit)
  
  # Mise en place des proba d'emission
  for (i in 1:nbrEtats*nbrObsv){
    emissionProbs <- c(emissionProbs, 1/nbrObsv)
  }
  matriceEmission <- matrix(emissionProbs,nrow = nbrEtats, ncol = nbrObsv, byrow = T)
  
  # Appel de initHMM
  resultat <- initHMM(etats, obsv, probInit, matriceInit, matriceEmission)
  return (resultat)
}

# classifyDigit <- function(hmmMatrice, testLst){
#   res <- numeric()
#   nbRows <- length(hmmMatrice)/5
#   
#   nbResources <- length(testLst)/30
#   
#   for(res_line in 1:nbResources) {
#     current_line <- numeric()
#     for(line in 1:nbRows) {
#       test <- loglikelihood(hmmMatrice[line,],testLst[res_line,])
#       current_line <- c(current_line,test)
#     }
#     res <- c(res, current_line)
#   }
#   return (matrix(res,nbResources,nbRows))
# }

# classifyDigit <- function(hmm, obs) 
# {
#     res <- numeric()
#     nbRows <- length(hmm)/5
#     nbResources <- length(obs)/30
#     
#     probs <- sapply(hmm, function(x) loglikelihood(x$hmm, obs))
# }

recognition <- function(type, model)
{
  classes = NULL
  
  o <- loadAll(paste("/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Test_compute_symbol_", type, "Digit", sep=""))
  
  for(i in 1:length(o$obs[,1])){
    classes <- c(classes, classifyJEdit(model, o$obs[i,]))
  }
  
  print("Confusion Matrix :")
  print(table(o$cl, classes))
  
  print("Recognition Rate :")
  print((sum(diag(table(o$cl,classes)))/length(o$obs[,1])) * 100)
}

classifyJEdit <- function(hmm, obs)
{
  max <- -Inf
  class <- 0
  k <- 0
  for (i in hmm){
    fwd <- forward(i$hmm,obs)
    fwdPb <- -Inf
    for(j in 1:length(i$hmm$States)){
      fwdPb <- logadd(fwdPb,fwd[j,length(obs)])
    }
    if (fwdPb > max){
      max <- fwdPb
      class <- k
    }
    k <- k + 1
  }
  return(class)
}

#########################################################
###                     MAIN                          ###
#########################################################


# # Initialisation des HMMs pour chaque chiffre :
# print("Initialisation des HMMs :")
# 
# ### Pour 5x3
# print("Initialisation des HMMs 5x3")

# HMM0_5x3 = initHMMDigit(3,15)
# HMM1_5x3 = initHMMDigit(3,15)
# HMM2_5x3 = initHMMDigit(3,15)
# HMM3_5x3 = initHMMDigit(3,15)
# HMM4_5x3 = initHMMDigit(3,15)
# HMM5_5x3 = initHMMDigit(3,15)
# HMM6_5x3 = initHMMDigit(3,15)
# HMM7_5x3 = initHMMDigit(3,15)
# HMM8_5x3 = initHMMDigit(3,15)
# HMM9_5x3 = initHMMDigit(3,15)
# 
# ### Pour 5x4
# print("Initialisation des HMMs 5x4")
# HMM0_5x4 = initHMMDigit(3,20)
# HMM1_5x4 = initHMMDigit(3,20)
# HMM2_5x4 = initHMMDigit(3,20)
# HMM3_5x4 = initHMMDigit(3,20)
# HMM4_5x4 = initHMMDigit(3,20)
# HMM5_5x4 = initHMMDigit(3,20)
# HMM6_5x4 = initHMMDigit(3,20)
# HMM7_5x4 = initHMMDigit(3,20)
# HMM8_5x4 = initHMMDigit(3,20)
# HMM9_5x4 = initHMMDigit(3,20)
# 
# ### Pour 8
# print("Initialisation des HMMs 8")
# HMM0_8 = initHMMDigit(3,8)
# HMM1_8 = initHMMDigit(3,8)
# HMM2_8 = initHMMDigit(3,8)
# HMM3_8 = initHMMDigit(3,8)
# HMM4_8 = initHMMDigit(3,8)
# HMM5_8 = initHMMDigit(3,8)
# HMM6_8 = initHMMDigit(3,8)
# HMM7_8 = initHMMDigit(3,8)
# HMM8_8 = initHMMDigit(3,8)
# HMM9_8 = initHMMDigit(3,8)
# 
# ### Pour 16
# print("Initialisation des HMMs 16")
# HMM0_16 = initHMMDigit(3,16)
# HMM1_16 = initHMMDigit(3,16)
# HMM2_16 = initHMMDigit(3,16)
# HMM3_16 = initHMMDigit(3,16)
# HMM4_16 = initHMMDigit(3,16)
# HMM5_16 = initHMMDigit(3,16)
# HMM6_16 = initHMMDigit(3,16)
# HMM7_16 = initHMMDigit(3,16)
# HMM8_16 = initHMMDigit(3,16)
# HMM9_16 = initHMMDigit(3,16)

# # Chargement des fichiers train :
# print("Chargement des fichiers train observations")
# # Attention, il est peut être nécessaire de changer la localisation des fichiers
# 
# ### Pour 5x3
# print("Chargement des observations pour 5x3")
# obs0_5X3 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_3Digit0.txt')
# obs1_5X3 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_3Digit1.txt')
# obs2_5X3 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_3Digit2.txt')
# obs3_5X3 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_3Digit3.txt')
# obs4_5X3 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_3Digit4.txt')
# obs5_5X3 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_3Digit5.txt')
# obs6_5X3 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_3Digit6.txt')
# obs7_5X3 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_3Digit7.txt')
# obs8_5X3 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_3Digit8.txt')
# obs9_5X3 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_3Digit9.txt')
# 
# ### Pour 5x4
# print("Chargement des observations pour 5x4")
# obs0_5X4 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_4Digit0.txt')
# obs1_5X4 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_4Digit1.txt')
# obs2_5X4 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_4Digit2.txt')
# obs3_5X4 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_4Digit3.txt')
# obs4_5X4 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_4Digit4.txt')
# obs5_5X4 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_4Digit5.txt')
# obs6_5X4 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_4Digit6.txt')
# obs7_5X4 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_4Digit7.txt')
# obs8_5X4 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_4Digit8.txt')
# obs9_5X4 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_5_4Digit9.txt')
# 
# ### Pour 8
# print("Chargement des observations pour 8")
# obs0_8 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_8Digit0.txt')
# obs1_8 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_8Digit1.txt')
# obs2_8 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_8Digit2.txt')
# obs3_8 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_8Digit3.txt')
# obs4_8 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_8Digit4.txt')
# obs5_8 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_8Digit5.txt')
# obs6_8 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_8Digit6.txt')
# obs7_8 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_8Digit7.txt')
# obs8_8 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_8Digit8.txt')
# obs9_8 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_8Digit9.txt')
# 
# ### Pour 16
# print("Chargement des observations pour 16")
# obs0_16 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_16Digit0.txt')
# obs1_16 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_16Digit1.txt')
# obs2_16 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_16Digit2.txt')
# obs3_16 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_16Digit3.txt')
# obs4_16 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_16Digit4.txt')
# obs5_16 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_16Digit5.txt')
# obs6_16 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_16Digit6.txt')
# obs7_16 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_16Digit7.txt')
# obs8_16 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_16Digit8.txt')
# obs9_16 = Load_Obs('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/data/Train_compute_symbol_dir_16Digit9.txt')

# # Entrainement des HMMs par baumWelchList :
# print("Entrainement des HMMs par baumWelchList")
# # Attention, il est peut être nécessaire de changer la localisation des fichiers
# 
# ## Pour 5x3
# print("baumWelchList 5X3 0")
# HMM0_5x3_entraine = baumWelchList(HMM0_5x3, obs0_5X3, 1000)$hmm
# print(HMM0_5x3_entraine)
# save(HMM0_5x3_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM0_5x3_entraine.rdata')
# 
# print("baumWelchList 5X3 1")
# HMM1_5x3_entraine = baumWelchList(HMM1_5x3, obs1_5X3, 1000)$hmm
# print(HMM1_5x3_entraine)
# save(HMM1_5x3_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM1_5x3_entraine.rdata')
# 
# print("baumWelchList 5X3 2")
# HMM2_5x3_entraine = baumWelchList(HMM2_5x3, obs2_5X3, 1000)$hmm
# print(HMM2_5x3_entraine)
# save(HMM2_5x3_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM2_5x3_entraine.rdata')
# 
# print("baumWelchList 5X3 3")
# HMM3_5x3_entraine = baumWelchList(HMM3_5x3, obs3_5X3, 1000)$hmm
# print(HMM3_5x3_entraine)
# save(HMM3_5x3_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM3_5x3_entraine.rdata')
# 
# print("baumWelchList 5X3 4")
# HMM4_5x3_entraine = baumWelchList(HMM4_5x3, obs4_5X3, 1000)$hmm
# print(HMM4_5x3_entraine)
# save(HMM4_5x3_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM4_5x3_entraine.rdata')
# 
# print("baumWelchList 5X3 5")
# HMM5_5x3_entraine = baumWelchList(HMM5_5x3, obs5_5X3, 1000)$hmm
# print(HMM5_5x3_entraine)
# save(HMM5_5x3_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM5_5x3_entraine.rdata')
# 
# print("baumWelchList 5X3 6")
# HMM6_5x3_entraine = baumWelchList(HMM6_5x3, obs6_5X3, 1000)$hmm
# print(HMM6_5x3_entraine)
# save(HMM6_5x3_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM6_5x3_entraine.rdata')
# 
# print("baumWelchList 5X3 7")
# HMM7_5x3_entraine = baumWelchList(HMM7_5x3, obs7_5X3, 1000)$hmm
# print(HMM7_5x3_entraine)
# save(HMM7_5x3_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM7_5x3_entraine.rdata')
# 
# print("baumWelchList 5X3 8")
# HMM8_5x3_entraine = baumWelchList(HMM8_5x3, obs8_5X3, 1000)$hmm
# print(HMM8_5x3_entraine)
# save(HMM8_5x3_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM8_5x3_entraine.rdata')
# 
# print("baumWelchList 5X3 9")
# HMM9_5x3_entraine = baumWelchList(HMM9_5x3, obs9_5X3, 1000)$hmm
# print(HMM9_5x3_entraine)
# save(HMM9_5x3_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM9_5x3_entraine.rdata')
# 
# ### Pour 5x4
# print("baumWelchList 5X4 0")
# HMM0_5x4_entraine = baumWelchList(HMM0_5x4, obs0_5X4, 1000)$hmm
# print(HMM0_5x4_entraine)
# save(HMM0_5x4_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM0_5x4_entraine.rdata')
# 
# print("baumWelchList 5X4 1")
# HMM1_5x4_entraine = baumWelchList(HMM1_5x4, obs1_5X4, 1000)$hmm
# print(HMM1_5x4_entraine)
# save(HMM1_5x4_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM1_5x4_entraine.rdata')
# 
# print("baumWelchList 5X4 2")
# HMM2_5x4_entraine = baumWelchList(HMM2_5x4, obs2_5X4, 1000)$hmm
# print(HMM2_5x4_entraine)
# save(HMM2_5x4_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM2_5x4_entraine.rdata')
# 
# print("baumWelchList 5X4 3")
# HMM3_5x4_entraine = baumWelchList(HMM3_5x4, obs3_5X4, 1000)$hmm
# print(HMM3_5x4_entraine)
# save(HMM3_5x4_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM3_5x4_entraine.rdata')
# 
# print("baumWelchList 5X4 4")
# HMM4_5x4_entraine = baumWelchList(HMM4_5x4, obs4_5X4, 1000)$hmm
# print(HMM4_5x4_entraine)
# save(HMM4_5x4_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM4_5x4_entraine.rdata')
# 
# print("baumWelchList 5X4 5")
# HMM5_5x4_entraine = baumWelchList(HMM5_5x4, obs5_5X4, 1000)$hmm
# print(HMM5_5x4_entraine)
# save(HMM5_5x4_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM5_5x4_entraine.rdata')
# 
# print("baumWelchList 5X4 6")
# HMM6_5x4_entraine = baumWelchList(HMM6_5x4, obs6_5X4, 1000)$hmm
# print(HMM6_5x4_entraine)
# save(HMM6_5x4_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM6_5x4_entraine.rdata')
# 
# print("baumWelchList 5X4 7")
# HMM7_5x4_entraine = baumWelchList(HMM7_5x4, obs7_5X4, 1000)$hmm
# print(HMM7_5x4_entraine)
# save(HMM7_5x4_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM7_5x4_entraine.rdata')
# 
# print("baumWelchList 5X4 8")
# HMM8_5x4_entraine = baumWelchList(HMM8_5x4, obs8_5X4, 1000)$hmm
# print(HMM8_5x4_entraine)
# save(HMM8_5x4_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM8_5x4_entraine.rdata')
# 
# print("baumWelchList 5X4 9")
# HMM9_5x4_entraine = baumWelchList(HMM9_5x4, obs9_5X4, 1000)$hmm
# print(HMM9_5x4_entraine)
# save(HMM9_5x4_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM9_5x4_entraine.rdata')
# 
# ### Pour 8
# print("baumWelchList 8 0")
# HMM0_8_entraine = baumWelchList(HMM0_8, obs0_8, 1000)$hmm
# print(HMM0_8_entraine)
# save(HMM0_8_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM0_8_entraine.rdata')
# 
# print("baumWelchList 8 1")
# HMM1_8_entraine = baumWelchList(HMM1_8, obs1_8, 1000)$hmm
# print(HMM1_8_entraine)
# save(HMM1_8_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM1_8_entraine.rdata')
# 
# print("baumWelchList 8 2")
# HMM2_8_entraine = baumWelchList(HMM2_8, obs2_8, 1000)$hmm
# print(HMM2_8_entraine)
# save(HMM2_8_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM2_8_entraine.rdata')
# 
# print("baumWelchList 8 3")
# HMM3_8_entraine = baumWelchList(HMM3_8, obs3_8, 1000)$hmm
# print(HMM3_8_entraine)
# save(HMM3_8_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM3_8_entraine.rdata')
# 
# print("baumWelchList 8 4")
# HMM4_8_entraine = baumWelchList(HMM4_8, obs4_8, 1000)$hmm
# print(HMM4_8_entraine)
# save(HMM4_8_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM4_8_entraine.rdata')
# 
# print("baumWelchList 8 5")
# HMM5_8_entraine = baumWelchList(HMM5_8, obs5_8, 1000)$hmm
# print(HMM5_8_entraine)
# save(HMM5_8_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM5_8_entraine.rdata')
# 
# print("baumWelchList 8 6")
# HMM6_8_entraine = baumWelchList(HMM6_8, obs6_8, 1000)$hmm
# print(HMM6_8_entraine)
# save(HMM6_8_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM6_8_entraine.rdata')
# 
# print("baumWelchList 8 7")
# HMM7_8_entraine = baumWelchList(HMM7_8, obs7_8, 1000)$hmm
# print(HMM7_8_entraine)
# save(HMM7_8_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM7_8_entraine.rdata')
# 
# print("baumWelchList 8 8")
# HMM8_8_entraine = baumWelchList(HMM8_8, obs8_8, 1000)$hmm
# print(HMM8_8_entraine)
# save(HMM8_8_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM8_8_entraine.rdata')
# 
# print("baumWelchList 8 9")
# HMM9_8_entraine = baumWelchList(HMM9_8, obs9_8, 1000)$hmm
# print(HMM9_8_entraine)
# save(HMM9_8_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM9_8_entraine.rdata')
# 
# ### Pour 16
# print("baumWelchList 16 0")
# HMM0_16_entraine = baumWelchList(HMM0_16, obs0_16, 1000)$hmm
# print(HMM0_16_entraine)
# save(HMM0_16_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM0_16_entraine.rdata')
# 
# print("baumWelchList 16 1")
# HMM1_16_entraine = baumWelchList(HMM1_16, obs1_16, 1000)$hmm
# print(HMM1_16_entraine)
# save(HMM1_16_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM1_16_entraine.rdata')
# 
# print("baumWelchList 16 2")
# HMM2_16_entraine = baumWelchList(HMM2_16, obs2_16, 1000)$hmm
# print(HMM2_16_entraine)
# save(HMM2_16_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM2_16_entraine.rdata')
# 
# print("baumWelchList 16 3")
# HMM3_16_entraine = baumWelchList(HMM3_16, obs3_16, 1000)$hmm
# print(HMM3_16_entraine)
# save(HMM3_16_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM3_16_entraine.rdata')
# 
# print("baumWelchList 16 4")
# HMM4_16_entraine = baumWelchList(HMM4_16, obs4_16, 1000)$hmm
# print(HMM4_16_entraine)
# save(HMM4_16_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM4_16_entraine.rdata')
# 
# print("baumWelchList 16 5")
# HMM5_16_entraine = baumWelchList(HMM5_16, obs5_16, 1000)$hmm
# print(HMM5_16_entraine)
# save(HMM5_16_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM5_16_entraine.rdata')
# 
# print("baumWelchList 16 6")
# HMM6_16_entraine = baumWelchList(HMM6_16, obs6_16, 1000)$hmm
# print(HMM6_16_entraine)
# save(HMM6_16_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM6_16_entraine.rdata')
# 
# print("baumWelchList 16 7")
# HMM7_16_entraine = baumWelchList(HMM7_16, obs7_16, 1000)$hmm
# print(HMM7_16_entraine)
# save(HMM7_16_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM7_16_entraine.rdata')
# 
# print("baumWelchList 16 8")
# HMM8_16_entraine = baumWelchList(HMM8_16, obs8_16, 1000)$hmm
# print(HMM8_16_entraine)
# save(HMM8_16_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM8_16_entraine.rdata')
# 
# print("baumWelchList 16 9")
# HMM9_16_entraine = baumWelchList(HMM9_16, obs9_16, 1000)$hmm
# print(HMM9_16_entraine)
# save(HMM9_16_entraine, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM9_16_entraine.rdata')

# # Chargement des Hmms deja entraines :
# print("Chargement des Hmms deja entraines")
# # Attention, il est peut être nécessaire de changer la localisation des fichiers
# 

# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/5_3')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/5_4')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/dir_8')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/dir_16')
# ### Pour 5x3
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM0_5x3_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM1_5x3_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM2_5x3_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM3_5x3_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM4_5x3_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM5_5x3_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM6_5x3_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM7_5x3_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM8_5x3_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM9_5x3_entraine.rdata')
# 
# ### Pour 5x4
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM0_5x4_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM1_5x4_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM2_5x4_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM3_5x4_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM4_5x4_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM5_5x4_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM6_5x4_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM7_5x4_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM8_5x4_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM9_5x4_entraine.rdata')
# 
# ### Pour 8
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM0_8_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM1_8_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM2_8_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM3_8_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM4_8_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM5_8_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM6_8_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM7_8_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM8_8_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM9_8_entraine.rdata')
# 
# ### Pour 16
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM0_16_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM1_16_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM2_16_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM3_16_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM4_16_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM5_16_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM6_16_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM7_16_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM8_16_entraine.rdata')
# load('/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/HMM9_16_entraine.rdata')
# 

# Creation des matrices de Hmms entraines regroupant les 10 symboles :
# print("Creation des matrices de Hmms entraines regroupant les 10 symboles")
#
# matrice_HMM_5X3 = rbind(HMM0_5x3_entraine,HMM1_5x3_entraine,HMM2_5x3_entraine,HMM3_5x3_entraine,HMM4_5x3_entraine,HMM5_5x3_entraine,HMM6_5x3_entraine,HMM7_5x3_entraine,HMM8_5x3_entraine,HMM9_5x3_entraine)
# save(matrice_HMM_5X3, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/matrice_HMM_5X3.rdata')
# matrice_HMM_5X4 = rbind(HMM0_5x4_entraine,HMM1_5x4_entraine,HMM2_5x4_entraine,HMM3_5x4_entraine,HMM4_5x4_entraine,HMM5_5x4_entraine,HMM6_5x4_entraine,HMM7_5x4_entraine,HMM8_5x4_entraine,HMM9_5x4_entraine)
# save(matrice_HMM_5X4, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/matrice_HMM_5X4.rdata')
# matrice_HMM_8 = rbind(HMM0_8_entraine,HMM1_8_entraine,HMM2_8_entraine,HMM3_8_entraine,HMM4_8_entraine,HMM5_8_entraine,HMM6_8_entraine,HMM7_8_entraine,HMM8_8_entraine,HMM9_8_entraine)
# save(matrice_HMM_dir_8, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/matrice_HMM_dir_8.rdata')
# matrice_HMM_16 = rbind(HMM0_16_entraine,HMM1_16_entraine,HMM2_16_entraine,HMM3_16_entraine,HMM4_16_entraine,HMM5_16_entraine,HMM6_16_entraine,HMM7_16_entraine,HMM8_16_entraine,HMM9_16_entraine)
# save(matrice_HMM_dir_16, file = '/home/coraline/Documents/Signal et Langue/SL_Digit_Recognition/src/Results/3_states/matrice_HMM_dir_16.rdata')

# Classification des résultats
# ### Pour 5x3
# recognition("5_3", models_optimal_5_3)
# ### Pour 5x4
# recognition("5_4", models_optimal_5_4)
# ### Pour 8
# recognition("dir_8", models_optimal_dir_8)
# ### Pour 16
# recognition("dir_16", models_optimal_dir_16)
