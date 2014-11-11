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

classifyDigit <- function(hmmMatrice, testLst){
  res <- numeric()
  nbRows <- length(hmmMatrice)/5
  
  nbResources <- length(testLst)/30
  
  for(res_line in 1:nbResources) {
    current_line <- numeric()
    for(line in 1:nbRows) {
      test <- loglikelihood(hmmMatrice[line,],testLst[res_line,])
      current_line <- c(current_line,test)
    }
    res <- c(res, current_line)
  }
  return (matrix(res,nbResources,nbRows))
}

classification <- function(reco_rate, test){
  ligne <- 1:129
  colonne <- 2:10
  max_val <- 0
  max_col <- 0
  result <- 0
  for (i in ligne){
    max_val <- reco_rate[i,1]
    max_col <- 1
    for (j in colonne){
      if (reco_rate[i,j] >= max_val){
        max_val = reco_rate[i,j]
        max_col = j
      }
    }
    if (max_col == (test+1)){
      result = result +1
    }
  }
  result = result/129
  return (result)
}

#########################################################
###                     MAIN                          ###
#########################################################

# Initialisation des HMMs pour chaque chiffre :
print("Initialisation des HMMs :")

### Pour 5x3
print("Initialisation des HMMs 5x3")
HMM0_5x3 = initHMMDigit(7,15)
HMM1_5x3 = initHMMDigit(7,15)
HMM2_5x3 = initHMMDigit(7,15)
HMM3_5x3 = initHMMDigit(7,15)
HMM4_5x3 = initHMMDigit(7,15)
HMM5_5x3 = initHMMDigit(7,15)
HMM6_5x3 = initHMMDigit(7,15)
HMM7_5x3 = initHMMDigit(7,15)
HMM8_5x3 = initHMMDigit(7,15)
HMM9_5x3 = initHMMDigit(7,15)

### Pour 5x4
print("Initialisation des HMMs 5x4")
HMM0_5x4 = initHMMDigit(7,20)
HMM1_5x4 = initHMMDigit(7,20)
HMM2_5x4 = initHMMDigit(7,20)
HMM3_5x4 = initHMMDigit(7,20)
HMM4_5x4 = initHMMDigit(7,20)
HMM5_5x4 = initHMMDigit(7,20)
HMM6_5x4 = initHMMDigit(7,20)
HMM7_5x4 = initHMMDigit(7,20)
HMM8_5x4 = initHMMDigit(7,20)
HMM9_5x4 = initHMMDigit(7,20)

### Pour 8
print("Initialisation des HMMs 8")
HMM0_8 = initHMMDigit(7,8)
HMM1_8 = initHMMDigit(7,8)
HMM2_8 = initHMMDigit(7,8)
HMM3_8 = initHMMDigit(7,8)
HMM4_8 = initHMMDigit(7,8)
HMM5_8 = initHMMDigit(7,8)
HMM6_8 = initHMMDigit(7,8)
HMM7_8 = initHMMDigit(7,8)
HMM8_8 = initHMMDigit(7,8)
HMM9_8 = initHMMDigit(7,8)

### Pour 16
print("Initialisation des HMMs 16")
HMM0_16 = initHMMDigit(7,16)
HMM1_16 = initHMMDigit(7,16)
HMM2_16 = initHMMDigit(7,16)
HMM3_16 = initHMMDigit(7,16)
HMM4_16 = initHMMDigit(7,16)
HMM5_16 = initHMMDigit(7,16)
HMM6_16 = initHMMDigit(7,16)
HMM7_16 = initHMMDigit(7,16)
HMM8_16 = initHMMDigit(7,16)
HMM9_16 = initHMMDigit(7,16)

# Chargement des fichiers train :
print("Chargement des fichiers train observations")
# Attention, il est peut être nécessaire de changer la localisation des fichiers

### Pour 5x3
print("Chargement des observations pour 5x3")
obs0_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit0.txt')
obs1_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit1.txt')
obs2_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit2.txt')
obs3_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit3.txt')
obs4_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit4.txt')
obs5_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit5.txt')
obs6_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit6.txt')
obs7_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit7.txt')
obs8_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit8.txt')
obs9_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit9.txt')

### Pour 5x4
print("Chargement des observations pour 5x4")
obs0_5X4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Train_compute_symbol_5_4Digit0.txt')
obs1_5X4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Train_compute_symbol_5_4Digit1.txt')
obs2_5X4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Train_compute_symbol_5_4Digit2.txt')
obs3_5X4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Train_compute_symbol_5_4Digit3.txt')
obs4_5X4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Train_compute_symbol_5_4Digit4.txt')
obs5_5X4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Train_compute_symbol_5_4Digit5.txt')
obs6_5X4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Train_compute_symbol_5_4Digit6.txt')
obs7_5X4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Train_compute_symbol_5_4Digit7.txt')
obs8_5X4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Train_compute_symbol_5_4Digit8.txt')
obs9_5X4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Train_compute_symbol_5_4Digit9.txt')

### Pour 8
print("Chargement des observations pour 8")
obs0_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Train_compute_symbol_dir_8Digit0.txt')
obs1_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Train_compute_symbol_dir_8Digit1.txt')
obs2_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Train_compute_symbol_dir_8Digit2.txt')
obs3_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Train_compute_symbol_dir_8Digit3.txt')
obs4_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Train_compute_symbol_dir_8Digit4.txt')
obs5_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Train_compute_symbol_dir_8Digit5.txt')
obs6_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Train_compute_symbol_dir_8Digit6.txt')
obs7_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Train_compute_symbol_dir_8Digit7.txt')
obs8_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Train_compute_symbol_dir_8Digit8.txt')
obs9_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Train_compute_symbol_dir_8Digit9.txt')

### Pour 16
print("Chargement des observations pour 16")
obs0_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Train_compute_symbol_dir_16Digit0.txt')
obs1_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Train_compute_symbol_dir_16Digit1.txt')
obs2_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Train_compute_symbol_dir_16Digit2.txt')
obs3_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Train_compute_symbol_dir_16Digit3.txt')
obs4_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Train_compute_symbol_dir_16Digit4.txt')
obs5_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Train_compute_symbol_dir_16Digit5.txt')
obs6_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Train_compute_symbol_dir_16Digit6.txt')
obs7_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Train_compute_symbol_dir_16Digit7.txt')
obs8_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Train_compute_symbol_dir_16Digit8.txt')
obs9_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Train_compute_symbol_dir_16Digit9.txt')

# Entrainement des HMMs par baumWelchList :
print("Entrainement des HMMs par baumWelchList")
# Attention, il est peut être nécessaire de changer la localisation des fichiers

## Pour 5x3
print("baumWelchList 5X3 0")
HMM0_5x3_entraine = baumWelchList(HMM0_5x3, obs0_5X3, 1000)$hmm
print(HMM0_5x3_entraine)
save(HMM0_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM0_5x3_entraine.rdata')

print("baumWelchList 5X3 1")
HMM1_5x3_entraine = baumWelchList(HMM1_5x3, obs1_5X3, 1000)$hmm
print(HMM1_5x3_entraine)
save(HMM1_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM1_5x3_entraine.rdata')

print("baumWelchList 5X3 2")
HMM2_5x3_entraine = baumWelchList(HMM2_5x3, obs2_5X3, 1000)$hmm
print(HMM2_5x3_entraine)
save(HMM2_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM2_5x3_entraine.rdata')

print("baumWelchList 5X3 3")
HMM3_5x3_entraine = baumWelchList(HMM3_5x3, obs3_5X3, 1000)$hmm
print(HMM3_5x3_entraine)
save(HMM3_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM3_5x3_entraine.rdata')

print("baumWelchList 5X3 4")
HMM4_5x3_entraine = baumWelchList(HMM4_5x3, obs4_5X3, 1000)$hmm
print(HMM4_5x3_entraine)
save(HMM4_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM4_5x3_entraine.rdata')

print("baumWelchList 5X3 5")
HMM5_5x3_entraine = baumWelchList(HMM5_5x3, obs5_5X3, 1000)$hmm
print(HMM5_5x3_entraine)
save(HMM5_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM5_5x3_entraine.rdata')

print("baumWelchList 5X3 6")
HMM6_5x3_entraine = baumWelchList(HMM6_5x3, obs6_5X3, 1000)$hmm
print(HMM6_5x3_entraine)
save(HMM6_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM6_5x3_entraine.rdata')

print("baumWelchList 5X3 7")
HMM7_5x3_entraine = baumWelchList(HMM7_5x3, obs7_5X3, 1000)$hmm
print(HMM7_5x3_entraine)
save(HMM7_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM7_5x3_entraine.rdata')

print("baumWelchList 5X3 8")
HMM8_5x3_entraine = baumWelchList(HMM8_5x3, obs8_5X3, 1000)$hmm
print(HMM8_5x3_entraine)
save(HMM8_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM8_5x3_entraine.rdata')

print("baumWelchList 5X3 9")
HMM9_5x3_entraine = baumWelchList(HMM9_5x3, obs9_5X3, 1000)$hmm
print(HMM9_5x3_entraine)
save(HMM9_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM9_5x3_entraine.rdata')

### Pour 5x4
print("baumWelchList 5X4 0")
HMM0_5x4_entraine = baumWelchList(HMM0_5x4, obs0_5X4, 1000)$hmm
print(HMM0_5x4_entraine)
save(HMM0_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM0_5x4_entraine.rdata')

print("baumWelchList 5X4 1")
HMM1_5x4_entraine = baumWelchList(HMM1_5x4, obs1_5X4, 1000)$hmm
print(HMM1_5x4_entraine)
save(HMM1_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM1_5x4_entraine.rdata')

print("baumWelchList 5X4 2")
HMM2_5x4_entraine = baumWelchList(HMM2_5x4, obs2_5X4, 1000)$hmm
print(HMM2_5x4_entraine)
save(HMM2_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM2_5x4_entraine.rdata')

print("baumWelchList 5X4 3")
HMM3_5x4_entraine = baumWelchList(HMM3_5x4, obs3_5X4, 1000)$hmm
print(HMM3_5x4_entraine)
save(HMM3_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM3_5x4_entraine.rdata')

print("baumWelchList 5X4 4")
HMM4_5x4_entraine = baumWelchList(HMM4_5x4, obs4_5X4, 1000)$hmm
print(HMM4_5x4_entraine)
save(HMM4_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM4_5x4_entraine.rdata')

print("baumWelchList 5X4 5")
HMM5_5x4_entraine = baumWelchList(HMM5_5x4, obs5_5X4, 1000)$hmm
print(HMM5_5x4_entraine)
save(HMM5_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM5_5x4_entraine.rdata')

print("baumWelchList 5X4 6")
HMM6_5x4_entraine = baumWelchList(HMM6_5x4, obs6_5X4, 1000)$hmm
print(HMM6_5x4_entraine)
save(HMM6_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM6_5x4_entraine.rdata')

print("baumWelchList 5X4 7")
HMM7_5x4_entraine = baumWelchList(HMM7_5x4, obs7_5X4, 1000)$hmm
print(HMM7_5x4_entraine)
save(HMM7_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM7_5x4_entraine.rdata')

print("baumWelchList 5X4 8")
HMM8_5x4_entraine = baumWelchList(HMM8_5x4, obs8_5X4, 1000)$hmm
print(HMM8_5x4_entraine)
save(HMM8_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM8_5x4_entraine.rdata')

print("baumWelchList 5X4 9")
HMM9_5x4_entraine = baumWelchList(HMM9_5x4, obs9_5X4, 1000)$hmm
print(HMM9_5x4_entraine)
save(HMM9_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM9_5x4_entraine.rdata')

### Pour 8
print("baumWelchList 8 0")
HMM0_8_entraine = baumWelchList(HMM0_8, obs0_8, 1000)$hmm
print(HMM0_8_entraine)
save(HMM0_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM0_8_entraine.rdata')

print("baumWelchList 8 1")
HMM1_8_entraine = baumWelchList(HMM1_8, obs1_8, 1000)$hmm
print(HMM1_8_entraine)
save(HMM1_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM1_8_entraine.rdata')

print("baumWelchList 8 2")
HMM2_8_entraine = baumWelchList(HMM2_8, obs2_8, 1000)$hmm
print(HMM2_8_entraine)
save(HMM2_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM2_8_entraine.rdata')

print("baumWelchList 8 3")
HMM3_8_entraine = baumWelchList(HMM3_8, obs3_8, 1000)$hmm
print(HMM3_8_entraine)
save(HMM3_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM3_8_entraine.rdata')

print("baumWelchList 8 4")
HMM4_8_entraine = baumWelchList(HMM4_8, obs4_8, 1000)$hmm
print(HMM4_8_entraine)
save(HMM4_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM4_8_entraine.rdata')

print("baumWelchList 8 5")
HMM5_8_entraine = baumWelchList(HMM5_8, obs5_8, 1000)$hmm
print(HMM5_8_entraine)
save(HMM5_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM5_8_entraine.rdata')

print("baumWelchList 8 6")
HMM6_8_entraine = baumWelchList(HMM6_8, obs6_8, 1000)$hmm
print(HMM6_8_entraine)
save(HMM6_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM6_8_entraine.rdata')

print("baumWelchList 8 7")
HMM7_8_entraine = baumWelchList(HMM7_8, obs7_8, 1000)$hmm
print(HMM7_8_entraine)
save(HMM7_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM7_8_entraine.rdata')

print("baumWelchList 8 8")
HMM8_8_entraine = baumWelchList(HMM8_8, obs8_8, 1000)$hmm
print(HMM8_8_entraine)
save(HMM8_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM8_8_entraine.rdata')

print("baumWelchList 8 9")
HMM9_8_entraine = baumWelchList(HMM9_8, obs9_8, 1000)$hmm
print(HMM9_8_entraine)
save(HMM9_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM9_8_entraine.rdata')

### Pour 16
print("baumWelchList 16 0")
HMM0_16_entraine = baumWelchList(HMM0_16, obs0_16, 1000)$hmm
print(HMM0_16_entraine)
save(HMM0_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM0_16_entraine.rdata')

print("baumWelchList 16 1")
HMM1_16_entraine = baumWelchList(HMM1_16, obs1_16, 1000)$hmm
print(HMM1_16_entraine)
save(HMM1_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM1_16_entraine.rdata')

print("baumWelchList 16 2")
HMM2_16_entraine = baumWelchList(HMM2_16, obs2_16, 1000)$hmm
print(HMM2_16_entraine)
save(HMM2_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM2_16_entraine.rdata')

print("baumWelchList 16 3")
HMM3_16_entraine = baumWelchList(HMM3_16, obs3_16, 1000)$hmm
print(HMM3_16_entraine)
save(HMM3_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM3_16_entraine.rdata')

print("baumWelchList 16 4")
HMM4_16_entraine = baumWelchList(HMM4_16, obs4_16, 1000)$hmm
print(HMM4_16_entraine)
save(HMM4_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM4_16_entraine.rdata')

print("baumWelchList 16 5")
HMM5_16_entraine = baumWelchList(HMM5_16, obs5_16, 1000)$hmm
print(HMM5_16_entraine)
save(HMM5_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM5_16_entraine.rdata')

print("baumWelchList 16 6")
HMM6_16_entraine = baumWelchList(HMM6_16, obs6_16, 1000)$hmm
print(HMM6_16_entraine)
save(HMM6_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM6_16_entraine.rdata')

print("baumWelchList 16 7")
HMM7_16_entraine = baumWelchList(HMM7_16, obs7_16, 1000)$hmm
print(HMM7_16_entraine)
save(HMM7_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM7_16_entraine.rdata')

print("baumWelchList 16 8")
HMM8_16_entraine = baumWelchList(HMM8_16, obs8_16, 1000)$hmm
print(HMM8_16_entraine)
save(HMM8_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM8_16_entraine.rdata')

print("baumWelchList 16 9")
HMM9_16_entraine = baumWelchList(HMM9_16, obs9_16, 1000)$hmm
print(HMM9_16_entraine)
save(HMM9_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM9_16_entraine.rdata')

# Chargement des Hmms deja entraines :
# print("Chargement des Hmms deja entraines")
# # Attention, il est peut être nécessaire de changer la localisation des fichiers
# 
# ### Pour 5x3
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM0_5x3_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM1_5x3_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM2_5x3_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM3_5x3_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM4_5x3_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM5_5x3_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM6_5x3_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM7_5x3_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM8_5x3_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM9_5x3_entraine.rdata')
# 
# ### Pour 5x4
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM0_5x4_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM1_5x4_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM2_5x4_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM3_5x4_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM4_5x4_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM5_5x4_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM6_5x4_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM7_5x4_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM8_5x4_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM9_5x4_entraine.rdata')
# 
# ### Pour 8
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM0_8_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM1_8_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM2_8_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM3_8_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM4_8_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM5_8_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM6_8_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM7_8_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM8_8_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM9_8_entraine.rdata')
# 
# ### Pour 16
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM0_16_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM1_16_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM2_16_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM3_16_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM4_16_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM5_16_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM6_16_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM7_16_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM8_16_entraine.rdata')
# load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/7_states/HMM9_16_entraine.rdata')

#Chargement des fichiers d'obervation de test :
print("Chargement des fichiers d'obervation de test")

### Pour 5x3
test0_5x3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit0.txt')
test1_5x3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit1.txt')
test2_5x3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit2.txt')
test3_5x3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit3.txt')
test4_5x3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit4.txt')
test5_5x3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit5.txt')
test6_5x3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit6.txt')
test7_5x3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit7.txt')
test8_5x3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit8.txt')
test9_5x3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit9.txt')

### Pour 5x4
test0_5x4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit0.txt')
test1_5x4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit1.txt')
test2_5x4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit2.txt')
test3_5x4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit3.txt')
test4_5x4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit4.txt')
test5_5x4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit5.txt')
test6_5x4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit6.txt')
test7_5x4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit7.txt')
test8_5x4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit8.txt')
test9_5x4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit9.txt')

### Pour 8
test0_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit0.txt')
test1_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit1.txt')
test2_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit2.txt')
test3_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit3.txt')
test4_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit4.txt')
test5_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit5.txt')
test6_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit6.txt')
test7_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit7.txt')
test8_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit8.txt')
test9_8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit9.txt')

### Pour 16

test0_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit0.txt')
test1_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit1.txt')
test2_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit2.txt')
test3_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit3.txt')
test4_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit4.txt')
test5_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit5.txt')
test6_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit6.txt')
test7_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit7.txt')
test8_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit8.txt')
test9_16 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit9.txt')

# Creation des matrices de Hmms entraines regroupant les 10 symboles :
print("Creation des matrices de Hmms entraines regroupant les 10 symboles")
matrice_HMM_5X3 = rbind(HMM0_5x3_entraine,HMM1_5x3_entraine,HMM2_5x3_entraine,HMM3_5x3_entraine,HMM4_5x3_entraine,HMM5_5x3_entraine,HMM6_5x3_entraine,HMM7_5x3_entraine,HMM8_5x3_entraine,HMM9_5x3_entraine)
matrice_HMM_5X4 = rbind(HMM0_5x4_entraine,HMM1_5x4_entraine,HMM2_5x4_entraine,HMM3_5x4_entraine,HMM4_5x4_entraine,HMM5_5x4_entraine,HMM6_5x4_entraine,HMM7_5x4_entraine,HMM8_5x4_entraine,HMM9_5x4_entraine)
matrice_HMM_8 = rbind(HMM0_8_entraine,HMM1_8_entraine,HMM2_8_entraine,HMM3_8_entraine,HMM4_8_entraine,HMM5_8_entraine,HMM6_8_entraine,HMM7_8_entraine,HMM8_8_entraine,HMM9_8_entraine)
matrice_HMM_16 = rbind(HMM0_16_entraine,HMM1_16_entraine,HMM2_16_entraine,HMM3_16_entraine,HMM4_16_entraine,HMM5_16_entraine,HMM6_16_entraine,HMM7_16_entraine,HMM8_16_entraine,HMM9_16_entraine)


# Classification des résultats
### Pour 5x3
reco_rate0_5x3 <- classifyDigit(matrice_HMM_5X3,test0_5x3)
print (reco_rate0_5x3)
reco_rate1_5x3 <- classifyDigit(matrice_HMM_5X3,test1_5x3)
print (reco_rate1_5x3)
reco_rate2_5x3 <- classifyDigit(matrice_HMM_5X3,test2_5x3)
print (reco_rate2_5x3)
reco_rate3_5x3 <- classifyDigit(matrice_HMM_5X3,test3_5x3)
print (reco_rate3_5x3)
reco_rate4_5x3 <- classifyDigit(matrice_HMM_5X3,test4_5x3)
print (reco_rate4_5x3)
reco_rate5_5x3 <- classifyDigit(matrice_HMM_5X3,test5_5x3)
print (reco_rate5_5x3)
reco_rate6_5x3 <- classifyDigit(matrice_HMM_5X3,test6_5x3)
print (reco_rate6_5x3)
reco_rate7_5x3 <- classifyDigit(matrice_HMM_5X3,test7_5x3)
print (reco_rate7_5x3)
reco_rate8_5x3 <- classifyDigit(matrice_HMM_5X3,test8_5x3)
print (reco_rate8_5x3)
reco_rate9_5x3 <- classifyDigit(matrice_HMM_5X3,test9_5x3)
print (reco_rate9_5x3)

### Pour 5x4
reco_rate0_5x4 <- classifyDigit(matrice_HMM_5X4,test0_5x4)
print (reco_rate0_5x4)
reco_rate1_5x4 <- classifyDigit(matrice_HMM_5X4,test1_5x4)
print (reco_rate1_5x4)
reco_rate2_5x4 <- classifyDigit(matrice_HMM_5X4,test2_5x4)
print (reco_rate2_5x4)
reco_rate3_5x4 <- classifyDigit(matrice_HMM_5X4,test3_5x4)
print (reco_rate3_5x4)
reco_rate4_5x4 <- classifyDigit(matrice_HMM_5X4,test4_5x4)
print (reco_rate4_5x4)
reco_rate5_5x4 <- classifyDigit(matrice_HMM_5X4,test5_5x4)
print (reco_rate5_5x4)
reco_rate6_5x4 <- classifyDigit(matrice_HMM_5X4,test6_5x4)
print (reco_rate6_5x4)
reco_rate7_5x4 <- classifyDigit(matrice_HMM_5X4,test7_5x4)
print (reco_rate7_5x4)
reco_rate8_5x4 <- classifyDigit(matrice_HMM_5X4,test8_5x4)
print (reco_rate8_5x4)
reco_rate9_5x4 <- classifyDigit(matrice_HMM_5X4,test9_5x4)
print (reco_rate9_5x4)

### Pour 8
reco_rate0_8 <- classifyDigit(matrice_HMM_8,test0_8)
print (reco_rate0_8)
reco_rate1_8 <- classifyDigit(matrice_HMM_8,test1_8)
print (reco_rate1_8)
reco_rate2_8 <- classifyDigit(matrice_HMM_8,test2_8)
print (reco_rate2_8)
reco_rate3_8 <- classifyDigit(matrice_HMM_8,test3_8)
print (reco_rate3_8)
reco_rate4_8 <- classifyDigit(matrice_HMM_8,test4_8)
print (reco_rate4_8)
reco_rate5_8 <- classifyDigit(matrice_HMM_8,test5_8)
print (reco_rate5_8)
reco_rate6_8 <- classifyDigit(matrice_HMM_8,test6_8)
print (reco_rate6_8)
reco_rate7_8 <- classifyDigit(matrice_HMM_8,test7_8)
print (reco_rate7_8)
reco_rate8_8 <- classifyDigit(matrice_HMM_8,test8_8)
print (reco_rate8_8)
reco_rate9_8 <- classifyDigit(matrice_HMM_8,test9_8)
print (reco_rate9_8)

### Pour 16
reco_rate0_16 <- classifyDigit(matrice_HMM_16,test0_16)
print (reco_rate0_16)
reco_rate1_16 <- classifyDigit(matrice_HMM_16,test1_16)
print (reco_rate1_16)
reco_rate2_16 <- classifyDigit(matrice_HMM_16,test2_16)
print (reco_rate2_16)
reco_rate3_16 <- classifyDigit(matrice_HMM_16,test3_16)
print (reco_rate3_16)
reco_rate4_16 <- classifyDigit(matrice_HMM_16,test4_16)
print (reco_rate4_16)
reco_rate5_16 <- classifyDigit(matrice_HMM_16,test5_16)
print (reco_rate5_16)
reco_rate6_16 <- classifyDigit(matrice_HMM_16,test6_16)
print (reco_rate6_16)
reco_rate7_16 <- classifyDigit(matrice_HMM_16,test7_16)
print (reco_rate7_16)
reco_rate8_16 <- classifyDigit(matrice_HMM_16,test8_16)
print (reco_rate8_16)
reco_rate9_16 <- classifyDigit(matrice_HMM_16,test9_16)
print (reco_rate9_16)

# result0_5x3 <- classification(reco_rate0_5x3, 0)
# print (result0_5x3)
# result0_5x4 <- classification(reco_rate0_5x4, 0)
# print (result0_5x4)
# result0_8 <- classification(reco_rate0_8, 0)
# print (result0_8)
# result0_16 <- classification(reco_rate0_16, 0)
# print (result0_16)
