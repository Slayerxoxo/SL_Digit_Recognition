initHMMDigit <- function(nbrEtats, nbrObsv)
{
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

#########################################################
# MAIN #

# Initialisation des HMM pour chaque chiffre
print("Initialisation des HMM pour chaque chiffre")

### Pour 5x3
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

## Pour 5x4
HMM0_5x4 = initHMMDigit(3,20)
HMM1_5x4 = initHMMDigit(3,20)
HMM2_5x4 = initHMMDigit(3,20)
HMM3_5x4 = initHMMDigit(3,20)
HMM4_5x4 = initHMMDigit(3,20)
HMM5_5x4 = initHMMDigit(3,20)
HMM6_5x4 = initHMMDigit(3,20)
HMM7_5x4 = initHMMDigit(3,20)
HMM8_5x4 = initHMMDigit(3,20)
HMM9_5x4 = initHMMDigit(3,20)

### Pour 8
HMM0_8 = initHMMDigit(3,8)
HMM1_8 = initHMMDigit(3,8)
HMM2_8 = initHMMDigit(3,8)
HMM3_8 = initHMMDigit(3,8)
HMM4_8 = initHMMDigit(3,8)
HMM5_8 = initHMMDigit(3,8)
HMM6_8 = initHMMDigit(3,8)
HMM7_8 = initHMMDigit(3,8)
HMM8_8 = initHMMDigit(3,8)
HMM9_8 = initHMMDigit(3,8)

### Pour 16
HMM0_16 = initHMMDigit(3,16)
HMM1_16 = initHMMDigit(3,16)
HMM2_16 = initHMMDigit(3,16)
HMM3_16 = initHMMDigit(3,16)
HMM4_16 = initHMMDigit(3,16)
HMM5_16 = initHMMDigit(3,16)
HMM6_16 = initHMMDigit(3,16)
HMM7_16 = initHMMDigit(3,16)
HMM8_16 = initHMMDigit(3,16)
HMM9_16 = initHMMDigit(3,16)


# Chargement des fichiers train

print("Chargement des fichiers train observations")
# ### Pour 5x3
# obs0_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit0.txt')
# obs1_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit1.txt')
# obs2_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit2.txt')
# obs3_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit3.txt')
# obs4_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit4.txt')
# obs5_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit5.txt')
# obs6_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit6.txt')
# obs7_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit7.txt')
# obs8_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit8.txt')
# obs9_5X3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Train_compute_symbol_5_3Digit9.txt')

### Pour 5x4
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


# baumWelchList
print("baumWelchList")

### Pour 5x3
HMM0_5x3_entraine = baumWelchList(HMM0_5x3, obs0_5X3, 1000)$hmm
print(HMM0_5x3_entraine)
save(HMM0_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM0_5x3_entraine.rdata')
# 
# HMM1_5x3_entraine = baumWelchList(HMM1_5x3, obs1_5X3, 1000)$hmm
# print(HMM1_5x3_entraine)
# save(HMM1_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM1_5x3_entraine.rdata')
# 
# HMM2_5x3_entraine = baumWelchList(HMM2_5x3, obs2_5X3, 1000)$hmm
# print(HMM2_5x3_entraine)
# save(HMM2_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM2_5x3_entraine.rdata')
# 
# HMM3_5x3_entraine = baumWelchList(HMM3_5x3, obs3_5X3, 1000)$hmm
# print(HMM3_5x3_entraine)
# save(HMM3_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM3_5x3_entraine.rdata')
# 
# HMM4_5x3_entraine = baumWelchList(HMM4_5x3, obs4_5X3, 1000)$hmm
# print(HMM4_5x3_entraine)
# save(HMM4_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM4_5x3_entraine.rdata')
# 
# HMM5_5x3_entraine = baumWelchList(HMM5_5x3, obs5_5X3, 1000)$hmm
# print(HMM5_5x3_entraine)
# save(HMM5_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM5_5x3_entraine.rdata')
# 
# HMM6_5x3_entraine = baumWelchList(HMM6_5x3, obs6_5X3, 1000)$hmm
# print(HMM6_5x3_entraine)
# save(HMM6_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM6_5x3_entraine.rdata')
# 
# HMM7_5x3_entraine = baumWelchList(HMM7_5x3, obs7_5X3, 1000)$hmm
# print(HMM7_5x3_entraine)
# save(HMM7_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM7_5x3_entraine.rdata')
# 
# HMM8_5x3_entraine = baumWelchList(HMM8_5x3, obs8_5X3, 1000)$hmm
# print(HMM8_5x3_entraine)
# save(HMM8_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM8_5x3_entraine.rdata')
# 
# HMM9_5x3_entraine = baumWelchList(HMM9_5x3, obs9_5X3, 1000)$hmm
# print(HMM9_5x3_entraine)
# save(HMM9_5x3_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM9_5x3_entraine.rdata')

### Pour 5x4
print("baumWelchList 5X4 0")
HMM0_5x4_entraine = baumWelchList(HMM0_5x4, obs0_5X4, 1000)$hmm
print(HMM0_5x4_entraine)
save(HMM0_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM0_5x4_entraine.rdata')

print("baumWelchList 5X4 1")
HMM1_5x4_entraine = baumWelchList(HMM1_5x4, obs1_5X4, 1000)$hmm
print(HMM1_5x4_entraine)
save(HMM1_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM1_5x4_entraine.rdata')

print("baumWelchList 5X4 2")
HMM2_5x4_entraine = baumWelchList(HMM2_5x4, obs2_5X4, 1000)$hmm
print(HMM2_5x4_entraine)
save(HMM2_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM2_5x4_entraine.rdata')

print("baumWelchList 5X4 3")
HMM3_5x4_entraine = baumWelchList(HMM3_5x4, obs3_5X4, 1000)$hmm
print(HMM3_5x4_entraine)
save(HMM3_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM3_5x4_entraine.rdata')

print("baumWelchList 5X4 4")
HMM4_5x4_entraine = baumWelchList(HMM4_5x4, obs4_5X4, 1000)$hmm
print(HMM4_5x4_entraine)
save(HMM4_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM4_5x4_entraine.rdata')

print("baumWelchList 5X4 5")
HMM5_5x4_entraine = baumWelchList(HMM5_5x4, obs5_5X4, 1000)$hmm
print(HMM5_5x4_entraine)
save(HMM5_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM5_5x4_entraine.rdata')

print("baumWelchList 5X4 6")
HMM6_5x4_entraine = baumWelchList(HMM6_5x4, obs6_5X4, 1000)$hmm
print(HMM6_5x4_entraine)
save(HMM6_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM6_5x4_entraine.rdata')

print("baumWelchList 5X4 7")
HMM7_5x4_entraine = baumWelchList(HMM7_5x4, obs7_5X4, 1000)$hmm
print(HMM7_5x4_entraine)
save(HMM7_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM7_5x4_entraine.rdata')

print("baumWelchList 5X4 8")
HMM8_5x4_entraine = baumWelchList(HMM8_5x4, obs8_5X4, 1000)$hmm
print(HMM8_5x4_entraine)
save(HMM8_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM8_5x4_entraine.rdata')

print("baumWelchList 5X4 9")
HMM9_5x4_entraine = baumWelchList(HMM9_5x4, obs9_5X4, 1000)$hmm
print(HMM9_5x4_entraine)
save(HMM9_5x4_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM9_5x4_entraine.rdata')

### Pour 8

print("baumWelchList 8 0")
HMM0_8_entraine = baumWelchList(HMM0_8, obs0_8, 1000)$hmm
print(HMM0_8_entraine)
save(HMM0_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM0_8_entraine.rdata')

print("baumWelchList 8 1")
HMM1_8_entraine = baumWelchList(HMM1_8, obs1_8, 1000)$hmm
print(HMM1_8_entraine)
save(HMM1_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM1_8_entraine.rdata')

print("baumWelchList 8 2")
HMM2_8_entraine = baumWelchList(HMM2_8, obs2_8, 1000)$hmm
print(HMM2_8_entraine)
save(HMM2_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM2_8_entraine.rdata')

print("baumWelchList 8 3")
HMM3_8_entraine = baumWelchList(HMM3_8, obs3_8, 1000)$hmm
print(HMM3_8_entraine)
save(HMM3_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM3_8_entraine.rdata')

print("baumWelchList 8 4")
HMM4_8_entraine = baumWelchList(HMM4_8, obs4_8, 1000)$hmm
print(HMM4_8_entraine)
save(HMM4_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM4_8_entraine.rdata')

print("baumWelchList 8 5")
HMM5_8_entraine = baumWelchList(HMM5_8, obs5_8, 1000)$hmm
print(HMM5_8_entraine)
save(HMM5_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM5_8_entraine.rdata')

print("baumWelchList 8 6")
HMM6_8_entraine = baumWelchList(HMM6_8, obs6_8, 1000)$hmm
print(HMM6_8_entraine)
save(HMM6_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM6_8_entraine.rdata')

print("baumWelchList 8 7")
HMM7_8_entraine = baumWelchList(HMM7_8, obs7_8, 1000)$hmm
print(HMM7_8_entraine)
save(HMM7_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM7_8_entraine.rdata')

print("baumWelchList 8 8")
HMM8_8_entraine = baumWelchList(HMM8_8, obs8_8, 1000)$hmm
print(HMM8_8_entraine)
save(HMM8_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM8_8_entraine.rdata')

print("baumWelchList 8 9")
HMM9_8_entraine = baumWelchList(HMM9_8, obs9_8, 1000)$hmm
print(HMM9_8_entraine)
save(HMM9_8_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM9_8_entraine.rdata')

### Pour 16

print("baumWelchList 16 0")
HMM0_16_entraine = baumWelchList(HMM0_16, obs0_16, 1000)$hmm
print(HMM0_16_entraine)
save(HMM0_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM0_16_entraine.rdata')

print("baumWelchList 16 1")
HMM1_16_entraine = baumWelchList(HMM1_16, obs1_16, 1000)$hmm
print(HMM1_16_entraine)
save(HMM1_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM1_16_entraine.rdata')

print("baumWelchList 16 2")
HMM2_16_entraine = baumWelchList(HMM2_16, obs2_16, 1000)$hmm
print(HMM2_16_entraine)
save(HMM2_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM2_16_entraine.rdata')

print("baumWelchList 16 3")
HMM3_16_entraine = baumWelchList(HMM3_16, obs3_16, 1000)$hmm
print(HMM3_16_entraine)
save(HMM3_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM3_16_entraine.rdata')

print("baumWelchList 16 4")
HMM4_16_entraine = baumWelchList(HMM4_16, obs4_16, 1000)$hmm
print(HMM4_16_entraine)
save(HMM4_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM4_16_entraine.rdata')

print("baumWelchList 16 5")
HMM5_16_entraine = baumWelchList(HMM5_16, obs5_16, 1000)$hmm
print(HMM5_16_entraine)
save(HMM5_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM5_16_entraine.rdata')

print("baumWelchList 16 6")
HMM6_16_entraine = baumWelchList(HMM6_16, obs6_16, 1000)$hmm
print(HMM6_16_entraine)
save(HMM6_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM6_16_entraine.rdata')

print("baumWelchList 16 7")
HMM7_16_entraine = baumWelchList(HMM7_16, obs7_16, 1000)$hmm
print(HMM7_16_entraine)
save(HMM7_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM7_16_entraine.rdata')

print("baumWelchList 16 8")
HMM8_16_entraine = baumWelchList(HMM8_16, obs8_16, 1000)$hmm
print(HMM8_16_entraine)
save(HMM8_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM8_16_entraine.rdata')

print("baumWelchList 16 9")
HMM9_16_entraine = baumWelchList(HMM9_16, obs9_16, 1000)$hmm
print(HMM9_16_entraine)
save(HMM9_16_entraine, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM9_16_entraine.rdata')



# load('Results/HMM0_5x3_entraine.rdata')

# print("Chargement des hmm entrain�s")
# 
# leHMM_5X3 = c()
# tmp0 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM0_5x3_entraine.rdata')
# tmp1 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM1_5x3_entraine.rdata')
# tmp2 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM2_5x3_entraine.rdata')
# tmp3 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM3_5x3_entraine.rdata')
# tmp4 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM4_5x3_entraine.rdata')
# tmp5 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM5_5x3_entraine.rdata')
# tmp6 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM6_5x3_entraine.rdata')
# tmp7 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM7_5x3_entraine.rdata')
# tmp8 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM8_5x3_entraine.rdata')
# tmp9 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM9_5x3_entraine.rdata')
# 
# leHMM_5X3 = list(tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9)
# matrice_HMM_5X3 = matrix(leHMM_5X3, nrow = 10, ncol = 1, byrow = T)
# 
# save(leHMM_5X3, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM_5x3_entraine.rdata')
# 
# leHMM_5X4 = c()
# tmp0_5X4 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM0_5x4_entraine.rdata')
# tmp1_5X4 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM1_5x4_entraine.rdata')
# tmp2_5X4 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM2_5x4_entraine.rdata')
# tmp3_5X4 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM3_5x4_entraine.rdata')
# tmp4_5X4 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM4_5x4_entraine.rdata')
# tmp5_5X4 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM5_5x4_entraine.rdata')
# tmp6_5X4 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM6_5x4_entraine.rdata')
# tmp7_5X4 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM7_5x4_entraine.rdata')
# tmp8_5X4 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM8_5x4_entraine.rdata')
# tmp9_5X4 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM9_5x4_entraine.rdata')
# 
# leHMM_5X4 = list(m0=tmp0_5X4, m1=tmp1_5X4, m2=tmp2_5X4, m3=tmp3_5X4, m4=tmp4_5X4, m5=tmp5_5X4, m6=tmp6_5X4, m7=tmp7_5X4, m8=tmp8_5X4, m9=tmp9_5X4)
# 
# save(leHMM_5X4, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM_5x4_entraine.rdata')
# 
# 
# leHMM_8 = c()
# tmp0_8 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM0_8_entraine.rdata')
# tmp1_8 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM1_8_entraine.rdata')
# tmp2_8 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM2_8_entraine.rdata')
# tmp3_8 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM3_8_entraine.rdata')
# tmp4_8 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM4_8_entraine.rdata')
# tmp5_8 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM5_8_entraine.rdata')
# tmp6_8 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM6_8_entraine.rdata')
# tmp7_8 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM7_8_entraine.rdata')
# tmp8_8 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM8_8_entraine.rdata')
# tmp9_8 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM9_8_entraine.rdata')
# 
# leHMM_8 = list(m0=tmp0_8, m1=tmp1_8, m2=tmp2_8, m3=tmp3_8, m4=tmp4_8, m5=tmp5_8, m6=tmp6_8, m7=tmp7_8, m8=tmp8_8, m9=tmp9_8)
# 
# save(leHMM_8, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM_8_entraine.rdata')
# 
# leHMM_16 = c()
# tmp0_16 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM0_16_entraine.rdata')
# tmp1_16 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM1_16_entraine.rdata')
# tmp2_16 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM2_16_entraine.rdata')
# tmp3_16 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM3_16_entraine.rdata')
# tmp4_16 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM4_16_entraine.rdata')
# tmp5_16 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM5_16_entraine.rdata')
# tmp6_16 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM6_16_entraine.rdata')
# tmp7_16 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM7_16_entraine.rdata')
# tmp8_16 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM8_16_entraine.rdata')
# tmp9_16 <- load('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM9_16_entraine.rdata')
# 
# leHMM_16 = list(m0=tmp0_16, m1=tmp1_16, m2=tmp2_16, m3=tmp3_16, m4=tmp4_16, m5=tmp5_16, m6=tmp6_16, m7=tmp7_16, m8=tmp8_16, m9=tmp9_16)
# 
# save(leHMM_16, file = 'C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/Results/HMM_16_entraine.rdata')


# classifyDigit <- function(leObs, leHMM, T = 10, N = 1){
#   true = NULL
#   class = NULL
#   
#   for(i in 1:15){
#     true <- c(true, 0)
#     ob <- leObs[i,]
#     print(i)
#     print(ob)
#     l0 <- loglikelihood(leHMM$m0, ob)
#     l1 <- loglikelihood(leHMM$m1, ob)
#     l2 <- loglikelihood(leHMM$m2, ob)
#     l3 <- loglikelihood(leHMM$m3, ob)
#     l4 <- loglikelihood(leHMM$m4, ob)
#     l5 <- loglikelihood(leHMM$m5, ob)
#     l6 <- loglikelihood(leHMM$m6, ob)
#     l7 <- loglikelihood(leHMM$m7, ob)
#     l8 <- loglikelihood(leHMM$m8, ob)
#     l9 <- loglikelihood(leHMM$m9, ob)
#       
#     if(l0>l1&&l0>l2&&l0>l3&&l0>l4&&l0>l5&&l0>l6&&l0>l7&&l0>l8&&l0>l9){
#       class <- c(class,0)
#     }else if(l1>l0&&l1>l2&&l1>l3&&l1>l4&&l1>l5&&l1>l6&&l1>l7&&l1>l8&&l1>l9){
#       class <- c(class,1)
#     }else if(l2>l1&&l2>l0&&l2>l3&&l2>l4&&l2>l5&&l2>l6&&l2>l7&&l2>l8&&l2>l9){
#       class <- c(class,2)
#     }else if(l3>l1&&l3>l2&&l3>l0&&l3>l4&&l3>l5&&l3>l6&&l3>l7&&l3>l8&&l3>l9){
#       class <- c(class,3)
#     }else if(l4>l1&&l4>l2&&l4>l3&&l4>l0&&l4>l5&&l4>l6&&l4>l7&&l4>l8&&l4>l9){
#       class <- c(class,4)
#     }else if(l5>l1&&l5>l2&&l5>l3&&l5>l4&&l5>l0&&l5>l6&&l5>l7&&l5>l8&&l5>l9){
#       class <- c(class,5)
#     }else if(l6>l1&&l6>l2&&l6>l3&&l6>l4&&l6>l5&&l6>l0&&l6>l7&&l6>l8&&l6>l9){
#       class <- c(class,6)
#     }else if(l7>l1&&l7>l2&&l7>l3&&l7>l4&&l7>l5&&l7>l6&&l7>l0&&l7>l8&&l7>l9){
#       class <- c(class,7)
#     }else if(l8>l1&&l8>l2&&l8>l3&&l8>l4&&l8>l5&&l8>l6&&l8>l7&&l8>l0&&l8>l9){
#       class <- c(class,8)
#     } else{
#       class <- c(class,9)
#     }
#       
#     print(class)
#     print(true)
#   }
# 
# 
#   ref = matrix(true, ncol=1)
#   pred = matrix(class,ncol=1)
# 
#   xtab = table(ref,pred)
# 
#   return(xtab)
# }

classifyDigit <- function(merge, test_resources){
  res <- numeric()
  nbRows <- length(merge)/5
  
  nbResources <- length(test_resources)/30
  
  for(res_line in 1:nbResources) {
    current_line <- numeric()
    for(line in 1:nbRows) {
      test <- loglikelihood(merge[line,],test_resources[res_line,])
      current_line <- c(current_line,test)
    }
    res <- c(res, current_line)
  }
  return (matrix(res,nbResources,nbRows))
}

# Chargement des fichiers test

### Pour 5x3
# test0 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit0.txt')
# test1 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit1.txt')
# test2 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit2.txt')
# test3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit3.txt')
# test4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit4.txt')
# test5 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit5.txt')
# test6 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit6.txt')
# test7 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit7.txt')
# test8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit8.txt')
# test9 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X3/Test_compute_symbol_5_3Digit9.txt')
# 
# ### Pour 5x4
# test0 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit0.txt')
# test1 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit1.txt')
# test2 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit2.txt')
# test3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit3.txt')
# test4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit4.txt')
# test5 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit5.txt')
# test6 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit6.txt')
# test7 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit7.txt')
# test8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit8.txt')
# test9 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Data5X4/Test_compute_symbol_5_4Digit9.txt')
# 
# ### Pour 8
# test0 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit0.txt')
# test1 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit1.txt')
# test2 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit2.txt')
# test3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit3.txt')
# test4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit4.txt')
# test5 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit5.txt')
# test6 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit6.txt')
# test7 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit7.txt')
# test8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit8.txt')
# test9 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir8/Test_compute_symbol_dir_8Digit9.txt')
# 
# ### Pour 16
# 
# test0 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/srcC:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit0.txt')
# test1 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit1.txt')
# test2 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit2.txt')
# test3 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit3.txt')
# test4 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit4.txt')
# test5 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit5.txt')
# test6 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit6.txt')
# test7 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit7.txt')
# test8 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit8.txt')
# test9 = Load_Obs('C:/Users/Coraline/Documents/Cora-Cours/SL_Digit_Recognition/src/dataset/Dir16/Test_compute_symbol_dir_16Digit9.txt')

# reco_rate0 <- classifyDigit(leHMM_5X3,test0)
# reco_rate1 <- classifyDigit(test1,#liste des hmm entrainé)
# reco_rate2 <- classifyDigit(test2,#liste des hmm entrainé)
# reco_rate3 <- classifyDigit(test3,#liste des hmm entrainé
# reco_rate4 <- classifyDigit(test4,#liste des hmm entrainé)
# reco_rate5 <- classifyDigit(test5,#liste des hmm entrainé)
# reco_rate6 <- classifyDigit(test6,#liste des hmm entrainé)
# reco_rate7 <- classifyDigit(test7,#liste des hmm entrainé)
# reco_rate8 <- classifyDigit(test8,#liste des hmm entrainé)
# reco_rate9 <- classifyDigit(test9,#liste des hmm entrainé)