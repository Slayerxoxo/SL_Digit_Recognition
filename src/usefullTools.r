# confusion matrix
test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  return (table(true, cres))
}

#recognition rate
test.reco <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  return (as.numeric(sum(true == cres)))
}

# convert a vector of classes in a matrix of 0 and 1 (objectif value matrix)
class.ind <- function (cl) {
  n <- length(cl)
  cl <- as.factor(cl)
  x <- matrix(0, n, length(levels(cl)))
  x[(1L:n) + n * (unclass(cl) - 1L)] <- 1
  dimnames(x) <- list(names(cl), levels(cl))
  x
}

#generate the matrix of observation from a text file => usefull for training
Load_Obs <- function(file){
  obs <- read.table(file)
  cat(dim(obs)[1], " examples loaded of size ",dim(obs)[2],"\n")
  return(matrix(t(obs),ncol=dim(obs)[2],byrow=T))
}

#load all file => usefull for test premet de charger tous les fichiers
#pour différencier les trains et les tests, il faut regarder le nom du fichier
loadAll <- function(rootName){
  cl <- NULL
  allobs <- NULL
  for(i in 0:9){
    obs <- Load_Obs(paste(rootName, i, ".txt", sep=""))
    allobs <- rbind(allobs, obs)
    cl <- c(cl, rep(i,dim(obs)[1]))
  }
  return (list(obs=allobs, cl=cl))
}

logadd <- function(min, max)
{
  if (min == -Inf & max == -Inf) {
    return(min)
  } else if (min <= max) {
    return(max + log1p(exp(min - max)))
  } else {
    return(min + log1p(exp(max - min)))
  }
}
