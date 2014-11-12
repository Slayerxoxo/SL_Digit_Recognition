stroke <-function(x0=-1,y0=-1,x1=1,y1=1,N=10)
{
  strk <- matrix(c(seq(x0,x1,length=N),seq(y0,y1,length=N)),ncol=2)
  return(strk)
}

simu_symbol <- function()
{
  digit_1 <- rbind(stroke(-0.3,0.5,0.3,1.0,10),stroke(0.3,0.9,0.3,-1.0,20))
  dimnames(digit_1) <- list(num=1:nrow(digit_1),point=c("x","y"))
  plot(digit_1,type="l",col="red",xlim=c(-1,1),ylim=c(-1,1))
  points(digit_1)
  
  digit_4 <- rbind(stroke(0.2,1.0,-0.8,-0.3,10),stroke(-0.85,-0.32,0.5,-0.1,10),stroke(0.3,0.1,0.2,-1.0,10))
  dimnames(digit_4) <- list(num=1:nrow(digit_4),point=c("x","y"))
  plot(digit_4,type="l",col="red",xlim=c(-1,1),ylim=c(-1,1))
  points(digit_4)
  
  digit_6 <- rbind(stroke(-0.5,1.0,-0.5,-0.5,10),stroke(-0.4,-0.5,0.5,-0.5,7),stroke(0.5,-0.4,0.5,0.4,6),stroke(0.4,0.4,-0.5,0.4,7))
  dimnames(digit_6) <- list(num=1:nrow(digit_6),point=c("x","y"))
  plot(digit_6,type="l",col="red",xlim=c(-1,1),ylim=c(-1,1))
  points(digit_6)
  return(list(d1=digit_1,d2=digit_4,d3=digit_6))
}

compute_symbol <- function (trace,nr=5,nc=3)
{
  LUT <- matrix(1:(nr*nc),nrow=nr,ncol=nc,byrow=T) #table de corespondance  Look Up Table
  NB <- length(trace[,"x"])
  Ix <- pmax(pmin(1+floor((trace[,"x"]-(-1))*nc/2),rep(nc,NB)),rep(1,NB))
  Iy <- pmax(pmin(1+floor((trace[,"y"]-(-1))*nr/2),rep(nr,NB)),rep(1,NB))
  return(LUT[matrix(c(Iy, Ix),ncol=2)])
}

compute_symbol_dir <- function (trace,nangle=8)
{
  NB <- length(trace[,"x"])
  delta <- trace
  delta[1:(NB-1),] <- delta[2:NB,]
  delta <- delta - trace
  delta[NB,] <- delta[NB-1,]
  angle <- atan2(delta[,"y"],delta[,"x"]) + pi/nangle
  angle[angle < 0] <- angle[angle < 0] + 2*pi
  angle <- pmin(1 + floor(angle*nangle/(2*pi)),nangle)
  return(angle)
}

tmp1 <- simu_symbol()
# res1 <- compute_symbol(tmp1$d1)
# res2 <- compute_symbol(tmp1$d2)
# res3 <- compute_symbol(tmp1$d3)
res1 <- compute_symbol_dir(tmp1$d1)
res2 <- compute_symbol_dir(tmp1$d2)
res3 <- compute_symbol_dir(tmp1$d3)
cat("pour 1 :", res1, "\n","pour 4 :", res2, "\n","pour 6 :", res3, "\n")