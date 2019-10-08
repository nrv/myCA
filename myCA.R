#' @author
#' Nicolas Hervé - http://herve.name

library(irlba)
library(Matrix)

myCA <- function (Y, lessMemory = TRUE) {
  myCA_log("Data preparation ...")
  s = sum(Y)
  P = Y/s
  r = P %*% data.matrix(rep(1, ncol(Y)))
  c = t(P) %*% data.matrix(rep(1, nrow(Y)))
  
  rp = r^-0.5
  cp = c^-0.5
  
  if (lessMemory) {
    d_r = Diagonal(,rp[,1])
    d_c = Diagonal(,cp[,1])
  } else {
    d_r = diag(rp[,1])
    d_c = diag(cp[,1])
  }
  
  myCA_log("Matrix multiplication ...")
  
  S = P - r %*% t(c)
  S = d_r %*% S %*% d_c
  
  if (lessMemory) {
    myCA_log("Launching IRLBA ...")
    svd = irlba(S)    
  } else {
    myCA_log("Launching SVD ...")
    svd = svd(S)
  }
  myCA_log("... done")

  rowProj = d_r %*% svd$u
  colProj = d_c %*% svd$v
  
  output <- list(rowProj = rowProj, colProj = colProj, c = c, d = svd$d)
  class(output) <- "myCA"
  return(output)
}

myCA_log <- function(msg) {
  message(paste(as.POSIXlt(Sys.time()), "[myCA]", msg))
}

myCA_project <- function (myCA, Y2) {
  r2 = Y2 %*% data.matrix(rep(1, ncol(Y2)))
  s2 = r2 %*% t(data.matrix(rep(1, ncol(Y2))))
  P2 = Y2/s2
  S2 = P2 - data.matrix(rep(1, nrow(P2))) %*% t(myCA$c)
  rowProj2 = S2 %*% myCA$colProj / (data.matrix(rep(1, nrow(P2))) %*% myCA$d)
  
  output <- list(rowProj = rowProj2)
  class(output) <- "myCA"
  return(output)
}