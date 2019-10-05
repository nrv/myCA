#' @author
#' Nicolas Hervé - http://herve.name

myCA <- function (Y) {
  s = sum(Y)
  P = Y/s
  r = P %*% data.matrix(rep(1, ncol(Y)))
  c = t(P) %*% data.matrix(rep(1, nrow(Y)))
  
  rp = r^-0.5
  cp = c^-0.5
  d_r = diag(rp[,1])
  d_c = diag(cp[,1])
  
  S = P - r %*% t(c)
  S = d_r %*% S %*% d_c
  
  svd = svd(S)
  
  rowProj = d_r %*% svd$u
  colProj = d_c %*% svd$v
  
  output <- list(rowProj = rowProj, colProj = colProj, c = c, d = svd$d)
  class(output) <- "myCA"
  return(output)
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