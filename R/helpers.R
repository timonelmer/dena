
#' @export
starIt <- function(x){
  ifelse(x > 0.05, "", ifelse(x > 0.01, "*", ifelse(x > 0.001, "**","***")))
}

#' @export
coxmeTable <- function(x){
  # taken from coxme.print funciton from coxme package
  out <- list()
  beta <- x$coefficients
  nvar <- length(beta)
  nfrail<- nrow(x$var) - nvar
  
  loglik <- x$loglik + c(0,0, x$penalty)
  temp <- matrix(loglik, nrow=1)
  #cat("\n  Iterations=", x$iter, "\n")
  dimnames(temp) <- list("Log-likelihood", 
                         c("NULL", "Integrated", "Fitted"))
  out[[1]] <- temp
  
  chi1 <- 2*diff(x$loglik[c(1,2)]) 
  chi1 <- 2*diff(loglik[1:2]) 
  chi2 <- 2*diff(loglik[c(1,3)])
  temp <- rbind(c(round(chi1,2), round(x$df[1],2),
                  signif(1- pchisq(chi1,x$df[1]),5),
                  round(chi1- 2*x$df[1],2),
                  round(chi1- log(x$n[1])*x$df[1],2)),
                c(round(chi2,2), round(x$df[2],2),
                  signif(1- pchisq(chi2,x$df[2]),5),
                  round(chi2- 2*x$df[2],2),
                  round(chi2- log(x$n[1])*x$df[2],2)))
  dimnames(temp) <- list(c("Integrated loglik", " Penalized loglik"),
                         c("Chisq", "df", "p", "AIC", "BIC"))
  out[[2]] <- temp
  
  if (nvar > 0)  { # Not a ~1 model
    se <- sqrt(diag(as.matrix(x$var))[nfrail+1:nvar])
    tmp <- cbind(beta, exp(beta), se, round(beta/se,2),
                 signif(1 - pchisq((beta/ se)^2, 1), 2))
    dimnames(tmp) <- list(names(beta), c("coef", "exp(coef)",
                                         "se(coef)", "z", "p"))
    out[[3]] <- tmp
  }
  coef <- unlist(lapply(coxme::ranef(x), function(y) {
    if (is.matrix(y)) {
      z <- c(y)
      dd <- dimnames(y)
      names(z) <- c(outer(dd[[1]], dd[[2]], paste,sep=':'))
      z}
    else y
  }))
  
  se <- sqrt(diag(as.matrix(x$var))[1:nfrail])
  rtmp <- cbind(coef, exp(coef), se)
  dimnames(rtmp) <- list(names(coef), c("coef", "exp(coef)",
                                        "Penalized se"))
  out[[4]] <- rtmp
  return(out)
}


summary.denafit <- function(fit){
  if(attr(fit,"model") == "frailty") print(fit[[2]])
  if(attr(fit,"model") == "cmm") print(fit[[1]])
}

#' @export
survDatCheck <- function(dat, nestVars = NULL, start.time = "start", end.time = "end", from = "from", to = "to"){
  
  
  if(is.null(nestVars)){
    for(row in 1:nrow(dat)){
      if(row == 1) next
      dat[row,"FlagGap_withBefore"] <- ifelse(dat[row-1,end.time] < dat[row,start.time], 1,0)
      dat[row,"FlagOverlap_withBefore"] <- ifelse(dat[row-1,end.time] > dat[row,start.time], 1,0)
      dat[row,"FlagTeleport_withBefore"] <- ifelse(dat[row-1,to] == dat[row,from], 0,1)
      cat(paste0("\r", row, " of: ", nrow(dat)))
    }
  }
  if(length(nestVars) == 1){
    for(nv1 in unique(dat[,nestVars[[1]]])){
      cat(paste0("\n ",nestVars[1], ": ", nv1))
      tmp <- dat[dat[,nestVars[1]] %in% nv1,]
      for(row in 1:nrow(tmp)){
        if(row == 1) next
        c1 <- ifelse(tmp[row-1,end.time] < tmp[row,start.time], 1,0)
        c2<- ifelse(tmp[row-1,end.time] > tmp[row,start.time], 1,0)
        c3 <- ifelse(tmp[row-1,to] == tmp[row,from], 0,1)
      }
      
      dat[dat[,nestVars[1]] %in% nv1,"FlagGap_withBefore"] <- c1
      dat[dat[,nestVars[1]] %in% nv1,"FlagOverlap_withBefore"] <- c2
      dat[dat[,nestVars[1]] %in% nv1,"FlagTeleport_withBefore"] <- c3
      
    }
  } # end nestVars = 1 loop
  if(length(nestVars) == 2){
    for(nv1 in unique(dat[,nestVars[[1]]])){
      cat(paste0("\n ",nestVars[1], ": ", nv1))
      #tmp <- dat[dat[,nestVars[1]] %in% nv1,]
      for(nv2 in unique(dat[dat[,nestVars[1]] %in% nv1,nestVars[2]])){
        cat(paste0(" || ",nestVars[2], ": ", nv2))
        tmp <- dat[dat[,nestVars[1]] %in% nv1 & dat[,nestVars[2]] %in% nv2,]
        for(row in 1:nrow(tmp)){
          if(row == 1) next
          dat[dat[,nestVars[1]] %in% nv1 & dat[,nestVars[2]] %in% nv2,"FlagGap_withBefore"][row] <- ifelse(tmp[row-1,end.time] < tmp[row,start.time], 1,0)
          dat[dat[,nestVars[1]] %in% nv1 & dat[,nestVars[2]] %in% nv2,"FlagOverlap_withBefore"][row] <- ifelse(tmp[row-1,end.time] > tmp[row,start.time], 1,0)
          dat[dat[,nestVars[1]] %in% nv1 & dat[,nestVars[2]] %in% nv2,"FlagTeleport_withBefore"][row] <- ifelse(tmp[row-1,to] == tmp[row,from], 0,1)
        }
      }
    }
  } # end nestVars = 2 loop
  if(length(nestVars) > 2) stop("more than two nest Vars not implemented")
  return(dat)
}


