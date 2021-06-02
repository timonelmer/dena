
#' @export
done <- function() system("say done")

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


#' MAIN TITLE
#' 
#' initial description
#'
#' @param datashape 
#' 
#' @return 
#' 
#' @examples 
#'
#' @export
getFrails <- function(fits, datashape = "long"){
  if(datashape == "long"){
    frails <- data.frame(ID = NA)
    for(i in 1:length(fits)){
      tmp <- as.data.frame(fits[[i]][[1]]$frail$ID)
      tmp$ID <- rownames(tmp)
      colnames(tmp)[1] <- attr(fits[[i]][[1]],"cat")
      frails <- merge(frails, tmp, by = "ID", all = T)
    }
  }else{
    frails <- sapply(fits, function(x) x$frail)
    colnames(frails) <- sapply(fits, function(x) attr(x, "cat"))
  }
  return(frails)
}


#summary.coxph from survival package
summary.coxphTE <- function(object,  conf.int = 0.95, scale = 1, ...) {
  cox<-object
  beta <- cox$coefficients * scale
  if (is.null(cox$coefficients)) {   # Null model
    return(object)  #The summary method is the same as print in this case
  }
  nabeta <- !(is.na(beta))          #non-missing coefs
  beta2 <- beta[nabeta]
  if(is.null(beta) | is.null(cox$var))
    stop("Input is not valid")
  se <- sqrt(diag(cox$var)) * scale
  if (!is.null(cox$naive.var)) nse <- sqrt(diag(cox$naive.var))
  
  rval<-list(call=cox$call,fail=cox$fail, na.action=cox$na.action,
             n=cox$n, loglik=cox$loglik)
  if (!is.null(cox$nevent)) rval$nevent <- cox$nevent
  
  if (is.null(cox$naive.var)) {
    tmp <- cbind(beta, exp(beta), se, beta/se,
                 pchisq((beta/ se)^2, 1, lower.tail=FALSE))
    dimnames(tmp) <- list(names(beta), c("coef", "exp(coef)",
                                         "se(coef)", "z", "Pr(>|z|)"))
  } else {
    tmp <- cbind(beta, exp(beta), nse, se, beta/se,
                 pchisq((beta/ se)^2, 1, lower.tail=FALSE))
    dimnames(tmp) <- list(names(beta), c("coef", "exp(coef)",
                                         "se(coef)", "robust se", "z", "Pr(>|z|)"))
  }
  rval$coefficients <- tmp
  
  if (conf.int) {
    z <- qnorm((1 + conf.int)/2, 0, 1)
    tmp <- cbind(exp(beta), exp(-beta), exp(beta - z * se),
                 exp(beta + z * se))
    dimnames(tmp) <- list(names(beta), c("exp(coef)", "exp(-coef)",
                                         paste("lower .", round(100 * conf.int, 2), sep = ""),
                                         paste("upper .", round(100 * conf.int, 2), sep = "")))
    rval$conf.int <- tmp
  }
  
  df <- length(beta2)
  logtest <- -2 * (cox$loglik[1] - cox$loglik[2])
  rval$logtest <- c(test=logtest,
                    df=df,
                    pvalue= pchisq(logtest, df, lower.tail=FALSE))
  # rval$sctest <- c(test=cox$score,
  #                  df=df,
  #                  pvalue= pchisq(cox$score, df, lower.tail=FALSE))
  rval$rsq<-c(rsq=1-exp(-logtest/cox$n),
              maxrsq=1-exp(2*cox$loglik[1]/cox$n))
  rval$waldtest<-c(test=as.vector(round(cox$wald.test, 2)),
                   df=df,
                   pvalue= pchisq(as.vector(cox$wald.test), df, 
                                  lower.tail=FALSE))
  if (!is.null(cox$rscore))
    rval$robscore<-c(test=cox$rscore,
                     df=df,
                     pvalue= pchisq(cox$rscore, df, lower.tail=FALSE))
  rval$used.robust<-!is.null(cox$naive.var)
  
  if (!is.null(cox$concordance)) {
    # throw away the extra info, in the name of backwards compatability
    rval$concordance <- cox$concordance[6:7]
    names(rval$concordance) <- c("C", "se(C)")
  }
  if (inherits(cox, "coxphms")) {
    rval$cmap <- cox$cmap
    rval$states <- cox$states
  }
  
  class(rval)    <-"summary.coxph"
  rval
}



corstarsl <- function(x, bonferroni = F, which.spearman = NULL){ 
  
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  if(!is.null(which.spearman)){
    R.spearman <- rcorr(x,type = "spearman")$r
    p.spearman <- rcorr(x,type = "spearman")$P
    
    R[which.spearman,] <- R.spearman[which.spearman,]
    R[,which.spearman] <- R.spearman[,which.spearman]
    
    p[which.spearman,] <- p.spearman[which.spearman,]
    p[,which.spearman] <- p.spearman[,which.spearman]
  }
  
  if(bonferroni == T){
    p <- p.adjust(rcorr(x)$P, method = "bonferroni")
  }
  
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

#' Cox & Snell pseudo R-squared 
#' 
#' @details part of the code was taken from the survival package
#' 
#' @references 
#' @export
coxR2 <- function(fit){
  logtest <- -2 * (fit$loglik[1] - fit$loglik[2])
  out <- c(rsq = 1 - exp(-logtest/fit$n), maxrsq = 1 - 
                  exp(2 * fit$loglik[1]/fit$n))
  return(out)
}
