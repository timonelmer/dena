QAP.MG <- function(dvs, ivs,  iv.names = iv.names, mode = "yQAP" ,samples = 1000, diag = F, directed = T, 
                   global.deltas = T, ecdf.plot = F, return.perms = F){
  pb <- txtProgressBar(min = 0, max = samples, style = 3) # set progress bar
  
  
  if(!diag){ # get rid of diagonal values
    for(DV in 1:length(dvs)){
      diag(dvs[[DV]]) <- NA
    }
    
    for(IV in 1:length(ivs)){  
      IV.dat <- ivs[[IV]]
      for(subIV in 1:length(ivs[[IV]])){
        diag(ivs[[IV]][[subIV]]) <- NA
      }
    }
  }
  
  
  
  # get the observed estimates
  
  if(directed){
    observedLm <- lm(unlist(dvs) ~ Reduce(rbind,lapply(1:length(ivs),
                                                       function(x) sapply(ivs[[x]], function(y) y)
    )))
    
  }else{
    
    observedLm <- lm(unlist(lapply(dvs, function(x) x[lower.tri(x, diag = diag)])) ~
                       Reduce(rbind,lapply(1:length(ivs),function(x) sapply(ivs[[x]], function(y) y[lower.tri(y, diag = diag)]))))
    
  }
  
  
  if(mode == "linearregression"){
    names(observedLm$coefficients) <- iv.names
    return(observedLm) 
  }
  
  # prepare output file
  observedEstimates <- observedLm$coefficients
  output <- data.frame(Estimates = observedEstimates)
  rownames(output) <- iv.names
  
  r.squared <- c(summary(observedLm)$r.squared, summary(observedLm)$adj.r.squared)
  names(r.squared) <- c("r.squared","adj.r.squared")
  
  
  
  if(mode == "yQAP"){
    sampledEstimates <- data.frame()
    for(sampleNr in 1:samples){
      if(directed){
        sampledEstimates <- rbind(sampledEstimates, lm(unlist(lapply(dvs, function(x) sample(x))) ~ Reduce(rbind,lapply(1:length(ivs),
                                                                                                                        function(x) sapply(ivs[[x]], function(y) y)
        )))$coefficients)
      }else{
        sampledEstimates <- rbind(sampledEstimates, lm(unlist(lapply(dvs, function(x) sample(x[lower.tri(x, diag = diag)]))) ~ 
                                                         Reduce(rbind,lapply(1:length(ivs), function(x) sapply(ivs[[x]], function(y) y[lower.tri(y, diag = diag)])
                                                         )))$coefficients)
      }
      
      setTxtProgressBar(pb, sampleNr)
    }
    
    ecdf.plots <- list()
    for(est in 1:length(observedEstimates)){
      
      output[est,"p(1sided)"] <- ecdf(sampledEstimates[,est])(observedEstimates[est])
      output[est,"abs(p)"] <- output[est,"p(1sided)"] 
      output[est,"abs(p)"][output[est,"p(1sided)"] > 0.5] <- 1-output[est,"abs(p)"][output[est,"p(1sided)"] > 0.5]
      output[est,"adj.d"] <- abs(mean(sampledEstimates[,est]) - observedEstimates[est])/sd(sampledEstimates[,est])
      output[est,"Exp.V"] <- mean(sampledEstimates[,est], na.rm = T)
      output[est,"Exp.V.sd"] <- sd(sampledEstimates[,est], na.rm = T)
      output[est,"2.5th P"] <- quantile(sampledEstimates[,est],probs=c(.025))
      output[est,"97.5th P"] <- quantile(sampledEstimates[,est],probs=c(.975))
      
      
      if(ecdf.plot){
        
        require(ggplot2)
        
        ecdf.plots[[est]] <- plot(ecdf(sampledEstimates[,est])(observedEstimates[est]))
        
      }
    }
    
  }
  
  
  output <- round(output,5)
  
  stars <- function(p.value){
    ifelse(p.value > 0.90 | p.value < 0.10,
           ifelse(p.value > 0.95 | p.value < 0.05, 
                  ifelse(p.value > 0.99 | p.value < 0.01, 
                         ifelse(p.value > 0.999 | p.value < 0.001, "***", "**"), "*"), "x"), "")
  }
  output[,"significance"] <- sapply(output$`p(1sided)`, stars)
  if(return.perms) return(list(sampledEstimates, observedEstimates))
  return(list(mode = c(mode, samples), plots = ecdf.plots,output = output, r.squared = r.squared))
  
}