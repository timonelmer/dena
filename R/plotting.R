# plotting #
testing = F 

#' MAIN TITLE
#' 
#' initial description
#'
#' @param 
#' 
#' @return 
#' 
#' @examples 
#'
#' @export
plot.coxph <- function(fit, fontsize = 12){
  require(ggplot2)
  coef <- as.data.frame(summary(fit)$coeff)
  
  coef$coef_UB <- coef$coef + 1.96*coef$`se(coef)`
  coef$coef_LB <- coef$coef - 1.96*coef$`se(coef)`
  coef$name <- rownames(coef)
  
  ggplot(coef[!is.na(coef$coef),], aes(x = name, y = coef, color = coef>0)) +
    geom_point() +
    geom_errorbar(aes(ymin = coef_LB, ymax = coef_UB), width = 0.1) +
    geom_hline(yintercept = 0, linetype ="dotted") +
    theme_minimal() +
    scale_color_manual(values = c("darkred","forestgreen")) +
    theme(axis.text.y = element_text(size = fontsize, color = "black"),
          axis.text.x= element_text(size = fontsize, color = "black"),
          legend.position = "none") +
    ylab("Coefficient") +
    xlab("") +
    coord_flip()
}

#' MAIN TITLE
#' 
#' initial description
#'
#' @param labels Vector with the labels to be displayed. Provided in the original order (i.e., before ordering)
#' 
#' @return 
#' 
#' @examples 
#'
#' @export
plot.coxme <- function(fit, fontsize = 12, labels = NULL, order = NULL, ...){
  require(ggplot2)
  coef <- as.data.frame(coxmeTable(fit)[[3]])
  
  coef$coef_UB <- coef$coef + 1.96*coef$`se(coef)`
  coef$coef_LB <- coef$coef - 1.96*coef$`se(coef)`
  if(!is.null(labels)) rownames(coef)<- labels 
  if(is.null(order)) order <- 1:nrow(coef)
  coef$name <- factor(rownames(coef), levels = rownames(coef)[order])
  
  
  g <- ggplot(coef[!is.na(coef$coef),], aes(x = name, y = coef, color = coef>0)) +
    geom_point() +
    geom_errorbar(aes(ymin = coef_LB, ymax = coef_UB), width = 0.1) +
    geom_hline(yintercept = 0, linetype ="dotted") +
    theme_minimal() +
    scale_color_manual(values = c("darkred","forestgreen")) +
    theme(axis.text.y = element_text(size = fontsize, color = "black"),
          axis.text.x= element_text(size = fontsize, color = "black"),
          legend.position = "none") +
    ylab("Coefficient") +
    xlab("") +
    coord_flip()
  plot(g)
}

#' MAIN TITLE
#' 
#' initial description
#'
#' @param 
#' 
#' @return 
#' 
#' @examples 
#'
#' @export
plot.cmm <- function(fits, fontsize = 12, order = NULL, labels = NULL, stars = F, ...){
  require(ggplot2)
  
  if(!is.null(labels)) fits$var <- labels 
  if(is.null(order)) order <- 1:length(unique(fits$var))
  fits$var <- factor(fits$var, levels = fits$var[1:length(unique(fits$var))][order])
  if(!stars) fits$sig <- NA 
  
  g <- ggplot(fits[!is.na(fits$coef),], aes(x = var, y = coef, color = cat)) +
    geom_point(position = position_dodge(width=0.5)) +
    geom_errorbar(aes(ymin = coef_LB, ymax = coef_UB),
                  width = 0.2,
                  position=position_dodge(width=0.5)) +
    coord_flip() +
    ylab("Coefficient") +
    xlab("") +
    geom_text(aes(x = var, y = coef, color = factor(cat), label = sig),
              position = position_dodge(width=0.5)) +
    scale_color_manual("",values = RColorBrewer::brewer.pal(n = length(unique(fits$cat)), name = "Dark2")) +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme(axis.text.x = element_text(angle = 90))  +
    guides(color = guide_legend(reverse = TRUE))
  plot(g)
}

#' MAIN TITLE
#' 
#' initial description
#'
#' @param 
#' 
#' @return 
#' 
#' @examples 
#'
#' @export
plot.denafit <- function(fit, type = "coef", ...){
  
  if(attr(fit,"model") == "frailty") {
    if(type == "frailty"){
      temp.df <- data.frame(id = names(fit[[2]]$frail[[1]]), frailty = fit[[2]]$frail[[1]])
      
      plot(ggplot(temp.df, aes(x = frailty))+geom_histogram(bins = 10)+theme_minimal())
    }else{
      if(attr(fit,"eMethod") == "coxme") plot.coxme(fit[[2]],...)
      if(attr(fit,"eMethod") == "coxph") plot.coxph(fit[[2]],...)
    }
  }
  if(attr(fit,"model") %in% c("cmm","multistate")) plot.cmm(fit[[1]],...)
  if(attr(fit,"model") == "cmm" & type == "frailcor") frailcor(fit[[2]]) 
  
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
frailcor <- function(fits, datashape = "short"){
  if(datashape == "long"){
    frails <- data.frame(ID = NA)
    for(i in 1:length(fits)){
      tmp <- as.data.frame(fits[[i]][[1]]$frail$ID)
      tmp$ID <- rownames(tmp)
      colnames(tmp)[1] <- attr(fits[[i]][[1]],"cat")
      frails <- merge(frails, tmp, by = "ID", all = T)
    }
    #car::spm(frails[,-1])
    GGally::ggpairs(as.data.frame(frails[,-1])) + jtools::theme_apa()
    #if(!return.plot) return(frails)
  }else{
    frails <- sapply(fits, function(x) x$frail)
    colnames(frails) <- sapply(fits, function(x) attr(x, "cat"))
    #psych::pairs.panels(frails)
    GGally::ggpairs(as.data.frame(frails)) + jtools::theme_apa()
    #if(!return.plot) return(frails)
  }
}

# survival function
#' MAIN TITLE
#' 
#' initial description
#'
#' @param 
#' 
#' @return 
#' 
#' @examples 
#'
#' @export
survivalFunction <- function(dat, timeVar, eventVar, plotType = NULL, verbose = T){
  require(RColorBrewer)
  #dat = dat.sim$data[dat.sim$data$X == c(1,0),]
  # timeVar = "y"
  # eventVar = "failed"
  
  min.t <- min(dat[,timeVar], na.rm =T )
  if(min.t < 0) min.t <- 0 
  max.t <- max(dat[,timeVar], na.rm = T)
  meta <- data.frame(t =  (min.t-1):(max.t+1))
  #for(t in min.t:(max.t+1)){
  times <- unique(dat[,timeVar])
  for(t in times[order(times)]){
    if(is.na(t)) next
    meta[meta$t == t,"prob.event"] <- nrow(dat[dat[,timeVar] < t,])/nrow(dat)
    
    if(nrow(dat[dat[,timeVar] == t,]) > 0) {
      # compute h.atom (see Broström, 2012, p. 25)
      meta[meta$t == t,"h.atom"] <- 
        nrow(dat[dat[,timeVar] == t,])/nrow(dat[dat[,timeVar] >= t,])
      
      meta[meta$t == t,"nelson.aalen.e"] <- sum(meta[meta$t <= t,"h.atom"], na.rm = T)
      meta[meta$t == t,"kaplan.meier.e"] <- prod(1-meta[meta$t <= t,"h.atom"], na.rm = T)
    }
   # if(verbose) cat(paste0("\r  timepoint ", t, " out of ", max(dat[,timeVar], na.rm = T)))
    if(verbose) cat(paste0("\r  timepoint ", which(t == times), " out of ", length(times)))
  }
  meta$surv.prob <- 1-meta$prob.event 
  #print(plot(meta[,c("t","surv.prob")]))
  
  if(!is.null(plotType)){
    #kaplan-meier estimation
    if(plotType == "Kaplan-Meier") {g.km <- ggplot2::ggplot(meta[!is.na(meta$surv.prob),], ggplot2::aes(x = t, y = kaplan.meier.e)) + ggplot2::geom_point() + ggplot2::theme_minimal()  +
      ggplot2::geom_line() + ggplot2::ylab("Kaplan-Meier estimate") +
      geom_line() +
      ggplot2::xlab("Time passed since last event")  #+ geom_smooth()
    plot(g.km)}
    
    #nelson.aalen estimation
    if(plotType == "Nelson-Aalen") {g.na <- ggplot2::ggplot(meta[!is.na(meta$surv.prob),], ggplot2::aes(x = t, y = nelson.aalen.e)) + ggplot2::geom_point() + ggplot2::theme_minimal()  +
      ggplot2::ylab("Nelson-Aalen estimate") +
      geom_line() +
      ggplot2::xlab("Time passed since last event")  #+ geom_smooth()
    plot(g.na)}
    
    # surv.plot
    if(plotType == "Survival") {g.s <- ggplot2::ggplot(meta[!is.na(meta$surv.prob),], ggplot2::aes(x = t, y = surv.prob)) + 
      ggplot2::geom_point() + ggplot2::theme_minimal()  +
      geom_line() +
      ggplot2::ylab("Survival probability S(t)") +
      ggplot2::xlab("Time passed since last event (T)")  #+ geom_smooth()
    plot(g.s)}
  }
  return(meta)
}

# test
if(testing){
dat.sim <- coxed::sim.survdata(N=200, T=500, num.data.frames=1, xvars = 1, 
                        mu = 0.5,
                        sd = 0.1,
                        censor = 0,
                        #hazard.fun = hazard.fun, 
                        beta=c(2))

survivalFunction(dat.sim$data, "y")

survivalFunction(int[int$ID %in% 1000,], "date", "event", plot = T)
}
# nestedSurvivalFunction
#' MAIN TITLE
#' 
#' initial description
#'
#' @param 
#' 
#' @return 
#' 
#' @examples 
#'
#' @export
nestedSurvivalFunction <- function(dat, nestVar, timeVar, eventVar, plot = T, verbose = F){
  meta <- data.frame()
  for(id in unique(dat[,nestVar])){
    tmp <- survivalFunction(dat[dat[,nestVar] %in% id,], timeVar, verbose = verbose, plot = F)
    tmp$id <- id
    meta <- rbind(meta, tmp)
  }
  
  
  # surv.plot
  if(plot){
    n.id <- length(unique(meta$id))
    mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(n.id)
  g.s <- ggplot2::ggplot(meta, ggplot2::aes(x = t, y = surv.prob, color = factor(id))) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(1)) + ggplot2::theme_minimal()  + 
    ggplot2::geom_line(position = ggplot2::position_dodge(1)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_color_manual(values = mycolors) +
    ggplot2::ylab("Survival probability / Still alone probabilty") +
    ggplot2::ylim(c(0,1)) +
    ggplot2::xlab("Time passed since last event")  #+ geom_smooth()
  plot(g.s)
  }
  
  return(meta[,c("id","t","prob.event","surv.prob")])
}

if(testing){
load("data/simdat2.RData")
nestedSurvivalFunction(simdat2, nestVar = "id",timeVar = "time")
}

### print
print.denafit <- function(fit, type = "none"){
  if(attr(fit,"model") == "frailty") print(fit[[2]])
  if(type == "none") print(fit[[1]])else{
    #if the estimates of a separate competing risk should be shown
    structure <- sapply(fit[[2]], attributes)
    show <- fit[[2]][[which(structure[3,]== type)]]
    cat(paste0(attributes(show)$cat,"\n"))
    show
  }
  
}


#' MAIN TITLE
#' 
#' initial description
#'
#' @param 
#' 
#' @return 
#' 
#' @examples 
#'
#' @export
getTransitionMatrix <- function(dat, catVar, type = "sum", categories = NULL){
  #catVar = "alter"
  
  if(is.null(categories)) categories <- unique(dat[,catVar])
   mat <- table(from  = categories, currentstate = categories)
   diag(mat) <- 0
  mat <- matrix(0, length(categories),length(categories), dimnames = list(categories, categories))
  
  tmp <- data.frame(from = c(NA,dat[-nrow(dat),catVar]),currentstate = dat[,catVar])
  
  #mat <- as.matrix(table(tmp))
  tmp.sum <- as.matrix(table(tmp))
    for(i in categories){
      for(j in categories){
        mat[i,j] <- tmp.sum[i,j]
      }
    }
  
  if(type == "sum") return(mat)
  if(type == "prob") return(mat/sum(mat))
  if(type == "rowProb") return(mat/rowSums(mat))
  
}
if(testing){
  load("../../Doktorat/Datasets/iSAHIB/iSAHIB_2021-03-16.RData")
  #dat <- int[int$ID == 1003,]
  getTransitionMatrix(dat = int[int$ID == 1001,], catVar = "alter")
  msm::statetable.msm(alter, ID, data=int[int$ID == 1001,])
  
  getTransitionMatrix(dat = int[1:3,], catVar = "alter")
  msm::statetable.msm(alter, ID, data=int[1:3,])
  
  getTransitionMatrix(int, catVar = "alter", type = "rowProb")
  }


#' MAIN TITLE
#' 
#' initial description
#'
#' @param 
#' 
#' @return 
#' 
#' @examples 
#'
#' @export
plotTransitionNetwork <- function(dat, catVar, type = "sum", categories = NULL, returnMat = T, title = NULL){
  require(ggraph)
  mat <- getTransitionMatrix(dat = dat, catVar = catVar, type = type, categories = categories)
  
  ggraph(mat)+
    geom_edge_loop(aes(width = weight, label = round(weight,2), color = scale(weight), alpha = (weight/100)-.80),
                  label_color ="black",
                  arrow = arrow(length = unit(3, 'mm'), type = "closed"),
                  end_cap = circle(3, 'mm')) + 
    geom_edge_fan(aes(width = weight, label = round(weight,2), color = scale(weight), alpha = weight/100),
                  label_color ="black",
                  arrow = arrow(length = unit(3, 'mm'), type = "closed"),
                  end_cap = circle(3, 'mm')) + 
    scale_edge_width(range = c(1, 10)) + # control size of edge width
    geom_node_point(size = 5, color = "gray20") +
    geom_node_text(aes(label = name), fontface = "bold",  repel = TRUE) +
    ggtitle(title)+
    theme_void() +
    theme(legend.position = 'none')
}
if(testing){
  load("../../Doktorat/Datasets/iSAHIB/iSAHIB_2021-03-16.RData")
  
  plotTransitionNetwork(dat = int[int$ID == 1001,], title = "ID = 1001", catVar = "alter")
  plotTransitionNetwork(dat = int[int$ID == 1001,], title = "ID = 1001", type ="rowProb",catVar = "alter")
  plotTransitionNetwork(dat = int, title = "ID = all", type ="sum",catVar = "alter")
}


