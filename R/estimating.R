# estimating
source("R/helpers.R")
usethis::use_package("ggplot2")
usethis::use_package("survival")
testing = F


#####################################################
##### basic frailty model with one event type ######
###################################################

#' @export
frailty.model <- function(formula, dat, # dat in long format
                 diagnostics = F, plot = F, verbose = F, ...){
    
  require(survival)
  # estimate
  # check if it is a frailty model from survival package, otherwise use coxme
  eMethod <- ifelse(length(grep("frailty",sapply(formula[[3]], unlist))) > 0,"coxph","coxme")
  if(eMethod == "coxph"){
    m <- survival::coxph(formula, data = dat, ...)
    fit <- as.data.frame(summary(m)$coefficients)
  
  }else{
    # frailty(ID) is depreciated, preferred is coxme  https://stat.ethz.ch/pipermail/r-help/2007-April/130315.html
    m <- coxme::coxme(formula, data = dat, ...)
    fit <- as.data.frame(coxmeTable(m)[[3]])
    fit$se2 <- fit$`se(coef)`
  }
  
  fit$coef_UB <- fit$coef + 1.96*fit$se2 
  fit$coef_LB <- fit$coef - 1.96*fit$se2 
  fit$var <- factor(rownames(fit))
  rownames(fit) <- 1:nrow(fit)
  
  if(plot){
    g <- ggplot(fit[!is.na(fit$coef),], aes(x = var, y = coef, color = coef > 0)) +
      geom_point(position = position_dodge(width=0.5)) +
      geom_errorbar(aes(ymin = coef_LB, ymax = coef_UB),
                    width = 0.2,
                    position=position_dodge(width=0.5)) +
      coord_flip() +
      ylab("Coefficient") +
      xlab("") +
      scale_color_manual(values = c("red","forestgreen")) +
      theme_minimal() +
      geom_hline(yintercept = 0, linetype = "dotted") +
      theme(axis.text.x = element_text(angle = 90), legend.position = "none") 
    plot(g)
  }
  
  out <- list(fit,m)
  class(out) <- c("list","denafit")
  attr(out,"model") <- "frailty"
  attr(out,"eMethod") <- eMethod # estimation method / package
  return(out)
  
}


if(testing){
  data <- data.frame(ID = c(rep(1,180), rep(2,180)),
                     n = 1:360,
                     cat = sample(c("X","Y","Z"), 180*2, replace = T),
                     a = runif(180*2, max = 10),
                     time = Sys.time()+(3*3600*1:180*2)+runif(180*2,max = 4000))
  
  data <- dena::lagVarsNested(data, vars = c("a","time"), nestVars = "ID", lags = 1,
                        diffvars = "time", unit = "secs")
  
  data <- dena::insertAloneTime(data, nestVars = "ID", timeVar = "time", catVar = "cat")
  
  data <- dena::lagVarsNested(data, vars = c("time"), nestVars = "ID", lags = 1,
                              diffvars = "time", unit = "secs")
  
  m <- frailty.model(formula = Surv(timeDiff1,int) ~ a + frailty(ID),
           dat = data)  
  m <- frailty.model(formula = Surv(timeDiff1,int) ~ a + (1 | ID),
                     dat = data) 
  m
}



#####################################################
##### competing multistate model with frailties ######
###################################################

#' @param formula time argument must be in format POSIXct or gap time (numeric)
#' @export
multistate <- function(formula, dat, AloneLag = 1, dateVar = NULL, diagnostics = F, plot = T, verbose = F, print.survcheck = T, ...){
  
  #dat <- simdat2
  #formula <-  Surv(time, type) ~ Covariate1 +Covariate3 + frailty(id)
  #timeformat = "gap"
  #dat <- dat.backup
  #verbose = T
  #print.survcheck = T
  #AloneLag = 1
  #dat.backup <- dat
  #dat <- dat.backup
  #dateVar <- "date"
  #AloneLag = 1
  
  # disentangle formula
  dv <- formula[[2]]
  iv <- formula[[3]]
  timeVar <- as.character(dv[[2]])
  catVar <- as.character(dv[[3]])
  if(length(grep("frailty",as.list(formula[[3]]))) > 0){
  frailtyTerm <- formula[[3]][grep("frailty",as.list(formula[[3]]))]
  nestVar <- as.character(frailtyTerm[[1]][2][[1]])
  frailty <- T
  cluster <- clusterTerm <-NULL
  }else{
    if(length(grep("cluster",as.list(formula[[3]]))) > 0){
      clusterTerm <- formula[[3]][grep("cluster",as.list(formula[[3]]))]
      nestVar <- as.character(clusterTerm[[1]][2][[1]])
      cluster <- T
    }else{frailtyTerm <- nestVar <- frailty <- clusterTerm <- cluster <- NULL}
    }
  
  
  
  if(!(class(dat[,catVar]) %in% "factor")) dat[,catVar] <- factor(dat[,catVar])
  cats <- levels(dat[,catVar])
  cats <- cats[!is.na(cats)]
  
  # check if censored category is there
  censored <- ifelse(length(grep("censor",cats))> 0, grep("censor",cats),"none")
  
  if(verbose) cat("\n States detected:", paste(paste(seq(along=cats), cats, sep='= '), 
                          collapse=", "), '\n')
  
  #some checks
  if(class(dat[,timeVar]) %in% c("POSIXct")) timeformat = "absolute" else{
    timeformat = "gap"
  }
  if(verbose) cat(paste0(timeformat," time format detected"))
  
  
# check if time variable is gap time or absoulte time (date)
  #TODO: 
  
  # data preparation 
  if(timeformat == "gap" & is.null(dateVar)) {
    if(verbose) cat("\n determining Absolute time" )
    now <- Sys.time()
    dat <- getAbsTime(dat,timeVar = timeVar, nestVars = nestVar, origin = now)
    dat$date <- as.POSIXct(dat$date, origin = "1970-01-01")
  }else{dat$date <- dat[,dateVar]}
  
  if(verbose) cat("\n adding 'Alone' time" )
  dat <- insertAloneTime(dat, nestVars = nestVar, timeVar = "date", catVar = catVar, 
                         timeLag = 1, insertNA = NULL)
  dat$id <- dat[,nestVar]
  head(dat[,c(nestVar,timeVar,catVar,"date")])
  if(verbose) cat("\n recalculating time differences and alter Lag" )
  dat[dat[,catVar] == "Alone",timeVar] <- AloneLag
  dat <- lagVarsNested(dat, vars = c("date",catVar),diffvars = "date", nestVars = nestVar, verbose = F)
  #head(dat[,c(nestVar,timeVar,catVar,"date")])
  #dat[,catVar] <- as.numeric(dat[,catVar])
  dat$start <- as.numeric((dat$date - dat$dateDiff1))
  dat$end <- as.numeric(dat$date)
  if(is.null(censored)){
  cats.all <- factor(labels = c("censored",cats,"Alone"),0:(length(cats)+1))
  dat$to <- factor(dat[,catVar],levels = 0:(length(cats)+1), labels = levels(cats.all))
  dat$from <- factor(dat[,paste0(catVar,"Lag1")],levels = 0:(length(cats)+1), labels = levels(cats.all))
  }else{
    cats.all <- c(cats,"Alone")
    labels <- c(cats.all[censored],cats.all[-censored])
    to <- factor(dat[,catVar],levels =  labels)
    dat$to <- factor(as.numeric(dd)-1, levels = 0:(length(cats.all)-1), 
                     labels = labels)
    
    # check
    #data.frame(dd, dat[,catVar], as.numeric(dd)-1, dat$to)
    
    
    lvs <- 0:(length(cats.all)-1)
    dat$from <- factor(as.numeric(dat[,paste0(catVar,"Lag1")])-1,
                       levels = c(lvs[censored],lvs[-censored]) ,labels = labels)
   
    
    #check
    #data.frame(from, dat[,paste0(catVar,"Lag1")], as.numeric(from)-1, dat$from)
  }
  
  
  new.formula <- as.formula(paste0("Surv(start, end, to)","~",
                                   ifelse(is.null(frailty),paste(deparse(iv), collapse = " "),
                                          paste(deparse(iv[[2]]), collapse =" "))))
  head(dat[,c(timeVar,catVar,"date","start","end","to","from")])
  
  if(verbose) cat(paste0("estimating : ", paste(deparse(new.formula), collapse =" "), 
                         ifelse(frailty,paste0(" with id as ",nestVar,""))))
  
 dat <- survDatCheck(dat)
  
#### TODO: implement multistate flow with and without Gap / Overlap / Teleport
 View(dat[dat$FlagGap_withBefore %in% 1,c(timeVar,catVar,"date","start","end","to","from")])
 View(dat[dat$FlagOverlap_withBefore %in% 1,c(timeVar,catVar,"date","start","end","to","from")])
 View(dat[dat$FlagTeleport_withBefore %in% 1,c(timeVar,catVar,"date","start","end","to","from")])
 
  if(print.survcheck) print(survcheck(formula =  new.formula, data = dat[ dat$end > dat$start,],
                                      #control = coxph.control(timefix = FALSE),
                                      id = id, istate = from))
  
  # estimation
  m1 <-coxph(new.formula , data =dat[dat$end > dat$start,],
             id = id, istate = from,control = coxph.control(timefix = FALSE))
  tmp <- as.data.frame(summary(m1)$coefficients)
  
  for(i in 1:nrow(tmp)){
    if(!is.null(nrow(summary(m1)$cmap[-1,]))){ # for one covariate
  tmp[i,"var"] <- names(which(rowSums(as.numeric(rownames(tmp))[i] ==  summary(m1)$cmap[-1,])>0))
  tmp[i,"transition"] <- names(which(colSums(as.numeric(rownames(tmp))[i] ==  summary(m1)$cmap[-1,])>0))
    }else{ #for more than one covariate
      tmp[i,"var"] <- rownames(summary(m1)$cmap)[2]
      tmp[i,"transition"] <- names(which(as.numeric(rownames(tmp))[i] ==  summary(m1)$cmap[-1,]))
    }
  tmp[i,"from"] <- cats.all[as.numeric(strsplit(tmp[i,"transition"],":")[[1]][1])+1]
  tmp[i,"to"] <- cats.all[as.numeric(strsplit(tmp[i,"transition"],":")[[1]][2])+1]
  tmp[i,"cat"] <- cats.all[as.numeric(strsplit(tmp[i,"transition"],":")[[1]][2])+1]
  }
  
  tmp$coef_UB <- tmp$coef + 1.96*tmp$`robust se`
  tmp$coef_LB <- tmp$coef - 1.96*tmp$`robust se`
  out <- list(tmp, m1)
  class(out) <- c("list","denafit")
  attr(out,"model") <- "multistate"
  return(out)
  
}

if(testing){
load("data/simdat2.RData")  

mstate <- multistate(Surv(time, type) ~ Covariate1 + Covariate3 + frailty(id), simdat2, verbose = T)  
mstate.cluster <- multistate(Surv(time, type) ~ Covariate1 + Covariate3 + cluster(id), simdat2, verbose = T)  

plot.cmm(mstate[[1]])

plot.cmm(mstate[[1]][mstate[[1]]$from == "Alone",])
plot.cmm(mstate.cluster[[1]][mstate.cluster[[1]]$from == "Alone",])

# use cmm function for comparison
cmm.model <- cmm(Surv(time, type) ~ Covariate1 + Covariate3 + frailty(id), dat = simdat2, 
                 catVar = "type", plot = T)

mstate[[1]]$model = "mstate"
cmm.model[[1]]$model = "cmm"
tmp <- plyr::rbind.fill(mstate[[1]],cmm.model[[1]])

ggplot(tmp[!is.na(tmp$coef) & !(tmp$to %in% "Alone"),], aes(x = var, y = coef, shape = model,color = factor(cat))) +
  geom_point(position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = coef_LB, ymax = coef_UB),
                width = 0.2,
                position=position_dodge(width=0.5)) +
  coord_flip() +
  ylab("Coefficient") +
  xlab("") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 90))

}

#' @export
cmm <- function(formula, dat, diagnostics = F, plot = T, verbose = F, ...){
    
    

    verbose = T
    fits <- data.frame()
    fit.list <- list()
    
    # formula needs to be in the format
    # formula = Surv(timeDiff1, cat) ~ a + frailty(ID)
    dv <- as.list(formula)[[2]]
    # some checks on the DV
    timeVar <- dv[[2]]
    catVar <- as.character(dv[[3]])
    
    if(!(class(dat[,catVar]) %in% "factor")) dat[,catVar] <- factor(dat[,catVar])
    cats <- levels(dat[,catVar])
    cats <- cats[!is.na(cats)]
    if(verbose) cat("\n States detected:", paste(paste(seq(along=cats), cats, sep='= '), 
                                                 collapse=", "), '\n')
    iv <- as.list(formula)[[3]]
    # some checks on the IV
    
    for(category in cats){
      # add category to left hand side of formula
      left.side <- paste0('Surv(',formula[[2]][[2]],',',
                          catVar, '== "',category,'") ~')
      right.side <- gsub(","," ",toString(deparse(formula[[3]])))
      formula.cat <- as.formula(paste0(left.side,right.side))
      if(verbose) cat(paste0("estimating: ", deparse(formula.cat), "\n "))
      
      
      # estimate 
      fit <- coxph(formula = formula.cat, data = dat)
      
      # test for formula parsing with test data
     # all(coxph(Surv(timeDiff1, cat == "X") ~ a + frailty(ID), data = dat)$coefficients ==
     #    fit$coefficients)
      
      
     # export
      attributes(fit)$cat <- category
      fit.list[[length(fit.list)+1]] <- fit
      #View(getS3method("summary", "coxph"))
      fits <- rbind(fits, data.frame(var = rownames(summary(fit)$coefficients),#names(fit$coefficients),
                                     cat = category, summary(fit)$coefficients))

    }
    
    
    
    # old way of estimating in the long format
    # for(cat in unique(dat[,catVar])){ # this is how competing risks is tough in the mstate vignette (Putter, 2019)
    #   if(is.na(cat)) next
    #   if(verbose) cat(paste0("\r Estimating cmm for category: ",cat,"\n"))
    #   # estimate
    #   if(datshape == "long"){
    #     # check if it is a frailty model from survival package, otherwise use coxme
    #     if(length(grep("frailty",sapply(formula[[3]], unlist))) > 0){
    #       f1 <- survival::coxph(formula, data = dat[dat[,catVar] == cat,])
    #       tmp <- as.data.frame(summary(f1)$coefficients)
    #     }else{
    #       f1 <- coxme::coxme(formula, data = dat[dat[,catVar] == cat,])
    #       tmp <- as.data.frame(coxmeTable(f1)[[3]])
    #     }
    #   }if(datshape == "short"){
    #     
    #   }
    #   #print(summary(f1))
    #   
    #   # check diagnostics
    #   if(diagnostics){
    #     # proportional hazards assumption
    #     ggcoxzph(cox.zph(f1)) # if  p < 0.05, violation of proportional hazards assumption
    #     # also see: https://stats.stackexchange.com/questions/422539/schoenfeld-test-cox-zph-shows-no-covariate-violates-ph-assumption-but-global-t
    #     
    #     # outliers
    #     ggcoxdiagnostics(f1, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())
    #     ggcoxdiagnostics(f1, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())
    #     
    #     # linearty assumption
    #     ggcoxfunctional(f1) #should be on a straight line
    #     
    #     # frailty distribution
    #     hist(f1$frail)
    #   }
    #   
    #   # export
    #   fit.list[[length(fit.list)+1]] <- list(cat,f1)
    #   
    #   tmp$cat <- cat
    #   tmp$var <- rownames(tmp)
    #   fits <- rbind(fits, tmp)
    # }
    
    # add confidence intervals
    fits$coef_UB <- fits$coef + 1.96*fits$se2 
    fits$coef_LB <- fits$coef - 1.96*fits$se2
    rownames(fits) <- 1:nrow(fits)
    #fits$sig <- starIt(fits$`Pr(>|z|)`)
    
   if(plot){
     g <- ggplot(fits[!is.na(fits$coef),], aes(x = var, y = coef, color = factor(cat))) +
        geom_point(position = position_dodge(width=0.5)) +
        geom_errorbar(aes(ymin = coef_LB, ymax = coef_UB),
                      width = 0.2,
                      position=position_dodge(width=0.5)) +
        coord_flip() +
        ylab("Coefficient") +
        xlab("") +
        scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(fits$cat)), name = "Dark2")) +
        theme_minimal() +
        geom_hline(yintercept = 0, linetype = "dotted") +
        theme(axis.text.x = element_text(angle = 90))
     plot(g)
   }
    # define output object and its characteristics 
    out <- list(fits, fit.list)
    class(out) <- c("list","denafit")
    attr(out,"model") <- "cmm"
    
    return(out)
}

# test
if(testing){
load("data/simdat2.RData")  
fit <- cmm(Surv(time, type) ~ Covariate2 + Covariate3 + frailty(id), dat = simdat2, 
                   catVar = "type", plot = F)

summary(fit)
plot(fit)  
plot(fit, type = "frailcor")
print(fit, type ="friend")
  
###
data <- data.frame(ID = c(rep(1,180), rep(2,180)),
                   n = 1:360,
                   cat = sample(c("X","Y","Z"), 180*2, replace = T),
                   a = runif(180*2, max = 10),
                   time = Sys.time()+(3*3600*1:180*2)+runif(180*2,max = 4000))

data <- lagVarsNested(data, vars = c("a","time"), nestVars = "ID", lags = 1,
                      diffvars = "time", unit = "secs")

#data <- dena::insertAloneTime(data,  nestVars = "ID", timeVar = "time", catVar = "cat") # does not make sense

data$timeDiff1 <- round(data$timeDiff1)
m <- cmm(Surv(timeDiff1,cat) ~ a + frailty(ID),
    dat = data, plot = T)
m

## test if competing rates from Putter et al., 2007 (Tutuorial paper, p. 2404) is identical to my method
library(survival)
set.seed(56)
dat <- data.frame(ID = 1,
                  day = 1:20,
                  time = Sys.Date()+1:20,
                  timeDiff = c(NA,1:19),
                  b = Sys.Date()+1:20,
                  alter = factor(sample(c("X","Y","Z"), 20, replace = T)),
                  a = runif(20, max = 10))


## competing rates for alter categories X, Y Z
# for(i in c("X","Y","Z"))  {
#   print(paste0("Results for alter category: ",i))
#   f <- coxph(Surv(timeDiff, alter == i) ~ a, data = dat)
#   print(f)
# }


dat <- lagVarsNested(dat, lags = 1, nestVars = c("ID"),
                     vars = c("a","b"), diffvars = "b", unit = "days")
dat.l <- toLong(dat, catVar = "alter")

coxph(Surv(bDiff1, alter == "X") ~a+aLag1, data = dat)$coefficients ==   # Putter et al 2007 method from short data format
  coxph(Surv(bDiff1, event) ~a+aLag1, data = dat.l[dat.l$cat == "X",])$coefficients # long data format method with subsetting
  
coxph(Surv(bDiff1, alter == "X") ~a+aLag1, data = dat)$coefficients == 
  cmm(Surv(bDiff1, alter) ~a+aLag1, dat = dat, verbose = F, plot = F)[[1]][1:2,3]

#second varification with thoureneau data
# use example data from the survival package and the example code from the package vignette 
# see https://cran.r-project.org/web/packages/survival/vignettes/compete.pdf
library(survival)
library(dena)
etime <- with(mgus2, ifelse(pstat==0, futime, ptime))
event <- with(mgus2, ifelse(pstat==0, 2*death, 1))
event <- factor(event, 0:2, labels=c("censor", "pcm", "death"))
mgus2$event <-event
mgus2$etime <- etime
rm(event)
cfit1 <- coxph(Surv(etime, event) ~ age + sex, mgus2, id = id) # thourneau/survival method
cfit1.pcm <- coxph(Surv(etime, event == "pcm") ~ age + sex, mgus2) # putter method
cfit1.death <- coxph(Surv(etime, event == "death") ~ age + sex, mgus2)

cfit1$coefficients
cfit1.pcm$coefficients 
cfit1.death$coefficients 


summary(cfit1)
print(cfit1)
mgus2$event.type <- as.numeric(mgus2$event)
mgus2$int <- 1
#cfit1 <- coxme::coxme(Surv(etime, event) ~ age + sex + mspike + ( 1 | id), data = mgus2)
cmm.model2 <- cmm(Surv(etime, event) ~ age + sex +  frailty(id),
                  dat = mgus2, catVar = "cat")

#psych::pairs.panels(mgus2[,c("age","sex","mspike","ptime")])
meta <- data.frame(coxph = cfit1$coefficients, cmm = cmm.model2[[1]]$coef[4:9])
meta$rel <- (meta$coxph-meta$cmm)/meta$coxph
plot(meta$rel)
# 
# # create fake data to test
# mgus2$testtime <- rnorm(nrow(mgus2), sd = 0.3) + (mgus2$event.type == 2)* (mgus2$sex == "F")
# cmm.model.test <- cmm(Surv(testtime, event) ~  sex + (1 | id), 
#                       dat = toLong(mgus2, "event.type") , catVar = "cat")
# 
# mgus2$event.f <- factor(mgus2$event, 1:3, labels=c( "censor", "pcm", "death"))
# cfit.test <- coxph(Surv(testtime, event) ~ sex , mgus2, id=id)
# cfit.test

# new data
library(survival)
library(coxme)
set.seed(55)
n = 500
rdat <- data.frame(id = rep(1:5,n/5), alter = factor(sample(c("friend","partner"), n, replace = T)))
rdat <- rdat[order(rdat$id),]
rdat$X <- sample(0:1, n, replace = T)
rdat$int <- 1
#rdat[,"int"] <- 0 # add right-censored data
rdat$time <- round(10*(rdat$X*(rdat$alter == "partner") + rnorm(nrow(rdat), mean = 3, sd = 0.3)))
rdat$alter.n <- as.numeric(rdat$alter)
rdat.l <- toLong(rdat, "alter")

target <- cmm(Surv(time, int) ~ X + frailty(id), dat = rdat ,catVar =  "alter")[[1]][c(1,3),1] #

############ All methods comparison ############
# competing risk from 
#https://cran.r-project.org/web/packages/survival/vignettes/compete.pdf
coxph(Surv(time, int) ~X + frailty(id), data = rdat, id = id)

# coxph one model
coxph(Surv(time, alter) ~X, data = rdat, id = id)

meta <- data.frame(type = rep(c("coxph+frailty+short",""),2))
# coxph separate models 
#short with subset in Surv, Putter et al., 2007 (Tutuorial paper, p. 2404) 
coxph(Surv(time, alter == "friend") ~X + frailty(id), data = rdat, id = id)$coefficient[1] - target[2]
coxph(Surv(time, alter == "partner") ~X + frailty(id), data = rdat, id = id)$coefficient[1] - target[1]

#short
coxph(Surv(time, int) ~X + frailty(id), data = rdat[rdat$alter %in% "friend",], id = id)$coefficient[1] -target[2]
coxph(Surv(time, int) ~X + frailty(id), data = rdat[rdat$alter %in% "partner",], id = id)$coefficient[1] - target[1]

# long
coxph(Surv(time, int) ~X + frailty(id), data = rdat.l[rdat.l$alter %in% "friend",], id = id)$coefficient[1] -  target[2]
coxph(Surv(time, int) ~X + frailty(id), data = rdat.l[rdat.l$alter %in% "partner",], id = id)$coefficient[1] - target[1]

# coxme model ###
coxme(Surv(time, alter) ~X + (1|id), data = rdat) # Cox model doesn't support 'mright' survival data

# short data format
coxme(Surv(time, int) ~X + (1|id), data = rdat[rdat$alter %in% "friend",])$coefficients  - target[2]
coxme(Surv(time, int) ~X + (1|id), data = rdat[rdat$alter %in% "partner",])$coefficients - target[1]

# long data format
coxme(Surv(time, int) ~X + (1|id), data = rdat.l[rdat.l$cat %in% "friend",])$coefficients  - target[2]
coxme(Surv(time, int) ~X + (1|id), data = rdat.l[rdat.l$cat %in% "partner",])$coefficients  - target[1]

# EM frailty
frailtyEM::emfrail(Surv(time, alter == "friend") ~X + cluster(id), data = rdat)$coefficients  - target[2]
frailtyEM::emfrail(Surv(time, alter == "partner") ~X + cluster(id), data = rdat)$coefficients  - target[1]

## subdistributional hazard model (Fine + Gary, 1999)
library(crr)
crr.1 <- crr(ftime = rdat$time,fstatus = rdat$int,cov1 = rdat$X,failcode=1,cencode=0) # what about frailties?

# cmm method

cmm(Surv(time, int) ~ X + frailty(id), dat = rdat ,catVar =  "alter")[[1]] # survival estimation
cmm(Surv(time, int) ~ X + (1 | id), dat = rdat ,catVar =  "alter")[[1]] # coxme estimation

cmm(Surv(time, int) ~ X + frailty(id), dat = rdat.l ,catVar =  "cat")[[1]] # survival estimation
cmm(Surv(time, int) ~ X + (1 | id), dat = rdat.l ,catVar =  "cat")[[1]] # coxme estimation

cmm(Surv(time, int) ~ X + frailty(id), dat = rdat.l ,catVar =  "alter")[[1]] # survival estimation
cmm(Surv(time, int) ~ X + (1 | id), dat = rdat.l ,catVar =  "alter")[[1]] # coxme estimation

# frailty HL estimation,    3) Cause-specific frailty models (Univariate)
#### Uses the il do ha (2018) Competing risk frailty models book chapter. R code p. 156
library(frailtyHL)
beta.init <-c(sapply(1:2, function(k) coxph(Surv(time,alter.n==k) ~ X,data=rdat)$coef))
theta.init = 0.05
n.id  = length(unique(rdat$id)) 
v.init=rep(0,n.id) #v.init = rnorm(q,0,1)
CSFM <- hlike.frailty(formula=CmpRsk(time,alter.n) ~ X + cluster(id), data=rdat,frailty.cov="none",
                      inits=list(beta=beta.init,theta=theta.init,v=v.init),order=1, MAX.ITER=500, TOL=1E-5)
summary(CSFM)
CSFM$beta

# Rueten 2018 method


}


#' @export
cmm.long <- function(formula, dat.l, eventVar = "event", datshape = "long",  diagnostics = F, plot = T, verbose = T, ...){

  #dat.backup <- dat.l
  verbose = T
  eventVar = "event"
  datshape = "long"
  
  fits <- data.frame()
  fit.list <- list()
  
  
  #checks
  if(class(dat.l[,eventVar]) != "numeric") stop("event variable needs to be numeric")
  
  # formula needs to be in the format
  # formula = Surv(timeDiff1, cat) ~ a + frailty(ID)
  dv <- as.list(formula)[[2]]
  # some checks on the DV
  timeVar <- dv[[2]]
  catVar <- as.character(dv[[3]])
  
  if(!(class(dat.l[,catVar]) %in% "factor")) dat.l[,catVar] <- factor(dat.l[,catVar])
  cats <- levels(dat.l[,catVar])
  cats <- cats[!is.na(cats)]
  if(verbose) cat("\n States detected:", paste(paste(seq(along=cats), cats, sep='= '), 
                                               collapse=", "), '\n')
  iv <- as.list(formula)[[3]]
  
  # exclude alter category in formula but take event
  left.side <- paste0('Surv(',dv[[2]],',event) ~')
  right.side <- gsub(","," ",toString(deparse(formula[[3]])))
  formula.new<- as.formula(paste0(left.side,right.side))
  if(verbose) cat(paste0("estimating: ", deparse(formula.new), " with catVar ", catVar," \n "))
  #dat.l$event <- ifelse(is.null(eventVar), 1, dat.l[,eventVar])
  
  #old way of estimating in the long format
  for(cat in unique(dat.l[,catVar])){ # this is how competing risks is tough in the mstate vignette (Putter, 2019)
    if(is.na(cat)) next
    if(verbose) cat(paste0("\r Estimating cmm for category: ",cat,"\n"))
    # estimate
    
    if(datshape == "long"){
      # check if it is a frailty model from survival package, otherwise use coxme
      
      if(length(grep("frailty",sapply(formula[[3]], unlist))) > 0){
        f1 <- survival::coxph(formula.new, data = dat.l[dat.l[,"cat"] == cat,])
        tmp <- as.data.frame(summary(f1)$coefficients)
      }else{
        f1 <- coxme::coxme(formula.new, data = dat.l[dat.l[,"cat"] == cat,])
        tmp <- as.data.frame(coxmeTable(f1)[[3]])
      }
    }
    if(datshape == "short"){

    }
    #print(summary(f1))

    # check diagnostics
    # if(diagnostics){
    #   # proportional hazards assumption
    #   ggcoxzph(cox.zph(f1)) # if  p < 0.05, violation of proportional hazards assumption
    #   # also see: https://stats.stackexchange.com/questions/422539/schoenfeld-test-cox-zph-shows-no-covariate-violates-ph-assumption-but-global-t
    # 
    #   # outliers
    #   ggcoxdiagnostics(f1, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())
    #   ggcoxdiagnostics(f1, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())
    # 
    #   # linearty assumption
    #   ggcoxfunctional(f1) #should be on a straight line
    # 
    #   # frailty distribution
    #   hist(f1$frail)
    # }

    # export
    attr(f1, "cat") <-cat
    fit.list[[length(fit.list)+1]] <- list(f1)

    tmp$cat <- cat
    tmp$var <- rownames(tmp)
    fits <- rbind(fits, tmp)
  }
  
  # add confidence intervals
  fits$coef_UB <- fits$coef + 1.96*fits$`se(coef)` 
  fits$coef_LB <- fits$coef - 1.96*fits$`se(coef)`
  rownames(fits) <- 1:nrow(fits)
  #fits$sig <- starIt(fits$`Pr(>|z|)`)
  
  if(plot){
    g <- ggplot(fits[!is.na(fits$coef),], aes(x = var, y = coef, color = factor(cat))) +
      geom_point(position = position_dodge(width=0.5)) +
      geom_errorbar(aes(ymin = coef_LB, ymax = coef_UB),
                    width = 0.2,
                    position=position_dodge(width=0.5)) +
      coord_flip() +
      ylab("Coefficient") +
      xlab("") +
      scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(fits$cat)), name = "Dark2")) +
      theme_minimal() +
      geom_hline(yintercept = 0, linetype = "dotted") +
      theme(axis.text.x = element_text(angle = 90))
    plot(g)
  }
  # define output object and its characteristics 
  out <- list(fits, fit.list)
  class(out) <- c("list","denafit")
  attr(out,"model") <- "cmm"
  attr(out,"datshape") <- datshape
  
  return(out)
}

## ## multistate frailty model
# require(frailtyEM)
# m <- emfrail(Surv(timeDiff1,event) ~ a + cluster(ID), data = dat[dat$cat == "Z",])
# summary(m)
# autoplot(m, type = "frail")
# 
# 
# ## multinominal choice model
# require(mlogit)
# m <- mlogit(cat ~ a | timeDiff1, data = dfidx(dat[dat$ID == 1,], idx = c("n", "cat")))
# 
# ## multilevel multinomial choice model
# require(MCMCglmm)
# m <- MCMCglmm(data = data, 
#               fixed = cat ~ -1 + trait, random = ~ us(trait):ID, 
#               family = "categorical")
# 
# 
# #
# dat <- data.frame(id = rep(1,60),
#                   receiver = c("friend","family","stranger"),
#                   time.since.last.event = c(1,1,1,4,4,4,5,5,5,3,3,3,2,2,2),
#                   choice = rbinom(60,1,0.5),  # ah, if it is multiple alters, this can be multiple 1s 
#                   weekday = rbinom(60,1,0.1))
# View(dat)
# 
# estmiate(formula, dat)

### verify model
##  1. estimate model
##  2. simulate data
##  3. reestimate model

# TODO:
# 2. independence of irrelevant alternatives
# 3. model multistate model with competing rates (data format as )
#dat # 
# 4. use goldfish for the preprocessing, or develop my own
# 5. the cox proportional hazard model is basically a mlogit model, that takes data like
#dat # with dv = Surv(dat$time, dat$choice)

