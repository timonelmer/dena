# survival multistate repeated events model example #
library(survival)
state5 <- c("0MC", "1MC", "2MC", "3MC", "death") 
tmat <- matrix(0L, 5, 5, dimnames=list(state5, state5)) 
tmat[1,2] <- tmat[2,3] <- tmat[3,4] <- 1 
tmat[-5,5] <- 1 
statefig(rbind(4,1), tmat)

ndata <- tmerge(nafld1[,1:8], nafld1, id=id, death= event(futime, status))
ndata <- tmerge(ndata, subset(nafld3, event=="nafld"), id, nafld= tdc(days))
ndata <- tmerge(ndata, subset(nafld3, event=="diabetes"), id = id, diabetes = tdc(days), e1= cumevent(days))
ndata <- tmerge(ndata, subset(nafld3, event=="htn"), id = id, htn = tdc(days), e2 = cumevent(days))
ndata <- tmerge(ndata, subset(nafld3, event=="dyslipidemia"), id=id, lipid = tdc(days), e3= cumevent(days))
ndata <- tmerge(ndata, subset(nafld3, event %in% c("diabetes", "htn", "dyslipidemia")),
                  id=id, comorbid= cumevent(days))
attr(ndata, "tcount")

ndata$cstate <- with(ndata, factor(diabetes + htn + lipid, 0:3, c("0mc", "1mc", "2mc", "3mc")))
temp <- with(ndata, ifelse(death, 4, comorbid)) 
ndata$event <- factor(temp, 0:4, c("censored", "1mc", "2mc", "3mc", "death"))
ndata$age1 <- ndata$age + ndata$tstart/365.25 # analysis on age scale 
ndata$age2 <- ndata$age + ndata$tstop/365.25 
survcheck(Surv(age1, age2, event) ~ nafld + male, data=ndata, id=id, istate=cstate)

nfit1 <- coxph(list(Surv(age1, age2, event) ~ nafld + male , 
                    "0mc":state("1mc", "2mc", "3mc") ~ nafld+ male / common,
                    2:3 + 2:4 ~ nafld + male / common, 
                    0:"death" ~ male ),
               data=ndata, id=id, istate=cstate)
print(coef(nfit1, matrix=TRUE), digits=3)
nfit1# 

# own random data 
set.seed(56)
library(dena)
dat.t <- data.frame(ID = c(rep(1,10), rep(2,10)),
                  day = rep(c(rep(1,5), rep(2,5)),2),
                  time = c(Sys.Date()+1:10,Sys.Date()+101:110),
                  timeDiff = 1:20,#runif(20)+rep(c(NA,1:9),2),
                  alter = factor(sample(c("X","Y","Z"), 20, replace = T)),
                  a = runif(20, max = 10))

dat.t <- lagVarsNested(dat.t, c("alter","a"),"ID")
dat.t[dat.t$alterLag1 %in% NA,"alterLag1"] <-  4
dat.t$alter <- as.numeric(dat.t$alter)
dat.t[c(10,20),"alter"] <-  5
dat.t$alterLag1 <- factor(dat.t$alterLag1, levels = 1:5, labels = c("X","Y","Z","lc","rc"))
dat.t$alter <- factor(dat.t$alter, levels = 1:5, labels = c("X","Y","Z","lc","rc"))
dat.t$start <- as.numeric(dat.t$time-Sys.Date())
dat.t$end <- as.numeric(dat.t$time-Sys.Date()+1)
dat.t$state <- factor(dat.t$alterLag1)
dat.t$to <- factor(dat.t$alter)


coxph(Surv(timeDiff, alter) ~ a, data = dat.t, id = ID)

survcheck(Surv(start, end, to) ~ 1, data=dat.t, id=ID, istate=state)
coxph(list(Surv(start, end, to) ~ a ), data=dat.t, id=ID, istate= state)

# isahib 
load("../../Doktorat/Datasets/iSAHIB/iSAHIB_2020-12-11.RData")
source("R/preprocessing.R")
states <- c("alone",alters)
tmat2 <- matrix(0L, 8, 8, dimnames=list(states, states)) 
tmat2[1,-1] <- tmat2[-1,1] <- 1
statefig(rbind(1,7),tmat2)



# get data in start end format with alone time in between
dat <- int[int$ID %in% ids[] & int$burst == 1,]
dat <- insertLeftCensor(dat, nestVars = "ID", timeVar = "date", catVar = "alter")
dat <- insertAloneTime(dat, nestVars = "ID")
dat <- lagVarsNested(dat, vars = c("date","alter"), diffvars = "date",nestVars = c("ID","dayID"))
for(id in unique(dat$ID)){
  dat[dat$ID %in% id,"obs.start"] <- dat[dat$ID %in% id,"date"][1]
}

dat$start <- as.numeric(dat$date-dat$obs.start-dat$dateDiff1)
dat$end <- as.numeric(dat$date-dat$obs.start)
dat$state <- factor(dat$alterLag1, levels = c("(left censored)", "Alone",alters))
dat$to <- factor(dat$alter, levels = c("(left censored)", "Alone",alters))

survcheck(Surv(start, end, to) ~ affect_v, data = dat[,], id = ID, istate = state)

# multistate method from the survival package (package manual, Therneau, 2020, p 61)
m1 <-coxph(Surv(start, end, to) ~ weekday , data = dat, id = ID, istate = state)
m1
meta <- summary(m1)$coefficients
meta <- cbind(rbind(expand.grid(alters,"Alone"),expand.grid("Alone",alters)), meta )
meta$model <- "proper_multistate"

# compare to the subsetting method
m2 <- list()
for(i in c(alters))  {
  print(paste0("Results for alter category: ",i))
  m2[[length(m2)+1]] <- coxph(Surv(dateDiff1, alter == i) ~ weekday , data = dat[!(dat$alter == "Alone"),]) 
  tmp <- as.data.frame(summary(m2[[length(m2)]])$coefficients)
  tmp$model <- "subsetting"
  tmp$Var2 <- i
  meta <- plyr::rbind.fill(meta, tmp[1,])
}

# cmm function from dena
m3 <- cmm(Surv(dateDiff1, alter) ~ weekday +frailty(ID), dat = dat[!(dat$alter == "Alone"),])
m3[[1]]$model <- "cmm-function/wFrail"
m3[[1]]$Var2 <- m3[[1]]$category
m3[[1]]$'se(coef)' <- m3[[1]]$se.coef.
meta <- plyr::rbind.fill(meta, m3[[1]])

# EM frail from Frailty EM
m3 <- list()
for(i in c(alters))  {
  print(paste0("Results for alter category: ",i))
  m3[[length(m3)+1]] <- frailtyEM::emfrail(formula = Surv(dateDiff1, alter == i) ~ weekday +cluster(ID),
                                           data = dat[!(dat$alter == "Alone"),])
  tmp <- as.data.frame(summary(m3[[length(m3)]])$coefmat)
  tmp$model <- "frailtyEM_sharedFrailty"
  tmp$Var2 <- i
  meta <- plyr::rbind.fill(meta, tmp[1,])
}


# almost identical results!!!
meta$coefLB <- meta$coef-1.96*meta$'se(coef)'
meta$coefUB <- meta$coef+1.96*meta$'se(coef)'
ggplot(meta[!(meta$Var2 == "Alone"),], aes(x = Var2, y = coef, color = model)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = coefLB, ymax = coefUB),
                width = 0.2,
                #linetype = "dotted",
                position=position_dodge(width=0.5))



