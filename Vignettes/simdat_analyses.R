###### analysis of simulated data #####
###### \TE February 2021 #####


# load the dena package with the data
rm(list = ls())
library(dena)
library(ggplot2)
library(survival)
data(simdat) # the  Rcode to simulate this dataset can be found at the bottom
# of this script

#### Basic concepts of Survival analysis ####

### survival Function (with plot) ##
survivalFunction(dat = simdat,timeVar =  "time", eventVar = "event")


#### Cox Proportional Hazard Model #####
## fit a coxph model ##
fit <-coxph(formula = Surv(time, event) ~ X, data = simdat)
summary(fit)
coxR2(fit) # computes the pseudo r-squared (based on the cox-snell method)

## Schoenfeld test for proportional hazard assumtion ##
# should be non-significant for proportional hazard assumption to hold
cox.zph(fit) 
survminer::ggcoxzph(cox.zph(fit)) # a plot of the residuals

#### Frailty models ####
data(simdat2) # the  Rcode to simulate this dataset can be found at the bottom
# of this script

# with the dena package
m.dena <- frailty.model(Surv(time, event) ~ Covariate1 + Covariate2 + (1|id), 
                        plot = F, dat = simdat2) # reyling on the coxme kacage
# m.dena <- frailty.model(Surv(time, event) ~ Covariate1 + Covariate2 + frailty(id), 
#                         plot = F, dat = simdat2) # relying on the surivial package
summary(m.dena)
plot(m.dena)
plot(m.dena, type ="frailty")
# TODO: coxR2(m.dena) # computes the pseudo r-squared (based on the cox-snell method)

# check robustness of findings of a few individual only have a few observations
# i.e., can the multilevel structure of the data "compensate" for individuals with small N?
simdat2.reduced <- simdat2
simdat2.reduced[#simdat2.reduced$id %in% 1:3 &
                  sample(c(F,T,T,T), 60, replace =T),# set 3/4 of the data to NA
                c("Covariate1","Covariate2")] <- NA
m.dena2 <- frailty.model(Surv(time, event) ~ Covariate1 + Covariate2 + frailty(id), 
                        plot = F, dat = simdat2.reduced)


# with the survival package
m.coxph <- coxph(Surv(time, event) ~ Covariate1 + Covariate2 + frailty(id), simdat2)

# with coxme
m.coxme <- coxme::coxme(Surv(time, event) ~ Covariate1 + Covariate2 + (1 | id), simdat2)

# with the frailtyEM
m.frailtyEM <- frailtyEM::emfrail(Surv(time, event) ~ Covariate1 + Covariate2 + cluster(id), simdat2)

# compare estiamtes
tmp <- cbind(m.dena[[1]]$coef[1:2], m.coxme$coefficients[1:2],m.coxph$coefficients[1:2], m.frailtyEM$coefficients)
colnames(tmp) <- c("dena","coxme","coxph","fraityEM")
tmp

### Multistate model ####
m.cmm <- cmm(Surv(time, type) ~ Covariate2 + Covariate3 + frailty(id), dat = simdat2)
plot.cmm(m.cmm[[1]])

m <- multistate(Surv(time, type) ~ Covariate2 + Covariate3 + cluster(id), simdat2, verbose = T)  
plot.cmm(m[[1]])


### Simulation of the simdata dataset ###

set.seed(44)
hazard.fun <- function(x){1-log(x)}
dat.sim <- coxed::sim.survdata(N=200, T=500, num.data.frames=1, xvars = 1, 
                        mu = 0.5,
                        sd = 0.1,
                        censor = 0,
                        #hazard.fun = hazard.fun, 
                        beta=c(2))

dat.sim$data$X <- round(dat.sim$data$X)
simdat <- dat.sim$data[,c(2,1,3)]
colnames(simdat) <- c("time","X","event")
simdat <- simdat[sample(1:nrow(simdat),nrow(simdat)),]
simdat$ID <- 1:nrow(simdat)
simdat <- simdat[,c(4,1,3,2)]
rownames(simdat) <- simdat$ID
head(simdat)
simdat$event <- simdat$event*1
#save(simdat, file = "data/simdat.RData")

### Simulation of the simdata2 dataset ###
set.seed(133)
simdat2 <- frailtySurv::genfrail(N = 30, K = 20, 
                                 beta = c(log(2), -log(1)),
                                 frailty = "gamma", theta = 2, 
                                 censor.rate = 0,
                                 lambda_0=function(t, tau=4.6, C=0.01) (tau*(C*t)^tau)/t)

colnames(simdat2) <- c("id","rep","time","event","Covariate1","Covariate2")
simdat2$type <- factor(sample(c("partner","friend","family"), nrow(simdat2), replace = T))

# create fake covariate 3 that is associated with eventtype and time 
simdat2$Covariate3 <- ifelse(simdat2$type == "partner" & simdat2$time < 200,1,0) + 
  rnorm(nrow(simdat2),mean = 0.2, sd = 0.2)
#save(simdat2, file = "data/simdat2.RData")
