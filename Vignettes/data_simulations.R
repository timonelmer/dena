#### Simulate dataset 1 ####
library(survival)
library(coxed)
library(frailtySurv)

set.seed(44)
hazard.fun <- function(x){1-log(x)}
dat.sim <- sim.survdata(N=200, T=500, num.data.frames=1, xvars = 1, 
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
#save(simdat, file = "../data/simdat.RData")

# plot the survival probability
survivalFunction(dat.sim$data, "y","failed", plotType = "Survival")

# estimate a cox proportional hazard model
fit <-coxph(Surv(time, event) ~ X, data = simdat)
summary(fit)


#### Simulate dataset 2 ####
set.seed(133)
simdat2 <- genfrail(N = 30, K = 20, 
                                 beta = c(log(2), -log(1)),
                                 frailty = "gamma", theta = 2, 
                                 censor.rate = 0,
                                 lambda_0=function(t, tau=4.6, C=0.01) (tau*(C*t)^tau)/t)

colnames(simdat2) <- c("id","rep","time","event","Covariate1","Covariate2")
simdat2$type <- factor(sample(c("partner","friend","family"), nrow(simdat2), replace = T))

# create fake covariate 3 that is associated with eventtype and time 
simdat2$Covariate3 <- ifelse(simdat2$type == "partner" & simdat2$time < 200,1,0) + 
  rnorm(nrow(simdat2),mean = 0.2, sd = 0.2)
#save(simdat2, file = "../data/simdat2.RData")

#simple coxph model, without taking the frailty into account
m.coxph <- coxph(Surv(time, event) ~ Covariate1 + Covariate2, dat = simdat2)
summary(m.coxph)

# frailty model estimated with coxme
m.coxme <- coxme::coxme(Surv(time, event) ~ Covariate1 + Covariate2 + (1 | id), dat = simdat2)
summary(m.coxme)
