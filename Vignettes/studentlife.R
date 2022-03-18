########################################
############ Analysis script ###########
## Loneliness and Social Interactions ##

# load some libraries
library(ggplot2)
library(dplyr)
library(survival)
library(scales)
library(psych)

rm(list = ls()) #clear the environment

# load the preprocessed dataset
# load("tmp_dat.Rdata")
# `%!in%` <- Negate(`%in%`)
# dat <- dat[which(1:nrow(dat) %!in% grep("NA",rownames(dat))),]
# save(dat,raw.dat, file = "public_datset.RData")
data("DartmouthStudentLife")
# the data is from 
# Wang, R., Chen, F., Chen, Z., Li, T., Harari, G., Tignor, S., Zhou, X., 
#   Ben-Zeev, D., & Campbell, A. T. (2014). 
#   Studentlife: Assessing mental health, academic performance and behavioral
#   trends of college students using smartphones. UbiComp 2014 - 
#   Proceedings of the 2014 ACM International Joint Conference on
#   Pervasive and Ubiquitous Computing, 3–14. https://doi.org/10.1145/2632048.2632054
# 
# #
head(dat)

## some descriptives ##

# number of interactions per participant and day
tmp <- dat %>% group_by(uid,state) %>% summarise(sum = length(uid))
mean(as.numeric(tmp[tmp$state == "interaction","sum"][[1]]), na.rm = T)
sd(as.numeric(tmp[tmp$state == "interaction","sum"][[1]]), na.rm = T)

tmp <- dat %>% group_by(uid,state, date) %>% summarise(sum = length(uid))
mean(as.numeric(tmp[tmp$state == "interaction","sum"][[1]]), na.rm = T)
sd(as.numeric(tmp[tmp$state == "interaction","sum"][[1]]), na.rm = T)

# mean and SD of interaction / alone duration
mean(dat[dat$state == "interaction", "duration.state"], na.rm = T)
sd(dat[dat$state == "interaction", "duration.state"], na.rm = T)

mean(dat[dat$state == "alone", "duration.state"], na.rm = T)
sd(dat[dat$state == "alone", "duration.state"], na.rm = T)


# time of the day plot
ggplot(dat, aes(x=as.POSIXct(format(as.POSIXct(start), "%H:%M:%S"),format="%H:%M"))) + 
  geom_histogram( fill="lightblue", binwidth = 15*60,  # 15 min *60 sec/min
                  color="grey50")


##### RQ1 Multistate Models ####


### checks for multistate model
check <- survcheck(Surv(start.time, end.time, to) ~ 1, 
                   data = dat, id = uid, istate = state)
checks <- !(1:nrow(dat) %in% check$overlap$row) & !(1:nrow(dat) %in% check$gap$row) &!(1:nrow(dat) %in% check$teleport$row) 

# multistate method from the survival package (package manual, Therneau, 2020, p 61)
m1 <-coxph(Surv(start.time, end.time, to) ~ 
             I(duration_mean_window7200/60) +
             weekend+
             morning + afternoon + evening +
             start_length_window86400 +  
             start_length_window7200, 
           data = dat[checks,], 
           id = uid, istate = state)

m1
