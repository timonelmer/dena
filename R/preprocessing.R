
testing = F
## 1. add relevant variables

# lag variables 
#separate function for lag

#' Lagging variables
#'
#' Creates extra column(s) with lagged variables according to the order of the data frame. 
#'
#' @param dat data.frame containing the variables to be lagged
#' @param lags vector defining the \code{n}th lag. Positive values (e.g., 1:2) indicatw 
#' the values from the previous rows (e.g., the values from the previous (lag = 1) observation) 
#' should be taken. 
#' Negative values indicate that the values from the following rows should be taken. 
#' @param vars names of the columns in the data.frame that should be lagged
#' @param diffvars names of columns in the data.frame of which the difference to the current row should be taken
#' (this is particularly helpful for dealing with time variables and time differences)
#' @param unit character value, if time variables are being lagged and the difference is taken, which time unit should be used
#' to calculate the difference
#' @param verbose boolean indicating about the state of the process be displayed
#'
#' @return data.frame with new columns containing the lagged variables
#'
#' @examples 
#' dat <- data.frame(a = runif(10, max = 10),
#' b = Sys.Date()+1:10)
#' lagVars(dat, lags = -1:2, vars = c("a","b"), 
#' diffvars = "b", unit = "hours")
#' 
#' @seealso \code{\link{lagVarsNested}}
#'
#' @export
lagVars <- function(dat, lags = 1, vars = vars, diffvars = diffvars, unit = "auto", 
                    verbose = F, ...){
  if(verbose) {
    cat("Preprocessing lagging variables: \n")
    pb <- txtProgressBar(min = 0, max = nrow(dat), style = 3) 
  }
  for(row in 1:nrow(dat)){
    for(lag in lags){
      #if(row - lag < 1) next 
      for(var in vars){
        if(var %in% diffvars){ # for diff vars (e.g, time difference)
          if(any(c("POSIXct","Date") %in% class(dat[row,var]))) { # if time variable
            dat[row,paste0(var,"Diff",lag)] <-  
              if(row - lag < 1) NA else{
              as.numeric(difftime(dat[row,var],dat[row-lag,var], units = unit))}
          }else{ # if difference variable
            dat[row,paste0(var,"Diff",lag)] <- 
              if(row - lag < 1) NA else{
                dat[row,var] - dat[row-lag,var]}
          }
        }else{ # for regular lagged
          dat[row,paste0(var,"Lag",lag)] <- 
            if(row - lag < 1) NA else{dat[row-lag,var]}
        }
        
      }
    }
    if(verbose) setTxtProgressBar(pb, row) #cat(paste0("\r row ",row," out of ",nrow(dat)))
  }
  return(dat)  
}  

# example
if(testing){
dat <- data.frame(a = runif(10, max = 10),
                  b = Sys.Date()+1:10)
lagVars(dat, lags = -1:2, vars = c("a","b"), diffvars = "b", unit = "hours")
}



#nested lagging

#' Lagging variables in nested (multilevel) data
#'
#' Creates extra column(s) with lagged variables according to the order of the
#' data.frame. The argument \code{nestVars} describes with regards to which
#' variables the data is nested. The function then creates lagged variables only
#' within each group of the \code{nestVars}. This is particularly useful when
#' working with multilevel data, where observations are nested, e.g., within
#' individuals.
#'
#' @param dat data.frame containing the variables to be lagged
#' @param lags vector defining the \code{n}th lag. Positive values (e.g., 1:2)
#'   indicatw the values from the previous rows (e.g., the values from the
#'   previous (lag = 1) observation) should be taken. Negative values indicate
#'   that the values from the following rows should be taken.
#' @param vars names of the columns in the data.frame that should be lagged
#' @param nestVars name(s) of the columns indicating how the data is nested
#'   (e.g., ID variable). Currently up to two \code{nestVars} are possible.
#' @param diffvars names of columns in the data.frame of which the difference to
#'   the current row should be taken (this is particularly helpful for dealing
#'   with time variables and time differences)
#' @param unit character value, if time variables are being lagged and the
#'   difference is taken, which time unit should be used to calculate the
#'   difference
#' @param verbose boolean indicating about the state of the process be displayed
#'
#' @return data.frame with new columns containing the lagged variables
#'
#' @examples dat <- data.frame(ID = c(rep(1,10), rep(2,10)),
#'                   day = rep(c(rep(1,5), rep(2,5)),2),
#'                   a = runif(20, max = 10),
#'                   b = Sys.Date()+1:20,
#'                   cat = sample(c("X","Y","Z"), 20, replace = T))
#' dat <- lagVarsNested(dat, lags = 1:2, nestVars = c("ID","day"),
#'                      vars = c("a","b"), diffvars = "b", unit = "days")
#'
#' @seealso \code{\link{lagVars}}
#'
#'
#' @export
lagVarsNested <- function(dat = dat, vars, nestVars, lags = 1, diffvars = NULL,
                          unit = "secs", 
                          verbose = T){
  
  
  # TODO: maybe it would be more efficient to not use the nested vars but use
  # a dummy for the first measure (i.e., for ID, burst, and day).
  

  #initial tests 
  if(length(nestVars) > 2)  stop(" more than 2 nestVars not supported") #TODO: support more than 2 nestVars
    
  #processing 
  out <- list()
  
  for(nv1 in unique(dat[,nestVars[1]])){
   #if(length(out) >= 273) stop()
  sub1 <- dat[dat[,nestVars[1]] == nv1,]
  #if(nrow(sub1) < 2) next
  if(length(nestVars) > 1){ # go in 2nd level nesting
    for(nv2 in unique(sub1[,nestVars[2]])){
      sub2 <- sub1[sub1[,nestVars[2]] == nv2,]
      #if(nrow(sub2) < 2) next
      if(length(nestVars) > 2) {
        stop(" more than 2 nestVars not supported") #TODO: support more than 2 nestVars
      }else{ # process for 2nd nesting
            
            out[[length(out)+1]] <- lagVars(dat = sub2, lags = lags, 
                                            vars = vars, diffvars = diffvars, unit = unit, verbose = F)
            
            if(length(out) > 1) if(ncol(out[[length(out)]]) != ncol(out[[length(out)-1]])) stop("Ncol does not match")
        }
      }
    }
  else{ #process for only 1 nesting
    
    out[[length(out)+1]] <- lagVars(dat = sub1, lags = lags, 
                                    vars = vars, diffvars = diffvars, unit = unit, verbose = verbose)
    
  }
  if(verbose) cat(paste0("\r ",which(nv1 == unique(dat[,nestVars[1]])), 
                         " out of ", length(unique(dat[,nestVars[1]])), " ",
                         nestVars[1],"s"))
  }
  
  out <- do.call(rbind, out)
  rownames(out) <- 1:nrow(out)
  return(out) 
}

if(testing){
dat <- data.frame(ID = c(rep(1,10), rep(2,10)),
                  day = rep(c(rep(1,5), rep(2,5)),2),
                  a = runif(20, max = 10),
                  b = Sys.Date()+1:20,
                  cat = sample(c("X","Y","Z"), 20, replace = T))
dat <- lagVarsNested(dat, lags = 1:2, nestVars = c("ID","day"),
              vars = c("a","b"), diffvars = "b", unit = "days")

}

#' Adding rows with censored data points
#' 
#' Creates extra rows for indicating left- or right-censored data. 
#' 
#' @param dat data.frame containing the variables to be lagged
#' @param nestVars name(s) of the columns indicating how the data is nested
#'   (e.g., ID variable). Currently up to two \code{nestVars} are possible.
#' @param timeVar name of the column with the time variable. 
#' @param eventVar name of the column indicating if an event (value = 1) happened or not/censored (0)
#' @param catVar name of the event (for coxph or frailty) or event-type (for multi-state)
#'  column where the indication of the censoring is stored.
#' @param timeGap time to be added (for right-censoring) or removed (for left-censoring) form \code{timeVar}
#' @param censoring character string \code{"right"} or \code{"left"} indicating if right- or left-
#' censored data row should be added
#' 
#' @return data.frame with extra rows for censored data.
#' 
#' @examples  dat <- data.frame(ID = c(rep(1,10), rep(2,10)),
#'                   day = rep(c(rep(1,5), rep(2,5)),2),
#'                   event = 1,
#'                   a = runif(20, max = 10),
#'                   t = rlnorm(20,1,.4),
#'                   cat = sample(c("X","Y","Z"), 20, replace = T))
#' censoringData(dat, nestVars = c("ID"), timeVar = "t")
#' censoringData(dat, nestVars = c("ID","day"), timeVar = "t")

#' censoringData(dat, nestVars = c("ID"), timeVar = "t", censoring = "left",TimeGap = 1
#' 
#' @export
censoringData <- function(dat, nestVars, timeVar, eventVar = "event", 
                          catVar = NULL, TimeGap = 1, censoring = "right", verbose =T){
  dat$nOb <- 1:nrow(dat)
  
  
  ## TODO: left censoring
  if(length(nestVars) > 2) stop("more than two nestVars not yet supported")
  
  for(nv1 in unique(dat[,nestVars[1]])){
    if(is.na(nv1))next
    if(length(nestVars) == 1){
        tmp.df <- dat[dat[,nestVars[1]] %in% nv1,]
      # for only one nest Var
      if(censoring == "right"){
        tmp.row <- tmp.df[nrow(tmp.df),c("nOb",nestVars[1],timeVar,eventVar)]
        tmp.row[,timeVar] <- tmp.row[,timeVar] + TimeGap
        tmp.row[,eventVar] <- 0
        if(!is.null(catVar)) tmp.row[,catVar] <- "right-censored" 
        tmp.row[,"nOb"] <- max(tmp.row[,"nOb"], na.rm = T)+0.5
        dat <- plyr::rbind.fill(dat, tmp.row)
      }
      if(censoring == "left"){
        tmp.row <- tmp.df[1,c("nOb",nestVars[1],timeVar,eventVar)]
        tmp.row[,timeVar] <- tmp.row[,timeVar] - TimeGap
        tmp.row[,eventVar] <- 0
        if(!is.null(catVar)) tmp.row[,catVar] <- "left-censored" 
        tmp.row[,"nOb"] <- min(tmp.row[,"nOb"], na.rm = T)-0.5
        dat <- plyr::rbind.fill(dat, tmp.row)
      }
      
    }else{
      for(nv2 in unique(dat[,nestVars[2]])){
        if(is.na(nv2))next
        # for the second layer nest Var
          tmp.df <- dat[dat[,nestVars[1]] %in% nv1 & dat[,nestVars[2]] %in% nv2 ,]
        if(censoring == "right"){
          tmp.row <- tmp.df[nrow(tmp.df),c("nOb",nestVars[1],nestVars[2],timeVar,eventVar)]
          tmp.row[,timeVar] <- tmp.row[,timeVar] + TimeGap
          tmp.row[,eventVar] <- 0
          if(!is.null(catVar)) tmp.row[,catVar] <- "right-censored" 
          tmp.row[,"nOb"] <- max(tmp.row[,"nOb"], na.rm = T)+0.5
          dat <- plyr::rbind.fill(dat, tmp.row)
        }
        if(censoring == "left") {
        tmp.row <- tmp.df[1,c("nOb",nestVars[1],nestVars[2],timeVar,eventVar)]
        tmp.row[,timeVar] <- tmp.row[,timeVar] - TimeGap
        tmp.row[,eventVar] <- 0
        if(!is.null(catVar)) tmp.row[,catVar] <- "left-censored" 
        tmp.row[,"nOb"] <- min(tmp.row[,"nOb"], na.rm = T)-0.5
        dat <- plyr::rbind.fill(dat, tmp.row)
        }
      }
      
    }
    if(verbose) cat(paste0("\r ",which(nv1 == unique(dat[,nestVars[1]])), 
                           " out of ", length(unique(dat[,nestVars[1]])), " ",
                           nestVars[1],"s"))
  }
  dat <- dat[order(dat$nOb),]
  return(dat)
}


if(testing){
  dat <- data.frame(ID = c(rep(1,10), rep(2,10)),
                    day = rep(c(rep(1,5), rep(2,5)),2),
                    event = 1,
                    a = runif(20, max = 10),
                    t = rlnorm(20,1,.4),
                    cat = sample(c("X","Y","Z"), 20, replace = T))
  censoringData(dat, nestVars = c("ID"), timeVar = "t")
  censoringData(dat, nestVars = c("ID","day"), timeVar = "t")
  
  censoringData(dat, nestVars = c("ID"), timeVar = "t", censoring = "left",TimeGap = 1)
  
}


#' Adding rows with left-censored data points
#' 
#' Creates extra rows for indicating left-censored data. 
#' 
#' #' @param dat data.frame containing the variables to be lagged
#' @param nestVars name(s) of the columns indicating how the data is nested
#'   (e.g., ID variable). Currently up to two \code{nestVars} are possible.
#' @param timeVar name of the column with the time variable. Only this variable 
#' will be copied if \code{all.falues = FALSE}.
#' @param all.values boolean if all values of the first row should be copied or only
#' the time information (timeVar)
#' @param catVar name of the event (for coxph or frailty) or event-type (for multi-state)
#'  column where the indication of the left-censoring is stored.
#' @param catName character or numeric value to be written in \code{catVar}
#' 
#' @return data.frame with extra rows for left-censored data.
#' 
#' @examples 
#'dat <- data.frame(ID = c(rep(1,10), rep(2,10)),
#'                  day = rep(c(rep(1,5), rep(2,5)),2),
#'                  a = runif(20, max = 10),
#'                  b = Sys.Date()+1:20,
#'                  cat = sample(c("X","Y","Z"), 20, replace = T))
#'insertLeftCensor(dat, nestVars = c("ID","day"), 
#'                 timeVar = "b", catVar = "cat")
#' 
#' @export
insertLeftCensor <- function(dat, nestVars, timeVar, all.values = F, catVar,  catName = "(left censored)"){
  #checks
  
  
  #processing 
  out <- list()
  
  for(nv1 in unique(dat[,nestVars[1]])){
    sub1 <- dat[dat[,nestVars[1]] == nv1,]
    if(!all.values){
    newrow <- data.frame(nv1, sub1[1,timeVar], catName)
    colnames(newrow) <- c(nestVars[1],timeVar, catVar)
    out[[length(out)+1]] <- plyr::rbind.fill(newrow, sub1)
    }
    if(all.values){
      newrow <- sub1[1,]
      newrow[,catVar] <- catName
      out[[length(out)+1]] <- plyr::rbind.fill(newrow, sub1)
    }
  }
  out <- do.call(rbind, out)
  rownames(out) <- 1:nrow(out)
  return(out) 
}
if(testing){
  dat <- data.frame(ID = c(rep(1,10), rep(2,10)),
                    day = rep(c(rep(1,5), rep(2,5)),2),
                    a = runif(20, max = 10),
                    b = Sys.Date()+1:20,
                    cat = sample(c("X","Y","Z"), 20, replace = T))
  insertLeftCensor(dat, nestVars = c("ID","day"), 
                   timeVar = "b", catVar = "cat")
}

#insert non-interaction row after each interaction
#' @param timeLag A positive number or vector with positive numbers indicating the time in the "not-alone" state. If a vector is provided random samples of the vector will be taken.
#' @param insertNA A vector with the variable names that should be filled with NAs for the "not-alone" state
#' @export
insertAloneTime <- function(dat, nestVars = NULL, timeVar = "date", catVar = "alter", insertNA = NULL,timeLag = 1){
  #insertNA <-  all.vars(formula[[3]][[2]])
  
  dat$rowNr <- 1:nrow(dat) # add rowNumber for ordering later
  
  if(any(timeLag < 0)) stop("please only provide positive values of the timeLag argument")
  dat.int <- dat
  dat.alone <- dat
  
  dat.int$int <- 1
  dat.alone$int <- 0
  dat.alone[,timeVar] <- dat.alone[,timeVar] + sample(timeLag,1, replace = T) 
  dat.alone[,catVar] <- "Alone"
  # if(!is.null(insertNA)) dat.alone[,!(colnames(dat.alone) %in%c("rowNr","int",nestVars,timeVar,catVar) | 
  #                                     sapply(dat.alone, class) %in% c("POSIXct","POSIXt"))] <- NA 
  if(!is.null(insertNA)) dat.alone[,(colnames(dat.alone) %in% insertNA)] <- NA

  out <- rbind(dat.int,dat.alone)
  out <- out[order(out$rowNr),]
  out <- out[,-which(colnames(out) == "rowNr")] # remove rowNr
  return(out)  
}
if(testing){
  dat <- data.frame(ID = c(rep(1,10), rep(2,10)),
                    day = rep(c(rep(1,5), rep(2,5)),2),
                    a = runif(20, max = 10),
                    time = Sys.time()+1:20,
                    cat = sample(c("X","Y","Z"), 20, replace = T))
  
  insertAloneTime(dat, nestVars = c("ID"), timeVar = "time", catVar = "cat")
}

#' @export
insertTimeBetween <- function(dat, startVar = "start", endVar = "end", nestVars = NULL,catVar = "int",catName = "alone"){
  
  for(nv1 in unique(dat[,nestVars[1]])){
    dat[dat[,nestVars[1]] %in% nv1,"obs.start"] <- dat[dat[,nestVars[1]] %in% nv1,startVar][1]
  }
  
  dat <- dat[order(dat[,nestVars[1]], dat[,startVar]),]
  dat.between <- dat # copy data
  
  dat.between[,startVar] <- dat[,endVar]
  
  for(row in 1:(nrow(dat.between)-1)){ # take end from lagged (future) start variable
    dat.between[row,endVar] <- dat[row+1,startVar]
    cat(paste0("\r",row," of ",nrow(dat.between)," rows"))
  }
  
  
  dat[,catVar] <- 1
  dat.between[,catVar] <- catName
  out <- rbind(dat,dat.between)
  out <- out[order(out[,nestVars[1]], out$start),]
  return(out)
}

### mean centering function
meanCenteringNested <- function(dat = dat, vars, nestVars, verbose = T, na.rm = T){
  #testing
  # vars = c("a","b")
  # nestVars = c("ID","day")
  
  if(length(nestVars) == 1){
    for(nv1 in unique(dat[,nestVars[1]])){
      for(var in vars){ 
        # mean center
        dat[dat[,nestVars[1]] == nv1,paste0(var,"_",nestVars[1],"_MeanC")] <-
          dat[dat[,nestVars[1]] == nv1,var]-
          mean(dat[dat[,nestVars[1]] == nv1,var], na.rm = na.rm)
        
        # mean 
        dat[dat[,nestVars[1]] == nv1,paste0(var,"_",nestVars[1],"_Mean")] <-
          mean(dat[dat[,nestVars[1]] == nv1,var], na.rm = na.rm)
      }
    }
  }
  if(length(nestVars) == 2){
    for(nv1 in unique(dat[,nestVars[1]])){
      for(nv2 in unique(dat[dat[,nestVars[1]] == nv1,nestVars[2]])){
        
      for(var in vars) {
        # mean center 
        dat[dat[,nestVars[1]] == nv1 & dat[,nestVars[2]] == nv2,
            paste0(var,"_",nestVars[1],nestVars[2],"_MeanC")] <- 
          dat[dat[,nestVars[1]] == nv1 & dat[,nestVars[2]] == nv2,var]-
          mean(dat[dat[,nestVars[1]] == nv1 & dat[,nestVars[2]] == nv2,var], na.rm = na.rm)
        
        # mean 
        dat[dat[,nestVars[1]] == nv1 & dat[,nestVars[2]] == nv2,
            paste0(var,"_",nestVars[1],nestVars[2],"_Mean")] <- 
          mean(dat[dat[,nestVars[1]] == nv1 & dat[,nestVars[2]] == nv2,var], na.rm = na.rm)
      }
        
      }
    }
  }
  if(length(nestVars) > 2){stop("nested centering not implemented for more than 2 variables")}
  
  return(dat)
}


if(testing){
  dat <- data.frame(ID = c(rep(1,10), rep(2,10)),
                    day = rep(c(rep(1,5), rep(2,5)),2),
                    a = runif(20, max = 10),
                    b = Sys.Date()+1:20,
                    cat = sample(c("X","Y","Z"), 20, replace = T))
  
  dat <- meanCenteringNested(dat, vars = c("a","b"), nestVars = c("ID"))
  meanCenteringNested(dat, vars = c("a","b"), nestVars = c("ID","day"))
}



## 2. bring into long format ##
#' @export
toLong <- function(dat = dat, catVar, fixed.categories = T){
  out <- list()
  
  if(fixed.categories){
    cats <- unique(dat[,catVar],fromLast = T)
    cats <- cats[!is.na(cats)]
    
    cat(paste0("Preprocessing toLong:\n"))
    pb <- txtProgressBar(min = 0, max = nrow(dat), style = 3) 
    for(row in 1:nrow(dat)){ 
      tmp.out <- data.frame()
      for(i in 1:length(cats)){ # multiply data 
        tmp.out <- rbind(tmp.out, dat[row,])
      }
      tmp.out$cat <- cats
      #tmp.out$n <- length(out)+1
      tmp.out$event <- 0
      tmp.out$event[which(dat[row,catVar] == cats)] <- 1 # add event 
      out[[length(out)+1]] <- tmp.out
      setTxtProgressBar(pb, row)
    }
    
    out <- do.call(rbind, out)
    rownames(out) <- 1:nrow(out)
    out$int <- 1
    return(out)
  }
  
  if(!fixed.categories){
    # TODO: implement
    stop("Code not yet implemented for flexible categorie (i.e., different 
         sets for differend IDs")
  }
}
# test
# dat <- data.frame(ID = c(rep(1,2), rep(2,2)),
#                   cat = sample(c("X","Y","Z"), 4, replace = T),
#                   a = runif(4, max = 10),
#                   b = Sys.Date()+1:4)

if(testing){
tmp <- toLong(dat, catVar = "cat")
tmp 
}



#' @export
defineMorningMeasure <- function(dat, dayVar, nestVars){
  
  dayVar = "date"
  nestVars = "ID"
  
  # determine morning measure
  dat$MorningMeasure <- rep(F, nrow(dat))
  for(nv1 in unique(dat[,nestVars[1]])){
    days <- unique(as.Date(dat[dat[,nestVars[1]] %in% nv1,dayVar]))
    for(day in days){
      first <- min(which(as.Date(dat[dat[,nestVars[1]] %in% nv1,dayVar]) == day))
      dat[dat[,nestVars[1]] %in% nv1,"MorningMeasure"][first] <- T
      
    }
    
    cat(paste0("\r ", which(nv1 == unique(dat[,nestVars[1]])),
               " of ", length(unique(dat[,nestVars[1]]))))
  }
  
  #check
  #View(dat[,c(nestVars[1], dayVar,"MorningMeasure")])
  
  #remove morning measure
  #dat[dat$MorningMeasure,vars] <- NA
  return(dat)
  
}

# window function
#' @export
computeWindowVars <- function(dat, vars = vars, nestVars, FUN = "mean", window = "All", timeVar = NULL, 
                              burnIn = 0, 
                    verbose = F, ...){
  
 # window <- 2
  # TODO: get window format right, i.e., transform timeVar and window so that they are compatible
  
  # create new variables
  dat[,paste0(vars,"_",FUN,"_window",window)] <- NA
  
  if(length(nestVars) > 1) stop("More than one nestVar currently not supported")
  if(window != "All") burnIn = 0 # remove burning for window variables
  
  for(nv1 in unique(dat[,nestVars[1]])){
    if(is.na(nv1)) next
  if(verbose) cat(paste0("\r ", nestVars[1]," ",which(nv1 == unique(dat[,nestVars[1]]))," out of ",length(unique(dat[,nestVars[1]]))))
  tmp <- dat[dat[,nestVars[1]] %in% nv1,]
  for(row in 1:nrow(tmp)){
      for(var in vars){
      if(window == "All"){window.start = 1}else{
        window.start <- min(which(tmp[row,timeVar]-window <= tmp[,timeVar]))
        if(length(window.start) == 0 | is.infinite(window.start) | window.start <= 1) next
      }
      #only compute function when burnIn value is reached
        var.dat <- tmp[window.start:(row-1),var]
        if(sum(!is.na(var.dat)) > burnIn){
          dat[dat[,nestVars[1]] %in% nv1,paste0(var,"_",FUN,"_window",window)][row] <- do.call(FUN, list(var.dat))
        }
    }
  }
  }
  return(dat)  
}  

# example
if(testing){
  dat <- data.frame(a = runif(10, max = 10),
                    b = Sys.Date()+1:10, cat = sample(c("A","B"), 10, replace = T))
  computeWindowVars(dat, vars = c("a"))
  computeWindowVars(dat, vars = c("a"), FUN = "sd")
  computeWindowVars(dat, vars = c("a"), FUN = "mean", window = 2, timeVar = "b")
}





getAbsTime <- function(dat, nestVars = "id",timeVar = "time",origin = Sys.time(), verbose = T, ...){
  if(verbose) {
    #cat("Getting absolute: \n")
    pb <- txtProgressBar(min = 0, max = length(unique(dat[,nestVars[[1]]])), style = 3) 
  }
  for(nv1 in unique(dat[,nestVars[[1]]])){
    #nv1 = 1
    tmp <- dat[dat[,nestVars[1]] %in% nv1,]
    dat[dat[,nestVars[1]] %in% nv1,"date"][1] <- tmp[1,"date"] <- origin + tmp[1, timeVar]
    for(row in 2:nrow(tmp)){
      dat[dat[,nestVars[1]] %in% nv1,"date"][row] <-  tmp[row,"date"] <- tmp[row-1,"date"] +
        tmp[row,timeVar]
    }
    if(verbose) setTxtProgressBar(pb, which(nv1 == unique(dat[,nestVars[[1]]])))
  }
  return(dat)
}

