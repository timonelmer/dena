########### DENA ############
############################
#  Datacheck function #
##########################


checkFormatDat <- function(dat){
  if(!is.data.frame(dat)) stop("dat object needs to be in data frame format")
}

checkFormatVars <- function(dat, vars){
  if(any(!(vars %in% colnames(dat)))) stop(paste0("\n variable ", vars[which(!(vars %in% colnames(dat)))],
                                                  " not in data.frame"))
}

