loadFun <- function(x) {
  x <- as.character(substitute(x))
  if(isTRUE(x %in% .packages(all.available=TRUE))) {
    eval(parse(text=paste("library(", x, ")", sep="")))
  } else {
    #update.packages()  ## good idea, but may take some time. can
    ## usually be safely skipped
    eval(parse(text=paste("install.packages('", x, "')", sep="")))
    eval(parse(text=paste("library(", x, ")", sep="")))
  }
}