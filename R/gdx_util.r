# CAPRI gdx utilities

# packages needed
library(tidyverse)
library(gdxrrw)
#igdx("c:/Users/Dev/24.9/")
igdx("/opt/GAMS")
library(xlsx)



load_dataout <- function(filename){
  
  dataout <- rgdx.param(filename, "dataOut")
  dataout <- as.tibble(dataout)
  colnames(dataout) <- c("region","dim5","cols","rows","year","value")
  
  return(dataout)  
}




capri_filter <- function(filename, selregion = "all", seldim5, selcols, selrows, simyear = "2030", scenarioname = "baseline"){
  
  
  x <- load_dataout(filename) 
  
# the grepl-paste trick is needed for case insensitive filtering  
  if(length(seldim5))       {x <- x %>% filter(grepl(paste("\\b", paste(seldim5, collapse = "\\b|"), "\\b", sep =""), 
                                                     dim5, ignore.case = TRUE))}
  if(seldim5[1] == "-1")    {x <- x %>% select(-dim5)}
  
  

  
if(selregion[1] != "all") {  
  if(length(selregion))       {x <- x %>% filter(grepl(paste("\\b", paste(selregion, collapse = "\\b|"), "\\b", sep =""), 
                                                       region, ignore.case = TRUE))}
  if(selregion[1] == "-1")    {x <- x %>% select(-region)}
}
  
  
  if(length(selcols))       {x <- x %>% filter(grepl(paste("\\b", paste(selcols, collapse = "\\b|"), "\\b", sep =""),
                                                     cols, ignore.case = TRUE))}
  if(selcols[1] == "-1")    {x <- x %>% select(-cols)}
  
  
  if(length(selrows))       {x <- x %>% filter(grepl(paste("\\b", paste(selrows, collapse = "\\b|"), "\\b", sep =""),
                                                     rows, ignore.case = TRUE))}
  if(selrows[1] == "-1")    {x <- x %>% select(-rows)}
  
  
  if(length(simyear))       {x <- x %>% filter(year %in% simyear)}
  if(simyear[1] == "-1")    {x <- x %>% select(-year)}
  
  
  # optional regional selection    
  if(selregion[1] == "nuts0") {x <- x %>% filter(grepl("*000000", region))}
  
  # optional scenario name to attach  
  if(nrow(x) > 0) {x$scenario <- scenarioname}
  
  return(x)
}

# function to calculate percentage changes
pchange <- function(a,b){(b-a)/abs(a)*100}

# write stuff to .gdx
write_param_togdx <- function(x, file, symname = "default", ts = "default") {
  
  x <- data.frame(x)
  attr(x, "symName") <- symname
  attr(x, "ts")      <- ts
  
  wgdx.lst(file, x)
  
} 