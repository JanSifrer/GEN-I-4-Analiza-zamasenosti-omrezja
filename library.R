library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(stringr)
library(lme4)
library(knitr)


#calculate_coefficients <- function(Joined, time_start, time_end, day_start, day_end){
#  my.data.model <- Joined[,c("Time","Day","Name1","Name2","AT", "BE", "DE", "FR", "NL", "ALBE", "ALDE", "RAM", "c", "congested","AT_NP", "BE_NP", "DE_NP", "FR_NP", "NL_NP", "ALBE_NP", "ALDE_NP")]
#  #First find all power grids:
#  Grids <- unique(my.data.model$Name1)
#  #Grids <- c("[AT-AT]")
#  Time <- unique(my.data.model$Time)
#  PTDF <- c("FREE_koef", "AT", "BE", "DE", "FR", "NL", "ALBE", "ALDE","RAM","R")
#  
#  #Prepare a table for koeficients:
#  koef <- matrix(nrow=(length(Grids)),ncol=(length(PTDF)))
#  rownames(koef) <- c(Grids)
#  colnames(koef) <- PTDF
#  
#  i =3
#  for (i in 1:length(Grids)){
#    #print(i)
#    i = 1
#    testna <- my.data.model[my.data.model$Name1 == Grids[i],]
#    testna <- testna[testna$Time >= time_start,]
#    testna <- testna[testna$Time <= time_end,]
#    testna <- testna[testna$Day >= day_start,]
#    testna <- testna[testna$Day <= day_end,]
#    testna$what <- testna$RAM - testna$c
#    if (nrow(testna)>10){
#      test <- lm(what ~ AT+ BE + DE + FR + NL + ALBE + ALDE + RAM, testna)
#      # print(summary(test)$r.squared)
#      koef[Grids[i],"FREE_koef"]<- summary(test)$coefficient[,1]["(Intercept)"]
#      koef[Grids[i],"AT"] <- summary(test)$coefficient[,1]["AT"]
#      koef[Grids[i],"BE"] <- summary(test)$coefficient[,1]["BE"]
#      koef[Grids[i],"DE"] <- summary(test)$coefficient[,1]["DE"]
#      koef[Grids[i],"FR"] <- summary(test)$coefficient[,1]["FR"]
#      koef[Grids[i],"NL"] <- summary(test)$coefficient[,1]["NL"]
#      koef[Grids[i],"ALBE"] <- summary(test)$coefficient[,1]["ALBE"]
#      koef[Grids[i],"ALDE"] <- summary(test)$coefficient[,1]["ALDE"]
#      koef[Grids[i],"RAM"] <- summary(test)$coefficient[,1]["RAM"]
#      koef[Grids[i],"R"] <- summary(test)$r.squared
#      
#    }
#    #  if(nrow(testna)<=10){
#    #   koef[Grids[i],] <- c(0,0,0,0,0,0,0,0,0,0)
#    #  }
#    #print(koef)
#  }
#  koef[is.na(koef)] <- 0
#  #return(koef)
#  return(testna)
#}