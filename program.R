#First we have to import librarys
source("library.r", encoding="UTF-8")

#Import data

X1_ptdf_ram <- read_csv("Data/1_ptdf_ram.csv")

#First we have to set our time period: we will observe this data just between 20.9.2021 and 31.10.2021
X1_ptdf_ram[c("Year", "Time")] <- str_split_fixed(X1_ptdf_ram$TimeLT, " ", 2)
X1_ptdf_ram$Year <- as.Date(X1_ptdf_ram$Year)
X1_ptdf_ram <- X1_ptdf_ram[X1_ptdf_ram$Year >= as.Date("2021-09-20") & X1_ptdf_ram$Year <= as.Date("2021-10-31") ,]
#X1_ptdf_ram <- X1_ptdf_ram[c(-12,-13)]

#Change zones for Germany

X1_ptdf_ram$Name <- str_replace_all(X1_ptdf_ram$Name,"D1", "DE")
X1_ptdf_ram$Name <- str_replace_all(X1_ptdf_ram$Name,"D2", "DE")
X1_ptdf_ram$Name <- str_replace_all(X1_ptdf_ram$Name,"D3", "DE")
X1_ptdf_ram$Name <- str_replace_all(X1_ptdf_ram$Name,"D4", "DE")
X1_ptdf_ram$Name <- str_replace_all(X1_ptdf_ram$Name,"D5", "DE")
X1_ptdf_ram$Name <- str_replace_all(X1_ptdf_ram$Name,"D6", "DE")
X1_ptdf_ram$Name <- str_replace_all(X1_ptdf_ram$Name,"D7", "DE")
X1_ptdf_ram$Name <- str_replace_all(X1_ptdf_ram$Name,"D8", "DE")

#Change the names
X1_ptdf_ram[c("Name1", "Name3")] <- str_split_fixed(X1_ptdf_ram$Name, " ", 2)
X1_ptdf_ram[c("Name4", "Name5")] <- str_split_fixed(X1_ptdf_ram$Name3, "_LTA",2)
X1_ptdf_ram[c("Name2", "Name6")] <- str_split_fixed(X1_ptdf_ram$Name4, "] - ",2)
X1_ptdf_ram <- X1_ptdf_ram[,c(-15,-16,-17,-19)]

#part of Swiss 
#ch <- grepl("CH", X1_ptdf_ram$Name1, fixed = TRUE)
X1_ptdf_ram <- X1_ptdf_ram[!grepl("CH", X1_ptdf_ram$Name1, fixed = TRUE),]
#part of Czech
#cz <- grepl("CZ", X1_ptdf_ram$Name1, fixed = TRUE)
X1_ptdf_ram <- X1_ptdf_ram[!grepl("CZ", X1_ptdf_ram$Name1, fixed = TRUE),]

X2_np <- read_csv("Data/2_np.csv")

Joined <- left_join(X1_ptdf_ram, X2_np, by="TimeLT")

Joined$c <- Joined$ALBE * Joined$ALBE_NP + Joined$ALDE * Joined$ALDE_NP + Joined$AT * Joined$AT_NP + Joined$BE * Joined$BE_NP + Joined$DE * Joined$DE_NP + Joined$FR * Joined$FR_NP + Joined$NL * Joined$NL_NP

# search for congested ones...
Joined$congested <- Joined$RAM - Joined$c <= 0.5

who_congested <- filter(Joined, congested==TRUE) %>% select(1,2,3,14,15,24)
head(who_congested,20)

Grouped <- group_by(who_congested, Name1, Name2) %>% summarise(sum(congested))
colnames(Grouped) <- c("Name of grid", "Name of element", "Sum")
#Order by the sum
Grouped <- Grouped[order(Grouped$Sum, decreasing = TRUE),]


Grouped_el <- group_by(who_congested, Name2)%>% summarise(sum(congested))
Grouped_el <- Grouped_el[-1,]
colnames(Grouped_el) <- c("Name of element", "Sum")
#Order by the sum
Grouped_el <- Grouped_el[order(Grouped_el$Sum, decreasing = TRUE),]

Grouped_el_2 <- Grouped_el[Grouped_el$Sum <= 2,]

################################################################################
################################################################################
################################################################################
################################################################################

Joined$congested <- as.numeric(Joined$congested)

#Split the data by days:

Joined$Day <- weekdays(Joined$Year)
Joined$Day <- str_replace_all(Joined$Day,"ponedeljek", "1")
Joined$Day <- str_replace_all(Joined$Day,"torek", "2")
Joined$Day <- str_replace_all(Joined$Day,"sreda", "3")
Joined$Day <- str_replace_all(Joined$Day,"četrtek", "4")
Joined$Day <- str_replace_all(Joined$Day,"petek", "5")
Joined$Day <- str_replace_all(Joined$Day,"sobota", "6")
Joined$Day <- str_replace_all(Joined$Day,"nedelja", "7")
Joined$Day <- as.numeric(Joined$Day)


#time_start = "00:00:00"
#time_end = "23:00:00"
#day_start = 1
#day_end = 7
#testna <- Joined[Joined$Time >= time_start,]
#testna <- testna[testna$Time <= time_end,]
#testna <- testna[testna$Day >= day_start,]
#testna <- testna[testna$Day <= day_end,]
#testna <- testna[testna$Name1 == "[BE-BE]",]
testna <- Joined
testna$what <- testna$RAM - testna$c
working.data <- Joined
working.data$RAM.c <- working.data$RAM - working.data$c
#working.data <- working.data[working.data$what < 700,]
testna <- testna[testna$what < 700,]
#koef <- testna[testna$RAM == 2952,]
koef <- testna
#koef <- calculate_coefficients(Joined,time_start,time_end,day_start,day_end)
#test <- lm(c ~ AT + BE + DE + FR + NL + ALBE + ALDE-1, koef)
#test1 <- lm(c ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM-1, koef)
#test2 <- lm(c ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM, koef)
#test5 <- lm(what ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM-1, koef)
#
#summary(test)$adj.r.squared
#summary(test1)$adj.r.squared
#summary(test2)$adj.r.squared
#summary(test5)$adj.r.squared
#
##plot(density(koef$what))
##
#library(lme4)
koef[koef$Name2 == "",]$Name2 = koef[koef$Name2 == "",]$Name1
#
#hh <- lmer(c ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM + (1|Name1/Day) + (1|Name1/Time), data = koef)
#hh1 <- lmer(c ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM + (1|Day) + (1|Time) + (1|Name1), data = koef)
#hh2 <- lmer(c ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM + (1|Name2/Day) + (1|Name2/Time), data = koef)
#hh3 <- lmer(c ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM + (1|Day) + (1|Time) + (1|Name2), data = koef)
#
#
#

#### GROUP BY:
## COUNTRY
## TIME
## DAY

#calculate_coefficients <- function(Joined, time_start, time_end, day_start, day_end){
calculate_coefficients <- function(Joined, my.name, my.time, my.day){
  my.data.model <- Joined[,c("Time","Day","Name1","Name2","AT", 
                             "BE", "DE", "FR", "NL", "ALBE", "ALDE", 
                             "RAM", "c", "congested","AT_NP", "BE_NP", 
                             "DE_NP", "FR_NP", "NL_NP", "ALBE_NP", "ALDE_NP")]
  my.data.model$RAM.c <- my.data.model$RAM - my.data.model$c
  #First find all power grids:
  Grids <- unique(my.data.model$Name1)
  #Grids <- c("[AT-AT]")
  #Time <- unique(my.data.model$Time)
  PTDF.names <- c("AT", "BE", "DE", "FR", "NL", "ALBE", "ALDE", "RAM", "R", "Probability of congested")
  
  #Prepare a table for koeficients:
  #koef <- matrix((nrow=1),ncol=(length(PTDF.names)))
  #rownames(koef) <- my.name
  #colnames(koef) <- PTDF.names
  
  #i =6
  for (i in 1:length(Grids)){
    #print(i)
    new.data <- my.data.model[my.data.model$Name1 == my.name,]
    new.data <- new.data[new.data$Time == my.time,]
    new.data <- new.data[new.data$Day == my.day,]
    new.data$AT2 <- new.data$AT^2
    new.data$BE2 <- new.data$BE^2
    new.data$DE2 <- new.data$DE^2
    new.data$FR2 <- new.data$FR^2
    new.data$NL2 <- new.data$NL^2
    new.data$ALBE2 <- new.data$ALBE^2
    new.data$ALDE2 <- new.data$ALDE^2
    
    new.data$AT3 <- new.data$AT^3
    new.data$BE3 <- new.data$BE^3
    new.data$DE3 <- new.data$DE^3
    new.data$FR3 <- new.data$FR^3
    new.data$NL3 <- new.data$NL^3
    new.data$ALBE3 <- new.data$ALBE^3
    new.data$ALDE3 <- new.data$ALDE^3
    
    if (nrow(new.data)>1){
      test <- lm(c ~ AT + BE + DE + FR + NL + ALBE + ALDE + 
                   AT2 + BE2 + DE2 + FR2 + NL2 + ALBE2 + ALDE2 +
                   AT3 + BE3 + DE3 + FR3 + NL3 + ALBE3 + ALDE3 -1, new.data)
      test1 <- lm(c ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM, new.data)
      test2 <- lm(c ~ AT + BE + DE + FR + NL + ALBE + ALDE + 
                    AT2 + BE2 + DE2 + FR2 + NL2 + ALBE2 + ALDE2 + RAM, new.data)
      # print(summary(test)$r.squared)
      #koef[1,"AT"] <- test$coefficient["AT"]
      #koef[1,"BE"] <- test$coefficient["BE"]
      #koef[1,"DE"] <- test$coefficient["DE"]
      #koef[1,"FR"] <- test$coefficient["FR"]
      #koef[1,"NL"] <- test$coefficient["NL"]
      #koef[1,"ALBE"] <- test$coefficient["ALBE"]
      #koef[1,"ALDE"] <- test$coefficient["ALDE"]
      #koef[1,"RAM"]<- test$coefficient["RAM"]
      #koef[1,"R"] <- summary(test)$r.squared
      #koef[1,"Probability of congested"] <- sum(new.data$congested)/dim(new.data)[1]
    }
    if(nrow(new.data)<=1){
      #koef[1,] <- c(0,0,0,0,0,0,0,0,0,999)
      return(0)
    }
    #print(koef)
  }
  #koef[is.na(koef)] <- 0
  return(test)
}
library(randomForest)
library(caTools)
set.seed(123)

my.predict <- function(Joined, PTDF){
  my.name <- PTDF["Name1"][[1]]
  my.time <- PTDF["Time"][[1]]
  my.day <- PTDF["Day"][[1]]
  koef1 <- calculate_coefficients(Joined, my.name, my.time, my.day)[c(-1,-2,-3,-4,-14,-15:-22)]
  
  koef <- new.data[c(-13,-15:-22)]
  split = sample.split(koef$congested, SplitRatio = 0.75)
  training_set = subset(koef, split == TRUE)
  test_set = subset(koef, split == FALSE)
  
  #training_set[-9] = scale(training_set[-9])
  #test_set[-9] = scale(test_set[-9])
  #training_set[is.na(training_set)] <- 0
  #test_set[is.na(test_set)] <- 0
  classifier = randomForest(x = training_set[-12],
                            y = training_set$congested,
                            ntree = 1000, random_state = 0)
  plot(classifier)
  y_pred = predict(classifier, newdata = test_set)
  cm = table(test_set[, 9], y_pred)
  cm
  # rf.fit <- randomForest(c ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM - 1, 
  #                        data=koef, ntree=1000,
  #                        keep.forest=FALSE, importance=TRUE)
  # #my.koef <- koef[1,]
  #my.prediction <- PTDF["AT"][[1]]*my.koef[[1]] + PTDF["BE"][[1]]*my.koef[[2]] + 
   # PTDF["DE"][[1]]*my.koef[[3]] + PTDF["FR"][[1]]*my.koef[[4]] + PTDF["NL"][[1]]*my.koef[[5]] +
    #PTDF["ALBE"][[1]]*my.koef[[6]] + PTDF["ALDE"][[1]]*my.koef[[7]] + PTDF["RAM"][[1]]*my.koef[[8]]
  #return(c(my.prediction, koef[9],koef[10]))
}

PTDF <- data.frame("Day" = 1,"Name2" = "Diele - Meeden 380 Black [DIR] [DE]", "Name1" = "[DE-NL]", "Time" = "00:00:00","AT"=0.58860, "BE"=0.25637, "DE"=0.58860, "FR"=0.38150, "NL"=0, "ALBE"=-0.30955, "ALDE"=0, "RAM"=1399)
PTDF01 <- data.frame("Day" = 1,"Name2" = "Diele - Meeden 380 Black [DIR] [DE]", "Name1" = "[DE-NL]", "Time" = "00:00:00","AT"=0.58860,"AT2"=0.58860^2, "BE"=0.25637, "BE2"=0.25637^2, "DE"=0.58860, "DE2"=0.58860^2, "FR"=0.38150, "FR2"=0.38150^2, "NL"=0, "NL2"=0^2, "ALBE"=-0.30955, "ALBE2"=-0.30955^2, "ALDE"=0, "ALDE2"=0^2, "RAM"=1399)
##true value is: 17.02078
##c = 1381.979
my.predict(Joined,PTDF)

PTDF1 <- data.frame("Day" = 2, "Name2"="Ensdorf - Vigy 2S [OPP] [DE]", "Name1" = "[DE-FR]", "Time" = "02:00:00","AT"=0.02458, "BE"=0.13275, "DE"=-0.01193, "FR"=0.20635, "NL"=0.02558, "ALBE"=0.10677, "ALDE"=-0.06474, "RAM" =1075)
PTDF11 <- data.frame("Day" = 2, "Name2"="Ensdorf - Vigy 2S [OPP] [DE]", "Name1" = "[DE-FR]", "Time" = "02:00:00","AT"=0.02458, "BE"=0.13275, "DE"=-0.01193, "FR"=0.20635, "NL"=0.02558, "ALBE"=0.10677, "ALDE"=-0.06474,"AT2"=0.02458^2, "BE2"=0.13275^2, "DE2"=-0.01193^2, "FR2"=0.20635^2, "NL2"=0.02558^2, "ALBE2"=0.10677^2, "ALDE2"=-0.06474^2, "RAM" =1075)
PTDF111 <- data.frame("Day" = 2, "Name2"="Ensdorf - Vigy 2S [OPP] [DE]", "Name1" = "[DE-FR]", "Time" = "02:00:00","AT"=0.02458, "BE"=0.13275, "DE"=-0.01193, "FR"=0.20635, "NL"=0.02558, "ALBE"=0.10677, "ALDE"=-0.06474,"AT2"=0.02458^2, "BE2"=0.13275^2, "DE2"=-0.01193^2, "FR2"=0.20635^2, "NL2"=0.02558^2, "ALBE2"=0.10677^2, "ALDE2"=-0.06474^2,
                     "AT3"=0.02458^3, 
                     "BE3"=0.13275^3, 
                     "DE3"=-0.01193^3, 
                     "FR3"=0.20635^3, 
                     "NL3"=0.02558^3, 
                     "ALBE3"=0.10677^3, 
                     "ALDE3"=-0.06474^3, 
                     "RAM" =1075)
##true value is: -0.27015
##c = 1075.2701
predict(test,PTDF111)
predict(test1,PTDF1)
predict(test2,PTDF11)

predict(hh2,PTDF111)

my.predict(Joined,PTDF1)

PTDF2 <- data.frame("Day" = 1, "Name2" = "Pleinting - St. Peter 258 [DIR] [AT]","Name1" = "[DE-AT]", "Time" = "17:00:00","AT"=0.15915, "BE"=0, "DE"=0.15915, "FR"=0.97386, "NL"=0, "ALBE"=0.03069, "ALDE"=0, "RAM" =2952)
##true value is: 0.14847
##c = 2951.852
my.predict(Joined,PTDF2)

PTDF3 <- data.frame("Day" = 7, "Name2"="Buers Transformer 37 [DIR]" ,"Name1" = "[DE-DE]", "Time" = "11:00:00","AT"=-0.41615, "BE"=0, "DE"=0.79506, "FR"=0, "NL"=0, "ALBE"=0.44125, "ALDE"=0, "RAM"=7982)
##true value is: 3.76918
##c = 7978.231
my.predict(Joined,PTDF3)

PTDF4 <- data.frame("Day" = 4,"Name2"="Diele - Meeden 380 Black [DIR] [DE]", "Name1" = "[DE-NL]", "Time" = "22:00:00","AT"=-0.31925, "BE"=0.0645, "DE"=0.90467, "FR"=0.0645, "NL"=0, "ALBE"=-0.26706, "ALDE"=0, "RAM"=7982)
##true value is: 353.46941
##c = 7628.531
my.predict(Joined,PTDF4)

######IF )# LINE OF THE CODE IS SET TO TOO LITLE; THAN THIS DON'T WORK!
##hh2per <- performance::check_predictions(hh2) #good
##performance::check_predictions(hh1) #good
##performance::check_predictions(hh)  #also good, but no so good as hh2
##
##performance::check_predictions(test)  #not good
##performance::check_predictions(test1) #not too bad
##performance::check_predictions(test2) #not bad
##performance::check_predictions(test5) #bad
##
##performance::performance_accuracy(hh2)
##a <- performance::compare_performance(hh2,hh1,hh,test,test1,test2)
##from this test we see, that the hh2 model is the best.
#
##ranef(hh)
##ranef(hh)$'Time:Name1'["00:00:00:[DE-NL]",]
##ranef(hh)$'Day:Name1'["1:[DE-NL]",]
##ranef(hh)$Name1["[DE-NL]",]
##Test predictions:
#
#hh2.res <- summary(hh2)$residuals
#sigma.my <- sigma(hh2)
#
write.csv(koef,"Data/koef.csv", row.names = FALSE)
#
##true value is: 17.02078
##c = 1381.979
#PTDF <- data.frame("Day" = 1,"Name2" = "Diele - Meeden 380 Black [DIR] [DE]", "Name1" = "[DE-NL]", "Time" = "00:00:00","AT"=0.58860, "BE"=0.25637, "DE"=0.58860, "FR"=0.38150, "NL"=0, "ALBE"=-0.30955, "ALDE"=0, "RAM"=1399)
##day <- 1
##time <- 00:00:00
##name <- [DE-NL]
#RAM=1399
#predict(hh, PTDF)
#predict(hh1, PTDF)
#predict(hh3, PTDF)
#napoved <- predict(hh2,PTDF)
#interval <- (c(napoved - 1.96*sigma.my, napoved + 1.96*sigma.my))
#napoved
#interval
#probability.of.congestion <- 1-(interval[2]-RAM)/(interval[2]-interval[1])
#probability.of.congestion
#
#predict.lm(test2,PTDF,interval = c("prediction"))
#
##true value is: -0.27015
##c = 1075.2701
#PTDF <- data.frame("Day" = 2,"Name2"="Ensdorf - Vigy 2S [OPP] [DE]", "Name1" = "[DE-FR]", "Time" = "02:00:00","AT"=0.02458, "BE"=0.13275, "DE"=-0.01193, "FR"=0.20635, "NL"=0.02558, "ALBE"=0.10677, "ALDE"=-0.06474, "RAM" =1075)
##day <- 2
##time <- 02:00:00
##name <- [DE-FR]
#RAM = 1075
#predict(hh, PTDF)
#predict(hh1, PTDF)
#napoved <- predict(hh2,PTDF)
#interval <- (c(napoved - 1.96*sigma.my, napoved + 1.96*sigma.my))
#napoved
#interval
#probability.of.congestion <- (RAM-interval[1])/(interval[2]-interval[1])*0.95
#probability.of.congestion
#
#predict.lm(test2,PTDF,interval = c("prediction"))
#
##true value is: 0.14847
##c = 2951.852
#PTDF <- data.frame("Day" = 1, "Name2" = "Pleinting - St. Peter 258 [DIR] [AT]","Name1" = "[DE-AT]", "Time" = "17:00:00","AT"=0.15915, "BE"=0, "DE"=0.15915, "FR"=0.97386, "NL"=0, "ALBE"=0.03069, "ALDE"=0, "RAM" =2952)
##day <- 1
##time <- 17:00:00
##name <- [DE-AT]
#RAM =2952
#predict(hh, PTDF)
#predict(hh1, PTDF)
#napoved <- predict(hh2,PTDF)
#interval <- (c(napoved - 1.96*sigma.my, napoved + 1.96*sigma.my))
#napoved
#interval
#probability.of.congestion <- (RAM-interval[1])/(interval[2]-interval[1])*0.95
#probability.of.congestion
#
#predict.lm(test2,PTDF,interval = c("prediction"))
#
##true value is: 3.76918
##c = 7978.231
#PTDF <- data.frame("Day" = 7, "Name2"="Buers Transformer 37 [DIR]" ,"Name1" = "[DE-DE]", "Time" = "11:00:00","AT"=-0.41615, "BE"=0, "DE"=0.79506, "FR"=0, "NL"=0, "ALBE"=0.44125, "ALDE"=0, "RAM"=7982)
##day <- 7
##time <- 11:00:00
##name <- [DE-DE]
#RAM=7982
#predict(hh, PTDF)
#predict(hh1, PTDF)
#napoved <- predict(hh2,PTDF)
#interval <- (c(napoved - 1.96*sigma.my, napoved + 1.96*sigma.my))
#napoved
#interval
#probability.of.congestion <- (RAM-interval[1])/(interval[2]-interval[1])*0.95
#probability.of.congestion
#
#predict.lm(test2,PTDF,interval = c("prediction"))
#
#
##true value is: 353.46941
##c = 7628.531
#PTDF <- data.frame("Day" = 4,"Name2"="Diele - Meeden 380 Black [DIR] [DE]", "Name1" = "[DE-NL]", "Time" = "22:00:00","AT"=-0.31925, "BE"=0.0645, "DE"=0.90467, "FR"=0.0645, "NL"=0, "ALBE"=-0.26706, "ALDE"=0, "RAM"=7982)
##day <- 4
##time <- 22:00:00
##name <- [DE-NL]
#RAM=7982
#predict(hh, PTDF)
#predict(hh1, PTDF)
#napoved <- predict(hh2,PTDF)
#interval <- (c(napoved - 1.96*sigma.my, napoved + 1.96*sigma.my))
#napoved
#interval
#probability.of.congestion <- (RAM-interval[1])/(interval[2]-interval[1])*0.95
#probability.of.congestion
#
#predict.lm(test2,PTDF,interval = c("prediction"))
#
##ranef(hh1)
##ranef(hh1)$'Time:Day'["00:00:00:1",]
##ranef(hh1)$Day["1",]
#
# 