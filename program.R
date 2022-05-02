#First we have to import librarys
source("library.r", encoding="UTF-8")

#Import data

X1_ptdf_ram <- read_csv("1_ptdf_ram.csv")

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
X1_ptdf_ram[c("Name2", "Name4")] <- str_split_fixed(X1_ptdf_ram$Name3, "_LTA",2)
X1_ptdf_ram <- X1_ptdf_ram[,c(-15,-17)]

#part of Swiss 
#ch <- grepl("CH", X1_ptdf_ram$Name1, fixed = TRUE)
X1_ptdf_ram <- X1_ptdf_ram[!grepl("CH", X1_ptdf_ram$Name1, fixed = TRUE),]
#part of Czech
#cz <- grepl("CZ", X1_ptdf_ram$Name1, fixed = TRUE)
X1_ptdf_ram <- X1_ptdf_ram[!grepl("CZ", X1_ptdf_ram$Name1, fixed = TRUE),]

X2_np <- read_csv("2_np.csv")

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

calculate_coefficients <- function(Joined, time_start, time_end, day_start, day_end){
  my.data.model <- Joined[,c("Time","Day","Name1","Name2","AT", "BE", "DE", "FR", "NL", "ALBE", "ALDE", "RAM", "c", "congested","AT_NP", "BE_NP", "DE_NP", "FR_NP", "NL_NP", "ALBE_NP", "ALDE_NP")]
  #First find all power grids:
  Grids <- unique(my.data.model$Name1)
  #Grids <- c("[AT-AT]")
  Time <- unique(my.data.model$Time)
  PTDF <- c("FREE_koef", "AT", "BE", "DE", "FR", "NL", "ALBE", "ALDE","RAM","R")
  
  #Prepare a table for koeficients:
  koef <- matrix(nrow=(length(Grids)),ncol=(length(PTDF)))
  rownames(koef) <- c(Grids)
  colnames(koef) <- PTDF
  
  i =3
  for (i in 1:length(Grids)){
    #print(i)
    i = 1
    testna <- my.data.model[my.data.model$Name1 == Grids[i],]
    testna <- testna[testna$Time >= time_start,]
    testna <- testna[testna$Time <= time_end,]
    testna <- testna[testna$Day >= day_start,]
    testna <- testna[testna$Day <= day_end,]
    testna$what <- testna$RAM - testna$c
    if (nrow(testna)>10){
      test <- lm(what ~ AT+ BE + DE + FR + NL + ALBE + ALDE + RAM, testna)
     # print(summary(test)$r.squared)
      koef[Grids[i],"FREE_koef"]<- summary(test)$coefficient[,1]["(Intercept)"]
      koef[Grids[i],"AT"] <- summary(test)$coefficient[,1]["AT"]
      koef[Grids[i],"BE"] <- summary(test)$coefficient[,1]["BE"]
      koef[Grids[i],"DE"] <- summary(test)$coefficient[,1]["DE"]
      koef[Grids[i],"FR"] <- summary(test)$coefficient[,1]["FR"]
      koef[Grids[i],"NL"] <- summary(test)$coefficient[,1]["NL"]
      koef[Grids[i],"ALBE"] <- summary(test)$coefficient[,1]["ALBE"]
      koef[Grids[i],"ALDE"] <- summary(test)$coefficient[,1]["ALDE"]
      koef[Grids[i],"RAM"] <- summary(test)$coefficient[,1]["RAM"]
      koef[Grids[i],"R"] <- summary(test)$r.squared
      
    }
  #  if(nrow(testna)<=10){
   #   koef[Grids[i],] <- c(0,0,0,0,0,0,0,0,0,0)
    #  }
    #print(koef)
  }
  koef[is.na(koef)] <- 0
  #return(koef)
  return(testna)
}

time_start = "17:00:00"
time_end = "17:00:00"
day_start = 1
day_end = 7
testna <- Joined[Joined$Time >= time_start,]
testna <- testna[testna$Time <= time_end,]
testna <- testna[testna$Day >= day_start,]
testna <- testna[testna$Day <= day_end,]
#testna <- testna[testna$Name1 == "[BE-BE]",]
testna$what <- testna$RAM - testna$c
testna <- testna[testna$what < 200,]
koef <- testna
#koef <- calculate_coefficients(Joined,time_start,time_end,day_start,day_end)
test <- lm(c ~ exp(AT) + BE + DE + FR + NL + ALBE + ALDE-1, koef)
test1 <- lm(c ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM-1, koef)
#test2 <- lm(congested ~ AT + BE + DE + FR + NL + ALBE + ALDE, koef)
#test3 <- lm(congested ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM, koef)
#test4 <- lm(what ~ AT + BE + DE + FR + NL + ALBE + ALDE-1, koef)
test5 <- lm(what ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM -1, koef)

summary(test)$adj.r.squared
summary(test1)$adj.r.squared
#summary(test2)$adj.r.squared
#summary(test3)$adj.r.squared
#summary(test4)$adj.r.squared
summary(test5)$adj.r.squared

#plot(density(koef$what))

PTDF <- data.frame("AT"=0.02458, "BE"=0.13275, "DE"=-0.01193, "FR"=0.20635, "NL"=0.02558, "ALBE"=0.10677, "ALDE"=-0.06474, "RAM" =1075)
RAM <- 1075
#predict.lm(test,PTDF,interval = c("confidence"))
predict.lm(test1,PTDF,interval = c("prediction"))
#predict.lm(test4,PTDF,interval = c("prediction"))
predict.lm(test5,PTDF,interval = c("prediction"))
#predict.lm(test5,PTDF,interval = c("confidence"))

PTDF <- data.frame("AT"=0.15915, "BE"=0, "DE"=0.15915, "FR"=0.97386, "NL"=0, "ALBE"=0.03069, "ALDE"=0, "RAM" =2952)
predict.lm(test1,PTDF,interval = c("prediction"))
#predict.lm(test4,PTDF,interval = c("prediction"))
predict.lm(test5,PTDF,interval = c("prediction"))
#This one should be congested


PTDF <- data.frame("AT"=0.01837, "BE"=0.11815, "DE"=-0.01796, "FR"=0.18027, "NL"=0.02695, "ALBE"=0.09507, "ALDE"=-0.06513, "RAM"=791)
#RAM <- 791
predict.lm(test,PTDF,interval = c("prediction"))
predict.lm(test1,PTDF,interval = c("prediction"))
#predict.lm(test4,PTDF,interval = c("prediction"))
predict.lm(test5,PTDF,interval = c("prediction"))

#This one should be congested

PTDF <- data.frame("AT"=0.02152, "BE"=0.13414, "DE"=-0.00874, "FR"=0.21006, "NL"=0.03323, "ALBE"=0.09382, "ALDE"=-0.05750, "RAM"=807)
#RAM <- 807
predict.lm(test,PTDF,interval = c("prediction"))
predict.lm(test1,PTDF,interval = c("prediction"))
#predict.lm(test4,PTDF,interval = c("prediction"))
predict.lm(test5,PTDF,interval = c("prediction"))

#This one should not be congested


PTDF <- data.frame("AT"=0.15915, "BE"=0, "DE"=0.15915, "FR"=0.97386, "NL"=0, "ALBE"=0.03069, "ALDE"=0, "RAM"=2952)
RAM <- 2952
#Grid_con <- "[DE-AT]"
predict.lm(test,PTDF,interval = c("prediction"))
predict.lm(test1,PTDF,interval = c("prediction"))
#predict.lm(test4,PTDF,interval = c("prediction"))
predict.lm(test5,PTDF,interval = c("prediction"))

#This one should not be congested:
PTDF <- data.frame("AT"=0.27184, "BE"=0.41495, "DE"=-0.80132, "FR"=-0.20641, "NL"=0, "ALBE"=-0.41102, "ALDE"=0, "RAM"=7982)
RAM <- 7982
predict.lm(test,PTDF,interval = c("prediction"))
predict.lm(test1,PTDF,interval = c("prediction"))
#predict.lm(test4,PTDF,interval = c("prediction"))
predict.lm(test5,PTDF,interval = c("prediction"))

#PTDF_testni_notcon <- data.frame("AT"=0.26781, "BE"=0.47176, "DE"=0.60565, "FR"=0.47176, "NL"=0, "ALBE"=0.34109, "ALDE"=0)
#RAM_testni_notcon <- 3106
#Grid_notcon <- "[DE-NL]"
#koef_notcon <- calculate_coefficients(Joined,"00:00:00","00:00:00",1,1)
#
#
#

#