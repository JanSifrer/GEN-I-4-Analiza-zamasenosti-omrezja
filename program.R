#First we have to import librarys
source("library.r", encoding="UTF-8")

#Import data

X1_ptdf_ram <- read_csv("1_ptdf_ram.csv")

#First we have to set our time period: we will observe this data just between 20.9.2021 and 31.10.2021
X1_ptdf_ram[c('Year', 'Time')] <- str_split_fixed(X1_ptdf_ram$TimeLT, ' ', 2)
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
X1_ptdf_ram[c('Name1', 'Name2')] <- str_split_fixed(X1_ptdf_ram$Name, ' ', 2)
#part of Swiss 
ch <- grepl("CH", X1_ptdf_ram$Name1, fixed = TRUE)
X1_ptdf_ram <- X1_ptdf_ram[!ch,]
#part of Czech
cz <- grepl("CZ", X1_ptdf_ram$Name1, fixed = TRUE)
X1_ptdf_ram <- X1_ptdf_ram[!cz,]

X2_np <- read_csv("2_np.csv")

Joined <- left_join(X1_ptdf_ram, X2_np, by="TimeLT")

c <- Joined$ALBE * Joined$ALBE_NP + Joined$ALDE * Joined$ALDE_NP + Joined$AT * Joined$AT_NP + Joined$BE * Joined$BE_NP + Joined$DE * Joined$DE_NP + Joined$FR * Joined$FR_NP + Joined$NL * Joined$NL_NP
Joined$c <- c

# search for clogged ones...
clogged <- Joined$c >= Joined$RAM
Joined$Clogged <- clogged

who_clogged <- filter(Joined, Clogged==TRUE) %>% select(1,2,3,14,15,24)

Grouped_el <- group_by(who_clogged, Name)%>% summarise(sum(Clogged))
colnames(Grouped_el) <- c("Name", "Sum")
#Order by the sum
Grouped_el <- Grouped_el[order(Grouped_el$Sum, decreasing = TRUE),]

Grouped_el_2 <- Grouped_el[Grouped_el$Sum <= 2,]
Grouped_el_2[c('name1', 'name2')] <- str_split_fixed(Grouped_el_2$Name, ' ', 2)
Grouped_el_2 <- group_by(Grouped_el_2, name1)%>% summarise(sum(Sum))
colnames(Grouped_el_2) <- c("Name", "Sum")
Grouped_el_2 <- Grouped_el_2[order(Grouped_el_2$Sum, decreasing = TRUE),]

Grouped <- group_by(who_clogged, Name1) %>% summarise(sum(Clogged))
colnames(Grouped) <- c("Name", "Sum")
#Order by the sum
Grouped <- Grouped[order(Grouped$Sum, decreasing = TRUE),]

################################################################################
################################################################################
################################################################################
################################################################################

#data <- Joined[c(1,2,11,12,13,14,15,16,17,18,19,20,22)]
Joined$Clogged <- as.numeric(Joined$Clogged)

#Split the data by days:

Day <- weekdays(Joined$Year)
Joined$Day <- Day
Joined$Day <- str_replace_all(Joined$Day,"ponedeljek", "1")
Joined$Day <- str_replace_all(Joined$Day,"torek", "2")
Joined$Day <- str_replace_all(Joined$Day,"sreda", "3")
Joined$Day <- str_replace_all(Joined$Day,"četrtek", "4")
Joined$Day <- str_replace_all(Joined$Day,"petek", "5")
Joined$Day <- str_replace_all(Joined$Day,"sobota", "6")
Joined$Day <- str_replace_all(Joined$Day,"nedelja", "7")
Joined$Day <- as.numeric(Joined$Day)

find_koef_at_time <- function(Joined,time,day){
    #First find all power grids:
    Grids <- unique(Joined$Name1)
    Time <- unique(Joined$Time)
    PTDF <- c('FREE', 'AT', 'BE', 'DE', 'FR', 'NL', 'ALBE', 'ALDE', 'RAM')
    
    #Prepare a table for koeficients:
    koef <- matrix(nrow=(length(Grids)),ncol=9)
    rownames(koef) <- c(Grids)
    colnames(koef) <- PTDF
    
    for (i in 1:length(Grids)){
      print(i)
      testna <- Joined[Joined$Name1 == Grids[i],]
      testna <- testna[testna$Time == time,]
      testna <- testna[testna$Day == day,]
      test <- lm(RAM-c ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM, testna)
      #print(summary(test))
      koef[grids[i],] <- test$coefficients
      print(koef)
    }
    
    return(koef)
}

#testna <- Joined[Joined$Name1 == Grids[18],]
#testna <- testna[testna$Time == "06:00:00",]
#testna <- testna[testna$day == 1,]
#test <- lm(RAM-c ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM, testna)
#summary(test)
