#First we have to import librarys
source("library.r", encoding="UTF-8")

#Import data

X1_ptdf_ram <- read_csv("1_ptdf_ram.csv")

#First we have to set our time period: we will observe this data just between 20.9.2021 and 31.10.2021
X1_ptdf_ram[c('Year', 'Time')] <- str_split_fixed(X1_ptdf_ram$TimeLT, ' ', 2)
X1_ptdf_ram$Year <- as.Date(X1_ptdf_ram$Year)
X1_ptdf_ram <- X1_ptdf_ram[X1_ptdf_ram$Year >= as.Date("2021-09-20") & X1_ptdf_ram$Year <= as.Date("2021-10-31") ,]
X1_ptdf_ram <- X1_ptdf_ram[c(-12,-13)]

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

who_clogged <- filter(Joined, Clogged==TRUE) %>% select(1,2,3,12,13,22)

Grouped_el <- group_by(who_clogged, Name)%>% summarise(sum(Clogged))
colnames(Grouped_el) <- c("Name", "Sum")
#Order by the sum
Grouped_el <- Grouped_el[order(Grouped_el$Sum, decreasing = TRUE),]

Grouped_el_2 <- Grouped_el[Grouped_el$Sum <= 2,]
Grouped_el_2[c('name1', 'name2')] <- str_split_fixed(Grouped_el_2$Name, ' ', 2)
Grouped_el_2 <- group_by(Grouped_el_2, name1)%>% summarise(sum(Sum))
colnames(Grouped_el_2) <- c("Name", "Sum")
Grouped_el_2 <- Grouped_el_2[order(Grouped_el_2$Sum, decreasing = TRUE),]


################################################################################
################################################################################
################################################################################
################################################################################

Grouped <- group_by(who_clogged, Name1) %>% summarise(sum(Clogged))
colnames(Grouped) <- c("Name", "Sum")
#Order by the sum
Grouped <- Grouped[order(Grouped$Sum, decreasing = TRUE),]

#The elements of the electrical power grid that clogged the most of the time are:
#print(Grouped[Grouped$Sum > 100,])         

#The elements of the electrical power grid that clogged the rarest are:
#print(Grouped[Grouped$Sum < 15,]) 

#The other elements:
#print(Grouped[Grouped$Sum <= 100 & Grouped$Sum >= 15,]) 
         