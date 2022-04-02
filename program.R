#First we have to import librarys
source("library.r", encoding="UTF-8")


#Import data
X1_ptdf_ram <- read_csv("1_ptdf_ram.csv")
X1_ptdf_ram[c('Name1', 'Name2')] <- str_split_fixed(X1_ptdf_ram$Name, ' ', 2)
#part of Swiss 
ch <- grepl("CH", X1_ptdf_ram$Name1, fixed = TRUE)
X1_ptdf_ram <- X1_ptdf_ram[!ch,]
#part of Czech
cz <- grepl("CZ", X1_ptdf_ram$Name1, fixed = TRUE)
X1_ptdf_ram <- X1_ptdf_ram[!cz,]
#Change zones for Germany
X1_ptdf_ram$Name1 <- str_replace_all(X1_ptdf_ram$Name1,"D1", "DE")
X1_ptdf_ram$Name1 <- str_replace_all(X1_ptdf_ram$Name1,"D2", "DE")
X1_ptdf_ram$Name1 <- str_replace_all(X1_ptdf_ram$Name1,"D3", "DE")
X1_ptdf_ram$Name1 <- str_replace_all(X1_ptdf_ram$Name1,"D4", "DE")
X1_ptdf_ram$Name1 <- str_replace_all(X1_ptdf_ram$Name1,"D5", "DE")
X1_ptdf_ram$Name1 <- str_replace_all(X1_ptdf_ram$Name1,"D6", "DE")
X1_ptdf_ram$Name1 <- str_replace_all(X1_ptdf_ram$Name1,"D7", "DE")
X1_ptdf_ram$Name1 <- str_replace_all(X1_ptdf_ram$Name1,"D8", "DE")

X2_np <- read_csv("2_np.csv")

Joined <- left_join(X1_ptdf_ram, X2_np, by="TimeLT")

c <- Joined$ALBE * Joined$ALBE_NP + Joined$ALDE * Joined$ALDE_NP + Joined$AT * Joined$AT_NP + Joined$BE * Joined$BE_NP + Joined$DE * Joined$DE_NP + Joined$FR * Joined$FR_NP + Joined$NL * Joined$NL_NP
Joined$c <- c

# search for clogged ones...
clogged <- Joined$c >= Joined$RAM
Joined$Clogged <- clogged

who_clogged <- filter(Joined, Clogged==TRUE) %>% select(1,2,12,22)

Grouped <- group_by(who_clogged, Name1) %>% summarise(sum(Clogged))
colnames(Grouped) <- c("Name", "Sum")
#Order by the sum
Grouped <- Grouped[order(Grouped$Sum),]

#The elements of the electrical power grid that clogged the most of the time are:
print(Grouped[Grouped$Sum > 100,])         

#The elements of the electrical power grid that clogged the rarest are:
print(Grouped[Grouped$Sum < 15,]) 

#The other elements:
print(Grouped[Grouped$Sum <= 100 & Grouped$Sum >= 15,]) 
         