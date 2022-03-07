#Najprej uvozimo potrebne knjiznice
source("library.r", encoding="UTF-8")


#Uvozimo podatke
X1_ptdf_ram <- read_csv("1_ptdf_ram.csv")
X2_np <- read_csv("2_np.csv")

test = left_join(X1_ptdf_ram, X2_np, by="TimeLT")

c = test$ALBE * test$ALBE_NP + test$ALDE * test$ALDE_NP + test$AT * test$AT_NP + test$BE * test$BE_NP + test$DE * test$DE_NP + test$FR * test$FR_NP + test$NL * test$NL_NP
test$c = c

zamaseno = test$c >= test$RAM
test$zamaseno = zamaseno

kdo_je_zamasil = filter(test, zamaseno==TRUE) %>% select(1,2,3,20)
