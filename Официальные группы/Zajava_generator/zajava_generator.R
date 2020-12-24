library(stringdist)
library(readxl)
library(dplyr)



stringdist("ca","abc",method="cosine")


registered <- read_excel("Data/ebc_distant_agreement.xlsx")

lmbe_group <- read_excel("Data/Группы ЛЭМБ 2020 2021.xlsx", sheet = "all")

Ch_Name_agree <- registered$Ch_Name

Ch_Name_lmbe <- paste(lmbe_group$Ch_Fam, lmbe_group$Ch_Name) 

d <- stringdistmatrix(Ch_Name_lmbe, Ch_Name_agree, method = "cosine")

row.names(d) <- Ch_Name_lmbe

colnames(d) <- Ch_Name_agree

Dist_names <- as.data.frame(t(d))

which(d < 0.2)

Dist_names < 0.06

Dist_names$`Коррой Алексей`)
