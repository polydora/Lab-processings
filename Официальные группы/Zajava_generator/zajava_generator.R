library(stringdist)
library(readxl)
library(dplyr)
library(reshape2)

library(knitr)
library(markdown)
library(rmarkdown)



registered <- read_excel("Data/ebc_distant_agreement.xlsx")

lmbe_group <- read_excel("Data/Группы ЛЭМБ 2020 2021.xlsx", sheet = "all")

Ch_Name_agree <- registered$Ch_Name

Ch_Name_lmbe <- paste(lmbe_group$Ch_Fam, lmbe_group$Ch_Name) 

d <- stringdistmatrix(Ch_Name_lmbe, Ch_Name_agree, method = "cosine")

row.names(d) <- Ch_Name_lmbe

colnames(d) <- Ch_Name_agree

Dist_names <- as.data.frame(t(d))

which(d < 0.2)

Dist_names < 0.1

found_in_registered <- melt(d) %>% filter(value < 0.09) %>% group_by(Var1) %>% summarize(Dist = mean(value))


lmbe_not_in_registered <- lmbe_group %>% filter(!(paste(Ch_Fam, Ch_Name) %in% as.character(found_in_registered$Var1)))



for (i in unique(rownames(lmbe_not_in_registered))){
  rmarkdown::render('Zajava.Rmd',  # file 2
                    output_file =  paste(lmbe_not_in_registered$Ch_Fam[i],".html", sep=''), 
                    output_dir = '/Zajaves')
  
  # for pdf reports  
  #   rmarkdown::render(input = "/Users/majerus/Desktop/R/auto_reporting/test/r_script_pdf.Rmd", 
  #           output_format = "pdf_document",
  #           output_file = paste("test_report_", car, Sys.Date(), ".pdf", sep=''),
  #           output_dir = "/Users/majerus/Desktop/R/auto_reporting/test/reports")
  
}



