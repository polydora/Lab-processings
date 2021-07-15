library(dplyr)

bis <- read.table("clipboard", header = T, sep = "\t")

str(bis)

nrow(bis)
# bis <- bis %>% filter( (True_Donor == Expected_Donor) & (True_Recipient == Expected_Recipient) )

bis <- bis %>% mutate(Total_Byss = Byss_to_Substrate +  Byss_to_Recipient, Prop_to_Recip = Byss_to_Recipient / Total_Byss)

library(ggplot2)


ggplot(bis, aes(x = True_Donor, y = Total_Byss, fill = True_Donor)) + geom_boxplot() + facet_grid(True_Donor~Date_of_End)


ggplot(bis, aes(x = True_Recipient , y = Total_Byss)) + geom_boxplot()

ggplot(bis, aes(x = True_Donor, y = Prop_to_Recip, fill = True_Recipient)) + geom_boxplot()

ggplot(bis, aes(x = True_Donor, y = Prop_to_Recip, fill = True_Recipient)) + geom_boxplot() + facet_grid(True_Donor~Date_of_End)


ggplot(bis, aes(x = True_Donor, y = Total_Byss, fill = True_Recipient)) + geom_boxplot() + facet_wrap(~Date_of_End, nrow = 1)

ggplot(bis, aes(x = True_Donor, y = Byss_to_Substrate, fill = True_Recipient)) + geom_boxplot() + facet_wrap(~Date_of_End, nrow =1)

ggplot(bis, aes(x = True_Donor, y = Byss_to_Recipient, fill = True_Recipient)) + geom_boxplot() + facet_wrap(~Date_of_End)

ggplot(bis, aes(x = Date_of_End, y = Total_Byss)) + geom_boxplot()

