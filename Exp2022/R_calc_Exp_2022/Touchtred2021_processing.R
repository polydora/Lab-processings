library(readxl)
library(dplyr)
library(ggplot2)

myt <- read_excel("Data/Touchtred_2021.xlsx", na = "NA") 

myt <- myt %>% mutate(Duration = as.numeric(Date_of_End - Date_of_Begin), Prop_to_Rec = Byss_to_Recipient/(Byss_to_Substrate + Byss_to_Recipient))

table(myt$Duration)

myt <- myt %>% mutate(N_bys = Byss_to_Recipient + Byss_to_Substrate)


myt<- myt %>% filter( (Expected_Donor == True_Donor)&(Expected_Recipient == True_Recipient))



myt$True_Donor <- factor(myt$True_Donor, labels =  c("Donor_E", "Donor_T"))
myt$True_Recipient <- factor(myt$True_Recipient, labels =  c("Rec_E", "Rec_T"))





myt %>% ggplot(., aes(x = Duration, y = Prop_to_Rec, color = True_Recipient)) + geom_point() +geom_smooth(method = "lm") + facet_wrap(~True_Donor)




myt %>% ggplot(., aes(x = Duration, y = N_bys)) + geom_point() +geom_smooth() + facet_grid(Year~True_Donor)

myt %>% ggplot(., aes(x = Duration, y = N_bys)) + geom_point() +geom_smooth() 

myt %>% ggplot(., aes(x = Duration, y = N_bys, group = Duration)) + geom_boxplot() 


myt %>% ggplot(., aes(x = True_Recipient, y = Prop_to_Rec, fill = True_Recipient)) + geom_boxplot()  + facet_grid(Year~True_Donor)

myt %>% ggplot(., aes(x = True_Recipient, y = Prop_to_Rec, fill = True_Recipient)) + geom_boxplot()  + facet_wrap(~True_Donor)



myt %>% ggplot(., aes(x = True_Recipient, y = N_bys, fill = True_Recipient)) + geom_boxplot()  + facet_grid(Year~True_Donor)





library(glmmTMB)

myt2 <- myt[complete.cases(myt), ]

myt2$Prop_to_Rec2 <- myt2$Prop_to_Rec
myt2$Prop_to_Rec2[myt2$Prop_to_Rec == 1] <- 0.9999 
myt2$Prop_to_Rec2[myt2$Prop_to_Rec == 0] <- 0.0001 
myt2$Year <- factor(myt2$Year)




Mod <- glmmTMB(Prop_to_Rec2 ~ True_Recipient*True_Donor*Duration*Year + (1|Plate), data = myt2, family = "beta_family")

summary(Mod)
  
drop1(Mod)  

Mod2 <- update(Mod, .~.-True_Recipient:True_Donor:Duration:Year)


drop1(Mod2)
Mod3 <- update(Mod2, .~.-True_Donor:Duration:Year)

drop1(Mod3)
Mod4 <- update(Mod3, .~.-True_Recipient:Duration:Year)

drop1(Mod4, test = "Chi")
Mod5 <- update(Mod4, .~.-True_Recipient:True_Donor:Year)


drop1(Mod5, test = "Chi")
Mod6 <- update(Mod5, .~.-True_Recipient:Year)

drop1(Mod6, test = "Chi")
Mod7 <- update(Mod6, .~.-Duration:Year)


drop1(Mod7, test = "Chi")
Mod8 <- update(Mod7, .~.-Duration:Year)


drop1(Mod8, test = "Chi")
Mod9 <- update(Mod8, .~.-True_Recipient:True_Donor:Duration )


drop1(Mod9, test = "Chi")
Mod10 <- update(Mod9, .~.-True_Recipient:Duration)


drop1(Mod10, test = "Chi")
Mod11 <- update(Mod10, .~.-True_Donor:Duration)


drop1(Mod11, test = "Chi")


summary(Mod11)



library(emmeans)

emm_obj <- emmeans(Mod11, specs = ~ True_Recipient*True_Donor*Year, type = "response" )


contrast(emm_obj)

pairs(emm_obj)


emm_obj2 <- emmeans(Mod11, specs = ~ True_Recipient*True_Donor, type = "response" )

contrast(emm_obj2, simple = list("True_Donor", "True_Recipient"))

pairs(emm_obj2)

contrast(emm_obj2, "poly")




Mod_summary <- as.data.frame(emm_obj)

ggplot(Mod_summary, aes(x = True_Recipient, y = response)) + geom_col(fill = "gray", color = "black") + facet_grid(Year~True_Donor) + geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) + geom_hline(yintercept = 0.5, linetype = 2)






emm_obj3 <- emtrends(Mod11,  var = "Duration", type = "response", delta.var = 1,
                     at = list(Duration = c(2, 4, 6, 8, 10)))

summary(emm_obj3)

