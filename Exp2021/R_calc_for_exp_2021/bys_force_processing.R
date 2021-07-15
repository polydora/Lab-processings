bys_force <- read.table("clipboard", header = T, sep = "\t")

library(ggplot2)


bys_force2 <- bys_force %>% filter((Label == "l" & Morphotype == "t" )|(Label == "r" & Morphotype == "e" ) )


nrow(bys_force)

nrow(bys_force2)/nrow(bys_force)




ggplot(bys_force2, aes(x = Type, y = N_byss, fill = Morphotype)) + geom_boxplot()


bys_force2 <- bys_force2 %>% filter(complete.cases(.))




ggplot(bys_force2, aes(x = Type, y = Force-Weight/100, fill = Morphotype)) + geom_boxplot()


ggplot(bys_force2, aes(x = N_byss, y = Force-Weight/100, color = Morphotype)) + geom_point() + facet_wrap(~Type) + geom_smooth(method = "lm")




ggplot(bys_force2, aes(y = N_byss, x = Size, color = Morphotype)) + geom_point() + facet_wrap(~Type) + geom_smooth(method = "lm")


ggplot(bys_force2, aes(y = log(Weight), x = log(Size), color = Morphotype)) + geom_point()  + geom_smooth(method = "lm")



Mod_size <- lm(log(Weight) ~ Size + Morphotype, data = bys_force2)


plot(Mod_size)

summary(Mod_size)



ggplot(bys_force2, aes(x = Weight, y = Force)) + geom_point()  + geom_smooth(method = "lm")



library(lme4)

library(nlme)



bys_force2 <- bys_force2 %>% mutate(Force2 = Force - Weight/100) 

Mod <- lme(Force ~ Type*Morphotype + Weight, random =  ~ 1|Cage, data = bys_force2)

Mod_ML <- lme(Force ~ Type*Morphotype + Weight, random =  ~ 1|Cage, data = bys_force2, method = "ML")





summary(Mod)

plot(Mod)

drop1(Mod_ML)



library(MuMIn)

TukeyHSD(Mod)

bys_force %>% mutate(Force2 = Force - Weight/100) %>% ggplot(., aes(x = Type, y = Force2, fill = Label)) + geom_boxplot(notch = T)


bys_force %>% mutate(Force2 = Force - Weight/100) %>%  mutate(R_Force = Force2/N_byss) %>%   ggplot(., aes(x = Type, y = R_Force, fill = Label)) + geom_boxplot(notch = T)



