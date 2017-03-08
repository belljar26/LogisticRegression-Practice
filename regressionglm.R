NatHealth11 <-readRDS("NatHealth2011.rds")
View(NatHealth11)
str(NatHealth11)
str(NatHealth11$hypev)
levels(NatHealth11$hypev)
#Collapse refused, n/a and DK to NA
NatHealth11$hypev[(NatHealth11$hypev == "7 Refused") |
                    (NatHealth11$hypev =="8 Not ascertained") |
                    (NatHealth11$hypev == "9 Don't know")] <- NA
# Run log. regression -predict prob. dx with hypertension based on age, sex
# sleep and bmi
names(NatHealth11)

NH11_Mod1 <- glm(hypev ~ age_p + bmi + sleep + sex, data= NatHealth11,
                 family= "binomial")
summary(NH11_Mod1)
#Transform coeff
Mod1est <- coef(summary(NH11_Mod1))
Mod1est[, "Estimate"] <- exp(coef(NH11_Mod1))
Mod1est
# How much more likely is 63 year old than 33 year old to have hypertension?
Mod2NatHealth11 <- with(NatHealth11, expand.grid(age_p= c(33,63),
                                                 sex= "2 Female",
                                                 bmi= mean(bmi, na.rm=TRUE),
                                                 sleep = mean(sleep, na.rm= TRUE)))
cbind(Mod2NatHealth11, predict(NH11_Mod1, type = "response",
      se.fit = TRUE, interval= "confidence",
      newdata= Mod2NatHealth11)) 
library(effects)
plot(allEffects(NH11_Mod1))
                                                 
                                                