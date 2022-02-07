rm(list=ls())

yelp <-read.csv("~/Desktop/Marketing Masters/Customer model/assign 2/yelp_oldies_a2_sample_2021.csv")
library(foreign)
library(rio)
library(Hmisc)
library(survival)
library(psych)
library(dplyr) 
describe(yelp)
glimpse(yelp)

colSums(is.na(yelp))#some missing values:secondtip/first_elite/secondrev
yelp$first_elite[is.na(yelp$first_elite)] <- 2018

#chaning date from chr to date
yelp$firsttip <- as.Date(yelp$firsttip, "%Y-%m-%d")
yelp$secondtip <- as.Date(yelp$secondtip, "%Y-%m-%d")
yelp$lasttip <- as.numeric(as.Date(yelp$lasttip, "%Y-%m-%d"))
yelp$lastrev <- as.numeric(as.Date(yelp$lastrev, "%Y-%m-%d"))
yelp$firstrev <- as.Date(yelp$firstrev, "%Y-%m-%d")
yelp$secondrev <- as.Date(yelp$secondrev, "%Y-%m-%d")
yelp$lastrev <- as.Date(yelp$lastrev, "%Y-%m-%d")
yelp$yelping_since <- as.numeric(as.Date(yelp$yelping_since, "%Y-%m-%d"))

glimpse(yelp)


#correlations/outliers
library(corrplot)
############Correlation graph yelp_1
yelpInt <- subset(yelp, select = -c(user_id, name, yelping_since, firsttip, secondtip, lasttip, firstrev, secondrev, lastrev))
glimpse(yelpInt)
colSums(is.na(yelpInt))

M <- cor(yelpInt)
corrplot(M, method = "circle")
yelp$lastDate <- pmax(yelp$lasttip, yelp$lastrev)
yelp$duration <- (yelp$lastDate - yelp$yelping_since)/30.42 #amount of months on first vs last activity on yelp_1

#building new variables
#location
library(geosphere)
mat <- distm(yelp[,c('range_long','range_lat')], fun=distVincentyEllipsoid)
yelp$GDP <- yelp$GDP[apply(mat, 1, which.min)]

#compliments 
#putting: hot, cool, cute, funny, profile  together as compliments for personality
yelp$compliment_personal <- rowSums(yelp[,c(17,18,20,21,25)]) 
#putting: plain, note, writer, list, photos and more together as compliments for reviews and tips
yelp$compliment_revtips <- rowSums(yelp[,c(19,22,23,24,26,27)]) 

#stars
yelp$lowstars <- rowSums(yelp[,c(57,58)])
yelp$mediumstars <- rowSums(yelp[,c(59,60)])
yelp$highstars <- (yelp[,c(61)])


#new subset
yelp_1 <- yelp[c(-1,-2,-5,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-39,-40,-41,
                 -42,-43,-44,-45,-46,-47,-48,-49,-50,-57,-58,-59,-60,-61,
                 -68,-69,-70,-71,-72,-73,-74,-75,-76,-77,-78,-79,-80,-81)]
View(yelp_1)
####################Question 2######################
#Checking for the cox model
glimpse(yelp_1)
library(survminer)

KM_SurvivalCurve <- survfit(Surv(duration, elite)~ 1, data=yelp_1)
ggsurvplot(KM_SurvivalCurve, data=yelp_1)
summary(KM_SurvivalCurve)

#timing variable
yelp_1$difference <- (yelp_1$first_elite - yelp_1$yelping_since_year)

#AIC:8328.63, BIC: 8368.172
CoxModel1 <-coxph(Surv(difference, elite) ~ nreviews + useful + funny + cool + number_friends + fans + average_stars + compliment_personal +compliment_revtips,
                  method = "efron", data = yelp_1)
summary(CoxModel1)
exp(coef(CoxModel1))
AIC(CoxModel1)
BIC(CoxModel1)

#AIC: 8454.768, BIC: 8454.786
CoxModel2 <-coxph(Surv(difference, elite) ~ nreviews + useful + funny + cool + number_friends + fans + lowstars + highstars,
                  method = "efron", data = yelp_1)
summary(CoxModel2)
AIC(CoxModel2)
BIC(CoxModel2)

#AIC 8320.722, BIC: 8364.657
CoxModel3 <-coxph(Surv(difference, elite) ~ nreviews + useful + funny + cool + number_friends + fans + lowstars + highstars + compliment_revtips + compliment_personal,
                  method = "efron", data = yelp_1)
summary(CoxModel3)
AIC(CoxModel3)
BIC(CoxModel3)


#AIC 8293.288, BIC: 8350.405
CoxModel4 <-coxph(Surv(difference, elite) ~ nreviews + useful + funny + cool + number_friends + fans + lowstars + highstars + compliment_revtips + compliment_personal + ntips + lasttip + lastrev,
                  method = "efron", data = yelp_1)
summary(CoxModel4)
AIC(CoxModel4)
BIC(CoxModel4)

#AIC 8219.633, BIC: 8276.75
CoxModel5 <-coxph(Surv(difference, elite) ~ nreviews + useful + funny + cool + number_friends + fans + lowstars + highstars + compliment_revtips + compliment_personal + ntips + lasttip + review_length_med,
                  method = "efron", data = yelp_1)
summary(CoxModel5)
AIC(CoxModel5)
BIC(CoxModel5)

#AIC 8211.66, BIC: 8273.17
CoxModel6 <-coxph(Surv(difference, elite) ~ nreviews + useful + funny + cool + number_friends + fans + lowstars + highstars + compliment_revtips + compliment_personal + ntips + lasttip + review_length_med + tips_length_med,
                  method = "efron", data = yelp_1)
summary(CoxModel6)
AIC(CoxModel6)
BIC(CoxModel6)

#AIC 8137.747, BIC: 8212.438
CoxModel7 <-coxph(Surv(difference, elite) ~ nreviews + useful + funny + cool + number_friends + fans + lowstars + highstars + compliment_revtips + compliment_personal + ntips + lasttip + review_length_med + tips_length_med + firstrev + firsttip +lastrev,
                  method = "efron", data = yelp_1)
summary(CoxModel7)
AIC(CoxModel7)
BIC(CoxModel7)

#AIC 8132.522, BIC: 8221.606 -> best model
CoxModel8 <-coxph(Surv(difference, elite) ~ nreviews + useful + funny + cool + number_friends + fans + lowstars + highstars + compliment_revtips + compliment_personal + ntips + lasttip + review_length_med + tips_length_med + firstrev + firsttip  + lastrev + negreviews,
                  method = "efron", data = yelp_1)
summary(CoxModel8)
AIC(CoxModel8)
BIC(CoxModel8)

#concordance test
library(survival)
concordance <-concordance(object = CoxModel6,CoxModel7, CoxModel8)
concordance

#according to concordance test of best 3 models, CoxModel8 =best


#Testing for multicollinearity for best model to verify whether some of the variables overlap
multitest <-coxph(Surv(difference, elite) ~ nreviews + useful + funny + cool + number_friends + fans + lowstars + highstars + compliment_revtips + compliment_personal + ntips + lasttip + review_length_med + tips_length_med + firstrev + firsttip  + lastrev+ negreviews,
                  method = "efron", data = yelp_1)
library(car)
#create vector of VIF values and a vector of tolerance values
vif_values <- vif(multitest)
tolerance <- 1/vif_values
# the closer to 1, the lower the level of multicollinearity. 
vif_values
#The closer to 0, the higher the level of multicollinearity. 
tolerance


#multicollinearity for useful, funny, cool, number_friends, fans, nreviews, lowstars, highstars, compliment_revtips, compliment_personal
#AIC: 8211.66, BIC: 8273.17
CoxModel8b <-coxph(Surv(difference, elite) ~ ntips + lasttip + review_length_med + tips_length_med + firstrev + firsttip  + lastrev + negreviews,
                   method = "efron", data = yelp_1)
summary(CoxModel8b)
AIC(CoxModel8b)
BIC(CoxModel8b)

#concordance test
library(survival)
concordance <-concordance(object = CoxModel8, CoxModel8b)
concordance

#baseline hazard
basehazyelp_1 <- basehaz(CoxModel8b, centered=TRUE)
basehazyelp_1
plot(basehazyelp_1)

####################Question 3#####################

#Checking for the cox model, DV -> number of years
KM_SurvivalCurve2 <- survfit(Surv(nyears_elite, elite)~ 1, data=yelp_1)
ggsurvplot(KM_SurvivalCurve2, data=yelp_1)
summary(KM_SurvivalCurve2)

#AIC:6229.189, BIC: 6277.519 
CoxModel3.1 <-coxph(Surv(nyears_elite, elite) ~ nreviews + useful + funny + cool + number_friends + fans + average_stars + 
                      review_length_med + compliment_personal + compliment_revtips + yelping_since_year,
                    method = "efron", data = yelp_1)
summary(CoxModel3.1)
exp(coef(CoxModel3.1))
AIC(CoxModel3.1)
BIC(CoxModel3.1)


#AIC:6323.636, BIC: 6349.997
CoxModel3.2 <-coxph(Surv(nyears_elite, elite) ~ nreviews + lastrev + average_stars + compliment_count_tips + nreviews + review_length_med, 
                    method = "efron", data = yelp_1) 
summary(CoxModel3.2)
exp(coef(CoxModel3.2))
AIC(CoxModel3.2)
BIC(CoxModel3.2)


#AIC:6298.793, BIC: 6338.335
CoxModel3.3 <-coxph(Surv(nyears_elite, elite) ~ nreviews + useful + funny + cool + fans + lowstars + highstars + compliment_count_tips + review_length_med,
                    method = "efron", data = yelp_1)
summary(CoxModel3.3)
exp(coef(CoxModel3.3))
AIC(CoxModel3.3)
BIC(CoxModel3.3)


#AIC:6303.885, BIC: 6347.821
CoxModel3.4 <-coxph(Surv(nyears_elite, elite) ~ nreviews + number_friends + fans + lowstars + highstars + ntips + review_length_med + avg_rest_stars_rev + avg_rest_stars + yelping_since_year,
                    method = "efron", data = yelp_1)
summary(CoxModel3.4)
exp(coef(CoxModel3.4))
AIC(CoxModel3.4)
BIC(CoxModel3.4)


#AIC:6282.237, BIC: 6321.851
CoxModel3.5 <-coxph(Surv(nyears_elite, elite) ~ nreviews + number_friends + fans + lowstars + highstars + review_length_med + compliment_personal + compliment_revtips + yelping_since_year,
                    method = "efron", data = yelp_1)
summary(CoxModel3.5)
exp(coef(CoxModel3.5))
AIC(CoxModel3.5)
BIC(CoxModel3.5)


#AIC:6297.056, BIC: 6332.205 --> compliments and review_length_med cause higher AIC and BIC
CoxModel3.6 <-coxph(Surv(nyears_elite, elite) ~ nreviews + useful + funny + cool + number_friends+ fans + lowstars + highstars,
                    method = "efron", data = yelp_1)
summary(CoxModel3.6)
exp(coef(CoxModel3.6))
AIC(CoxModel3.6)
BIC(CoxModel3.6)

#AIC:6231.254, BIC: 6279.583 -> compliments and yelping since improves AIC and BIC
CoxModel3.8 <-coxph(Surv(nyears_elite, elite) ~ nreviews + useful + funny + cool + number_friends+ fans + lowstars + highstars + yelping_since + compliment_revtips + compliment_personal,
                    method = "efron", data = yelp_1)
summary(CoxModel3.8)
exp(coef(CoxModel3.8))
AIC(CoxModel3.8)
BIC(CoxModel3.8)

#AIC:6221.941, BIC: 6274.664 -> review_length improves model
CoxModel3.9 <-coxph(Surv(nyears_elite, elite) ~ nreviews + useful + funny + cool + number_friends+ fans + lowstars + highstars + yelping_since + compliment_revtips + compliment_personal + tips_length_med,
                    method = "efron", data = yelp_1)
summary(CoxModel3.9)
exp(coef(CoxModel3.9))
AIC(CoxModel3.9)
BIC(CoxModel3.9)

#AIC:6219.962, BIC: 6268.291 -> excl. funny improves
CoxModel3.10 <-coxph(Surv(nyears_elite, elite) ~ nreviews + useful + cool + number_friends+ fans + lowstars + highstars + yelping_since + compliment_revtips + compliment_personal + tips_length_med,
                     method = "efron", data = yelp_1)
summary(CoxModel3.10)
exp(coef(CoxModel3.10))
AIC(CoxModel3.10)
BIC(CoxModel3.10)

#AIC:6223.052, BIC: 6280.168 -> review_length_med + ntips does not improve
CoxModel3.11 <-coxph(Surv(nyears_elite, elite) ~ nreviews + useful + cool + number_friends+ fans + lowstars + highstars + yelping_since + compliment_revtips + compliment_personal + tips_length_med + review_length_med +ntips,
                     method = "efron", data = yelp_1)
summary(CoxModel3.11)
exp(coef(CoxModel3.11))
AIC(CoxModel3.11)
BIC(CoxModel3.11)

#AIC:6217.011, BIC: 6269.734 -> lasttip improves
CoxModel3.12 <-coxph(Surv(nyears_elite, elite) ~ nreviews + useful + cool + number_friends+ fans + lowstars + highstars + yelping_since + compliment_revtips + compliment_personal + tips_length_med + lasttip,
                     method = "efron", data = yelp_1)
summary(CoxModel3.12)
exp(coef(CoxModel3.12))
AIC(CoxModel3.12)
BIC(CoxModel3.12)

#AIC:6093.883, BIC: 6151 -> first_elite improves highly
CoxModel3.13 <-coxph(Surv(nyears_elite, elite) ~ nreviews + useful + cool + number_friends+ fans + lowstars + highstars + yelping_since + compliment_revtips + compliment_personal + tips_length_med + lasttip + first_elite,
                     method = "efron", data = yelp_1)
summary(CoxModel3.13)
exp(coef(CoxModel3.13))
AIC(CoxModel3.13)
BIC(CoxModel3.13)

#AIC:6097.115, BIC: 6163.019 -> first_tip + avg_rest_stars does not improve
CoxModel3.14 <-coxph(Surv(nyears_elite, elite) ~ nreviews + useful + cool + number_friends+ fans + lowstars + highstars + yelping_since + compliment_revtips + compliment_personal + tips_length_med + lasttip + firsttip + first_elite + avg_rest_stars,
                     method = "efron", data = yelp_1)
summary(CoxModel3.14)
exp(coef(CoxModel3.14))
AIC(CoxModel3.14)
BIC(CoxModel3.14)

#AIC:6084.039, BIC: 6145.549  -> average_stars improves
CoxModel3.15 <-coxph(Surv(nyears_elite, elite) ~ nreviews + useful + cool + number_friends+ fans + lowstars + highstars + yelping_since + compliment_revtips + compliment_personal + tips_length_med + lasttip + first_elite +average_stars,
                     method = "efron", data = yelp_1)
summary(CoxModel3.15)
exp(coef(CoxModel3.15))
AIC(CoxModel3.15)
BIC(CoxModel3.15)


#concordance test, 
library(survival)
concordance3 <-concordance(object = CoxModel3.10, CoxModel3.11, CoxModel3.12, CoxModel3.13, CoxModel3.14, CoxModel3.15)
concordance3

#Testing for multicollinearity 
multitest3 <- CoxModel3.16 <-coxph(Surv(nyears_elite, elite) ~ nreviews + useful + cool + number_friends+ fans + lowstars + highstars + yelping_since + compliment_revtips + compliment_personal + tips_length_med + lasttip + first_elite +average_stars,
                                   method = "efron", data = yelp_1)
library(car)
#create vector of VIF values and a vector of tolerance values
vif_values3 <- vif(multitest3)
tolerance3 <- 1/vif_values3
# the closer to 1, the lower the level of multicollinearity. 
vif_values3
#The closer to 0, the higher the level of multicollinearity. 
tolerance3


###indicates that nreviews,useful, cool, fans, high_stars, compliment_revtips and compliment_personal cause high multicollinearity (higher than 5)
#AIC: 6183.992, BIC: 6214.748
CoxModel3.15 <-coxph(Surv(nyears_elite, elite) ~ number_friends + lowstars + yelping_since + tips_length_med + lasttip + first_elite + average_stars,
                     method = "efron", data = yelp_1)
summary(CoxModel3.15)
exp(coef(CoxModel3.15))
AIC(CoxModel3.15)
BIC(CoxModel3.15)

#baseline hazard 3.15
basehazyelp_15 <- basehaz(CoxModel3.15, centered=TRUE)
basehazyelp_15
plot(basehazyelp_15)


####################Question 4#####################

library(foreign) 
library(haven) 
library(MASS) 
library(pscl) 
library(AER) 
library(VGAM)
library(ggplot2)

###question 4 - tips

mean(yelp_1$ntips) # mean just above 10, trying both possion and standrad regression
var(yelp_1$ntips)
ggplot(yelp_1, aes(x=ntips)) + geom_histogram(binwidth=1) + scale_x_continuous(breaks = seq(0, 100, 1)) + xlim(0, 100)


#AIC: 18865.65, BIC: 18944.06
poissonmodel1 <- glm(ntips ~ nreviews + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + compliment_personal + compliment_revtips + yelping_since_year,
                     data = yelp_1)
summary(poissonmodel1)
AIC(poissonmodel1)
BIC(poissonmodel1)

#AIC: 46605.3, BIC: 46678.11
poissonmodel2 <- glm(ntips ~ nreviews + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + compliment_personal + compliment_revtips + yelping_since_year,
                     data = yelp_1, family = "poisson") 
summary(poissonmodel2)
AIC(poissonmodel2)
BIC(poissonmodel2)

#AIC: 19520.32, BIC: 19531.52
poissonmodel3 <- glm(ntips ~ 1, data = yelp_1 ) 
summary(poissonmodel3)
AIC(poissonmodel3)
BIC(poissonmodel3)


#AIC:61788.82; 61794.42
poissonmodel4 <- glm(ntips ~ 1, data = yelp_1, family = "poisson") 
summary(poissonmodel4)
AIC(poissonmodel4)
BIC(poissonmodel4)

anova(poissonmodel1, poissonmodel3, test = "Chisq")
anova(poissonmodel2, poissonmodel4, test = "Chisq")

dispersiontest(poissonmodel2, trafo = 2) #significant meaning we have overdispersion and negative binomial model is needed

#deleted some variables with "0" as otherwise it didn't work
#AIC: 12160.92, BIC: 12228.13
poissonmodelNB <- glm.nb(ntips ~ factor(yelping_since_year) + nreviews + number_friends + average_stars + compliment_count_tips + review_length_med,
                         data = yelp_1) 

summary(poissonmodelNB)
AIC(poissonmodelNB)
BIC(poissonmodelNB)

library(VGAM)

#AIC:11160.11, BIC: 11238.53
TruncNBModel <- vglm(ntips ~ nreviews + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + compliment_revtips + compliment_personal + yelping_since_year,
                     family = posnegbinomial(), data = yelp_1)
summary(TruncNBModel)
exp(coef(TruncNBModel))
AIC(TruncNBModel)
BIC(TruncNBModel)


#AIC: 11149.14, BIC: 11233.15
TruncNBModel10 <- vglm(ntips ~ nreviews + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + compliment_revtips + compliment_personal + yelping_since_year + tips_length_med,
                       family = posnegbinomial(), data = yelp_1)
summary(TruncNBModel10)
exp(coef(TruncNBModel10))
AIC(TruncNBModel10)
BIC(TruncNBModel10)

#AIC: 7327.256,  BIC: 7436.005
TruncNBModel11 <- vglm(ntips ~ yelping_since_year + useful + funny + cool + elite + nyears_elite + tips_length_med + number_friends + fans + average_stars + compliment_revtips + compliment_personal + postips + negtips + avg_rest_stars + compliment_count_tips + firsttip + secondtip + lasttip,
                       family = posnegbinomial(), data = yelp_1)

summary(TruncNBModel11)
AIC(TruncNBModel11)
BIC(TruncNBModel11)
exp(coef(TruncNBModel11))

#AIC: 7317.688,  BIC: 7452.33
TruncNBModel12 <- vglm(ntips ~ yelping_since_year + useful + funny + cool + elite + nyears_elite + nreviews + review_length_med + tips_length_med + number_friends + fans + average_stars + compliment_revtips + compliment_personal + postips + negtips + avg_rest_stars + compliment_count_tips + firsttip + secondtip + lasttip + duration + highstars + lowstars,
                       family = posnegbinomial(), data = yelp_1)
summary(TruncNBModel12)
AIC(TruncNBModel12)
BIC(TruncNBModel12)

#AIC: 7312.327,  BIC: 7405.54
TruncNBModel13 <- vglm(ntips ~ yelping_since_year + funny + cool  + nreviews + tips_length_med + number_friends + compliment_personal + postips + negtips + avg_rest_stars + compliment_count_tips + firsttip + secondtip + lasttip + duration,
                       family = posnegbinomial(), data = yelp_1)
summary(TruncNBModel13)
AIC(TruncNBModel13)
BIC(TruncNBModel13)

#AIC: 7312.485,  BIC: 7395.342
TruncNBModel14 <- vglm(ntips ~ + funny + cool + nyears_elite + nreviews + tips_length_med + number_friends + compliment_personal + postips + negtips + compliment_count_tips + firsttip + secondtip + lasttip + duration,
                       family = posnegbinomial(), data = yelp_1)
summary(TruncNBModel14)
AIC(TruncNBModel14)
BIC(TruncNBModel14)

#AIC: 8103.285,  BIC: 8165.428
TruncNBModel15 <- vglm(ntips ~  nyears_elite + nreviews + tips_length_med + number_friends + compliment_personal + compliment_revtips + firsttip + secondtip + lasttip + duration,
                       family = posnegbinomial(), data = yelp_1)
summary(TruncNBModel15)
AIC(TruncNBModel15)
BIC(TruncNBModel15)
exp(coef(TruncNBModel15))


##question 4 - nr reviews
mean(yelp_1$nreviews) #mean < 10 - using poisson model
var(yelp_1$nreviews)

ggplot(yelp_1, aes(x=nreviews)) + geom_histogram(binwidth=1) + scale_x_continuous(breaks = seq(0, 100, 1)) + xlim(0, 100)


#AIC: 13931.12, BIC: 14003.93
poissonmodel22 <- glm(nreviews ~ ntips + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + compliment_revtips + compliment_personal + yelping_since_year,
                      data = yelp_1, family = "poisson") 
summary(poissonmodel22)
AIC(poissonmodel22)
BIC(poissonmodel22)

#AIC: 19465.13, BIC: 19470.73
poissonmodel44 <- glm(nreviews ~ 1, data = yelp_1, family = "poisson") 
summary(poissonmodel44)
AIC(poissonmodel44)
BIC(poissonmodel44)

anova(poissonmodel22, poissonmodel44, test = "Chisq")


dispersiontest(poissonmodel22, trafo = 2)#significant meaning we have overdispersion and negative binomial model is needed

#AIC: 9738.269, BIC: 9805.48
poissonmodelNB1 <- glm.nb(nreviews ~ factor(yelping_since_year) + ntips + number_friends + average_stars + compliment_count_tips + review_length_med,
                          data = yelp_1) 
summary(poissonmodelNB1)
AIC(poissonmodelNB1)
BIC(poissonmodelNB1)

library(VGAM)

#AIC:7964.264, BIC: 8042.677   
TruncNBModel1 <- vglm(nreviews ~ ntips + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + compliment_revtips + compliment_personal + yelping_since_year,
                      family = posnegbinomial(), data = yelp_1)
summary(TruncNBModel1)
exp(coef(TruncNBModel1))
AIC(TruncNBModel1)
BIC(TruncNBModel1)


#AIC:7898.098, BIC: 7982.111
TruncNBModel2 <- vglm(nreviews ~ ntips + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + compliment_revtips + compliment_personal + yelping_since_year + first_elite,
                      family = posnegbinomial(), data = yelp_1)
summary(TruncNBModel2)
exp(coef(TruncNBModel2))
AIC(TruncNBModel2)
BIC(TruncNBModel2)

#AIC:7933.969, BIC: 7989.978
TruncNBModel3 <- vglm(nreviews ~ ntips + useful + number_friends + fans + compliment_count_tips + compliment_revtips + yelping_since_year + first_elite,
                      family = posnegbinomial(), data = yelp_1)
summary(TruncNBModel3)
AIC(TruncNBModel3)
BIC(TruncNBModel3)

#AIC:5330.096, BIC: 5420.62
TruncNBModel4 <- vglm(nreviews ~ nyears_elite + ntips + tips_length_med + useful + number_friends + fans + compliment_personal + compliment_count_tips + compliment_revtips + yelping_since_year + first_elite + postips + negtips +firstrev + secondrev + lastrev,
                      family = posnegbinomial(), data = yelp_1)
summary(TruncNBModel4)
AIC(TruncNBModel4)
BIC(TruncNBModel4)

#AIC:4185.368, BIC: 4284.82
TruncNBModel5 <- vglm(nreviews ~ nyears_elite + ntips + tips_length_med + useful + number_friends + compliment_personal + compliment_count_tips + compliment_revtips + yelping_since_year + first_elite + postips + negtips +firstrev + secondrev + lastrev + firsttip + secondtip + lasttip + duration,
                      family = posnegbinomial(), data = yelp_1)
summary(TruncNBModel5)
AIC(TruncNBModel5)
BIC(TruncNBModel5)

#AIC:4190.067, BIC: 4261.104
TruncNBModel6 <- vglm(nreviews ~ useful + number_friends + compliment_count_tips + compliment_revtips + yelping_since_year + first_elite + firstrev + secondrev + lastrev + firsttip + secondtip + lasttip + duration,
                      family = posnegbinomial(), data = yelp_1)
summary(TruncNBModel6)
AIC(TruncNBModel6)
BIC(TruncNBModel6)
exp(coef(TruncNBModel6))

####################Question 5#####################

library(dplyr)
library(cleaner)


yelp_1$secondrev2 <- yelp_1$secondrev2 %>%
  na.replace(yelp_1$secondrev2,'0')

  
yelp_1$secondrev2[is.na(yelp_1$secondrev2)] <- "2018-11-13"
yelp_1$secondrev2 <- as.numeric(yelp_1$secondrev)



yelp_1$firstrev2 <- as.numeric(yelp_1$firstrev)

yelp_1$IsSecRev[!(is.na(yelp_1$secondrev))] <- 1
yelp_1$IsSecRev[(is.na(yelp_1$secondrev))] <- 0

yelp_1$differenceRev2 <- (yelp_1$secondrev2 - yelp_1$firstrev2)

#Checking for the cox model, DV -> differenceRev
KM_SurvivalCurve3 <- survfit(Surv(differenceRev, IsSecRev)~ 1, data=yelp_1)
ggsurvplot(KM_SurvivalCurve3, data=yelp_1)
summary(KM_SurvivalCurve3)


CoxModel10 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews 
                    + average_stars + compliment_personal + compliment_revtips
                    + review_length_med + yelping_since_year,
                    method = "efron", data = yelp_1)
summary(CoxModel10) # Concordance= 0.64  (se = 0.007 )
exp(coef(CoxModel10))
AIC(CoxModel10) # 26244.76
BIC(CoxModel10) # 26272.76

CoxModel10.1 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews 
                      + average_stars + compliment_personal + compliment_revtips
                      + ntips+ yelping_since_year,
                      method = "efron", data = yelp_1)
summary(CoxModel10.1) # Concordance= 0.638  (se = 0.007 )
exp(coef(CoxModel10.1))
AIC(CoxModel10.1) # 26212.99
BIC(CoxModel10.1) # 26212.99


CoxModel10.2 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + elite +
                        compliment_personal + compliment_revtips
                      + review_length_med + ntips + yelping_since_year,
                      method = "efron", data = yelp_1)
summary(CoxModel10.2) # Concordance= 0.619  (se = 0.007 )
exp(coef(CoxModel10.2))
AIC(CoxModel10.2) # 26188
BIC(CoxModel10.2) # 26222

CoxModel10.3 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews 
                      + average_stars + ntips + compliment_personal + compliment_revtips
                      + review_length_med,
                      method = "efron", data = yelp_1)
summary(CoxModel10.3) # Concordance= 0.671  (se = 0.006 )
exp(coef(CoxModel10.3))
AIC(CoxModel10.3) # 26230.86
BIC(CoxModel10.3) # 26258.86

CoxModel10.4 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews 
                      + average_stars + compliment_personal + compliment_revtips
                      + review_length_med + yelping_since_year,
                      method = "efron", data = yelp_1)
summary(CoxModel10.4) # Concordance= 0.64  (se = 0.007 )
exp(coef(CoxModel10.4))
AIC(CoxModel10.4) # 26244.76
BIC(CoxModel10.4) # 26272.76

CoxModel10.5 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews 
                      + average_stars + compliment_revtips + compliment_personal
                      + ntips + yelping_since_year,
                      method = "efron", data = yelp_1)
summary(CoxModel10.5) 
exp(coef(CoxModel10.5))
AIC(CoxModel10.5) 
BIC(CoxModel10.5) 

CoxModel10.6 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews 
                      + elite + 
                        + compliment_revtips + compliment_personal + review_length_med + yelping_since_year,
                      method = "efron", data = yelp_1)
summary(CoxModel10.6)
exp(coef(CoxModel10.6))
AIC(CoxModel10.6) 
BIC(CoxModel10.6) 

CoxModel10.7 <- coxph(Surv(differenceRev, IsSecRev) ~ elite
                      + highstars + lowstars + ntips + compliment_revtips + compliment_personal 
                      + review_length_med,
                      method = "efron", data = yelp_1)
summary(CoxModel10.7) 
exp(coef(CoxModel10.7))
AIC(CoxModel10.7) 
BIC(CoxModel10.7) 


CoxModel10.8 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + elite
                      + lowstars + highstars + compliment_revtips + compliment_personal
                      + yelping_since_year,
                      method = "efron", data = yelp_1)
summary(CoxModel10.8) 
exp(coef(CoxModel10.8))
AIC(CoxModel10.8) 
BIC(CoxModel10.8)

CoxModel10.9 <- coxph(Surv(differenceRev, IsSecRev) ~ elite
                      + highstars + lowstars + compliment_revtips + compliment_personal + 
                        + review_length_med + yelping_since_year,
                      method = "efron", data = yelp_1)
summary(CoxModel10.9) 
exp(coef(CoxModel10.9))
AIC(CoxModel10.9) 
BIC(CoxModel10.9) 

#baseline hazard
basehazyelp_1 <- basehaz(CoxModel10.2, centered=TRUE)
basehazyelp_1
plot(basehazyelp_1)

AIC(CoxModel10,
    CoxModel10.1,
    CoxModel10.2,
    CoxModel10.3,
    CoxModel10.4,
    CoxModel10.5,
    CoxModel10.6,
    CoxModel10.7,
    CoxModel10.8,
    CoxModel10.9)

BIC(CoxModel10,
    CoxModel10.1,
    CoxModel10.2,
    CoxModel10.3,
    CoxModel10.4,
    CoxModel10.5,
    CoxModel10.6,
    CoxModel10.7,
    CoxModel10.8,
    CoxModel10.9)

## Tips

yelp_1$IsSecTip <- 0
yelp_1$IsSecTip[!(is.na(yelp_1$secondtip))] <- 1
yelp_1$IsSecTip[(is.na(yelp_1$secondtip))] <- 0

yelp_1$secondtip[is.na(yelp_1$secondtip)] <- "2018-10-30"
yelp_1$differenceTip <- (yelp_1$secondtip - yelp_1$firsttip)


#Checking for the cox model, DV -> differenceRev
KM_SurvivalCurve4 <- survfit(Surv(differenceTip, IsSecTip)~ 1, data=yelp_1)
ggsurvplot(KM_SurvivalCurve4, data=yelp_1)
summary(KM_SurvivalCurve4)

CoxModel11 <- coxph(Surv(differenceTip, IsSecTip) ~ nreviews 
                    + average_stars + totalcompliments 
                    + review_length_med 
                    + yelping_since_year,
                    method = "efron", data = yelp_1)

summary(CoxModel11) # 0.572  (se = 0.008 )
exp(coef(CoxModel11))
AIC(CoxModel11) # 18532.99
BIC(CoxModel11) # 18564.06

CoxModel11.1 <- coxph(Surv(differenceTip, IsSecTip) ~ nreviews
                      + average_stars + compliment_revtips + compliment_personal 
                      + review_length_med + yelping_since_year,
                      method = "efron", data = yelp_1)

summary(CoxModel11.1) # Concordance= 0.57  (se = 0.008 )
exp(coef(CoxModel11.1))
AIC(CoxModel11.1) # 18533.22
BIC(CoxModel11.1) # 18569.47

CoxModel11.2 <- coxph(Surv(differenceTip, IsSecTip) ~ ntips
                      + lowstars + highstars + compliment_revtips + compliment_personal
                      + review_length_med + yelp_1ing_since_year,
                      method = "efron", data = yelp_1)

summary(CoxModel11.2) # Concordance= 0.626  (se = 0.008 )
exp(coef(CoxModel11.2))
AIC(CoxModel11.2) # 18469.53
BIC(CoxModel11.2) # 18505.78

CoxModel11.3 <- coxph(Surv(differenceTip, IsSecTip) ~ ntips +
                        + average_stars + compliment_revtips + compliment_personal
                      + review_length_med + elite +
                        + yelping_since_year,
                      method = "efron", data = yelp_1)

summary(CoxModel11.3) # Concordance=0.62  (se = 0.008 )
exp(coef(CoxModel11.3))
AIC(CoxModel11.3) # 18442.66
BIC(CoxModel11.3) # 18478.91

CoxModel11.4 <- coxph(Surv(differenceTip, IsSecTip) ~ ntips
                      + average_stars + compliment_revtips + compliment_personal
                      + elite + yelping_since_year,
                      method = "efron", data = yelp_1)

summary(CoxModel11.4) # Concordance= 0.625  (se = 0.008 )
exp(coef(CoxModel11.4))
AIC(CoxModel11.4) # 18467.53
BIC(CoxModel11.4) # 18503.78

CoxModel11.5 <- coxph(Surv(differenceTip, IsSecTip) ~ ntips 
                      + average_stars + nreviews 
                      + review_length_med 
                      + yelping_since_year,
                      method = "efron", data = yelp_1)

summary(CoxModel11.5) # Concordance= 0.627  (se = 0.008 )
exp(coef(CoxModel11.5))
AIC(CoxModel11.5) # 18470.63
BIC(CoxModel11.5) # 18506.88

CoxModel11.6 <- coxph(Surv(differenceTip, IsSecTip) ~ ntips
                      + highstars + lowstars + compliment_revtips + compliment_personal
                      + review_length_med  
                      + yelping_since_year,
                      method = "efron", data = yelp_1)

summary(CoxModel11.6) # Concordance= 0.628  (se = 0.008 )
exp(coef(CoxModel11.6))
AIC(CoxModel11.6) # 18469.05
BIC(CoxModel11.6) # 18505.3

CoxModel11.7 <- coxph(Surv(differenceTip, IsSecTip) ~ ntips + nreviews + elite +
                        + average_stars + compliment_revtips + compliment_personal,
                      method = "efron", data = yelp_1)

summary(CoxModel11.7) # Concordance= 0.621  (se = 0.008 )
exp(coef(CoxModel11.7))
AIC(CoxModel11.7) # 18423.92
BIC(CoxModel11.7) # 18460.17

CoxModel11.8 <- coxph(Surv(differenceTip, IsSecTip) ~ ntips + nreviews + elite +
                        compliment_revtips + compliment_personal +  yelping_since_year,
                      method = "efron", data = yelp_1)

summary(CoxModel11.8) # Concordance=  0.622  (se = 0.008 )
exp(coef(CoxModel11.8))
AIC(CoxModel11.8) # 18424.61
BIC(CoxModel11.8) # 18460.86

CoxModel11.9 <- coxph(Surv(differenceTip, IsSecTip) ~ ntips + nreviews
                      + average_stars + elite 
                      + yelping_since_year,
                      method = "efron", data = yelp_1)

summary(CoxModel11.9) # Concordance= 0.619  (se = 0.008 )
exp(coef(CoxModel11.9))
AIC(CoxModel11.9) # 18433.89
BIC(CoxModel11.9) # 18459.79


AIC(CoxModel11,
    CoxModel11.1,
    CoxModel11.2,
    CoxModel11.3,
    CoxModel11.4,
    CoxModel11.5,
    CoxModel11.6,
    CoxModel11.7,
    CoxModel11.8,
    CoxModel11.9)

BIC(CoxModel11,
    CoxModel11.1,
    CoxModel11.2,
    CoxModel11.3,
    CoxModel11.4,
    CoxModel11.5,
    CoxModel11.6,
    CoxModel11.7,
    CoxModel11.8,
    CoxModel11.9)


#baseline hazard
basehazyelp_1 <- basehaz(CoxModel11.9, centered=TRUE)
basehazyelp_1
plot(basehazyelp_1)

--------------------------------------------------------
#Question nr 5 some models:
  CoxModel10 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + compliment_revtips + compliment_personal + yelping_since_year,
                      method = "efron", data = yelp_1)
summary(CoxModel10)


CoxModel12 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + compliment_revtips + compliment_personal + yelping_since_year + first_elite + nyears_elite + compliment_hot,
                    method = "efron", data = yelp_1)

CoxModel13 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + compliment_revtips + compliment_personal + yelping_since_year + first_elite + nyears_elite + negreviews + posreviews,
                    method = "efron", data = yelp_1)

CoxModel14 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + compliment_revtips + compliment_personal + yelping_since_year + first_elite + nyears_elite + negreviews + posreviews + avg_rest_stars_rev,
                    method = "efron", data = yelp_1)

CoxModel15 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + yelping_since_year + first_elite + nyears_elite + negreviews + posreviews,
                    method = "efron", data = yelp_1)

CoxModel16 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + yelp_1ing_since_year + first_elite + nyears_elite + negreviews + posreviews,
                    method = "efron", data = yelp_1)

CoxModel17 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + funny + cool + fans + average_stars + compliment_count_tips + review_length_med + yelping_since_year + first_elite + nyears_elite + negreviews + posreviews,
                    method = "efron", data = yelp_1)
CoxModel18 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + funny + cool + fans + compliment_count_tips + review_length_med + yelping_since_year + first_elite + nyears_elite + negreviews + posreviews + lowstars + highstars,
                    method = "efron", data = yelp_1)

CoxModel19 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + funny + cool + fans + compliment_count_tips  + yelping_since_year + first_elite + negreviews + posreviews + lowstars + highstars,
                    method = "efron", data = yelp_1)

CoxModel10 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + funny + cool + fans + compliment_count_tips  + yelping_since_year + first_elite + posreviews + lowstars + highstars,
                    method = "efron", data = yelp_1)
CoxModel101 <- coxph(Surv(differenceRev, IsSecRev) ~ nreviews + funny + cool + fans + compliment_count_tips  + yelping_since_year + first_elite + posreviews + review_length_med,
                     method = "efron", data = yelp_1)

AIC(CoxModel10,CoxModel11, CoxModel12, CoxModel13, CoxModel14)
BIC(CoxModel10,CoxModel11, CoxModel12, CoxModel13, CoxModel14)
AIC(CoxModel13, CoxModel15, CoxModel16, CoxModel17)
BIC(CoxModel13, CoxModel15, CoxModel16, CoxModel17)
AIC(CoxModel17, CoxModel18, CoxModel101, CoxModel10)
BIC(CoxModel17, CoxModel18, CoxModel101, CoxModel10)

#cox model for tips


yelp_1$IsSecTip[!(is.na(yelp_1$secondtip))] <- 1
yelp_1$IsSecTip[(is.na(yelp_1$secondtip))] <- 0

yelp_1$secondtip[is.na(yelp_1$secondtip)] <- "2018-10-30"
yelp_1$differenceTip <- (yelp_1$secondtip - yelp_1$firsttip)

CoxModel20 <- coxph(Surv(differenceTip, IsSecTip) ~ nreviews + useful + funny + cool + number_friends + fans + average_stars + compliment_count_tips + review_length_med + compliment_revtips + compliment_personal + yelping_since_year,
                    method = "efron", data = yelp_1)


CoxModel21 <- coxph(Surv(differenceTip, IsSecTip) ~ nreviews + funny + cool + fans + compliment_count_tips  + yelping_since_year + first_elite + posreviews,
                    method = "efron", data = yelp_1)

CoxModel22 <- coxph(Surv(differenceTip, IsSecTip) ~ nreviews + funny + cool + fans + compliment_count_tips  + yelping_since_year + first_elite + posreviews + average_stars +compliment_revtips + compliment_personal,
                    method = "efron", data = yelp_1)

CoxModel23 <- coxph(Surv(differenceTip, IsSecTip) ~ nreviews + funny + cool + fans + compliment_count_tips  + yelping_since_year + first_elite + posreviews + average_stars +compliment_revtips + compliment_personal + tips_length_med,
                    method = "efron", data = yelp_1)

AIC(CoxModel20, CoxModel21, CoxModel22, CoxModel23)
BIC(CoxModel20, CoxModel21, CoxModel22, CoxModel23)
