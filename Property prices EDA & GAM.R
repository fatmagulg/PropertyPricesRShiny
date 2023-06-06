##### Assignment 1 - House prices #####
#### Questions of interest ####
### Investigate relationship between property prices and different property characteristics
# Analyse using a generalized linear model
#1. How do prices vary with the size of the house? 
#2. Do people pay more for energy efficient homes? 
#3. What are the most affordable and most expensive areas of the county? 

##### Libraries & packages ####
library(dplyr)
library(mgcv) # for gam
library(GGally) # for ggpairs
library(broom) # for glance
library(gridExtra)


#### Set up ####
setwd("~/Desktop/y4/Advanced Data Analysis/Lab 4:Poster assignment")
#please set your working directory accordingly
wrexham.full <- read.csv("WREXHAM.csv")
# subset the data by only taking interesting variables
wrexham <- wrexham.full %>%
  dplyr::select(price, propertytype, oldnew, duration, towncity,
         year, tfarea, numberrooms, CURRENT_ENERGY_RATING, BUILT_FORM)
# it has been reduced to 10 variables


#### Exploratory plots ####
p1 <- ggplot(data = wrexham, aes(x = numberrooms, y = price)) +
  geom_bar(stat = 'identity') +
  labs(x = "Number of rooms", y = "Property price")
p2 <- ggplot(data = wrexham, aes(x = CURRENT_ENERGY_RATING, y = price)) +
  geom_boxplot() +
  labs(x = "Energy rating", y = "")
p3 <- ggplot(data = wrexham, aes(x = towncity, y = price)) +
  geom_boxplot() +
  labs(x = "Location", y = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(p1, p2, p3, nrow = 1, ncol = 3)



#### Simple linear regression models ####
## Fit a simple linear regression model with all the variables to start off
model1 <- lm(data = wrexham, price ~ . )
summary(model1)
# significant variables: propertytype, towncityOSWESTRY, year, tfarea, numerrooms, energy rating, built form
# remove oldnew and duration, keeping towncity even though it is not significant bc of question 3

model2 <- lm(data = wrexham, price ~ .-oldnew-duration )
summary(model2)

glance(model1); glance(model2)
# AIC of model 2 is slightly lower, Rsq is the same
# model 2 aka the reduced model is favourable 


#### GLMs: gaussian, gamma log, gamma inverse ####
# write model2 out again this time using gam() so that i can use gam.check for it
glm2 <- gam(price ~ propertytype+ towncity+ year+ tfarea+ numberrooms+ CURRENT_ENERGY_RATING+ 
           BUILT_FORM, family = gaussian(link = "identity"), data = wrexham)
par(mfrow = c(2,2))
gam.check(glm2)
# q-q plot shows some trend in the residuals, histogram is fine
# resids vs linear predictor - looks like variance is increasing

# Gamma model with log link
glm3 <- gam(price ~ propertytype+ towncity+ year+ tfarea+ numberrooms+ CURRENT_ENERGY_RATING+ 
             BUILT_FORM, family = Gamma(link = "log"), data = wrexham)
gam.check(glm3)
# q-q plot is better behaved, histogram is good
# resids vs linear predictor - variance looks a bit more constant
# response vs fitted values follows decently y=x

# Gamma model with inverse link
glm4 <- gam(price ~ propertytype+ towncity+ year+ tfarea+ numberrooms+ CURRENT_ENERGY_RATING+ 
             BUILT_FORM, family = Gamma(link = "inverse"), data = wrexham)
gam.check(glm4)
# q-q, histogram and resids vs predictor are well behaved.
# response vs fitted values is completely off 


#### AIC Model comparison ####
AIC(glm2) ; AIC(glm3) ; AIC(glm4)
# 2 > 4 > 3
### choose model 3



#### Multicollinearity check ####
wrexham %>%
  dplyr::select(!c(oldnew,duration)) %>% #remove the variables that werent signift and take out the response too
  ggpairs()
# looks like tfarea and numberrooms are highly correlated (not surprising)
# correlation = 0.774

# Fit a model with only tfarea
glm5 <- gam(price ~ propertytype+ towncity+ year+ tfarea+ CURRENT_ENERGY_RATING+ 
              BUILT_FORM, family = Gamma(link = "log"), data = wrexham)
gam.check(glm5)


# Fit a model with only numberrooms
glm6 <- gam(price ~ propertytype+ towncity+ year+ numberrooms+ CURRENT_ENERGY_RATING+ 
              BUILT_FORM, family = Gamma(link = "log"), data = wrexham)
gam.check(glm6)


# glm3: both / glm5: tfarea only / glm6: numberrooms only
AIC(glm3) ; AIC(glm5) ; AIC(glm6)
## select glm6 as the final model (lowest AIC)


#### Deviance test ####
# again write glm6 using glm() rather than gam() so the summary shows the residual deviance
glm6 <- glm(price ~ propertytype+ towncity+ year+ numberrooms+ CURRENT_ENERGY_RATING+ 
              BUILT_FORM, family = Gamma(link = "log"), data = wrexham)
summary(glm6)
#Residual deviance: 811.8  on 10375 degrees of freedom
qchisq(0.95, 10375) # = 10613.07
# the deviance is much smaller than the chisq percentile => no evidence of lack of fit




#### Answering questions of interest #####
# The final model i selected was model #6 ie the Gamma model with a log link and collinear 
# variables removed
# Answer the questions by interpreting the model summary
summary(glm6)

## Q1: How do prices vary with size of house?
# The parameter estimate for numberrooms is 0.143027 ie for each extra room a house has
# the price increases multiplicatively by exp(0.143027) = 1.153761
# ie ≈15% increase per room
exp(0.143027)

## Q2: Do people pay more energy efficient homes
# The baseline for the factor variables was energy rating A 
# Coefficient for energy rating B = 0.148702
exp(0.148702)
# Price of house with rating B is exp(0.148702) = 1.160327 times greater than an energy 
# rating A house
# etc..
# The estimate for the variables corresponding to the different energy ratings do not 
# appear significant however these were included in the final model in order to answer 
# the question of interest


## Q3: What are the most affordable and most expensive areas of the county?
# baseline town is Chester 
exp(-0.511773)
# Houses in Mold are on average exp(-0.511773) = 0.5994318 times the price of a house
# in Chester ie about 40% lower in price 
exp(0.425811)
# Houses in Oswestry are on average exp(0.425811) = 1.530831 times the price of a house
# in Chester ie about 53% higher in price
# However the effects of location do not appear significant in the model


#### Further remarks ####
# The terms in the model corresponding to mid-terrace, end-terrace, and semi-detached 
# are significant. The baseline for the built-form variable is Detached
exp(-0.166197)
# End terrace houses are 0.8468794 times the price of a detached house
exp(-0.214241)
# Mid terrace houses are 0.8071538 times the price of a detached house
exp(-0.127790)
# Semi detached houses are 0.8800382 times the price of a detached house
# :. the model shows that detached houses are worth more on average than other build forms.



