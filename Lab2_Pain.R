# POSTOPERATIVE PAIN

#packages
library(psych)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)
library(pscl)
library(lmtest) 	
library(dominanceanalysis) 	
library(car)
library(brms)
library(tidyverse)



#data
surgery1_original <- read.csv("surgery_data_1.csv")
surgery2_original <- read.csv("surgery_data_2.csv")


#check the variables

str(surgery1_original)
str(surgery2_original)

summary(surgery1_original)
table(surgery1_original$sex)
table(surgery1_original$hospital)

summary(surgery2_original)
table(surgery2_original$sex)
table(surgery2_original$hospital)

surgery_train <- surgery1_original
surgery_test <- surgery2_original

#possible outliers are negative household income and "woman" in sex
# in training dataset

#get rid of outliers
surgery_train <- surgery_train %>% 
  mutate(sex = recode(sex, woman = "female")) %>% 
  filter(household_income > 0)

surgery_train <- surgery_train %>% 
  mutate(pain = as.numeric(pain),
         sex = as.factor(sex),
         age = as.numeric(age),
         STAI_trait = as.numeric(STAI_trait),
         pain_cat = as.numeric(pain_cat),
         IQ = as.numeric(IQ),
         household_income = as.numeric(household_income),
         hospital = as.factor(hospital))


surgery_test <- surgery_test %>% 
  mutate(pain = as.numeric(pain),
         sex = as.factor(sex),
         age = as.numeric(age),
         STAI_trait = as.numeric(STAI_trait),
         pain_cat = as.numeric(pain_cat),
         IQ = as.numeric(IQ),
         household_income = as.numeric(household_income),
         hospital = as.factor(hospital))



#ALL THE VARIABLES

#using training dataset
mod1_fixed = lm(pain ~ pain_cat +
                  age +
                  sex +
                  STAI_trait +
                  mindfulness +
                  cortisol_serum,
                data = surgery_train)

summary(mod1_fixed)


mod1_mixed = lmer(pain ~ pain_cat +
                  age +
                  sex +
                  STAI_trait +
                  mindfulness +
                  cortisol_serum +
                  (1 | hospital),
                data = surgery_train)

mod1_mixed
cAIC(mod1_mixed)
r.squaredGLMM(mod1_mixed)

#the most influential predictor seems to be the cortisol_serum in both models
#PLOT IT

surgery_train %>%
  ggplot() + 
  aes(y = pain, x = cortisol_serum) + 
  geom_point(aes(color = hospital), size = 2) + 
  geom_smooth(method = "lm", se = F)

surgery_train %>%
  ggplot() + 
  aes(y = pain, x = cortisol_serum, color = hospital) +
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = F, fullrange = TRUE)

#the second is quite a mess, but all the lines have more or less same slope


#using testing dataset

surgery_train_pred <- predict(mod1_mixed, 
                              surgery_train, 
                              type = "response")

surgery_test_pred <- predict(mod1_mixed, 
                             surgery_test, 
                             type = "response",
                             allow.new.levels = TRUE)

#residual sum of squares
RSS = sum((surgery_test$pain - surgery_test_pred)^2)
RSS

#total sum of squared differences
mod_mean <- lm(pain ~ 1, data = surgery_test)
TSS = sum((surgery_test$pain - predict(mod_mean))^2)
TSS

#r squared
R2 = 1 - (RSS/TSS)
R2


#ONLY THE MOST INFLUENTIAL VARIABLES
mod2_fixed = lm(pain ~ cortisol_serum,
                data = surgery_train)

summary(mod2_fixed)


mod2_mixed = lmer(pain ~ pain_cat +
                    (pain_cat | hospital),
                  control = lmerControl(optimizer = "Nelder_Mead"),
                  data = surgery_train) 

mod2_mixed
cAIC(mod2_mixed)
r.squaredGLMM(mod2_mixed)