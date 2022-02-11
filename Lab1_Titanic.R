library(pscl)
library(lmtest) 	
library(dominanceanalysis) 	
library(tidyverse)
library(car)

titanic_original <- read.csv("Titanic - training set.csv")

titanic <- titanic_original

titanic <- titanic %>%
  mutate(Survived = factor(recode(Survived,	
                             "0" = "No",	
                             "1" = "Yes")),
         Pclass = factor(recode(Pclass,
                              "1" = "1st class",
                              "2" = "2nd class",
                              "3" = "3rd class")),
         Sex = factor(recode(Sex,
                              "0" = "male",
                              "1" = "female")))

titanic$PassengerId <- as.factor(titanic$PassengerId)
titanic$SibSp <- as.numeric(titanic$SibSp)
titanic$Parch <- as.numeric(titanic$Parch)
titanic$Cabin <- as.factor(titanic$Cabin)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)





# PLOT THE DATA

ggplot(titanic, aes(factor(Survived), fill=factor(Sex))) +
  geom_bar()

ggplot(titanic, aes(x=Fare, y=Survived)) +
  geom_jitter(aes(colour = factor(Sex)))

#There three people paying much more than others
# might possibly be outliers

ggplot(titanic, aes(x=SibSp, y=Survived)) +
  geom_point() +
  geom_jitter(width = 0.3)

# People with 8 SibSp did not survive, none of them

titanic %>%
  filter(Sex == "male") %>%
  ggplot(aes(x=Age, y=Survived)) +
  geom_jitter(aes(colour = factor(Pclass)))


# Possible outliers:

titanic %>%
  filter(Fare < 500 & Fare > 0) %>% 
  ggplot(aes(x=Fare, y=Sex)) +
  geom_jitter(aes(colour = factor(Survived)))

titanic %>%
  filter(SibSp < 8) %>% 
  ggplot(aes(x=SibSp, y=Sex)) +
  geom_jitter(aes(colour = factor(Survived)))


# get rid of NAs...

titanic_noNA <- drop_na(titanic)

# ... and potential outliers
# these are Fares with value of 0 or nearly 500, and
# 8 SibSp

titanic_noOut <- titanic_noNA %>%
  filter(Fare > 0 & Fare < 500) %>% 
  filter(SibSp < 8)




#CREATE KATE

#Let's create Kate with and without Leonardo

# is Kate in the dataset or we have to make her up?

titanic %>% 
  filter(grepl('Kat', Name) & Pclass == 3) 

# She's not there -> create new variable

Kate_Sue <- data.frame(row.names = c("Kate_without_L", "Kate_with_L", 
                                "Sue_without_L", "Sue_with_L"),
                   Pclass = "3",
                   Sex = "female",
                   Age = c(20, 20, 4, 4),
                   SibSp = c(0, 1, 0, 0),
                   Parch = c(1, 1, 1, 2),
                   Fare = 8,
                   Embarked = "S")

Kate_Sue$Pclass <- as.factor(Kate_Sue$Pclass) 
Kate_Sue$Sex <- as.factor(Kate_Sue$Sex) 
Kate_Sue$Embarked <- as.factor(Kate_Sue$Embarked)









# MODEL 1 WITH POTENTIAL OUTLIERS

mod1a = glm(Survived ~ Fare +
             Sex * Age +
             Parch +
             SibSp +
             Pclass, 	
           family = binomial(), data = titanic_noNA)

summary(mod1a)	

pR2(mod1a)	

vif(mod1a)


titanic_noNA = titanic_noNA %>% 	
  mutate(pred_mod1a = predict(mod1a)) %>% 	
  mutate(pred_mod1a = case_when(pred_mod1a <= 0 ~ "0",	
                               pred_mod1a > 0 ~ "1"))

# coding correct guesses	

titanic_noNA = titanic_noNA %>%	
  mutate(correct_prediction1a = case_when(
    pred_mod1a == Survived ~ "correct",
    pred_mod1a != Survived ~ "incorrect"))	

Prediction_1a <- titanic_noNA %>%	
  group_by(correct_prediction1a) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	


# correctly categorized as surviving

Prediction_1a_alive <- titanic_noNA %>%	
  filter(Survived == "1") %>% 	
  group_by(correct_prediction1a) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	

# correctly categorized as having not surviving	

Prediction_1a_dead <- titanic_noNA %>%	
  filter(Survived == "0") %>% 	
  group_by(correct_prediction1a) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	

Prediction_1a_Kate_Sue <- predict(mod1a, Kate_Sue, type = "response")


Prediction_1a
Prediction_1a_alive
Prediction_1a_dead
Prediction_1a_Kate_Sue







# MODEL 1 WITHOUT POTENTIAL OUTLIERS


mod1b = glm(Survived ~ Fare +
             Sex * Age +
             Parch +
             SibSp +
             Pclass, 	
           family = binomial(), data = titanic_noOut)

summary(mod1b)	

pR2(mod1b)	

vif(mod1b)

titanic_noOut = titanic_noOut %>% 	
  mutate(pred_mod1b = predict(mod1b)) %>% 	
  mutate(pred_mod1b = case_when(pred_mod1b <= 0 ~ "0",	
                               pred_mod1b > 0 ~ "1"))

titanic_noOut = titanic_noOut %>%	
  mutate(correct_prediction1b = case_when(
    pred_mod1b == Survived ~ "correct",
    pred_mod1b != Survived ~ "incorrect"))	

Prediction_1b <- titanic_noOut %>%	
  group_by(correct_prediction1b) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	

Prediction_1b_alive <- titanic_noOut %>%	
  filter(Survived == "1") %>% 	
  group_by(correct_prediction1b) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	

Prediction_1b_dead <- titanic_noOut %>%	
  filter(Survived == "0") %>% 	
  group_by(correct_prediction1b) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	

Prediction_1b_Kate_Sue <- predict(mod1b, Kate_Sue, type = "response")

Prediction_1b
Prediction_1b_alive
Prediction_1b_dead
Prediction_1b_Kate_Sue











# MODEL 2 WITH POTENTIAL OUTLIERS

mod2a = glm(Survived ~ Fare +
             Age +
             Parch +
             SibSp +
             Pclass * Sex, 	
           family = binomial(), data = titanic_noNA)

pR2(mod2a)	

vif(mod2a)

titanic_noNA = titanic_noNA %>% 	
  mutate(pred_mod2a = predict(mod2a)) %>% 	
  mutate(pred_mod2a = case_when(pred_mod2a <= -1 ~ "0",	
                               pred_mod2a > -1 ~ "1"))

titanic_noNA = titanic_noNA %>%	
  mutate(correct_prediction2a = case_when(
    pred_mod2a == Survived ~ "correct",
    pred_mod2a != Survived ~ "incorrect"))

Prediction_2a <- titanic_noNA %>%	
  group_by(correct_prediction2a) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))

Prediction_2a_alive <- titanic_noNA %>%	
  filter(Survived == "1") %>% 	
  group_by(correct_prediction2a) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	

Prediction_2a_dead <- titanic_noNA %>%	
  filter(Survived == "0") %>% 	
  group_by(correct_prediction2a) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))

Prediction_2a_Kate_Sue <- predict(mod2a, Kate_Sue, type = "response")

Prediction_2a
Prediction_2a_alive
Prediction_2a_dead
Prediction_2a_Kate_Sue








# MODEL 2 WITHOUT POTENTIAL OUTLIERS

mod2b = glm(Survived ~ Fare +
             Age +
             Parch +
             SibSp +
             Pclass * Sex, 	
           family = binomial(), data = titanic_noOut)

summary(mod2b)	

pR2(mod2b)	

vif(mod2b)

titanic_noOut = titanic_noOut %>% 	
  mutate(pred_mod2b = predict(mod2b)) %>% 	
  mutate(pred_mod2b = case_when(pred_mod2b <= 0 ~ "0",	
                                pred_mod2b > 0 ~ "1"))

titanic_noOut = titanic_noOut %>%	
  mutate(correct_prediction2b = case_when(
    pred_mod2b == Survived ~ "correct",
    pred_mod2b != Survived ~ "incorrect"))	

Prediction_2b <- titanic_noOut %>%	
  group_by(correct_prediction2b) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	

Prediction_2b_alive <- titanic_noOut %>%	
  filter(Survived == "1") %>% 	
  group_by(correct_prediction2b) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	

Prediction_2b_dead <- titanic_noOut %>%	
  filter(Survived == "0") %>% 	
  group_by(correct_prediction2b) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))	

Prediction_2b_Kate_Sue <- predict(mod2b, Kate_Sue, type = "response")

Prediction_2b
Prediction_2b_alive
Prediction_2b_dead
Prediction_2b_Kate_Sue
