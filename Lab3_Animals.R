library(GGally) # for ggcorr
library(corrr) # network_plot
library(ggcorrplot) # for ggcorrplot
library(FactoMineR) # multiple PCA functions
library(factoextra) # visualisation functions for PCA (e.g. fviz_pca_var)
library(paran) # for paran
library(psych) # for the mixedCor, cortest.bartlett, KMO, fa functions
library(car) # for vif
library(GPArotation) # for the psych fa function to have the required rotation functionalities
library(MVN) # for mvn function
library(ICS) # for multivariate skew and kurtosis test
library(tidyverse)

#custom function

fviz_loadnings_with_cor <- function(mod, axes = 1, loadings_above = 0.4){	
  require(factoextra)	
  require(dplyr)	
  require(ggplot2)	
  
  if(!is.na(as.character(mod$call$call)[1])){	
    if(as.character(mod$call$call)[1] == "PCA"){	
      contrib_and_cov = as.data.frame(rbind(mod[["var"]][["contrib"]], mod[["var"]][["cor"]]))	
      
      vars = rownames(mod[["var"]][["contrib"]])	
      attribute_type = rep(c("contribution","correlation"), each = length(vars))	
      contrib_and_cov = cbind(contrib_and_cov, attribute_type)	
      contrib_and_cov	
      
      plot_data = cbind(as.data.frame(cbind(contrib_and_cov[contrib_and_cov[,"attribute_type"] == "contribution",axes], contrib_and_cov[contrib_and_cov[,"attribute_type"] == "correlation",axes])), vars)	
      names(plot_data) = c("contribution", "correlation", "vars")	
      
      plot_data = plot_data %>% 	
        mutate(correlation = round(correlation, 2))	
      
      plot = plot_data %>% 	
        ggplot() +	
        aes(x = reorder(vars, contribution), y = contribution, gradient = correlation, label = correlation)+	
        geom_col(aes(fill = correlation)) +	
        geom_hline(yintercept = mean(plot_data$contribution), col = "red", lty = "dashed") + scale_fill_gradient2() +	
        xlab("variable") +	
        coord_flip() +	
        geom_label(color = "black", fontface = "bold", position = position_dodge(0.5))	
      
    }	
  } else if(!is.na(as.character(mod$Call)[1])){	
    
    if(as.character(mod$Call)[1] == "fa"){	
      loadings_table = mod$loadings %>% 	
        matrix(ncol = ncol(mod$loadings)) %>% 	
        as_tibble() %>% 	
        mutate(variable = mod$loadings %>% rownames()) %>% 	
        gather(factor, loading, -variable) %>% 	
        mutate(sign = if_else(loading >= 0, "positive", "negative"))	
      
      if(!is.null(loadings_above)){	
        loadings_table[abs(loadings_table[,"loading"]) < loadings_above,"loading"] = NA	
        loadings_table = loadings_table[!is.na(loadings_table[,"loading"]),]	
      }	
      
      if(!is.null(axes)){	
        
        loadings_table = loadings_table %>% 	
          filter(factor == paste0("V",axes))	
      }	
      
      plot = loadings_table %>% 	
        ggplot() +	
        aes(y = loading %>% abs(), x = reorder(variable, abs(loading)), fill = loading, label =       round(loading, 2)) +	
        geom_col(position = "dodge") +	
        scale_fill_gradient2() +	
        coord_flip() +	
        geom_label(color = "black", fill = "white", fontface = "bold", position = position_dodge(0.5)) +	
        facet_wrap(~factor) +	
        labs(y = "Loading strength", x = "Variable")	
    }	
  }	

  return(plot)	
  
}



animals_original <- read.csv("animalrights.csv")


animals <- animals_original %>% 
  drop_na()

#there were 5 rows with NAs

summary(animals)
str(animals)

animals$sex <- as.factor(animals$sex)
animals$party <- as.factor(animals$party)

table(animals$party)


animals_questions <- animals %>% 
  select(ar1:ar28)

cor <- animals_questions %>% 
  cor()

cor

ggcorr(cor)

ggcorrplot(cor(animals_questions), p.mat = cor_pmat(animals_questions),
           hc.order = TRUE, type = "lower")

pca_mod <- PCA(animals_questions) 


# Continuous variables that we don’t want to include in the PCA 
# are in the quanti.sup parameter
#     (liberal)
# those that are categorical in the quali.sup parameter
#     (sex, party)

which(names(animals) == "liberal")
which(names(animals) == "sex")
which(names(animals) == "party")

pca_mod2 <- PCA(animals, 
                quanti.sup = 31, 
                quali.sup = c(29, 30))

fviz_screeplot(pca_mod2, addlabels = TRUE)

pca_ret = paran(animals_questions, graph = TRUE)
pca_ret$Retained

#what about doing what we are supposed to do... meaning the EFA, not PCA

#TASK

#factorability

animals_cor <- animals %>% 
  select(ar1:ar28) %>% 
  cor()

#Bartlett sphericity test
#only reliable, when number of observations/number of variables is < 5

nrow(animals)/28

#it's bigger than 5, so it's not reliable

cortest.bartlett(animals_cor)

#Kaiser-Meyer-Olkin (KMO) test

KMO(animals_cor)

#KMO is higher than 0.6 in all cases, 
# and the total KMO is also higher than 0.6, 
# so the data seems to be factorable






#multivariate normality

mvn(hsq[, 1:32], mvnTest = "hz")

mvnorm.kur.test(na.omit(hsq[, 1:32]))

mvnorm.skew.test(na.omit(hsq[, 1:32]))


animals %>% 
  select(ar1:ar28) %>% 
  mvn(mvnTest = "hz")

mvnorm.kur.test(na.omit(animals[, 1:28]))

mvnorm.skew.test(na.omit(animals[, 1:28]))

# the tests indicate that the assumption of normality is violated 
# so we will use the paf extraction method

EFA_mod1 <- fa(animals_cor, nfactors = 5, fm = "pa")



# Sorted communality
EFA_mod1_common <- as.data.frame(sort(EFA_mod1$communality, decreasing = TRUE))
EFA_mod1_common
mean(EFA_mod1$communality)

#how many factors to choose?

fa.parallel(animals_cor, n.obs = nrow(animals), fa = "fa", fm = "pa")
#suggests 5

nfactors(animals_cor, n.obs = nrow(animals))
#VSS suggests 1 or 2
# Velicer MAP suggests 2
# Empirical BIC suggests 2
# Adjusted BIC suggests 7

EFA_mod2 <- fa(animals_cor, nfactors = 2, fm = "pa")
EFA_mod2_common <- as.data.frame(sort(EFA_mod2$communality, decreasing = TRUE))
EFA_mod2_common
mean(EFA_mod2$communality)

#so like this the mean communality is 0.34, which is really small
# it should be at least 0.6

EFA_mod3 <- fa(animals_cor, nfactors = 3, fm = "pa")
EFA_mod3_common <- as.data.frame(sort(EFA_mod2$communality, decreasing = TRUE))
EFA_mod3_common
mean(EFA_mod3$communality)

# since the mean communality is very low with 2 factors, I might at the end
# extract 3 factors,
# and now I try to remove some factors

# so let's remove 3, 8, 16, 28 and see what happens

animals_cor_selection <- animals %>% 
  select(ar1:ar28, -c(ar3, ar8, ar16, ar28)) %>% 
  cor()

EFA_mod3_sel <- fa(animals_cor_selection, nfactors = 3, fm = "pa")
EFA_mod3_sel_common <- as.data.frame(sort(EFA_mod3_sel$communality, 
                                          decreasing = TRUE))
EFA_mod3_common
mean(EFA_mod3_sel$communality)

# with 4 variables removed, the communality rose from 0.37 to 0.41
# with 5 variables removed it's 0.42, with 6 it's 0.43, with 7 it's 0.44, 
# and finally 8 is 0.45

animals_cor_selection <- animals %>% 
  select(ar1:ar28, -c(ar3, ar8, ar16, ar28, ar14, ar21, ar1, ar22)) %>% 
  cor()

EFA_mod2_sel <- fa(animals_cor_selection, nfactors = 2, fm = "pa")
EFA_mod2_sel_common <- as.data.frame(sort(EFA_mod2_sel$communality, 
                                          decreasing = TRUE))
EFA_mod2_common
mean(EFA_mod2_sel$communality)


EFA_mod3_sel <- fa(animals_cor_selection, nfactors = 3, fm = "pa")
EFA_mod3_sel_common <- as.data.frame(sort(EFA_mod3_sel$communality, 
                                          decreasing = TRUE))
EFA_mod3_common
mean(EFA_mod3_sel$communality)

#just realised there are more criteria to select variables... 
# so let's do some more stuff and come back to this later





#diagrams with 2 or 3 factors
fa.diagram(EFA_mod2)
fa.diagram(EFA_mod3)





#factor rotation
# I suppose they are correlated, so I'll use the promax method

EFA_mod2_promax <- fa(animals_cor, 
                     nfactors = 2, fm = "pa", rotate = "promax")
EFA_mod2_promax
fa.diagram(EFA_mod2_promax)


EFA_mod3_promax <- fa(animals_cor, 
                      nfactors = 3, fm = "pa", rotate = "promax")
EFA_mod3_promax
fa.diagram(EFA_mod3_promax)

#just to see, there is model with 4 factors
EFA_mod4_promax <- fa(animals_cor, 
                      nfactors = 4, fm = "pa", rotate = "promax")
EFA_mod4_promax
fa.diagram(EFA_mod4_promax)












#LOADINGS

fviz_loadnings_with_cor(EFA_mod2, axes = 1, loadings_above = 0.4)
#highest loading have 10, 7, 5, 24 and 4
#consumption, saving lives

fviz_loadnings_with_cor(EFA_mod2, axes = 2, loadings_above = 0.4)
#highest loading have 6, 27, 2, 17 and 9
#research


fviz_loadnings_with_cor(EFA_mod3, axes = 1, loadings_above = 0.4)
#highest loading have 6, 27, 2, 20 and 9
#research

fviz_loadnings_with_cor(EFA_mod3, axes = 2, loadings_above = 0.4)
#highest loading have 5, 13, 10, 4 and 26
#consumption

fviz_loadnings_with_cor(EFA_mod3, axes = 3, loadings_above = 0.4)
#there is only ar24
#saving lives


# rotated models

fviz_loadnings_with_cor(EFA_mod2_promax, axes = 1, loadings_above = 0.4)
#highest loading have 10, 24, 7, 5 and 4
#consumption, captivity

fviz_loadnings_with_cor(EFA_mod2_promax, axes = 2, loadings_above = 0.4)
#highest loading have 6, 27, 2, 17 and 9
#research


fviz_loadnings_with_cor(EFA_mod3_promax, axes = 1, loadings_above = 0.4)
#highest loading have 5, 13, 10, 4 and 26
#consumption, captivity

fviz_loadnings_with_cor(EFA_mod3_promax, axes = 2, loadings_above = 0.4)
#highest loading have 20, 24, 21, 19 and 15
#saving lives, research, animal/human rights

fviz_loadnings_with_cor(EFA_mod3_promax, axes = 3, loadings_above = 0.4)
#highest loading have 6, 17, 27, 2 and 18
#research










# REMOVING VARIABLES

#2nd try
# I'll start with removing variable ar28, 
# because it's topic is the most different

animals_cor_selection <- animals %>% 
  select(ar1:ar28, -c(ar28)) %>% 
  cor()

EFA_mod2_sel <- fa(animals_cor_selection, 
                   nfactors = 2, fm = "pa", rotate = "promax")
mean(EFA_mod2_sel$communality)
#mean communality = 0.41
#mean communality promax = 0.35

EFA_mod3_sel <- fa(animals_cor_selection, 
                   nfactors = 3, fm = "pa", rotate = "promax")
mean(EFA_mod3_sel$communality)
#mean communality = 0.38
#mean communality promax = 0.38

#loadings below 0.4 in PAs for EFA_mod2
#1, 3, 8, 11, 14, 15, 16, 19, 21, 22, 25

#loadings below 0.4 in PAs for EFA_mod2_promax
#1, 3, 8, 11, 12, 14, 15, 16, 19, 21, 25

#loadings below 0.4 in PAs for EFA_mod3
#1, 8, 9, 12, 14, 16, 25

#loadings below 0.4 in PAs for EFA_mod3_promax
#1, 11, 12, 14, 16, 25

#loadings below 0.4 in all models
#1, 14, 16, 25
#from those, especially 1 and 16 are different from the others

#------------------------------------------------------------------------

animals_cor_selection2 <- animals %>% 
  select(ar1:ar28, - c(ar1, ar28)) %>% 
  cor()

EFA_mod2_sel <- fa(animals_cor_selection2, 
                   nfactors = 2, fm = "pa", rotate = "promax")
mean(EFA_mod2_sel$communality)
#mean communality = 0.35
#mean communality promax = 0.35

EFA_mod3_sel <- fa(animals_cor_selection2, 
                   nfactors = 3, fm = "pa", rotate = "promax")
mean(EFA_mod3_sel$communality)
#mean communality = 0.39
#mean communality promax = 0.39

#-------------------------------------------------------------------------

animals_cor_selection3 <- animals %>% 
  select(ar1:ar28, - c(ar1, ar16, ar28)) %>% 
  cor()

EFA_mod2_sel <- fa(animals_cor_selection3, 
                   nfactors = 2, fm = "pa", rotate = "promax")
mean(EFA_mod2_sel$communality)
#mean communality = 0.36
#mean communality promax = 0.36

EFA_mod3_sel <- fa(animals_cor_selection3, 
                   nfactors = 3, fm = "pa", rotate = "promax")
mean(EFA_mod3_sel$communality)
#mean communality = 0.40
#mean communality promax = 0.40

#let's check the communalities

as.data.frame(sort(EFA_mod2_sel$communality, decreasing = TRUE))
as.data.frame(sort(EFA_mod3_sel$communality, decreasing = TRUE))

# questions 14 and 25 have in addition to low loading also low communality
# but 25 is theoretically relevant (asking about research)
# 14 includes suffering and pain of humans, which is strange
# so I remove only number 14

#------------------------------------------------------------------------

animals_cor_selection4 <- animals %>% 
  select(ar1:ar28, - c(ar1, ar14, ar16, ar28)) %>% 
  cor()

EFA_mod2_sel <- fa(animals_cor_selection4, 
                   nfactors = 2, fm = "pa", rotate = "promax")
mean(EFA_mod2_sel$communality)
#mean communality = 0.37
#mean communality promax = 0.37

EFA_mod3_sel <- fa(animals_cor_selection4, 
                   nfactors = 3, fm = "pa", rotate = "promax")
mean(EFA_mod3_sel$communality)
#mean communality = 0.41
#mean communality promax = 0.41

# loading of selected and rotated models

fviz_loadnings_with_cor(EFA_mod2_sel, axes = 1, loadings_above = 0.4)
fviz_loadnings_with_cor(EFA_mod2_sel, axes = 2, loadings_above = 0.4)

fviz_loadnings_with_cor(EFA_mod3_sel, axes = 1, loadings_above = 0.4)
fviz_loadnings_with_cor(EFA_mod3_sel, axes = 2, loadings_above = 0.4)
fviz_loadnings_with_cor(EFA_mod3_sel, axes = 3, loadings_above = 0.4)


EFA_mod2_sel$loadings < 0.4 & EFA_mod2_sel$loadings > -0.4
EFA_mod3_sel$loadings < 0.4 & EFA_mod3_sel$loadings > -0.4

#ar11 have low loading in both models
#also it is the only one specifically about insect, so I'll remove it now too

#-------------------------------------------------------------------------

animals_cor_selection5 <- animals %>% 
  select(ar1:ar28, - c(ar1, ar11, ar14, ar16, ar28)) %>% 
  cor()

EFA_mod2_sel <- fa(animals_cor_selection5, 
                   nfactors = 2, fm = "pa", rotate = "promax")
mean(EFA_mod2_sel$communality)
#mean communality = 0.37
#mean communality promax = 0.37

EFA_mod3_sel <- fa(animals_cor_selection5, 
                   nfactors = 3, fm = "pa", rotate = "promax")
mean(EFA_mod3_sel$communality)
#mean communality = 0.41
#mean communality promax = 0.41


fviz_loadnings_with_cor(EFA_mod2_sel, axes = 1, loadings_above = 0.4)
#highest loading have 10, 24, 7, 5 and 4
#consumption, captivity

fviz_loadnings_with_cor(EFA_mod2_sel, axes = 2, loadings_above = 0.4)
#highest loading have 6, 27, 2, 17 and 9
#research


fviz_loadnings_with_cor(EFA_mod3_sel, axes = 1, loadings_above = 0.4)
#highest loading have 5, 13, 10, 4 and 26
#consumption, captivity

fviz_loadnings_with_cor(EFA_mod3_sel, axes = 2, loadings_above = 0.4)
#highest loading have 20, 24, 21, 19 and 15
#saving lives, research, animal/human rights

fviz_loadnings_with_cor(EFA_mod3_sel, axes = 3, loadings_above = 0.4)
#highest loading have 6, 17, 27, 2 and 18
#research

EFA_mod2_sel$loadings < 0.4 & EFA_mod2_sel$loadings > -0.4
#loadings lower than 0.4: 3, 8, 19, 21, 25

EFA_mod3_sel$loadings < 0.4 & EFA_mod3_sel$loadings > -0.4
#loadings lower than 0.4: 8, 12, 24, 25

as.data.frame(sort(EFA_mod2_sel$communality, decreasing = TRUE))
#8, 3, 18

as.data.frame(sort(EFA_mod3_sel$communality, decreasing = TRUE))
#3, 8, 22

#3 and 8 have low loading and communality, so I remove 8 first
# since it's also asking about animal right to kill other animal

#------------------------------------------------------------------------

animals_cor_selection6 <- animals %>% 
  select(ar1:ar28, - c(ar1, ar8, ar11, ar14, ar16, ar28)) %>% 
  cor()

EFA_mod2_sel <- fa(animals_cor_selection6, 
                   nfactors = 2, fm = "pa", rotate = "promax")
mean(EFA_mod2_sel$communality)
#mean communality = 0.38
#mean communality promax = 0.38

EFA_mod3_sel <- fa(animals_cor_selection6, 
                   nfactors = 3, fm = "pa", rotate = "promax")
mean(EFA_mod3_sel$communality)
#mean communality = 0.42
#mean communality promax = 0.42

# after removing number 8, question 3 is still low on loading and communality
# so I remove it too

#-----------------------------------------------------------------------

animals_cor_selection7 <- animals %>% 
  select(ar1:ar28, - c(ar1, ar3, ar8, ar11, ar14, ar16, ar28)) %>% 
  cor()

EFA_mod2_sel <- fa(animals_cor_selection7, 
                   nfactors = 2, fm = "pa", rotate = "promax")
mean(EFA_mod2_sel$communality)
#mean communality = 0.38
#mean communality promax = 0.38

EFA_mod3_sel <- fa(animals_cor_selection7, 
                   nfactors = 3, fm = "pa", rotate = "promax")
mean(EFA_mod3_sel$communality)
#mean communality = 0.44
#mean communality promax = 0.44

EFA_mod2_sel$loadings < 0.4 & EFA_mod2_sel$loadings > -0.4
# 19, 21, 25
EFA_mod3_sel$loadings < 0.4 & EFA_mod3_sel$loadings > -0.4
# 25

as.data.frame(sort(EFA_mod2_sel$communality, decreasing = TRUE))
as.data.frame(sort(EFA_mod3_sel$communality, decreasing = TRUE))

#so the communality is rising just by 0.01 - 0.02,
# so I don't think I can make it over 0.6








# MAKING SENCE OF THE FINAL FACTORS

# 2 FACTOR MODEL
fa.diagram(EFA_mod2_sel)

# PA1
#  4   Captivity
#  5   Consumption
#  7   Save lives
#  10  Consumption
#  13  Consumption
#  22  Captivity
#  23  Consumption
#  24  Save lives
#  26  Consumption

# PA2
#  2   Research
#  6   Research
#  9   Research
#  12  Animal/human rights
#  15  Animal/human rights
#  17  Research
#  18  Research
#  19  Research
#  20  Save lives
#  21  Research
#  25  Research
#  27  Research

fa.diagram(EFA_mod3_sel)

# PA1
#  4   Captivity
#  5   Consumption
#  10  Consumption
#  13  Consumption
#  22  Captivity
#  23  Consumption
#  26  Consumption

# PA2
#  2   Research
#  6   Research
#  18  Research
#  17  Research
#  27  Research

# PA3
#  7   Save lives
#  9   Research
#  12  Animal/human rights
#  15  Animal/human rights
#  19  Research
#  20  Save lives
#  21  Research
#  24  Save lives
#  25  Research


# In 2 factor model, there is a factor related to research,
# and a factor related to consumption and and saving
# lives of animals

# In 3 factor model, there is a factor related to consumption
# of animal products and holding animals in captivity,
# then a factor for research, and finally research
# and protection of animal rights and lives 


fviz_loadnings_with_cor(EFA_mod2_sel, axes = 1, loadings_above = 0.4)
# 5, 10, 13, 7, 4
# consumption

fviz_loadnings_with_cor(EFA_mod2_sel, axes = 2, loadings_above = 0.4)
# 6, 27, 2, 17, 9
# research


fviz_loadnings_with_cor(EFA_mod3_sel, axes = 1, loadings_above = 0.4)
# 5, 13, 4, 10, 26
# consumption

fviz_loadnings_with_cor(EFA_mod3_sel, axes = 2, loadings_above = 0.4)
# 20, 21, 9, 24, 15
# research and saving lives

fviz_loadnings_with_cor(EFA_mod3_sel, axes = 3, loadings_above = 0.4)
# 6, 18, 27, 17, 2
# research



#SAVE FACTOR SCORES

animals_selection7 <- animals %>% 
  select(ar1:ar28, - c(ar1, ar3, ar8, ar11, ar14, ar16, ar28))

animals_selection8 <- animals %>% 
  select(- c(ar1, ar3, ar8, ar11, ar14, ar16, ar28))




fa2 <- factor.scores(animals_selection7, EFA_mod2_sel)

fa_scores2 <- data.frame(fa2$scores)

rownames(fa_scores2) <- 1:nrow(fa_scores2)

fa_scores2$rowname <- rownames(fa_scores2)
animals_selection8$rowname <- rownames(fa_scores2)

fa_scores2 <- fa_scores2 %>% 
  rename(consumption = PA1,
         research = PA2)

animals_factors2 <- left_join(fa_scores2, animals_selection8, 
                              by = "rowname")

linear_model_2 = lm(liberal ~ research +
                      consumption,
                    data = animals_factors2)

summary(linear_model_2)





fa3 <- factor.scores(animals_selection7, EFA_mod3_sel)

fa_scores3 <- data.frame(fa3$scores)

rownames(fa_scores3) <- 1:nrow(fa_scores3)

fa_scores3$rowname <- rownames(fa_scores3)
animals_selection8$rowname <- rownames(fa_scores3)

fa_scores3 <- fa_scores3 %>% 
  rename(consumption = PA1,
         research = PA2,
         research_lives = PA3)

animals_factors3 <- left_join(fa_scores3, animals_selection8, 
                             by = "rowname")

linear_model_3 = lm(liberal ~ research +
                  consumption +
                  research_lives,
                data = animals_factors3)

summary(linear_model_3)












#factor rotation VARIMAX

EFA_mod3_varimax <- fa(animals_cor, 
                       nfactors = 3,
                       fm = "pa", 
                       rotate = "varimax")


fviz_loadnings_with_cor(EFA_mod3_varimax, axes = 1, loadings_above = 0.4)
#highest loading have 5, 13, 10, 4 and 26
#consumption, captivity (the same as in promax version)

fviz_loadnings_with_cor(EFA_mod3_varimax, axes = 2, loadings_above = 0.4)
#highest loading have 6, 2, 27, 17 and 18
#research

fviz_loadnings_with_cor(EFA_mod3_varimax, axes = 3, loadings_above = 0.4)
#highest loading have 15, 20, 24, 19 and 21
#saving lives, animal/human rights, research

as.data.frame(sort(EFA_mod3_varimax$communality, decreasing = TRUE))
#3, 8, 16, 28
mean(EFA_mod3_varimax$communality)
#0.37
EFA_mod3_varimax$loadings < 0.4 & EFA_mod3_varimax$loadings > -0.4

# FIRST OUT - 28
# low communality, low loading, doesn't make sense



animals_CSV1 <- animals %>% 
  select(ar1:ar28, - c(ar28)) %>% 
  cor()

EFA_mod3_varimax <- fa(animals_CSV1, 
                       nfactors = 3,
                       fm = "pa", 
                       rotate = "varimax")

as.data.frame(sort(EFA_mod3_varimax$communality, decreasing = TRUE))
#3, 16, 1, 14
mean(EFA_mod3_varimax$communality)
#0.38
EFA_mod3_varimax$loadings < 0.4 & EFA_mod3_varimax$loadings > -0.4
#25, 16, 14, 11, 3, 1

# SECOND OUT - 16
# low communality, low loading, doesn't make sense



animals_CSV2 <- animals %>% 
  select(ar1:ar28, - c(ar16, ar28)) %>% 
  cor()

EFA_mod3_varimax <- fa(animals_CSV2, 
                       nfactors = 3,
                       fm = "pa", 
                       rotate = "varimax")

as.data.frame(sort(EFA_mod3_varimax$communality, decreasing = TRUE))
#3, 8, 14, 1, 22, 25
mean(EFA_mod3_varimax$communality)
#0.39
EFA_mod3_varimax$loadings < 0.4 & EFA_mod3_varimax$loadings > -0.4
# 25, 14, 11, 8, 3, 1

# THIRD OUT - 14
# low communality, low loading, its factor is quite full already



animals_CSV3 <- animals %>% 
  select(ar1:ar28, - c(ar14, ar16, ar28)) %>% 
  cor()

EFA_mod3_varimax <- fa(animals_CSV3, 
                       nfactors = 3,
                       fm = "pa", 
                       rotate = "varimax")

as.data.frame(sort(EFA_mod3_varimax$communality, decreasing = TRUE))
#3, 8, 1, 22, 25
mean(EFA_mod3_varimax$communality)
#0.40
EFA_mod3_varimax$loadings < 0.4 & EFA_mod3_varimax$loadings > -0.4
# 25, 8, 3, 1

# FOURTH OUT - 3
# it makes sense, but has low communality and loading, and the factor is full


animals_CSV4 <- animals %>% 
  select(ar1:ar28, - c(ar3, ar14, ar16, ar28)) %>% 
  cor()

EFA_mod3_varimax <- fa(animals_CSV4, 
                       nfactors = 3,
                       fm = "pa",
                       rotate = "varimax")

as.data.frame(sort(EFA_mod3_varimax$communality, decreasing = TRUE))
#8, 1, 21, 22, 25
mean(EFA_mod3_varimax$communality)
#0.41
EFA_mod3_varimax$loadings < 0.4 & EFA_mod3_varimax$loadings > -0.4
# 25, 11, 8

# FIFTH OUT - 8
# low communality, low loading, doesn't make sense in PA2



animals_CSV5 <- animals %>% 
  select(ar1:ar28, - c(ar3, ar8, ar14, ar16, ar28)) %>% 
  cor()

EFA_mod3_varimax <- fa(animals_CSV5, 
                       nfactors = 3,
                       fm = "pa",
                       rotate = "varimax")

as.data.frame(sort(EFA_mod3_varimax$communality, decreasing = TRUE))
#21, 1, 22, 25, 11
mean(EFA_mod3_varimax$communality)
#0.42
EFA_mod3_varimax$loadings < 0.4 & EFA_mod3_varimax$loadings > -0.4
# 25, 21, 11



# SIXTH OUT - 21
# low communality, low loading, factor already packed

animals_CSV6 <- animals %>% 
  select(ar1:ar28, - c(ar3, ar8, ar14, ar16, ar21, ar28)) %>% 
  cor()

EFA_mod3_varimax <- fa(animals_CSV6, 
                       nfactors = 3,
                       fm = "pa",
                       rotate = "varimax")

as.data.frame(sort(EFA_mod3_varimax$communality, decreasing = TRUE))
#19, 22, 25, 1, 11
mean(EFA_mod3_varimax$communality)
#0.43
EFA_mod3_varimax$loadings < 0.4 & EFA_mod3_varimax$loadings > -0.4
# 25, 19, 11

#At this point, the number of variables is more or less equal
# and the distribution kinda make sense
# so I'll leave it like this
# I don't think reaching communality higher than 0.6 is not possible
# since removing the variables only increases the communality by 0.01





animals_sel_a <- animals %>% 
  select(ar1:ar28, - c(ar3, ar8, ar14, ar16, ar21, ar28))

animals_sel_b <- animals %>% 
  select(- c(ar3, ar8, ar14, ar16, ar21, ar28))

fa3 <- factor.scores(animals_sel_a, EFA_mod3_varimax)

fa_scores3 <- data.frame(fa3$scores)

rownames(fa_scores3) <- 1:nrow(fa_scores3)

fa_scores3$rowname <- rownames(fa_scores3)
animals_sel_b$rowname <- rownames(fa_scores3)

fa_scores3 <- fa_scores3 %>% 
  rename(protection = PA1,
         research = PA2,
         consumption = PA3)

animals_factors3 <- left_join(fa_scores3, animals_sel_b, 
                              by = "rowname")

linear_model_3a = lm(liberal ~ protection +
                      research +
                      consumption,
                    data = animals_factors3)

summary(linear_model_3a)

linear_model_3b = lm(liberal ~ protection +
                      research +
                      consumption +
                       sex +
                       party,
                    data = animals_factors3)

summary(linear_model_3b)


