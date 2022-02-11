library(lavaan) # for SEM fit and model functions
library(semPlot) # for semPaths()
library(semptools) # for set_sem_layout
library(tidyverse) # for tidy code
library(CompQuadForm) # for mvnorm.kur.test and mvnorm.skew.test (prerequisite)
library(ICS) # for mvnorm.kur.test and mvnorm.skew.test
library(psychTools)

my_data = holzinger.swineford

model <- '
visual =~ t01_visperc + t02_cubes + t03_frmbord + t04_lozenges
verbal =~ t06_paracomp + t07_sentcomp + t09_wordmean
processing =~ t10_addition + t12_countdot + t13_sccaps
'

fit <- sem(model, data = my_data)

semPaths(fit)

summary(fit, fit.measures = T)

#model estimator      ML
#chi-squared          0.00
#degrees of freedom   32
#p-value              0.00
#TLI (<0.95)          0.916
#CFI (<0.95)          0.940
#RMSEA (<0.06)        0.076


model2 <- '
visual =~ t01_visperc + t02_cubes + t03_frmbord + t04_lozenges
verbal =~ t06_paracomp + t07_sentcomp + t09_wordmean
processing =~ t10_addition + t12_countdot + t13_sccaps
t10_addition ~~ t12_countdot
'

fit2 <- sem(model2, data = my_data)

semPaths(fit2)

summary(fit2, fit.measures = T)

#model estimator      ML
#chi-squared          0.003
#degrees of freedom   31
#p-value              0.003
#TLI (<0.95)          0.960
#CFI (<0.95)          0.972
#RMSEA (<0.06)        0.053


#check multivariate normality

mvnorm.kur.test(na.omit(my_data[, 8:33]))
mvnorm.skew.test(na.omit(my_data[, 8:33]))

#p-value is significant, so, there is violation of normality





#bootstrapping

fit_boot <- sem(model, data = my_data, se = "bootstrap", test = "bootstrap")
fit2_boot <- sem(model2, data = my_data, se = "bootstrap", test = "bootstrap")

summary(fit_boot, fit.measures = T)
#model estimator      ML
#chi-squared          0.000
#degrees of freedom   32
#p-value              0.000
#TLI                  0.916
#CFI                  0.940
#RMSEA                0.075

summary(fit2_boot, fit.measures = T)
#model estimator      ML
#chi-squared          0.003
#degrees of freedom   31
#p-value              0.011
#TLI                  0.916
#CFI                  0.960
#RMSEA                0.053




fit_MLM <- sem(model, data = my_data, estimator = "MLM")
fit2_MLM <- sem(model2, data = my_data, estimator = "MLM")



semPaths(fit2_boot, whatLabels = "est")
summary(fit2_boot, standardized = T, rsquare = T)
#least influenced is t02
standardizedsolution(fit2_boot)









model3 <- '
t13_sccaps ~ t01_visperc + t12_countdot
t12_countdot ~ t01_visperc
'

fit3 <- sem(model3, data = my_data)

semPaths(fit3, whatLabels = "est")

summary(fit3)

