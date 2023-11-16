#########################################
# Generalized Linear Models - Project
# Group 6
# Aertgeerts Ine    r0589446
# Dufie Nana Abena  r0881091
# Herregods Stef    r0669884
# Schoepen Sander   r0737134
# Zhao Yang*        r0823619
#   *Did not contribute to the project
#########################################

setwd("C:/Users/saaan/Downloads")

# Required packages
install.packages("DHARMa")
install.packages("corrplot")
install.packages("pscl")



library(pscl)
library(DHARMa)
library(car)
library(corrplot)
library(DHARMa)
library(dplyr)
library(MASS)

# Load data

geriatric <- read.table(file="07-geriatric-Data.txt", header=TRUE, sep="")
head(geriatric)

### EXPLORATORY DATA ANALYSIS ###

# Boxplots and scatterplots
par(mfrow=c(2,2))
boxplot(Y ~ tto, data=geriatric)
boxplot(Y ~ gender, data=geriatric)
plot(Y ~ BI, data=geriatric)
plot(Y ~ SI, data=geriatric)
par(mfrow=c(1,1))

# Correlation matrix
cor(geriatric)
corrplot(cor(geriatric))

# Average number of falls for both intervention types
tto0 = geriatric[geriatric$tto==0,]
tto1 = geriatric[geriatric$tto==1,]
mean_tt0 <- mean(tto0$Y)
mean_tt0
mean_tt1 <- mean(tto1$Y)
mean_tt1

#check group sizes

gender<- factor(geriatric$gender)
levels(gender)<-c("Female", "Male") 

treatment<-factor(geriatric$tto)
levels(treatment)<-c("without aerobics", "with aerobics")

summary(treatment)
summary(geriatric)

par(mfrow=c(1,2))

barplot(summary(gender),ylab = "group size", xlab="gender", main="Gender group size")
barplot(summary(treatment), ylab="frequency", xlab="tto",main="Treatment group size")
#Group sizes per category almost identical=> no need to use rates instead of counts

### POISSON MODEL ###


model1 <- glm(Y ~ tto, family=poisson(link="log"), data=geriatric)
summary(model1)
# Adding the covariates gender, BI and SI
model2 <- glm(Y ~ tto + gender + BI + SI, family=poisson(link="log"), data=geriatric)
summary(model2)
# Significant effect of intervention type on the frequency of falls
# Also significant effect of BI and SI
# No significant effect of gender

# Stepwise regression using stepAIC()
# Starting from the full model with all 2-way interactions
model.full <- glm(Y ~ tto + gender + BI + SI 
                      + tto*gender + tto*BI + tto*SI 
                      + gender*BI + gender*SI 
                      + BI*SI, 
                      family=poisson(link="log"), data=geriatric)
stepAIC(model.full)
# Best model: Y ~ tto + BI + SI
model3 <- glm(Y ~ tto + BI + SI, family=poisson(link="log"), data=geriatric)
summary(model3)



# Model comparison using AIC
AIC(model1, model2, model3, model.full)

# Predictions for both interventions, based on the Poisson model
exp(predict(model3, tto0))[1]
exp(predict(model3, tto1))[1]
# Or alternatively:
predict(model3, tto0, type="response")[1]
predict(model3, tto1, type="response")[1]

# Risk ratios
glm.RR <- function(glm.result, digits=3) {
  coef      <- coef(glm.result)
  confint   <- confint(glm.result)
  table     <- cbind(coef, confint)
  table.exp <- round(exp(table), digits)
  colnames(table.exp)[1] <- "RR"
  table.exp
}
glm.RR(model3)

### GOODNESS OF FIT ###

# Uniformity of the residuals: Kolmogorov-Smirnov test
sim.model3 = simulateResiduals(model3, plot=T)
testUniformity(sim.model3)
# Not significant --> residuals follow a uniform distribution
hist(sim.model3)

# Overdispersion: dispersion test
testDispersion(sim.model3)
# Not significant --> no overdispersion

###ZIP model###
model_zip<-zeroinfl(Y ~ tto + BI + SI |1, data=geriatric)
summary(model_zip)

AIC(model3, model_zip)
#model3 still has a lower BIC


#comparing observed and predicted values
zobs <- geriatric$Y == 0
zpoi <- exp(-exp(predict(model3)))

zip.pois <- predict(model_zip,type="prob")[,1]

c(obs=mean(zobs), poi=mean(zpoi), zip=mean(zip.pois))
#model3 does a better job at predicting the amount of 0s than model_zip
