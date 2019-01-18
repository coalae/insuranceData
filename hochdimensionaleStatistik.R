######################################################################
# Hochdimensionale Statistik 
######################################################################

# Vorbereitung:
install.packages("insuranceData")
install.packages("dplyr")
require(insuranceData)
require(dplyr)
rm(list=ls())
setwd("C:/Users/Coala/Desktop/HOCHDIM") 

#---------------------------------------------------------------------

# 1.a. Get acquainted with dataset DataCar:
data("dataCar")
head(dataCar, n=10)

nrow(dataCar)
ncol(dataCar)

#---------------------------------------------------------------------

# 1.b. (see report)

#---------------------------------------------------------------------

# 1.c. Display descriptive statistics of interest about the number and 
#      distributions of claims 
#      (also across different areas, vehicle ages, etc.)

summary(dataCar) # min, 1st quantile, median, mean, 3rd quantile, max

boxplot(dataCar$veh_value)
sd(dataCar$veh_value)
range(dataCar$veh_value)

boxplot(dataCar$exposure)
sd(dataCar$exposure)
range(dataCar$exposure)

sum(dataCar$clm)/length(dataCar$clm) # % of claims based on entire dataset

boxplot(dataCar$claimcst0)
sd(dataCar$claimcst0)
range(dataCar$claimcst0)

table_veh <- table(dataCar$veh_body)
barplot(table_veh, cex.axis=1, xlab="vehicle body", ylab="frequency", 
        col="darkslategray3", cex.names=0.7,
        xlim=c(0.5,length(table_veh)), ylim=c(0,25000))

boxplot(dataCar$veh_age)
sd(dataCar$veh_age)
range(dataCar$veh_age)

table_gender <- table(dataCar$gender)
barplot(table_gender, cex.axis=1, xlab="gender", ylab="frequency", 
        col="darkslategray3", cex.names=1,
        xlim=c(0,length(table_gender)+0.7), ylim=c(0,40000))

table_area <- table(dataCar$area)
barplot(table_area, cex.axis=1, xlab="area", ylab="frequency", 
        col="darkslategray3", cex.names=1,
        xlim=c(0,length(table_area)+0.7), ylim=c(0,25000))

table_agecat <- table(dataCar$agecat)
barplot(table_agecat, cex.axis=1, xlab="agecat", ylab="frequency", 
        col="darkslategray3", cex.names=1,
        xlim=c(0,length(table_agecat)+0.7), ylim=c(0,20000))


# nr. of claims per area:
clm_by_area <- aggregate(dataCar$numclaims, by=list(dataCar$area), FUN=sum)
colnames(clm_by_area) <- c("area","clms")
clm_by_area["mean"] <- aggregate(dataCar$numclaims, by=list(dataCar$area), FUN=mean)[2]
clm_by_area["sd"] <- aggregate(dataCar$numclaims, by=list(dataCar$area), FUN=sd)[2]
plot(clm_by_area[1:2], ylab="frequency", xlab="areas", yaxt="n")
axis(2, at = seq(0, max(clm_by_area[2])+200, by = 250))


# nr. of claims per veh_age:
clm_by_vehicle_age <- aggregate(dataCar$numclaims, by=list(dataCar$veh_age), FUN=sum)
colnames(clm_by_vehicle_age) <- c("veh_age","clms")
clm_by_vehicle_age["mean"] <- aggregate(dataCar$numclaims, by=list(dataCar$veh_age), FUN=mean)[2]
clm_by_vehicle_age["sd"] <- aggregate(dataCar$numclaims, by=list(dataCar$veh_age), FUN=sd)[2]
plot(clm_by_vehicle_age[1:2], ylab="frequency", xlab="veh_age", xaxt="n", yaxt="n", 
     pch=18, cex=2, col="mediumpurple4", ylim=c(0,max(clm_by_vehicle_age[2])+100))
axis(1, at = seq(1, nrow(clm_by_vehicle_age), by = 1))
axis(2, at = seq(0, max(clm_by_vehicle_age[2])+200, by = 250))


# nr. of claims per gender:
clm_by_gender <- aggregate(dataCar$numclaims, by=list(dataCar$gender), FUN=sum)
colnames(clm_by_gender) <- c("gender","clms")
plot(clm_by_gender[1:2], ylab="frequency", xlab="gender", 
     ylim=c(0, max(clm_by_gender[2])+250))

# nr. of claims per agecat:
clm_by_agecat <- aggregate(dataCar$numclaims, by=list(dataCar$agecat), FUN=sum)
colnames(clm_by_agecat) <- c("agecat","clms")
clm_by_agecat["mean"] <- aggregate(dataCar$numclaims, by=list(dataCar$agecat), FUN=mean)[2]
clm_by_agecat["sd"] <- aggregate(dataCar$numclaims, by=list(dataCar$agecat), FUN=sd)[2]
plot(clm_by_agecat[1:2], ylab="frequency", xlab="agecat", xaxt="n", yaxt="n", 
     pch=18, cex=2, col="mediumpurple4", ylim=c(0,max(clm_by_agecat[2])+100))
axis(1, at = seq(1, nrow(clm_by_agecat), by = 1))
axis(2, at = seq(0, max(clm_by_agecat[2])+200, by = 250))


# nr. of claims per veh_body:
clm_by_veh_body <- aggregate(dataCar$numclaims, by=list(dataCar$veh_body), FUN=sum)
colnames(clm_by_veh_body) <- c("veh_body","clms")
plot(clm_by_veh_body[1:2], ylab="frequency", xlab="veh_body", xaxt="n", yaxt="n")
axis(1, at = seq(1, nrow(clm_by_veh_body), by = 1), 
     labels=as.character(clm_by_veh_body$veh_body), cex.axis=0.7, las=2)
axis(2, at = seq(0, max(clm_by_veh_body[2])+200, by = 250))



# distribution of claim amounts (claimcst0)
total_claims <- sum(dataCar$numclaims)
total_claims_amount <- sum(dataCar$claimcst0)

amount_per_claim_overallData <- total_claims_amount/nrow(dataCar) 
  # or: mean(dataCar$claimcst0) 
amount_per_claim_claimsData <- total_claims_amount/total_claims

#---------------------------------------------------------------------

# 1.d. Compare the observed mean to the observed variance of the number of claims.
#      Which one is larger? Test for overdispersion in R. One way to check for overdispersion
#      is to run a quasi-poisson model, which fits an extra dispersion parameter to
#      account for the extra variance. Then argue whether Poisson distribution or Negative
#      Binomial distribution is to be used to fit our data.
# 1.e. Fit both models in R using glm (generalized linear model). Note that an adjustment
#      for the exposure variable needs to be done. An offset in the glm function has to be
#      used. This concept relates to the fact that if a one-year policy (exposure=1) has 2
#      claims, one would expect that a half-year policy (exposure=0.5) has 1 claim.  

# mean vs. variance of numclaims
meanOfNumclaims_observed <- mean(dataCar$numclaims)
varianceOfNumclaims_observed <- (sd(dataCar$numclaims))^2

# if E[x]==Var[x], then using poisson distribution is ok 
# if E[x]!=Var[x], then using poisson distribution is not ok -> take negative binomal distr.
#                  (glm.nb to fit the model)

varianceOfNumclaims_observed > meanOfNumclaims_observed # variance is larger in this case

# try poisson model
model_poisson=glm(dataCar$numclaims~offset(log(dataCar$exposure))+dataCar$area+dataCar$gender, 
                  family=poisson)
                  # +offset(log(sum(dataCar$numclaims)))
                  # offset=log(sum(dataCar$numclaims))
summary(model_poisson)

# goodness-of-fit test (p>0.05)
1-pchisq(model_poisson$deviance,model_poisson$df.residual) # fits here


# test for overdispersion (using quasi-poisson model with extra dispersion parameter)
model_quasipoisson=glm(dataCar$numclaims~offset(log(dataCar$exposure))+dataCar$area+dataCar$gender,
                  family=quasipoisson)
summary(model_quasipoisson)

# ratio of residual deviance to redisual dfs should be ca. 1 (i.e. taken dispersion param.)
model_quasipoisson$deviance / model_quasipoisson$df.residual  
                                                        

print(1-pnorm(model_quasipoisson$deviance, model_quasipoisson$df.residual))
qqnorm(resid(model_quasipoisson))

# goodness-of-fit test (p>0.05)
1-pchisq(model_quasipoisson$deviance,model_quasipoisson$df.residual) # fits here

#install.packages("AER")
library(AER)
dispersiontest(model_poisson) # true dispersion is slightly >1 acc. to test
dispersiontest(model_poisson, trafo=1) # equidispersion: 0 (near 0 in this case)


#install.packages("devtools")
#install.packages("DHARMa")
library(DHARMa)
sim_model_poisson <- simulateResiduals(model_poisson, refit=T)
testOverdispersion(sim_model_poisson)
plotSimulatedResiduals(sim_model_poisson)


# try negative binomial distribution  
#install.packages("MASS")
library(MASS)
model_negativeBinomial <- glm.nb(dataCar$numclaims~offset(log(dataCar$exposure))+
                                 dataCar$area+dataCar$gender, 
                                 data=dataCar)
summary(model_negativeBinomial) # lower residual deviance than poisson models

# goodness-of-fit test (p>0.05)
1-pchisq(model_negativeBinomial$deviance,model_negativeBinomial$df.residual) # fits here

sim_nb <- simulateResiduals(model_negativeBinomial, refit=T,n=99)
x11()
plotSimulatedResiduals(sim_nb)

testOverdispersion(sim_nb)  
  
#---------------------------------------------------------------------
  
# 1.f. Understand and explain the concept of zero-inflated distribution and fit it to the
#      data. https://rdrr.io/rforge/countreg/man/zeroinfl.html

#install.packages("pscl")
library(pscl)  

# zero-inflated poisson model
model_zeroinfl_poisson=zeroinfl(dataCar$numclaims~offset(log(dataCar$exposure))+dataCar$area+
                                  dataCar$gender, dist="poisson")
summary(model_zeroinfl_poisson)

# zero-inflated negative binomial model
model_zeroinfl_nb=zeroinfl(dataCar$numclaims~offset(log(dataCar$exposure))+dataCar$area+
                                  dataCar$gender, dist="negbin")
summary(model_zeroinfl_nb) # log(theta) estimation is not significant 
                           # >> zero-inflated poisson model fits better

#---------------------------------------------------------------------

# 1.g. Compare all your candidate models using AIC. See also Vuong test 
#      (from pscl package).
#      https://www.rdocumentation.org/packages/pscl/versions/1.5.2/topics/vuong

# calculate AIC for all models
aic_poisson <- AIC(model_poisson)
aic_nb <- AIC(model_negativeBinomial)
aic_zeroinfl_poisson <- AIC(model_zeroinfl_poisson)
aic_zeroinfl_nb <- AIC(model_zeroinfl_nb)
aic_models <- c(aic_poisson=aic_poisson,aic_nb=aic_nb,
                aic_zeroinfl_poisson=aic_zeroinfl_poisson,
                aic_zeroinfl_nb=aic_zeroinfl_nb)

# min. AIC among models
min_AIC_model <- min(aic_models)
names(min_AIC_model) <- names(which(aic_models==min_AIC_model))
min_AIC_model

# vuong test (model vs. zero-inflated model)
vuong(model_poisson,model_zeroinfl_poisson)
vuong(model_negativeBinomial,model_zeroinfl_nb)

#---------------------------------------------------------------------




