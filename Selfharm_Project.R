library(haven)
library(mzipmed)
##############test for ZI############
library(pscl)  
library(MASS)  
library(lmtest) 
library(ggplot2) 
#  count_harm
zero_percentage <- mean(Selfharm_eddited_sav$SH == 0, na.rm = TRUE)
print(paste(" SIB:", round(zero_percentage * 100, 2), "%"))

#  count_harm
ggplot(Selfharm_eddited_sav, aes(x = harm)) + 
  geom_bar(fill = "steelblue") + 
  theme_classic() +
  ggtitle("distribution harm")
#######Poisson###
poisson_model <- glm(SH ~ 1, family = poisson, data = Selfharm_eddited_sav, na.action = na.exclude)
summary(poisson_model)
AIC(poisson_model)
######################NB###############
nb_model <- glm.nb(harm ~ 1, data = Selfharm_eddited_sav, na.action = na.exclude)
summary(nb_model)
########ZIP########
zip_model <- zeroinfl(SH ~ 1 | 1, dist = "poisson", data = Selfharm_eddited_sav, na.action = na.exclude)
summary(zip_model)
AIC(zip_model)
#################ZINB##############
zinb_model <- zeroinfl(harm ~ 1 | 1, dist = "negbin", data = Selfharm_eddited_sav, na.action = na.exclude)
summary(zinb_model)
###########ZIP and P comparison###########
vuong(poisson_model, zip_model)
###########ZINB and NB comparison########
vuong(nb_model, zinb_model)
########NB and Poisson comparison#########
lrtest(poisson_model, nb_model)
table(Selfharm_eddited_sav$harm)  # 
class(Selfharm_eddited_sav$harm)  # 
#####################Poisson and NB comparison#######
library(pscl)
library(MASS)
library(ggplot2)
zip_model <- zeroinfl(harm ~ Age+ Gender+ Marital+ Occupation, 
                      data = Selfharm_eddited_sav, 
                      dist = "poisson")

zinb_model <- zeroinfl(harm ~ Age+ Gender+ Marital+ Occupation, 
                       data = Selfharm_eddited_sav, 
                       dist = "negbin")

AIC(zip_model, zinb_model)
BIC(zip_model, zinb_model)
#######
library(pscl)
vuong(zip_model, zinb_model)
#########Poisson and NB comparison######
vuong(poisson_model, zip_model)
############Sims model#############
library(haven)
Selfharm_eddited_sav <- read_sav("E:/Thesis - Copy/Selfharm/Selfharm. eddited sav.sav")
attach(Selfharm_eddited_sav)
library(mzipmed)
names(Selfharm_eddited_sav)
##################Avoidant############
model1<-zioutlmmed(SH,ZDERSTotal,ZDelbastegiEjtenabi,confounder=cbind(Age,Gender,Marital,Occupation),X = 1,Xstar = 0,error = "Delta", n = 5000,robust = FALSE,zioff = NULL)
model2<-zioutlmmed(SH,ZDERSTotal,ZDelbastegiEjtenabi,confounder=cbind(Age,Gender,Marital,Occupation),X = 1,Xstar = 0,error = "Delta", n = 5000,robust = TRUE,zioff = NULL)
################Anxious-Ambivalent############################
model1<-zioutlmmed(SH,ZDERSTotal,ZDelbastegi2sugera,confounder=cbind(Age,Gender,Marital,Occupation),X = 1,Xstar = 0,error = "Delta", n = 5000,robust = FALSE,zioff = NULL)
model2<-zioutlmmed(SH,ZDERSTotal,ZDelbastegi2sugera,confounder=cbind(Age,Gender,Marital,Occupation),X = 1,Xstar = 0,error = "Delta", n = 5000,robust = TRUE,zioff = NULL)
################Secure############################
model1<-zioutlmmed(SH,ZDERSTotal,ZDelbastegiImen,confounder=cbind(Age,Gender,Marital,Occupation),X = 1,Xstar = 0,error = "Delta", n = 5000,robust = FALSE,zioff = NULL)
model2<-zioutlmmed(SH,ZDERSTotal,ZDelbastegiImen,confounder=cbind(Age,Gender,Marital,Occupation),X = 1,Xstar = 0,error = "Delta", n = 5000,robust = TRUE,zioff = NULL)
library(MASS)
summary(model1)
summary(model2)
# Extract log-likelihood
logLik_value <- model1$loglik
logLik_value
# Count the number of parameters (including zero-inflation and Poisson components)
num_params <- length(model1$coefficients)

# Compute AIC manually
AIC_value <- -2 * logLik_value + 2 * 2
print(AIC_value)
str(model1)
logLik_value <- logLik(model1)
print(logLik_value)

library(pscl)
AIC(model1,model2)
any(is.na(count_harm))
any(is.na(ZLESSTotal))
any(is.na(ZDelbastegi2sugera))
any(is.na(Age))
summary(output)$coefficients  # If coefficients exist
install.packages("medflex")   # If not installed
install.packages("mediation") # If not installed
library(medflex)
library(mediation)
head(Selfharm_eddited_sav)  # Check the first few rows
mediator_model <- lm(ZLESSTotal ~ ZDelbastegi2sugera, data = Selfharm_eddited_sav)
summary(mediator_model)
outcome_model <- glm(SHI15.1 ~ ZDelbastegi2sugera + ZLESSTotal+ Age, family = poisson, data =Selfharm_eddited_sav)
summary(outcome_model)
library(mediation)

mediation_result <- mediate(mediator_model, outcome_model, 
                            treat = "ZDelbastegi2sugera", mediator = "ZLESSTotal", 
                            boot = TRUE, sims = 1000)

summary(mediation_result)
# Extract log effects
log_NDE <- mediation_result$d0
log_NIE <- mediation_result$z0
log_TE  <- log_NDE + log_NIE

# Extract standard errors
se_NDE <- sd(mediation_result$d0.sims)
se_NIE <- sd(mediation_result$z0.sims)
se_TE  <- sqrt(se_NDE^2 + se_NIE^2)  # Approximate SE for total effect

# Compute 95% CI using Normal Approximation
z_score <- 1.96  # 95% CI multiplier

# IRR and 95% CI for Natural Direct Effect
IRR_NDE <- exp(log_NDE)
CI_NDE  <- exp(log_NDE + c(-1, 1) * z_score * se_NDE)

# IRR and 95% CI for Natural Indirect Effect
IRR_NIE <- exp(log_NIE)
CI_NIE  <- exp(log_NIE + c(-1, 1) * z_score * se_NIE)

# IRR and 95% CI for Total Effect
IRR_TE <- exp(log_TE)
CI_TE  <- exp(log_TE + c(-1, 1) * z_score * se_TE)

# Compute Proportion Mediated
Prop_Mediated <- log_NIE / log_TE

# Display results
cat("IRR_NDE:", IRR_NDE, "95% CI:", CI_NDE, "\n")
cat("IRR_NIE:", IRR_NIE, "95% CI:", CI_NIE, "\n")
cat("IRR_TE:", IRR_TE, "95% CI:", CI_TE, "\n")
cat("Proportion Mediated:", Prop_Mediated, "\n")
###############################
Selfharm_eddited_sav$Gender <- as.factor(Selfharm_edited_sav$Gender)
Selfharm_edited_sav$Marital <- as.factor(Selfharm_edited_sav$Marital)
Selfharm_edited_sav$Occupation <- as.factor(Selfharm_edited_sav$Occupation)
Selfharm_edited_sav$Medications <- as.factor(Selfharm_edited_sav$Medications)
Selfharm_edited_sav$Attemptosuicide <- as.factor(Selfharm_edited_sav$Attemptosuicide)
Selfharm_edited_sav$Admission <- as.factor(Selfharm_edited_sav$Admission)


zioutlmmed(SHI15.1, ZLESSTotal, ZDelbastegi2sugera,
           confounder = cbind(Age, Gender, Marital, Occupation, Medications, Attemptosuicide, Admission),
           X = 5, Xstar = 0, error = "Bootstrap", n = 5000, robust = FALSE, zioff = NULL)

###########AIC####################
names(model1)
library(stats)
# Automated AIC Calculation
AIC_automated <- AIC(model1)
##############AIC##################
compute_AIC_mzipmed <- function(model1) {
  logLik_value <- model$loglik
  num_params <- length(unlist(model$coefficients))
  return(-2 * logLik_value + 2 * num_params)
}

# Compute AIC
AIC_mzipmed <- compute_AIC_mzipmed(model1)
print(AIC_mzipmed)
logLik_value <- logLik(model1)
AIC(model1)


