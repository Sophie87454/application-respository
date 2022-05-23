# MODEL CHECKING SCRIPT - ADD ON (9/1/19)
# FOR glmer objects
install.packages("coefplot2",repos="http://www.math.mcmaster.ca/bolker/R",
                 type="source")
library(coefplot)
library(r2glmm)  #https://cran.r-project.org/web/packages/r2glmm/README.html
library(dotwhisker)
library(DHARMa)
library(jtools)
library(mgcv)  # for plot.lme
library(effects) # for plot effects
library(car) # FOR ANOVA OF MODEL
library(lattice) # qq math
# NB - singualr fit issues
#https://stats.stackexchange.com/questions/378939/dealing-with-singular-fit-in-mixed-models
###############################
# For differing random effects see;
#https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet

M1 <- MF.NFEM5M.BEST # CHANGE MODEL NAME HEREN (I.E. COPY YOUR BEST MODEL TO 'M1'
# THEN JUST USE M1 THRU-OUT MODEL CHECKING PROCEDURES)
  
summary(M1)
sigma(M1)
anova(M1)
Anova(M1)
plot.lme(M1)

 
# PLOT EFFECTS
par(mfrow = c(1,1))
plot(effect("NFEM.2M", M1))
plot(effect("NFEM.5M", M1))
plot(effect("PUPAGE", M1))
plot(effect("PUP.SEX", M1))
plot(effect("RAIN.DRY", M1))
plot(effect("SUN.SHADE", M1))
plot(effect("SUBSTRATE", M1))
plot(effect("TOPOGRAPHY", M1))
plot(effect("YEAR", M1))
plot(effect("Z_med.rMSSD.adj.RPCALL", M1))

library(ggeffects)

p <- ggpredict(MF.NFEM5M.BEST, "Z_med.rMSSD.adj.RPCALL")

p <-as.data.frame(ggpredict(MF.NFEM5M.BEST, "Z_med.rMSSD.adj.RPCALL"))
plot(p)+ labs(x="Z-transformed median resting HRV within a breeding season", y="Number of females between 2m to 5m", title=" ")+ theme(text = element_text(size = 12)) 

ggp <- ggplot(p, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high)) +                        # Create basic ggplot
  geom_line()+
  geom_ribbon(alpha=0.1)+
  labs(x="Z-transformed median resting HRV within a breeding season", y = "Predicted effects on number of females between 2m to 5m")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))

# THIS DOES REASONABLE PLOTS - LIBRARY EFFECTS
dev.off(dev.list()["RStudioGD"])
e <- allEffects(M1,partial.residuals= TRUE) 
plot(e, ylab="NFEM2M",type="response",residuals.pch=1.5,rows=1, cols=2)  

# VALIDATION & DIAGNOSTICS
# Checking model assumptions####
# A. Look at independence: plot fitted values vs residuals


qqmath(M1)  # http://www.ashander.info/posts/2015/04/D-RUG-mixed-effects-viz/ ##FILE NOT FOUND
#qqnorm(residuals(M1))
#qqline(residuals(M1)) # same as qqmath
dev.off(dev.list()["RStudioGD"])
par(mfrow = c(2,2))
E1 <- resid(M1)
F1<-fitted(M1)
plot(x = F1, 
     y = E1, 
     xlab = "Fitted Values",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

# B. Look at independence:
# i. plot residuals vs each covariate in the model
# NFEM.2M
plot(x = dfmod$NFEM.2M, 
     y = E1, 
     xlab = "NFEM.2M",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)
#NFEM.5M
plot(x = dfmod$NFEM.5M, 
     y = E1, 
     xlab = "NFEM.5M",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

# PUPAGE
plot(x = dfmod$PUPAGE, 
     y = E1, 
     xlab = "PUPAGE",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)
# SUBSTRATE       
plot(x = dfmod$SUBSTRATE, 
     y = E1, 
     xlab = "SUBSTRATE",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)
# SUN.SHADE       
plot(x = dfmod$SUN.SHADE, 
     y = E1, 
     xlab = "SUN.SHADE",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)
# TOPO       
plot(x = dfmod$TOPO, 
     y = E1, 
     xlab = "TOPO",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)
# YEAR       
plot(x = dfmod$YEAR, 
     y = E1, 
     xlab = "YEAR",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)


par(mfrow = c(2,2))
# EXCLUDED VARS
# Z_med.rMSSD.adj.RPCALL
plot(x = dfmod$Z_med.rMSSD.adj.RPCALL, 
     y = E1, 
     xlab = "Z_med.rMSSD.adj.RPCALL",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)
#PUP.SEX
plot(x = dfmod$PUP.SEX, 
     y = E1, 
     xlab = "PUP.SEX",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)
#RAIN.DRY
plot(x = dfmod$RAIN.DRY, 
     y = E1, 
     xlab = "RAIN.DRY",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)






#The equal spread above and below zero indicate that there are no homogeneity problems with 
# these variables.
# Ideally you would also do the above analysis with every covariate not in your model as well.
# If you observe patterns in these plots you will know that there is variation in your dataset 
# that could be accounted for by these covariates that were not included in the model, 
# and so you should re-consider the inclusion of this variable in your model. 

# D. Look at normality: histogram
par(mfrow = c(1,1))
hist(E1)

#  Random effect exploration - ICC???
#########################################
#> ranef(M1)
#>  fixef(M1)


r1Var <- as.numeric(VarCorr(M1)[["ID"]])
residVar <- attr(VarCorr(M1), "sc")^2
r1Var
residVar
r1Var / (r1Var + residVar)  # x100 == % of the stochastic variation is accounted for by ID
# https://www.ssc.wisc.edu/sscc/pubs/MM/MM_Models.html
# https://www.ssc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html


# USING library(r2glmm)  #https://cran.r-project.org/web/packages/r2glmm/README.html
# Check the result with MuMIn's r.squaredGLMM
r.squaredGLMM(M1)
# the marginal R2 is the fixed effects variance, divided by the total variance 
#(i.e. fixed + random + residual). This value indicates how much of the "model variance" 
#is explained by the fixed effects part only.
# The conditional R2 is the fixed+random effects variance divided by the total variance, 
# and indicates how much of the "model variance" is explained by your "complete" model.




#R2 for the Generalized Linear Mixed Model (GLMM)
r2beta(model = M1, method = 'sgv', data = df)
# R??2, the proportion of generalized variance explained by the fixed predictors.
#This statistic is primarily used to select a covariance structure in the linear and generalized 
#linear mixed model.

r2beta(M1, method = 'nsj', partial = TRUE) # v little diff to above

# Compute the R2 statistic using Nakagawa and Schielzeth's approach. 
#R(m)2, the proportion of variance explained by the fixed predictors. 
#This statistic is a simplified version of R??2 that can be used as a substitute 
#for models fitted to very large datasets.


################################################################################
################################################################################
# EFFECT SIZES - ALTERNATE - PLOTS 


 library(broom.mixed)  # to get plots from glMMTMB objects : https://stackoverflow.com/questions/49116327/error-message-when-using-broom-to-get-coefficients-from-glmmtmb-zero-inflation-m

## this wrks for glmer
# USING library(dotwhisker)
 dwplot(M1)  
 dwplot(M1,effects="fixed")  

par(mfrow = c(2,1))
#library(coefplot2)  #Ok - this works for lmer (not glmmTMB) 
# random effects
coefplot(M1,ptype="vcov",intercept=TRUE,main="Random effect variance")
# Fixed effects
coefplot(M1,intercept=F,main="Fixed effect coefficient")


# Random intercept plots - ID
#We can also extract the random effect (or group-level) deviations from the fixed 
#intercept using the ranef function. This will tell us how much the intercept is shifted 
# up or down in particular populations or genotypes relative to the fixed intercept. 
# The deviations can then be plotted using dotplot, which will return a two-facetted plot 
#for each random effect (i.e., popu and gen). Note: the grid.arrange function was used 
#to omit the observation-level random effect (i.e. (1|X)).

pp <- list(layout.widths=list(left.padding=0, right.padding=0),
           layout.heights=list(top.padding=0, bottom.padding=0))
r2 <- ranef(M1,condVar=TRUE)
d2 <- dotplot(r2, par.settings=pp)
d2




############################################

#install.packages("DHARMa")
library(DHARMa)
#https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

simulationOutput <- simulateResiduals(fittedModel = M1, n = 1000)
simulationOutput$scaledResiduals
#Plotting the scaled residuals
plot(simulationOutput)



par(mfrow = c(1,1))

testDispersion(simulationOutput = simulationOutput)
# https://cran.r-project.org/web/packages/DHARMa/index.html
# https://stackoverflow.com/questions/9447329/how-to-plot-the-results-of-a-mixed-model
# https://stats.stackexchange.com/questions/98958/plots-to-illustrate-results-of-linear-mixed-effect-model

## ==============================================================================================
## ==============================================================================================
## ==============================================================================================
##


