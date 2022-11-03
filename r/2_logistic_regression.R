# In this script, we perform a RANDOM EFFECTS model (GLMM)            #

### --- 0. Loading libraries --- ####
library(INLA)

### --- 1. Loading the data --- ####
# GLMM
# (Martino and Rue 2010; Breslow and Clayton (1993))
# Proportion of seeds that germinated on each of 21 plates arranged according 
# to a 2 by 2 factorial layout by seed and type of root extract
# variables:
# r: number of germinated seeds per plate
# n: number of total seeds per plate
# x1: seed type (0: seed O. aegyptiaco 75; 1: seed O. aegyptiaco 73) 
# x2: root extracted (0:bean; 1:cucumber); 
# plate: indicator for the plate

#Model:     logit(pi) = beta0 + beta1*x1 + beta2*x2 + random.effect(plate)
data(Seeds)
Seeds$x1 <- factor(Seeds$x1, labels=c("ae75", "ae73"))
Seeds$x2 <- factor(Seeds$x2, labels=c("bean", "cucumber"))
Seeds
str(Seeds)

### --- 2. Defining the formula and fitting the model --- ####
### ----- 2.1. Without random effects --- ###
formula1 <- r ~  1 + x1 + x2 
mod1.seeds = inla(formula1, 
                  data            = Seeds, 
                  family          = "binomial", 
                  Ntrials         = Seeds$n, 
                  control.compute = list(dic = TRUE, 
                                         waic = TRUE, 
                                         cpo = TRUE))
summary(mod1.seeds)


### ----- 2.2. Interactions between factors --- ####
formula2 <- r ~  1 + x1*x2
mod2.seeds = inla(formula2, 
                  data            = Seeds, 
                  family          = "binomial", 
                  Ntrials         = Seeds$n, 
                  control.compute = list(dic = TRUE, 
                                         waic = TRUE, 
                                         cpo = TRUE))
summary(mod2.seeds)


### ----- 2.2. Random effects are introduced to the model using the function 'f()' --- ###
formula <- r ~ 1 + x1*x2 + f(plate, 
                               model  = "iid",
                               hyper  = list(theta = list(prior = "loggamma",
                                                          param = c(1,0.01)))) 

# To change priors for the hyperparameter: 'f(plate, model="iid",param=c(a,b))';
# Precision for the hyperparameter ~ Gamma(a,b)
# By default precision ~ Gamma(1, 0.00005), we can change 
mod.seeds = inla(formula, 
                 data            = Seeds, 
                 family          = "binomial", 
                 Ntrials         = n, 
                 control.compute = list(dic  = TRUE, 
                                        waic = TRUE, 
                                        cpo  = TRUE))
summary(mod.seeds)


### --- 3. Best model --- ####
selection <- data.frame(DIC = c(mod1.seeds$dic$dic, mod2.seeds$dic$dic, mod.seeds$dic$dic),
                      WAIc=c(mod1.seeds$waic$waic, mod2.seeds$waic$waic, mod.seeds$waic$waic),
                      LCPO=c(-mean(log(mod1.seeds$cpo$cpo)), 
                             -mean(mod2.seeds$cpo$cpo), - mean(log(mod.seeds$cpo$cpo))))
rownames(selection)<-c("mod1.seeds", "mod2.seeds", "mod.seeds")
selection

### --- Model which include a random effects is better --- ###

### --- 4. Posterior distribution --- ####
summary.random <- mod.seeds$summary.random$plate
round(head(summary.random),3)

### --- Posterior distribution of the standard deviation --- ###
### Transform precision in the standard deviation
sigma.v <- inla.tmarginal(function(x) sqrt(1/x), 
                         mod.seeds$marginals.hyperpar[[1]])

### --- Plot  --- ###
#pdf("posteriori_v.pdf", width=5, height=5)
plot(inla.smarginal(sigma.v),type="l",xlab="",
     ylab="",main=expression(paste("Posterior distribution ", sigma)))
hpd.sigma = inla.hpdmarginal(p=0.95, sigma.v)
abline(v=hpd.sigma[1],lty=2,col=2)
abline(v=hpd.sigma[2],lty=2,col=2)
#dev.off()
### --- Summary --- ###
inla.zmarginal(sigma.v)


### --- 5. Probability to be positive the coefficient associated with x2? --- ####
# Pr(beta_x2 > 0)?
1 - inla.pmarginal(0, mod.seeds$marginals.fixed$x2)
plot(inla.smarginal(mod.seeds$marginals.fixed$x2), type="l")














