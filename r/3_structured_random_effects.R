#' Structured random effects

library(INLA)
library(ggplot2)
library(gridExtra)

### --- 1. Read the data --- ####
# Binomial model with temporal effect (without covariates) (Rue and Held 2005)

# The number of occurrences of rainfall over 1 mm in the Tokyo area for each calendar year during two
# years (1983-84) are registered. It is of interest to estimate the underlying probability pt of rainfall
# for calendar day t which is, apriori, assumed to change gradually over time.
#  (http://www.math.ntnu.no/~hrue/r-inla.org/examples/tokyo/tokyo.pdf)

# Variables: 
#   y: number of days with rain
#   n: total number of days
#   time: day of the year
# Model: logit(pi) = ef.aleatorio(time)

### --- Loading the data --- ###
data(Tokyo)
summary(Tokyo)

### --- 2. Defining the formula and fitting the model --- ####
### --- Random walk order 1 --- ###
formula.rw1.tk = y ~ -1 + f(time, 
                            model  = "rw1", 
                            cyclic = TRUE)

modelo.rw1.tk <- inla(formula.rw1.tk, 
                      family          = "binomial", 
                      Ntrials         = n, 
                      data            = Tokyo, 
                      control.compute = list(dic = TRUE, waic=TRUE, cpo=TRUE))
summary(modelo.rw1.tk)


### --- Random walk order 2 --- ###
formula.rw2.tk = y ~ -1 + f(time, 
                            model  = "rw2", 
                            cyclic = TRUE)

modelo.rw2.tk <- inla(formula.rw2.tk, 
                      family          = "binomial", 
                      Ntrials         = n, 
                      data            = Tokyo, 
                      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
summary(modelo.rw2.tk)

### --- 4. Posterior distribution of pi_i --- ####
### ----- 4.1. Rw1 --- ####
result_rw1 <- data.frame(rw1_mean     = modelo.rw1.tk$summary.fitted.values$mean,
                     rw1_025quant = modelo.rw1.tk$summary.fitted.values$`0.025quant`,
                     rw1_975quant = modelo.rw1.tk$summary.fitted.values$`0.975quant`)
result_rw1 <- cbind(time = Tokyo$time, result_rw1)

plot_rw1 <- ggplot(result_rw1, aes(x = time)) +
  geom_ribbon(aes(ymin = rw1_025quant,
                  ymax = rw1_975quant),
              fill = "grey70") +
  geom_line(aes(y = rw1_mean), 
            col = "blue4") +
  ylab("Probability") +
  ggtitle("RW1") +
  theme(plot.title = element_text(hjust = 0.5))
plot_rw1

### ----- 4.2. Rw2 --- ####
result_rw2 <- data.frame(rw2_mean     = modelo.rw2.tk$summary.fitted.values$mean,
                         rw2_025quant = modelo.rw2.tk$summary.fitted.values$`0.025quant`,
                         rw2_975quant = modelo.rw2.tk$summary.fitted.values$`0.975quant`)
result_rw2 <- cbind(time = Tokyo$time, result_rw2)

plot_rw2 <- ggplot(result_rw2, aes(x = time)) +
  geom_ribbon(aes(ymin = rw2_025quant,
                  ymax = rw2_975quant),
              fill = "grey70") +
  geom_line(aes(y = rw2_mean), 
            col = "red4") +
  ylab("Probability") +
  ggtitle("RW2") +
  theme(plot.title = element_text(hjust = 0.5))

#png("rws.png", width = 1000, height = 700, res = 150)
grid.arrange(plot_rw1, plot_rw2)
#dev.off()

### --- BEST MODEL --- ###
resumen<-data.frame(DIC=c(modelo.rw1.tk$dic$dic, modelo.rw2.tk$dic$dic),
                    WAIc=c(modelo.rw1.tk$waic$waic, modelo.rw2.tk$waic$waic),
                    LCPO=c(-mean(log(modelo.rw1.tk$cpo$cpo)), - mean(log(modelo.rw2.tk$cpo$cpo))))
rownames(resumen)<-c("modelo.rw1.tk", "modelo.rw2.tk")
resumen


