#' We fit a model with continuous time


### --- 0. Loading libraries --- ####
library(INLA)
library(ggplot2)
library(gridExtra)

### --- 1. Just an example about the knots and weights --- ####
obst <- c(1:2,4:7,10)
n <- length(obst)
knots <- c(2, 5, 8)
k <- length(knots)


mesh1d <- inla.mesh.1d(loc = knots)
wmat <- inla.spde.make.A(mesh = mesh1d, loc = obst)
wmat


inla.mesh.1d.A(mesh1d, loc = obst)
inla.mesh.1d.fem(mesh1d)
inla.mesh.1d.bary(mesh1d, loc = obst)

### --- 1. Loading the data --- ####
data <- readRDS("data/data_cont/data_cont_time.RDS")
p <- ggplot() +
  geom_point(aes(x = x, y = y), data = data, col = "blue4") +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), 
               data = data.frame(x1 = data$x, x2 = data$x, y1 = -1.8, y2 = -1.9),
               color = "red4") +
  xlab("time")


#png("images/data1d_cont.png", width = 1000, height = 600, res = 120)
p
#dev.off()

# png("images/data1d_cont_ex.png", width = 800, height = 500, res = 150)
# p + 
#   xlim(c(0,2)) +
#   ylim(c(0,2)) +
#   geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), 
#                data = data.frame(x1 = data$x, x2 = data$x, y1 = 0, y2 = 0.3),
#                color = "red4") +
#   geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
#                data = data.frame(x1 = 0:2, x2 = 0:2, y1 = 0, y2 = 0.5))
# dev.off()





#Data is simulated in this way
  # set.seed(1)
  # n <- 100
  # obst <- runif(n, 0, 20)
  # obst <- obst[order(obst)]
  # 
  # k <- 20
  # knots <- seq(0, 20, length.out = k + 1)
  # mesh1d <- inla.mesh.1d(loc = knots)
  # wmat <- inla.spde.make.A(mesh = mesh1d,
  #                          loc  = obst)
  # taue <- 5
  # taux <- 3
  # gamma <- 0.7
  # set.seed(1)
  # x <- as.vector(arima.sim(n=k + 1,
  #                          model=list(ar=gamma), sd=sqrt(1/taux)))
  # e <- rnorm(n=n, mean=0, sd=sqrt(1/taue))
  # y <- drop(wmat%*%x) + e
  # 
  # data <- data.frame(x = obst, y = y)
  # saveRDS(data , "data/data_cont/data_cont_time.RDS")
###

### --- 2. Fitting a model using random walk 2 with 20 knots --- ####
### ----- 2.1. Defining the number of knots k = 21 --- ####
summary(data)
#As the data take values between 0 and 50, we define the notes in that range
k <- 20
knots <- seq(0, 20, length.out = k ) 

### ----- 2.2. Create the mesh --- ####
mesh1d <- inla.mesh.1d(loc = knots)
mesh1d
### ----- 2.3. Matrix to project the data in the mesh --- ####
wmat <- inla.spde.make.A(mesh = mesh1d, 
                         loc  = data$x)

wmat[1:7,1:3]
#saveRDS(wmat[1:7,1:3], "rds/matrix_1D.rds")

### ----- 2.4. Fit an randon walk with the weights --- ####
formula <- y ~ -1 + f(i, model = "rw2")
rw2 <- inla(formula, 
             data              = list(y = data$y, i = 1:k),
             control.predictor = list(A = wmat, compute = TRUE),
             control.compute   = list(dic = TRUE))


summary(rw2)

### ----- 2.5. Plot the posterior distribution of the mean --- ####
result_rw2 <- data.frame(rw2_mean     = rw2$summary.fitted.values$mean[1:(dim(data)[1])],
                         rw2_025quant = rw2$summary.fitted.values$`0.025quant`[1:(dim(data)[1])],
                         rw2_975quant = rw2$summary.fitted.values$`0.975quant`[1:(dim(data)[1])])
result_rw2 <- cbind(x = data$x, result_rw2)


plot_rw2 <- ggplot() +
  geom_ribbon(data = result_rw2,
              aes(x = x,
                  ymin = rw2_025quant,
                  ymax = rw2_975quant),
              fill = "grey70") +
  geom_line(data = result_rw2,
            aes(x = x,
                y = rw2_mean), 
            col = "blue4") + 
  ylab("Probability") +
  ggtitle("rw2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data = data,
             aes(x=x, y=y)) +
  xlab("time")

png("images/rw2_cont.png", width = 1000, height = 500,res = 120)
  plot_rw2
dev.off()

### --- 3. Function to fit using a random walk 2 with k knots --- ####
fitting_model <- function(k = 20, color = "blue4"){
  ### ----- 3.1. Defining the number of knots --- ####
  summary(data)
  #As the data take values between 0 and 50, we define the notes in that range
  knots <- seq(0, 20, length.out = k ) 
  
  ### ----- 3.2. Create the mesh --- ####
  mesh1d <- inla.mesh.1d(loc = knots)
  mesh1d
  ### ----- 3.3. Matrix to project the data in the mesh --- ####
  wmat <- inla.spde.make.A(mesh = mesh1d, 
                           loc  = data$x)
  
  wmat[1:5,]
  data$x[1:5]
  
  ### ----- 3.4. Fit an autoregressive model with the weights --- ####
  formula <- y ~ -1 + f(i, model = "rw2")
  rw2 <- inla(formula, 
              data              = list(y = data$y, i = 1:k),
              control.predictor = list(A = wmat, compute = TRUE),
              control.compute   = list(dic = TRUE))
  
  
  summary(rw2)
  
  ### ----- 3.5. Plot the posterior distribution of the mean --- ####
  result_rw2 <- data.frame(rw2_mean     = rw2$summary.fitted.values$mean[1:(dim(data)[1])],
                           rw2_025quant = rw2$summary.fitted.values$`0.025quant`[1:(dim(data)[1])],
                           rw2_975quant = rw2$summary.fitted.values$`0.975quant`[1:(dim(data)[1])])
  result_rw2 <- cbind(x = data$x, result_rw2)
  
  
  plot_rw2 <- ggplot() +
    geom_ribbon(data = result_rw2,
                aes(x = x,
                    ymin = rw2_025quant,
                    ymax = rw2_975quant),
                fill = "grey70") +
    geom_line(data = result_rw2,
              aes(x = x,
                  y = rw2_mean), 
              col = color) + 
    ylab("Probability") +
    ggtitle(paste0("rw2.", "Knots = ", k)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(data = data,
               aes(x=x, y=y))+
    xlab("time")
  
  plot_rw2
  
}
### --- 4. Fitting the model with different knots --- ####
png("images/rws_cont.png", width = 1000, height = 700, res = 100)
grid.arrange(
  fitting_model(k = 10, color = "blue1"),
  fitting_model(k = 20, color = "red1"),
  fitting_model(k = 30, color = "blue2"), 
  fitting_model(k = 40, color = "red2"),
  fitting_model(k = 50, color = "blue3"),
  fitting_model(k = 60, color = "red3"), ncol = 2)
dev.off()  


### --- 5. Using inla.stack --- ####
stk <- inla.stack(
  data = list(y = data$y),
  A    = list(wmat), 
  effects = list(i = 1:k),
  tag = 'est')

formula_stk <- y ~ -1 + f(i, model = "rw2")
rw2_stk <- inla(formula_stk, 
                data              = inla.stack.data(stk),
                control.predictor = list(A = inla.stack.A(stk), compute = TRUE),
                control.compute   = list(dic = TRUE))

rw2_stk$summary.random$i$mean == rw2$summary.random$i$mean


  
