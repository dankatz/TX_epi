#playing around with lags
library(ggplot2)
library(dplyr)

#####creating a dataset without any lags
n <- 1500
beta0 <- 1.5
beta1 <- 0.7

#generate covariate values
x1 <- runif(n=n, min=0, max=1)
e <- rnorm(n =n, mean = 0, sd = 0.5)

#compute mu's
mu <- exp(beta0 + beta1 * x1 + e)

#generate Y-values
y <- rpois(n=n, lambda=mu)

#data set
data <- data.frame(y=y, x=x1)
ggplot(data, aes(x= x1, y = y)) + geom_point() + geom_smooth(method = "lm")

fit1 <- glm(y ~ x1, data = data, family = "poisson"); summary(fit1)



##### creating a dataset with just one lag ################
n <- 1500
beta0 <- 1.5
beta1 <- 0.7

#generate covariate values
x1 <- runif(n=n, min=0, max=1)
e <- rnorm(n =n, mean = 0, sd = 0.5)


x1_lag <- rep(NA, n)
for(i in 6:n){x1_lag[i] <- x1[i-5]}

#compute mu's
mu <- exp(beta0 + beta1 * x1 + e + x1_lag)

#generate Y-values
y <- rpois(n=n, lambda=mu)

#data set
data <- data.frame(y=y, x=x1)
ggplot(data, aes(x= x1_lag, y = y)) + geom_point() + geom_smooth(method = "lm")

fit1 <- glm(y ~ x1, data = data, family = "poisson"); summary(fit1)



##### creating a dataset with tapering lags #############
n <- 1500
beta0_intercept <- 0.6
beta1_lag0 <- 0.0
beta1_lag1 <- 0.5
beta1_lag2 <- 0.4
beta1_lag3 <- 0.3
beta1_lag4 <- 0.2
beta1_lag5 <- 0.1


#generate covariate values
x1 <- runif(n=n, min=0, max=1)
x1_lag0 <- x1
x1_lag1 <- dplyr::lag(x1, 1)
x1_lag2 <- lag(x1, 2)
x1_lag3 <- lag(x1, 3)
x1_lag4 <- lag(x1, 4)
x1_lag5 <- lag(x1, 5)
e <- rnorm(n =n, mean = 0, sd = 0.5)

# lag_weights <- c(0.5, 0.4, 0.3, 0.2, 0.1)
# x1_lag <- rep(NA, n)
# for(i in 6:n){x1_lag[i] <- 
#   lag_weights[1] * x1_lag1[i] + #current day (t0) 
#   lag_weights[2] * x1_lag2[i] + #previous day (t-1) 
#   lag_weights[3] * x1_lag3[i] + 
#   lag_weights[4] * x1_lag4[i] + 
#   lag_weights[5] * x1_lag5[i] 
# }

#compute mu's
mu <- exp(beta0_intercept + 
          + beta1_lag0 * x1_lag0 
          + beta1_lag1 * x1_lag1  
          + beta1_lag2 * x1_lag2 
          + beta1_lag3 * x1_lag3 
          + beta1_lag4 * x1_lag4 
          + beta1_lag5 * x1_lag5 
          + e )
mu[is.na(mu)] <- 5
#generate Y-values
y <- rpois(n=n, lambda=mu)
y2 <- rpois(n =n, lambda = (exp(beta0_intercept)))
mean(y); mean(y2); mean(y) - mean(y2)
#data set
data <- data.frame(obs_n = 1:length(y), y, x1, x1_lag0, x1_lag1, x1_lag2, x1_lag3, x1_lag4, x1_lag5)

#ggplot(data, aes(x= x1_lag1, y = y)) + geom_point() + geom_smooth(method = "lm")
fit1 <- glm(y ~ x1_lag0 + x1_lag1 + x1_lag2 + x1_lag3 + x1_lag4 + x1_lag5, data = data, family = "poisson"); summary(fit1)

ggplot(data, aes(x= obs_n, y = y)) + geom_point(color = "red") + geom_line(color = "red")+ coord_cartesian(xlim = c(0,30)) +
  #geom_line(aes(x = obs_n -0, y = x1 * 20), color = "blue") +
  geom_line(aes(x = obs_n -1, y = x1 * 20), color = "light blue") + 
  # geom_line(aes(x = obs_n -2, y = x1 * 20), color = "green") +
  # geom_line(aes(x = obs_n -3, y = x1 * 20), color = "light green") +
  theme_bw()



### trying out the dlnm package #####################################
library("dlnm")
library(splines)

#the built in example
#https://cran.r-project.org/web/packages/dlnm/vignettes/dlnmTS.pdf
head(chicagoNMMAPS)
cb1.pm <- crossbasis(chicagoNMMAPS$pm10, lag=15, argvar=list(fun="lin"),
                     arglag=list(fun="poly",degree=4))
cb1.temp <- crossbasis(chicagoNMMAPS$temp, lag=3, argvar=list(df=5),
                       arglag=list(fun="strata",breaks=1))

model1 <- glm(death ~ cb1.pm + cb1.temp + ns(time, 7*14) + dow,
              family=quasipoisson(), chicagoNMMAPS)

pred1.pm <- crosspred(cb1.pm, model1, at=0:20, bylag=0.2, cumul=TRUE)

plot(pred1.pm, "slices", var=10, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
       main="Association with a 10-unit increase in PM10")
plot(pred1.pm, "slices", var=10, col=2, cumul=TRUE, ylab="Cumulative RR",
       main="Cumulative association with a 10-unit increase in PM10")

#trying with the simulated data
data2 <- dplyr::select(data, timestep = obs_n, x = x1, y) %>% slice(1:1500) %>% 
  mutate(x2 = scale(x)) #centered values divided by one SD
head(data2)
hist(data2$y)
hist(data2$x2)
x2_min <- min(data2$x2)
x_lag <- crossbasis(data2$x2, lag = 10, 
                    #argvar = list(fun = "ns"), arglag = list())
                    #argvar=list(fun = "thr", side = "d"))
                    argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 4))
          # crossbasis(data2$x2, lag=5, argvar=list(df=1),
          #   arglag=list(fun="strata",breaks=5))
summary(x_lag)
model1 <- glm(y ~ x_lag, family = quasipoisson, data = data2)
summary(model1)

pred1.pm <- crosspred(x_lag, model1, at = 1, bylag = 0.2, cen = x2_min, cumul = TRUE)
plot(pred1.pm, "slices", var=1, cumul = TRUE, 
     main="Association with a 1-unit increase in independent variable")

plot(pred1.pm, "slices", var=0.5, col=2, cumul=TRUE, ylab="Cumulative RR",
     main="Cumulative association with a 10-unit increase in PM10")

#### trying out Wells 2016 bayesian approach to distributed lags (following Welty et al. 2009) #####################
#using data from simulation run above
library(rjags)

data2 <- dplyr::select(data, timestep = obs_n, x = x1, y) %>% slice(1:1500) %>% 
  mutate(x2 = scale(x)) #centered values divided by one SD
data2$x2[10] <- NA#data2$x2[10]
head(data2)

scale(data2$x)
hist(data2$x2)

sink("model_a.txt")
cat("  
  ### model ########### 
    model{
    
    # assign missing data to its distribution
    for(d in 1:N){
    x[d] ~ dnorm(x_mean, x_prec)
    #x_prec <- 1/(x_sd * x_sd)
    }

    
    ## Likelihood
    for(d in 6:N){
    
      y[d] ~ dpois(lambda[d])

      log(lambda[d]) <- 
      intercept +
      lag_temp[d]
      
      for(t in 1: NLag_Env){
        lagS_temp[d,t] <- x[d - (t - 1)] * 
                            theta_temp[t]	
      }
    
      lag_temp[d] <- sum(lagS_temp[d,])    
      }     
      
    ## Priors 
    
      intercept ~ dnorm(0, 0.0001)
      X2_temp ~ dnorm(0, 1)

      for(t in 1:NLag_Env){
        X1_temp[t] ~ dnorm(0,1)
        l.dist_temp[t] <- max(t, pi_temp)
        l.weight_temp[t] <- exp(eta2_temp * l.dist_temp[t])
        l.var_temp[t] <- exp(eta1_temp * l.dist_temp[t] / 2) * sigma_temp[ind_temp]
        theta.prime_temp[t] <- l.weight_temp[t] * X1_temp[t] + (1 - l.weight_temp[t]) * X2_temp
        theta_temp[t] <- theta.prime_temp[t] * l.var_temp[t]
      }
      
      pi_temp ~ dunif(0, NLag_Env)
      eta1_temp ~ dunif(-1,-0.00001)
      eta2_temp ~ dunif(-1, -0.00001)
      sigma_temp[2] ~ dunif(0.0001, 100)
      sigma_temp[1] <- 0
      ind_temp <- sel_temp + 1
      sel_temp ~ dbern(0.5)
 }
 ",fill=TRUE)
sink() 

jags <- jags.model('model_a.txt', 
                   data = list(
                     #general stuff
                     #timestep = as.numeric(data2$t),
                     x = (as.numeric(data2$x2)),
                     x_mean = mean(data2$x2, na.rm = TRUE),
                     x_sd = sd(data2$x2, na.rm = TRUE),
                     x_prec = 1/(sd(data2$x2, na.rm = TRUE) * sd(data2$x2, na.rm = TRUE)),
                     y = as.numeric(data2$y),
                     N = nrow(data2),
                     NLag_Env = 5
                     ),
                   n.chains = 3,
                   n.adapt = 100)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter= 2000) #update(jags,n.iter=1000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("theta_temp"),  n.iter = 2000) #variables to monitor
mcmc_samples_params <- coda.samples(jags, variable.names=c("x"),  n.iter = 10) #variables to monitor
plot(mcmc_samples_params)
Sys.time() #started at noon
results_param <- summary(mcmc_samples_params)
results_params <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params$parameter<-row.names(results_params)
results_params$param<-substr(results_params$parameter,1,2)

filter(results_params, param == "al" | param == "be") %>%
  ggplot(aes(x = parameter, y = Mean, ymax = X97.5., ymin = X2.5.)) + geom_pointrange() + theme_bw() +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  ylab("parameter estimate")



library(raster)
tgb <- raster("E:/lidar/2017 LiDAR/trees_ground_build_v2_UTM17_v2_within_D.tif")
summary(tgb)
sampleRandom(tgb, size = 10000) %>%
  as.factor()%>%
  summary()
2160/(10000)

test <- crop(tgb, )
plot(test)
small <- extent(tgb)
small@xmin <- 320000
