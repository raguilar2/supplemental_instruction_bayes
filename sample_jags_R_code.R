# This is sample Jags and R code for the Supplemental Instruction project that 
# was conducted with a Bayesian framework. The code works by creating a txt 
# file containing Jags code that specifies the models and priors. The R code 
# sets the initial values, parameters, data used, numbers of iterations, and 
# other arguments. For the sake of space, only the code for different models 
# was included since the number of iterations can be changed in the code 
# provided below. 

## Jags Code for Model with Normal Priors with Both Random Intercept Terms
sink("projectmodel.txt")
cat("
model{
  for(i in 1:N) {
    y[i] ~ dbern(p[i])
    logit(p[i]) <- inprod(x[i,],alpha[]) + beta1[term[i]] + beta2[class[i]]
  }
  for(j in 1:J) {
    alpha[j] ~ dnorm( m[j], prec[j] )
    alphastep[j] <- step(alpha[j])
  }
  for(k in 1:K) {
    beta1[k] ~ dnorm(0, dinv1)
  }
  for(l in 1:L) {
    beta2[l] ~ dnorm(0, dinv2)
  }	
  
  dinv1 ~ dgamma(da1,db1) 
  dinv2 ~ dgamma(da2,db2)
}", fill = TRUE)
sink()

## Sample R Code for Parameters in Normal Model with Both Random Intercept Terms

set.seed(234)
sidata = list(y = mod$dfw, term = mod$term, class = mod$class, 
              x = matrix(data = c(rep(1, 12730),
                                  mod[, 1]-mean(mod[, 1]), 
                                  mod[, 2]-mean(mod[, 2]), 
                                  mod[, 7], mod[, 8], mod[, 9]), 
                         byrow = F, ncol = 6), 
              N = 12730, J = 6, K = 11, L = 19, 
              da1 = 8, db1 = 18, da2 = 8, db2 = 18, 
              m = c(-1.39, -1.11, -0.69, 0.0, -0.69, 0.095), 
              prec = c(1, 4, 4, 4, 16, 16))

siinits = rep(list(list(alpha = c(-1.39, -1.11, -0.69, 0.0, -0.69, 0.095), 
                        beta1 = as.vector(rnorm(11)), 
                        beta2 = as.vector(rnorm(19)),
                        dinv1=2.25, dinv2 = 2.25)), 5)

siparameters = c("alpha", "alphastep", "beta1", "beta2",
                 "dinv1", "dinv2")

si.sim_50 = jags(sidata, siinits, siparameters, "projectmodel.txt", 
                 n.chains = 5, n.iter = 55000, n.burnin = 0, n.thin = 1)

Output_50 = AddBurnin(si.sim_50$BUGSoutput$sims.array, 
                      burnin = 5000, n.thin = 1)

## Jags Code for Model with t-Distribution Priors with Both Random Intercept Terms

sink("projectmodelt.txt")
cat("
model{
  for(i in 1:N) {
    y[i] ~ dbern(p[i])
    logit(p[i]) <- inprod(x[i,],alpha[]) + beta1[term[i]] + beta2[class[i]]
  }
  for(j in 1:J) {
    alpha[j] ~ dt( m[j], prec[j], dff)
    alphastep[j] <- step(alpha[j])
  }
  for(k in 1:K) {
    beta1[k] ~ dt(0, dinv1, dft)
  }
  for(l in 1:L) {
    beta2[l] ~ dt(0, dinv2, dfc)
  }	
  
  dinv1 ~ dgamma(da1,db1) 
  dinv2 ~ dgamma(da2,db2)
  dff ~ dgamma(2, .1)
  dft ~ dgamma(2, .1)
  dfc ~ dgamma(2, .1)
}", fill = TRUE)
sink()

## Sample R Code for Parameters in T Model with Both Random Intercept Terms

set.seed(234)
sidata_t = list(y = mod$dfw, term = mod$term, class = mod$class, 
                x = matrix(data = c(rep(1, 12730), 
                                    mod[, 1]-mean(mod[, 1]), 
                                    mod[, 2]-mean(mod[, 1]), 
                                    mod[, 7], mod[, 8], mod[, 9]), 
                           byrow  = F, ncol  = 6), 
                N = 12730, J = 6, K = 11, L = 19, 
                da1 = 8, db1 = 18, da2 = 8, db2 = 18, 
                m=c(-1.39, -1.11, -0.69, 0.0, -0.69, 0.095), 
                prec = c(1, 4 ,4, 4, 16, 16))

siinits_t = rep(list(list(alpha = c(-1.39, -1.11, -0.69, 0.0, -0.69, 0.095), 
                          beta1 = as.vector(rnorm(11)), 
                          beta2 = as.vector(rnorm(19)),
                          dinv1 = 2.25, dinv2 = 2.25)), 5)

siparameters_t = c("alpha", "alphastep", "beta1", "beta2", 
                   "dinv1", "dinv2 ")

si.sim_t_50 = jags(sidata_t, siinits_t, siparameters_t, "projectmodelt.txt", 
                   n.chains = 5, n.iter = 55000, n.burnin = 0, n.thin = 1)

Output_t_50 = AddBurnin(si.sim_t_50$BUGSoutput$sims.array, 
                        burnin = 5000, n.thin = 1)

## Jags Code for Model with Normal Priors and No Random Intercept for Term

sink("projectmodel_noterm.txt")
cat("
model{
  for(i in 1:N) {
    y[i] ~ dbern(p[i])
    logit(p[i]) <- inprod(x[i,],alpha[]) + beta2[class[i]]
  }
  for(j in 1:J) {
    alpha[j] ~ dnorm( m[j], prec[j] )
    alphastep[j] <- step(alpha[j])
  }
  
  for(l in 1:L) {
    beta2[l] ~ dnorm(0, dinv2)
  }	
  
  dinv2 ~ dgamma(da2,db2)
}", fill = TRUE)
sink()

## Sample R Code for Parameters in Normal Model and No Random Intercept for Term

set.seed(234)
sidata_noterm = list(y = mod$dfw, class = mod$class, 
                     x = matrix(data = c(rep(1, 12730), 
                                         mod[, 1]-mean(mod[, 1]), 
                                         mod[, 2]-mean(mod[, 2]), 
                                         mod[, 7], mod[, 8], mod[, 9]), 
                                byrow = F, ncol = 6), 
                     N = 12730, J = 6, L = 19, da2 = 8.0, db2 = 18.0, 
                     m = c(-1.39, -1.11, -0.69, 0.0, -0.69, 0.095), 
                     prec = c(1, 4, 4, 4, 16, 16))

siinits_noterm = rep(list(list(alpha = c(-1.39, -1.11, -0.69, 
                                         0.0, -0.69, 0.095),  
                               beta2 = as.vector(rnorm(19)), 
                               dinv2 = 2.25)), 5)

siparameters_noterm = c("alpha", "alphastep", "beta2", "dinv2")


si.sim_noterm = jags(sidata_noterm, siinits_noterm, siparameters_noterm,
                     "projectmodel_noterm.txt", n.chains = 5, n.iter = 55000, 
                     n.burnin = 0, n.thin = 1)

Output_noterm = AddBurnin(si.sim_noterm$BUGSoutput$sims.array,
                          burnin = 5000, n.thin = 1)

## Jags Code for Model with t-Distribution Priors and No Random Intercept for Term

sink("projectmodelt_noterm.txt")
cat("
model{
  for(i in 1:N) {
    y[i] ~ dbern(p[i])
    logit(p[i]) <- inprod(x[i,],alpha[]) + beta2[class[i]]
  }
  for(j in 1:J) {
    alpha[j] ~ dt( m[j], prec[j], dff)
    alphastep[j] <- step(alpha[j])
  }
  
  for(l in 1:L) {
    beta2[l] ~ dt(0, dinv2, dfc)
  }	
  
  dinv2 ~ dgamma(da2,db2)
  dff ~ dgamma(2, .1)
  dfc ~ dgamma(2, .1)
  
}", fill = TRUE)
sink()





## Sample R Code for Parameters in T Model and No Random Intercept for Term

set.seed(234)
sidata_noterm = list(y = mod$dfw, class = mod$class, 
                     x = matrix(data = c(rep(1 ,12730), 
                                         mod[, 1]-mean(mod[, 1]), 
                                         mod[, 2]-mean(mod[, 1]), 
                                         mod[, 7], mod[, 8], mod[, 9]), 
                                byrow = F, ncol = 6), 
                     N = 12730, J = 6, L = 19, da2 = 8.0, db2 = 18.0, 
                     m = c(-1.39, -1.11, -0.69, 0.0, -0.69, 0.095), 
                     prec = c(1 ,4, 4, 4, 16, 16))

siinits_noterm = rep(list(list(alpha = c(-1.39, -1.11, -0.69, 
                                         0.0, -0.69, 0.095), 
                               beta2 = as.vector(rnorm(19)),
                               dinv2 = 2.25)), 5)

siparameters_noterm = c("alpha", "alphastep", "beta2", "dinv2")

si.sim_t_noterm = jags(sidata_noterm, siinits_noterm, siparameters_noterm,
                       "projectmodelt_noterm.txt", n.chains = 5, n.iter = 55000, 
                       n.burnin = 0, n.thin = 1)

Output_t_noterm = AddBurnin(si.sim_t_noterm$BUGSoutput$sims.array,
                            burnin = 5000, n.thin = 1)
