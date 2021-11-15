## - Bi factor lab
## -- Fit propensity
## -- Tasos
## -- 07/11/2021

# ---- load data 
load(url("https://github.com/tasospsy/noesis/blob/main/WAIS_US.Rdata?raw=true"))
## Sample size
N <- 1800
## Observed variables
obsvars <- rownames(WAIS_US)
## Number of observed variables 
nobs <- length(obsvars)

## ----------------------------------
## FIT PROPENSITY in Bi-FACTOR MODELS 
##      of GENERAL INTELLIGENCE
## ----------------------------------
library(ockhamSEM)

# Create a temp matrix
# p <- 15 # number of variables
# temp_mat <- diag(p) # identity matrix
# dimnames(temp_mat) <- list(obsvars,obsvars)

## Model Specification
## Bifactor model
bimodel <- '
          g =~ SI + VC + IN + CO + BD + MR + VP + PC+ AR + LN + DS + FW  + SS + CD + CA
          V =~ SI + VC + IN + CO
          P =~ BD + MR + VP + PC 
          WM =~ DS + AR + LN + FW
          S =~ SS + CD + CA'
## Higher-order factor model
hmodel <- '
          V =~ SI + VC + IN + CO
          P =~ BD + MR + VP + PC + FW
          WM =~ FW + AR + LN + DS 
          S =~ SS + CD + CA
          g =~ V + P + WM + S'
## Oblique factor model w/ four correlated common factors
obliq <- 'V =~ SI + VC + IN + CO
          P =~ BD + MR + VP + PC + FW
          WM =~ FW + AR + LN + DS 
          S =~ SS + CD + CA'

## Model Estimation
bi.fit <- cfa(bimodel, sample.cov = WAIS_US, 
              sample.nobs = N,
              orthogonal = TRUE, std.lv = TRUE,
              optim.force.converged = TRUE) #!
h.fit <- cfa(hmodel, sample.cov = WAIS_US, 
             sample.nobs = N, std.lv = TRUE,
             optim.force.converged = TRUE) #!
obl.fit <- cfa(obliq, sample.cov = WAIS_US, 
             sample.nobs = N, std.lv = TRUE,
             optim.force.converged = TRUE) #!


# Test Fit Propensity using
# 1000 random matrices w/ "onion" method
library(parallel)
cores <- detectCores()-1
cl <- makeCluster(cores)
startt <- Sys.time()
res1k <- run.fitprop(bi.fit, h.fit, obl.fit,
                     fit.measure=c("rmsea","cfi", "tli", "nfi"),
                     rmethod="onion", reps=1000, cluster = cl)
endt <- Sys.time()
endt - startt
# Time difference of 2.344544 hours
stopCluster(cl)

summary(res1k)
plot(res1k)

## --- MCMC 
# library(parallel)
# cl <- makeCluster(6)
# res.mcmc <- run.fitprop(mod1a.fit,mod2a.fit,fit.measure="srmr",
#                         rmethod = "mcmc", reps = 5000, onlypos=TRUE,
#                         cluster=cl)
# stopCluster(cl)
# plot(res.mcmc)


