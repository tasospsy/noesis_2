# -------------------------------------------------------------------------
# ADDITIONAL ANALYSIS -----------------------------------------------------
# -------------------------------------------------------------------------

# Set Working Directory ---------------------------------------------------
setwd("~/")

# Required Sources --------------------------------------------------------
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/Background_Functions.R"))

# Load Packages -----------------------------------------------------------
Packages <- c("psychonetrics", "patchwork", "qgraph", "MASS", "dplyr", "GPArotation", "ucminf", "tidyverse")
invisible(lapply(Packages, installpackages))

# Load data ---------------------------------------------------------------
load(url("https://github.com/tasospsy/noesis/blob/main/WAIS_US.Rdata?raw=true"))
load(url("https://github.com/tasospsy/noesis/blob/main/WAIS_Hungary.Rdata?raw=true"))
load(url("https://github.com/tasospsy/noesis/blob/main/WAIS_Germany.Rdata?raw=true"))

## A list with all data sets and the sample sizes 
WAIS <- list(Data = list(US = WAIS_US, GER = WAIS_Germany, HUN = WAIS_Hungary),
             size = list(US = 1800, GER = 1425, HUN = 1112))

# -------------------------------------------------------------------------

## Observed variables
obsvars <- rownames(WAIS$Data$US)

## Number of observed variables 
nobs <- length(obsvars)


# -------------------------------------------------------------------------
# EFA ---------------------------------------------------------------------

efa1 <- factanal(covmat=WAIS$Data$US, 
         factors = 4, 
         n.obs = WAIS$size$US)
efa1$loadings

#efa3 <- lavaan::efa(sample.cov = WAIS$Data$US,
#            sample.nobs = WAIS$size$US,
#            nfactors = 4,
#            ov.names = obsvars,
#            rotation = 'varimax')
#summary(efa3)
# -------------------------------------------------------------------------

## remove factor loadings < .30
efa1$loadings[efa1$loadings < .3] <- NA
## lambda structure
lambda_EFA <- t(apply(efa1$loadings, 1, function(x) ifelse(is.na(x), 0, 1)))

# -------------------------------------------------------------------------
# CFA ---------------------------------------------------------------------
# -------------------------------------------------------------------------

cfa_extra_mod <- lvm(lambda = lambda_EFA,
                  sigma_zeta = "empty", # Ψ
                  covs = ((WAIS$size$GER - 1) / 
                    (WAIS$size$GER * WAIS$Data$GER)),
                  nobs = WAIS$size$GER,
                  identification = "variance",
                  optimizer = "ucminf") %>% runmodel

saveRDS(cfa_extra_mod, '~/Google Drive/_UvA/Research Internship/BifactorLab/veltiosis/Extra_analysis/cfa_extra_mod.RDS')
## Get Sigma
#extra_Sigma <- GetCorMat(cfa_add_mod)
#
#
## -------------------------------------------------------------------------
## generate data -----------------------------------------------------------
#
#Test_raw <- replicate(n = 1000,                            
#                      mvrnorm(n = WAIS$size$GER,  
#                              mu = rep(0, nobs),  
#                              Sigma = add_Sigma,
#                              empirical = FALSE),
#                      simplify = FALSE)
#
## -------------------------------------------------------------------------
#
## Perform the simulations
#startt <- Sys.time()
#out <- lapply(Rawdata, lapply, function(d) mapply(ModelsFitFun,
#                                                  args$fitHF,
#                                                  args$fitBF,
#                                                  args$fitNW,
#                                                  MoreArgs = list(Raw = T,
#                                                                  dat = d),
#                                                  SIMPLIFY = FALSE))
#endt <- Sys.time()
#endt-startt
