## Internship project
## Tasos Psychogyiopoulos
## 2_nSIMULATIONS
## c.15/3/2021

# ---- required sources
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/1_Preparation.R"))

## -- Data generation procedure
set.seed(1992) # Set a seed
Rawdata <- replicate(n = 1000,                            # 1000 replications
                     lapply(lapply(modslist, GetCorMat),  # Get sigmas inside 
                            function(s){
                              mvrnorm(n = WAIS$size$GER,  # N = 1425 
                                      mu = rep(0, nobs),  # nobs = 15
                                      Sigma = s,
                                      empirical = FALSE)
                            }
                     ),
                     simplify = FALSE)

# save(Rawdata, file = "Rawdata.Rda")

## -- Simulations
# Let's specify the arguments to combine later with mapply function
args <-  list(fitHF = c(HFmodel = T, BFmodel = F, NWmodel = F),
              fitBF = c(HFmodel = F, BFmodel = T, NWmodel = F),
              fitNW = c(HFmodel = F, BFmodel = F, NWmodel = T))

# Perform the simulations
startt <- Sys.time()
out <- lapply(Rawdata, lapply, function(d) mapply(ModelsFitFun,
                                                  args$fitHF,
                                                  args$fitBF,
                                                  args$fitNW,
                                                  MoreArgs = list(Raw = T,
                                                                  dat = d),
                                                  SIMPLIFY = FALSE))
endt <- Sys.time()
endt-startt
#  save(out, file = "out.Rda")


# NOTE:
# Time difference of 53.93103 mins

