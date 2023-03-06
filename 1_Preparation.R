# -------------------------------------------------------------------------
## SUPPLEMENTARY MATERIAL for the article:
## "A network perspective on why bifactor models outperform
## higher order g factor models", by
## Psychogyiopoulos, A., Groot, L.J., Ten Hove, D., De Jonge, H., Kan, K.J.
# -------------------------------------------------------------------------
# 1. Preparation ----------------------------------------------------------
# -------------------------------------------------------------------------

# Set Working Directory ---------------------------------------------------
setwd("~/")

# Required Sources --------------------------------------------------------
source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/Background_Functions.R"))

# Load Packages -----------------------------------------------------------
Packages <- c("psychonetrics", "patchwork", "qgraph", "MASS", "dplyr", "GPArotation", "ucminf", "tidyverse")
invisible(lapply(Packages, install_or_source_lib))

# Load data ---------------------------------------------------------------
load(url("https://github.com/tasospsy/noesis/blob/main/WAIS_US.Rdata?raw=true"))
load(url("https://github.com/tasospsy/noesis/blob/main/WAIS_Hungary.Rdata?raw=true"))
load(url("https://github.com/tasospsy/noesis/blob/main/WAIS_Germany.Rdata?raw=true"))

## A list with all data sets and the sample sizes 
WAIS <- list(Data = list(US = WAIS_US, GER = WAIS_Germany, HUN = WAIS_Hungary),
             size = list(US = 1800, GER = 1425, HUN = 1112))

## Observed variables
obsvars <- rownames(WAIS$Data$US)

## Number of observed variables 
nobs <- length(obsvars)

# NETWORK MODEL (exploratory analysis) ------------------------------------
## Exploratory search
GGM_search(dat = WAIS, 
           from = "US",
           alpha_prune = 0.01,
           fast = TRUE)
# Note: the above function gives automatically an object "adj.matrix"
# in the global environment.


# FACTOR MODELS -----------------------------------------------------------
## Latent variables
lvars <- c("P", # Perceptual Organization
           "V", # Verbal Ability
           "W", # Working Memory
           "S") # Processing Speed 

## Number of latent variables
nlvars <- length(lvars)

# ---- Pattern matrix of factor loadings
lambda.t <- matrix(c(
# V  P  W  S
  0, 1, 0, 0, #BD: Block Design
  1, 0, 0, 0, #SI: Similarities
  0, 0, 1, 0, #DS: Digit Span
  0, 1, 0, 0, #MR: Matrix Reasoning
  1, 0, 0, 0, #VC: Vocabulary
  0, 0, 1, 0, #AR: Arithmetic
  0, 0, 0, 1, #SS: Symbol Search
  0, 1, 0, 0, #VP: Visual Puzzles
  1, 0, 0, 0, #IN: Information
  0, 0, 0, 1, #CD: Coding
  0, 0, 1, 0, #LN: Letter-Number Seq.
  0, 1, 0, 0, #FW: Figure Weights
  1, 0, 0, 0, #CO: Comprehension
  0, 0, 0, 1, #CA: Cancellation
  0, 1, 0, 0  #PC: Picture Comprehension
),
nrow = nobs,
ncol = nlvars, 
byrow = TRUE,
dimnames = list(obsvars, lvars)
)

# Higher-order Factor Model (hmodel) --------------------------------------
## Adding a second-order general factor 'g' to the lambda matrix 
lambda_h <- cbind(lambda.t, g = 0)

## Adding factor loading from 'g' to latent variables
beta_h   <- matrix(0, nlvars + 1, nlvars + 1)
beta_h[1:nlvars, nlvars + 1] <- 1  

# Bi-factor Model (BFmodel) -----------------------------------------------
## Adding factor loading from 'g' to observed variables
lambda_bi <- cbind(lambda.t, g = 1)

## Fit all 3 models to the GERMAN WAIS correlation Matrix 
ModelsFitFun(HFmodel = TRUE,
             BFmodel = TRUE,
             NWmodel = TRUE,
             Raw = FALSE,
             dat = WAIS,
             which = "GER")

## Create a list for the 3 models
modslist <- list(HF = hmodel, BF = BFmodel, NW = NWmodel)

## Chisq Difference test for the three models
testmods <- psychonetrics::compare(hmodel, BFmodel, NWmodel)

# Model Vizualization -----------------------------------------------------
# Plot Network model ------------------------------------------------------
Omega <- getmatrix(NWmodel, 'omega')

library("qgraph")
set.seed(1992)
qgraph(Omega, 
       labels = obsvars,
       groups = list( V  = which( lambda.t[ , 1 ] == 1 ),
                      PO = which( lambda.t[ , 2 ] == 1 ),
                      WM = which( lambda.t[ , 3 ] == 1 ),
                      PS = which( lambda.t[ , 4 ] == 1 )),
       layout = 'spring',
       theme = "colorblind",
       color = c('#7aafdf',
                 '#d7ae9d',
                 '#89cd84',
                 '#ede164'),
       edge.color = 'black'
)

 # -------------------------------------------------------------------------
# Extra -------------------------------------------------------------------
library(kableExtra)

# Produce a table for the LRT test 
kbl(testmods, 
    caption = '',
    booktabs = T,
    format = 'latex') %>% 
  kable_styling() %>% 
  footnote(general = "")

## Print Sigmas (modelimplied var-cov matrices) 
Smats <- lapply(modslist, GetCorMat)

# Produce latex tables for tthe Sigmas
tbsS <- c()
for(i in 1:length(Smats)) {
tbsS[i] <- kbl(Smats[[i]] %>% round(.,2),
    caption = paste(names(Smats)[i],'Model Implied Correlation Matrix'),
    booktabs = TRUE, format = 'latex') %>% 
    kable_styling(full_width = F) %>% 
    footnote(general = "test ")
cat(tbsS[i])
}

# END 1.Preparation -------------------------------------------------------
