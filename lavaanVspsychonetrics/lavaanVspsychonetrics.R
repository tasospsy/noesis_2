## - Bi factor lab
## -- lavaan Vs psychonetrics
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

## ------
## Lavaan
## ------

bimodel <- '
          g =~ SI + VC + IN + CO + BD + MR + VP + PC+ AR + LN + DS + FW  + SS + CD + CA
          V =~ SI + VC + IN + CO
          P =~ BD + MR + VP + PC 
          WM =~ DS + AR + LN + FW
          S =~ SS + CD + CA'

pseudo.ucmnif <- cfa(bimodel, sample.cov = WAIS_US, 
                     sample.nobs = N,
                     orthogonal = TRUE, std.lv = TRUE, 
                     control = list(eval.max = 500, #y
                                    iter.max = 10000L,
                                    rel.tol = 1e-6, # y
                                    x.tol = 1e-12, #y
                                    step.min = 2.2e-14),
                     optim.force.converged = TRUE)

using.nlminb <- cfa(bimodel, sample.cov = WAIS_US, 
                    sample.nobs = N,
                    orthogonal = TRUE, std.lv = TRUE, 
                    optim.force.converged = TRUE)

using.bfgs <- cfa(bimodel, sample.cov = WAIS_US, 
                  sample.nobs = N,
                  orthogonal = TRUE, std.lv = TRUE, 
                  optim.method = "BFGS")

lavaan.bis <- c(pseudo.ucmnif, using.nlminb, using.bfgs)
anova(pseudo.ucmnif, using.nlminb, using.bfgs)

## Extracting residual factor variances
lavaan.Thetas <- lapply(lavaan.bis, 
                        function(x) lavInspect(x, "est")$theta[obsvars, obsvars]) %>% 
  `names<-`(c("pseudo.ucmnif", "using.nlminb", "using.bfgs")) %>% 
  lapply(., function(x) diag(x))

## Plotting BF models
library(semPlot)
sapply(lavaan.bis, function(x) semPaths(x, 
                                        what = "std",
                                        whatLabels = "est",
                                        style = "lisrel"))

#bi2 <- cbind(bimodel, 'DS ~~ v*DS
#             AR ~~ v*AR
#             LN ~~ v*LN
#             FW ~~ v*FW')
#using.nlminb.EQ <- cfa(bi2, sample.cov = WAIS_US, 
#                       sample.nobs = N,
#                       orthogonal = TRUE, std.lv = TRUE)
#semPaths(using.nlminb.EQ, 
#         what = "std",
#         whatLabels = "est",
#         style = "lisrel")

# -- ------------- 
# -- PSYCHONETRICS 
# -- ------------- 

library(psychonetrics)
library(dplyr)
## Latent variables
lvars <- c("V", "P", "W", "S")
## Number of latent variables
nlvars <- length(lvars)

# ---- Pattern matrix of factor loadings
lambda <- matrix(c(
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
  0, 0, 0, 1, #CA: Cancelation
  0, 1, 0, 0  #PC: Picture Comprehension
),
nrow = nobs,
ncol = nlvars, 
byrow = TRUE,
dimnames = list(obsvars, lvars)
)
lambda_bi <- cbind(lambda, g = 1)

pschn.ucminf <- lvm(lambda = lambda_bi,
                    sigma_zeta = "empty", # Ψ
                    covs = (N - 1)/
                      N*WAIS_US,
                    nobs = N,
                    identification = "variance", 
                    optimizer = "ucminf") %>% 
  runmodel() 
pschn.ucminf.f <-fit(pschn.ucminf)

pschn.nlminb <- lvm(lambda = lambda_bi,
                    sigma_zeta = "empty", # Ψ
                    covs = (N - 1)/
                      N*WAIS_US,
                    nobs = N,
                    identification = "variance", 
                    optimizer = "nlminb") %>% 
  runmodel()
pschn.nlminb.f <-fit(pschn.nlminb)

pschn.bfgs <- lvm(lambda = lambda_bi,
                  sigma_zeta = "empty", # Ψ
                  covs = (N - 1)/
                    N*WAIS_US,
                  nobs = N,
                  identification = "variance", 
                  optimizer = "cpp_BFGS") %>% 
  runmodel() 
pschn.bfgs.f <-fit(pschn.bfgs)

compare(pschn.ucminf, pschn.nlminb, pschn.bfgs )

pschn.bis <- c(pschn.ucminf, pschn.nlminb, pschn.bfgs)


pschn.Thetas <- lapply(pschn.bis,
                       function(x)getmatrix(x, "sigma_epsilon")) %>% 
  `names<-`(c("pschn.ucminf", "pschn.nlminb", "pschn.bfgs")) 
for (i in 1:3){
  dimnames(pschn.Thetas[[i]]) <- list(obsvars, obsvars)
}
pschn.Thetas <- lapply(pschn.Thetas, function(x) diag(x))

