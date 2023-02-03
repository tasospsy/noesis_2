# -------------------------------------------------------------------------
## SUPPLEMENTARY MATERIAL for the article:
## "A network perspective on why bifactor models outperform
## higher order g factor models", by
## Psychogyiopoulos, A., Groot, L.J., Ten Hove, D., De Jonge, H., Kan, K.J.
# -------------------------------------------------------------------------
# 0. Background Functions ----------------------------------------------------------
# -------------------------------------------------------------------------


# NOTE! - DO NOT RUN  -----------------------------------------------------


## Install packages Function (from the internet)
install_or_source_lib <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,1])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# -------------------------------------------------------------------------

## 'GGM_search' Functions: Performs model search to extract
## a GGM Network model, given a specified sample (Kan et al, 2020) 
GGM_search <- function(dat,
                       from = c("US", "GER", "HUN"),
                       alpha_prune = 0.01, fast = TRUE) {
  ## Specify a saturated Network Model according to 'from' WAIS data
  NWmodel_sat <- ggm(covs = (dat[['size']][[from]] - 1)/dat[['size']][[from]] * dat[['Data']][[from]],
                     omega = "Full",
                     nobs = dat[['size']][[from]])
  ## Removing the insignificant partial correlations (alpha = .01) 
  ## & Further improvement until minimizes BIC criterion
  NWmodel_imp <- NWmodel_sat %>%
    prune(alpha = alpha_prune, recursive = TRUE)
  if (fast) NWmodel_imp %>% stepup 
  if (!fast) NWmodel_imp %>% modelsearch #different algorithm; slower
  ## Extract adjacency matrix & save in .GlobalEnv *automatically*
  adj.matrix <<- 1*(getmatrix(NWmodel_imp, "omega")!=0)
  return(NWmodel_imp)
}

# -------------------------------------------------------------------------

## 'ModelsFitFun' Function: Fit the 3 models to our data.
ModelsFitFun <- function (HFmodel = FALSE,
                          BFmodel = FALSE,
                          NWmodel = FALSE,
                          Raw = FALSE,
                          dat, 
                          which = c("US", "GER", "HUN")){
  if(!Raw){
    if (HFmodel){
      hmodel <<- lvm(lambda = lambda_h,
                     beta = beta_h,
                     sigma_zeta = "empty",
                     covs = (dat[['size']][[which]] - 1) / 
                             dat[['size']][[which]] * dat[['Data']][[which]],
                     nobs = dat[['size']][[which]],
                     identification = "variance",
                     optimizer = "ucminf") %>% runmodel
      }
    if (BFmodel){
      BFmodel <<- lvm(lambda = lambda_bi,
                      sigma_zeta = "empty", # Ψ
                      covs = (dat[['size']][[which]] - 1) /
                              dat[['size']][[which]] * dat[['Data']][[which]],
                      nobs = dat[['size']][[which]],
                      identification = "variance",
                      optimizer = "ucminf") %>% runmodel
      }
    if (NWmodel){
      NWmodel <<- ggm(covs = (dat[['size']][[which]] - 1) /
                              dat[['size']][[which]] * dat[['Data']][[which]],
                      omega = adj.matrix,
                      nobs = dat[['size']][[which]],
                      optimizer = "ucminf") %>% runmodel
    }
  }
   if(Raw){
     if (HFmodel){
      hmodelraw <- lvm(lambda = lambda_h,
                       beta = beta_h,
                       sigma_zeta = "empty",
                       data = dat,
                       identification = "variance",
                       optimizer = "ucminf") %>% runmodel
      return(hmodelraw)
      }
    if (BFmodel){
      BFmodelraw <- lvm(lambda = lambda_bi,
                        sigma_zeta = "empty", # Ψ
                        data = dat,
                        identification = "variance",
                        optimizer = "ucminf") %>% runmodel
      return(BFmodelraw)
      }
    if (NWmodel) {
      NWmodelraw <- ggm(data = dat,
                        omega = adj.matrix,
                        optimizer = "ucminf") %>% runmodel
      return(NWmodelraw)
    }
  }
}


# -------------------------------------------------------------------------

## 'GetCorMat' Function: Gives correlation implied matrix from a psychonetrics model
GetCorMat <- function(x){
  tempCor <- cov2cor(getmatrix(x, "sigma"))
  dimnames(tempCor) <- list(obsvars, obsvars)
  return(tempCor)
}
# -------------------------------------------------------------------------

Decomp2 <- function(output, extract = c("fitmeasures", "parameters", "modelmatrices")){
   
  if(extract == "fitmeasures") {
    io <-  apply(do.call(rbind,lapply(output, sapply, lapply, function(q) q@fitmeasures)),2, rbind) 
    o <- list()
    o$trueHF$fitHF <- as.data.frame(do.call(rbind, io$HF[,seq(1, length(io$HF) -2, 3)])) 
    o$trueHF$fitBF <- as.data.frame(do.call(rbind, io$HF[,seq(2, length(io$HF) -1, 3)]))
    o$trueHF$fitNW <- as.data.frame(do.call(rbind, io$HF[,seq(3, length(io$HF)   , 3)]))
    o$trueBF$fitHF <- as.data.frame(do.call(rbind, io$BF[,seq(1, length(io$BF) -2, 3)]))
    o$trueBF$fitBF <- as.data.frame(do.call(rbind, io$BF[,seq(2, length(io$BF) -1, 3)]))
    o$trueBF$fitNW <- as.data.frame(do.call(rbind, io$BF[,seq(3, length(io$BF)   , 3)]))
    o$trueNW$fitHF <- as.data.frame(do.call(rbind, io$NW[,seq(1, length(io$NW) -2, 3)]))
    o$trueNW$fitBF <- as.data.frame(do.call(rbind, io$NW[,seq(2, length(io$NW) -1, 3)]))
    o$trueNW$fitNW <- as.data.frame(do.call(rbind, io$NW[,seq(3, length(io$NW)   , 3)]))
    o <- lapply(o, lapply, 'rownames<-', NULL) %>%
      lapply(lapply,lapply,as.numeric)
    return(o)
  }
  
  if(extract == "parameters") {
    io <-apply(do.call(rbind,lapply(output, sapply, lapply, function(q) q@parameters)),2, rbind)
    return(io)
  } 
  if(extract == "modelmatrices") {
    io <-apply(do.call(rbind,lapply(output, sapply, lapply, function(q) q@modelmatrices)),2, rbind)
    return(io)
  }
}

# END  --------------------------------------------------------------------
