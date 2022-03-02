## Results (New)
## Psychonetrics & OpenMx
## 14/2/2022

source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/Background_Functions.R"))

library(psychonetrics)
library(OpenMx)
library(tidyverse)

#################################
# CLEANING AND TIDYING THE DATA # 
#################################

##################
# 'Psychonetrics'# 

setwd('/Users/tasospsy/Google Drive/_UvA/Research Internship/Noesis/')
load('out.Rdata')


hoi <- Decomp2(output = out, extract = "fitmeasures")

## Cleaning and tidying the results! (New) 
td_hoi <- hoi %>% tibble() %>% unnest_longer(c(.)) %>% 
  rename(Fit = '._id', info = '.') %>% 
  group_by(Fit) %>% 
    mutate(True = names(hoi)) %>% ungroup() %>% 
  mutate_at(c('True', 'Fit'), ~str_sub(.,-2)) %>% #retains the last 2 chrs 
  unnest_wider(info) %>% 
  unnest(c(-Fit,-True)) %>% 
  add_column(Rep = rep(1:(nrow(.)/(n_distinct(.$Fit)*n_distinct(.$True))),# 1:9000/3*3
                       (n_distinct(.$Fit)*n_distinct(.$True)) # 3*3
                       )) %>% 
  relocate(Rep, True, Fit) %>% 
  

# Fit measures for this study
exact.fit   <-  c("chisq", "df", "pvalue")
approx.fit  <-  c("rmsea", "cfi", "tli", "nfi")
comp.criteria    <-  c("aic.ll", "bic")

td_psyc <- td_hoi %>% 
  select(Rep, True, Fit, all_of(c(exact.fit, approx.fit, comp.criteria))) %>% 
  rename(RMSEA = 'rmsea', 
         CFI = 'cfi',
         TLI = 'tli',
         AIC = 'aic.ll',
         BIC = 'bic') %>% select(-nfi) %>% 
  add_column(pckg = 'Psychonetrics')
#%>% 
#  as.data.frame() %>% format(scientific=TRUE)

td_psyc %>% filter(True == 'BF', Fit == 'BF')


##################
#### 'OpenMx' ####

setwd("/Users/tasospsy/Google Drive/_UvA/Research Internship/BifactorLab/Simulation_paper/Lennert_res/")
load("FIT_nrep_1000.RData")  
#FIT

## Cleaning and tidying the results (OpenMx)
td_mx <- FIT %>% tibble() %>% unnest_longer(c(.)) %>% 
  rename(True = '._id', info = '.') %>% 
  mutate(True = case_when(True == 'gdat' ~ 'HF', 
                          True == 'bdat' ~ 'BF',
                          True == 'nwdat' ~ 'NW')) %>% 
  unnest_longer(info) %>% rename(Fit = 'info_id') %>% 
  group_by(Fit, True) %>% 
    mutate(stat = names(FIT)) %>% 
  pivot_wider(names_from = 'stat', values_from = 'info') %>% 
  unnest(c(-Fit,-True)) %>% 
  add_column(Rep = rep(1:(nrow(.)/(n_distinct(.$Fit)*n_distinct(.$True))),# 1:9000/3*3
                                             (n_distinct(.$Fit)*n_distinct(.$True)) # 3*3
  )) %>% 
  relocate(Rep, True, Fit) 

td_mx <- td_mx %>%  
  rename(chisq = 'Chi',
         df = 'ChiDoF',
         pvalue = 'p',
         AIC = `AIC par`,
         BIC = `BIC par`) %>% 
  select(Rep, True, Fit, chisq, df, pvalue,RMSEA, CFI, TLI, AIC, BIC) %>% 
  add_column(pckg = 'OpenMx')

###############################################
#### 'Combining 'Psychonetrics' & 'openMx' ####

td_comb <- bind_rows(td_psyc, td_mx)

exact.fit   <-  c("chisq", "df", "pvalue")
approx.fit  <-  c("RMSEA", "CFI", "TLI")
comp.criteria    <-  c("AIC", "BIC")

########################
## HYPOTHESES TESTING ##
########################

## BIFACTOR TRUE MODEL
plots <- td_comb %>%
  pivot_longer(cols = all_of(c(exact.fit, approx.fit, comp.criteria)),
             names_to = 'stat', values_to = 'value') %>% 
  group_by(True, Fit) %>%
  do(
    plots=
  ggplot(data = .) +
    geom_histogram(aes(x = value, fill = pckg), 
                   bins = 15, show.legend = TRUE, color = "white", alpha = 0.8) + 
    facet_wrap(~stat, scales = 'free', ncol = 3)+
    #geom_text(data = SumBFBF, aes(label = lab), 
    #          x=Inf, y=Inf, hjust=1, vjust=1, size=3) +
    ggtitle(paste("True:", .$True, '| Fit:', .$Fit))+
    xlab("Fit Indices") +
    ylab("") +
    theme_minimal()
  )
library(patchwork)
ps1 <- plots$plots[[2]] / plots$plots[[1]] / plots$plots[[3]] 
ps2 <- plots$plots[[5]] / plots$plots[[4]] / plots$plots[[6]] 
ps3 <- plots$plots[[8]] / plots$plots[[7]] / plots$plots[[9]]

#td_psyc %>% group_by(True, Fit) %>% 
#  summarize(rmsea = FitModel[which.min(rmsea)],
#            aic.ll   = FitModel[which.min(aic.ll)],
#            bic   = FitModel[which.min(bic)],
#            tli   = FitModel[which.max(tli)],
#            cfi   = FitModel[which.max(cfi)],
#            nfi   = FitModel[which.max(nfi)]
#            )
