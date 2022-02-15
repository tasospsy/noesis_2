## Cucina and Byle Table
library(tidyverse)
library(papaja)
studies <- c('Gignac & Watkins, 2013',
             'Gignac & Watkins, 2013',
             'Gignac & Watkins, 2013',
             'Gignac & Watkins, 2013',
             'Gignac [11]',
             'Gignac [13]',
             'Golay & Lecerf',
             'Niikelsela et al.')

batteries <- c('WAIS-IV',
               'WAIS-IV',
               'WAIS-IV',
               'WAIS-IV',
               'WAIS-R',
               'WAIS-III',
               'WAIS-III',
               'WAIS-IV')
# HF table
HFtable <- tibble(
  Study = studies,
  Battery = batteries,
  CFI = c(.945, .959, .943, .948, .970, .968, .965, .694),
  TLI = c(.933, .950, .930, .937, .959, .959, .956, .967),
  NFI = c(.918, .944, .920, .927, .967, .965, .957, .942),
  RMSEA = c(.068, .064, .075, .074, .068, .064, .059, .067),
  AIC = c(314.75, 366.51, 347.28, 341.93, 443.97, 723.38, 359.50, 193.62),
  chisq = c(246.75, 298.51, 279.28, 273.93, 391.97, 663.38, 301.50, 179.62),
  df = c(86,86,86,86,40,61,62,71)
) %>% add_column(Model = 'HF') %>% 
  rowid_to_column()

HF_summary <-  HFtable %>% 
  pivot_longer(CFI:df, names_to = 'Stat', values_to = 'value') %>% 
  group_by(Stat) %>% summarise(Median = median(value), 
                               Min = min(value),
                               Max = max(value)) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  as.data.frame()
HF_summary

# BF table
BFtable <- tibble(
  Study = studies,
  Battery = batteries,
  CFI = c(.975,.967,.975,.967,.989,.979,.990,.966),
  TLI = c(.965,.967,.965,.954,.982,.968,.985,.966),
  NFI = c(.951,.963,.954,.948,.986,.976,.983,.945),
  RMSEA = c(.049,.052,.053,.063,.046,.056,.035,.062),
  AIC = c(237.28,287.21,250.43,284.95,228.28,528.25,199.00,192.86),
  chisq = c(147.28,197.21,16.43,194.95,162.28,448.25,123.00,168.86),
  df = c(75,75,75,75,33,51,53,66)
) %>% add_column(Model = 'BF') %>% 
  rowid_to_column()

BF_summary <-  BFtable %>% 
  pivot_longer(CFI:df, names_to = 'Stat', values_to = 'value') %>% 
  group_by(Stat) %>% summarise(Median = median(value), 
                               Min = min(value),
                               Max = max(value)) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  as.data.frame()
BF_summary

## JOins
join <- left_join(HFtable, BFtable, by = 'rowid') %>% 
  select(-rowid, -Study.y, - Battery.y, - Model.x, -Model.y)
  

library(kableExtra)
 
kbl(join, 
    booktabs = T,
    format = 'latex', 
    caption = "Cucina WAIS studies ") %>% 
  add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6)) %>% 
  kable_styling(position = "center",
                latex_options = c("scale_down")) %>% 
  footnote(general = "")
