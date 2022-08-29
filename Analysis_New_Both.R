## Results (New)
## Psychonetrics & OpenMx
## 14/2/2022

source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/Background_Functions.R"))

library(psychonetrics)
library(OpenMx)
library(tidyverse)
library(scales)
#################################
# CLEANING AND TIDYING THE DATA # 
#################################

##################
# 'Psychonetrics'# 

#SOS:
#Run only if you have salready run the simulations (see '2_nSIMULATIONS.R)
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
  relocate(Rep, True, Fit) 
  

# Fit measures for this study
exact.fit   <-  c("chisq", "df", "pvalue")
approx.fit  <-  c("rmsea", "cfi", "tli", "nfi")
comp.criteria    <-  c("aic.ll", "bic")

td_psyc <- td_hoi %>% 
  select(Rep, True, Fit, all_of(c(exact.fit, approx.fit, comp.criteria))) %>% 
  rename(RMSEA = 'rmsea', 
         CFI = 'cfi',
         TLI = 'tli',
         NFI = 'nfi',
         AIC = 'aic.ll',
         BIC = 'bic') %>%  
  add_column(pckg = 'Psychonetrics') 
#%>% as.data.frame() %>% format(scientific=TRUE)


###################
##### 'OpenMx' ####
#
#setwd("/Users/tasospsy/Google Drive/_UvA/Research Internship/BifactorLab/Simulation_paper/Lennert_res/")
#load("FIT_nrep_1000.RData")  
##FIT
#
### Cleaning and tidying the results (OpenMx)
#td_mx <- FIT %>% tibble() %>% unnest_longer(c(.)) %>% 
#  rename(True = '._id', info = '.') %>% 
#  mutate(True = case_when(True == 'gdat' ~ 'HF', 
#                          True == 'bdat' ~ 'BF',
#                          True == 'nwdat' ~ 'NW')) %>% 
#  unnest_longer(info) %>% rename(Fit = 'info_id') %>% 
#  group_by(Fit, True) %>% 
#    mutate(stat = names(FIT)) %>% 
#  pivot_wider(names_from = 'stat', values_from = 'info') %>% 
#  unnest(c(-Fit,-True)) %>% 
#  add_column(Rep = rep(1:(nrow(.)/(n_distinct(.$Fit)*n_distinct(.$True))),# 1:9000/3*3
#                                             (n_distinct(.$Fit)*n_distinct(.$True)) # 3*3
#  )) %>% 
#  relocate(Rep, True, Fit) 
#
#td_mx <- td_mx %>%  
#  rename(chisq = 'Chi',
#         df = 'ChiDoF',
#         pvalue = 'p',
#         AIC = `AIC par`,
#         BIC = `BIC par`) %>% 
#  select(Rep, True, Fit, chisq, df, pvalue,RMSEA, CFI, TLI, AIC, BIC) %>% 
#  add_column(pckg = 'OpenMx')
#
#################################################
###### 'Combining 'Psychonetrics' & 'openMx' ####
##
#td_comb <- bind_rows(td_psyc, td_mx)

exact.fit   <-  c("chisq", "df", "pvalue")
approx.fit  <-  c("RMSEA", "CFI", "TLI", "NFI")
comp.criteria    <-  c("AIC", "BIC")

########################
## PLOTS ##
########################
theme1 <- theme(plot.background = element_rect(fill = "white", color = NA), #background color
                text = element_text(family = "mono", color = "black"), # color of all text in the plot 
                plot.title = element_text(hjust = 0.5, color = "black", size = 13), # specs of the title
                strip.text = element_text(colour = "black", size = 15), # specs of the text inside plot
                panel.grid.major.x = element_line(size = .5), # change the grid layout
                panel.grid.major.y = element_line(size = .5), # change the grid layout
                panel.grid.minor.x = element_blank(), # remove the grid layout
                panel.grid.minor.y = element_blank(), # remove the grid layout
                axis.text=element_text(size=11, color = "black"), # specs of the text in axis
                #axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                legend.position = "bottom", # legend down
                #legend.title = element_blank(), # remove legend title,
                legend.text = element_text(colour = "black", size = 10),
                #plot.title = element_markdown(size = 12,hjust = 0,lineheight = 1, 
                #                              color = "black", family = 'mono'),
                strip.background =element_rect(fill="grey100")
)


#####################
## exact fit plot
exp1 <- td_psyc %>% 
  ggplot() +
  geom_histogram(aes(x = chisq, color = Fit, fill = Fit), 
                 bins = 30, show.legend = TRUE, 
                 alpha = 0.4, position = 'identity') + 
  geom_vline(aes(xintercept= df, color = Fit), 
              linetype="dashed", size=.6)+
  #geom_text(aes(label = df, x = chisq, 
  #          y=chisq), hjust=1, vjust=1, size=3) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1))+
  facet_grid(True~Fit, scales = 'free') +
  scale_color_brewer(palette = 'Set1')+ scale_fill_brewer(palette = 'Set1')+
  labs(y = expression(chi^2),
       x = '',
       color='Fitting Model:',
       fill='Fitting Model:') +
  theme_bw() + theme1 + 
  theme(#axis.text.x = element_text(angle = 40, vjust = 1, hjust=1),
        legend.position = 'none')
exp1

exp2 <- td_psyc %>% 
  ggplot() +
  geom_histogram(aes(x = pvalue, color = Fit, fill = Fit), 
                 bins = 15, show.legend = TRUE, 
                 alpha = 0.5, position = 'identity') + 
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  facet_grid(True~Fit, scales = 'free') +
  scale_color_brewer(palette = 'Set1')+ scale_fill_brewer(palette = 'Set1')+
  
  labs(y = expression(italic('p'),paste('values')),
       x = 'Values',
       color='Fitting Model:',
       fill='Fitting Model:')+
  theme_bw() + theme1 +
  theme(legend.position = 'none')
exp2
#####################
## approximate & incremental fit plot
td_psyc %>% 
  pivot_longer(cols = c("RMSEA"),
               names_to = 'stat', values_to = 'value') %>% 
  ggplot() +
  geom_histogram(aes(x = value, color = stat, fill = stat), 
                 bins = 30, show.legend = TRUE, 
                 alpha = .6, position = 'identity') + 
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  facet_grid(True ~ Fit, scales = 'free') +
  scale_color_brewer(palette = 'Dark2')+ scale_fill_brewer(palette = 'Dark2')+
  #geom_text(data = SumBFBF, aes(label = lab), 
  #          x=Inf, y=Inf, hjust=1, vjust=1, size=3) +
  labs(y = '',
       x = 'Fit Values',
       color='Statistic:',
       fill='Statistic:')+
  theme_bw() + theme1 +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1))

td_psyc %>% 
  pivot_longer(cols = c("TLI", "CFI", "NFI"),
               names_to = 'stat', values_to = 'value') %>% 
  ggplot() +
  geom_histogram(aes(x = value, color = stat, fill = stat), 
                 bins = 30, show.legend = TRUE, 
                 alpha = .6, position = 'identity') + 
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  facet_grid(True ~ Fit, scales = 'free') +
  scale_color_brewer(palette = 'Dark2')+ scale_fill_brewer(palette = 'Dark2')+
  #geom_text(data = SumBFBF, aes(label = lab), 
  #          x=Inf, y=Inf, hjust=1, vjust=1, size=3) +
  labs(y = '',
       x = 'Fit Values',
       color='Statistic:',
       fill='Statistic:')+
  theme_bw() + theme1 +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1))


## Descriptives table 
td_psyc %>% 
  pivot_longer(cols = all_of(approx.fit),
               names_to = 'stat', values_to = 'value') %>% 
  group_by(True, Fit, stat) %>% 
  summarize(min = round(min(value),2), 
            max = round(max(value),2), 
            mean = round(mean(value),2)) %>% 
  mutate(mean.in = case_when(
                            ## RMSEA
                            stat == "RMSEA" & mean <= .05 ~ 'good',
                            stat == "RMSEA" & mean > .05 & mean <= .08 ~ 'reasonable',
                            stat == "RMSEA" & mean > .08 & mean <= .1 ~ 'mediocre',
                            stat == "RMSEA" & mean > .1 ~ 'inacceptable',
                            ## CFI & TLI
                            stat == "CFI" | stat == 'TLI' & mean >= .98 ~ 'good',
                            stat == "CFI" | stat == 'TLI' & mean > .95 & mean <.98 ~ 'acceptable',
                           
                            ## NFI
                            stat == "NFI" & mean > .95 ~ 'good'
                            
                            )) %>% 
  group_by(stat) %>% group_split()

##==========================================
# Test: Do fit Indices pick the right model?
##==========================================
##====================
## table percentages %
##====================
table.perc <- td_psyc %>% 
    dplyr::select(True, Fit, RMSEA, CFI, TLI, NFI, AIC, BIC, Rep) %>% 
    group_by(True, Rep) %>% 
    summarize(RMSEA = Fit[which.min(RMSEA)],
              AIC   = Fit[which.min(AIC)],
              BIC   = Fit[which.min(BIC)],
              TLI   = Fit[which.max(TLI)],
              CFI   = Fit[which.max(CFI)],
              NFI   = Fit[which.max(NFI)]
    ) %>% 
    ungroup() %>% 
    gather(key = "IC", value = "Fit", - True, -Rep) %>% 
    group_by(True, Fit, IC) %>% 
    count(Fit) %>% 
    mutate(percent = n /10) 

table.perc.wide <- table.perc %>% 
  group_by(True, Fit, IC) %>% 
  pivot_wider(names_from = True, values_from = percent) %>% 
  fill(everything(), .direction = "downup") %>% 
  select(-n) %>%
  distinct(.)

#library(kableExtra)

kbl(table.perc.wide, 
    booktabs = T,
    format = 'latex',
    caption = "Do fit indices pick the right model?") %>% 
  kable_styling(position = "center",
                latex_options = c("scale_down")) %>% 
  footnote(general = "test")
##====================
## plot percentages %
##====================
table.perc %>% ggplot(aes(x=fct_relevel(IC,
                                        'RMSEA', 'CFI', 'TLI', 'NFI', 
                                        'AIC', 'BIC'), y=percent)) +
  geom_col( aes(color= Fit, fill = Fit), width = .15, alpha= .7 ) +
  geom_text(aes(x=IC, y=percent, label = percent, color = Fit),
            position = position_stack(vjust = .5), 
            vjust = -.9, family ='mono') +
  scale_color_brewer(palette = 'Set1')+ 
  scale_fill_brewer(palette = 'Set1') +
  coord_flip() +
  facet_wrap(~True) +
  labs(y = 'Frequency (%)',
       x = 'Fit statistic',
       color='Fitting Model:',
       fill='Fitting Model:')+
  theme_bw() + theme1 +
  theme(axis.text.y=element_text(size=10, color = "black"))
  
  
  
  
  