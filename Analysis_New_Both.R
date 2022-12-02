## ANALYSIS (New)
## Psychonetrics (& OpenMx)
## c.14/2/2022| m. 03/12/2022


source(url("https://raw.githubusercontent.com/tasospsy/noesis/main/1_Preparation.R"))
#################################
# CLEANING AND TIDYING THE DATA # 
#################################

##################
# 'Psychonetrics::'# 



setwd('/Users/tasospsy/Google Drive/_UvA/Research Internship/Noesis/')
load('out.Rdata')

## Unnest the results using a custom function
hoi <- Decomp2(output = out, extract = "fitmeasures")

## Cleaning and tidying the results
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
  

# Fit measures used for this study
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

## NOTE: Uncomment and RUN the lines 52-90 ONLY if 
## you would like to compare psychonetrics:: and OpenMx:: results

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

## Re-write the old names
exact.fit   <-  c("chisq", "df", "pvalue")
approx.fit  <-  c("RMSEA", "CFI", "TLI", "NFI")
comp.criteria    <-  c("AIC", "BIC")


# -------------------------------------------------------------------------
# Results Analysis & Visualization -----------------------------------------
# -------------------------------------------------------------------------

## creating a custom theme
theme1 <- theme(plot.background = element_rect(fill = "white", color = NA), #background color
                text = element_text(family = "mono", color = "black"), # color of all text in the plot 
                strip.text = element_text(colour = "black", size = 15), # specs of the text inside plot
                panel.grid.major.x = element_line(size = .5), # change the grid layout
                panel.grid.major.y = element_line(size = .5), # change the grid layout
                panel.grid.minor.x = element_blank(), # remove the grid layout
                panel.grid.minor.y = element_blank(), # remove the grid layout
                axis.text=element_text(size=11, color = "black"), # specs of the text in axis
                #axis.text.x = element_blank(),
                #axis.text.y = element_blank(),
                legend.position = "bottom", # legend down
                #legend.title = element_blank(), # remove legend title,
                legend.text = element_text(colour = "black", size = 10),
                strip.background =element_rect(fill="grey100"),
                plot.margin = unit(c(1,1,1,1), "lines"),
                plot.title = element_text(hjust = 0.5, color = "black", size = 13), # specs of the title
                plot.tag.position = 'right',
                plot.tag = element_text(
                  size = 12,                     # Font size
                  hjust = 1,                     # Horizontal adjustment
                  vjust = 1,                     # Vertical adjustment
                  angle = -90,                   # Font angle
                  margin = margin(0, 0, 0, 10)), # Margins (t, r, b, l)
                plot.subtitle =   element_text(
                  size = 12,                     # Font size
                  hjust = .5,                     # Horizontal adjustment
                  vjust = 1,                     # Vertical adjustment
                  margin = margin(0, 0, 5, 0)) # Margins (t, r, b, l)
)



# -------------------------------------------------------------------------
# Exact Fit plot ----------------------------------------------------------
td_psyc %>% 
  ggplot() +
  geom_histogram(aes(x = chisq, color = Fit, fill = Fit), 
                 bins = 40, show.legend = FALSE, 
                 alpha = 0.2, position = 'identity') + 
  geom_vline(aes(xintercept= df, color = Fit), 
              linetype="dashed", size=.6, show.legend = FALSE)+
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1))+
  facet_grid(cols = vars(factor(True, levels=c('HF', 'BF','NW'))), 
             rows = vars(factor(Fit, levels=c('HF', 'BF','NW'))),
             scales = 'free') +
  scale_color_brewer(palette = 'Dark2')+ 
  scale_fill_brewer(palette = 'Dark2') +
  labs(y = "",
       x = expression(chi^2 ~ "value"),
       color='Fitting Model:',
       fill='Fitting Model:',
       tag = "Fitting Model",
       subtitle = 'True Model') +
  theme_bw() + theme1 

# -------------------------------------------------------------------------
# P-values of Exact Fit Plot ----------------------------------------------
td_psyc %>% 
  ggplot() +
  geom_histogram(aes(x = pvalue, color = Fit, fill = Fit), 
                 bins = 15, show.legend = FALSE, 
                 alpha = 0.2, position = 'identity') + 
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  facet_grid(cols = vars(factor(True, levels=c('HF', 'BF','NW'))), 
             rows = vars(factor(Fit, levels=c('HF', 'BF','NW'))),
             scales = 'free') +
  scale_color_brewer(palette = 'Dark2')+ 
  scale_fill_brewer(palette = 'Dark2') +
  labs(y = "",
       x = expression(italic('p')~"-value"),
       color='Fitting Model:',
       fill='Fitting Model:',
       tag = "Fitting Model",
       subtitle = 'True Model') +
  theme_bw() + theme1



# -------------------------------------------------------------------------
# approximate & Incremental Plot Function ---------------------------------

Plot_Other_Fit <- function(dat, statistic = c("RMSEA", "TLI", "CFI", "NFI")){
  dat %>% 
    pivot_longer(cols = any_of(statistic),
                 names_to = 'stat', values_to = 'value') %>% 
    ggplot() +
    geom_histogram(aes(x = value, color = Fit, fill = Fit), 
                   bins = 40, show.legend = FALSE, 
                   alpha = .2, position = 'identity') + 
    scale_x_continuous(
      labels = scales::number_format(accuracy = 0.01))+
    facet_grid(cols = vars(factor(True, levels=c('HF', 'BF','NW'))), 
               rows = vars(factor(Fit, levels=c('HF', 'BF','NW'))),
               scales = 'free') +
    scale_color_brewer(palette = 'Dark2')+ 
    scale_fill_brewer(palette = 'Dark2') +
    labs(y = '',
         x = paste(statistic,'Values'),
         color='Statistic:',
         fill='Statistic:',
         tag = "Fitting Model",
         subtitle = 'True Model') +
    theme_bw() + theme1 +
    theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1))
}


## Approximate fit plot
Plot_Other_Fit(dat = td_psyc, statistic = c('RMSEA'))
 

## Incremental fit plot
Plot_Other_Fit(dat = td_psyc, statistic = c('CFI'))
Plot_Other_Fit(dat = td_psyc, statistic = c('TLI'))
Plot_Other_Fit(dat = td_psyc, statistic = c('NFI'))

# Descriptives of approximate & incremental fit ---------------------------

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



# Create a frequency table ------------------------------------------------

table.perc <- 
  td_psyc %>% 
    dplyr::select(True, Fit, RMSEA, CFI, TLI, NFI, AIC, BIC, Rep) %>% 
  #filter(True=='NW') %>%  
  #filter(Fit!='NW') %>% 
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
  select(-n) %>%
  pivot_wider(names_from = True, values_from = percent) 

table.perc.wide

## Uncomment and RUN the commented lines below
## if you would like the above table in latex format

#library(kableExtra)
#kbl(table.perc.wide, 
#    booktabs = T,
#    format = 'latex',
#    caption = "Do fit indices pick the right model?") %>% 
#  kable_styling(position = "center",
#                latex_options = c("scale_down")) %>% 
#  footnote(general = "test")


# Plot the frequencies ----------------------------------------------------
table.perc.wide %>% 
  pivot_longer(cols = c('BF', 'HF', 'NW'),
               names_to = 'True', values_to = 'percent') %>% 
  replace(is.na(.), 0) %>% 
  ggplot(aes(x=fct_relevel(IC,'RMSEA', 'CFI', 'TLI', 'NFI','AIC', 'BIC'), 
             y=percent)) +
  geom_col( aes(fill = Fit), width = .30, alpha= .7, color= 'white', 
            show.legend = FALSE) +
  geom_point(aes(color = Fit), show.legend = FALSE)+
  geom_text(aes(x=IC, y=percent, label = paste0(percent,'%')),
            vjust=.5,hjust=-.2,
            color= 'black', family ='mono', 
            show.legend = FALSE) +
  scale_color_brewer(palette = 'Dark2')+ 
  scale_fill_brewer(palette = 'Dark2') +
  coord_flip() +
  scale_y_continuous(breaks = c(0,25, 50, 75, 100), limits = c(0,122))+
  facet_grid(cols = vars(factor(True, levels=c('HF', 'BF','NW'))), 
             rows = vars(factor(Fit, levels=c('HF', 'BF','NW'))),
             scales = 'free') +
  labs(y = 'Frequency (%)',
       x = 'Fit statistic',
       color='Fitting Model:',
       fill='Fitting Model:',
       tag = "Fitting Model",
       subtitle = 'True Model') +
  theme_bw() + theme1 +
  theme(panel.grid.major.y = element_blank())
  

# Comparison Tests - HF Vs BF ----------------------------------------------------

## Extract the comparison (HF Vs BF) results
HFvsBF <- out %>% tibble() %>% unnest_longer(c(.)) %>% 
  unnest_wider(c(.)) %>% 
  rename("True" = "._id") %>% 
  select(-'NWmodel') %>% 
  mutate(Dchisq = map2(.x = HFmodel, 
                       .y = BFmodel,
                       ~ psychonetrics::compare(.x, .y))) %>% 
  select(-HFmodel, -BFmodel) 

## Tidy the results
HFvsBF_table <- HFvsBF%>% 
  unnest_wider(Dchisq)  %>% 
  select(True, model, Chisq_diff, DF_diff, p_value, AIC, BIC) %>% 
  unnest(model:p_value) %>% 
  filter(!if_any(model:p_value, is.na)) %>% 
  #distinct(model) %>% 
  select(-model) 

## When BF is the true model
cat(paste('According to chisq difference test, if BF is the true model, the BF is prefered over the HF in',
HFvsBF_table %>% filter(True == 'BF') %>% filter(p_value <= .05) %>% 
  count() %>% mutate(. /10) %>% paste0('%'), 'of the cases'))
## When HF is the true model
cat(paste('According to chisq difference test, if HF is the true model, the BF is prefered over the HF in',
            HFvsBF_table %>% filter(True == 'HF') %>% filter(p_value <= .05) %>% 
              count() %>% mutate(. /10) %>% paste0('%'), 'of the cases'))
## When NW is the true model
cat(paste('According to chisq difference test, if NW is the true model, the BF is prefered over the HF in',
            HFvsBF_table %>% filter(True == 'NW') %>% filter(p_value <= .05) %>% 
              count() %>% mutate(. /10) %>% paste0('%'), 'of the cases'))

## (EXTRA) plot the χ2 diff. distribution
HFvsBF_table %>% 
  ggplot() +
  geom_histogram(aes(x = Chisq_diff, color = True, fill = True), 
                 bins = 30, show.legend = FALSE, 
                 alpha = .6, position = 'identity') + 
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  facet_wrap(~factor(True, levels=c('HF', 'BF','NW')), scales = 'free') +
  scale_color_brewer(palette = 'Dark2')+ scale_fill_brewer(palette = 'Dark2')+
  labs(y = expression(chi^2-diff),
       x = '',
       color='',
       fill='')+
  theme_bw() + theme1 +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1))




  