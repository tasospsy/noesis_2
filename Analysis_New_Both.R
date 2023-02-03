# -------------------------------------------------------------------------
## SUPPLEMENTARY MATERIAL for the article:
## "A network perspective on why bifactor models outperform
## higher order g factor models", by
## Psychogyiopoulos, A., Groot, L.J., Ten Hove, D., De Jonge, H., Kan, K.J.
# -------------------------------------------------------------------------
# 3. Analysis ----------------------------------------------------------
# -------------------------------------------------------------------------

setwd('/Users/tasospsy/Google Drive/_UvA/Research Internship/Noesis/')
# CLEANING AND TIDYING THE DATA -------------------------------------------

# 'Psychonetrics::'
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
                strip.text = element_text(colour = "black", size = 16), # specs of the text inside plot
                panel.grid.major.x = element_line(linewidth = .5), # change the grid layout
                panel.grid.major.y = element_line(linewidth = .5), # change the grid layout
                panel.grid.minor.x = element_blank(), # remove the grid layout
                panel.grid.minor.y = element_blank(), # remove the grid layout
                axis.text=element_text(size=12, color = "black"), # specs of the text in axis
                #axis.text.x = element_blank(),
                #axis.text.y = element_blank(),
                legend.position = "bottom", # legend down
                #legend.title = element_blank(), # remove legend title,
                legend.text = element_text(colour = "black", size = 11),
                strip.background =element_rect(fill="grey100"),
                plot.margin = unit(c(1,1,1,1), "lines"),
                plot.title = element_text(hjust = 0.5, color = "black", size = 14), # specs of the title
                plot.tag.position = 'right',
                plot.tag = element_text(
                  size = 13,                     # Font size
                  hjust = 1,                     # Horizontal adjustment
                  vjust = 1,                     # Vertical adjustment
                  angle = -90,                   # Font angle
                  margin = margin(0, 0, 0, 10)), # Margins (t, r, b, l)
                plot.subtitle =   element_text(
                  size = 13,                     # Font size
                  hjust = .5,                     # Horizontal adjustment
                  vjust = 1,                     # Vertical adjustment
                  margin = margin(0, 0, 5, 0)) # Margins (t, r, b, l)
)

# -------------------------------------------------------------------------
# Exact Fit plot ----------------------------------------------------------

## add the TF conditions

chisqPlot <- 
  td_psyc %>% 
  mutate(TF = case_when(True == Fit ~ TRUE, 
                        True != Fit ~ FALSE)) %>% 
  ggplot() +
  geom_histogram(aes(x = chisq, color = Fit, fill = Fit), 
                 bins = 35, show.legend = FALSE, 
                 alpha = 0.2, position = 'identity') + 
  geom_histogram(data = . %>% filter(TF == TRUE), 
                 aes(x = chisq, color = Fit, fill = Fit), 
                 bins = 35, show.legend = FALSE, 
                 alpha = 0.4, position = 'identity') +
  geom_vline(aes(xintercept= df),color = 'grey30', 
              linetype="dashed", linewidth=.6, show.legend = FALSE)+
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1))+
  facet_grid(cols = vars(factor(Fit, levels=c('HF', 'BF','NW'))), 
             rows = vars(factor(True, levels=c('HF', 'BF','NW'))),
             scales = 'free') +
  scale_color_brewer(palette = 'Dark2')+ 
  scale_fill_brewer(palette = 'Dark2')+ 
  labs(y = "",
       x = expression(chi^2 ~ "value"),
       #color='Fitted Model:',
       #fill='Fitted Model:',
       tag = "True Model",
       subtitle = 'Fitted Model') +
  theme_bw() + theme1 

# -------------------------------------------------------------------------
# P-values of Exact Fit Plot ----------------------------------------------
td_psyc %>% 
  mutate(TF = case_when(True == Fit ~ TRUE, 
                        True != Fit ~ FALSE)) %>% 
  ggplot() +
  geom_histogram(aes(x = pvalue, color = Fit, fill = Fit), 
                 bins = 15, show.legend = FALSE, 
                 alpha = 0.2, position = 'identity') + 
  geom_histogram(data = . %>% filter(TF == TRUE), 
                 aes(x = pvalue, color = True, fill = True), 
                 bins = 15, show.legend = FALSE, 
                 alpha = 0.4, position = 'identity') + 
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  facet_grid(cols = vars(factor(Fit, levels=c('HF', 'BF','NW'))), 
             rows = vars(factor(True, levels=c('HF', 'BF','NW'))),
             scales = 'free') +
  scale_color_brewer(palette = 'Dark2')+ 
  scale_fill_brewer(palette = 'Dark2') +
  labs(y = "",
       x = expression(italic('p')~"-value"),
      # color='Fitted Model:',
      # fill='Fitted Model:',
       tag = "True Model",
       subtitle = 'Fitted Model') +
  theme_bw() + theme1


# -------------------------------------------------------------------------
# approximate & Incremental Plot Function ---------------------------------

Plot_Other_Fit <- function(dat, statistic = c("RMSEA", "TLI", "CFI", "NFI")){
  dat %>% 
    mutate(TF = case_when(True == Fit ~ TRUE, 
                          True != Fit ~ FALSE)) %>% 
    pivot_longer(cols = any_of(statistic),
                 names_to = 'stat', values_to = 'value') %>% 
    ggplot() +
    geom_histogram(aes(x = value, color = Fit, fill = Fit), 
                   bins = 35, show.legend = FALSE, 
                   alpha = .2, position = 'identity') + 
    geom_histogram(data = . %>% filter(TF == TRUE),
                   aes(x = value, color = Fit, fill = Fit), 
                   bins = 35, show.legend = FALSE, 
                   alpha = .4, position = 'identity') + 
    scale_x_continuous(
      labels = scales::number_format(accuracy = 0.01))+
    facet_grid(cols = vars(factor(Fit, levels=c('HF', 'BF','NW'))), 
               rows = vars(factor(True, levels=c('HF', 'BF','NW'))),
               scales = 'free') +
    scale_color_brewer(palette = 'Dark2')+ 
    scale_fill_brewer(palette = 'Dark2') +
    labs(y = '',
         x = paste(statistic,'Values'),
        # color='Statistic:',
        # fill='Statistic:',
         tag = "True Model",
         subtitle = 'Fitted Model') +
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
  #filter(True=='BF') %>%  
  #filter(Fit!='BF') %>% 
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

## Extra comparisons 2x2 only relative fit
relativeComp <- function(true = c('HF', 'BF', 'NW'),
                         fitNOT = c('HF', 'BF', 'NW')){
  td_psyc %>% 
    dplyr::select(True, Fit, AIC, BIC, Rep) %>% 
    filter(True==true) %>%  
    filter(Fit!=fitNOT) %>% 
    group_by(True, Rep) %>% 
    summarize(AIC = Fit[which.min(AIC)],
              BIC = Fit[which.min(BIC)]
    ) %>% 
    ungroup() %>% 
    gather(key = "IC", value = "Fit", - True, -Rep) %>% 
    group_by(True, Fit, IC) %>% 
    count(Fit) %>% 
    ungroup() %>% 
    mutate(percent = n /10) %>% dplyr::select(-n, -True)
}

## We need to run all possible combinations of the function above:
# all possible compinations
allcomb <- expand.grid(true = c('HF', 'BF', 'NW'), 
                       fitNOT = c('HF', 'BF', 'NW'))
res_allcomb <- mapply(relativeComp, allcomb$true, allcomb$fitNOT, SIMPLIFY = FALSE)

names(res_allcomb) <- allcomb %>% 
  mutate(title = paste0('True=',true,';','Not=', fitNOT)) %>% 
  select(title) %>% .$title

tidyres <- 
  res_allcomb %>% tibble() %>% 
  unnest_wider(c(.)) %>% 
  mutate(cond = allcomb %>% 
           mutate(cond = paste0('True=',true,';','Not=', fitNOT)) %>% 
           select(cond) %>% .$cond) %>% 
  unnest(cols = c(Fit, IC, percent)) %>% 
  mutate(True = str_remove(cond,';.*' ),
         cond = str_remove(cond,'.*;' )) %>% 
  mutate(cond = as.factor(case_when(
                          cond == 'Not=NW'~ 'HF vs BF',
                          cond == 'Not=BF'~ 'HF vs NW',
                          cond == 'Not=HF'~ 'BF vs NW'))) %>% 
  mutate(True = str_remove(True,'.*=' )) %>% 
  # add cols to use for plotting
  mutate(TrueIncluded = case_when(
    True == 'HF' & cond == 'BF vs NW' ~ FALSE, 
    True == 'BF' & cond == 'HF vs NW' ~ FALSE,                          
    True == 'NW' & cond == 'HF vs BF' ~ FALSE, 
    TRUE ~ TRUE)) %>% 
  mutate(Explabel = case_when(
    True == 'BF' & cond == 'HF vs BF' ~ '1', 
    True == 'NW' & cond == 'HF vs BF' ~ '3',
    True == 'HF' & cond == 'HF vs BF' ~ '2'
    )) %>% 
  complete(True, IC, Fit, cond) %>% 
  mutate(percent = replace_na(percent, 0)) %>% 
  filter(cond == 'BF vs NW' & Fit != 'HF' | cond == 'HF vs NW' & Fit != 'BF' |
         cond == 'HF vs BF' & Fit != 'NW')

    
  

tidyres %>% 
  ggplot(
    aes(x=factor(Fit, levels=c('HF', 'BF','NW')),
        y=factor(IC, levels=c('BIC', 'AIC')))) + 
  # gray rectangle
  geom_rect(data = . %>% filter(TrueIncluded == TRUE),
    aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf), 
    alpha = 0.1, fill = 'grey70', 
    linewidth = 1.5)+
  # percentages
  geom_text(aes(color = Fit, label = paste0(percent,'%')),
            vjust=.5,hjust=.5,fontface='bold',
            family ='mono', size = 4.5,
            show.legend = FALSE) +
  # symbols
  geom_text(data = . %>% filter(IC == 'AIC' & Fit == 'BF'),
            aes(label = Explabel),
            vjust=-.05,hjust=-3.7, fontface = 'italic',
            family ='mono', size = 4,
            show.legend = FALSE) +
  facet_grid(cols = vars(factor(cond, levels=c('HF vs BF',
                                               'HF vs NW',
                                               'BF vs NW'))), 
             rows = vars(factor(True, levels=c('HF', 'BF','NW'))),
             scales = 'free') +
  scale_color_brewer(palette = 'Dark2') +
  labs(y = 'Fit statistic',
       x = 'Fitted Model',
       tag = "True Model",
       subtitle = '2x2 Comparisons') +
  theme_bw() + theme1 +
  theme(panel.grid.major.y = element_blank())
  


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
  geom_point(aes(color = Fit), show.legend = FALSE, size = 2)+
  geom_text(aes(x=IC, y=percent, label = paste0(percent,'%')),
            vjust=.5,hjust=-.2,
            color= 'black', family ='mono', 
            show.legend = FALSE) +
  scale_color_brewer(palette = 'Dark2')+ 
  scale_fill_brewer(palette = 'Dark2') +
  coord_flip() +
  scale_y_continuous(breaks = c(0,25, 50, 75, 100), limits = c(0,122))+
  facet_grid(cols = vars(factor(Fit, levels=c('HF', 'BF','NW'))), 
             rows = vars(factor(True, levels=c('HF', 'BF','NW'))),
             scales = 'free') +
  labs(y = 'Frequency (%)',
       x = 'Fit statistic',
      # color='Fitted Model:',
      # fill='Fitted Model:',
       tag = "True Model",
       subtitle = 'Fitted Model') +
  theme_bw() + theme1 +
  theme(panel.grid.major.y = element_blank())
  

# Comparison Tests - Nested Models ----------------------------------------------------

## Extract the comparison (HF Vs BF) results
HFvsBF <- out %>% tibble() %>% unnest_longer(c(.), values_to = 'mod') %>% 
  unnest_longer(mod, values_to = 'fit') %>% 
  filter(fit_id != 'NWmodel') %>% 
  rename('True' = 'mod_id') %>% 
  pivot_wider(names_from = 'fit_id', values_from = 'fit',
              values_fn = list) %>% 
  unnest(cols = c(HFmodel, BFmodel)) %>% 
  mutate(Dchisq = map2(.x = HFmodel, 
                       .y = BFmodel,
                       ~ psychonetrics::compare(.x, .y))) %>% 
  select(-HFmodel, -BFmodel) 

## Tidy the results
HFvsBF_table <- HFvsBF %>% 
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

## Plot the χ2 diff. distribution
chisqdif <- 
  HFvsBF_table %>% 
  mutate(critical = case_when(Chisq_diff > qchisq(.05, DF_diff, lower.tail=FALSE) ~ TRUE, 
                          TRUE ~ FALSE)) %>% 
  ggplot() +
  geom_histogram(aes(x = Chisq_diff, fill = critical), color = "azure1", 
                 bins = 23, show.legend = FALSE, 
                 alpha = .9, position = 'identity') + 
  geom_vline(aes(xintercept= DF_diff),color = 'grey30', 
             linetype="dashed", linewidth=.6, show.legend = FALSE)+
  facet_wrap(~factor(True, levels=c('HF', 'BF','NW')), 
             scales = 'free') +
  scale_fill_manual(values = c("cyan3", "red3"))+
  geom_vline(aes(xintercept= 19.67514),color = 'black', 
             linetype="dotted", linewidth=.6, show.legend = FALSE)+
  labs(x = expression(chi^2-diff),
       y = '',
       color='',
       fill='')+
  theme_bw() + theme1 +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1))

## Plot the of χ2 diff P-values
pdif <- 
  HFvsBF_table %>% 
  ## point values <.05
  mutate(sigp = case_when(p_value < .05 ~ TRUE, 
                          TRUE ~ FALSE)) %>% 
  ggplot() +
  geom_histogram(aes(x = p_value, fill = sigp), color = "azure1",
                 bins = 21, show.legend = FALSE, position = 'identity',
                 alpha = .8,  boundary = 0) + 
  facet_wrap(~factor(True, levels=c('HF', 'BF','NW')), 
             scales= 'free_y') +
  scale_x_continuous(limits = c(NA,NA), breaks = c(0, 0.25, 0.50, 0.75, 1))+
  scale_fill_manual(values = c("cyan3", "red3"))+
  geom_vline(aes(xintercept= 0.05),color = 'black', 
             linetype="dotted", linewidth=.6, show.legend = FALSE)+
  labs(x = expression(italic('p')~"-value"),
       y = '',
       color='',
       fill='')+
  theme_bw() + theme1 +
  theme(strip.background = element_blank(), strip.text = element_blank(),
        axis.text.x = element_text(size = 9, vjust = 1, hjust=.5))

tog <- chisqdif / pdif
tog
# END 3. Analysis  -------------------------------------------------------



  