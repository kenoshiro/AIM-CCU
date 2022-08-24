library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(magrittr)
library(ggplot2)
library(ggalluvial)
library(cowplot)
library(readxl)

dir.create(path='data',showWarnings=F)
dir.create(path='output',showWarnings=F)

AR6_data_file <- 'data/AR6/AR6_Scenarios_Database_World_v1.0.csv'
AR6_meta_file <- 'data/AR6/AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx'

df <- list()
lst <- list()
obj <- list()
p <- list()

df$var_config <- read_csv('define/variable.csv',col_names=c('Variable2','Variable'))
df$all <- read_csv('data/scenario_data.csv') %>% 
    gather(-Model,-Scenario,-Region,-Variable,-Unit,key=Year,value=Value) %>% 
    filter(Value != 'N/A') %>% 
    inner_join(df$var_config,by='Variable') %>% select(-Variable) %>% rename(Variable='Variable2') %>% 
    spread(key=Year,value='Value',fill=0) %>% 
    gather(-c('Model','Scenario','Region','Variable','Unit'), key='Year', value='Value') %>% 
    spread(key=Scenario,value='Value',fill=0) %>% 
    gather(-c('Model','Region','Variable','Year','Unit'), key='Scenario', value='Value') %>% 
    mutate(Year=as.integer(as.character(Year)))

df$load_AR6_global_meta <- read_xlsx(path=AR6_meta_file,sheet='meta_Ch3vetted_withclimate') %>% 
    select(Model,Scenario,Category,IMP_marker)

df$load_AR6_global <- read_csv(file=AR6_data_file) %>% 
    pivot_longer(cols=-c(Model,Scenario,Region,Variable,Unit),names_to='Year',values_to='Value') %>% 
    filter(!is.na(Value)) %>% 
    mutate(Year=as.numeric(Year)) %>% 
    inner_join(df$var_config,by='Variable') %>% select(-Variable) %>% rename(Variable='Variable2') %>% 
    inner_join(df$load_AR6_global_meta,by=c('Model','Scenario'))

source('prog/func_calc.R')
source('prog/calc_var.R')
source('prog/plot.R',echo=T)
