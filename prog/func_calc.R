fcalc_relBase <- function(df_in=df$all,vars){
    df_base <- filter(df_in,Variable%in%vars,Scenario%in%unique(df$scen_base$Baseline)) %>%
        rename(Baseline='Scenario', Value_Base='Value')
    tmp <- filter(df_in,Variable%in%vars) %>% inner_join(df$scen_base,key='Scenario') %>% 
        inner_join(df_base) %>% 
        mutate(Value=Value-Value_Base) %>% 
        select(-c('Baseline','Value_Base'))
    return(tmp)
}

fcalc_sum <- function(df_in=df$all,vars,name_new){
    tmp <- filter(df_in, Variable%in%vars) %>%
        group_by(Model,Scenario,Region,Year,Unit) %>%
        summarise(Value=sum(Value, na.rm=T),.groups='drop') %>%
        mutate(Variable=name_new)
    return(tmp)
}

#First entry should be denominator. Other entries are summed up.
fcalc_share <- function(df_in=df$all,vars,name_new,unit_new='%'){
    tmp1 <- filter(df_in, Variable==vars[1]) %>%
        rename(TotValue='Value', TotVariable='Variable') %>%
        select(-Unit)
    tmp <- filter(df_in, Variable%in%vars) %>%
        filter(Variable!=vars[1]) %>%
        select(-Unit) %>% 
        group_by(Model,Scenario,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>%  
        inner_join(tmp1, by=c('Model','Scenario','Region','Year')) %>%
        mutate(Value=Value/TotValue, Variable=name_new, Unit=unit_new) %>%
        select(-TotValue,-TotVariable)
    return(tmp)
}

fcalc_diff <- function(df_in=df$all,vars,name_new,unit_new='%'){
    tmp1 <- filter(df_in,Variable==vars[1]) %>%
        rename(Value1='Value', Variable1='Variable') %>%
        select(-Unit)
    tmp <- filter(df_in,Variable%in%vars) %>%
        filter(Variable!=vars[1]) %>%
        select(-Unit) %>% 
        left_join(tmp1, by=c('Model','Scenario','Region','Year')) %>% 
        mutate(Value1=ifelse(is.na(Value1),0,Value1)) %>% 
        mutate(Value=Value-Value1, Variable=name_new, Unit=unit_new) %>%
        select(-Value1,-Variable1)
    return(tmp)
}

fcalc_diff2 <- function(df_in=df$all,vars,name_new,unit_new='%'){
    tmp1 <- filter(df_in,Variable==vars[1]) %>%
        rename(Value1='Value', Variable1='Variable') %>%
        select(-Unit)
    tmp <- filter(df_in,Variable%in%vars) %>%
        filter(Variable!=vars[1]) %>%
        select(-Unit) %>% 
        group_by(Model,Scenario,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>%  
        left_join(tmp1, by=c('Model','Scenario','Region','Year')) %>% 
        mutate(Value1=ifelse(is.na(Value1),0,Value1)) %>% 
        mutate(Value=Value1-Value, Variable=name_new, Unit=unit_new) %>%
        select(-Value1,-Variable1)
    return(tmp)
}

fcalc_diff_scen <- function(df_in=df$all,var,new_var,base){
    df_base <- filter(df_in,Variable==var,Scenario==base) %>%
        rename(Baseline='Scenario', Value_Base='Value')
    tmp <- filter(df_in,Variable==var) %>%  
        inner_join(df_base) %>% 
        mutate(Value=Value-Value_Base,Variable=new_var) %>% 
        select(-c('Baseline','Value_Base'))
    return(tmp)
}

fcalc_cumulate <- function(df_in=df$all,var,name_new=str_c('Cum_',var),intrate=0,p_year=2010,unit_new,startyr=2010,endyr=2050){
    tmp <- filter(df_in,Variable==var)
    if(nrow(tmp)>0){
        tmp2 <- expand.grid(
            Model=unique(tmp$Model),
            Scenario=unique(tmp$Scenario),
            Region=unique(tmp$Region),
            Variable=var,
            Year=seq(2010,2100,1)
        ) %>% 
            left_join(tmp,by=c('Model','Scenario','Region','Variable','Year')) %>% 
            mutate(Year=as.integer(Year)) %>% 
            group_by(Model,Scenario,Region,Variable) %>% 
            mutate(Value=approx(x=Year,y=Value,xout=Year)$y) %>% 
            mutate(Value=Value*(1-intrate)**(Year-p_year)) %>% 
            filter(Year>=startyr) %>% filter(Year<=endyr) %>% arrange(Year) %>% 
            mutate(Value=cumsum(Value)) %>% ungroup() %>% 
            mutate(Variable=name_new,Unit=unit_new)
        return(tmp2)
    }
}

fcalc_relbaseyear <- function(df_in=df$all,var,name_new=str_c(var,'|Change from base year'),unit_new='%',baseyear=2010){
    tmp1 <- filter(df_in,Variable==var,Year==baseyear) %>% 
        rename(BaseYear='Year', BaseValue='Value')
    tmp <- filter(df_in, Variable==var, Year!=baseyear) %>%
        inner_join(tmp1, by=c('Model','Scenario','Region','Variable','Unit')) %>%
        mutate(Value=Value/BaseValue, Variable=name_new, Unit=unit_new) %>%
        select(-BaseValue,-BaseYear)
    return(tmp)
}
fcalc_baseyearchange <- function(df_in=df$all,var,name_new=str_c(var,'|Change from base year'),unit_new='%',baseyear=2010){
    tmp1 <- filter(df_in,Variable==var,Year==baseyear) %>% 
        rename(BaseYear='Year', BaseValue='Value')
    tmp <- filter(df_in, Variable==var, Year!=baseyear) %>%
        inner_join(tmp1, by=c('Model','Scenario','Region','Variable','Unit')) %>%
        mutate(Value=Value/BaseValue, Variable=name_new, Unit=unit_new) %>%
        select(-BaseValue,-BaseYear)
    return(tmp)
}
fcalc_baseyearreduction <- function(df_in=df$all,var,name_new=str_c(var,'|Reduction from base year'),unit_new='%',baseyear=2010){
    tmp1 <- filter(df_in,Variable==var,Year==baseyear) %>% 
        rename(BaseYear='Year', BaseValue='Value')
    tmp <- filter(df_in, Variable==var, Year!=baseyear) %>%
        inner_join(tmp1, by=c('Model','Scenario','Region','Variable','Unit')) %>%
        mutate(Value=1-Value/BaseValue, Variable=name_new, Unit=unit_new) %>%
        select(-BaseValue,-BaseYear)
    return(tmp)
}

fcalc_annualchange <-function(df_in=df$all,var,name_new=str_c(var,'|Annual Change Rate'),unit_new='%/yr'){
    tmp <- filter(df_in,Variable==var) %>% 
        mutate(Year_p=Year+5) %>% 
        select(-Year,-Unit) %>% rename(Year='Year_p',Value_p='Value') %>% 
        inner_join(df_in,key=c('Model','Scenario','Region')) %>% 
        mutate(Value=(Value/Value_p)**(1/5)-1) %>% select(-Value_p) %>% 
        mutate(Variable=name_new,Unit=unit_new)
    return(tmp)
}
fcalc_annualreduction <-function(df_in=df$all,var,name_new=str_c(var,'|Annual Reduction Rate'),unit_new='%/yr'){
    tmp <- filter(df_in,Variable==var) %>% 
        mutate(Year_p=Year+5) %>% 
        select(-Year,-Unit) %>% rename(Year='Year_p',Value_p='Value') %>% 
        inner_join(df_in,key=c('Model','Scenario','Region')) %>% 
        mutate(Value=1-(Value/Value_p)**(1/5)) %>% select(-Value_p) %>% 
        mutate(Variable=name_new,Unit=unit_new)
    return(tmp)
}
fcalc_relBaselinereductionrate <- function(df_in=df$all,var,name_new=str_c(var,'|Reduction from baseline')){
    df_base <- filter(df_in,Variable%in%var,Scenario%in%unique(df$scen_base$Baseline)) %>% rename(Baseline='Scenario',Value_Base='Value')
    tmp <- filter(df_in,Variable%in%var) %>% inner_join(df$scen_base,key='Scenario') %>% 
        inner_join(df_base) %>% mutate(Value=1-Value/Value_Base,Variable=name_new) %>% select(-c('Baseline','Value_Base'))
    return(tmp)
}

fcalc_range_category <- function(df_in){
    df_in %>% group_by(Variable,Category,Region,Year) %>% 
        summarise(p50=median(Value),p90=quantile(Value,.9),p10=quantile(Value,.1),p0=min(Value),p100=max(Value),n=n(),.groups='drop') %>% 
        return()
}
