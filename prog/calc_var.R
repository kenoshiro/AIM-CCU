
# Final energy
df$all %<>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_SolidsBio','Fin_Ene_Liq_Bio'),name_new='Fin_Ene_Liq_and_Sol_Bio')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Ind_Liq_Bio','Fin_Ene_Ind_SolidsBio'),name_new='Fin_Ene_Ind_Liq_and_Sol_Bio')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_exc_fee_Ind_Liq_Bio','Fin_Ene_exc_fee_Ind_SolidsBio'),name_new='Fin_Ene_exc_fee_Ind_Liq_and_Sol_Bio')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Ind_Liq_and_Sol_Bio','Fin_Ene_Ind_Ele','Fin_Ene_Ind_Heat','Fin_Ene_Ind_Hyd'),name_new='Fin_Ene_Ind_Low_Carbon')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Res_and_Com_SolidsBio','Fin_Ene_Res_and_Com_Ele','Fin_Ene_Res_and_Com_Heat','Fin_Ene_Res_and_Com_Hyd'),name_new='Fin_Ene_Res_and_Com_Low_Carbon')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Tra_Liq_Bio','Fin_Ene_Tra_Ele','Fin_Ene_Tra_Hyd','Fin_Ene_Tra_Liq_Hyd_syn'),name_new='Fin_Ene_Tra_Low_Carbon')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Ind_SolidsCoa','Fin_Ene_Ind_Liq_Oil','Fin_Ene_Ind_Liq_Hyd_syn','Fin_Ene_Ind_Gas_Fos','Fin_Ene_Ind_Gas_Hyd_syn','Fin_Ene_Ind_Liq_and_Sol_Bio'),name_new='Fin_Ene_Ind_Hyd_Car')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Res_and_Com_SolidsCoa','Fin_Ene_Res_and_Com_Liq_Oil','Fin_Ene_Res_and_Com_Liq_Hyd_syn','Fin_Ene_Res_and_Com_Gas_Fos','Fin_Ene_Res_and_Com_Gas_Hyd_syn','Fin_Ene_Res_and_Com_SolidsBio'),name_new='Fin_Ene_Res_and_Com_Hyd_Car')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Tra_Liq_Coa','Fin_Ene_Tra_Liq_Oil','Fin_Ene_Tra_Liq_Nat_Gas','Fin_Ene_Tra_Liq_Hyd_syn','Fin_Ene_Tra_Gas','Fin_Ene_Tra_Liq_Bio'),name_new='Fin_Ene_Tra_Hyd_Car')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Solids','Fin_Ene_Gas','Fin_Ene_Liq'),name_new='Fin_Ene_Hyd_Car')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Liq_Oil','Fin_Ene_Liq_Nat_Gas'),name_new='Fin_Ene_Liq_Oil_and_Nat_Gas'))

# Final energy share
df$all %<>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Ele'),name_new='Fin_Ene_Share_Ele')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Ind','Fin_Ene_Ind_Ele'),name_new='Fin_Ene_Ind_Share_Ele')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Tra','Fin_Ene_Tra_Ele'),name_new='Fin_Ene_Tra_Share_Ele')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Res_and_Com','Fin_Ene_Res_and_Com_Ele'),name_new='Fin_Ene_Res_and_Com_Share_Ele')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Hyd'),name_new='Fin_Ene_Share_Hyd')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Hyd','Fin_Ene_Liq_Hyd_syn'),name_new='Fin_Ene_Share_Hyd_Syn')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Ind','Fin_Ene_Ind_Hyd'),name_new='Fin_Ene_Ind_Share_Hyd')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Tra','Fin_Ene_Tra_Hyd'),name_new='Fin_Ene_Tra_Share_Hyd')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Tra','Fin_Ene_Tra_Hyd','Fin_Ene_Tra_Liq_Hyd_syn'),name_new='Fin_Ene_Tra_Share_Hyd_Syn')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Res_and_Com','Fin_Ene_Res_and_Com_Hyd'),name_new='Fin_Ene_Res_and_Com_Share_Hyd')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Ind','Fin_Ene_Ind_Liq_Hyd_syn','Fin_Ene_Ind_Gas_Hyd_syn'),name_new='Fin_Ene_Ind_Share_Syn_Hyd')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Tra','Fin_Ene_Tra_Liq_Hyd_syn'),name_new='Fin_Ene_Tra_Share_Syn_Hyd')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Res_and_Com','Fin_Ene_Res_and_Com_Gas_Hyd_syn'),name_new='Fin_Ene_Res_and_Com_Share_Syn_Hyd')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_SolidsBio'),name_new='Fin_Ene_Share_Bio')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Heat'),name_new='Fin_Ene_Share_Heat')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Liq_Hyd_syn','Fin_Ene_Gas_Hyd_syn'),name_new='Fin_Ene_Share_Syn_Hyd')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_SolidsCoa','Fin_Ene_Liq_Oil','Fin_Ene_Gas_Fos'),name_new='Fin_Ene_Share_Fos')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Ind','Fin_Ene_Ind_Hyd_Car'),name_new='Fin_Ene_Ind_Share_Hyd_Car')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Res_and_Com','Fin_Ene_Res_and_Com_Hyd_Car'),name_new='Fin_Ene_Res_and_Com_Share_Hyd_Car')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Tra','Fin_Ene_Tra_Hyd_Car'),name_new='Fin_Ene_Tra_Share_Hyd_Car')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Hyd_Car'),name_new='Fin_Ene_Share_Hyd_Car'))

# Secondary energy
df$all %<>% 
    bind_rows(fcalc_sum(vars=c('Sec_Ene_Ele_Solar','Sec_Ene_Ele_Win'),name_new='Sec_Ene_Ele_VRE')) %>% 
    bind_rows(fcalc_sum(vars=c('Sec_Ene_Ele_NonBioRen','Sec_Ene_Ele_Bio','Sec_Ene_Ele_Nuc','Sec_Ene_Ele_Fos_w_CCS','Sec_Ene_Ele_Hyd_GT'),name_new='Sec_Ene_Ele_Low_Carbon')) %>% 
    bind_rows(fcalc_sum(vars=c('Sec_Ene_Hyd_Ele','Sec_Ene_Hyd_Bio','Sec_Ene_Hyd_Fos_w_CCS'),name_new='Sec_Ene_Hyd_Low_Carbon'))
df$all %<>%
    bind_rows(fcalc_share(vars=c('Sec_Ene_Ele','Sec_Ene_Ele_VRE'),name_new='Sec_Ene_Ele_Share_VRE')) %>% 
    bind_rows(fcalc_share(vars=c('Sec_Ene_Ele','Sec_Ene_Ele_Low_Carbon'),name_new='Sec_Ene_Ele_Share_Low_Carbon')) %>% 
    bind_rows(fcalc_share(vars=c('Sec_Ene_Hyd','Sec_Ene_Hyd_Low_Carbon'),name_new='Sec_Ene_Hyd_Share_Low_Carbon'))

# Emissions
df$all %<>% bind_rows(fcalc_sum(vars=c('Emi_CO2_Ene_Dem_Ind','Emi_CO2_Ene_Dem_AFO'),name_new='Emi_CO2_Ene_Dem_Ind_and_AFO'))
df$all %<>% bind_rows(fcalc_sum(vars=c('Car_Seq_CCS_Bio_Ene_Sup','Car_Seq_Dir_Air_Cap'),name_new='Car_Seq_NETs'))

# Policy costs
df$all %<>% 
    bind_rows(fcalc_share(vars=c('GDP_MER','Pol_Cos_Add_Tot_Ene_Sys_Cos'),name_new='Pol_Cos_per_GDP',unit_new='%'))
df$all %<>% 
    bind_rows(fcalc_cumulate(var='Pol_Cos_Add_Tot_Ene_Sys_Cos',name_new='Pol_Cos_Cum_Disc',intrate=0.05,p_year=2010,unit_new='billion US$',startyr=2021,endyr=2050)) %>% 
    bind_rows(fcalc_cumulate(var='GDP_MER',name_new='GDP_MER_Cum_Disc',intrate=0.05,p_year=2010,unit_new='billion US$',startyr=2021,endyr=2050))
df$all %<>% 
    bind_rows(fcalc_share(vars=c('GDP_MER_Cum_Disc','Pol_Cos_Cum_Disc'),name_new='Pol_Cos_per_GDP_Disc'))

# sankey
df$all %<>% 
    bind_rows(fcalc_diff2(vars=c('Sec_Ene_Ele','Sec_Ene_Hyd_Ele','Fin_Ene_Ele','Sec_Ene_Ele_Own_Use_DAC'),name_new='Los_Ele')) %>% 
    bind_rows(fcalc_diff2(vars=c('Sec_Ene_Hyd','Fin_Ene_Liq_Hyd_syn','Fin_Ene_Gas_Hyd_syn','Fin_Ene_Hyd'),name_new='Los_Hyd'))

# AR6 scenario data
df$load_AR6_global %<>% 
    bind_rows(fcalc_share(df_in=df$load_AR6_global,vars=c('Fin_Ene','Fin_Ene_Ele'),name_new='Fin_Ene_Share_Ele')) %>% 
    bind_rows(fcalc_share(df_in=df$load_AR6_global,vars=c('Fin_Ene_Ind','Fin_Ene_Ind_Ele'),name_new='Fin_Ene_Ind_Share_Ele')) %>% 
    bind_rows(fcalc_share(df_in=df$load_AR6_global,vars=c('Fin_Ene_Tra','Fin_Ene_Tra_Ele'),name_new='Fin_Ene_Tra_Share_Ele')) %>% 
    bind_rows(fcalc_share(df_in=df$load_AR6_global,vars=c('Fin_Ene_Res_and_Com','Fin_Ene_Res_and_Com_Ele'),name_new='Fin_Ene_Res_and_Com_Share_Ele')) %>% 
    bind_rows(fcalc_sum(df_in=df$load_AR6_global,vars=c('Fin_Ene_Ind_Solids','Fin_Ene_Ind_Liq','Fin_Ene_Ind_Gas'),name_new='Fin_Ene_Ind_Hyd_Car')) %>% 
    bind_rows(fcalc_sum(df_in=df$load_AR6_global,vars=c('Fin_Ene_Res_and_Com_Solids','Fin_Ene_Res_and_Com_Liq','Fin_Ene_Res_and_Com_Gas'),name_new='Fin_Ene_Res_and_Com_Hyd_Car')) %>% 
    bind_rows(fcalc_sum(df_in=df$load_AR6_global,vars=c('Fin_Ene_Tra_Liq','Fin_Ene_Tra_Gas'),name_new='Fin_Ene_Tra_Hyd_Car')) %>% 
    bind_rows(fcalc_sum(df_in=df$load_AR6_global,vars=c('Fin_Ene_Solids','Fin_Ene_Gas','Fin_Ene_Liq'),name_new='Fin_Ene_Hyd_Car'))
df$load_AR6_global %<>% 
    bind_rows(fcalc_share(df_in=df$load_AR6_global,vars=c('Fin_Ene_Ind','Fin_Ene_Ind_Hyd_Car'),name_new='Fin_Ene_Ind_Share_Hyd_Car')) %>% 
    bind_rows(fcalc_share(df_in=df$load_AR6_global,vars=c('Fin_Ene_Res_and_Com','Fin_Ene_Res_and_Com_Hyd_Car'),name_new='Fin_Ene_Res_and_Com_Share_Hyd_Car')) %>% 
    bind_rows(fcalc_share(df_in=df$load_AR6_global,vars=c('Fin_Ene_Tra','Fin_Ene_Tra_Hyd_Car'),name_new='Fin_Ene_Tra_Share_Hyd_Car')) %>% 
    bind_rows(fcalc_share(df_in=df$load_AR6_global,vars=c('Fin_Ene','Fin_Ene_Hyd_Car'),name_new='Fin_Ene_Share_Hyd_Car'))
df$load_AR6_global %<>% 
    bind_rows(fcalc_share(df_in=df$load_AR6_global,vars=c('Prm_Ene','Prm_Ene_NonBioRen'),name_new='Prm_Ene_Share_NonBioRen')) %>% 
    bind_rows(fcalc_share(df_in=df$load_AR6_global,vars=c('Prm_Ene','Prm_Ene_Bio'),name_new='Prm_Ene_Share_Bio')) %>% 
    bind_rows(fcalc_share(df_in=df$load_AR6_global,vars=c('Prm_Ene','Prm_Ene_Coa','Prm_Ene_Oil','Prm_Ene_Gas'),name_new='Prm_Ene_Share_Fos'))
