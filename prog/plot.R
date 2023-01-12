mytheme <- list()
mytheme$set1 <- theme_bw()+theme(plot.title=element_text(size=9.5),
                                 panel.grid=element_blank(),
                                 panel.border=element_blank(),
                                 axis.line=element_line(color='black',size=.3),
                                 axis.ticks=element_line(color='black',size=.3),
                                 axis.text=element_text(size=9),
                                 axis.title=element_text(size=10),
                                 legend.title=element_text(size=10))

df$scen_lab <- tribble(~Scenario,~scen_lab,~scen_wrap,
                       'NoPOL','NoPOL','NoPOL',
                       '500C-CCU','1.5C-CCU','1.5C-CCU',
                       '500C-DEC','1.5C-DEC','1.5C-DEC',
                       '500C-CDR','1.5C-CDR','1.5C-CDR',
                       '500C-CCU-Adv','1.5C-CCU-AdvTech','1.5C-CCU\n-AdvTech',
                       '500C-DEC-Adv','1.5C-DEC-AdvTech','1.5C-DEC\n-AdvTech',
                       '500C-CDR-Adv','1.5C-CDR-AdvTech','1.5C-CDR\n-AdvTech',
                       '500C-CCU-Conv','1.5C-CCU-ConvTech','1.5C-CCU\n-ConvTech',
                       '500C-DEC-Conv','1.5C-DEC-ConvTech','1.5C-DEC\n-ConvTech',
                       '500C-CDR-Conv','1.5C-CDR-ConvTech','1.5C-CDR\n-ConvTech',
                       '500C-CCU-LimElecP','1.5C-CCU-LimElec+','1.5C-CCU\n-LimElec+',
                       '500C-CCU-LimElecM','1.5C-CCU-LimElec-','1.5C-CCU\n-LimElec-',
                       '1000C-CCU','WB2C-CCU','WB2C-CCU',
                       '1000C-DEC','WB2C-DEC','WB2C-DEC',
                       '1000C-CDR','WB2C-CDR','WB2C-CDR')
df$scen_sens_cat <- tribble(~Scenario,~scen_sens_base,~scen_sens_var,
                            '1000C-CCU','WB2C-CCU','Default',
                            '1000C-DEC','WB2C-DEC','Default',
                            '1000C-CDR','WB2C-CDR','Default',
                            '500C-CCU','1.5C-CCU','Default',
                            '500C-DEC','1.5C-DEC','Default',
                            '500C-CDR','1.5C-CDR','Default',
                            '500C-CCU-Adv','1.5C-CCU','AdvTech',
                            '500C-DEC-Adv','1.5C-DEC','AdvTech',
                            '500C-CDR-Adv','1.5C-CDR','AdvTech',
                            '500C-CCU-Conv','1.5C-CCU','ConvTech',
                            '500C-DEC-Conv','1.5C-DEC','ConvTech',
                            '500C-CDR-Conv','1.5C-CDR','ConvTech',
                            '500C-CCU-LimElecP','1.5C-CCU','LimElec+',
                            '500C-CCU-LimElecM','1.5C-CCU','LimElec-')

lst$scen_rep <- c('500C-CCU','500C-DEC','500C-CDR')
lst$scen_500all <- c('500C-CCU','500C-DEC','500C-CDR',
                  '500C-CCU-Adv','500C-DEC-Adv','500C-CDR-Adv',
                  '500C-CCU-Conv','500C-DEC-Conv','500C-CDR-Conv',
                  '500C-CCU-LimElecP','500C-CCU-LimElecM')
lst$scen_cat <- c('1.5C-CCU','1.5C-DEC','1.5C-CDR','WB2C-CCU','WB2C-DEC','WB2C-CDR')
lst$scen_sens <- c('Default','AdvTech','ConvTech')
lst$scen_sens_all <- c('Default','AdvTech','ConvTech','LimElec+','LimElec-')
lst$scen_col <- c('1.5C-CCU'='#E64B35FF','1.5C-DEC'='4DBBD5FF','1.5C-CDR'='#00A087FF')
lst$scen_col_all <- c('1.5C-CCU'='#E64B35FF','1.5C-DEC'='4DBBD5FF','1.5C-CDR'='#00A087FF',
                      'WB2C-CCU'='#3C5488FF','WB2C-DEC'='#F39B7FFF','WB2C-CDR'='#8491B4FF')
lst$lin_scen <- c('Default'='solid','AdvTech'='blank','ConvTech'='blank','LimElec+'='blank','LimElec-'='blank')
lst$IMP_all <- c('GS','Neg','Ren','LD','SP','Neg-2.0','Ren-2.0','ModAct','CurPol')
lst$IMP_main <- c('GS','Neg','Ren','LD','SP','Neg-2.0','Ren-2.0')
lst$IMP_main_shp <- c('GS'=8,'Neg'=9,'Ren'=3,'LD'=11,'SP'=4,'Neg-2.0'=10,'Ren-2.0'=12)
lst$IMP_cat_shp <- c('C1'=1,'C2'=2,'C3'=0,lst$IMP_main_shp)

df$scen_sens_shape <- tribble(~scen_sens_var,~Shape,
                              'Default',21,
                              'AdvTech',23,
                              'ConvTech',25,
                              'LimElec+',22,
                              'LimElec-',24)
df$R5map <- tribble(~Region,~R5lab,
                    'R5ASIA','Asia',
                    'R5LAM','Latin\nAmerica',
                    'R5MAF','Middle East\n& Africa',
                    'R5OECD90+EU','OECD & EU',
                    'R5REF','Reforming\nEconomies')

p$l_rangeleg <- tribble(~y,~label,
                        -.1,'Min',
                        1.1,'Max',
                        .5,'Median',
                        .2,'10th percentile',
                        .8,'90th percentile') %>% 
    mutate(x=0) %>% 
    ggplot()+
    geom_crossbar(aes(x=x),ymin=.2,ymax=.8,y=.5,width=.25,color='white',fill='grey')+
    geom_crossbar(aes(x=x),ymin=0,ymax=1,y=0,width=.25,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(y=y,label=label),x=.22,hjust=0,size=3)+
    labs(title='  AR6 range')+
    xlim(-.2,1.6)+ylim(-.2,1.3)+
    theme_void()+theme(plot.margin=unit(c(1,0,0,3),'mm'),plot.title=element_text(size=10))

# Fig.1 -----------------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Liq_Oil_and_Nat_Gas','Liquids-fossil','sandybrown',
                  'Fin_Ene_Liq_Hyd_syn','Liquids-synfuel','orchid',
                  'Fin_Ene_Gas_Fos','Gases-fossil','moccasin',
                  'Fin_Ene_Gas_Hyd_syn','Gases-synfuel','orchid1',
                  'Fin_Ene_Liq_and_Sol_Bio','Biomass','darkolivegreen2',
                  'Fin_Ene_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Heat','Heat','salmon',
                  'Fin_Ene_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Oth_inc_Solarand_Geo','Other','grey90')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)

df$var2 <- tribble(~Variable,~Legend,~Color,~Shape,
                   'Fin_Ene_Share_Ele','Electricity share','blue',21,
                   'Fin_Ene_Share_Syn_Hyd','Synfuels share','purple',23,
                   'Fin_Ene_Share_Hyd_Car','Hydrocarbon share','orange',24)
lst$leg2 <- as.character(df$var2$Legend); names(lst$leg2) <- as.character(df$var2$Variable)
lst$col2 <- as.character(df$var2$Color); names(lst$col2) <- as.character(df$var2$Variable)
lst$shp2 <- as.numeric(df$var2$Shape); names(lst$shp2) <- as.character(df$var2$Variable)

lst$finene_max <- df$all %>% 
    filter(Variable=='Fin_Ene',Scenario%in%lst$scen_rep,Year==2050,Region=='World') %>% 
    mutate(Value=ceiling(Value/10)*10) %>% .$Value %>% max()
df$tmp <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    filter(Variable%in%c(df$var$Variable,df$var2$Variable)) %>% 
    left_join(df$var,by='Variable') %>% 
    left_join(df$var2,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    mutate(Variable=factor(Variable,levels=rev(c(df$var$Variable,df$var2$Variable))),scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab))
df$subaxis <- df$tmp %>% filter(Variable%in%df$var2$Variable) %>% 
    mutate(Value=Value*lst$finene_max)
p$fec2 <- df$tmp %>% filter(Region=='World') %>% 
    filter(Variable%in%df$var$Variable) %>% 
    ggplot()+
    geom_area(aes(x=Year,y=Value,fill=Variable),stat='identity',position='stack',show.legend=T)+
    geom_path(data=df$subaxis,aes(x=Year,y=Value,color=Variable),show.legend=T)+
    geom_point(data=df$subaxis,aes(x=Year,y=Value,color=Variable,shape=Variable),fill='white',show.legend=T)+
    labs(x=NULL,y=expression(paste('Final energy (EJ ',yr^{-1},')')))+
    facet_grid(.~scen_lab)+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_y_continuous(limits=c(0,NA),sec.axis=sec_axis(~./lst$finene_max*100,labels=function(x){paste0(sprintf('%.0f',x),'%')},name='Final energy share (%)'))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    scale_color_manual(values=lst$col2,labels=lst$leg2,name=NULL)+
    scale_shape_manual(values=lst$shp2,labels=lst$leg2,name=NULL)+
    guides(fill=guide_legend(title='Demand (left-axis)',ncol=1,override.aes=list(linetype=NULL,shape=NULL,color='transparent')),
           color=guide_legend(title='Share (right-axis)',override.aes=list(fill='transparent')),
           shape=guide_legend(title='Share (right-axis)',override.aes=list(fill='transparent')))

df$tmp <- df$load_AR6_global %>% 
    filter(Variable=='Fin_Ene_Share_Hyd_Car',Year=='2050',Category%in%c('C1','C2','C3'),IMP_marker%in%lst$IMP_main) %>% 
    mutate(IMP_marker=factor(IMP_marker,levels=lst$IMP_main))
p$fec_hydcar_AR6 <- df$load_AR6_global %>% 
    filter(Variable=='Fin_Ene_Share_Hyd_Car',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n)),y=0,size=2)+
    geom_text(x=.7,label='Hydrocarbon\nshare',y=.9,size=2.8,hjust=0,color='grey50')+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5,color='orange')+
    scale_y_continuous(limits=c(0,1),sec.axis=sec_axis(~.,labels=scales::percent_format(accuracy=1),name='Final energy share (%)'))+
    labs(x=NULL,y=NULL)+
    mytheme$set1+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5),
                       axis.text.y.left=element_blank(),axis.line.y.left=element_blank(),axis.ticks.y.left=element_blank())+
    scale_shape_manual(values=lst$IMP_main_shp)+
    guides(shape=guide_legend(title='AR6 IMPs'))

lst$fin_ene_sec_max <- df$all %>% 
    filter(Region=='World',Scenario%in%lst$scen_rep,Variable%in%c('Fin_Ene_Ind','Fin_Ene_Res_and_Com','Fin_Ene_Tra'),Year==2050) %>% 
    mutate(Value=ceiling(Value/10)*10) %>% .$Value %>% max()
df$var <- tribble(~Variable,~Sector,~Carrier,
                  'Fin_Ene_Ind_SolidsCoa','Industry','Coal',
                  'Fin_Ene_Ind_Liq_Oil','Industry','Oil',
                  'Fin_Ene_Ind_Liq_Hyd_syn','Industry','Synfuel-liquids',
                  'Fin_Ene_Ind_Gas_Fos','Industry','Gas',
                  'Fin_Ene_Ind_Gas_Hyd_syn','Industry','Synfuel-gases',
                  'Fin_Ene_Ind_Liq_and_Sol_Bio','Industry','Biomass',
                  'Fin_Ene_Ind_Heat','Industry','Heat',
                  'Fin_Ene_Ind_Ele','Industry','Electricity',
                  'Fin_Ene_Ind_Hyd','Industry','Hydrogen',
                  'Fin_Ene_Ind_Oth','Industry','Other',
                  'Fin_Ene_Res_and_Com_SolidsCoa','Buildings','Coal',
                  'Fin_Ene_Res_and_Com_Liq_Oil','Buildings','Oil',
                  'Fin_Ene_Res_and_Com_Liq_Hyd_syn','Buildings','Synfuel-liquids',
                  'Fin_Ene_Res_and_Com_Gas_Fos','Buildings','Gas',
                  'Fin_Ene_Res_and_Com_Gas_Hyd_syn','Buildings','Synfuel-gases',
                  'Fin_Ene_Res_and_Com_SolidsBio','Buildings','Biomass',
                  'Fin_Ene_Res_and_Com_Heat','Buildings','Heat',
                  'Fin_Ene_Res_and_Com_Ele','Buildings','Electricity',
                  'Fin_Ene_Res_and_Com_Hyd','Buildings','Hydrogen',
                  'Fin_Ene_Res_and_Com_Oth','Buildings','Other',
                  'Fin_Ene_Tra_Liq_Coa','Transport','Coal',
                  'Fin_Ene_Tra_Liq_Oil','Transport','Oil',
                  'Fin_Ene_Tra_Liq_Nat_Gas','Transport','Gas',
                  'Fin_Ene_Tra_Liq_Hyd_syn','Transport','Synfuel-liquids',
                  'Fin_Ene_Tra_Gas','Transport','Gas',
                  'Fin_Ene_Tra_Liq_Bio','Transport','Biomass',
                  'Fin_Ene_Tra_Ele','Transport','Electricity',
                  'Fin_Ene_Tra_Hyd','Transport','Hydrogen',
                  'Fin_Ene_Tra_Oth','Transport','Other')
df$car <- tribble(~Carrier,~Color,
                  'Oil','sandybrown',
                  'Coal','grey70',
                  'Gas','moccasin',
                  'Biomass','darkolivegreen2',
                  'Heat','salmon',
                  'Electricity','lightsteelblue',
                  'Hydrogen','thistle2',
                  'Synfuel-liquids','orchid',
                  'Synfuel-gases','orchid1',
                  'Other','grey90')
lst$leg <- as.character(df$car$Carrier); names(lst$leg) <- as.character(df$car$Carrier)
lst$col <- as.character(df$car$Color); names(lst$col) <- as.character(df$car$Carrier)
p$fin_sec <- df$all %>% filter(Region=='World',Year==2050) %>% 
    filter(Variable%in%df$var$Variable) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    mutate(Carrier=factor(Carrier,levels=rev(unique(df$var$Carrier))),scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           Sector=factor(Sector,levels=unique(df$var$Sector))) %>% 
    ggplot()+
    geom_bar(aes(x=scen_lab,y=Value,fill=Carrier),stat='identity',position='stack',show.legend=T)+
    facet_grid(.~Sector)+
    labs(title=NULL,x=NULL,y=expression(paste('Final energy (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    guides(fill=guide_legend(ncol=1))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)

df$var2 <- tribble(~Variable,~Sector,~Carrier,
                   'Fin_Ene_Ind_Share_Ele','Industry','Electricity share',
                   'Fin_Ene_Ind_Share_Syn_Hyd','Industry','Synfuels share',
                   'Fin_Ene_Ind_Share_Hyd_Car','Industry','Hydrocarbon share',
                   'Fin_Ene_Res_and_Com_Share_Ele','Buildings','Electricity share',
                   'Fin_Ene_Res_and_Com_Share_Syn_Hyd','Buildings','Synfuels share',
                   'Fin_Ene_Res_and_Com_Share_Hyd_Car','Buildings','Hydrocarbon share',
                   'Fin_Ene_Tra_Share_Ele','Transport','Electricity share',
                   'Fin_Ene_Tra_Share_Syn_Hyd','Transport','Synfuels share',
                   'Fin_Ene_Tra_Share_Hyd_Car','Transport','Hydrocarbon share')
df$car2 <- tribble(~Carrier,~Color,~Shape,
                   'Electricity share','blue',21,
                   'Synfuels share','purple',23,
                   'Hydrocarbon share','orange',24)
lst$leg2 <- as.character(df$car2$Carrier); names(lst$leg2) <- as.character(df$car2$Carrier)
lst$col2 <- as.character(df$car2$Color); names(lst$col2) <- as.character(df$car2$Carrier)
lst$shp2 <- as.numeric(df$car2$Shape); names(lst$shp2) <- as.character(df$car2$Carrier)
df$tmp <- df$all %>% filter(Region=='World') %>% 
    filter(Variable%in%c(df$var$Variable,df$var2$Variable)) %>% 
    inner_join(bind_rows(df$var,df$var2),by=c('Variable')) %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    mutate(Variable=factor(Variable,levels=rev(c(df$var$Variable,df$var2$Variable))),Sector=factor(Sector,levels=unique(df$var$Sector)),
           Carrier=factor(Carrier,levels=rev(unique(c(df$var$Carrier,df$var2$Carrier)))),scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab))
df$subaxis <- df$tmp %>% filter(Variable%in%df$var2$Variable,Year==2050) %>% 
    mutate(Value=Value*lst$fin_ene_sec_max)
p$fec_sec2 <- df$tmp %>% filter(Region=='World',Year==2050) %>% 
    filter(Variable%in%df$var$Variable) %>% 
    ggplot()+
    geom_bar(aes(x=scen_lab,y=Value,fill=Carrier),stat='identity',position='stack',show.legend=T)+
    geom_point(data=df$subaxis,aes(x=scen_lab,y=Value,color=Carrier,shape=Carrier),fill='white',show.legend=T)+
    facet_grid(.~Sector)+
    labs(title=NULL,x=NULL,y=expression(paste('Final energy (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_y_continuous(limits=c(0,lst$fin_ene_sec_max),sec.axis=sec_axis(~./lst$fin_ene_sec_max*100,labels=function(x){paste0(sprintf('%.0f',x),'%')},name='Final energy share (%)'))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    scale_color_manual(values=lst$col2,labels=lst$leg2,name=NULL)+
    scale_shape_manual(values=lst$shp2,labels=lst$leg2,name=NULL)+
    guides(fill=guide_legend(title='Demand (left-axis)',ncol=1,override.aes=list(linetype=NULL,shape=NULL,color='transparent')),
           color=guide_legend(title='Share (right-axis)',override.aes=list(fill='transparent')),
           shape=guide_legend(title='Share (right-axis)',override.aes=list(fill='transparent')))

p$fec_sec2_ind <- df$tmp %>% filter(Region=='World',Year==2050) %>% 
    filter(Variable%in%df$var$Variable) %>% 
    filter(Sector=='Industry') %>% 
    ggplot()+
    geom_bar(aes(x=scen_lab,y=Value,fill=Carrier),stat='identity',position='stack',show.legend=T)+
    geom_point(data=df$subaxis %>% filter(Sector=='Industry'),
               aes(x=scen_lab,y=Value,color=Carrier,shape=Carrier),fill='white',show.legend=T)+
    labs(title='Industry',x=NULL,y=expression(paste('Final energy (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_y_continuous(limits=c(0,lst$fin_ene_sec_max),sec.axis=sec_axis(~./lst$fin_ene_sec_max*100,labels=function(x){paste0(sprintf('%.0f',x),'%')},name='Final energy share (%)'))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    scale_color_manual(values=lst$col2,labels=lst$leg2,name=NULL)+
    scale_shape_manual(values=lst$shp2,labels=lst$leg2,name=NULL)+
    guides(fill=guide_legend(title='Demand (left-axis)',ncol=1,override.aes=list(linetype=NULL,shape=NULL,color='transparent')),
           color=guide_legend(title='Share (right-axis)',override.aes=list(fill='transparent')),
           shape=guide_legend(title='Share (right-axis)',override.aes=list(fill='transparent')))
p$fec_sec2_bui <- df$tmp %>% filter(Region=='World',Year==2050) %>% 
    filter(Variable%in%df$var$Variable) %>% 
    filter(Sector=='Buildings') %>% 
    ggplot()+
    geom_bar(aes(x=scen_lab,y=Value,fill=Carrier),stat='identity',position='stack',show.legend=T)+
    geom_point(data=df$subaxis %>% filter(Sector=='Buildings'),
               aes(x=scen_lab,y=Value,color=Carrier,shape=Carrier),fill='white',show.legend=T)+
    labs(title='Buildings',x=NULL,y=expression(paste('Final energy (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_y_continuous(limits=c(0,lst$fin_ene_sec_max),sec.axis=sec_axis(~./lst$fin_ene_sec_max*100,labels=function(x){paste0(sprintf('%.0f',x),'%')},name='Final energy share (%)'))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    scale_color_manual(values=lst$col2,labels=lst$leg2,name=NULL)+
    scale_shape_manual(values=lst$shp2,labels=lst$leg2,name=NULL)+
    guides(fill=guide_legend(title='Demand (left-axis)',ncol=1,override.aes=list(linetype=NULL,shape=NULL,color='transparent')),
           color=guide_legend(title='Share (right-axis)',override.aes=list(fill='transparent')),
           shape=guide_legend(title='Share (right-axis)',override.aes=list(fill='transparent')))
p$fec_sec2_tra <- df$tmp %>% filter(Region=='World',Year==2050) %>% 
    filter(Variable%in%df$var$Variable) %>% 
    filter(Sector=='Transport') %>% 
    ggplot()+
    geom_bar(aes(x=scen_lab,y=Value,fill=Carrier),stat='identity',position='stack',show.legend=T)+
    geom_point(data=df$subaxis %>% filter(Sector=='Transport'),
               aes(x=scen_lab,y=Value,color=Carrier,shape=Carrier),fill='white',show.legend=T)+
    labs(title='Transport',x=NULL,y=expression(paste('Final energy (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_y_continuous(limits=c(0,lst$fin_ene_sec_max),sec.axis=sec_axis(~./lst$fin_ene_sec_max*100,labels=function(x){paste0(sprintf('%.0f',x),'%')},name='Final energy share (%)'))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    scale_color_manual(values=lst$col2,labels=lst$leg2,name=NULL)+
    scale_shape_manual(values=lst$shp2,labels=lst$leg2,name=NULL)+
    guides(fill=guide_legend(title='Demand (left-axis)',ncol=1,override.aes=list(linetype=NULL,shape=NULL,color='transparent')),
           color=guide_legend(title='Share (right-axis)',override.aes=list(fill='transparent')),
           shape=guide_legend(title='Share (right-axis)',override.aes=list(fill='transparent')))


df$tmp <- df$load_AR6_global %>% 
    filter(Variable=='Fin_Ene_Ind_Share_Hyd_Car',Year=='2050',Category%in%c('C1','C2','C3'),IMP_marker%in%lst$IMP_main) %>% 
    mutate(IMP_marker=factor(IMP_marker,levels=lst$IMP_main))
p$fec_sec_hydcar_ind_AR6 <- df$load_AR6_global %>% 
    filter(Variable=='Fin_Ene_Ind_Share_Hyd_Car',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    mutate(Value=pmin(Value,1)) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p100),size=2,angle=90,hjust=-.2,vjust=.5)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5,color='orange',show.legend=F)+
    scale_y_continuous(limits=c(0,1),sec.axis=sec_axis(~.,labels=scales::percent_format(accuracy=1),name='Final energy share (%)'))+
    labs(x=NULL,y=NULL)+
    mytheme$set1+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5),
                       axis.text.y.left=element_blank(),axis.line.y.left=element_blank(),axis.ticks.y.left=element_blank(),
                       strip.background=element_blank())+
    scale_shape_manual(values=lst$IMP_main_shp)
df$tmp <- df$load_AR6_global %>% 
    filter(Variable=='Fin_Ene_Res_and_Com_Share_Hyd_Car',Year=='2050',Category%in%c('C1','C2','C3'),IMP_marker%in%lst$IMP_main) %>% 
    mutate(IMP_marker=factor(IMP_marker,levels=lst$IMP_main))
p$fec_sec_hydcar_bui_AR6 <- df$load_AR6_global %>% 
    filter(Variable=='Fin_Ene_Res_and_Com_Share_Hyd_Car',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    mutate(Value=pmin(Value,1)) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p100),size=2,angle=90,hjust=-.2,vjust=.5)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5,color='orange',show.legend=F)+
    scale_y_continuous(limits=c(0,1),sec.axis=sec_axis(~.,labels=scales::percent_format(accuracy=1),name='Final energy share (%)'))+
    labs(x=NULL,y=NULL)+
    mytheme$set1+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5),
                       axis.text.y.left=element_blank(),axis.line.y.left=element_blank(),axis.ticks.y.left=element_blank(),
                       strip.background=element_blank())+
    scale_shape_manual(values=lst$IMP_main_shp)
df$tmp <- df$load_AR6_global %>% 
    filter(Variable=='Fin_Ene_Tra_Share_Hyd_Car',Year=='2050',Category%in%c('C1','C2','C3'),IMP_marker%in%lst$IMP_main) %>% 
    mutate(IMP_marker=factor(IMP_marker,levels=lst$IMP_main))
p$fec_sec_hydcar_tra_AR6 <- df$load_AR6_global %>% 
    filter(Variable=='Fin_Ene_Tra_Share_Hyd_Car',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    mutate(Value=pmin(Value,1)) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p0),size=2,angle=90,hjust=1.2,vjust=.5)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5,color='orange',show.legend=F)+
    scale_y_continuous(limits=c(0,1),sec.axis=sec_axis(~.,labels=scales::percent_format(accuracy=1),name='Final energy share (%)'))+
    labs(x=NULL,y=NULL)+
    mytheme$set1+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5),
                       axis.text.y.left=element_blank(),axis.line.y.left=element_blank(),axis.ticks.y.left=element_blank(),
                       strip.background=element_blank())+
    scale_shape_manual(values=lst$IMP_main_shp)

df$var <- tribble(~Variable,~Device,
                  'Tec_sto_Sha_Tra_Fre_Roa_BEV','Freight',
                  'Tec_sto_Sha_Tra_Fre_Roa_FCV','Freight',
                  'Tec_sto_Sha_Tra_Pss_Roa_PHV','Passenger',
                  'Tec_sto_Sha_Tra_Pss_Roa_BEV','Passenger',
                  'Tec_sto_Sha_Tra_Pss_Roa_FCV','Passenger',
                  'Tec_sto_Sha_Ind_HeatingBoi_Ele','Boiler',
                  'Tec_sto_Sha_Ind_HeatingBoi_Hyd','Boiler',
                  'Tec_sto_Sha_Ind_HeatingFur_Ele','Furnace',
                  'Tec_sto_Sha_Ind_HeatingFur_Hyd','Furnace',
                  'Tec_sto_Sha_Com_HeatingSpa_EHP','Commercial',
                  'Tec_sto_Sha_Com_HeatingSpa_Oth','Commercial',
                  'Tec_sto_Sha_Res_HeatingSpa_EHP','Residential',
                  'Tec_sto_Sha_Res_HeatingSpa_Oth','Residential')
df$lab_tech <- tribble(~Device,~Tech_Label,
                       'Passenger','Passenger\nroad transport',
                       'Freight','Freight\nroad transport',
                       'Boiler','Industry\nboiler',
                       'Furnace','Industry\nfurnace',
                       'Residential','Residential\nspace heating',
                       'Commercial','Commercial\nspace heating')
p$techshare <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    inner_join(df$var,by='Variable') %>% 
    group_by(Model,Scenario,Device,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    inner_join(df$lab_tech,by='Device') %>% 
    mutate(Tech_Label=factor(Tech_Label,levels=df$lab_tech$Tech_Label)) %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab)) %>% 
    ggplot()+
    geom_line(aes(x=Year,y=Value,color=scen_lab))+
    geom_point(aes(x=Year,y=Value,color=scen_lab),shape=21,fill='white')+
    scale_x_continuous(limits=c(2020,2050),breaks=seq(2020,2050,by=10))+
    scale_y_continuous(limits=c(0,1), labels=scales::percent)+
    facet_wrap(~Tech_Label,nrow=1)+
    scale_color_manual(values=lst$scen_col)+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),strip.text=element_text(size=9),axis.text.x=element_text(angle=45,hjust=1),legend.margin=margin(0,0,0,0))+
    labs(x=NULL,y='Diffusion rate')+
    guides(color=guide_legend(title=NULL))


p$l_fec <- get_legend(p$fec2+theme(legend.position='right',legend.key.size=unit(4,'mm'),legend.spacing=unit(0,'mm')))
p$l_fec2 <- get_legend(p$fec_hydcar_AR6+theme(legend.position='right',legend.key.size=unit(3.5,'mm'))+
                           guides(shape=guide_legend(ncol=2,title='AR6 IMPs',override.aes=list(color='black'))))
p$l_tmp <- plot_grid(p$l_fec,ggplotGrob(p$l_rangeleg),p$l_fec2,ncol=1,rel_heights=c(1,.35,.35))
p$tmp0 <- plot_grid(p$fec2+theme(legend.position='none',axis.title.y.right=element_blank(),axis.text.y.right=element_blank(),axis.line.y.right=element_blank(),axis.ticks.y.right=element_blank(),plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt')),
                    p$fec_hydcar_AR6+theme(legend.position='none',plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt')),nrow=1,axis='tb',align='h',rel_widths=c(1,.35))
p$tmp1 <- plot_grid(p$fec_sec2_ind+theme(legend.position='none',axis.title.y.right=element_blank(),axis.text.y.right=element_blank(),axis.line.y.right=element_blank(),axis.ticks.y.right=element_blank(),plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt')),
                    p$fec_sec_hydcar_ind_AR6+theme(plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt'),axis.title.y.right=element_blank(),axis.text.y.right=element_blank()),
                    p$fec_sec2_bui+theme(legend.position='none',axis.title.y=element_blank(),axis.text.y=element_blank(),axis.line.y.right=element_blank(),axis.ticks.y.right=element_blank(),plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt')),
                    p$fec_sec_hydcar_bui_AR6+theme(plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt'),axis.title.y.right=element_blank(),axis.text.y.right=element_blank()),
                    p$fec_sec2_tra+theme(legend.position='none',axis.title.y=element_blank(),axis.text.y=element_blank(),axis.line.y.right=element_blank(),axis.ticks.y.right=element_blank(),plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt')),
                    p$fec_sec_hydcar_tra_AR6+theme(plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt')),
                    nrow=1,axis='tb',align='h',rel_widths=c(1.4,.4,1,.4,1,.88))
p$tmp2 <- plot_grid(p$tmp0,p$tmp1,ncol=1,labels=c('a','b'),rel_heights=c(1,1.1))
p$tmp3 <- plot_grid(p$tmp2,p$l_tmp,nrow=1,rel_widths=c(1,.3))
p$tmp <- plot_grid(p$tmp3,p$techshare,ncol=1,rel_heights=c(1,.4),labels=c('','c'))

ggsave(filename='output/fig1.png',plot=p$tmp,width=180,height=170,units='mm',dpi=300)


# Fig.2 -------------------------------------------------------------------

lst$sec_ene_ele_max <- df$all %>% 
    filter(Scenario%in%lst$scen_rep,Region=='World',Year==2050) %>% 
    filter(Variable=='Sec_Ene_Ele') %>% 
    group_by(Model,Scenario,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    mutate(Value=ceiling(Value/50)*50) %>% .$Value %>% max()
df$var <- tribble(~Variable,~Source,~Hydrogen,~Synfuel,~Final,
                  'Sec_Ene_Hyd_Fos','Fossil','Hydrogen','Loss2','Loss',
                  'Sec_Ene_Hyd_Bio','Biomass','Hydrogen','Loss2','Loss',
                  'Los_Ele','Electricity','Loss','Loss','Loss',
                  'Sec_Ene_Ele_Own_Use_DAC','Electricity','DAC','Loss2','Loss',
                  'Fin_Ene_Ele','Electricity','Electricity','Electricity','Electricity',
                  'Los_Hyd','Electricity','Hydrogen','Loss2','Loss',
                  'Fin_Ene_Liq_Hyd_syn','Electricity','Hydrogen','Synfuel','Synfuel',
                  'Fin_Ene_Gas_Hyd_syn','Electricity','Hydrogen','Synfuel','Synfuel',
                  'Fin_Ene_Hyd','Electricity','Hydrogen','Hydrogen','Hydrogen'
)
lst$carlev <- c('Synfuel','Hydrogen','Electricity','Fossil','Biomass',
                'DAC','Loss2','Loss')
lst$col <- c('Synfuel'='orchid','Hydrogen'='thistle2','Electricity'='lightsteelblue','Fossil'='sandybrown','Biomass'='darkolivegreen2',
             'DAC'='darkgoldenrod2','Loss'='grey','Loss2'='grey')
p$seceneflo2 <- df$all %>% filter(Region=='World',Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    select(colnames(df$var),Value,scen_lab) %>% 
    pivot_longer(cols=!c(scen_lab,Variable,Value),names_to='x',values_to='Carrier') %>% 
    mutate(Label=Carrier,Alpha=1,Positionh=.5,Positionv=.5) %>% 
    mutate(Label=ifelse(x=='Hydrogen'&Carrier%in%c('Electricity'),' ',Label)) %>%
    mutate(Label=ifelse(x=='Synfuel'&Carrier%in%c('Electricity','Hydrogen','Loss'),' ',Label)) %>%
    mutate(Label=ifelse(x=='Synfuel'&Carrier=='Loss2','Loss',Label)) %>%
    mutate(Label=ifelse(x=='Final'&Carrier=='Synfuel',' ',Label)) %>%
    mutate(Alpha=ifelse(x=='Hydrogen'&Carrier%in%c('Electricity'),.5,Alpha)) %>%
    mutate(Alpha=ifelse(x=='Synfuel'&Carrier%in%c('Electricity','Hydrogen','Loss'),.5,Alpha)) %>%
    mutate(Alpha=ifelse(x=='Final'&Carrier=='Loss',.5,Alpha)) %>%
    mutate(Positionh=ifelse(x=='Source',.2,ifelse(x=='Final',.8,.5))) %>%
    mutate(Positionv=ifelse(x=='Source'&Carrier=='Fossil',-.5,Positionv)) %>%
    # filter(!is.na(Carrier)) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           Carrier=factor(Carrier,levels=rev(lst$carlev))) %>% 
    ggplot(aes(x=x,y=Value,alluvium=Variable,stratum=Carrier,label=Carrier))+
    geom_flow(aes(fill=Carrier),show.legend=F)+
    geom_stratum(aes(fill=Carrier,alpha=Alpha),color='transparent',show.legend=F)+
    geom_text(aes(label=Label,hjust=Positionh,vjust=Positionv),stat='stratum',size=2.5)+
    labs(title=NULL,x=NULL,y=expression(paste('Secondary energy (EJ ',yr^{-1},')')))+
    ylim(0,lst$sec_ene_ele_max)+
    scale_x_discrete(limits=colnames(df$var)[-1],labels=c('Source','','','Demand'),expand=c(.05,.05))+
    scale_fill_manual(values=lst$col,name=NULL)+
    scale_alpha_continuous(limits=c(0,1),range=c(0,1))+
    facet_grid(.~scen_lab)+
    mytheme$set1+
    theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    guides(fill=guide_legend(title=NULL))

lst$Prm_Ene_NonBioRen_max <- df$all %>% 
    filter(Scenario%in%lst$scen_rep,Region=='World',Year==2050) %>% 
    filter(Variable=='Prm_Ene_NonBioRen') %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max()
p$tmp1 <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    filter(Variable=='Prm_Ene_NonBioRen') %>%
    inner_join(df$scen_lab,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab)) %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=scen_lab),show.legend=T)+
    geom_point(aes(x=Year,y=Value,color=scen_lab),fill='white',shape=21,show.legend=T)+
    ylim(0,lst$Prm_Ene_NonBioRen_max)+
    scale_color_manual(values=lst$scen_col)+
    labs(title='Non-biomass renewables\nprimary supply',x=NULL,y=expression(paste('Primary energy (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='none',strip.background=element_blank())+
    guides(color=guide_legend(title=NULL))
df$tmp <- df$load_AR6_global %>% 
    filter(Variable=='Prm_Ene_NonBioRen',Year=='2050',Category%in%c('C1','C2','C3'),!(is.na(IMP_marker)))
p$tmp2 <- df$load_AR6_global %>% 
    filter(Variable=='Prm_Ene_NonBioRen',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p100),size=2,angle=90,hjust=-.2,vjust=.5)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5,show.legend=F)+
    ylim(0,lst$Prm_Ene_NonBioRen_max)+
    labs(x=NULL,y=NULL)+
    scale_shape_manual(values=lst$IMP_main_shp)+
    mytheme$set1
p$Prm_Ene_NonBioRen <- plot_grid(p$tmp1+theme(plot.margin=unit(c(5.5,5.5,5.5,5.5),unit='pt')),
                                 p$tmp2+theme(plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt'),axis.text.x=element_text(angle=90,hjust=1,vjust=.5),axis.text.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank(),plot.background=element_blank()),
                                 nrow=1,axis='tb',align='h',rel_widths=c(1,.3))

lst$Sec_Ene_Hyd_max <- df$all %>% 
    filter(Scenario%in%lst$scen_rep,Region=='World',Year==2050) %>% 
    filter(Variable=='Sec_Ene_Hyd') %>% 
    mutate(Value=ceiling(Value/10)*10) %>% .$Value %>% max()
p$tmp1 <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    filter(Variable=='Sec_Ene_Hyd') %>%
    inner_join(df$scen_lab,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab)) %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=scen_lab),show.legend=T)+
    geom_point(aes(x=Year,y=Value,color=scen_lab),fill='white',shape=21,show.legend=T)+
    ylim(0,lst$Sec_Ene_Hyd)+
    scale_color_manual(values=lst$scen_col)+
    labs(title='Hydrogen generation',x=NULL,y=expression(paste('Hydrogen supply (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position=c(.47,.9),strip.background=element_blank(),legend.key.height=unit(3,'mm'),legend.text=element_text(size=7.5),legend.background=element_blank())+
    guides(color=guide_legend(title=NULL))
df$tmp <- df$load_AR6_global %>% 
    filter(Variable=='Sec_Ene_Hyd',Year=='2050',Category%in%c('C1','C2','C3'),!(is.na(IMP_marker)))
p$tmp2 <- df$load_AR6_global %>% 
    filter(Variable=='Sec_Ene_Hyd',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p100),size=2,angle=90,hjust=-.2,vjust=.5)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5,show.legend=F)+
    ylim(0,lst$Sec_Ene_Hyd)+
    scale_shape_manual(values=lst$IMP_main_shp)+
    labs(x=NULL,y=NULL)+
    mytheme$set1
p$Sec_Ene_Hyd <- plot_grid(p$tmp1+theme(plot.margin=unit(c(5.5,5.5,5.5,5.5),unit='pt')),
                           p$tmp2+theme(plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt'),axis.text.x=element_text(angle=90,hjust=1,vjust=.5),axis.text.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank(),plot.background=element_blank()),
                           nrow=1,axis='tb',align='h',rel_widths=c(1,.3))

df$var <- tribble(~Variable,~Legend,~Color,
                  'Sec_Ene_Ele_Fos','Fossil','grey50',
                  'Sec_Ene_Ele_Nuc','Nuclear','moccasin',
                  'Sec_Ene_Ele_Hyd','Hydro','lightsteelblue',
                  'Sec_Ene_Ele_Bio','Biomass','darkolivegreen2',
                  'Sec_Ene_Ele_SolarPV','Solar PV','lightsalmon',
                  'Sec_Ene_Ele_Win','Wind','lightskyblue3',
                  'Sec_Ene_Ele_Oth_Ren','Other renewables','grey')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
df$tmp <- df$all %>% filter(Variable=='Sec_Ene_Ele',Region=='World',Year>=2020) %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab))
p$sec_ene_ele_CCU <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    filter(scen_lab=='1.5C-CCU') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab)) %>%
    ggplot()+
    geom_area(aes(x=Year,y=Value,fill=Variable),stat='identity',position='stack',show.legend=T)+
    geom_line(data=df$tmp,aes(x=Year,y=Value,color=scen_lab))+
    geom_point(data=df$tmp,aes(x=Year,y=Value,color=scen_lab),shape=21,fill='white')+
    labs(x=NULL,y=expression(paste('Electricity supply (EJ ',yr^{-1},')')))+
    ylim(0,lst$sec_ene_ele_max)+
    mytheme$set1+
    theme(legend.position=c(.35,.85),strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),legend.background=element_blank(),legend.key.size=unit(4,'mm'))+
    scale_fill_manual(values=lst$col,labels=lst$leg,name=NULL)+
    scale_color_manual(values=lst$scen_col)+
    guides(color=guide_legend(title=NULL,override.aes=list(fill='transparent')),fill='none')

p$l_sec_ene_ele_CCU <- p$sec_ene_ele_CCU+
    guides(fill=guide_legend(title=NULL,nrow=1),color='none')+
    theme(legend.position='bottom',legend.margin=margin(0,0,0,0))

df$var <- tribble(~Variable,~Energy,
                  'Prm_Ene_Coa','Primary',
                  'Prm_Ene_Oil','Primary',
                  'Prm_Ene_Gas','Primary',
                  'Prm_Ene_Bio','Primary',
                  'Fin_Ene_Solids','Final',
                  'Fin_Ene_Gas','Final',
                  'Fin_Ene_Liq','Final')
lst$scen_sens_size <- c('Default'=3,'Sensitivity'=1)
df$tmp <- df$load_AR6_global %>% filter(Region=='World',Year==2050,Category%in%c('C1','C2','C3')) %>% 
    inner_join(df$var,by='Variable') %>% 
    group_by(Model,Scenario,Region,Energy,Category) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    pivot_wider(names_from='Energy',values_from='Value') %>% 
    filter(!is.na(Primary),!is.na(Final))
df$tmp2 <- df$load_AR6_global %>% filter(Region=='World',Year==2050,!(is.na(IMP_marker)),Category%in%c('C1','C2','C3')) %>% 
    inner_join(df$var,by='Variable') %>% 
    group_by(IMP_marker,Region,Energy,Category) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    pivot_wider(names_from='Energy',values_from='Value') %>% 
    filter(!is.na(Primary),!is.na(Final))
p$tmp <- df$all %>% filter(Region=='World',Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    group_by(Model,Scenario,Region,Energy) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    pivot_wider(names_from='Energy',values_from='Value',values_fill=0) %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(scen_sens_var=ifelse(scen_sens_var=='Default','Default','Sensitivity')) %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat)) %>% 
    filter(Scenario%in%lst$scen_500all) %>% 
    ggplot()+
    geom_point(data=df$tmp,aes(x=Primary,y=Final,shape=Category),color='grey',size=1)+
    geom_point(data=df$tmp2,aes(x=Primary,y=Final,shape=IMP_marker),color='black',size=1,show.legend=F)+
    geom_text(data=df$tmp2,aes(x=Primary,y=Final,label=IMP_marker),color='black',size=2.5,vjust=1.2,show.legend=F)+
    geom_point(aes(x=Primary,y=Final,color=scen_sens_base,size=scen_sens_var))+
    geom_text(data=. %>% filter(Scenario%in%lst$scen_rep),hjust=.8,vjust=-1,
              aes(x=Primary,y=Final,color=scen_sens_base,label=scen_sens_base),size=2.5,show.legend=F)+
    geom_segment(x=600,xend=200,y=30,yend=30,arrow=arrow(length=unit(2.5,'mm')))+
    geom_text(x=400,y=10,label='Low dependency on fossil and biomass',size=3)+
    geom_segment(x=30,xend=30,y=100,yend=300,arrow=arrow(length=unit(2.5,'mm')))+
    geom_text(x=0,y=200,label='Hydrocarbon availability in end-use',size=3,angle=90)+
    xlim(0,NA)+ylim(0,NA)+
    scale_color_manual(values=lst$scen_col)+
    scale_size_manual(values=lst$scen_sens_size)+
    scale_shape_manual(breaks=c('C1','C2','C3'),values=lst$IMP_cat_shp)+
    labs(x=expression(paste('Fossil and biomass primary supply (EJ ',yr^{-1},')')),
         y=expression(paste('Hydrocarbon energy in end-use (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='right',legend.box='vertical',legend.margin=margin(0,0,0,0),legend.key.height=unit(3,'mm'))+
    guides(color='none',shape=guide_legend(title='AR6 category'),size=guide_legend(title="This study's\nscenarios"))
p$tmp2 <- p$tmp+
    scale_shape_manual(breaks=lst$IMP_main,values=lst$IMP_cat_shp)+
    theme(legend.key.size=unit(3,'mm'))+
    guides(shape=guide_legend(title='AR6 IMPs',override.aes=list(color='black')),size='none')
p$l1 <- get_legend(p$tmp)
p$l2 <- get_legend(p$tmp2)
p$l_hydcar <- plot_grid(p$l1,p$l2,ncol=1)
p$hydrocar <- p$tmp+theme(legend.position='none')

p$l_tmp1 <- plot_grid(p$l_hydcar,ggplotGrob(p$l_rangeleg),ncol=1,rel_heights=c(1,.35))
p$l_tmp2 <- get_legend(p$l_sec_ene_ele_CCU+theme(legend.key.size=unit(3.5,'mm'),legend.spacing=unit(0,'mm')))
p$tmp1 <- plot_grid(p$Sec_Ene_Hyd+theme(plot.margin=unit(c(0,0,0,2),unit='mm')),p$Prm_Ene_NonBioRen+theme(plot.margin=unit(c(0,0,0,2),unit='mm')),
                    ncol=1,labels=c('b','c'),rel_heights=c(1,1.1))
p$tmp2 <- plot_grid(p$tmp1,p$hydrocar+theme(legend.position='none'),p$l_tmp1,
                    nrow=1,labels=c('','d',''),rel_widths=c(1,1.7,.55))
p$tmp3 <- plot_grid(p$sec_ene_ele_CCU+theme(plot.margin=unit(c(5.5,0,0,5.5),unit='pt')),
                    p$seceneflo2+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.line.y=element_blank(),plot.margin=unit(c(5.5,5.5,0,0),unit='pt')),
                    nrow=1,axis='tb',align='h',rel_widths=c(1,2.8))
p$tmp <- plot_grid(p$tmp3,p$l_tmp2,p$tmp2,ncol=1,labels=c('a','',''),rel_heights=c(1,.15,1.25))
ggsave(filename='output/fig2.png',plot=p$tmp,width=180,height=170,units='mm',dpi=300)


# Fig.3 ------------------------------------------------------------------

lst$CDR_max_AR6 <- df$load_AR6_global %>% 
    filter(Variable%in%c('Car_Seq_Dir_Air_Cap','Car_Seq_CCS_Bio'),Region=='World',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    group_by(Model,Scenario,Category,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    mutate(Value=Value/1000) %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max()
lst$CDR_max <- df$all %>% 
    filter(Scenario%in%lst$scen_rep,Region=='World',Year==2050) %>% 
    filter(Variable%in%c('CCUSGeo_Sto_Bio','CCUSGeo_Sto_Dir_Air_Cap')) %>% 
    mutate(Value=Value/1000) %>% 
    group_by(Model,Scenario,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max(lst$CDR_max_AR6)+2
p$tmp1 <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    filter(Variable%in%c('CCUSGeo_Sto_Bio','CCUSGeo_Sto_Dir_Air_Cap')) %>% 
    group_by(Model,Scenario,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab)) %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    mutate(Value=Value/1000) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=scen_lab),show.legend=T)+
    geom_point(aes(x=Year,y=Value,color=scen_lab),fill='white',shape=21,show.legend=T)+
    ylim(0,lst$CDR_max)+
    labs(title='CDR in energy sector',x=NULL,y=expression(paste('CDR (Gt-',CO[2],' ',yr^{-1},')')))+
    scale_color_manual(values=lst$scen_col)+
    mytheme$set1+theme(legend.position=c(.35,.8),strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    guides(color=guide_legend(title=NULL))
df$tmp <- df$load_AR6_global %>% 
    filter(Variable%in%c('Car_Seq_Dir_Air_Cap','Car_Seq_CCS_Bio'),Year=='2050',Category%in%c('C1','C2','C3'),IMP_marker%in%lst$IMP_main) %>% 
    group_by(Model,Scenario,Category,IMP_marker,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    mutate(Value=Value/1000) %>% 
    mutate(IMP_marker=factor(IMP_marker,levels=lst$IMP_main))
p$tmp2 <- df$load_AR6_global %>% 
    filter(Variable%in%c('Car_Seq_Dir_Air_Cap','Car_Seq_CCS_Bio'),Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    group_by(Model,Scenario,Category,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    mutate(Variable='Car_Seq_CDR',Value=Value/1000) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p100),size=2,angle=90,hjust=-.2,vjust=.5)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5,show.legend=F)+
    ylim(0,lst$CDR_max)+
    labs(x=NULL,y=NULL)+
    scale_shape_manual(values=lst$IMP_main_shp)+
    mytheme$set1
p$CDR <- plot_grid(p$tmp1+theme(legend.position='none',plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt')),
                   p$tmp2+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5),axis.text.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank(),plot.background=element_blank(),plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt')),
                   nrow=1,axis='tb',align='h',rel_widths=c(1,.3))

lst$DAC_max <- df$all %>% 
    filter(Scenario%in%lst$scen_rep,Region=='World',Year==2050) %>% 
    filter(Variable=='Car_Cap_Dir_Air_Cap') %>% 
    mutate(Value=Value/1000) %>% 
    group_by(Model,Scenario,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    mutate(Value=ceiling(Value*10)/10) %>% .$Value %>% max()
p$tmp1 <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    filter(Variable=='Car_Cap_Dir_Air_Cap') %>% 
    group_by(Model,Scenario,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab)) %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    mutate(Value=Value/1000) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=scen_lab),show.legend=T)+
    geom_point(aes(x=Year,y=Value,color=scen_lab),fill='white',shape=21,show.legend=T)+
    ylim(0,lst$DAC_max)+
    scale_color_manual(values=lst$scen_col)+
    labs(title=expression(paste(CO[2],' capture by DAC')),x=NULL,y=expression(paste('Carbon capture (Gt-',CO[2],' ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position=c(.35,.8),strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    guides(color=guide_legend(title=NULL))
df$tmp <- df$load_AR6_global %>% 
    filter(Variable=='Car_Seq_Dir_Air_Cap',Year=='2050',Category%in%c('C1','C2','C3'),IMP_marker%in%lst$IMP_main) %>% 
    mutate(Value=Value/1000) %>% 
    mutate(IMP_marker=factor(IMP_marker,levels=lst$IMP_main))
p$tmp2 <- df$load_AR6_global %>% 
    filter(Variable=='Car_Seq_Dir_Air_Cap',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    filter(Value>=0) %>% 
    mutate(Value=Value/1000) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p100),size=2,angle=90,hjust=-.2,vjust=.5)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5,show.legend=F)+
    ylim(0,lst$DAC_max)+
    labs(x=NULL,y=NULL)+
    scale_shape_manual(values=lst$IMP_main_shp)+
    mytheme$set1
p$DAC <- plot_grid(p$tmp1+theme(legend.position='none',legend.background=element_blank(),plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt')),
                   p$tmp2+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5),axis.text.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank(),plot.background=element_blank(),plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt')),
                   nrow=1,axis='tb',align='h',rel_widths=c(1,.3))

lst$CCS_max_AR6 <- df$load_AR6_global %>% 
    filter(Variable=='Car_Seq_CCS',Region=='World',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    mutate(Value=Value/1000) %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max()
lst$CCS_max <- df$all %>% 
    filter(Scenario%in%lst$scen_rep,Region=='World',Year==2050) %>% 
    filter(Variable=='Car_Seq_CCS') %>% 
    mutate(Value=Value/1000) %>% 
    group_by(Model,Scenario,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max(lst$CCS_max_AR6)+2
p$tmp1 <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    filter(Variable=='Car_Seq_CCS') %>% 
    group_by(Model,Scenario,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab)) %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    mutate(Value=Value/1000) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=scen_lab),show.legend=T)+
    geom_point(aes(x=Year,y=Value,color=scen_lab),fill='white',shape=21,show.legend=T)+
    ylim(0,lst$CCS_max)+
    scale_color_manual(values=lst$scen_col)+
    labs(title='CCS',x=NULL,y=expression(paste('CCS (Gt-',CO[2],' ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position=c(.35,.8),strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    guides(color=guide_legend(title=NULL))
df$tmp <- df$load_AR6_global %>% 
    filter(Variable=='Car_Seq_CCS',Year=='2050',Category%in%c('C1','C2','C3'),IMP_marker%in%lst$IMP_main) %>% 
    mutate(Value=Value/1000) %>% 
    mutate(IMP_marker=factor(IMP_marker,levels=lst$IMP_main))
p$tmp2 <- df$load_AR6_global %>% 
    filter(Variable=='Car_Seq_CCS',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    filter(Value>=0) %>% 
    mutate(Value=Value/1000) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p100),size=2,angle=90,hjust=-.2,vjust=.5)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5)+
    ylim(0,lst$CCS_max)+
    labs(x=NULL,y=NULL)+
    scale_shape_manual(values=lst$IMP_main_shp)+
    mytheme$set1
p$l_IMP <- get_legend(p$tmp2+theme(legend.key.size=unit(5,'mm'))+guides(shape=guide_legend(title='AR6 IMPs')))
p$l_CCS_AR6 <- get_legend(p$tmp2+theme(legend.key.height=unit(4,'mm'))+guides(shape=guide_legend(title='AR6 IMPs')))
p$CCS <- plot_grid(p$tmp1+theme(legend.position=c(.48,.8),plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt'),legend.background=element_blank()),
                   p$tmp2+theme(legend.position='none',axis.text.x=element_text(angle=90,hjust=1,vjust=.5),axis.text.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank(),plot.background=element_blank(),plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt')),
                   nrow=1,axis='tb',align='h',rel_widths=c(1,.3))

df$var <- tribble(~Variable,~Source,~'Capture & Use',~Sequestration,~Destination,
                  'Emi_CO2_Ene_Com_exc_CCUS','Fossil','Fossil','Emission','Atmosphere',
                  'Emi_CO2_Ene_Bio_exc_CCUS','Atmosphere','Biomass','Neutral','Atmosphere',
                  'CCUSUti_Ene_Bio','Atmosphere','Biomass','Utilization','Atmosphere',
                  'CCUSUti_Ene_Dir_Air_Cap','Atmosphere','DAC','Utilization','Atmosphere',
                  'CCUSUti_Ene_Fos','Fossil','Fossil','Utilization','Atmosphere',
                  'CCUSGeo_Sto_Bio','Atmosphere','Biomass','Storage','Ground',
                  'CCUSGeo_Sto_Dir_Air_Cap','Atmosphere','DAC','Storage','Ground',
                  'CCUSGeo_Sto_Fos','Fossil','Fossil','Storage','Ground')
lst$Seq <- c('Storage','Utilization','Neutral','Emission','DAC','Biomass','Fossil','Ground','Atmosphere')
lst$col <- c('Storage'='moccasin','Utilization'='orchid1','Neutral'='darkolivegreen2','Emission'='grey',
             'Biomass'='darkolivegreen2','DAC'='thistle2','Fossil'='grey',
             'Ground'='grey','Atmosphere'='lightsteelblue2')
p$co2eneflo <- df$all %>% filter(Region=='World',Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Value=Value/1000) %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    select(colnames(df$var),Value,scen_lab) %>% 
    pivot_longer(cols=!c(scen_lab,Variable,Value),names_to='x',values_to='Carrier') %>% 
    mutate(Alpha=ifelse(x=='Sequestration'&Carrier%in%c('Neutral','Emission'),0.5,1)) %>% 
    mutate(Label=ifelse(x=='Sequestration'&Carrier%in%c('Neutral','Emission'),' ',Carrier)) %>% 
    mutate(Position=ifelse(x=='Destination',.8,ifelse(x=='Source',.2,.5))) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           Carrier=factor(Carrier,levels=rev(lst$Seq))) %>% 
    ggplot(aes(x=x,y=Value,alluvium=Variable,stratum=Carrier,label=Carrier))+
    geom_flow(aes(fill=Carrier),alpha=.5,show.legend=F)+
    geom_stratum(aes(fill=Carrier,alpha=Alpha),color='transparent',show.legend=F)+
    geom_text(aes(label=Label,hjust=Position),stat='stratum',size=3)+
    labs(title=NULL,x=NULL,y=expression(paste('Carbon flow (Gt-',CO[2],' ',yr^{-1},')')))+
    scale_x_discrete(limits=colnames(df$var)[-1],expand=c(.05,.05))+
    scale_fill_manual(values=lst$col,name=NULL)+
    scale_alpha_continuous(limits=c(0,1),range=c(0,1))+
    facet_grid(.~scen_lab)+
    mytheme$set1+
    theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    guides(fill=guide_legend(title=NULL))

p$l_tmp <- plot_grid(p$l_CCS_AR6,ggplotGrob(p$l_rangeleg),ncol=1,rel_heights=c(1,.7))
p$tmp1 <- plot_grid(p$DAC,p$CDR,p$CCS,p$l_tmp,nrow=1,labels=c('a','b','c',''),rel_widths=c(1,1,1,.6))
p$tmp <- plot_grid(p$tmp1,p$co2eneflo,ncol=1,labels=c('','d'),rel_heights=c(1,1.5))
ggsave(filename='output/fig3.png',plot=p$tmp,width=180,height=170,units='mm',dpi=300)


# Fig.4 -------------------------------------------------------------------

lst$shp <- as.integer(df$scen_sens_shape$Shape); names(lst$shp) <- as.character(df$scen_sens_shape$scen_sens_var)
p$polcosdisc <- df$all %>% filter(Region=='World',Year==2050) %>% 
    filter(Variable=='Pol_Cos_per_GDP_Disc') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>% 
    filter(Scenario%in%lst$scen_500all) %>% 
    ggplot()+
    geom_bar(data=. %>% filter(Scenario%in%lst$scen_rep),
             aes(x=scen_sens_base,y=Value,fill=scen_lab),stat='identity',show.legend=F)+
    geom_point(aes(x=scen_sens_base,y=Value,shape=scen_sens_var),fill='white',stat='identity',show.legend=T)+
    scale_y_continuous(limits=c(0,NA),labels=scales::percent)+
    labs(title=NULL,x=NULL,y='Energy system cost\n(% of GDP)')+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank())+
    scale_fill_manual(values=lst$scen_col)+
    scale_shape_manual(values=lst$shp,name=NULL)+
    guides(fill='none')

lst$Prc_Car_AR6_max <- df$load_AR6_global %>% 
    filter(Variable=='Prc_Car',Year==2050,Category%in%c('C1','C2','C3')) %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max()
lst$Prc_Car_max <- df$all %>% 
    filter(Scenario%in%lst$scen_rep,Region=='World',Year==2050) %>% 
    filter(Variable=='Prc_Car') %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max(lst$Prc_Car_AR6_max)
p$tmp1 <- df$all %>% filter(Region=='World',Year==2050) %>% 
    filter(Variable=='Prc_Car') %>%
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>%
    ggplot()+
    geom_bar(data=. %>% filter(scen_sens_var=='Default'),
             aes(x=scen_sens_base,y=Value,fill=scen_sens_base),stat='identity',position='stack',show.legend=T)+
    geom_point(aes(x=scen_sens_base,y=Value,shape=scen_sens_var),color='black',fill='white',show.legend=T)+
    scale_y_continuous(limits=c(0,lst$Prc_Car_max))+
    coord_cartesian(ylim=c(0,2000))+
    scale_fill_manual(values=lst$scen_col_all)+
    scale_shape_manual(values=lst$shp,name=NULL)+
    labs(title=NULL,x=NULL,y=expression(paste('Carbon prices (US$ t-',{CO[2]}^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    guides(fill=guide_legend(title=NULL,override.aes=list(color='transparent')),
           shape=guide_legend(title="This study's\nscenario",override.aes=list(fill='transparent')))
df$tmp <- df$load_AR6_global %>% 
    filter(Variable=='Prc_Car',Year=='2050',Category%in%c('C1','C2','C3'),IMP_marker%in%lst$IMP_main) %>% 
    mutate(IMP_marker=factor(IMP_marker,levels=lst$IMP_main))
p$tmp2 <- df$load_AR6_global %>% 
    filter(Variable=='Prc_Car',Year==2050,Category%in%c('C1','C2','C3')) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n)),y=2000,size=2,angle=90,hjust=1.3,vjust=.5)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5,show.legend=T)+
    scale_y_continuous(limits=c(0,lst$Prc_Car_max))+
    coord_cartesian(ylim=c(0,2000))+
    labs(x=NULL,y=NULL)+
    scale_shape_manual(values=lst$IMP_main_shp)+
    mytheme$set1+theme(axis.text.x=element_text(angle=45,hjust=1),
                       axis.text.y.left=element_blank(),axis.line.y.left=element_blank(),axis.ticks.y.left=element_blank())
p$l_carpri <- get_legend(p$tmp1)

p$tmp3 <- df$all %>% filter(Region=='World',Year==2050) %>% 
    filter(Variable=='Prc_Car') %>%
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    filter(Scenario%in%lst$scen_500all) %>%
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>% 
    ggplot()+
    geom_bar(data=. %>% filter(scen_sens_var=='Default'),
             aes(x=scen_sens_base,y=Value,fill=scen_sens_base),stat='identity',position='stack',show.legend=T)+
    geom_point(aes(x=scen_sens_base,y=Value,shape=scen_sens_var),color='black',fill='white',show.legend=T)+
    scale_y_continuous(limits=c(0,lst$Prc_Car_max))+
    coord_cartesian(ylim=c(0,2000))+
    scale_fill_manual(values=lst$scen_col_all)+
    scale_shape_manual(values=lst$shp,name=NULL)+
    labs(title=NULL,x=NULL,y=expression(paste('Carbon prices (US$ t-',{CO[2]}^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    guides(fill=guide_legend(title=NULL,override.aes=list(color='transparent')),
           shape=guide_legend(title="This study's\nscenario",override.aes=list(fill='transparent')))
p$carpol_all <- plot_grid(p$tmp3+theme(legend.position='none',plot.margin=unit(c(5.5,0,5.5,13),unit='pt')),
                          p$tmp2+theme(legend.position='none',plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt')),
                          nrow=1,axis='tb',align='h',rel_widths=c(1,.4))

df$var <- tribble(~Variable,~Legend,~Color,
                  'Prc_Sec_Ene_Ele','Electricity','lightsteelblue',
                  'Prc_Sec_Ene_Hyd','Hydrogen','thistle2',
                  'Prc_Sec_Ene_Liq_Hyd_syn','Synfuel','orchid')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
lst$shp <- as.integer(df$scen_sens_shape$Shape); names(lst$shp) <- as.character(df$scen_sens_shape$scen_sens_var)
lst$eneprc_max <- df$all %>% filter(Region=='World') %>% 
    filter(Variable%in%df$var$Variable,Value>0,Year>=2030) %>%
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    filter(scen_sens_base=='1.5C-CCU') %>% 
    mutate(Value=ceiling(Value)) %>% 
    .$Value %>% max()
p$eneprc <- df$all %>% filter(Region=='World') %>% 
    filter(Variable%in%df$var$Variable,Value>0,Year>=2030) %>%
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>%
    filter(scen_sens_base=='1.5C-CCU') %>% 
    ggplot()+
    geom_line(aes(x=Year,y=Value,color=Variable,linetype=scen_sens_var),show.legend=T)+
    geom_point(aes(x=Year,y=Value,color=Variable,shape=scen_sens_var),fill='transparent',show.legend=T)+
    ylim(0,lst$eneprc_max)+
    labs(x=NULL,y=expression(paste('Production costs (US$ ',GJ^{-1},')')))+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_color_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    scale_shape_manual(values=lst$shp,name=NULL)+
    scale_linetype_manual(values=lst$lin_scen,name=NULL)+
    guides(shape='none',linetype='none')

df$var <- tribble(~Variable,~Carrier,~Legend,~Color,
                  'Prc_Sec_Ene_Liq_Hyd_syn_Cap_Cos','Synfuel\nliquids','Capital','lightskyblue3',
                  'Prc_Sec_Ene_Liq_Hyd_syn_Hyd_Cos','Synfuel\nliquids','Hydrogen','thistle2',
                  'Prc_Sec_Ene_Liq_Hyd_syn_CO2_Cos','Synfuel\nliquids','CO2','moccasin',
                  'Prc_Sec_Ene_Liq_Hyd_syn_Oth_Cos','Synfuel\nliquids','Other','grey')
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Legend)
p$synf_cost <- df$all %>% filter(Region=='World',Year==2050) %>% 
    filter(Variable%in%df$var$Variable) %>%
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all),
           Legend=factor(Legend,levels=rev(df$var$Legend)),Carrier=factor(Carrier,levels=unique(df$var$Carrier))) %>%
    filter(Scenario%in%lst$scen_500all,scen_sens_base=='1.5C-CCU') %>% 
    ggplot()+
    geom_bar(aes(x=scen_sens_var,y=Value,fill=Legend),stat='identity',position='stack',show.legend=T)+
    ylim(0,lst$eneprc_max)+
    # facet_grid(.~Carrier)+
    labs(x=NULL,y=expression(paste('Energy production costs (US$ ',GJ^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=rev(lst$col),name=NULL)

df$var <- tribble(~Variable,~Legend,~Color,
                  'Inv_Add_Ene_Dem','Energy\ndemand','darkolivegreen2',
                  'Inv_Add_Ene_Sup_Ele','Electricity','lightsteelblue',
                  'Inv_Add_Ene_Sup_Hyd','Hydrogen','thistle2',
                  'Inv_Add_Ene_Sup_Oth','Other energy\nsupply','moccasin',
                  'Inv_Add_CCS','CCS','darkgoldenrod2')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$inv <- df$all %>% filter(Region=='World',Year>2020) %>% 
    filter(Variable%in%df$var$Variable) %>% 
    mutate(Year=ceiling(Year/10)*10) %>% 
    group_by(Model,Scenario,Region,Variable,Year) %>% summarise(Value=sum(Value)/n(),.groups='drop') %>% 
    mutate(Year=str_c(Year-9,'-',Year-2000)) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens)) %>%
    ggplot()+
    geom_bar(aes(x=Year,y=Value,fill=Variable),stat='identity',position='stack',show.legend=T)+
    facet_grid(.~scen_lab)+
    labs(x=NULL,y=expression(paste('Additional investment (billion US$ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)

p$l_tmp <- plot_grid(p$l_IMP,ggplotGrob(p$l_rangeleg),ncol=1,rel_heights=c(1,.6))
p$tmp1 <- plot_grid(p$polcosdisc+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1)),
                    p$carpol_all+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1)),
                    p$l_tmp,nrow=1,rel_widths=c(1,.85,.5),labels=c('a','b'))
p$l_tmp1 <- get_legend(p$eneprc+theme(legend.position='right'))
p$l_tmp2 <- get_legend(p$synf_cost+theme(legend.position='right'))
p$l_tmp <- plot_grid(p$l_tmp1,p$l_tmp2,ncol=1)
p$tmp2 <- plot_grid(p$eneprc+theme(legend.position='none',plot.margin=unit(c(5.5,0,5.5,13),unit='pt')),
                    p$synf_cost+theme(legend.position='none',axis.line.y=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank(),plot.background=element_blank(),plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt')),
                    p$l_tmp,nrow=1,rel_widths=c(1,.6,.6),axis='tb',align='h')
p$tmp3 <- plot_grid(p$tmp2,p$inv+theme(legend.position='right',plot.margin=unit(c(5.5,5.5,5.5,13),unit='pt')),nrow=1,rel_widths=c(1,1.3),labels=c('c','d'))
p$tmp <- plot_grid(p$tmp1,p$tmp3,ncol=1,rel_heights=c(1,1))
ggsave(filename='output/fig4.png',plot=p$tmp,width=180,height=130,units='mm',dpi=300)


# Supplementary Fig.1 ------------------------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Liq_Oil_and_Nat_Gas','Liquids-fossil','sandybrown',
                  'Fin_Ene_Liq_Hyd_syn','Liquids-synfuel','orchid',
                  'Fin_Ene_Gas_Fos','Gases-fossil','moccasin',
                  'Fin_Ene_Gas_Hyd_syn','Gases-synfuel','orchid1',
                  'Fin_Ene_Liq_and_Sol_Bio','Biomass','darkolivegreen2',
                  'Fin_Ene_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Heat','Heat','salmon',
                  'Fin_Ene_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Oth_inc_Solarand_Geo','Other','grey90')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$tmp <- df$all %>% filter(Region=='World') %>% 
    filter(Variable%in%df$var$Variable) %>%
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_wrap=factor(scen_wrap,levels=df$scen_lab$scen_wrap),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>%
    ggplot()+
    geom_area(aes(x=Year,y=Value,fill=Variable),position='stack',show.legend=T)+
    facet_wrap(~scen_wrap,nrow=2)+
    labs(title=NULL,x=NULL,y=expression(paste('Final energy demand (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)
ggsave(filename='output/figS1.png',plot=p$tmp,width=180,height=120,units='mm',dpi=300)


# Supplementary Fig.2 -----------------------------------------------------

df$var <- tribble(~Variable,~Device,
                  'Tec_sto_Sha_Tra_Fre_Roa_BEV','Freight',
                  'Tec_sto_Sha_Tra_Fre_Roa_FCV','Freight',
                  'Tec_sto_Sha_Tra_Pss_Roa_PHV','Passenger',
                  'Tec_sto_Sha_Tra_Pss_Roa_BEV','Passenger',
                  'Tec_sto_Sha_Tra_Pss_Roa_FCV','Passenger',
                  'Tec_sto_Sha_Ind_HeatingBoi_Ele','Boiler',
                  'Tec_sto_Sha_Ind_HeatingBoi_Hyd','Boiler',
                  'Tec_sto_Sha_Ind_HeatingFur_Ele','Furnace',
                  'Tec_sto_Sha_Ind_HeatingFur_Hyd','Furnace',
                  'Tec_sto_Sha_Com_HeatingSpa_EHP','Commercial',
                  'Tec_sto_Sha_Com_HeatingSpa_Oth','Commercial',
                  'Tec_sto_Sha_Res_HeatingSpa_EHP','Residential',
                  'Tec_sto_Sha_Res_HeatingSpa_Oth','Residential')
df$lab_tech <- tribble(~Device,~Tech_Label,
                       'Passenger','Passenger\nroad transport',
                       'Freight','Freight\nroad transport',
                       'Boiler','Industry\nboiler',
                       'Furnace','Industry\nfurnace',
                       'Residential','Residential\nspace heating',
                       'Commercial','Commercial\nspace heating')
p$tmp <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    inner_join(df$var,by='Variable') %>% 
    group_by(Model,Scenario,Device,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    inner_join(df$lab_tech,by='Device') %>% 
    mutate(Tech_Label=factor(Tech_Label,levels=df$lab_tech$Tech_Label)) %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>%
    filter(Scenario%in%lst$scen_500all) %>% 
    ggplot()+
    geom_line(aes(x=Year,y=Value,color=scen_sens_base,linetype=scen_sens_var),show.legend=T)+
    geom_point(aes(x=Year,y=Value,color=scen_sens_base,shape=scen_sens_var),show.legend=T)+
    scale_x_continuous(limits=c(2020,2050),breaks=seq(2020,2050,by=10))+
    scale_y_continuous(limits=c(0,1), labels=scales::percent)+
    facet_wrap(~Tech_Label,nrow=1)+
    scale_color_manual(values=lst$scen_col)+
    scale_shape_manual(values=lst$shp,name=NULL)+
    scale_linetype_manual(values=lst$lin_scen,name=NULL)+
    mytheme$set1+theme(legend.position='bottom',legend.box='vertical',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    labs(x=NULL,y='Technology diffusion rate')+
    guides(color=guide_legend(title=NULL))
ggsave(filename='output/figS2.png',plot=p$tmp,width=180,height=95,units='mm',dpi=300)


# Supplementary Fig.3 -----------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,
                  'Sec_Ene_Ele_Tra_Los','Transmission loss','moccasin',
                  'Sec_Ene_Ele_Sto_Los','Storage loss','darkgoldenrod2',
                  'Sec_Ene_Ele_Cur','Curtailment','tan3')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$tmp <- df$all %>% filter(Region=='World',Year%in%c(2030,2040,2050)) %>% 
    filter(Variable%in%df$var$Variable) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_wrap=factor(scen_wrap,levels=df$scen_lab$scen_wrap)) %>%
    ggplot()+
    geom_bar(aes(x=Year,y=Value,fill=Variable),stat='identity',position='stack',show.legend=T)+
    facet_wrap(~scen_wrap,nrow=2)+
    labs(x=NULL,y=expression(paste('Electricity losses (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)
ggsave(filename='output/figS3.png',plot=p$tmp,width=180,height=100,units='mm',dpi=300)


# Supplementary Fig.4 -----------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,
                  'Sec_Ene_Hyd_Ele','Electricity','lightsteelblue',
                  'Sec_Ene_Hyd_Fos_wo_CCS','Fossil w/o CCS','sandybrown',
                  'Sec_Ene_Hyd_Fos_w_CCS','Fossil w/ CCS','tan3',
                  'Sec_Ene_Hyd_Bio_wo_CCS','Biomass w/o CCS','darkolivegreen2',
                  'Sec_Ene_Hyd_Bio_w_CCS','Biomass w/ CCS','darkolivegreen4')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$tmp <- df$all %>% filter(Region=='World',Year%in%c(2030,2040,2050)) %>% 
    filter(Variable%in%df$var$Variable) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_wrap=factor(scen_wrap,levels=df$scen_lab$scen_wrap),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>%
    ggplot()+
    geom_bar(aes(x=Year,y=Value,fill=Variable),position='stack',stat='identity',show.legend=T)+
    facet_wrap(~scen_wrap,nrow=2)+
    labs(title=NULL,x=NULL,y=expression(paste('Hydrogen generation (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=lst$col,labels=lst$leg,name=NULL)
ggsave(filename='output/figS4.png',plot=p$tmp,width=180,height=100,units='mm',dpi=300)


# Supplementary Fig.5 -----------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,
                  'Prm_Ene_Oil_wo_CCS','Oil w/o CCS','sandybrown',
                  'Prm_Ene_Oil_w_CCS','Oil w/ CCS','tan3',
                  'Prm_Ene_Coa_wo_CCS','Coal w/o CCS','grey50',
                  'Prm_Ene_Coa_w_CCS','Coal w/ CCS','grey30',
                  'Prm_Ene_Gas_wo_CCS','Gas w/o CCS','lightgoldenrod',
                  'Prm_Ene_Gas_w_CCS','Gas w/ CCS','lightgoldenrod3',
                  'Prm_Ene_Nuc','Nuclear','moccasin',
                  'Prm_Ene_Bio_wo_CCS','Biomass w/o CCS','darkolivegreen2',
                  'Prm_Ene_Bio_w_CCS','Biomass w/ CCS','darkolivegreen4',
                  'Prm_Ene_Hyd','Hydro','lightsteelblue',
                  'Prm_Ene_Geo','Geothermal','peru',
                  'Prm_Ene_Solar','Solar','lightsalmon',
                  'Prm_Ene_Win','Wind','lightskyblue3',
                  'Prm_Ene_Oce','Ocean','paleturquoise3',
                  'Prm_Ene_Oth','Other','grey')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$tmp1 <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    filter(Variable%in%df$var$Variable) %>%
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_wrap=factor(scen_wrap,levels=df$scen_lab$scen_wrap),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>%
    ggplot()+
    geom_area(aes(x=Year,y=Value,fill=Variable),position='stack',show.legend=T)+
    facet_wrap(~scen_wrap,nrow=2)+
    labs(title=NULL,x=NULL,y=expression(paste('Primary energy (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)

df$var <- tribble(~Variable,~Legend,~Color,
                  'Sec_Ene_Ele_Oil_wo_CCS','Oil w/o CCS','sandybrown',
                  'Sec_Ene_Ele_Oil_w_CCS','Oil w/ CCS','tan3',
                  'Sec_Ene_Ele_Coa_wo_CCS','Coal w/o CCS','grey50',
                  'Sec_Ene_Ele_Coa_w_CCS','Coal w/ CCS','grey30',
                  'Sec_Ene_Ele_Gas_wo_CCS','Gas w/o CCS','lightgoldenrod',
                  'Sec_Ene_Ele_Gas_w_CCS','Gas w/ CCS','lightgoldenrod3',
                  'Sec_Ene_Ele_Nuc','Nuclear','moccasin',
                  'Sec_Ene_Ele_Hyd','Hydro','lightsteelblue',
                  'Sec_Ene_Ele_Bio_wo_CCS','Biomass w/o CCS','darkolivegreen2',
                  'Sec_Ene_Ele_Bio_w_CCS','Biomass w/ CCS','darkolivegreen4',
                  'Sec_Ene_Ele_Geo','Geothermal','peru',
                  'Sec_Ene_Ele_SolarCSP','CSP','darksalmon',
                  'Sec_Ene_Ele_SolarPV','Solar PV','lightsalmon',
                  'Sec_Ene_Ele_Win','Wind','lightskyblue3',
                  'Sec_Ene_Ele_Oce','Ocean','paleturquoise3',
                  'Sec_Ene_Ele_Hyd_GT','Hydrogen','orchid',
                  'Sec_Ene_Ele_Oth','Other','grey')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$tmp2 <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    filter(Variable%in%df$var$Variable) %>%
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_wrap=factor(scen_wrap,levels=df$scen_lab$scen_wrap),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>%
    ggplot()+
    geom_area(aes(x=Year,y=Value,fill=Variable),position='stack',show.legend=T)+
    facet_wrap(~scen_wrap,nrow=2)+
    labs(x=NULL,y=expression(paste('Power generation (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)
p$tmp <- plot_grid(p$tmp2,p$tmp1,ncol=1,rel_heights=c(1,1),labels=c('a','b'))
ggsave(filename='output/figS5.png',plot=p$tmp,width=180,height=240,units='mm',dpi=300)

# Supplementary Fig.6 -----------------------------------------------------

lst$Prm_Ene_Fos_max <- df$all %>% 
    filter(Scenario%in%lst$scen_rep,Region=='World',Year>=2020) %>% 
    filter(Variable%in%c('Prm_Ene_Coa','Prm_Ene_Oil','Prm_Ene_Gas')) %>% 
    group_by(Model,Scenario,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max()
lst$Prm_Ene_Fos_AR6_max <- df$load_AR6_global %>% 
    filter(Variable%in%c('Prm_Ene_Coa','Prm_Ene_Oil','Prm_Ene_Gas'),Year==2050,Category%in%c('C1','C2','C3')) %>% 
    group_by(Model,Scenario,Region,Year,Category) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max(lst$Prm_Ene_Fos_max)
p$tmp1 <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    filter(Variable%in%c('Prm_Ene_Coa','Prm_Ene_Oil','Prm_Ene_Gas')) %>%
    group_by(Model,Scenario,Region,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab)) %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=scen_lab),show.legend=F)+
    geom_point(aes(x=Year,y=Value,color=scen_lab),fill='white',shape=21,show.legend=F)+
    ylim(0,lst$Prm_Ene_Fos_AR6_max)+
    scale_color_manual(values=lst$scen_col)+
    labs(title='Fossil primary supply',x=NULL,y=expression(paste('Primary energy (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position=c(.35,.8),strip.background=element_blank(),plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt'),axis.text.x=element_text(angle=45,hjust=1))
df$tmp <- df$load_AR6_global %>% 
    filter(Variable%in%c('Prm_Ene_Coa','Prm_Ene_Oil','Prm_Ene_Gas'),Year=='2050',Category%in%c('C1','C2','C3'),IMP_marker%in%lst$IMP_main) %>% 
    group_by(Model,Scenario,Region,Year,Category,IMP_marker) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    mutate(IMP_marker=factor(IMP_marker,levels=lst$IMP_main))
p$tmp2 <- df$load_AR6_global %>% 
    filter(Variable%in%c('Prm_Ene_Coa','Prm_Ene_Oil','Prm_Ene_Gas'),Year==2050,Category%in%c('C1','C2','C3')) %>% 
    group_by(Model,Scenario,Region,Year,Category) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    mutate(Variable='Prm_Ene_Fos') %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p0),size=2,angle=90,hjust=1.3,vjust=.5)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5,show.legend=F)+
    scale_y_continuous(limits=c(0,lst$Prm_Ene_Fos_AR6_max))+
    labs(x=NULL,y=NULL)+
    scale_shape_manual(values=lst$IMP_main_shp)+
    mytheme$set1+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5),
                       axis.text.y.left=element_blank(),axis.line.y.left=element_blank(),axis.ticks.y.left=element_blank(),
                       plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt'))
p$prm_ene_fos <- plot_grid(p$tmp1,p$tmp2,nrow=1,axis='tb',align='h',rel_widths=c(1,.35))

lst$Prm_Ene_Bio_max <- df$all %>% 
    filter(Scenario%in%lst$scen_rep,Region=='World',Year>=2020) %>% 
    filter(Variable=='Prm_Ene_Bio') %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max()
lst$Prm_Ene_Bio_AR6_max <- df$load_AR6_global %>% 
    filter(Variable=='Prm_Ene_Bio',Year==2050,Category%in%c('C1','C2','C3')) %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max(lst$Prm_Ene_Bio_max)
p$tmp1 <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    filter(Variable=='Prm_Ene_Bio') %>%
    inner_join(df$scen_lab,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab)) %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=scen_lab),show.legend=T)+
    geom_point(aes(x=Year,y=Value,color=scen_lab),fill='white',shape=21,show.legend=T)+
    ylim(0,lst$Prm_Ene_Bio_AR6_max)+
    scale_color_manual(values=lst$scen_col)+
    labs(title='Biomass primary supply',x=NULL,y=expression(paste('Primary energy (EJ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position=c(.4,.8),strip.background=element_blank(),plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt'),axis.text.x=element_text(angle=45,hjust=1))+
    guides(color=guide_legend(title=NULL))
df$tmp <- df$load_AR6_global %>% 
    filter(Variable=='Prm_Ene_Bio',Year=='2050',Category%in%c('C1','C2','C3'),IMP_marker%in%lst$IMP_main) %>% 
    mutate(IMP_marker=factor(IMP_marker,levels=lst$IMP_main))
p$tmp2 <- df$load_AR6_global %>% 
    filter(Variable=='Prm_Ene_Bio',Year==2050,Category%in%c('C1','C2','C3')) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p0),size=2,angle=90,hjust=1.3,vjust=.5)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5,show.legend=T)+
    scale_y_continuous(limits=c(0,lst$Prm_Ene_Bio_AR6_max))+
    labs(x=NULL,y=NULL)+
    scale_shape_manual(values=lst$IMP_main_shp)+
    mytheme$set1+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5),
                       axis.text.y.left=element_blank(),axis.line.y.left=element_blank(),axis.ticks.y.left=element_blank(),
                       plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt'))
p$l_IMP <- get_legend(p$tmp2+theme(legend.key.size=unit(5,'mm'))+guides(shape=guide_legend(title='AR6 IMPs')))
p$prm_ene_bio <- plot_grid(p$tmp1,p$tmp2+theme(legend.position='none'),nrow=1,axis='tb',align='h',rel_widths=c(1,.35))

p$l_tmp <- plot_grid(p$l_IMP,ggplotGrob(p$l_rangeleg),ncol=1,rel_heights=c(1,.6))
p$tmp <- plot_grid(p$prm_ene_fos,p$prm_ene_bio,p$l_tmp,nrow=1,rel_widths=c(1,1,.4),labels=c('a','b',''),axis='tb',align='h')
ggsave(filename='output/figS6.png',plot=p$tmp,width=180,height=75,units='mm',dpi=300)


# Supplementary Fig.7 -----------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,~Axis,
                  'Car_Seq_Geo_Sto','Underground\nstorage','darkgoldenrod2','Storage',
                  'Car_Uti_Ene','Utilization','orchid1','Storage',
                  'Car_Cap_Fos_Ene_Sup','Energy\nsupply','moccasin','Capture',
                  'Car_Cap_Fos_Ene_Dem_Ind','Industry','salmon','Capture',
                  'Car_Cap_Ind_Pro','Industrial\nprocess','grey','Capture',
                  'Car_Cap_Bio_Ene_Sup','Bioenergy','darkolivegreen2','Capture',
                  'Car_Cap_Dir_Air_Cap','DAC','lightsteelblue','Capture')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$tmp <- df$all %>% filter(Region=='World',Year%in%seq(2030,2050,10)) %>% 
    filter(Variable%in%df$var$Variable) %>%
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(Value=Value/1000) %>% 
    mutate(Value=if_else(Axis=='Storage',-Value,Value)) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_wrap=factor(scen_wrap,levels=df$scen_lab$scen_wrap),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>%
    ggplot()+
    geom_hline(yintercept=0,color='black',size=.25)+
    geom_bar(aes(x=Year,y=Value,fill=Variable),position='stack',stat='identity',show.legend=T)+
    facet_wrap(~scen_wrap,nrow=2)+
    labs(title=NULL,x=NULL,y=expression(paste('Carbon capture and sequestration (Gt-',CO[2],' ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)
ggsave(filename='output/figS7.png',plot=p$tmp,width=180,height=100,units='mm',dpi=300)


# Supplementary Fig.8 -----------------------------------------------------

df$var <- tribble(~Variable,~Source,~'Capture & Use',~Sequestration,~Destination,
                  'Emi_CO2_Ene_Com_exc_CCUS','Fossil','Fossil','Emission','Atmosphere',
                  'Emi_CO2_Ene_Bio_exc_CCUS','Atmosphere','Biomass','Neutral','Atmosphere',
                  'CCUSUti_Ene_Bio','Atmosphere','Biomass','Utilization','Atmosphere',
                  'CCUSUti_Ene_Dir_Air_Cap','Atmosphere','DAC','Utilization','Atmosphere',
                  'CCUSUti_Ene_Fos','Fossil','Fossil','Utilization','Atmosphere',
                  'CCUSGeo_Sto_Bio','Atmosphere','Biomass','Storage','Ground',
                  'CCUSGeo_Sto_Dir_Air_Cap','Atmosphere','DAC','Storage','Ground',
                  'CCUSGeo_Sto_Fos','Fossil','Fossil','Storage','Ground')
lst$Seq <- c('Storage','Utilization','Neutral','Emission','DAC','Biomass','Fossil','Ground','Atmosphere')
lst$col <- c('Storage'='moccasin','Utilization'='orchid1','Neutral'='darkolivegreen2','Emission'='grey',
             'Biomass'='darkolivegreen2','DAC'='thistle2','Fossil'='grey',
             'Ground'='grey','Atmosphere'='lightsteelblue2')
p$tmp <- df$all %>% filter(Region=='World',Year%in%c(2020,2030,2050)) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Value=Value/1000) %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    filter(scen_lab=='1.5C-CCU') %>% 
    select(colnames(df$var),Value,Year) %>% 
    pivot_longer(cols=!c(Year,Variable,Value),names_to='x',values_to='Carrier') %>% 
    mutate(Alpha=ifelse(x=='Sequestration'&Carrier%in%c('Neutral','Emission'),0.5,1)) %>% 
    mutate(Label=ifelse(x=='Sequestration'&Carrier%in%c('Neutral','Emission'),' ',Carrier)) %>% 
    mutate(Position=ifelse(x=='Destination',.8,ifelse(x=='Source',.2,.5))) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),
           Carrier=factor(Carrier,levels=rev(lst$Seq))) %>% 
    ggplot(aes(x=x,y=Value,alluvium=Variable,stratum=Carrier,label=Carrier))+
    geom_flow(aes(fill=Carrier),alpha=.5,show.legend=F)+
    geom_stratum(aes(fill=Carrier,alpha=Alpha),color='transparent',show.legend=F)+
    geom_text(aes(label=Label,hjust=Position),stat='stratum',size=2.5)+
    labs(title=NULL,x=NULL,y=expression(paste('Carbon flow (Gt-',CO[2],' ',yr^{-1},')')))+
    scale_x_discrete(limits=colnames(df$var)[-1],expand=c(.05,.05))+
    scale_fill_manual(values=lst$col,name=NULL)+
    scale_alpha_continuous(limits=c(0,1),range=c(0,1))+
    facet_grid(.~Year)+
    mytheme$set1+
    theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    guides(fill=guide_legend(title=NULL))
ggsave(filename='output/figS8.png',plot=p$tmp,width=180,height=100,units='mm',dpi=300)


# Supplementary Fig.9 -----------------------------------------------------

lst$Prc_Car_AR6_max <- df$load_AR6_global %>% 
    filter(Variable=='Prc_Car',Year==2050,Category%in%c('C1','C2','C3')) %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max()
lst$Prc_Car_max <- df$all %>% 
    filter(Scenario%in%lst$scen_rep,Region=='World',Year==2050) %>% 
    filter(Variable=='Prc_Car') %>% 
    mutate(Value=ceiling(Value)) %>% .$Value %>% max(lst$Prc_Car_AR6_max)
p$tmp1 <- df$all %>% filter(Region=='World',Year==2050) %>% 
    filter(Variable=='Prc_Car') %>%
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>%
    ggplot()+
    geom_bar(data=. %>% filter(scen_sens_var=='Default'),
             aes(x=scen_sens_base,y=Value,fill=scen_sens_base),stat='identity',position='stack',show.legend=T)+
    geom_point(aes(x=scen_sens_base,y=Value,shape=scen_sens_var),color='black',fill='white',show.legend=T)+
    scale_y_continuous(limits=c(0,lst$Prc_Car_max))+
    coord_cartesian(ylim=c(0,2000))+
    scale_fill_manual(values=lst$scen_col_all)+
    scale_shape_manual(values=lst$shp,name=NULL)+
    labs(title=NULL,x=NULL,y=expression(paste('Carbon prices (US$ t-',{CO[2]}^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    guides(fill=guide_legend(title=NULL,override.aes=list(color='transparent')),
           shape=guide_legend(title="This study's\nscenario",override.aes=list(fill='transparent')))
df$tmp <- df$load_AR6_global %>% 
    filter(Variable=='Prc_Car',Year=='2050',Category%in%c('C1','C2','C3'),IMP_marker%in%lst$IMP_main) %>% 
    mutate(IMP_marker=factor(IMP_marker,levels=lst$IMP_main))
p$tmp2 <- df$load_AR6_global %>% 
    filter(Variable=='Prc_Car',Year==2050,Category%in%c('C1','C2','C3')) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n)),y=2000,size=2,angle=90,hjust=1.3,vjust=.5)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5,show.legend=T)+
    scale_y_continuous(limits=c(0,lst$Prc_Car_max))+
    coord_cartesian(ylim=c(0,2000))+
    labs(x=NULL,y=NULL)+
    scale_shape_manual(values=lst$IMP_main_shp)+
    mytheme$set1+theme(axis.text.x=element_text(angle=45,hjust=1),
                       axis.text.y.left=element_blank(),axis.line.y.left=element_blank(),axis.ticks.y.left=element_blank())
p$l_carpri <- get_legend(p$tmp1)

p$tmp3 <- df$all %>% filter(Region=='World',Year==2050) %>% 
    filter(Variable=='Pol_Cos_per_GDP') %>%
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>%
    ggplot()+
    geom_bar(data=. %>% filter(scen_sens_var=='Default'),
             aes(x=scen_sens_base,y=Value,fill=scen_sens_base),stat='identity',position='stack',show.legend=T)+
    geom_point(aes(x=scen_sens_base,y=Value,shape=scen_sens_var),color='black',fill='white',show.legend=T)+
    scale_y_continuous(limits=c(0,NA),labels=scales::percent_format(accuracy=1))+
    scale_fill_manual(values=lst$scen_col_all)+
    scale_shape_manual(values=lst$shp,name=NULL)+
    scale_linetype_manual(values=lst$lin_scen,name=NULL)+
    labs(title=NULL,x=NULL,y='Energy system cost (% of GDP)')+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    guides(fill=guide_legend(title=NULL,override.aes=list(color='transparent')),
           shape=guide_legend(title=NULL,override.aes=list(fill='transparent')))

p$polcos_all <- plot_grid(p$tmp1+theme(legend.position='none',plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt')),
                          p$tmp2+theme(legend.position='none',plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt')),
                          p$tmp3+theme(legend.position='none'),
                          nrow=1,axis='tb',align='h',rel_widths=c(1,.4,1),labels=c('a','','b'))

df$var <- tribble(~Variable,~Legend,~Color,
                  'Inv_Add_Ene_Dem','Energy demand','darkolivegreen2',
                  'Inv_Add_Ene_Sup_Ele','Electricity','lightsteelblue',
                  'Inv_Add_Ene_Sup_Hyd','Hydrogen','thistle2',
                  'Inv_Add_Ene_Sup_Oth','Other energy supply','moccasin',
                  'Inv_Add_CCS','CCS','darkgoldenrod2')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$inv_R5 <- df$all %>% filter(Region%in%df$R5map$Region,Year>2020) %>% 
    filter(Variable%in%df$var$Variable) %>% 
    mutate(Year=ceiling(Year/10)*10) %>% 
    group_by(Model,Scenario,Region,Variable,Year) %>% summarise(Value=sum(Value)/n(),.groups='drop') %>% 
    filter(Year==2050) %>% 
    mutate(Year=str_c(Year-9,'-',Year-2000)) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    inner_join(df$R5map,by='Region') %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens),
           R5lab=factor(R5lab,levels=df$R5map$R5lab)) %>%
    ggplot()+
    geom_bar(aes(x=scen_lab,y=Value,fill=Variable),stat='identity',position='stack',show.legend=T)+
    facet_wrap(~R5lab,nrow=1)+
    labs(x=NULL,y=expression(paste('Additional investment (billion US$ ',yr^{-1},')')))+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),plot.margin=unit(c(5.5,0,5.5,13),unit='pt'))+
    scale_fill_manual(values=lst$col,labels=lst$leg,name=NULL)

p$l_tmp <- plot_grid(p$l_IMP,ggplotGrob(p$l_rangeleg),ncol=1,rel_heights=c(1,.6))
p$tmp1 <- plot_grid(p$polcos_all,p$l_carpri,p$l_tmp,nrow=1,rel_widths=c(1,.25,.25))
p$tmp <- plot_grid(p$tmp1,p$inv_R5,ncol=1,rel_widths=c(1,1),labels=c('','c'))
ggsave(filename='output/figS9.png',plot=p$tmp,width=180,height=170,units='mm',dpi=300)


# Supplementary Fig.10 ----------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Liq_Oil_and_Nat_Gas','Liquids\n-fossil','sandybrown',
                  'Fin_Ene_Liq_Hyd_syn','Liquids\n-synfuel','orchid',
                  'Fin_Ene_Gas_Fos','Gases\n-fossil','moccasin',
                  'Fin_Ene_Gas_Hyd_syn','Gases\n-synfuel','orchid1',
                  'Fin_Ene_Liq_and_Sol_Bio','Biomass','darkolivegreen2',
                  'Fin_Ene_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Heat','Heat','salmon',
                  'Fin_Ene_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Oth_inc_Solarand_Geo','Other','grey90')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$tmp <- df$all %>% filter(Region%in%df$R5map$Region,Year>=2020) %>% 
    filter(Variable%in%df$var$Variable) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$R5map,by='Region') %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           R5lab=factor(R5lab,levels=df$R5map$R5lab)) %>% 
    ggplot()+
    geom_area(aes(x=Year,y=Value,fill=Variable),stat='identity',position='stack',show.legend=T)+
    labs(x=NULL,y=expression(paste('Final energy demand (EJ ',yr^{-1},')')))+
    facet_grid(R5lab~scen_lab,scales='free_y')+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),legend.spacing=unit(10,'mm'),axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    guides(fill=guide_legend(byrow=T))
print(p$tmp)
ggsave(filename='output/figS10.png',plot=p$tmp,width=180,height=130,units='mm',dpi=300)


# Supplementary Fig.11 ----------------------------------------------------

df$var <- tribble(~Variable,~Device,
                  'Cap_Cos_Ele_SolarPV','Solar PV',
                  'Cap_Cos_Ele_Win_Ons','Wind onshore',
                  'Cap_Cos_Hyd_Ele','Electrolysis',
                  'Cap_Cos_Liq_Hyd_syn','Synfuel\nproduction',
                  'Cap_Cos_Dir_Air_Cap','DAC')
p$tmp <- df$all %>% filter(Region=='R5OECD90+EU',Year>=2020) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    inner_join(df$scen_sens_cat,by='Scenario') %>% 
    mutate(scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab),
           scen_sens_base=factor(scen_sens_base,levels=lst$scen_cat),scen_sens_var=factor(scen_sens_var,lst$scen_sens_all)) %>%
    mutate(Device=factor(Device,levels=df$var$Device)) %>% 
    filter(Scenario%in%lst$scen_500all) %>% 
    filter(scen_sens_base=='1.5C-CCU') %>% 
    mutate(Value=ifelse(Variable=='Cap_Cos_Dir_Air_Cap'&Year<2030,NA,Value)) %>% 
    ggplot()+
    geom_line(aes(x=Year,y=Value,linetype=scen_sens_var),show.legend=T)+
    geom_point(aes(x=Year,y=Value,shape=scen_sens_var),fill='white',show.legend=T)+
    scale_x_continuous(limits=c(2020,2050),breaks=seq(2020,2050,by=10))+
    scale_y_continuous(limits=c(0,NA))+
    facet_wrap(~Device,nrow=1,scales='free_y')+
    scale_shape_manual(values=lst$shp,name=NULL)+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    labs(x=NULL,y='Capital cost')+
    guides(linetype=guide_legend(title=NULL),
           shape=guide_legend(title=NULL))
ggsave(filename='output/figS11.png',plot=p$tmp,width=180,height=75,units='mm',dpi=300)


# Supplementary Fig.12 ----------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,
                  'Emi_CO2_Ene_Sup','Energy Supply','moccasin',
                  'Emi_CO2_Ene_Dem_Ind_and_AFO','Industry','salmon',
                  'Emi_CO2_Ene_Dem_Res_and_Com','Buildings','lightsteelblue',
                  'Emi_CO2_Ene_Dem_Tra','Transportation','darkolivegreen2',
                  'Emi_CO2_Oth','DACCS','darkgoldenrod2')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
lst$emi_co2ene_max <- df$all %>% 
    filter(Variable=='Emi_CO2_Ene_inc_Dir_Air_Cap',Region=='World',Scenario%in%lst$scen_rep) %>% 
    mutate(Value=ceiling(Value/1000)) %>% .$Value %>% max()
p$emi_co2ene <- df$all %>% filter(Region=='World',Variable%in%c(df$var$Variable,'Emi_CO2_Ene_inc_Dir_Air_Cap')) %>% 
    left_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    filter(Scenario%in%lst$scen_rep) %>% 
    mutate(Value=Value/1000) %>% 
    mutate(Variable=factor(Variable,levels=rev(c(df$var$Variable,'Emi_CO2_Ene_inc_Dir_Air_Cap'))),scen_lab=factor(scen_lab,levels=df$scen_lab$scen_lab)) %>% 
    group_by(Model,Scenario,Region,Variable) %>% arrange(Year) %>% 
    mutate(Value=if_else(Variable=='Emi_CO2_Oth'&Value==0&lead(Value)<0,-.001,Value)) %>% ungroup() %>%
    ggplot()+
    geom_area(data=. %>% filter(Variable!='Emi_CO2_Ene_inc_Dir_Air_Cap'),
              aes(x=Year,y=Value,fill=Variable),stat='identity',position='stack',show.legend=T)+
    geom_path(data=. %>% filter(Variable=='Emi_CO2_Ene_inc_Dir_Air_Cap'),aes(x=Year,y=Value),show.legend=F)+
    geom_point(data=. %>% filter(Variable=='Emi_CO2_Ene_inc_Dir_Air_Cap'),aes(x=Year,y=Value),shape=21,fill='white',show.legend=F)+
    labs(x=NULL,y=expression(paste(CO[2],' emissions (Gt-',CO[2],' yr'^{-1},')')))+
    facet_grid(.~scen_lab)+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_y_continuous(limits=c(-10,lst$emi_co2ene_max))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    guides(fill=guide_legend(title=NULL,override.aes=list(linetype=NULL,shape=NULL,color='transparent')))

p$emi_co2ene_all <- df$all %>% filter(Region=='World',Variable%in%c(df$var$Variable,'Emi_CO2_Ene_inc_Dir_Air_Cap')) %>% 
    left_join(df$var,by='Variable') %>% 
    inner_join(df$scen_lab,by='Scenario') %>% 
    mutate(Value=Value/1000) %>% 
    mutate(Variable=factor(Variable,levels=rev(c(df$var$Variable,'Emi_CO2_Ene_inc_Dir_Air_Cap'))),scen_wrap=factor(scen_wrap,levels=df$scen_lab$scen_wrap)) %>% 
    group_by(Model,Scenario,Region,Variable) %>% arrange(Year) %>% 
    mutate(Value=if_else(Variable=='Emi_CO2_Oth'&Value==0&lead(Value)<0,-.001,Value)) %>% ungroup() %>%
    ggplot()+
    geom_area(data=. %>% filter(Variable!='Emi_CO2_Ene_inc_Dir_Air_Cap'),
              aes(x=Year,y=Value,fill=Variable),stat='identity',position='stack',show.legend=T)+
    geom_path(data=. %>% filter(Variable=='Emi_CO2_Ene_inc_Dir_Air_Cap'),aes(x=Year,y=Value),show.legend=F)+
    geom_point(data=. %>% filter(Variable=='Emi_CO2_Ene_inc_Dir_Air_Cap'),
               aes(x=Year,y=Value),shape=21,fill='white',show.legend=F)+
    labs(x=NULL,y=expression(paste(CO[2],' emissions (Gt-',CO[2],' yr'^{-1},')')))+
    facet_wrap(~scen_wrap,nrow=2)+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_y_continuous(limits=c(-10,lst$emi_co2ene_max))+
    scale_fill_manual(values=lst$col,labels=lst$leg,name=NULL)+
    guides(fill=guide_legend(title=NULL,override.aes=list(linetype=NULL,shape=NULL,color='transparent')))

df$tmp <- df$load_AR6_global %>% 
    filter(Variable=='Emi_CO2_Ene',Year=='2050',Category%in%c('C1','C2','C3'),!(is.na(IMP_marker))) %>% 
    mutate(Value=Value/1000)
p$Emi_CO2ene_AR6 <- df$load_AR6_global %>% 
    filter(Variable=='Emi_CO2_Ene',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    mutate(Value=Value/1000) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p100),size=2,vjust=-1)+
    geom_point(data=df$tmp,aes(x=Category,y=Value,shape=IMP_marker),size=1.5)+
    ylim(-10,lst$emi_co2ene_max)+
    labs(x=NULL,y=NULL)+
    scale_shape_manual(values=lst$IMP_main_shp)+
    mytheme$set1+
    guides(shape=guide_legend(title='AR6 IMPs'))

p$l_co2 <- get_legend(p$emi_co2ene+theme(legend.position='right'))
p$tmp1 <- plot_grid(p$emi_co2ene+theme(legend.position='none',plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt')),
                    p$Emi_CO2ene_AR6+theme(axis.text.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank(),plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt')),
                    p$l_co2,nrow=1,rel_widths=c(1,.5,.3),axis='tb',align='h')
p$tmp <- plot_grid(p$tmp1,p$emi_co2ene_all+theme(legend.position='none'),ncol=1,labels=c('a','b'),rel_heights=c(1,1))
print(p$tmp)
ggsave(filename='output/figS12.png',plot=p$tmp,width=180,height=130,units='mm',dpi=300)

