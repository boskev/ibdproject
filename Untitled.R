
X <-

IBD2 %>% 
    mutate( RACEREP = ifelse( RACE_REPORTED == 'Y', 1, 0) ) %>% tbl_df 


    summary(glm( RACEREP ~ INTL_SITES, family= 'binomial', data= X ))

    
    chisq.test( x= IBD2$RACE_REPORTED, y= IBD2$INTL_SITES)

    CrossTable(x= IBD2$RACE_REPORTED, y= IBD2$INTL_SITES, prop.chisq=F)
    
    
    
    install.packages("plotly")

    library(plotly)
    
    
    IBD2 %>% 
      select( RACE_REPORTED, WHT:MIX) %>% 
      filter( RACE_REPORTED == 'Y' ) %>% 
      group_by( RACE_REPORTED) %>% 
      summarize ( TRIALS =  n() ,
                  WHT =     sum(WHT, na.rm=T), 
                  BLK =     sum(BLK, na.rm=T),
                  ASN =     sum(ASN, na.rm=T), 
                  AIA =     sum(AIA, na.rm=T), 
                  HPI =     sum(HPI, na.rm=T), 
                  MIX =     sum(MIX, na.rm=T)  ) %>% 
      
      pivot_longer( cols= c(WHT,BLK,ASN,AIA,HPI,MIX),
                    names_to = "RACE",
                    values_to = "COUNT" ) 
    
    
    
    IBD2 %>% 
      select( RACE_REPORTED, ANALYZED) %>% 
      group_by( RACE_REPORTED) %>% 
      summarize ( TRIALS =  n() ,
                  SUBJECTS =     sum(ANALYZED, na.rm=T) ) %>% 
      mutate( PCT = percent(substr(round(SUBJECTS / sum(SUBJECTS),3),1,5)) )  %>% 
      mutate( RACE_REPORTED =  ifelse( RACE_REPORTED=='Y', "YES", "NO") ) %>% 
      arrange( desc(factor(RACE_REPORTED)) ) %>% 
      mutate(ypos = cumsum(SUBJECTS)- 0.5*SUBJECTS  ) %>% 
      mutate( CUSTLABEL = paste(RACE_REPORTED,"\n",PCT) ) %>% 
      
      # Basic piechart
      ggplot( aes(x="", y= SUBJECTS, fill=RACE_REPORTED)) +
      geom_bar(stat="identity", width= 10, color="white", lwd=5) +
      coord_polar("y", start= 0.4) +
      theme_void() + 
      theme(legend.position="none") +
      scale_fill_manual( values= c('darkgrey','#006cc5') )+
      geom_text( aes( y = ypos, label = CUSTLABEL ), color = "white", size=6, fontface='bold' ) 

      
      
      
      
      
      
      
      
      
    
                  
      plot_ly(     labels = ~RACE_REPORTED,  values = ~SUBJECTS,  type = 'pie', sort= F,
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   texttemplate = '<b>%{label}</br></br>%{percent}</b>',
                   insidetextfont = list(color = '#FFFFFF', size= 25, style= "bold" ),
              
                   hoverinfo = 'text',
                   text = ~paste(RACE_REPORTED,"\n", prettyNum(SUBJECTS, big.mark=','), "subjects"    ),
                   
                   marker = list(colors = c( "#006cc5", "darkgrey" ),
                                 line = list(color = '#FFFFFF', width = 10) ) ,
                   showlegend = FALSE )
 
      
trialcount<-   sqldf(" select count(distinct(ID)) from SITES ")  %>% pull()
      

    rm(X)
    
   
    
    IBD2 %>% 
      select( RACE_REPORTED, WHT:MIX) %>% 
      filter( RACE_REPORTED == 'Y' ) %>% 
      
      group_by( RACE_REPORTED) %>% 
      
      summarize ( TRIALS =  n() ,
                  WHT =     sum(WHT, na.rm=T), 
                  BLK =     sum(BLK, na.rm=T),
                  ASN =     sum(ASN, na.rm=T), 
                  AIA =     sum(AIA, na.rm=T), 
                  HPI =     sum(HPI, na.rm=T), 
                  MIX =     sum(MIX, na.rm=T)  ) %>% 
      
      
      pivot_longer( cols= c(WHT,BLK,ASN,AIA,HPI,MIX),
                    names_to = "RACE",
                    values_to = "COUNT" )  %>% 
      mutate(RACE = ifelse( RACE %in%  c('AIA','HPI','MIX'), 'OTHER', RACE ) ) %>% 
      group_by ( RACE_REPORTED, RACE ) %>% 
      summarize( COUNT = sum(COUNT) ) %>% 
      mutate( PCT = COUNT / sum(COUNT),
              RACE  = factor(RACE, levels= c('WHT','ASN','BLK','OTHER') ),
              CUSTLABEL = paste(RACE,"\n",percent(   substr(round(PCT,3),1,5) )) )    %>% 
      arrange( COUNT) %>% 
      mutate( ypos = ifelse(RACE== 'OTHER', NA, cumsum(COUNT)- 0.5*COUNT )) %>% 
              
              
      # Basic piechart
      ggplot( aes(x="", y= COUNT, fill= RACE)) +
      geom_bar(stat="identity", width= 10, color="white", lwd=.2 ) +
      scale_fill_manual( values= c("#006cc5","#5f7fbc", "#8894b3", "#a9a9a9" ) )+
    
      geom_text( aes(y= ypos, label = CUSTLABEL ), color = "white", size=4.5, fontface='bold')+
      coord_polar("y", start= 1.1) +
      theme_void() + 
      theme(legend.position="none") 
    
    
levels(X$RACE)


      
    #   
    #   
    #   
    # 
    # plot_ly(     labels = ~RACE,  values = ~COUNT,  type = 'pie', sort= F,
    #              textposition = 'inside',
    #              textinfo = 'label+percent',
    #              texttemplate = '<b>%{label}</br></br>%{percent}</b>',
    #              insidetextfont = list(color = '#FFFFFF', size= 16, style= "bold" ),
    #              
    #              hoverinfo = 'text',
    #              text = ~paste(RACE,"\n", prettyNum(COUNT, big.mark=','), "subjects"    ),
    #              
    #              marker = list( colors= list( "#006cc5","#5f7fbc", "#8894b3", "#a9a9a9"),
    #                            line = list(color = '#FFFFFF', width = 5) ) ,
    #              showlegend = FALSE )
    # 
    # 
    # 
    # 