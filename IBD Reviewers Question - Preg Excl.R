



chisq.test( ZOOM$LATE, ZOOM$DAY, correct=FALSE)


CrossTable(IBD2$PHASES, IBD2$PREGNANT_EXCL, prop.chisq=F, prop.t=F)  #rows then/over columns


sqldf( "select PHASES,
               count(*) as N,
               count(*)*100 / sum(count()) over () as PCT,
                (COUNT(*) * 1.0) / sum(COUNT(*)) OVER() as PCTN,
               sum(ANALYZED) as SUBJECTS,
               sum(ANALYZED)*100 / sum(sum(ANALYZED)) over () as PCTSUB
       
       from IBD2
       group by 1")






IBD3  <-
    IBD2 %>% 
    select( CATEGORY, PHASES, ANALYZED, FEMALES, PREGNANT_EXCL) %>% 
    mutate( PHASES = ifelse(is.na(PHASES), 0, PHASES)) %>% 
    mutate( PHASES2 = factor( PHASES,  levels= c(1,2,3,4,0))) %>% 
    mutate( PHASES2 = recode_factor( PHASES2, '1'= 'P1', '2'= 'P2', '3' = 'P3', '4' = 'P4', '0' = 'PNA', .ordered = T) )
            


# Includes NA Trials

CrossTable(IBD3$PHASES2, IBD3$PREGNANT_EXCL, prop.chisq=F, prop.t=F)  #rows then/over columns


#Excludes NA Trials

IBD3A <-  IBD3 %>% filter( PHASES2 != "PNA") 
  CrossTable( IBD3A$PHASES2, IBD3A$PREGNANT_EXCL, prop.chisq=F, prop.t=F)  #rows then/over columns

  
  
  #3-4 TRIALS ONLYs
  
  IBD3B <-  IBD3 %>% filter( PHASES2 %in% c('P3','P4') ) 
  
  CrossTable( IBD3B$PHASES2, IBD3B$PREGNANT_EXCL, prop.chisq=F, prop.t=F)  #rows then/over columns
  
  
  
  
  
IBD3_SUMMARY <- 
  
    IBD3 %>% group_by( PHASES2 ) %>% 
      summarise( COUNT = n(),
                 SUBJECTS = sum(ANALYZED)) %>% 
      mutate( PCT_N = percent(COUNT / sum(COUNT)),
              PCT_SUBJ = percent(SUBJECTS / sum(SUBJECTS))) %>%  tbl_df

IBD3A_SUMMARY <- 
  
  IBD3A %>% group_by( PHASES2 ) %>% 
  summarise( COUNT = n(),
             SUBJECTS = sum(ANALYZED)) %>% 
  mutate( PCT_N = percent(COUNT / sum(COUNT)),
          PCT_SUBJ = percent(SUBJECTS / sum(SUBJECTS))) %>%  tbl_df

print(IBD3A_SUMMARY)  
  


IBD3B_SUMMARY <- 
  
  IBD3B %>% group_by( PHASES2, PREGNANT_EXCL ) %>% 
  summarise( COUNT = n(),
             SUBJECTS = sum(ANALYZED)) %>% 
  mutate( PCT_N = percent(COUNT / sum(COUNT)),
          PCT_SUBJ = percent(SUBJECTS / sum(SUBJECTS))) %>%  tbl_df



print(IBD3_SUMMARY)  
print(IBD3A_SUMMARY)  
print(IBD3B_SUMMARY)  




kbl(IBD3_SUMMARY, booktabs = T, format= "html", 
    caption = "<center><strong>IBD Clinical Trials by Phase Type</strong></center>",
    align= c('l','r','r','r','r'), escape=F, 
    col.names = linebreak(c("Phase", "Trials<br>N", "Subjects<br>N", "Trials<br>Pct", "Subjects<br>Pct"))) %>% 
  
  kable_styling(full_width = F, "hover") %>% column_spec(1, width = "4cm") %>%
  scroll_box(height = "6in")
