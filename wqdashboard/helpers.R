#This script holds helper functions for the wqdashboard R shiny app



fixUnits<-function(df){
  
  
  df<-df %>% 
      mutate(UNITS = case_when(
      
      Units == 'mg/l' ~ 'mg/L',
      TRUE ~ 'mg/L'
      
      )
    
    )
  
  return(df)
}

#Functions----------------------------------
#Fix units cases
# fixUnits<-function(x){
#   df<-x
#   units<-df$Units
#   unitsOut<-case_when(
#     units == 'MG/L' ~ 'mg/l',
#     units == 'mg/l' ~ 'mg/l',
#     units == 'mg/L' ~ 'mg/l',
#     units == 'UG/L' ~ 'ug/l',
#     units == 'ug/l' ~ 'ug/l',
#     units == 'ug/L' ~ 'ug/l',
#     units == 'MG/KG' ~ 'mg/kg',
#     units == 'mg/kg' ~ 'mg/kg'
#   )
#   
#   return(unitsOut)
# }

#Calculate percent ND
perND<-function(x,ndChar = 0){
  
  count<-length(x)
  nd<-length(x[x==ndChar])
  pND<-nd/count*100
  return(pND)
  
}

#Plot functions----------------------------


hPlot<-function(data){
  
  hdat<-data
  hdat$Parameter<-paste0(hdat$Parameter," (",hdat$Units,")")
  g<-ggplot(hdat,aes(x=RESULT_ND,fill=Location))+
    geom_histogram(alpha=0.5)+
    facet_wrap(~Parameter,scales="free")+
    theme(strip.background = element_rect(fill = '#727272'),strip.text = element_text(colour='white',face='bold',size = 12))+
    theme(legend.position = "bottom", legend.title = element_blank())+
    labs(x="Value Bins=30",y="Count",title="Distribution (histogram) Non-Detects at Zero")+
    theme(plot.title = element_text(face='bold',size=14))
}


