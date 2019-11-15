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


