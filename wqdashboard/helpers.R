#This script holds helper functions for the wqdashboard R shiny app



fixUnits<-function(df){
  
  
  df<-df %>% 
      mutate(UNITS = case_when(
      
      result_unit == 'mg/l' ~ 'mg/L',
      TRUE ~ 'mg/L'
      
      )
    
    )
  
  return(df)
}


