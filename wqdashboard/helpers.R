#This script holds helper functions for the wqdashboard R shiny app

#Make column names dataframe

fields<-data.frame(
  FIELD_NAME = c(
    'facility_code',
    'loc_name',
    'matrix_desc',
    'sample_date',
    
    'chemical_name',
    'fraction',
    'result_numeric',
    'result_unit',
    'detect_flag',
    'reporting_detection_limit',
    '[Limit/Guideline]',
    'latitude',
    'longitude'
    
  ),
  REQUIRED = c(
    'Y',
    'Y',
    'Y',
    'Y',
    
    'Y',
    'Y',
    'Y',
    'Y',
    'Y',
    'Y',
    'N',
    'N',
    'N'
    
  ),
  DATA_TYPE =c(
    'Text',
    'Text',
    'Text',
    'Date',
    
    'Text',
    'Text',
    'Numeric',
    'Text',
    'Text',
    'Numeric',
    'Numeric',
    'Numeric',
    'Numeric'
    
  ),
  DESCRIPTION=c(
    'A grouping variable for locations such as "Background", "Exposure". Or Sites such as "OU2", "OU8"',
    'Sample location names',
    'Sample matrix such as "Groundwater", "Air", etc.',
    'Date in mm/dd/yyyy format',
    
    'Parameter name such as "Aluminum", "Depth to Water"',
    'Analytical sample fraction. Must be in ("D","T", or blank. Other codes ignored)',
    'Value of sample result. Non detect results must be entered as blank.',
    'Units of sample result',
    'Binary "Y" for detections or "N" for non-detections. Other codes ignored',
    'Sample result reporting limit. Must not be blank in cases where detect_flag = "N"',
    'One or more fields of environmental limits/guidelines (e.g., US federal drinking water MCL). Any column name(s) will work.',
    'Latitude in decimal degreees',
    'Longitude in decimal degreees'
    
    
  ),
  stringsAsFactors = FALSE
  
  
)

#Fix up differnt spelled units
fixUnits<-function(df){
  
  
  df<-df %>% 
      mutate(UNITS = case_when(
      
      result_unit == 'mg/l' ~ 'mg/L',
      result_unit == 'MG/KG' ~ 'mg/kg',
      result_unit == 'PCI/G' ~ 'pCi/g',
      result_unit == 'MG/L' ~ 'mg/L',
      result_unit == 'UG/L' ~ 'ug/L',
      result_unit == '%' ~ 'percent',
      result_unit == 'PCI/L' ~ 'pCi/L',
      result_unit == 'DEG C' ~ 'deg C',
      result_unit == 'SU' ~ 'pH units',
      result_unit == 'US/CM' ~ 'uS/cm',
      result_unit == 'MV' ~ 'mV',
      TRUE ~ result_unit
      
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


