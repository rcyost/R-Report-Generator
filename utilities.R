

# author: Ryan Yost
# date: 2020 - 11 - 22

## ----Helper functions----

####### returns country of study info
# TODO checking logic 
getCountryInfo <- function(iso3, longOrWide){
  
  comment <- " This chunck will pull data from a local excel book and display the country's  attributes "
  # TODO put in logic in case the countryCode is put in wrong
  
  ## - - - - - - - - - - - - - - -  SET  UP 
  
  # country code for country to get data for 
  countryCode <- iso3
  
  # use this data to reference iso3 codes 
  
  countryClass_list <- read.xlsx(file = COUNTRY_CLASS_PATH, sheetIndex = 1,      stringsAsFactors = FALSE )
  
  countries_for_chart <- countryClass_list %>%
    filter(WEO_ISO_3 %in% countryCode)
  
  # WEO Code being a number gets it deleted by melt soon so changing it to a char
  countries_for_chart$WEO_Code <- as.character(countries_for_chart$WEO_Code) 
  
  # removing columns with NA 
  CoSdata <- countries_for_chart %>%
    select_if(~ !any(is.na(.)))
  
  
  
  ########  - - - - - - - - - - - - - - - - -FINAL OUPUT LOGIC
  if(longOrWide == "long"){
    # it would be better for viewing to have the data long 
    CoSmelt <- CoSdata %>%
      melt(id.vars="Value") %>%
      subset(select = c("variable","value"))
    
    colnames(CoSmelt) <- c("Attribute", "Value")
    
    return(CoSmelt)
    
  } else if (longOrWide == "wide") {
    return(CoSdata)
  } else {
    print("Please use long or wide as correct answers for longOrWide")
  }
  
  
  
  
}

##### returns countries based on one filter
# TODO checking logic
# TODO: put in another file
getCompInfo <- function(Codetype, CodeValue, longOrWide, path){

  comment <- " if oneOrMany == one then we are only pulling info about one country, 
               if it == many, then we are pulling data for a group,
              if we are pulling for a group we need to know how we define a group, manyCodetype,
              and then what group in that type we want manyCodeValue"
  
  # use this data to reference iso3 codes 
    
    countryClass_list <- read.xlsx(file = path, sheetIndex = 1,      stringsAsFactors = FALSE )
  
    CoSdata <- countryClass_list %>%
                        filter(countryClass_list[Codetype] == CodeValue)
  
    ########  - - - - - - - - - - - - - - - - -FINAL OUPUT LOGIC
    if(longOrWide == "long"){
      # it would be better for viewing to have the data long 
      CoSmelt <- CoSdata %>%
                  melt(id.vars="Value") %>%
                    subset(select = c("variable","value"))
        
      colnames(CoSmelt) <- c("Attribute", "Value")
        
      return(CoSmelt)
          
      } else if (longOrWide == "wide") {
          return(CoSdata)
      } else {
          print("Please use long or wide as correct answers for longOrWide")
      }

}  # end of entire function


####### ---- returns countries based on 2 filters, do not use not supported
getTwoCompInfo <- function(Codetype, CodeValue, Codetype2, CodeValue2, longOrWide){

  comment <- "  Codetype = type of classification, column header
                CodeValue = what we are filtering for,
                this function allows for two levels of filtering"
  
  # use this data to reference iso3 codes 
    
    countryClass_list <- read.xlsx(file = COUNTRY_CLASS_PATH, sheetIndex = 1,      stringsAsFactors = FALSE )
  
    CoSdata <- countryClass_list %>%
                        filter(countryClass_list[Codetype] == CodeValue)
    
    CoSdata <- CoSdata %>%
                        filter(CoSdata[Codetype2] == CodeValue2)
  
    ########  - - - - - - - - - - - - - - - - -FINAL OUPUT LOGIC
    if(longOrWide == "long"){
      # it would be better for viewing to have the data long 
      CoSmelt <- CoSdata %>%
                  melt(id.vars="Value") %>%
                    subset(select = c("variable","value"))
        
      colnames(CoSmelt) <- c("Attribute", "Value")
        
      return(CoSmelt)
          
      } else if (longOrWide == "wide") {
          return(CoSdata)
      } else {
          print("Please use long or wide as correct answers for longOrWide")
      }
    

  
    
    
}  # end of entire function


## ----groupings table, echo=FALSE, fig.align="center"---------------------

cos_groupingTable <- function(){
comment <- " This chunck will pull data from a local excel book and display the country's attributes "
# TODO put in logic in case the countryCode is put in wrong

## - - - - - - - - - - - - - - -  SET  UP 

# country code for country to get data for 
countryCode <- COS_ISO3

# use this data to reference iso3 codes 

countryClass_list <- read.xlsx(file = COUNTRY_CLASS_PATH, sheetIndex = 1, stringsAsFactors = FALSE )

countries_for_chart <- countryClass_list %>%
                          filter(WEO_ISO_3 == countryCode)

# WEO Code being a number gets it deleted by melt soon so changing it to a char
countries_for_chart$WEO_Code <- as.character(countries_for_chart$WEO_Code) 

# removing columns with NA 
CoSdata <- countries_for_chart %>%
              select_if(~ !any(is.na(.)))


# it would be better for viewing to have the data long 

CoSmelt <- CoSdata %>%
            melt(id.vars="WEO_ISO_3") %>%
              subset(select = c("variable","value"))

colnames(CoSmelt) <- c("Attribute", "Value")

kable(CoSmelt)

}


## ----01_scatter_WEO_WoRLD_TaxGDP_to_GDPPC, echo=FALSE, fig.align="center", message=FALSE, warning=FALSE----


make_chart_1_v2 <- function(COS_WEO3, COMP_DF, COMP_VALUE, MAX_DATE, DATA){
  
  # take in raw data and format to use for ggplot chart
  chart_data <- DATA %>%
    
    filter(COUNTRY %in% COMP_DF$WEO_Code,
           variable %in% c("NGDPRPC", "GGRT_GDP"),
           # most recent year
           dates == MAX_DATE) %>%
    select(COUNTRY, value, variable) %>%
    
    pivot_wider(names_from = variable, values_from = value) %>%
    
    drop_na()
  
  
  
  ########################## CHART
  
  # if no error we just return a blank string
  errorMess = ""
  
  
  # we need to create a set of points for the countries of comparison
  # check if there is any data
  if(dim(chart_data)[1]==0){
    compPoint <- NULL
    errorMess = "no data"
  } else{
  # scatterplots for group
  compPoint <- geom_point(data = chart_data,
                          aes(x = log(NGDPRPC), 
                              y = GGRT_GDP/100, 
                              size = 0.15))
  }
  # handle if no data for cos
  if(dim(chart_data %>% filter(COUNTRY == COS_WEO3))[1]==0){
    cosPoint <- NULL
    errorMess = "no data for COS"
  } else{
    # scatterplot for country of study in red
    cosPoint <- geom_point(data = (chart_data %>% filter(COUNTRY == COS_WEO3)),
                           aes(x = log(NGDPRPC), 
                               y = GGRT_GDP/100, 
                               size = 0.15), color="red")
  }
  
  ggplot(chart_data, aes(x = log(NGDPRPC), y = GGRT_GDP/100)) + 
    compPoint + 
    cosPoint + 
    geom_text(label=chart_data$COUNTRY, label.size = 1, nudge_x = 2, nudge_y=.01) +
    
    labs(
      title = paste("Tax to GDP by GDP per Capita for", MAX_DATE),
      subtitle = paste("Grouping:", gsub("_", " ", COMP_VALUE)), 
      y = "Tax to GDP",
      x = "Log Real GDP per Capita USD",
      caption = "Source: Random Data") +
    
    theme_chart_01() +
    
    scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    
    expand_limits(y = 0)
  
  
  
}    


chart_1 <- function(COS_WEO3, COMP_DF, COMP_VALUE, MAX_DATE, DATA){
  
  tryCatch(
    expr = make_chart_1_v2(COS_WEO3, COMP_DF, COMP_VALUE, MAX_DATE, DATA),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 4,
                 label = "Failed to make Tax-GDP scatterplot chart") + 
        theme_void()
    }
  )
}



## ----weo charts----

make_weoLine <- function(weo, weoCode, title, subtitle, COS_NAME, COS_WEO3, COMP_CODE, COMP_VALUE, COMP_DF){
  

  chart_data <- weo %>%
    filter(WEO_Code == COS_WEO3,
           dates > MIN_DATE,
           dates < MAX_DATE,
           variable == weoCode) %>%
    select(dates, variable, value, Value)
  
  # ex country of study
  compData <- weo %>%
    filter(WEO_Code %in% COMP_DF$WEO_Code,
           WEO_Code != COS_WEO3,
           dates > MIN_DATE,
           dates < MAX_DATE,
           variable == weoCode) %>%
  
    group_by(dates, variable) %>%
    summarize(value = mean(value, na.rm = TRUE)) %>%
    mutate(Value = COMP_VALUE)
  
  mergedData <- bind_rows(chart_data, compData)
  
  line <- mergedData %>%
    
    ggplot(aes(x = dates, y = value, color = Value)) + 
    
    # line 1, Country of Study  
    geom_line(size = 1) + 
    
    # Title and caption 
    labs(title = paste(title),
         subtitle = paste(subtitle),
         y = "",
         x = "",
         caption = "Source: Random Data") + 
    # set y axis height 
    expand_limits(y = 0) +
    
    scale_x_continuous(breaks = seq(min(mergedData$dates), max(mergedData$dates), by = 2)) +
    
    # styling 
    scale_color_manual(values = c("#1B4F72","#2874A6"))+
    #theme_piotr_edited() 
    ISORA_staffing() #+ 
    
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  
  line
  
  
}

weoLine <- function(weo, weoCode, title, subtitle, COS_NAME, COS_WEO3, COMP_CODE, COMP_VALUE, COMP_DF){
  
  tryCatch(
    expr = make_weoLine(weo, weoCode, title, subtitle, COS_NAME, COS_WEO3, COMP_CODE, COMP_VALUE, COMP_DF),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 8,
                 label = paste("Failed to make", title, "chart")) + 
        theme_void()
    }
  )
}


make_weoBarStacked <- function(weo, COS_NAME, COS_WEO3){
  
  
  indicators <- c("GGRTIgdp",
                  "GGRTGSgdp",
                  "GGRTTgdp",
                  "GGRTOEgdp")
  
  chart_data <- weo %>%
    filter(WEO_Code == COS_WEO3,
           dates > MIN_DATE,
           dates < MAX_DATE,
           variable %in% indicators) %>%
    select(dates, variable, value, Value) %>%
    
    # https://stackoverflow.com/questions/35610437/using-dplyr-to-conditionally-replace-values-in-a-column
    mutate(variable = case_when(variable == 'GGRTIgdp' ~ 'Income, Profits, Capital Gains',
                                variable == 'GGRTGSgdp' ~ 'Goods and Services',
                                variable == 'GGRTTgdp' ~ 'International Trade',
                                variable == 'GGRTOEgdp' ~ 'Other',
                                TRUE ~ 'Error'))
  
  
  blues <- c("#1B4F72","#2874A6","#3498DB", "#85C1E9")#,"#AED6F1", "#5DADE2")#,"#D6EAF8")
  
  
  chart <- ggplot(chart_data, aes(x = dates, y = value, fill = variable)) +
    
    geom_bar(position="fill", stat="identity") + 
    
    # Title and caption 
    labs(title = paste(COS_NAME, "Taxes By Tax Type"),
         #subtitle = paste(subtitle),
         y = "",
         x = "",
         caption = "Source: Random Data") + 
    
    scale_fill_manual(values = blues) + 
    
    expand_limits(y = 0) +
    
    scale_x_continuous(breaks = seq(min(chart_data$dates), max(chart_data$dates), by = 2)) + 
    
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    
    ISORA_staffing() +guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  
  chart
  
  
}

weoBarStacked <- function(weo, COS_NAME, COS_WEO3){
  
  tryCatch(
    expr = make_weoBarStacked(weo, COS_NAME, COS_WEO3),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 3,
                 label = paste("Failed to make WEO Tax-GDP breakdown chart")) + 
        theme_void()
    }
  )
}



make_weoBarStacked_GGR <- function(weo, COS_NAME, COS_WEO3){
  
  
  indicators <- c("GGRT_GDP",
                  "GGRSgdp",
                  "GGRGgdp",
                  "GGROgdp")
  
  chart_data <- weo %>%
    filter(WEO_Code == COS_WEO3,
           dates > MIN_DATE,
           dates < MAX_DATE,
           variable %in% indicators) %>%
    select(dates, variable, value, Value) %>%
    
    # https://stackoverflow.com/questions/35610437/using-dplyr-to-conditionally-replace-values-in-a-column
    mutate(variable = case_when(variable == 'GGRT_GDP' ~ 'Taxes',
                                variable == 'GGRSgdp' ~ 'Social Contributions',
                                variable == 'GGRGgdp' ~ 'Grants',
                                variable == 'GGROgdp' ~ 'Other',
                                TRUE ~ 'Error'))
  
  
  
  blues <- c("#1B4F72","#2874A6","#3498DB", "#85C1E9")#,"#AED6F1", "#5DADE2")#,"#D6EAF8")
  
  
  chart <- ggplot(chart_data, aes(x = dates, y = value, fill = variable)) +
    
    geom_bar(position="fill", stat="identity") + 
    
    # Title and caption 
    labs(title = paste(COS_NAME, "General Government Revenue Breakdown"),
         #subtitle = paste(subtitle),
         y = "",
         x = "",
         caption = "Source: Random Data") + 
    
    scale_fill_manual(values = blues) + 
    
    expand_limits(y = 0) +
    
    scale_x_continuous(breaks = seq(min(chart_data$dates), max(chart_data$dates), by = 2)) + 
    
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    
    ISORA_staffing() +guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  
  chart
  
  
}

weoBarStacked_GGR <- function(weo, COS_NAME, COS_WEO3){
  
  tryCatch(
    expr = make_weoBarStacked_GGR(weo, COS_NAME, COS_WEO3),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 3,
                 label = paste("Failed to make WEO Tax-GDP breakdown chart")) + 
        theme_void()
    }
  )
}




## ----fad tp   ----

make_tpChart <- function(rates, code, title, subtitle, citation, COS_NAME, COS_WEO3, COMP_CODE, COMP_VALUE, COMP_DF){
  
  chart_data <- rates %>%
    filter(WEO_Code == COS_WEO3,
           dates > MIN_DATE,
           dates < MAX_DATE,
           variable == code) %>%
    select(dates, variable, value, Value)
  
  compData <- rates %>%
    filter(WEO_Code %in% COMP_DF$WEO_Code,
           WEO_Code != COS_WEO3,
           dates > MIN_DATE,
           dates < MAX_DATE,
           variable == code) %>%
    
    group_by(dates, variable) %>%
    summarize(value = mean(value, na.rm = TRUE)) %>%
    mutate(Value = COMP_VALUE)
  
  mergedData <- bind_rows(chart_data, compData)
  
  line <- mergedData %>%
    
    ggplot(aes(x = dates, y = value, color = Value)) + 
    
    # line 1, Country of Study  
    geom_line(size = 1) + 
    
    # Title and caption 
    labs(title = paste(title),
         subtitle = paste(subtitle),
         y = "",
         x = "",
         caption = citation) + 
    # set y axis height 
    expand_limits(y = 0) +
    
    scale_x_continuous(breaks = seq(min(mergedData$dates), max(mergedData$dates), by = 2)) +
    
    # styling 
    scale_color_manual(values = c("#1B4F72","#2874A6"))+
    #theme_piotr_edited() 
    ISORA_staffing() 
  
  
  line
  
  
}

tpChart <- function(rates, code, title, subtitle, citation, COS_NAME, COS_WEO3, COMP_CODE, COMP_VALUE, COMP_DF){
  
  tryCatch(
    expr = make_tpChart(rates, code, title, subtitle, citation, COS_NAME, COS_WEO3, COMP_CODE, COMP_VALUE, COMP_DF),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 8,
                 label = paste("Failed to make", title, "chart")) + 
        theme_void()
    }
  )
}


make_tpBarChart <- function(world, COS_NAME, COS_WEO3){
  
  
  indicators <- c("Corporate Income Tax Revenue as a % of GDP.A", 
                  "Goods and Services Tax Revenue as a % of GDP.A", 
                  "Individual Income Tax Revenue as a % of GDP.A", 
                  "Property Tax Revenue as a % of GDP.A", 
                  "Taxes on Payroll and Workforce Revenue as a % of GDP.A", 
                  "Trade Tax Revenue as a % of GDP.A")
  
  chart_data <- world %>%
    filter(WEO_Code == COS_WEO3,
           dates > MIN_DATE,
           dates < MAX_DATE,
           variable %in% indicators) %>%
    select(dates, variable, value, Value) %>%
    
    # https://stackoverflow.com/questions/35610437/using-dplyr-to-conditionally-replace-values-in-a-column
    mutate(variable = case_when(variable == 'Corporate Income Tax Revenue as a % of GDP.A' ~ 'Corporate Income Tax',
                                variable == 'Goods and Services Tax Revenue as a % of GDP.A' ~ 'Goods and Services Tax',
                                variable == 'Individual Income Tax Revenue as a % of GDP.A' ~ 'Individual Income Tax',
                                variable == 'Property Tax Revenue as a % of GDP.A' ~ 'Property Tax',
                                variable == 'Taxes on Payroll and Workforce Revenue as a % of GDP.A' ~ 'Payroll Tax',
                                variable == 'Trade Tax Revenue as a % of GDP.A' ~ 'Trade Tax',
                                TRUE ~ 'Error'))
  
  
  
  blues <- c("#1B4F72","#2874A6","#3498DB", "#85C1E9","#AED6F1", "#5DADE2")#,"#D6EAF8")
  
  
  TPchart <- ggplot(chart_data, aes(x = dates, y = value, fill = variable)) +
    
    geom_bar(position="fill", stat="identity") + 
    
    # Title and caption 
    labs(title = paste(COS_NAME, "Taxes By Tax Type"),
         #subtitle = paste(subtitle),
         y = "",
         x = "",
         caption = "Source: Random Data") + 
    
    scale_fill_manual(values = blues) + 
    
    expand_limits(y = 0) +
    
    scale_x_continuous(breaks = seq(min(chart_data$dates), max(chart_data$dates), by = 2)) + 
    
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    
    ISORA_staffing() + guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  
  TPchart
  
  
}

tpBarChart <- function(world, COS_NAME, COS_WEO3){
  
  tryCatch(
    expr = make_tpBarChart(world, COS_NAME, COS_WEO3),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 3,
                 label = paste("Failed to make Tax Policy Tax-GDP breakdown chart")) + 
        theme_void()
    }
  )
}



## ----17_ISORA_Staffing, echo=FALSE, fig.align="center", message=FALSE, warning=FALSE----


functionalStaffing_v2 <- function(ISORA, COS_WEO3, COMP_DF, COS_NAME){
  tryCatch(
    expr = make_functionalStaffing_v2(ISORA, COS_WEO3, COMP_DF, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 8,
                 label = "Failed to make ISORA functional staffing chart") + 
        theme_void()
    }
  )
}
make_functionalStaffing_v2 <- function(ISORA, COS_WEO3, COMP_DF, COS_NAME) {
  

    chart_data <- ISORA %>%
      
      # filter for data needed for chart
      #filter(dataset == "ISORA") %>%
      mutate(indicatorCode = as.character(indicatorCode))%>%
      filter(indicatorCode %in% c("94030_207", "95230_207", "95240_207","94020_207")) %>%
      # remove values that are missing, noted with a D
      filter(value != 'D') %>%
      # now we can cast to numeric
      mutate(value = as.double(value))%>%

      # filter by country of study
      filter(CountryCode == COS_WEO3) %>%
      
      # filter by latest year
      filter(year == max(year)) %>%
      
      # clean up names to chart
      mutate(
      Staff_Functional_Group = ifelse(indicatorCode == "94030_207", "Debt Collection",
                               ifelse(indicatorCode == "95230_207", "Taxpayer Services",
                               ifelse(indicatorCode == "95240_207", "Other",
                               ifelse(indicatorCode == "94020_207", "Audit and Verification", "Error"))))) %>%
              
      select(Staff_Functional_Group, year, value, country) %>%
      
      mutate(country = COS_NAME)
    
    maxYear = unique(chart_data$year)
    
    chart_data <- chart_data %>% select(-year)
    
    chart_data_comp <- ISORA %>%
      
      # filter for data needed for chart
      #filter(dataset == "ISORA") %>%
      mutate(indicator = as.character(indicatorCode))%>%
      filter(indicatorCode %in% c("94030_207", "95230_207", "95240_207","94020_207")) %>%
      # remove values that are missing, noted with a D
      filter(value != 'D') %>%
      # now we can cast to numeric
      mutate(value = as.double(value))%>%
      
      # remove any groups that do not sum to 100
      group_by(CountryCode, year)%>%
      mutate(group_sum_check = sum(value)) %>%
      filter(between(group_sum_check, 99.5, 100.5)) %>%
      ungroup() %>%
      
      filter(CountryCode %in% COMP_DF$WEO_Code)%>%
      
      filter(year == max(maxYear)) %>%
      
      # clean up names to chart
      mutate(
        Staff_Functional_Group = ifelse(indicatorCode == "94030_207", "Debt Collection",
                                 ifelse(indicatorCode == "95230_207", "Taxpayer Services",
                                 ifelse(indicatorCode == "95240_207", "Other",
                                 ifelse(indicatorCode == "94020_207", "Audit and Verification", "Error"))))) %>%
      
      group_by(Staff_Functional_Group) %>%
      summarize(value = mean(value)) %>%
      mutate(country = COMP_VALUE)
    
    c <- rbind(chart_data, chart_data_comp)
    
    plot <- c %>%
      
      ggplot(aes(x = reorder(Staff_Functional_Group,  value), y = value/100, fill = country)) +
      
      geom_bar(position = "dodge", stat = "identity") + 
      
      ggtitle(paste(COS_NAME, "Staffing by Functional Groups")) +
      
      labs(subtitle = paste("percent of full time employees,", maxYear), 
           y = " ",
           x = " ",
           caption = "Source: Random Data") +
      
      ISORA_staffing() +
      
      #blues <- c("#1B4F72","#2874A6","#3498DB", "#85C1E9","#5DADE2","#AED6F1","#D6EAF8")
      
      
      scale_fill_manual(values = c("#1B4F72","#2874A6")) +
      
      coord_flip()+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    
    plot
  

}


genderStaffing <- function(ISORA, COS_ISO3, COMP_DF, COS_NAME){
  tryCatch(
    expr = make_genderStaffing(ISORA, COS_WEO3, COMP_DF, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 8,
                 label = "Failed to make ISORA gender staffing chart") + 
        theme_void()
    }
  )
}
make_genderStaffing <- function(ISORA, COS_ISO3, COMP_DF, COS_NAME) {
  
  
  chart_data <- ISORA %>%
    
    select(country, CountryCode, IndicatorName, indicatorCode, Attribute, isora_surveyYear, year, value) %>%
    
    # filter for data needed for chart
    #filter(dataset == "ISORA") %>%
    mutate(indicator = as.character(indicatorCode))%>%
    
    # trim dataset to needed countries and indicators
    filter(
      indicator %in% c("84070_75", "84060_75", "95120_75"),
      # remove countries not in comparable group
      CountryCode %in% COS_WEO3,
      # remove values that are missing, noted with a D
      value != 'D',
      # odd isora stuff to remove, data is is value attribute 
      Attribute != 'Observation',
      # filter by latest year
      year == max(year)
    ) %>%
    
    # cast to numeric to ensure it plays nice
    mutate(value = as.double(value)) %>%
    
    # for each country in a year calculate total staff  
    group_by(CountryCode, year) %>%
    mutate(totalStaff = sum(value)) %>%
    ungroup() %>%
    
    # for each country, indicator, and year calculate percent of total staff
    group_by(CountryCode, indicatorCode, year) %>%
    
    mutate(percentStaff = value / totalStaff,
           label = ifelse(indicatorCode == "84070_75", "Female Staff",
                          ifelse(indicatorCode == "84060_75", "Male Staff",
                                 ifelse(indicatorCode == "95120_75", "Other Staff", "Error"))),
           count = n()) %>%
    ungroup()%>%
    
    
    # group by gender and calculate averages
    group_by(label) %>%
    mutate(averagePercent = mean(percentStaff)) %>%
    select(label, averagePercent, year, count) %>%
    unique() %>%
    
    # include country/group label
    mutate(country = COS_NAME)
  
  
  chart_dataCOMP <- ISORA %>%
    
    select(country, CountryCode, IndicatorName, indicatorCode, Attribute, isora_surveyYear, year, value) %>%
    
    # filter for data needed for chart
    #filter(dataset == "ISORA") %>%
    mutate(indicator = as.character(indicatorCode))%>%
    
    # trim dataset to needed countries and indicators
    filter(
      indicator %in% c("84070_75", "84060_75", "95120_75"),
      # remove countries not in comparable group
      CountryCode %in% COMP_DF$WEO_Code,
      # remove values that are missing, noted with a D
      value != 'D',
      # odd isora stuff to remove, data is is value attribute 
      Attribute != 'Observation',
      # filter by latest year for the country of study so it matches
      year == max(chart_data$year)
    ) %>%
    
    # cast to numeric to ensure it plays nice
    mutate(value = as.double(value)) %>%
    
    # for each country in a year calculate total staff  
    group_by(CountryCode, year) %>%
    mutate(totalStaff = sum(value)) %>%
    ungroup() %>%
    
    # for each country, indicator, and year calculate percent of total staff
    group_by(CountryCode, indicatorCode, year) %>%
    
    mutate(percentStaff = value / totalStaff,
           label = ifelse(indicatorCode == "84070_75", "Female Staff",
                          ifelse(indicatorCode == "84060_75", "Male Staff",
                                 ifelse(indicatorCode == "95120_75", "Other Staff", "Error")))) %>%
    ungroup()%>%
    
    
    mutate(count = n())%>%
    
    
    # group by gender and calculate averages
    group_by(label) %>%
    mutate(averagePercent = mean(percentStaff)) %>%
    select(label, averagePercent, year, count) %>%
    unique() %>%
    
    # include country/group label
    mutate(country = COMP_VALUE)
  
  
  
  
  # combine group and country of study to chart
  c <- rbind(chart_data, chart_dataCOMP)
  
  
  plot <- c %>%
    
    ggplot(aes(x = reorder(label,  averagePercent), y = averagePercent, fill = country)) +
    
    geom_bar(position = "dodge", stat = "identity") + 
    
    ggtitle(paste(COS_NAME, "Staffing by Gender")) +
    
    labs(subtitle = paste("percent of full time employees,", unique(c$year)), 
         y = " ",
         x = " ",
         caption = "Source: Random Data") +
    
    ISORA_staffing() +
    
    #blues <- c("#1B4F72","#2874A6","#3498DB", "#85C1E9","#5DADE2","#AED6F1","#D6EAF8")
    
    
    scale_fill_manual(values = c("#1B4F72","#2874A6")) +
    
    coord_flip() + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  plot
}


genderStaffingExec <- function(ISORA, COS_ISO3, COMP_DF, COS_NAME){
  tryCatch(
    expr = make_genderStaffingExec(ISORA, COS_WEO3, COMP_DF, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 8,
                 label = "Failed to make ISORA gender executive staffing chart") + 
        theme_void()
    }
  )
}
make_genderStaffingExec <- function(ISORA, COS_ISO3, COMP_DF, COS_NAME) {
  
  
  chart_data <- ISORA %>%
    
    select(country, CountryCode, IndicatorName, indicatorCode, Attribute, isora_surveyYear, year, value) %>%
    
    # filter for data needed for chart
    #filter(dataset == "ISORA") %>%
    mutate(indicator = as.character(indicatorCode))%>%
    
    # trim dataset to needed countries and indicators
    filter(
      indicator %in% c("84070_215", "84060_215", "95120_215"),
      # remove countries not in comparable group
      CountryCode %in% COS_WEO3,
      # remove values that are missing, noted with a D
      value != 'D',
      # odd isora stuff to remove, data is is value attribute 
      Attribute != 'Observation',
      # filter by latest year
      year == max(year)
    ) %>%
    
    # cast to numeric to ensure it plays nice
    mutate(value = as.double(value)) %>%
    
    # for each country in a year calculate total staff  
    group_by(CountryCode, year) %>%
    mutate(totalStaff = sum(value)) %>%
    ungroup() %>%
    
    # for each country, indicator, and year calculate percent of total staff
    group_by(CountryCode, indicatorCode, year) %>%
    
    mutate(percentStaff = value / totalStaff,
           label = ifelse(indicatorCode == "84070_215", "Female Executives",
                          ifelse(indicatorCode == "84060_215", "Male Executives",
                                 ifelse(indicatorCode == "95120_215", "Other Executives", "Error"))),
           count = n()) %>%
    ungroup()%>%
    
    
    # group by gender and calculate averages
    group_by(label) %>%
    mutate(averagePercent = mean(percentStaff)) %>%
    select(label, averagePercent, year, count) %>%
    unique() %>%
    
    # include country/group label
    mutate(country = COS_NAME)
  
  
  chart_dataCOMP <- ISORA %>%
    
    select(country, CountryCode, IndicatorName, indicatorCode, Attribute, isora_surveyYear, year, value) %>%
    
    # filter for data needed for chart
    #filter(dataset == "ISORA") %>%
    mutate(indicator = as.character(indicatorCode))%>%
    
    # trim dataset to needed countries and indicators
    filter(
      indicator %in% c("84070_215", "84060_215", "95120_215"),
      # remove countries not in comparable group
      CountryCode %in% COMP_DF$WEO_Code,
      # remove values that are missing, noted with a D
      value != 'D',
      # odd isora stuff to remove, data is is value attribute 
      Attribute != 'Observation',
      # filter by latest year for the country of study so it matches
      year == max(chart_data$year)
    ) %>%
    
    # cast to numeric to ensure it plays nice
    mutate(value = as.double(value)) %>%
    
    # for each country in a year calculate total staff  
    group_by(CountryCode, year) %>%
    mutate(totalStaff = sum(value)) %>%
    ungroup() %>%
    
    # for each country, indicator, and year calculate percent of total staff
    group_by(CountryCode, indicatorCode, year) %>%
    
    mutate(percentStaff = value / totalStaff,
           label = ifelse(indicatorCode == "84070_215", "Female Executives",
                          ifelse(indicatorCode == "84060_215", "Male Executives",
                                 ifelse(indicatorCode == "95120_215", "Other Executives", "Error")))) %>%
    ungroup()%>%
    
    
    mutate(count = n())%>%
    
    
    # group by gender and calculate averages
    group_by(label) %>%
    mutate(averagePercent = mean(percentStaff, na.rm = TRUE)) %>%
    select(label, averagePercent, year, count) %>%
    unique() %>%
    
    # include country/group label
    mutate(country = COMP_VALUE)
  
  
  
  
  # combine group and country of study to chart
  c <- rbind(chart_data, chart_dataCOMP)
  
  
  plot <- c %>%
    
    ggplot(aes(x = reorder(label,  averagePercent), y = averagePercent, fill = country)) +
    
    geom_bar(position = "dodge", stat = "identity") + 
    
    ggtitle(paste(COS_NAME, "Executives by Gender")) +
    
    labs(subtitle = paste("percent of Executives", unique(c$year)), 
         y = " ",
         x = " ",
         caption = "Source: Random Data") +
    
    ISORA_staffing() +
    
    #blues <- c("#1B4F72","#2874A6","#3498DB", "#85C1E9","#5DADE2","#AED6F1","#D6EAF8")
    
    
    scale_fill_manual(values = c("#1B4F72","#2874A6")) +
    
    coord_flip() + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  plot
}


ageStaffing <- function(ISORA, COS_WEO3, COMP_DF, COS_NAME){
  tryCatch(
    expr = make_ageStaffing(ISORA, COS_WEO3, COMP_DF, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 8,
                 label = "Failed to make ISORA staffing by age chart") + 
        theme_void()
    }
  )
}
make_ageStaffing <- function(ISORA, COS_WEO3, COMP_DF, COS_NAME) {
  
  chart_data <- ISORA %>%
    
    # filter for data needed for chart
    #filter(dataset == "ISORA") %>%
    mutate(indicatorCode = as.character(indicatorCode))%>%
    filter(indicatorCode %in% c("83950_28", "83960_28", "83970_28","83980_28", "83940_28", "83990_28")) %>%
    # remove values that are missing, noted with a D
    filter(value != 'D') %>%
    # now we can cast to numeric
    mutate(value = as.double(value))%>%
    
    # filter by country of study
    filter(CountryCode %in% COS_WEO3) %>%
    
    # filter by latest year
    filter(year == max(year)) %>%
    
    # clean up names to chart
    mutate(
      Staff_Functional_Group = str_sub(IndicatorName, 50, nchar(IndicatorName)),
      Staff_Functional_Group = str_trim(Staff_Functional_Group)
    ) %>%
    
    
    group_by(country) %>%
    
    mutate(total = sum(value)) %>%
    ungroup() %>%
    
    mutate(percent = value/total) %>%
    
    select(Staff_Functional_Group, percent, country, year) %>%
    
    mutate(country = COS_NAME)
  
  maxYear = unique(chart_data$year)
  
  chart_data <- chart_data %>% select(-year)
  
  chart_data_comp <- ISORA %>%
    
    # filter for data needed for chart
    #filter(dataset == "ISORA") %>%
    mutate(indicator = as.character(indicatorCode))%>%
    filter(indicatorCode %in% c("83950_28", "83960_28", "83970_28","83980_28", "83940_28", "83990_28")) %>%
    
    # filter out uneeded data
    filter(
      value != 'D',
      CountryCode %in% COMP_DF$WEO_Code,
      year == max(maxYear)
    ) %>%
    
    # cast to numeric
    mutate(
      value = as.double(value),
      Staff_Functional_Group = str_sub(IndicatorName, 50, nchar(IndicatorName)),
      Staff_Functional_Group = str_trim(Staff_Functional_Group)
    ) %>%
    
    # calculate totals
    group_by(country, year) %>%
    mutate(total = sum(value))%>%
    ungroup()%>%
    # calculate percentages
    mutate(percent = value/total) %>%
    group_by(Staff_Functional_Group)%>%
    summarise(
      percent = mean(percent)
    )%>%
    
    mutate(country = COMP_VALUE,
           year = chart_data$year)
  
  c <- rbind(chart_data, chart_data_comp)
  
  c$Staff_Functional_Group <- factor(c$Staff_Functional_Group,
                                     levels = c("Under 25 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "Over 64 years"))
  
  plot <- c %>%
    
    #ggplot(aes(x = reorder(Staff_Functional_Group,  percent), y = percent, fill = country)) +
    ggplot(aes(x = Staff_Functional_Group, y = percent, fill = country)) +
    
    geom_bar(position = "dodge", stat = "identity") + 
    
    ggtitle(paste(COS_NAME, "Staffing by Age")) +
    
    labs(subtitle = paste("percent of full time employees,", maxYear), 
         y = " ",
         x = " ",
         caption = "Source: Random Data") +
    
    ISORA_staffing() +
    
    #blues <- c("#1B4F72","#2874A6","#3498DB", "#85C1E9","#5DADE2","#AED6F1","#D6EAF8")
    
    
    scale_fill_manual(values = c("#1B4F72","#2874A6")) +
    
    coord_flip()+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  plot
}



locationStaffing <- function(ISORA, COS_WEO3, COMP_DF, COS_NAME){
  tryCatch(
    expr = make_locationStaffing(ISORA, COS_WEO3, COMP_DF, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 8,
                 label = "Failed to make ISORA staffing by location chart") + 
        theme_void()
    }
  )
}
make_locationStaffing <- function(ISORA, COS_WEO3, COMP_DF, COS_NAME) {
  
  chart_data <- ISORA %>%
    
    # filter for data needed for chart
    #filter(dataset == "ISORA") %>%
    mutate(indicatorCode = as.character(indicatorCode))%>%
    filter(indicatorCode %in% c("83640_520", "83610_520", "83620_520","83600_520", "83630_520", "83650_520")) %>%
    # remove values that are missing, noted with a D
    filter(value != 'D') %>%
    # now we can cast to numeric
    mutate(value = as.double(value))%>%
    
    # remove any groups that do not sum to 100
    group_by(CountryCode, year)%>%
    mutate(total = sum(value),
           percent = value/total) %>%
    #filter(between(group_sum_check, 99.5, 100.5)) %>%
    ungroup() %>%
    
    
    # filter by country of study
    filter(CountryCode == COS_WEO3) %>%
    
    # filter by latest year
    filter(year == max(year)) %>%
    
    # clean up names to chart
    mutate(
      label = ifelse(indicatorCode == "83640_520", "Service Centres",
                     ifelse(indicatorCode == "83610_520", "Regional Offices",
                            ifelse(indicatorCode == "83620_520", "Branch Offices",
                                   ifelse(indicatorCode == "83600_520", "Headquarters",
                                          ifelse(indicatorCode == "83630_520", "Data Centers",
                                                 ifelse(indicatorCode == "83650_520", "Other", "Error"))))))
    ) %>%
    
    select(label, percent, country, year)
  
  # take max year for filtering group data so they match
  maxYear = unique(chart_data$year)
  
  # this is a snapchat so no longer need date
  chart_data <- chart_data %>% select(-year)
  
  chart_data_comp <- ISORA %>%
    
    # filter for data needed for chart
    #filter(dataset == "ISORA") %>%
    mutate(indicator = as.character(indicatorCode))%>%
    filter(indicatorCode %in% c("83640_520", "83610_520", "83620_520","83600_520", "83630_520", "83650_520")) %>%
    
    
    # filter out uneeded data
    filter(
      value != 'D',
      CountryCode %in% COMP_DF$WEO_Code,
      year == max(maxYear)
    ) %>%
    
    
    mutate(
      value = as.double(value),
      label = ifelse(indicatorCode == "83640_520", "Service Centres",
                     ifelse(indicatorCode == "83610_520", "Regional Offices",
                            ifelse(indicatorCode == "83620_520", "Branch Offices",
                                   ifelse(indicatorCode == "83600_520", "Headquarters",
                                          ifelse(indicatorCode == "83630_520", "Data Centers",
                                                 ifelse(indicatorCode == "83650_520", "Other", "Error"))))))
    ) %>%
    
    # remove any groups that do not sum to 100
    group_by(CountryCode, year)%>%
    mutate(total = sum(value)) %>%
    #filter(between(group_sum_check, 99.5, 100.5)) %>%
    ungroup() %>%
    
    mutate(percent = value / total) %>%
    
    group_by(label)%>%
    summarise(
      percent = mean(percent)
    )%>%
    
    mutate(country = COMP_VALUE,
           year = chart_data$year)
  
  c <- rbind(chart_data, chart_data_comp)
  
  plot <- c %>%
    
    #ggplot(aes(x = reorder(Staff_Functional_Group,  percent), y = percent, fill = country)) +
    ggplot(aes(x = reorder(label, percent), y = percent, fill = country)) +
    
    geom_bar(position = "dodge", stat = "identity") + 
    
    ggtitle(paste(COS_NAME, "Staffing by Office Type")) +
    
    labs(subtitle = paste("percent of ,", maxYear), 
         y = " ",
         x = " ",
         caption = "Source: Random Data") +
    
    ISORA_staffing() +
    
    #blues <- c("#1B4F72","#2874A6","#3498DB", "#85C1E9","#5DADE2","#AED6F1","#D6EAF8")
    
    
    scale_fill_manual(values = c("#1B4F72","#2874A6")) +
    
    coord_flip()+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  plot  
  
}




## ----18_ISORA_Comment, echo=FALSE, fig.align="center", message=FALSE, warning=FALSE----

ISORA_comment <- function(ISORA, comment_code, COS_ISO3, COS_NAME, section){
  
  comment <- ISORA %>%
    filter(
      indicator == comment_code,
      year == max(year),
      iso3c == COS_ISO3
    )
  
  if(nrow(comment) == 0) {
    return(paste("No comment for", COS_NAME, "in", section, "section"))
  }
  else{
    return(comment$value)
  }
  
}

## ----19_IOn-Time_filing, echo=FALSE, fig.align="center", message=FALSE, warning=FALSE----

onTimeFilingBar_v3 <- function(ISORA, COS_ISO3, COS_NAME, COMP_DF){
  
  chart_data <- ISORA %>%
    
    select(country, CountryCode, IndicatorName, indicatorCode, Attribute, isora_surveyYear, year, value) %>%
    
    # filter for data needed for chart
    #filter(dataset == "ISORA") %>%
    mutate(indicator = as.character(indicatorCode))%>%
    
    # trim dataset to needed countries and indicators
    filter(
      indicator %in% c("88140_37", "88140_40", "88140_38", "88140_39"),
      # remove countries not in comparable group
      CountryCode %in% COS_WEO3,
      # remove values that are missing, noted with a D
      value != 'D',
      # odd isora stuff to remove, data is is value attribute 
      Attribute != 'Observation',
      
    ) %>%
    # filter by latest year
    # this has to be outside of above filter or it takes max year of entire dataset
    # this could try and filter for a year thats not in the resulting filters
    filter(
      year == max(year)
    ) %>%
    # cast to numeric to ensure it plays nice
    mutate(value = as.double(value),
           country = COS_NAME) %>%
    select(IndicatorName, value, year, country)
  
  
  
  
  
  
  chart_dataCOMP <- ISORA %>%
    
    select(country, CountryCode, IndicatorName, indicatorCode, Attribute, isora_surveyYear, year, value) %>%
    
    # filter for data needed for chart
    #filter(dataset == "ISORA") %>%
    mutate(indicator = as.character(indicatorCode))%>%
    
    # trim dataset to needed countries and indicators
    filter(
      indicator %in% c("88140_37", "88140_40", "88140_38", "88140_39"),
      # remove countries not in comparable group
      CountryCode %in% COMP_DF$WEO_Code,
      # remove values that are missing, noted with a D
      value != 'D',
      # odd isora stuff to remove, data is is value attribute 
      Attribute != 'Observation',
      
    ) %>%
    
    # filter by latest year
    # this has to be outside of above filter or it takes max year of entire dataset
    # this could try and filter for a year thats not in the resulting filters
    filter(
      year == max(year)
    ) %>%
    
    # cast to numeric to ensure it plays nice
    mutate(value = as.double(value)) %>%
    
    
    # group by gender and calculate averages
    group_by(indicator) %>%
    mutate(value = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    
    # include country/group label
    mutate(country = COMP_VALUE) %>%
    select(IndicatorName, value, year, country)%>%
    unique() 
  
  
  
  
  # combine group and country of study to chart
  c <- rbind(chart_data, chart_dataCOMP)
  
  
  plot <- c %>%
    
    ggplot(aes(x = reorder(IndicatorName,  value), y = value/100, fill = country)) +
    
    geom_bar(position = "dodge", stat = "identity") + 
    
    ggtitle(paste(COS_NAME, "On Time Filing Rates")) +
    
    labs(subtitle = paste("percent of ,", unique(c$year)), 
         y = " ",
         x = " ",
         caption = "Source: Random Data") +
    
    ISORA_staffing() +
    
    #blues <- c("#1B4F72","#2874A6","#3498DB", "#85C1E9","#5DADE2","#AED6F1","#D6EAF8")
    
    
    scale_fill_manual(values = c("#1B4F72","#2874A6")) +
    
    coord_flip() + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  plot
}

## ----20_WB-doing business, echo=FALSE, fig.align="center", message=FALSE, warning=FALSE----


doingBizChart <- function(doingBiz, COS_ISO3, COS_NAME, COMP_VALUE, COMP_DF, indicator, indicatorRename, subtitle=""){
  
  tryCatch(
    expr = make_doingBiz(doingBiz, COS_ISO3, COS_NAME, COMP_VALUE, COMP_DF, indicator, indicatorRename, subtitle),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 8,
                 label = paste("Failed to make World Bank Doing Business\n", indicatorRename)) + 
        theme_void()
    }
  )
  
  
}


make_doingBiz <- function(doingBiz, COS_ISO3, COS_NAME, COMP_VALUE, COMP_DF, indicator, indicatorRename, subtitle){
  
  chartData <- doingBiz %>%
    
    filter(
      countryCode == COS_ISO3,
      name == indicator
    ) %>%
    
    mutate(name = ifelse(name == indicator, indicatorRename, "Error")) %>%
    
    select(countryname, dates, value, name)
  
  chartDataComp <- doingBiz %>%
    
    filter(
      countryCode %in% COMP_DF$WEO_ISO_3,
      name == indicator
    ) %>%
    
    select(countryname, dates, value, name) %>%
    
    group_by(name, dates) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    
    mutate(name = ifelse(name == indicator, indicatorRename, "Error"),
           countryname = COMP_VALUE) %>%
    
    select(countryname, dates, value, name)
  
  c <- rbind(chartData, chartDataComp) %>%
    filter(!is.na(value),
           value != 0) %>%
    mutate(dates = year(dates))
  
  line <- c %>%
    
    ggplot(aes(x = dates, y = value, color = countryname)) + 
    
    # line 1, Country of Study  
    geom_line(size = 1) + 
    
    # Title and caption 
    labs(title = paste(indicatorRename),
         subtitle = subtitle,
         y = "",
         x = "",
         caption = "Source: Random Data") + 
    # set y axis height 
    expand_limits(y = 0) +
    
    scale_x_continuous(breaks = seq(min(c$dates), max(c$dates), by = 1)) +
    
    # styling 
    scale_color_manual(values = c("#1B4F72","#2874A6"))+
    #theme_piotr_edited() 
    ISORA_staffing()
  
  
  line
}




## ---- RAGAP ----


ragapTable <- function(path, COMP_DF) {
  
  
  ragapTable <- read.xlsx(file=path, sheetName = "reports")
  
  
  
  ragapTable <- ragapTable %>%
    filter(Country.Code %in% COMP_DF$WEO_ISO_3) %>%
    select(Country.Name, Date.Completed, Report.Classification, External.Classification, Years.Covered, Lead, Tax) %>%
    arrange(Date.Completed)
  
  colnames(ragapTable) <- c("Country", "Date Completed", "Classification", "External", "Years Covered", "Lead", "Tax Type")
  
  
  kbl(ragapTable, 
      # make pdf table
      format="latex",
      # fancy headers
      booktabs=TRUE,
      # if this isn't set every 5 rows has an extra space
      linesep = "",
      # let table go over page if too big
      longtable = T)%>%
    
    
    kable_styling(font_size = 8,
                  latex_options = c("striped", "repeat_header"),
                  position = "center") %>%
    # font blake  
    row_spec(1:length(ragapTable), color = "black") %>%
    
    # make columns certain widths so the text looks nice
    # latex valign doesn't work with bookends sadly
    column_spec(c(0,dim(ragapTable)[2]), width_min = "0.75in", latex_valign = "m") %>%
    column_spec(c(1, 3, 4), width = "1in")
  
}


## ---- TADAT ---- 


make_tadat_table <- function(tadat, COS_NAME, COS_ISO3){
  
  if(!(COS_ISO3 %in% tadat$iso3c)){
    stop()
  }
  
  chartData <- tadat %>%
    filter(iso3c == COS_ISO3) %>%
    mutate(pivotCol = paste(country, region, year)) %>%
    select(pivotCol, description, rating) %>%
    pivot_wider(names_from = pivotCol, values_from = rating)
  
  
  colnames(chartData)[1] <- c("Indicator")
  
  kbl(chartData, 
      # fancy headers
      booktabs=TRUE,
      # if this isn't set every 5 rows has an extra space
      linesep = "",
      # let table go over page if too big
      longtable = T)%>%
    
    
    kable_styling(font_size = 8,
                  latex_options = c("striped", "repeat_header"),
                  position = "center")
}

tadat_table <- function(tadat, COS_NAME, COS_ISO3){
  
  tryCatch(
    expr = make_tadat_table(tadat, COS_NAME, COS_ISO3),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 3,
                 label = paste("Failed to make Tadat Table")) + 
        theme_void()
    }
  )
}




## ---- Duties ----

make_wto_duties_line_chart <- function(duty_data, COS_ISO3, title, subtitle, indicator_list) {
  
  
  chart_data <- duty_data %>%
    
    filter(original_indicator_code %in% indicator_list,
           iso3A == COS_ISO3)
  
  chart_data$Indicator <- factor(chart_data$Indicator,
                                 levels = c("> 100", "50 <= 100", "25 <= 50", "15 <= 25", "10 <= 15", "5 <= 10", "0 <= 5"))
  
  
  chart_data %>%
    ggplot(aes(x = Year, y = Value/100, color = Indicator)) + 
    
    # line 1, Country of Study  
    geom_line(size = 0.75) +
    
    # Title and caption 
    labs(title = paste(title),
         subtitle = paste(subtitle),
         y = "",
         x = "",
         caption = "Source: Random Data") + 
    
    # set y axis height 
    expand_limits(y = 0) +
    
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    
    # styling 
    #scale_color_manual(values = c()) +
    
    #theme_piotr_edited() 
    ISORA_staffing()
  
}



wto_duties_line_chart <- function(duty_data, COS_ISO3, title, subtitle, indicator_list){
  
  tryCatch(
    expr = make_wto_duties_line_chart(duty_data, COS_ISO3, title, subtitle, indicator_list),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 3,
                 label = paste("Failed to make Duty Chart")) + 
        theme_void()
    }
  )
  
  
}

## ---- Orbis CIT RDF ----


make_total_rev_line <- function(orbis_data, COS_NAME){
  
  total_rev_line <- orbis_data %>%
    group_by(Closing_date) %>%
    summarize(Revenue = sum(Sales)) %>%
    ggplot(aes(x=as.POSIXct(Closing_date), y=Revenue/1000000)) +
    geom_line() +
    
    # line 1, Country of Study  
    geom_line(size = 1) + 
    
    # Title and caption 
    labs(title = paste("Total Revenue of ", COS_NAME, " in Orbis", sep=""),
         subtitle = "billions, USD",
         y = "",
         x = "",
         caption = "Source: Random Data") + 
    # set y axis height 
    expand_limits(y = 0) +
    
    scale_x_datetime(breaks = "2 year", date_labels = "%Y") +
    
    # styling 
    scale_color_manual(values = c("#1B4F72")) +
    
    ISORA_staffing()
  
  total_rev_line
}

total_rev_line <- function(orbis_data, COS_NAME){
  
  tryCatch(
    expr = make_total_rev_line(orbis_data, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 2,
                 label = "Failed to make Orbis Revenue chart") + 
        theme_void()
    }
  )
}

make_total_taxes_paid_line <- function(orbis_data, COS_NAME){
  
  total_taxes_paid_line <- orbis_data %>%
    filter(P_L_before_tax > 0) %>%
    group_by(Closing_date) %>%
    summarize(Taxes = sum(Taxation)) %>%
    ggplot(aes(x=as.POSIXct(Closing_date), y=Taxes/1000000)) +
    geom_line() +
    
    # line 1, Country of Study  
    geom_line(size = 1.25) + 
    
    # Title and caption 
   labs(title = paste("Total Taxes Paid ", COS_NAME, " in Orbis", sep=""),
   subtitle = "billions, USD",
   y = "",
   x = "",
   caption = "Source: Random Data") + 
    # set y axis height 
    expand_limits(y = 0) +
    
    scale_x_datetime(breaks = "2 year", date_labels = "%Y") +
    
    # styling 
    scale_color_manual(values = c("#1B4F72")) +
    
    ISORA_staffing()
  
  total_taxes_paid_line
}

total_taxes_paid_line <- function(orbis_data, COS_NAME){
  
  tryCatch(
    expr = make_total_taxes_paid_line(orbis_data, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 2,
                 label = "Failed to make Orbis taxes paid chart") + 
        theme_void()
    }
  )
}

make_avg_e_rate_line <- function(orbis_data, COS_NAME){
  
  avg_e_rate_line <- orbis_data %>%
    filter(P_L_before_tax > 0) %>%
    group_by(Closing_date) %>%
    summarize(effective_rate = median(sum(Taxation) / sum(P_L_before_tax)))%>%
    
    ggplot(aes(x=as.POSIXct(Closing_date), y=effective_rate)) +
    geom_line() +
    
    # line 1, Country of Study  
    geom_line(size = 1) + 
    
    # Title and caption 
    labs(title = paste("Average Effective Rate: ", COS_NAME, sep=""),
         subtitle = "",
         y = "",
         x = "",
         caption = "Source: Random Data") + 
    # set y axis height 
    expand_limits(y = 0) +
    
    scale_x_datetime(breaks = "2 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::percent) +
    
    # styling 
    scale_color_manual(values = c("#1B4F72")) +
    
    ISORA_staffing()
  
  avg_e_rate_line
}

avg_e_rate_line <- function(orbis_data, COS_NAME){
  
  tryCatch(
    expr = make_avg_e_rate_line(orbis_data, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 2,
                 label = "Failed to make Orbis taxes paid chart") + 
        theme_void()
    }
  )
}


make_count_firms_line <- function(orbis_data, COS_NAME){
  
  count_firms_line <- orbis_data %>%
    filter(P_L_before_tax > 0) %>%
    group_by(Closing_date) %>%
    count(Closing_date)%>%
    
    
    ggplot(aes(x=as.POSIXct(Closing_date), y=n)) +
    geom_line() +
    
    # line 1, Country of Study  
    geom_line(size = 1) + 
    
    # Title and caption 
    labs(title = paste("Count of firms: ", COS_NAME, sep=""),
         subtitle = "",
         y = "",
         x = "",
         caption = "Source: Random Data") + 
    # set y axis height 
    expand_limits(y = 0) +
    
    scale_x_datetime(breaks = "2 year", date_labels = "%Y") +
    #scale_y_continuous(labels = scales::percent) +
    
    # styling 
    scale_color_manual(values = c("#1B4F72")) +
    
    ISORA_staffing()
  
  count_firms_line
}

count_firms_line <- function(orbis_data, COS_NAME){
  
  tryCatch(
    expr = make_count_firms_line(orbis_data, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 2,
                 label = "Failed to make Orbis taxes paid chart") + 
        theme_void()
    }
  )
}

make_rev_histo <- function(orbis_data, COS_NAME){
  
  rev_histo<- orbis_data %>%
    filter(P_L_before_tax > 0) %>%
    group_by(BvD_ID_number, Closing_date) %>%
    summarize(revenue = sum(Sales))%>%
    ggplot(aes(x=revenue/1000000)) +
    geom_histogram(fill = "#69b3a2") +
    
    # Title and caption 
    labs(title = paste("Distribution of Revenue: ", COS_NAME, sep=""),
         subtitle = "in billions USD, pooled by firms for all years",
         y = "",
         x = "",
         caption = "Source: Random Data") + 
    
    ISORA_staffing()
  
  rev_histo
}

rev_histo <- function(orbis_data, COS_NAME){
  
  tryCatch(
    expr = make_rev_histo(orbis_data, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 2,
                 label = "Failed to make Orbis taxes paid chart") + 
        theme_void()
    }
  )
}


make_taxes_histo <- function(orbis_data, COS_NAME){
  
  taxes_histo <- data %>%
    filter(P_L_before_tax > 0) %>%
    group_by(BvD_ID_number, Closing_date) %>%
    summarize(taxes = sum(Taxation))%>%
    ggplot(aes(x=taxes/1000000)) +
    geom_histogram(fill = "#69b3a2") +
    
    # Title and caption 
    labs(title = paste("Distribution of Taxes Paid: ", COS_NAME, sep=""),
         subtitle = "in billions USD, pooled by firms for all years",
         y = "",
         x = "",
         caption = "Source: Random Data") + 
    
    ISORA_staffing()
  
  taxes_histo
}

taxes_histo <- function(orbis_data, COS_NAME){
  
  tryCatch(
    expr = make_taxes_histo(orbis_data, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 2,
                 label = "Failed to make Orbis taxes paid chart") + 
        theme_void()
    }
  )
}

make_pNl_histo <- function(orbis_data, COS_NAME){
  
  pNl_histo <- data %>%
    filter(P_L_before_tax > 0) %>%
    group_by(BvD_ID_number, Closing_date) %>%
    summarize(taxes = sum(P_L_before_tax))%>%
    ggplot(aes(x=taxes/1000000)) +
    geom_histogram(fill = "#69b3a2") +
    
    # Title and caption 
    labs(title = "Distribution of Profit-Loss before Tax",
         subtitle = "in billions USD, pooled by firms for all years",
         y = "",
         x = "",
         caption = "Source: Random Data") + 
    
    ISORA_staffing()
}

pNl_histo <- function(orbis_data, COS_NAME){
  
  tryCatch(
    expr = make_pNl_histo(orbis_data, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 2,
                 label = "Failed to make Orbis taxes paid chart") + 
        theme_void()
    }
  )
}


make_ef_rate_histo <- function(orbis_data, COS_NAME){
  
  ef_rate_data <- data %>%
    filter(P_L_before_tax > 0,
           Taxation > 0) %>%
    group_by(BvD_ID_number, Closing_date) %>%
    mutate(effective_rate = sum(Taxation) / sum(P_L_before_tax)) %>%
    filter(effective_rate < 1)
  
  ef_rate_data %>%
    ggplot(aes(x=effective_rate)) +
    geom_histogram(fill = "#69b3a2") +
    
    # Title and caption 
    labs(title = "Distribution of Effective Rate",
         subtitle = "pooled by firms for all years - Taxes Paid / PnL Before Tax",
         y = "",
         x = "",
         caption = "Source: Random Data") +
    scale_x_continuous(breaks = seq(0, 1, by = .1), labels = scales::percent_format(accuracy = 1)) + 
    #scale_x_continuous(labels = scales::percent) +
    
    ISORA_staffing()
}

ef_rate_histo <- function(orbis_data, COS_NAME){
  
  tryCatch(
    expr = make_ef_rate_histo(orbis_data, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 2,
                 label = "Failed to make Orbis taxes paid chart") + 
        theme_void()
    }
  )
}


make_rdf <- function(orbis_data, COS_NAME){
  
  rdf_data <- data %>%
    mutate(rev_rank = rank(-Sales),
           rev_score = 1 - (rev_rank / max(rev_rank)),
           effective_rate = (Taxation / Sales),
           effective_rate_rank = rank(effective_rate),
           e_rate_score = 1 - (effective_rate_rank / max(effective_rate_rank)),
           
           quadrant = ifelse(rev_score >= .5 & e_rate_score >= .5, "High Risk", 
                             ifelse(rev_score > .5 & e_rate_score < .5, "Key",
                                    ifelse(rev_score < .5 & e_rate_score < .5, "Medium Risk", 
                                           ifelse(rev_score <= .5 & e_rate_score >= .5, "Lower", "Error")))))
  
  
  rdf_quad <-  rdf_data %>%
    ggplot(aes(x=rev_score*100, y=e_rate_score*100)) +
    geom_point() +
    
    # Title and caption 
    labs(title = "Risk Differentiation Framework",
         subtitle = "",
         y = "Financial Importance",
         x = "Risk Assessment",
         caption = "Source: Random Data") +
    
    lims(x=c(0,100),y=c(1,100)) +
    theme_piotr_edited() +
    coord_fixed() + 
    geom_vline(xintercept = 50) + geom_hline(yintercept = 50) +
    
    annotate("text", x = 100, y = 100, label = "1", color = "red") +
    annotate("text", x = 0, y = 100, label = "2", color = "red") +
    annotate("text", x = 0, y = 45, label = "3", color = "red") +
    annotate("text", x = 100, y = 45, label = "4", color = "red") 
  
  rdf_quad
  
}

rdf <- function(orbis_data, COS_NAME){
  
  tryCatch(
    expr = make_rdf(orbis_data, COS_NAME),
    
    error = function(e){
      ggplot() +                      
        annotate("text",
                 x = 1,
                 y = 1,
                 size = 2,
                 label = "Failed to make Orbis taxes paid chart") + 
        theme_void()
    }
  )
}


## ----Chart Themes ---------------- 

# Credit to Piotr Nghia Le NLe2@imf.org  for much of the theme code

# https://ggplot2.tidyverse.org/reference/theme.html 




#  https://ggplot2.tidyverse.org/reference/theme.html 
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour 



ISORA_staffing <- function(base_size = 12, base_family = "URWBookman") {
  
  theme(
    
    line = element_line(colour = "#888888", 
                        size = 0.25, 
                        linetype = 1L, 
                        lineend = "butt"), 
    rect = element_rect(fill = "#FFFFFF", 
                        colour = "#000000", 
                        size = 0.25, 
                        linetype = 1L), 
    text = element_text(family = base_family, 
                        face = "plain", 
                        colour = "#000000", 
                        size = base_size, 
                        hjust = 0.5, 
                        vjust = 0.5, 
                        angle = 0, 
                        lineheight = 0.9, 
                        margin = margin(),
                        debug = FALSE),
    
    ## Plot Attributes
    
    plot.title = element_text(size = 8L,
                              face = "bold",
                              hjust = 0,
                              margin = margin(t = 10, b = 5, r = 1, l = 1, unit = "pt")),
    
    plot.subtitle = element_text(size = 6L,
                                 hjust = 0,
                                 margin = margin(t = 0, b = 10, r = 1, l = 1, unit = "pt")),
    
    plot.caption = element_text(size = 5L,
                                hjust = 0.8,
                                vjust = 1,
                                margin = margin(t = base_size / 1.5)),
    plot.background = NULL, 
    
    # t, r, b, l
    plot.margin = (unit(c(0,0,0,0),"cm")), 
    
    
    ## Axis Attributes
    
    axis.text = element_text(size = 6),
    axis.text.x = element_text(margin = margin(t = 4)),
    axis.text.y = element_text(margin = margin(t = 4),
                               hjust = 1), 
    axis.text.x.top = NULL, 
    axis.text.y.right = NULL, 
    
    axis.ticks = element_line(), 
    axis.title = element_text(size = 12L), 
    axis.title.x = element_text(margin = margin(t = 8)), 
    axis.title.y = element_text(angle = 90,
                                margin = margin(r = 4)),
    axis.title.x.top = NULL, 
    axis.title.y.right = NULL,
    
    axis.ticks.length = unit(4L, "pt"),
    axis.ticks.x = element_line(colour = NULL, 
                                size = NULL, 
                                linetype = NULL, 
                                lineend = NULL), 
    axis.ticks.y = element_blank(), 
    
    axis.line = element_line(), 
    axis.line.x = element_line(), 
    axis.line.y = element_line(), 
    
    ## Legend Attributes
    
    legend.background = element_blank(), 
    
    legend.spacing = unit(14L, "pt"), 
    legend.spacing.x = unit(5L, "pt"), 
    legend.spacing.y = NULL,
    
    legend.key = element_blank(), 
    legend.key.size = unit(10L, "pt"), 
    legend.key.height = NULL, 
    legend.key.width = NULL, 
    
    legend.text = element_text(size= 6), 
    legend.text.align = NULL, 
    legend.title = element_blank(), 
    legend.title.align = NULL, 
    
    # top, bottom, left, right are all allowed
    legend.position = "bottom", 
    legend.direction = "horizontal", 
    legend.justification = NULL, 
    legend.margin = margin(t = .5, r = 0, b = .5, l = 0, "pt"), 
    
    legend.box = "horizontal", 
    legend.box.margin = NULL, 
    legend.box.background = NULL, 
    legend.box.spacing = NULL, 
    
    ## Panel Attributes
    
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.ontop = FALSE, 
    
    panel.spacing = unit(6L, "pt"),
    panel.spacing.x = NULL, 
    panel.spacing.y = NULL, 
    
    panel.grid.major = element_line(), 
    panel.grid.major.x = element_line(colour = "#f0f0f0"), 
    panel.grid.major.y = element_line(colour = "#f0f0f0"), 
    panel.grid.minor = element_line(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    
    strip.background = element_rect(fill = "#aeaeae", 
                                    colour = NA,
                                    size = 8), 
    strip.text = element_text(face = "bold", 
                              size = rel(0.75)),
    
    strip.text.x = element_text(margin = margin(t = 4.5, b = 0.5)), 
    strip.text.y = element_text(angle = -90, 
                                margin = margin(l = 4.5, r = 4.5)), 
    
    strip.placement = "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    
    strip.switch.pad.grid = unit(0.1, "cm"), 
    strip.switch.pad.wrap = unit(0.1, "cm"), 
    ## Create a 'complete' format
    complete = TRUE
  )  
}


theme_chart_01 <- function(base_size = 12, base_family = "URWBookman") {
  
  theme(
    
    line = element_line(colour = "#888888", 
                        size = 0.25, 
                        linetype = 1L, 
                        lineend = "butt"), 
    rect = element_rect(fill = "#FFFFFF", 
                        colour = "#000000", 
                        size = 0.25, 
                        linetype = 1L), 
    text = element_text(family = base_family, 
                        face = "plain", 
                        colour = "#000000", 
                        size = base_size, 
                        hjust = 0.5, 
                        vjust = 0.5, 
                        angle = 0, 
                        lineheight = 0.9, 
                        margin = margin(),
                        debug = FALSE),
    
    ## Plot Attributes
    
    plot.title = element_text(size = 8L,
                              face = "bold",
                              hjust = 0,
                              margin = margin(t = 10, b = 5, r = 1, l = 1, unit = "pt")),
    
    plot.subtitle = element_text(size = 6L,
                                 hjust = 0,
                                 margin = margin(t = 0, b = 10, r = 1, l = 1, unit = "pt")),
    
    plot.caption = element_text(size = 5L,
                                hjust = 0.8,
                                vjust = 1,
                                margin = margin(t = base_size / 1.5)),
    plot.background = NULL, 
    
    # t, r, b, l
    plot.margin = (unit(c(0,0,0,0),"cm")), 
    
    
    ## Axis Attributes
    
    axis.text = element_text(size = 6),
    axis.text.x = element_text(margin = margin(t = 4)),
    axis.text.y = element_text(margin = margin(t = 4),
                               hjust = 1), 
    axis.text.x.top = NULL, 
    axis.text.y.right = NULL, 
    
    axis.ticks = element_line(), 
    axis.title = element_text(size = 6L), 
    axis.title.x = element_text(margin = margin(t = 8)), 
    axis.title.y = element_text(angle = 90,
                                margin = margin(r = 4)),
    axis.title.x.top = NULL, 
    axis.title.y.right = NULL,
    
    axis.ticks.length = unit(4L, "pt"),
    axis.ticks.x = element_line(colour = NULL, 
                                size = NULL, 
                                linetype = NULL, 
                                lineend = NULL), 
    axis.ticks.y = element_blank(), 
    
    axis.line = element_line(), 
    axis.line.x = element_line(), 
    axis.line.y = element_line(), 
    
    ## Legend Attributes
    
    legend.background = element_blank(), 
    
    legend.spacing = unit(14L, "pt"), 
    legend.spacing.x = unit(5L, "pt"), 
    legend.spacing.y = NULL,
    
    legend.key = element_blank(), 
    legend.key.size = unit(10L, "pt"), 
    legend.key.height = NULL, 
    legend.key.width = NULL, 
    
    legend.text = element_text(size= 6), 
    legend.text.align = NULL, 
    legend.title = element_blank(), 
    legend.title.align = NULL, 
    
    # top, bottom, left, right are all allowed, none
    legend.position = "none", 
    legend.direction = "horizontal", 
    legend.justification = NULL, 
    legend.margin = margin(t = 6, r = 0, b = 6, l = 0, "pt"), 
    
    legend.box = "horizontal", 
    legend.box.margin = NULL, 
    legend.box.background = NULL, 
    legend.box.spacing = NULL, 
    
    ## Panel Attributes
    
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.ontop = FALSE, 
    
    panel.spacing = unit(6L, "pt"),
    panel.spacing.x = NULL, 
    panel.spacing.y = NULL, 
    
    panel.grid.major = element_line(), 
    panel.grid.major.x = element_line(colour = "#f0f0f0"), 
    panel.grid.major.y = element_line(colour = "#f0f0f0"), 
    panel.grid.minor = element_line(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    
    strip.background = element_rect(fill = "#aeaeae", 
                                    colour = NA,
                                    size = 8), 
    strip.text = element_text(face = "bold", 
                              size = rel(0.75)),
    
    strip.text.x = element_text(margin = margin(t = 4.5, b = 4.5)), 
    strip.text.y = element_text(angle = -90, 
                                margin = margin(l = 4.5, r = 4.5)), 
    
    strip.placement = "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    
    strip.switch.pad.grid = unit(0.1, "cm"), 
    strip.switch.pad.wrap = unit(0.1, "cm"), 
    ## Create a 'complete' format
    complete = TRUE
  )  
}





theme_piotr_edited <- function(base_size = 12, base_family = "URWBookman") {
  
  theme(
    
    line = element_line(colour = "#888888", 
                        size = 0.25, 
                        linetype = 1L, 
                        lineend = "butt"), 
    rect = element_rect(fill = "#FFFFFF", 
                        colour = "#000000", 
                        size = 0.25, 
                        linetype = 1L), 
    text = element_text(family = base_family, 
                        face = "plain", 
                        colour = "#000000", 
                        size = base_size, 
                        hjust = 0.5, 
                        vjust = 0.5, 
                        angle = 0, 
                        lineheight = 0.9, 
                        margin = margin(),
                        debug = FALSE),
    
    ## Plot Attributes
    
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0,
                              margin = margin(b = 4)), 
    plot.subtitle = element_text(size = 12L,
                                 hjust = 0,
                                 margin = margin(b = 5)),
    
    plot.caption = element_text(size = 8L,
                                hjust = 1,
                                vjust = 1,
                                margin = margin(t = base_size / 2)),
    plot.background = NULL, 
    
    
    ## Axis Attributes
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(margin = margin(t = 4)),
    axis.text.y = element_text(margin = margin(t = 4),
                               hjust = 1), 
    axis.text.x.top = NULL, 
    axis.text.y.right = NULL, 
    
    axis.ticks = element_line(), 
    axis.title = element_text(size = 12L), 
    axis.title.x = element_text(margin = margin(t = 8)), 
    axis.title.y = element_text(angle = 90,
                                margin = margin(r = 4)),
    axis.title.x.top = NULL, 
    axis.title.y.right = NULL,
    
    axis.ticks.length = unit(4L, "pt"),
    axis.ticks.x = element_line(colour = NULL, 
                                size = NULL, 
                                linetype = NULL, 
                                lineend = NULL), 
    axis.ticks.y = element_blank(), 
    
    axis.line = element_line(), 
    axis.line.x = element_line(), 
    axis.line.y = element_line(), 
    
    ## Legend Attributes
    
    legend.background = element_blank(), 
    
    legend.spacing = unit(14L, "pt"), 
    legend.spacing.x = unit(5L, "pt"), 
    legend.spacing.y = NULL,
    
    legend.key = element_blank(), 
    legend.key.size = unit(10L, "pt"), 
    legend.key.height = NULL, 
    legend.key.width = NULL, 
    
    legend.text = NULL, 
    legend.text.align = NULL, 
    legend.title = element_blank(), 
    legend.title.align = NULL, 
    
    # top, bottom, left, right are all allowed
    legend.position = "bottom", 
    legend.direction = "horizontal", 
    legend.justification = NULL, 
    legend.margin = margin(t = 6, r = 0, b = 6, l = 0, "pt"), 
    
    legend.box = "horizontal", 
    legend.box.margin = NULL, 
    legend.box.background = NULL, 
    legend.box.spacing = NULL, 
    
    ## Panel Attributes
    
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.ontop = FALSE, 
    
    panel.spacing = unit(6L, "pt"),
    panel.spacing.x = NULL, 
    panel.spacing.y = NULL, 
    
    panel.grid.major = element_line(), 
    panel.grid.major.x = element_line(colour = "#f0f0f0"), 
    panel.grid.major.y = element_line(colour = "#f0f0f0"), 
    panel.grid.minor = element_line(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    
    strip.background = element_rect(fill = "#aeaeae", 
                                    colour = NA,
                                    size = 8), 
    strip.text = element_text(face = "bold", 
                              size = rel(0.75)),
    
    strip.text.x = element_text(margin = margin(t = 4.5, b = 4.5)), 
    strip.text.y = element_text(angle = -90, 
                                margin = margin(l = 4.5, r = 4.5)), 
    
    strip.placement = "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    
    strip.switch.pad.grid = unit(0.1, "cm"), 
    strip.switch.pad.wrap = unit(0.1, "cm"), 
    ## Create a 'complete' format
    complete = TRUE
  )  
}

update_geom_defaults("bar", list(fill = "#003262"))
update_geom_defaults("point", list(colour = "#003262"))
update_geom_defaults("line", list(colour = "#003262"))
update_geom_defaults("boxplot", list(colour = "#003262"))
update_geom_defaults("text", list(colour = "#FDB515"))

theme_chart_05 <- function(base_size = 12, base_family = "URWBookman") {
  
  theme(
    
    line = element_line(colour = "#888888", 
                        size = 0.25, 
                        linetype = 1L, 
                        lineend = "butt"), 
    rect = element_rect(fill = "#FFFFFF", 
                        colour = "#000000", 
                        size = 0.25, 
                        linetype = 1L), 
    text = element_text(family = base_family, 
                        face = "plain", 
                        colour = "#000000", 
                        size = base_size, 
                        hjust = 0.5, 
                        vjust = 0.5, 
                        angle = 0, 
                        lineheight = 0.9, 
                        margin = margin(),
                        debug = FALSE),
    
    ## Plot Attributes
    
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0,
                              margin = margin(b = 4)), 
    plot.subtitle = element_text(size = 12L,
                                 hjust = 0,
                                 margin = margin(b = 5)),
    
    plot.caption = element_text(size = 8L,
                                hjust = 1,
                                vjust = -3,
                                margin = margin(t = base_size )),
    plot.background = NULL, 
    
    # top, right, bottom, left
    plot.margin = unit(c(0.75 ,0.3, 0.65, 0), "cm"),
    
    ## Axis Attributes
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(margin = margin(t = 4)),
    axis.text.y = element_text(margin = margin(t = 4),
                               hjust = 1), 
    axis.text.x.top = NULL, 
    axis.text.y.right = NULL, 
    
    axis.ticks = element_line(), 
    axis.title = element_text(size = 12L), 
    axis.title.x = element_text(margin = margin(t = 8)), 
    axis.title.y = element_text(angle = 90,
                                margin = margin(r = 4)),
    axis.title.x.top = NULL, 
    axis.title.y.right = NULL,
    
    axis.ticks.length = unit(4L, "pt"),
    axis.ticks.x = element_line(colour = NULL, 
                                size = NULL, 
                                linetype = NULL, 
                                lineend = NULL), 
    axis.ticks.y = element_blank(), 
    
    axis.line = element_line(), 
    axis.line.x = element_line(), 
    axis.line.y = element_line(), 
    
    ## Legend Attributes
    
    legend.background = element_blank(), 
    
    legend.spacing = unit(10L, "pt"), 
    legend.spacing.x = NULL, 
    legend.spacing.y = NULL,
    
    legend.key = element_blank(), 
    legend.key.size = unit(10L, "pt"), 
    legend.key.height = NULL, 
    legend.key.width = NULL, 
    
    legend.text = element_text(size= 8), 
    legend.text.align = NULL, 
    legend.title = element_blank(), 
    legend.title.align = NULL, 
    
    # top, bottom, left, right are all allowed
    legend.position = c(.45,-0.2), 
    legend.direction = "horizontal", 
    legend.justification = NULL, 
    legend.margin = margin(t = 6, r = 2, b = 6, l = 2, "pt"), 
    
    legend.box = "horizontal", 
    legend.box.margin = NULL, 
    legend.box.background = NULL, 
    legend.box.spacing = NULL, 
    
    ## Panel Attributes
    
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.ontop = FALSE, 
    
    panel.spacing = unit(2L, "pt"),
    panel.spacing.x = NULL, 
    panel.spacing.y = NULL, 
    
    panel.grid.major = element_line(), 
    panel.grid.major.x = element_line(colour = "#f0f0f0"), 
    panel.grid.major.y = element_line(colour = "#f0f0f0"), 
    panel.grid.minor = element_line(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    
    strip.background = element_rect(fill = "#aeaeae", 
                                    colour = NA,
                                    size = 8), 
    strip.text = element_text(face = "bold", 
                              size = rel(0.75)),
    
    strip.text.x = element_text(margin = margin(t = 4.5, b = 4.5)), 
    strip.text.y = element_text(angle = -90, 
                                margin = margin(l = 4.5, r = 4.5)), 
    
    strip.placement = "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    
    strip.switch.pad.grid = unit(0.1, "cm"), 
    strip.switch.pad.wrap = unit(0.1, "cm"), 
    ## Create a 'complete' format
    complete = TRUE
  )  
}


theme_chart_14 <- function(base_size = 12, base_family = "URWBookman") {
  
  theme(
    
    line = element_line(colour = "#888888", 
                        size = 0.25, 
                        linetype = 1L, 
                        lineend = "butt"), 
    rect = element_rect(fill = "#FFFFFF", 
                        colour = "#000000", 
                        size = 0.25, 
                        linetype = 1L), 
    text = element_text(family = base_family, 
                        face = "plain", 
                        colour = "#000000", 
                        size = base_size, 
                        hjust = 0.5, 
                        vjust = 0.5, 
                        angle = 0, 
                        lineheight = 0.9, 
                        margin = margin(),
                        debug = FALSE),
    
    ## Plot Attributes
    
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0,
                              margin = margin(b = 4)), 
    plot.subtitle = element_text(size = 12L,
                                 hjust = 0,
                                 margin = margin(b = 5)),
    
    plot.caption = element_text(size = 8L,
                                hjust = 1,
                                vjust = 1,
                                margin = margin(t = base_size / 2)),
    plot.background = NULL, 
    
    
    ## Axis Attributes
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(margin = margin(t = 4)),
    axis.text.y = element_text(margin = margin(t = 4),
                               hjust = 1), 
    axis.text.x.top = NULL, 
    axis.text.y.right = element_text(color="steelblue"), 
    
    axis.title.y.left=element_text(color="red"),
    axis.text.y.left=element_text(color="red"),
    
    axis.ticks = element_line(), 
    axis.title = element_text(size = 12L), 
    axis.title.x = element_text(margin = margin(t = 8)), 
    axis.title.y = element_text(angle = 90,
                                margin = margin(r = 4)),
    axis.title.x.top = NULL, 
    axis.title.y.right = element_text(color="steelblue"),
    
    axis.ticks.length = unit(4L, "pt"),
    axis.ticks.x = element_line(colour = NULL, 
                                size = NULL, 
                                linetype = NULL, 
                                lineend = NULL), 
    axis.ticks.y = element_blank(), 
    
    axis.line = element_line(), 
    axis.line.x = element_line(), 
    axis.line.y = element_line(), 
    
    ## Legend Attributes
    
    legend.background = element_blank(), 
    
    legend.spacing = unit(14L, "pt"), 
    legend.spacing.x = unit(8L, "pt"), 
    legend.spacing.y = unit(2L, "pt"),
    
    legend.key = element_blank(), 
    legend.key.size = unit(6L, "pt"), 
    legend.key.height = NULL, 
    legend.key.width = NULL, 
    
    legend.text = NULL, 
    legend.text.align = NULL, 
    legend.title = element_blank(), 
    legend.title.align = NULL, 
    
    # top, bottom, left, right are all allowed
    legend.position = "bottom", 
    legend.direction = "horizontal", 
    legend.justification = NULL, 
    legend.margin = margin(t = 6, r = 0, b = 6, l = 0, "pt"), 
    
    legend.box = "horizontal", 
    legend.box.margin = NULL, 
    legend.box.background = NULL, 
    legend.box.spacing = NULL, 
    
    ## Panel Attributes
    
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.ontop = FALSE, 
    
    panel.spacing = unit(6L, "pt"),
    panel.spacing.x = NULL, 
    panel.spacing.y = NULL, 
    
    panel.grid.major = element_line(), 
    panel.grid.major.x = element_line(colour = "#f0f0f0"), 
    panel.grid.major.y = element_line(colour = "#f0f0f0"), 
    panel.grid.minor = element_line(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    
    strip.background = element_rect(fill = "#aeaeae", 
                                    colour = NA,
                                    size = 8), 
    strip.text = element_text(face = "bold", 
                              size = rel(0.75)),
    
    strip.text.x = element_text(margin = margin(t = 4.5, b = 4.5)), 
    strip.text.y = element_text(angle = -90, 
                                margin = margin(l = 4.5, r = 4.5)), 
    
    strip.placement = "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    
    strip.switch.pad.grid = unit(0.1, "cm"), 
    strip.switch.pad.wrap = unit(0.1, "cm"), 
    ## Create a 'complete' format
    complete = TRUE
  )  
}


percentOnTimeFilingBar_ggtheme <- function(base_size = 12, base_family = "URWBookman") {
  
  theme(
    
    line = element_line(colour = "#888888", 
                        size = 0.25, 
                        linetype = 1L, 
                        lineend = "butt"), 
    rect = element_rect(fill = "#FFFFFF", 
                        colour = "#000000", 
                        size = 0.25, 
                        linetype = 1L), 
    text = element_text(family = base_family, 
                        face = "plain", 
                        colour = "#000000", 
                        size = base_size, 
                        hjust = 0.5, 
                        vjust = 0.5, 
                        angle = 0, 
                        lineheight = 0.9, 
                        margin = margin(),
                        debug = FALSE),
    
    ## Plot Attributes
    
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0,
                              margin = margin(t = 10, b = 5, r = 1, l = 1, unit = "pt")),
    
    plot.subtitle = element_text(size = 12L,
                                 hjust = 0,
                                 margin = margin(t = 0, b = 10, r = 1, l = 1, unit = "pt")),
    
    plot.caption = element_text(size = 10L,
                                hjust = 0.8,
                                vjust = 1,
                                margin = margin(t = base_size / 1.5)),
    plot.background = NULL, 
    
    # t, r, b, l
    plot.margin = (unit(c(0,0,0,0),"cm")), 
    
    
    ## Axis Attributes
    
    axis.text = element_text(size = 10),
    axis.text.x = element_text(margin = margin(t = 4)),
    axis.text.y = element_text(margin = margin(t = 4),
                               hjust = 1), 
    axis.text.x.top = NULL, 
    axis.text.y.right = NULL, 
    
    axis.ticks = element_line(), 
    axis.title = element_text(size = 12L), 
    axis.title.x = element_text(margin = margin(t = 8)), 
    axis.title.y = element_text(angle = 90,
                                margin = margin(r = 4)),
    axis.title.x.top = NULL, 
    axis.title.y.right = NULL,
    
    axis.ticks.length = unit(4L, "pt"),
    axis.ticks.x = element_line(colour = NULL, 
                                size = NULL, 
                                linetype = NULL, 
                                lineend = NULL), 
    axis.ticks.y = element_blank(), 
    
    axis.line = element_line(), 
    axis.line.x = element_line(), 
    axis.line.y = element_line(), 
    
    ## Legend Attributes
    
    legend.background = element_blank(), 
    
    legend.spacing = unit(14L, "pt"), 
    legend.spacing.x = unit(5L, "pt"), 
    legend.spacing.y = NULL,
    
    legend.key = element_blank(), 
    legend.key.size = unit(10L, "pt"), 
    legend.key.height = NULL, 
    legend.key.width = NULL, 
    
    legend.text = element_text(size= 8), 
    legend.text.align = NULL, 
    legend.title = element_blank(), 
    legend.title.align = NULL, 
    
    # top, bottom, left, right are all allowed
    legend.position = "bottom", 
    legend.direction = "horizontal", 
    legend.justification = NULL, 
    legend.margin = margin(t = 6, r = 0, b = 6, l = 0, "pt"), 
    
    legend.box = "horizontal", 
    legend.box.margin = NULL, 
    legend.box.background = NULL, 
    legend.box.spacing = NULL, 
    
    ## Panel Attributes
    
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.ontop = FALSE, 
    
    panel.spacing = unit(6L, "pt"),
    panel.spacing.x = NULL, 
    panel.spacing.y = NULL, 
    
    panel.grid.major = element_line(), 
    panel.grid.major.x = element_line(colour = "#f0f0f0"), 
    panel.grid.major.y = element_line(colour = "#f0f0f0"), 
    panel.grid.minor = element_line(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    
    strip.background = element_rect(fill = "#aeaeae", 
                                    colour = NA,
                                    size = 8), 
    strip.text = element_text(face = "bold", 
                              size = rel(0.75)),
    
    strip.text.x = element_text(margin = margin(t = 4.5, b = 4.5)), 
    strip.text.y = element_text(angle = -90, 
                                margin = margin(l = 4.5, r = 4.5)), 
    
    strip.placement = "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    
    strip.switch.pad.grid = unit(0.1, "cm"), 
    strip.switch.pad.wrap = unit(0.1, "cm"), 
    ## Create a 'complete' format
    complete = TRUE
  )  
}


