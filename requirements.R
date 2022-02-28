
## title: "RA Primer Draft"
## author: "Ryan Yost"
## date: "March, 25, 2020"
## 


## ----libraries and other settings, include=FALSE-------------------------

options(java.parameters = "-Xmx8g" ) # Increase memory size for JAVA to 8 GB


                              ############ checking Java  ##############
  


  # check and set version of Java ; taking the 64bit out of matlab 
    setJavaBitVersion <- function(){
      if (R.Version()$arch == "x86_64") {
        Sys.setenv(JAVA_HOME = "C:/Program Files/MatLab/R2018b/sys/java/jre/win64/jre")
        } 
      else {
      Sys.setenv(JAVA_HOME = "C:/Program Files (x86)/Java/jre8/bin")
        }
      Sys.getenv("JAVA_HOME")
    }
    # Run It
    #setJavaBitVersion()
                            ############ imf_dataTools ##############
    

    # imf datatools stuff, leaving as is 
library(reticulate)
#setwd("C:/R/R-3.6.3/library/imf_datatools")
#sys <- import('sys')
#sys$path[length(sys$path) + 1] <- "."
#install.packages("imf_datatools"), this is the internal API
imf_datatools <- import("imf_datatools")

# data collector
#dataCollector <- import("datacollector")

    
                            ############ Other Packages ##############
    
      ###     Data Cleaning ### 
    # reading and writing xlsx
    library(xlsx)
    
    # reshaping data, uses melt function the make long data
    library(reshape)
    
        #using to make row names a column
    library(dplyr)
    
    # used to work with dates 
    library(lubridate)
    
         ###     Graphic Libraries ### 
    #charts, 
    library(ggplot2)
    library(scales)
    library(gridExtra)
    
    # tables
    library(knitr)
    library(tidyverse)

    library(kableExtra)
    library(tableHTML)
    library(tinytex)


    
