#################
###################### Cleaning 2002-2012 California API Data
###################### R: Models for Clustered
###################### and Correlated Data
###################### NICAR 2019
###################### INSTRUCTOR: Mary Ryan
###################### CREATED 12.1.2018
###################### UPDATED 12.2.2018
##################

#### LOAD PACKAGES ####
#install.packages( 'tidyverse' )
#install.packages( 'readr' )

library( tidyverse, quietly=T )
library( readr )

#### LOAD ORIGINAL DATA FILES ####
recordLayout6 <- read.csv( "originalData_clusteredCorrelatedData19/CaliAPIscore_recordLayout2006.csv", header = F )
recordLayout6 <- recordLayout6[,-1]
recordLayout7 <- read.csv( "originalData_clusteredCorrelatedData19/CaliAPIscore_recordLayout2007.csv", header = F )
recordLayout7 <- recordLayout7[,-1]
recordLayout8 <- read.csv( "originalData_clusteredCorrelatedData19/CaliAPIscore_recordLayout2008.csv", header = F )
recordLayout8 <- recordLayout8[,-1]
recordLayout9 <- read.csv( "originalData_clusteredCorrelatedData19/CaliAPIscore_recordLayout2009.csv", header = F )
recordLayout9 <- recordLayout9[,-1]
recordLayout10 <- read.csv( "originalData_clusteredCorrelatedData19/CaliAPIscore_recordLayout2010.csv", header = F )
recordLayout10 <- recordLayout10[,-1]
recordLayout12 <- read.csv( "originalData_clusteredCorrelatedData19/CaliAPIscore_recordLayout2012.csv", header = F )
recordLayout12 <- recordLayout12[,-1]
colnames(recordLayout6) <-colnames(recordLayout7) <-colnames(recordLayout8) <-colnames(recordLayout9) <-colnames(recordLayout10) <-colnames(recordLayout12) <- c("colName", "dataType", "width", "description")


## files downloaded from 'https://www.cde.ca.gov/ta/ac/ap/apidatafiles.asp' ##
api6 <- read_fwf( file = "originalData_clusteredCorrelatedData19/api06btx.txt",
                  fwf_widths( recordLayout6$width ),
                  cols( .default=col_character() )  )
colnames(api6) <- recordLayout6$colName

api7 <- read_fwf( file = "originalData_clusteredCorrelatedData19/api07btx.txt",
                  fwf_widths( recordLayout7$width ),
                  cols( .default=col_character() )  )
colnames(api7) <- recordLayout7$colName

api8 <- read_fwf( file = "originalData_clusteredCorrelatedData19/api08btx.txt",
                  fwf_widths( recordLayout8$width ),
                  cols( .default=col_character() )  )
colnames(api8) <- recordLayout8$colName

api9 <- read_fwf( file = "originalData_clusteredCorrelatedData19/api09btx.txt",
                  fwf_widths( recordLayout9$width ),
                  cols( .default=col_character() )  )
colnames(api9) <- recordLayout9$colName

api10 <- read_fwf( file = "originalData_clusteredCorrelatedData19/api10btx.txt",
                   fwf_widths( recordLayout10$width ),
                   cols( .default=col_character() ) )
colnames(api10) <- recordLayout10$colName

api11 <- read_fwf( file = "originalData_clusteredCorrelatedData19/api11btx.txt",
                   fwf_widths( recordLayout12$width ),
                   cols( .default=col_character() )  )
colnames(api11) <- recordLayout12$colName
colnames(api11)[12] <- "API11B"

api12 <- read_fwf( file = "originalData_clusteredCorrelatedData19/apib12tx.txt",
                      fwf_widths( recordLayout12$width ),
                   cols( .default=col_character() )  )
colnames(api12) <- recordLayout12$colName

#### FORMAT FILES INTO ONE LARGE FILE ####
colsWant6 <- c("CDS", "RTYPE", "STYPE","SPED","CHARTER","SNAME","DNAME","CNAME",
              "VALID","API06B",
              "AA_NUM", "AI_NUM","AS_NUM","FI_NUM","HI_NUM","PI_NUM","WH_NUM","SD_NUM",
              "EL_NUM","DI_NUM","PCT_AA","PCT_AI","PCT_AS","PCT_FI","PCT_HI","PCT_WH",
              "MEALS","P_EL","P_DI")
api6.trunc <- api6[,colsWant6]
api6.trunc$year <- rep(2006, nrow(api6.trunc))
colnames(api6.trunc)[10] <- "API"

colsWant7 <- c("CDS", "RTYPE", "STYPE","SPED","CHARTER","SNAME","DNAME","CNAME",
              "VALID","API07B",
              "AA_NUM", "AI_NUM","AS_NUM","FI_NUM","HI_NUM","PI_NUM","WH_NUM","SD_NUM",
              "EL_NUM","DI_NUM","PCT_AA","PCT_AI","PCT_AS","PCT_FI","PCT_HI","PCT_WH",
              "MEALS","P_EL","P_DI")
api7.trunc <- api7[,colsWant7]
api7.trunc$year <- rep(2007, nrow(api7.trunc))
colnames(api7.trunc)[10] <- "API"

colsWant8 <- c("CDS", "RTYPE", "STYPE","SPED","CHARTER","SNAME","DNAME","CNAME",
              "VALID","API08B",
              "AA_NUM", "AI_NUM","AS_NUM","FI_NUM","HI_NUM","PI_NUM","WH_NUM","SD_NUM",
              "EL_NUM","DI_NUM","PCT_AA","PCT_AI","PCT_AS","PCT_FI","PCT_HI","PCT_WH",
              "MEALS","P_EL","P_DI")
api8.trunc <- api8[,colsWant8]
api8.trunc$year <- rep(2008, nrow(api8.trunc))
colnames(api8.trunc)[10] <- "API"

colsWant9 <- c("CDS", "RTYPE", "STYPE","SPED","CHARTER","SNAME","DNAME","CNAME",
              "VALID","API09B",
              "AA_NUM", "AI_NUM","AS_NUM","FI_NUM","HI_NUM","PI_NUM","WH_NUM","SD_NUM",
              "EL_NUM","DI_NUM","PCT_AA","PCT_AI","PCT_AS","PCT_FI","PCT_HI","PCT_WH",
              "MEALS","P_EL","P_DI")
api9.trunc <- api9[,colsWant9]
api9.trunc$year <- rep(2009, nrow(api9.trunc))
colnames(api9.trunc)[10] <- "API"

colsWant10 <- c("CDS", "RTYPE", "STYPE","SPED","CHARTER","SNAME","DNAME","CNAME",
              "VALID","API10B",
              "AA_NUM", "AI_NUM","AS_NUM","FI_NUM","HI_NUM","PI_NUM","WH_NUM","SD_NUM",
              "EL_NUM","DI_NUM","PCT_AA","PCT_AI","PCT_AS","PCT_FI","PCT_HI","PCT_WH",
              "MEALS","P_EL","P_DI")
api10.trunc <- api10[,colsWant10]
api10.trunc$year <- rep(2010, nrow(api10.trunc))
colnames(api10.trunc)[10] <- "API"

colsWant11 <- c("CDS", "RTYPE", "STYPE","SPED","CHARTER","SNAME","DNAME","CNAME",
              "VALID","API11B",
              "AA_NUM", "AI_NUM","AS_NUM","FI_NUM","HI_NUM","PI_NUM","WH_NUM","SD_NUM",
              "EL_NUM","DI_NUM","PCT_AA","PCT_AI","PCT_AS","PCT_FI","PCT_HI","PCT_WH",
              "MEALS","P_EL","P_DI")
api11.trunc <- api11[,colsWant11]
api11.trunc$year <- rep(2011, nrow(api11.trunc))
colnames(api11.trunc)[10] <- "API"

colsWant12 <- c("CDS", "RTYPE", "STYPE","SPED","CHARTER","SNAME","DNAME","CNAME",
              "VALID","API12B",
              "AA_NUM", "AI_NUM","AS_NUM","FI_NUM","HI_NUM","PI_NUM","WH_NUM","SD_NUM",
              "EL_NUM","DI_NUM","PCT_AA","PCT_AI","PCT_AS","PCT_FI","PCT_HI","PCT_WH",
              "MEALS","P_EL","P_DI")
api12.trunc <- api12[,colsWant12]
api12.trunc$year <- rep(2012, nrow(api12.trunc))
colnames(api12.trunc)[10] <- "API"

apiMaster <- list(api6.trunc, api7.trunc, api8.trunc, api9.trunc,
                  api10.trunc, api11.trunc, api12.trunc) %>%
   reduce(full_join, by = c(colsWant12[c(1:9,11:29)], "API","year")
   )

#### WRITE OUT FILE ####
write.csv(apiMaster, "CaliAPIscore_master.csv")
