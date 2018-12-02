#################
###################### R: Models for Clustered
###################### and Correlated Data
###################### NICAR 2019
###################### March 7-10
###################### INSTRUCTOR: Mary Ryan
###################### CREATED 11.27.2018
###################### UPDATED 12.1.2018
##################

#### LOAD PACKAGES ####
#install.packages( 'tidyverse' )
#install.packages( 'nlme' )
#install.packages( 'lme4' )
#install.packages( 'survey' )

library( tidyverse, quietly=T )
library( nlme, quietly=T )
library( lme4, quietly=T )
library( survey )

#### LOAD DATA ####
## example 3 ##
api <- read.csv("CaliAPIscore_master.csv", header=T)
api <- api[,-1]

#### EXAMPLE 1: Heart Disease ####

#### EXAMPLE 2: Median Housing Value in Texas ####

#### EXAMPLE 3: California Academic Performance Index (API) scores ####