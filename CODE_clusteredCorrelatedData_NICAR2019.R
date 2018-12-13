#################
###################### R: Models for Clustered
###################### and Correlated Data
###################### NICAR 2019
###################### March 7-10
###################### INSTRUCTOR: Mary Ryan
###################### CREATED 11.27.2018
###################### UPDATED 12.13.2018
##################

#### LOAD PACKAGES ####
#install.packages( 'tidyverse' )
#install.packages( 'gee' )
#install.packages( 'survey' )
#install.packages( 'geepack' )

library( tidyverse, quietly=T, warn.conflicts = F )
library( gee, quietly=T, warn.conflicts = F )
library( geepack, quietly=T, warn.conflicts = F )
library( survey, quietly=T, warn.conflicts = F  )

#### FUNCTIONS WE NEED ####
### function that allows us to get confidence intervals on binary longitudinal glms ###
## also lets us implement a robust variance fix-em-up ##
glmCI.long <- function (model, transform = TRUE, robust = FALSE) {
   link <- model$family$link
   coef <- summary(model)$coef[, 1]
   se <- ifelse1(robust, summary(model)$coef[,4], summary(model)$coef[, 
                                                                      2])
   zvalue <- coef/se
   pvalue <- 2 * (1 - pnorm(abs(zvalue)))
   if (transform & is.element(link, c("logit", "log"))) {
      ci95.lo <- exp(coef - qnorm(0.975) * se)
      ci95.hi <- exp(coef + qnorm(0.975) * se)
      est <- exp(coef)
   }
   else {
      ci95.lo <- coef - qnorm(0.975) * se
      ci95.hi <- coef + qnorm(0.975) * se
      est <- coef
   }
   rslt <- round(cbind(est, ci95.lo, ci95.hi, zvalue, pvalue), 
                 4)
   colnames(rslt) <- ifelse1(robust, c("Est", "robust ci95.lo", 
                                       "robust ci95.hi", "robust z value", "robust Pr(>|z|)"), 
                             c("Est", "ci95.lo", "ci95.hi", "z value", "Pr(>|z|)"))
   colnames(rslt)[1] <- ifelse(transform & is.element(link, 
                                                      c("logit", "log")), "exp( Est )", "Est")
   rslt
}
#### LOAD DATA ####
### example 1 ###
# data from geepack package; subset of six-city study #
# original data from: Fitzmaurice, G.M. and Laird, N.M. (1993) A likelihood-based method for analyzing longitudinal binary responses, Biometrika 80: 141–151. #
data( ohio )

### example 2 ###
# original data obtained from American Community Survey: Table S2506 #
# for data cleaning, see https://github.com/maryryan/R_MLandDataModels/blob/master/R_MLandDataModels_housingAndEduDataSetCleaning.R #
housing <- read.csv( "house_edu.csv", header=T )
housing <- housing[,-1]

### example 3 ###
# original data downloaded from: https://www.cde.ca.gov/ta/ac/ap/apidatafiles.asp #
# for data cleaning, see https://github.com/maryryan/R_clusteredCorrelatedData19/blob/master/DATACLEANING_CaliAPIscores_06-12.R #
api <- read.csv( "CaliAPIscore_master.csv", header=T )
api <- api[,-1]

## create indicators for charter ##
api$charter_df <- ifelse( api$CHARTER_fact == 2, 1, 0 )
api$charter_ndf <- ifelse( api$CHARTER_fact == 1, 1, 0 )

## restrict to school districts ##
api.districts <- api[which( api$RTYPE == "D" ),]

#### EXAMPLE 1: Wheezing and Maternal Smoking ####
## Independence correlation structure ##
ohio.indep <- gee( resp ~ age + smoke,
                   id = id,
                   data = ohio,
                   family=binomial(link='logit'),
                   corstr="independence" )

summary( ohio.indep )$coef
glmCI.long( ohio.indep, robust = T )

## Exchangeable correlation ##
ohio.exch <- gee( resp ~ age + smoke,
                   id = id,
                   data = ohio,
                   family=binomial(link='logit'),
                   corstr="exchangeable" )

summary( ohio.exch )$coef
glmCI.long( ohio.exch, robust = T )

## AR-1 correlation structure ##
ohio.ar1 <- gee( resp ~ age + smoke,
                  id = id,
                  data = ohio,
                  family=binomial(link='logit'),
                  corstr="AR-M",
                  Mv = 1 )

summary( ohio.ar1 )$coef
glmCI.long( ohio.ar1, robust = T )

#### EXAMPLE 2: Median Housing Value in Texas ####
## Independence correlation structure ##
house.indep <- gee( Median ~ BApctTotPop18plus + yrsSince2009,
                 id = countyID,
                 data = housing,
                 corstr = "independence" )

summary( house.indep )$coef

## Exchangeable correlation structure ##
house.exch <- gee (Median ~ BApctTotPop18plus + yrsSince2009,
                 id = countyID,
                 data = housing,
                 corstr = "exchangeable" )
summary( house.exch )$coef

## AR-1 correlation structure ##
house.ar1 <- gee( Median ~ BApctTotPop18plus + yrsSince2009,
                 id = countyID,
                 data = housing,
                 corstr = "AR-M",
                 Mv = 1 )
summary( house.ar1 )$coef

#### EXAMPLE 3: California Academic Performance Index (API) scores ####
## Independence correlation structure ##
api.indep <- gee( API ~ PCT_AA + PCT_AS + PCT_HI + charter_df + charter_ndf + MEALS + P_EL + year,
                   id = CDS,
                   data = api.districts,
                   corstr = "independence" )

summary( api.indep )$coef

## Exchangeable correlation structure ##
api.exch <- gee( API ~ PCT_AA + PCT_AS + PCT_HI + charter_df + charter_ndf + MEALS + P_EL + year,
                  id = CDS,
                  data = api.districts,
                  corstr = "exchangeable" )

summary( api.exch )$coef

## AR-1 correlation structure ##
api.ar1 <- gee( API ~ PCT_AA + PCT_AS + PCT_HI + charter_df + charter_ndf + MEALS + P_EL + year,
                 id = CDS,
                 data = api.districts,
                 corstr = "AR-M",
                 Mv = 1 )

summary( api.ar1 )$coef