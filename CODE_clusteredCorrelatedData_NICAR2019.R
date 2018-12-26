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
#####   ifelse1() : Helper function that allows for 
#####                           returning a multivariate response
#####                           based upon a univariate logical comparison
ifelse1 <- function (test, yes, no){
   if (test) yes
   else no
}

#### LOAD DATA ####
### example 1 ###
# original data obtained from American Community Survey: Table S2506 #
# for data cleaning, see https://github.com/maryryan/R_MLandDataModels/blob/master/R_MLandDataModels_housingAndEduDataSetCleaning.R #
housing <- read.csv( "house_edu.csv", header=T )
housing <- housing[,-1]

### example 2 ###
# original data downloaded from: https://www.cde.ca.gov/ta/ac/ap/apidatafiles.asp #
# for data cleaning, see https://github.com/maryryan/R_clusteredCorrelatedData19/blob/master/DATACLEANING_CaliAPIscores_06-12.R #
api <- read.csv( "CaliAPIscore_master.csv", header=T )
api <- api[,-1]

# restrict to observations with valid API scores #
api.valid <- api[!(is.na(api$VALID)),]

# restrict to just district-wide observations #
api.districts <- api.valid[which( api.valid$RTYPE == "D" ),]

### example 3 ###
# data from geepack package; subset of six-city study #
# original data from: Fitzmaurice, G.M. and Laird, N.M. (1993) A likelihood-based method for analyzing longitudinal binary responses, Biometrika 80: 141–151. #
data( ohio )

#### EXAMPLE 1: Median Housing Value in Texas ####
## forest plot ##
housing.grp <- groupedData( Median ~ yrsSince2009 | countyID,
                            data=housing )

housing.grp.lm <- lmList( Median ~ yrsSince2009 | countyID,
                          data=housing.grp )

## creating forest plots of the intercepts & slopes of all the regressions ##
len <- length(housing.grp.lm)
forest.matrix <- matrix(NA, nrow = len, ncol = 7)
forest.matrix[,1] <- names(housing.grp.lm)#county names
forest.matrix[,2] <- intervals(housing.grp.lm)[1:len]#lower int
forest.matrix[,3] <- intervals(housing.grp.lm)[(len+1):(2*len)]#est. int
forest.matrix[,4] <- intervals(housing.grp.lm)[(2*len + 1):(3*len)]#upper int
forest.matrix[,5] <- intervals(housing.grp.lm)[(3*len + 1):(4*len)]#lower sope
forest.matrix[,6] <- intervals(housing.grp.lm)[(4*len + 1):(5*len)]#est. slope
forest.matrix[,7] <- intervals(housing.grp.lm)[(5*len + 1):(6*len)]#upper slope


par(mfrow = c(1,2))
plot( forest.matrix[1,2:4], rep(1, 3), type = "l", ylim = c(1, len),
      xlim=c(70000,210000),xlab = "Intercept",
      ylab= "Order of Intercept",
      cex.lab=0.8, cex.axis=0.7)
for( j in 1:length(unique(forest.matrix[,1]))){
   lines( forest.matrix[j,2:4], rep(j, 3) )
   
}

plot( forest.matrix[1,5:7], rep(1, 3), type = "l", ylim = c(1, len),
      xlim=c(-200,12000), xlab = "Slope", ylab= "Order of Intercept",
      cex.lab=0.8, cex.axis=0.7)
for( j in 1:length(unique(forest.matrix[,1]))){
   lines( forest.matrix[j,5:7], rep(j, 3) )
   
}
mtext("Random Intercepts & Slopes of Texas Median Home Values",
      outer=TRUE,line=-2, cex=0.9)

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

#### EXAMPLE 2: California Academic Performance Index (API) scores ####
## forest plot ##

obs1 <- table(api.districts$CDS)
obs1.ids <- as.numeric(names(obs1[obs1==1]))

api.grp <- groupedData( API ~ year | CDS,
                        data=api.districts[which(!(api.districts$CDS %in% obs1.ids)),] )

api.grp.lm <- lmList( API ~ year | CDS,
                      data=api.grp[,c("CDS", "API","year")])


## creating forest plots of the intercepts & slopes of all the regressions ##
len <- length(api.grp.lm)
forest.matrix <- matrix(NA, nrow = len, ncol = 7)
forest.matrix[,1] <- as.numeric(names(api.grp.lm))#district names
forest.matrix[,2] <- intervals(api.grp.lm)[1:len]#lower int
forest.matrix[,3] <- intervals(api.grp.lm)[(len+1):(2*len)]#est. int
forest.matrix[,4] <- intervals(api.grp.lm)[(2*len + 1):(3*len)]#upper int
forest.matrix[,5] <- intervals(api.grp.lm)[(3*len + 1):(4*len)]#lower sope
forest.matrix[,6] <- intervals(api.grp.lm)[(4*len + 1):(5*len)]#est. slope
forest.matrix[,7] <- intervals(api.grp.lm)[(5*len + 1):(6*len)]#upper slope


par(mfrow = c(1,2))
plot( forest.matrix[1,2:4], rep(1, 3), type = "l", ylim = c(1, len),
      xlim=c( -120000, 120000), xlab = "Intercept",
      ylab= "Order of Intercept",
      cex.lab=0.8, cex.axis=0.7)
for( j in 1:length(unique(forest.matrix[,1]))){
   lines( as.numeric(forest.matrix[j,2:4]), rep(j, 3) )
   
}

plot( forest.matrix[1,5:7], rep(1, 3), type = "l", ylim = c(1, len),
      xlim=c( -50, 75 ), xlab = "Slope", ylab= "Order of Intercept",
      cex.lab=0.8, cex.axis=0.7)
for( j in 1:length(unique(forest.matrix[,1]))){
   lines( forest.matrix[j,5:7], rep(j, 3) )
   
}
mtext("Random Intercepts & Slopes of California API Scores",
      outer=TRUE,line=-2, cex=0.9)

## Independence correlation structure ##
api.indep <- gee( API ~ PCT_AA + PCT_AS + PCT_HI + MEALS + P_EL + year,
                   id = CDS,
                   data = api.districts,
                   corstr = "independence" )

summary( api.indep )$coef

## Exchangeable correlation structure ##
api.exch <- gee( API ~ PCT_AA + PCT_AS + PCT_HI + MEALS + P_EL + year,
                  id = CDS,
                  data = api.districts,
                  corstr = "exchangeable" )

summary( api.exch )$coef

## AR-1 correlation structure ##
api.ar1 <- gee( API ~ PCT_AA + PCT_AS + PCT_HI + MEALS + P_EL + year,
                 id = CDS,
                 data = api.districts,
                 corstr = "AR-M",
                 Mv = 1 )

summary( api.ar1 )$coef


#### EXAMPLE 3: Wheezing and Maternal Smoking ####
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

