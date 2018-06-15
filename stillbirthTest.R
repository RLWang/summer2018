library(haven)
SBR_input <- read_dta("~/Desktop/stillbirth rate files/data inputs and modelling/SBR_input.dta",encoding='latin1')

library(distortr)
library(R2jags)
library(fields)
library(splines)
library(boot)
library(RColorBrewer)
library(tidyverse)
library(dplyr)
national_covar <- read_dta("~/Desktop/stillbirth rate files/data inputs and modelling/national_covariates.dta",encoding='latin1')
reported_loess <- read_dta("~/Desktop/stillbirth rate files/data inputs and modelling/reported_loess.dta",encoding='latin1')
SBR_results<-read_dta("~/Desktop/stillbirth rate files/results/SBR_results.dta",encoding='latin1')


nationalCovar_countries<-unique (national_covar$country)
nationalCovar_countries
SBR_countries<-unique (SBR_input$country)
SBR_countries
loess_countries<-unique (reported_loess$country)
loess_countries

withoutLoess<-SBR_input %>% 
  filter(!country %in% lowess_countries)

withoutLoess_countries <-unique(withoutLoess$country)

all.equal(withoutLoess_countries,SBR_countries)

test <-cbind(withoutLoess_countries,SBR_countries)
test


# to see the source mean of each country
avg_source<-aggregate(withoutLoess[,13], list(withoutLoess$country), mean)
source1 <- arrange(avg_source, avg_source$n_context5)
##################

# get a list of country and its matching region
countryRegionList <- national_covar[,c(2,7)]
countryRegionList<-distinct(countryRegionList)
countryRegionList

##get the numeric representation of the country at index i
countryRegionList$country_fac <- as.numeric(factor(countryRegionList$country))
# connect SBR_input countries with national_covar countries 

totalIndex_SBR = length(SBR_input$country)
country_SBR = as.vector(SBR_input$country)

totalIndex_covar = length(countryRegionList$country)
country_covar= as.vector(countryRegionList$country)
country_num_covar = as.vector(countryRegionList$country_fac)
getc.i <- rep (NA, totalIndex_SBR)


for (i in 1: totalIndex_SBR){
  for (j in 1:totalIndex_covar){
       if (country_SBR[i] == country_covar[j]){   
            getc.i[i]<-country_num_covar[j]
       }
  }
}

## get the year at index i, get numeric 1,2,3,4
gett.i <- national_covar$year
gett.i

## get the region at index i
getr.c <- countryRegionList$nmdg

# create a c by t matrix for each covariate 
yearStart = min(national_covar$year)
yearStart
yearEnd = max(national_covar$year)
yearEnd
yearLength = yearEnd-yearStart +1
## for edu
edu_matrix = matrix(nrow=totalIndex_covar,ncol=yearLength)
# for (i in 1:totalIndex_covar){
#   for ( j in 1: yearLength){
#     edu_matrix[i,j] <- as.numeric(national_covar[national_covar$country == country_covar[i] & national_covar$year== seq(yearStart,yearEnd)[j],"mean_edu"])
#   }
# }
totalIndex_covar
covarMatrix <-function (cMatrix, covar){
  for (i in 1:totalIndex_covar){
    for ( j in 1: yearLength){
      cMatrix[i,j] <- as.numeric(national_covar[national_covar$country == country_covar[i] & national_covar$year== seq(yearStart,yearEnd)[j],covar])
    }
  }
  return (cMatrix)
}

edu_matrix = covarMatrix(edu_matrix,"mean_edu")

## for data type
dataType_matrix =matrix(nrow=totalIndex_covar,ncol=yearLength)
dataType_matrix= covarMatrix(dataType_matrix,"n_context5")

## for ANC
anc_matrix = matrix(nrow=totalIndex_covar,ncol=yearLength)
anc_matrix = covarMatrix(anc_matrix,"anc4")

## for NMR
nmr_matrix = matrix(nrow=totalIndex_covar,ncol=yearLength)
nmr_matrix = covarMatrix(nmr_matrix,"ln_nmr")

## for GNI
gni_matrix = matrix(nrow=totalIndex_covar,ncol=yearLength)
gni_matrix = covarMatrix(gni_matrix,"ln_gni")

## for LBW
national_covar$ln_lbw = log(national_covar$lbw_final)
lbw_matrix = matrix(nrow=totalIndex_covar,ncol=yearLength)
lbw_matrix = covarMatrix(lbw_matrix,"ln_lbw")

## for region
region_matrix = matrix(nrow=totalIndex_covar,ncol=yearLength)
region_matrix = covarMatrix(region_matrix,"nmdg")


# fit the model
## show i create a y somewhere??? 
totalObs_SBR = length(SBR_input)
totalRegion = length(unique(countryRegionList$nmdg))

model<-"model{
for (i in 1:totalObs_SBR){
 y.i[i] ~dnorm (mu.ct[getc.i[i]], gett.i[i], tauy)
}

for (c in 1:totalIndex_covar){
 a.c[c] ~ dnorm(a.r[getr.c[c]], tau.c)
  for( t in 1:yearLength){
    log(mu.ct[c,t]) = (a.c[c] + beta1*dataType_matrix[c,t] +beta2*edu_matrix[c,t]+beta3*anc_matrix[c,t]+beta4*lbw_matrix[c,t]+beta5*nmr_matrix+beta6*gni_matrix+beta7*region_matrix)
  }
}

for (r in 1:totalRegion){
a.r[r] ~dnorm(beta0, tau.r)
}
 
tauy<- pow(sdy,-2)
sdy <-dunif(0,5)
// same for tau.c
tau.c<-


}"

jags.data <- list(  y.i = y.i, getc.i = getc.i, getr.c = getr.c, gett.i= gett.i, totalObs_SBR = totalObs_SBR, totalIndex_covar=totalIndex_covar, totalRegion= totalRegion)
parnames <- c("alpha.j", "mu.alpha", "sigma.y", "sigma.alpha","beta0","beta1","beta2","beta3","beta4","beta5","beta6")
mod0 <-jags(data = jags.data, 
            parameters.to.save=parnames, 
            model.file = textConnection(model))
