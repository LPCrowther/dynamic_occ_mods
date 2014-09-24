###### Testing occupancy on BWARS data ######
# Powney & Isaac - 2014 #

rm(list=ls()) # clear R

### add packages ###
library("sparta")
install.packages("W:/PYWELL_SHARED/Pywell Projects/BRC/Colin/R packages/BRCmap_0.0.7.zip",repos = NULL)
library("BRCmap")


## set datadir
setwd("W:/PYWELL_SHARED/Pywell Projects/BRC/Gary/Bayesian/Liam")
datadir <- paste(getwd(),"/data/",sep="")

### add data ###
#load('W:/PYWELL_SHARED/Pywell Projects/BRC/Tom August/Mixed Models - all taxa/Data_10kmYearCompleted/Ants_BRC_130730.Rdata')
#ants <- taxa_data
#load('W:/PYWELL_SHARED/Pywell Projects/BRC/Tom August/Mixed Models - all taxa/Data_10kmYearCompleted/Bees_BRC_130730.Rdata')
#bees <- taxa_data
#load('W:/PYWELL_SHARED/Pywell Projects/BRC/Tom August/Mixed Models - all taxa/Data_10kmYearCompleted/Wasps_BRC_130730.Rdata')
#wasps <- taxa_data

### USE NEW DATA ###
bwars <- read.csv(paste(datadir,"20140405Nick Isaacs.csv",sep=""),header=T)
bwars$Date.Collected <- as.Date(bwars$Date.Collected,format="%d/%m/%Y")

nrow(bwars[is.na(bwars$Date.Collected),]) # this loses 21449 records that are either not day precision or have a day that doesn't exist, i.e. 31st of April!
bwars <- bwars[!is.na(bwars$Date.Collected),]

nrow(bwars)  # 468474

### prep the taxa data ###
# convert data to the 1 km level #
#taxa_data$kmsq <- reformat_gr(taxa_data$TO_GRIDREF, prec_out = 1000)
bwars$kmsq <- reformat_gr(bwars$GridReference, prec_out = 1000)
bwars <- bwars[!is.na(bwars$kmsq),] # loses 7822 records at coarser than 1 km resolution.

# sort naming
names(bwars)[2] <- "taxa"
names(bwars)[3] <- "time_period"
names(bwars)[8] <- "site"

# drop unwanted columns #
#taxa_data <- taxa_data[,c(1,6:8,10)]
bwars <- bwars[,c("BRC.Number","taxa","time_period","site")]

# only keep data with day precision #
#taxa_data <- taxa_data[taxa_data$TO_STARTDATE == taxa_data$TO_ENDDATE,]  # modify this code
# assume records on the 1st Jan and 31 Dec are yearly records therfore drop them?! CHECK THIS WITH NICK AND MIKE EDWARDS

# 57 records on Jan 1
bwars <- bwars[!row.names(bwars)%in%row.names(bwars[format(bwars$time_period,"%m")=="01" & format(bwars$time_period,"%d")=="01",]),]
# 73 records on Dec 31
bwars <- bwars[!row.names(bwars)%in%row.names(bwars[format(bwars$time_period,"%m")=="12" & format(bwars$time_period,"%d")=="31",]),]

bwars <- bwars[!bwars$taxa=="",]# drop blank species records

# test #
#selectdates <-  
   #(format(bwars$date,"%m") == "04" & as.numeric(format(test$date,"%d")) >= 10) |
   #(format(test$date,"%m") == "05" & as.numeric(format(test$date,"%d")) <= 21)
     
     ## subset the original data
#result <- test[selectdates,]
     
     ## which looks as expected:    
#result 

#create subset containing only the records for Bombus spp.
bombus<- subset(bwars, "TRUE"==grepl("BOMBUS", bwars$taxa))


### Split the data into bees, wasps and ants ### 

###this is now redundant see below for Bombus LL calculation

#taxonomy <- read.csv(paste(datadir,"Aculeate_Species_Family_Group.csv",sep=""),header=T) # load in the names file
#taxonomy <- taxonomy[,c("FullName","Popular_group_Name")]

# correct 4 names prior to merge
#bwars$taxa <- as.character(bwars$taxa)
#bwars[bwars$taxa=="PEMPHREDON rugifera","taxa"] <- rep("PEMPHREDON rugifer",nrow(bwars[bwars$taxa=="PEMPHREDON rugifera",])) 

#bwars <- merge(bwars,taxonomy,by.x="taxa",by.y="FullName")

##### NEED TO SPLIT BEES, WASPS AND ANTS ######
#ants <- bwars[bwars$Popular_group_Name=="Ant",]
#wasps <- bwars[bwars$Popular_group_Name=="Wasp",]
#bees <- bwars[bwars$Popular_group_Name=="Bee",]

#ants <- ants[,1:4]
#wasps <- wasps[,1:4]
#bees <- bees[,1:4]

# ID LL for each visit (unique site/date combination) # SO SEPERATELY FOR EACH GROUP #
### ANTS ###
#taxa_data <- ants
#site_date <- unique(taxa_data[,c("site","time_period")])
#loop_thru <- 1:length(site_date[,1]) # not sure why this wasn't when stated in the start of the loop?!

#L <- NULL
### convert this to an apply or table function ###

#for (i in loop_thru) {
 # L <- c(L,nrow(unique(taxa_data[taxa_data$site==site_date[i,"site"]&taxa_data$time_period==site_date[i,"time_period"],])))
#}

#site_date$L <- L

### join site L to the bwars taxa data ###
#ant_data <- merge(taxa_data,site_date)
#ant_data$rec_group <- "ant"

### WASPS ###
#taxa_data <- wasps
#site_date <- unique(taxa_data[,c("site","time_period")])
#loop_thru <- 1:length(site_date[,1]) # not sure why this wasn't when stated in the start of the loop?!

#L <- NULL
### convert this to an apply or table function ###

#for (i in loop_thru) {
 # L <- c(L,nrow(unique(taxa_data[taxa_data$site==site_date[i,"site"]&taxa_data$time_period==site_date[i,"time_period"],])))
#}

#site_date$L <- L

### join site L to the bwars taxa data ###
#wasp_data <- merge(taxa_data,site_date)
#wasp_data$rec_group <- "wasp"

### BEES ###
#taxa_data <- bees
#site_date <- unique(taxa_data[,c("site","time_period")])
#loop_thru <- 1:length(site_date[,1]) # not sure why this wasn't when stated in the start of the loop?!

#L <- NULL
### convert this to an apply or table function ###

#for (i in loop_thru) {
 # L <- c(L,nrow(unique(taxa_data[taxa_data$site==site_date[i,"site"]&taxa_data$time_period==site_date[i,"time_period"],])))
#}

#site_date$L <- L

### join site L to the bwars taxa data ###
#bee_data <- merge(taxa_data,site_date)
#bee_data$rec_group <- "Bee"

### BOMBUS ###
taxa_data <- bombus
site_date <- unique(taxa_data[,c("site","time_period")])
loop_thru <- 1:length(site_date[,1]) # not sure why this wasn't when stated in the start of the loop?!

L <- NULL
### convert this to an apply or table function ###

for (i in loop_thru) {
L <- c(L,nrow(unique(taxa_data[taxa_data$site==site_date[i,"site"]&taxa_data$time_period==site_date[i,"time_period"],])))
  }

site_date$L <- L

### join site L to the bwars taxa data ###
bombus_data <- merge(taxa_data,site_date)
#bee_data$rec_group <- "Bee"

# merge all groups together
#new_taxa <- rbind(ant_data,wasp_data)
#new_taxa <- rbind(new_taxa,bee_data)

### SAVE THE NEW FILE ###
#write.csv(new_taxa, file="data/bwars_may2014_data_for_occ.csv")
###version for Bombus only
write.csv(bombus_data, file="data/bwars_may2014_data_for_occ.csv")






### NOW OPEN occ2bwars.R ###

# testing bayesian script #

### Load new_taxa ###
new_taxa <- read.csv("data/bwars_may2014_data_for_occ.csv",header=T) # this is the new bwars data (may 2014) which has been cleaned in prep for the bayesian analysis
# old_taxa <- read.csv("data/bwars_data_for_occ.csv",header=T)

# drop first column as row ID were saved
new_taxa <- new_taxa[,-1]

# ensure date is treated as date
new_taxa$time_period <- as.Date(new_taxa$time_period)

# only include records later than 1970 
#new_taxa <- new_taxa[new_taxa$YEAR>1969,]
#LC edit include only since 2000
new_taxa <- new_taxa[new_taxa$YEAR>1999,]

# add visit column - this is a date ordered rank per year (based on sims) - check with Nick as this seems to contradict his paper (unique combination of year and site)
new_taxa$year <- format(new_taxa$time_period,"%Y") # add a year column

bw_year <- unique(new_taxa$year)

YEAR <- NULL
visit <- NULL
time_p <- as.Date("",format="%d/%m/%Y")
time_p <- time_p[-1]

for (i in bw_year){ #loop through each year, order dates of the records within them and rank them from early to late
  tp <- unique(new_taxa[new_taxa$year==i,"time_period"])
  tp <- sort(tp)
  tp_rank <- 1:length(tp)
  time_p <- c(time_p,tp)
  visit <- c(visit,tp_rank)
  YEAR <- c(YEAR,rep(i,length(tp)))
}

visit_data <- data.frame(year=YEAR,Visit=visit,time_period=time_p)

# merge visits into new_taxa
new_taxa <- merge(new_taxa,visit_data)

### Define WHICH OccMods should be fitted (this defines the file name) ###
#OccMods <- c('SS_LL','SS_Site','LL_Site','Simple','Arco')
#OccMods <- c('LL_Site','SS_LL','simple')
#OccMods <- c('LL_Site','Simple')
OccMods <- c('Simple')

nyr <- 3  # this parameter will need to be specified by the user when in sparta, could have a default of 3?!

### currently occ coded for one species ###


### Loop through species ###
spp_list <- unique(as.character(new_taxa$taxa))

#### TEST MODEL FOR ONE SPECIES ####
# BW_00472 B. hypnorum # (should be increasing, first recorded in 2000/2001!)
# BW_00475 B. lucorum s.l.
# BW_00476 B. lucorum s.s.
# BW_00136 M. ruginodis

test <- new_taxa[new_taxa$taxa=="BOMBUS pascuorum",] # species with most records (n = 18519)
test <- new_taxa[new_taxa$taxa=="FORMICA rufa",]

### match the format of Nick's example (simdata) ###
test <- test[c("year","site","Visit","L")]
names(test) <- c("Year","Site","Visit","L")
test$focal <- TRUE # as we have presence only set this to true

# year and site need to be numeric starting from 1 to length of them.  This is due to the way the R bugs code is written (line 44).
test$Year <- as.numeric(as.factor(test$Year))
test$Site <- as.numeric(as.factor(test$Site))

simdata <- test

######################################## Setup BUGS data
require(R2jags)

ni<-5000; nb<-2500; nt<-3; nc<-3 # as Marnix (~ 2 minutes)   # MCMC settings

# need to get a measure of whether the species was on that site in that year, unequivocally, in zst
#it should have dims=c(nsite, nyear)
zst <- acast(simdata, Site~factor(Year), value.var='focal', max, fill=0) # initial values for the latent state = observed state
nyear <- LenUniq(simdata$Year) 

parameters <- c("fit", "fit.new", "psi.fs", "regres.psi","regres.pdet", "sigma2","sd.lp", "mu.lp", "tau.lp", "pdet.alpha")

##################################### function

initiate <- function(z, i=1) {
  init <- list (z=z,  alpha.p=rep(runif(1, -2, 2), nyear))
  if(i>=2) init$LL.p=runif(1, -2, 2)
  if(i==3) {
    init$dtype2.p=runif(1, -2, 2)
    init$dtype3.p=runif(1, -2, 2)
  }
  init
}

##################################### Loop through each OccMod
system.time(
  OccResults <- sapply(OccMods, function(OccMod){ 
    # work out which Occmod we're using and make the necessary arrangments
    # the name of the OccMod defines how it should be set up
    #print(OccMod)
    init=1
    
    # first figure out if we're doing the site selection
    if(grepl('SS', OccMod)) { # Site Selection Criterion: restrict the data to sites represented 3 times
      # identify the visits that are on sites wih three years data
      # first determine the number of years per site
      yps <- rowSums(acast(simdata, Site~Year, length, value.var='L')>0)
      sites_to_include <- as.numeric(names(yps[yps>=nyr]))
      zst <- zst[dimnames(zst)[[1]] %in% sites_to_include,]
      i <- simdata$Site %in% sites_to_include
      #i <-is.gridcell.wellsampled2(simdata) # same as three steps
    } else i <- TRUE # use all the rows
    
    # now assemble the bugs_data and related objects
    #need to convert Site identities into row numbers
    site_to_row_lookup <- data.frame(Site=as.integer(dimnames(zst)[[1]]),rownum=1:nrow(zst)) 
    
    bugs_data <- with(merge(simdata[i,], site_to_row_lookup), # adds rownum to simdata (used below)
                      list(y=as.numeric(focal), Year=Year, Site=rownum, 
                           nyear=nyear, nsite=nrow(zst), nvisit=nrow(simdata[i,]),
                           sumX=sum(unique(Year)), sumX2=sum(unique(Year)^2)))
    
    if(grepl('LL', OccMod)) { # List length as a covariate
      bugs_data$logL=log(simdata[i,]$L)
      bugs_data$dtype2p_min = -10; bugs_data$dtype2p_max=10 #constraints on the priors
      parameters <- c(parameters, 'LL.p')
      init=2 # for defining which initial values are required
    } 
    
    if(!grepl('Site', OccMod)) parameters <- setdiff(parameters, 'sigma2')
    
    if(grepl('Arco', OccMod)) { # Arco's model
      bugs_data$DATATYPE2 = as.numeric(simdata$L %in% 2:3) # a short list
      bugs_data$DATATYPE3 = as.numeric(simdata$L > 3) # 'full' list
      bugs_data$dtype2p_min = -10; bugs_data$dtype2p_max=10 #constraints on the priors
      bugs_data$dtype3p_min= -10; bugs_data$dtype3p_max=10 #constraints on the priors
      parameters <- c(parameters, "pdet.d2","dtype2.p", "pdet.d3", "dtype3.p")
      init=3
    }
    
    # it also defines the name of the file ('SS' criterion is not used to determine which file is used)
    OccModFile <- gsub('SS_', '', OccMod)
    
    # set the initial values
    init.vals <- replicate(nc, initiate(z=zst, i=init), simplify=F)
    
    out <- jags(bugs_data, init.vals, parameters, model.file=paste0('Occ_',OccModFile,'.bugs'), 
                n.chains=nc, n.iter=ni, n.thin=nt, n.burnin=nb, DIC=TRUE)   
    
  
    # in parallel this code has been through several changes, each time generating more informative errors
    #attach(bugs_data)
    #out <- jags.parallel(data=list(names(bugs_data))[[1]], 
    #                     inits=init.vals, param=parameters, model.file=paste0('Occ_',OccModFile,'.bugs'), 
    #                     n.chains=3, n.iter=5000, n.thin=3, n.burnin=2500, DIC=TRUE)
    # it now claims that n.chains != length(inits)
    #detach(bugs_data)
    ######
    
    
    out$BUGSoutput$summary
  }, USE.NAMES=T, simplify=F)
)

names(OccResults) <- OccMods


Occ_trends <- as.data.frame(t(sapply(
  OccResults, function(x) x['regres.psi',c('mean','2.5%','97.5%','Rhat')])))
# convert to a effective p-value: just a 1 or 0 
names(Occ_trends)[1] <- 'trend'
Occ_trends$p <- as.numeric(0>apply(Occ_trends, 1, function(x) x['2.5%']*x['97.5%']))
Occ_trends$name <- rownames(Occ_trends)
Occ_trends <- melt(Occ_trends[,c(6,1,5,4)], id='name')

Occ_out <- Occ_trends$value
names(Occ_out) <- with(Occ_trends, paste0('Occ+',name,'_',variable))

Occ_out
