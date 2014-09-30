###### Testing occupancy on BWARS data ######
# Powney & Isaac - 2014 #

rm(list=ls()) # clear R

#### add packages ####
#library(sparta)

### Load new_taxa ###
taxa_data <- read.csv("data/bwars_may2014_data_for_occ.csv",header=T) # this is the new bwars data (may 2014) which has been cleaned in prep for the bayesian analysis

##This is now hashed out as only Bombus remain in data following changes to data prep script
#new_taxa <- taxa_data[taxa_data$rec_group=="Bee",] # select group of interest
#new_taxa <- taxa_data[taxa_data$rec_group=="ant",] # select group of interest
new_taxa <- taxa_data

# drop first column as row ID were saved
new_taxa <- new_taxa[,-1]

# ensure date is treated as date
new_taxa$time_period <- as.Date(new_taxa$time_period)

#define year based on time-period
new_taxa$year <- format(new_taxa$time_period,"%Y") # add a year column

#LC edit include only since 2000
new_taxa <- new_taxa[new_taxa$year>1999,]

# drop duplicate rows
new_taxa <- unique(new_taxa)

### add visit column - this is a date ordered rank per year (based on sims) - check with Nick as this seems to contradict his paper (unique combination of year and site) ###

#new_taxa$year <- format(new_taxa$time_period,"%Y") # add a year column

#bw_year <- unique(new_taxa$year)

#YEAR <- NULL
#visit <- NULL
#time_p <- as.Date("",format="%d/%m/%Y")
#time_p <- time_p[-1]

#for (i in bw_year){ #loop through each year, order dates of the records within them and rank them from early to late
#  tp <- unique(new_taxa[new_taxa$year==i,"time_period"])
#  tp <- sort(tp)
#  tp_rank <- 1:length(tp)
#  time_p <- c(time_p,tp)
#  visit <- c(visit,tp_rank)
#  YEAR <- c(YEAR,rep(i,length(tp)))
#}

#visit_data <- data.frame(year=YEAR,Visit=visit,time_period=time_p)

# merge visits into new_taxa
#new_taxa <- merge(new_taxa,visit_data)

### Define WHICH OccMods should be fitted (this defines the file name) ###
#OccMods <- c('SS_LL','SS_Site','LL_Site','Simple','Arco')
#OccMods <- c('LL_Site','SS_LL','simple')
#OccMods <- c('LL_Site','Simple')
#OccMods <- c('Simple')
#OccMods <- c('SS_LL_site')
OccMods <- c('Arco')

nyr <- 3  # this parameter will need to be specified by the user when in sparta, could have a default of 3?!

#### TEST MODEL FOR ONE SPECIES ####
# simdata <- new_taxa[new_taxa$taxa=="BOMBUS pascuorum",] # species with most records (n = 18519)

# simdata <- new_taxa[new_taxa$taxa=="FORMICA rufa",]

require(R2jags)
library("reshape2", lib.loc="~/R/win-library/3.1")

##################################### function

### function for setting the initial values for the bayesian runs - based on which model is run
initiate <- function(z, i=1) {
  init <- list (z=z,  alpha.p=rep(runif(1, -2, 2), nyear))
  if(i>=2) init$LL.p=runif(1, -2, 2)
  if(i==3) {
    init$dtype2.p=runif(1, -2, 2)
    init$dtype3.p=runif(1, -2, 2)
  }
  init
}


### currently occ coded for one species, therefore loop through species ###
spp_list <- unique(as.character(new_taxa$taxa))
#OccMod <- "SS_LL_Site"
OccMod <- "Arco"

####@@@ test on individual species @@@###
new_taxa$focal <- FALSE
simdata <- new_taxa
simdata[simdata$taxa=="BOMBUS hypnorum","focal"] <- TRUE
#simdata[simdata$taxa=="BOMBUS lapidarius","focal"] <- TRUE
#simdata[simdata$taxa=="FORMICA rufa","focal"] <- TRUE

# create dataframe to fill
#test <- as.data.frame(matrix(ncol=7,nrow=length(spp_list))) # need to automate this

#for (i in 1:length(spp_list)) {  ### Loop through species and run the models ###
#for (i in 1:3) {  ### Loop through species and run the models ###
  #spp <- spp_list[i]
  #simdata <- new_taxa[new_taxa$taxa==spp,]
  #cat(spp,"-",i,"of",length(spp_list),"at", Sys.time())
  #print(spp)
  #print(Sys.time())
  
  ### sort the format ###
  #simdata <- simdata[c("year","site","Visit","L","focal")]
  #names(simdata) <- c("Year","Site","Visit","L","focal")
  simdata <- simdata[c("year","site","L","focal")]
  names(simdata) <- c("Year","Site","L","focal")
  
  # year and site need to be numeric starting from 1 to length of them.  This is due to the way the R bugs code is written (line 44).
  simdata$Year <- as.numeric(as.factor(simdata$Year))
  simdata$Site <- as.numeric(as.factor(simdata$Site))

  ######################################## Setup BUGS data
    
  ni<-5000; nb<-2500; nt<-3; nc<-3 # as Marnix (~ 2 minutes)   # MCMC settings

  # need to get a measure of whether the species was on that site in that year, unequivocally, in zst
  #it should have dims=c(nsite, nyear)
  zst <- acast(simdata, Site~factor(Year), value.var='focal', max, fill=0) # initial values for the latent state = observed state

  LenUniq <- function(x) length(unique(x)) 

  nyear <- LenUniq(simdata$Year) 
  
  parameters <- c("fit", "fit.new", "psi.fs", "regres.psi","regres.pdet", "sigma2","sd.lp", "mu.lp", "tau.lp", "pdet.alpha")
   # fit = part of bayesian goodness of fit
   # fit.new = part of bayesian goodness of fit
   # psi.fs = the finate sample occupancy for each year, this is the proportion of sites occupied each year
   # regres.psi = trend in occupancy
   # regres.pdet = trend in probability of detection
   # sigma2 = prior for random effect for site...? not in the simple models, therefore throws a warning
   # sd.lp = prior for the standard deviation which is converted to variance and used in the observation model prior
   # mu.lp = priot for informing the mean in the observation model prior
   # tau.lp = the inverse of the variance 
   # pdet.alpha = This appears to be the detection probability per year?
    
  ############### Alternative for just one model


  ##################################### Loop through each OccMod

    #OccResults <- sapply(OccMods, function(OccMod){ 
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
      
      ### HERE IS THE BUGS DATA ###     
      bugs_data <- with(merge(simdata[i,], site_to_row_lookup), # adds rownum to simdata (used below)
                        list(y=as.numeric(focal), Year=Year, Site=rownum, 
                             nyear=nyear, nsite=nrow(zst), nvisit=nrow(simdata[i,]),
                             sumX=sum(unique(Year)), sumX2=sum(unique(Year)^2)))
      
      ### ADD EXTRA PARTS TO THE BUGS DATA & ADDITIONAL PARAMETERS DEPENDING ON WHICH OCCMOD IS USED ###
      
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
      
      # set the initial values... 
      init.vals <- replicate(nc, initiate(z=zst, i=init), simplify=F)
      
      # nc = number chains, set init values for each run
      # zst = original detection history matrix, year by site
      # i = sets the initial value for each parameter in the model, varies based on which OccMod is used
      
      ### GIVE JAGS THE BUGS DATA, THE INIT VALUES, THE PARAMS OF INTEREST AND THE LOCATION OF THE BUGS MODEL CODE
      out <- jags(bugs_data, init.vals, parameters, model.file="W:/PYWELL_SHARED/Pywell Projects/BRC/Gary/Bayesian/Liam/data/Occ_Arco.bugs", 
                  n.chains=nc, n.iter=ni, n.thin=nt, n.burnin=nb, DIC=TRUE)   
      
      
      # in parallel this code has been through several changes, each time generating more informative errors
      #attach(bugs_data)
      #out <- jags.parallel(data=list(names(bugs_data))[[1]], 
      #                     inits=init.vals, param=parameters, model.file=paste0('Occ_',OccModFile,'.bugs'), 
      #                     n.chains=3, n.iter=5000, n.thin=3, n.burnin=2500, DIC=TRUE)
      # it now claims that n.chains != length(inits)
      #detach(bugs_data)
      ######
      
     #}, USE.NAMES=T, simplify=F) 
      
      #write.csv(out$BUGSoutput$summary, file = "B_hypn_no_vis_column_SS_LL_Bombus_Site.csv")
      #out$BUGSoutput$summary
    
###save output with date in the filename

write.csv(out$BUGSoutput$summary, file = paste("B_hypn_no_vis_column_Arco_Bombus_Site", format(Sys.time(), "%Y-%m-%d %I-%p"), "csv", sep = "."))

out$BUGSoutput$summary
}, USE.NAMES=T, simplify=F)

## filename needs editing for current version
#output <- read.csv("B_hypn_no_vis_column_SS_LL_Site.2014-09-29 11-AM.csv")
#output <- read.csv("B_hypn_no_vis_column_SS_LL_Bombus_Site.2014-09-29 02-PM.csv")
output <- read.csv("B_hypn_no_vis_column_Arco_Bombus_Site.2014-09-30 09-AM.csv")

#####LC plotting functions
###generate variables, for different models need to match coordinates to variables
surveyyear <- c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013)
#post_2.5_detect <- c(output[ 6:19, 4])
#post_50_detect <- c(output[6:19, 6])
#post_97.5_detect <- c(output[ 6:19, 8])
#post_2.5_psi <- c(output[ 20:33, 4])
#post_50_psi <- c(output[20:33, 6])
#post_97.5_psi <- c(output[ 20:33, 8])

post_2.5_detect <- c(output[ 7:20, 4])
post_50_detect <- c(output[7:20, 6])
post_97.5_detect <- c(output[ 7:20, 8])
post_2.5_psi <- c(output[ 49:62, 4])
post_50_psi <- c(output[49:62, 6])
post_97.5_psi <- c(output[ 49:62, 8])
post_2.5_detect_L2.3 <- c(output[ 21:34, 4])
post_50_detect_L2.3 <- c(output[21:34, 6])
post_97.5_detect_L2.3 <- c(output[ 21:34, 8])
post_2.5_detect_L4 <- c(output[ 35:48, 4])
post_50_detect_L4 <- c(output[ 35:48, 6])
post_97.5_detect_L4 <- c(output[ 35:48, 8])

###plot posterior detection probabilty over time
plot(post_50_detect  ~ surveyyear, data = output$summary, xlab = "Year", 
     ylab = "Probability", col = "red", type = "line", ylim =c(0,1), 
     main = "Detect (LL=1) = Red, Detect (2 <=LL<=3) = light blue, Detect (LL>=4) = dark blue, 
     Occupancy = Green, 'LL'= Bombus, dashed = 95% ci", cex.main = 0.8, lwd =3)

###plot posterior occupancy probability over time
lines(post_50_psi  ~ surveyyear, data = output$summary, col = "green", lwd = 3)

##plot credible intervals
##sinlgetons
lines(post_2.5_detect  ~ surveyyear, data = output$summary, col = "red", lty = 2)
lines(post_97.5_detect  ~ surveyyear, data = output$summary, col = "red", lty = 2)
##occupancy
lines(post_2.5_psi  ~ surveyyear, data = output$summary, col = "green", lty = 2)
lines(post_97.5_psi  ~ surveyyear, data = output$summary, col = "green", lty = 2)
##short list
lines(post_2.5_detect_L2.3  ~ surveyyear, data = output$summary, col = "steelblue3", lty = 2)
lines(post_50_detect_L2.3  ~ surveyyear, data = output$summary, col = "steelblue3", lwd = 3)
lines(post_97.5_detect_L2.3  ~ surveyyear, data = output$summary, col = "steelblue3", lty = 2)
##long list
lines(post_2.5_detect_L4  ~ surveyyear, data = output$summary, col = "blue", lty = 2)
lines(post_50_detect_L4  ~ surveyyear, data = output$summary, col = "blue", lwd =3)
lines(post_97.5_detect_L4  ~ surveyyear, data = output$summary, col = "blue", lty = 2)

out<- read.csv("B_hypn_no_vis_column_SS_LL_Site.csv")

##distribution of list length
hist(read.csv("data/bwars_may2014_data_for_occ.csv")$L, main = "List lengths of all bees")
hist(L, main = "List length of Bombus")


###plot posterior detection probabilty over time
plot(out$BUGSoutput$mean$pdet.alpha  ~ surveyyear, data = out$summary, xlab = "Year", ylab = "Probability", col = "red", type = "line", ylim =c(0,1), main = "Detect (LL=1) = Red, Detect (2 <=LL<=3), Detect (LL>=4), Occupancy = Green, 'LL'= Bombus, dashed = 95% ci", cex.main = 0.5)

###plot posterior occupancy probability over time
lines(out$BUGSoutput$mean$psi.fs  ~ surveyyear, data = out$summary, col = "green")

##plot credible intervals
lines(post_2.5_detect  ~ surveyyear, data = out$summary, col = "red", lty = 2)
lines(post_97.5_detect  ~ surveyyear, data = out$summary, col = "red", lty = 2)
lines(post_2.5_psi  ~ surveyyear, data = out$summary, col = "green", lty = 2)
lines(post_97.5_psi  ~ surveyyear, data = out$summary, col = "green", lty = 2)






out$BUGSoutput$mean$pdet.alpha 

warnings(out$BUGSoutput$mean$pdet.alpha)

out$BUGSoutput$mean$psi.fs
out$BUGSoutput$"2.5%"






out$BUGSoutput$summary[ 4:4, 6:19]
  names(OccResults) <- OccMods
  
  
  Occ_trends <- as.data.frame(t(sapply(
    OccResults, function(x) x['regres.psi',c('mean','2.5%','97.5%','Rhat')])))
  # convert to an effective p-value: just a 1 or 0 
  names(Occ_trends)[1] <- 'trend'
  Occ_trends$p <- as.numeric(0>apply(Occ_trends, 1, function(x) x['2.5%']*x['97.5%']))
  Occ_trends$name <- rownames(Occ_trends)
  Occ_trends <- melt(Occ_trends[,c(6,1,5,4)], id='name')
  
  Occ_out <- Occ_trends$value
  names(Occ_out) <- with(Occ_trends, paste0('Occ+',name,'_',variable))
  
  test[i,] <- c(Occ_out,spp)
}

names(test) <- c(names(Occ_out),"species")


