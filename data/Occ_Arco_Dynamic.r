#simple occupancy model with data in long format (one line per visit) AND DATATYPE
#Also known as 'OccMod2', LC edited to include 'process model' i.e. 'memory' in the state model
model {                       

# Priors 
# state model priors
for(t in 1:nyear){
#a[t] ~ dunif(-10,10) # No longer necessary,  LC removed
gamma[t] ~ dunif(0,1) # Colonisation probabilities by year
}                 

#for (i in 1:nsite) {
#eta[i] ~ dnorm(mu2, tau2)       # extra random site-effect op occ LC edit, removed
#} 

#mu2 ~ dnorm(0, 0.001) # site effect variance removed
tau2 <- 1/(sigma2 * sigma2)
sigma2 ~ dunif(0, 5)
phi ~ dunif(0, 1) # persistence is same across years and sites
psi.fs[1] ~ dnorm(0, 1000)I(0, 1) # LC edit, strong prior that no sites were occupied previous to 2001

# observation model priors 
for (t in 1:nyear) {
alpha.p[t] ~ dnorm(mu.lp, tau.lp)            # p random year
}

mu.lp ~ dnorm(0, 0.01)                         
tau.lp <- 1 / (sd.lp * sd.lp)                 
sd.lp ~ dunif(0, 5)   

dtype2.p ~ dunif(dtype2p_min,dtype2p_max) #ignore for now
dtype3.p ~ dunif(dtype3p_min,dtype3p_max) #ignore for now

# State model
for (i in 1:nsite){ 
for (t in 1:nyear){   
z[i,t] ~ dbern(muZ[i,t]) # True occupancy z at site i
muZ[i,t]<- z[i,t-1]*phi + (1 - z[i,t-1])*gamma[t-1] # No random site effect, 
													# so no need for logit as only probabilities affect muZ.
													# Now includes phi, global persistence prob; gamma, 
													# yearly colonisation prob
}}   

# Observation model 
# attempt 1: go through the visits and find the matching year and site identity
for(j in 1:nvisit) {
#for each visit, find the matching site and year identities
Py[j]<- z[Site[j],Year[j]]*p[j]      
logit(p[j]) <- alpha.p[Year[j]]  + dtype2.p*DATATYPE2[j] + dtype3.p*DATATYPE3[j] 
y[j] ~ dbern(Py[j])  
Presi[j] <- abs(y[j]-p[j])
y.new[j] ~ dbern(Py[j]) 
Presi.new[j] <- abs(y.new[j]-p[j])
}

# Bayesian Goodness-of-Fit
fit <- sum(Presi[])
fit.new <- sum(Presi.new[])

# Derived parameters state model

# Finite sample occupancy
for (t in 1:nyear) {  
psi.fs[t] <- sum(z[1:nsite,t])/nsite
} 

# Overall trend in occpuancy
sumY <- sum(psi.fs[1:nyear])
for (t in 1:nyear) {
sumxy[t] <- psi.fs[t]*t
}
sumXY <- sum(sumxy[1:nyear])
regres.psi <- (sumXY - ((sumX*sumY)/nyear))/(sumX2 - ((sumX*sumX)/nyear))

# Derived parameters observation model
for (t in 1:nyear) {          
pdet.alpha[t] <- exp(alpha.p[t])/(1 + exp(alpha.p[t])) 
pdet.d2[t] <- exp(alpha.p[t]+dtype2.p)/(1 + exp(alpha.p[t]+dtype2.p))
pdet.d3[t] <- exp(alpha.p[t]+dtype3.p)/(1 + exp(alpha.p[t]+dtype3.p))
}

# overall trend in pdet.alpha
sumYpdet <- sum(pdet.alpha[1:nyear])  
for (t in 1:nyear) {          
sumxypdet[t] <- pdet.alpha[t]*t
}
sumXYpdet <- sum(sumxypdet[1:nyear])
regres.pdet <- (sumXYpdet - ((sumX*sumYpdet)/nyear))/(sumX2 - ((sumX*sumX)/nyear))

# end of model formulation
}    			