?prcomp
?read.csv
rm(list=ls()) # clear R

getwd()

LCM <-read.table("data/LCM_ONE_KM_PERCENTAGES.csv", header =T, sep =",")



names(LCM)<-c("gridref","Broadleaf woodland","Coniferous woodland", "Arable", 
              "Improved grassland", "Rough grassland", "Neutral grassland", 
              "Calcareous grassland", "Acid grassland", "Fen, marsh and swamp", 
              "Heather", "Heather grassland", "Montane habitats", "Inland rock", 
              "Saltwater", "Freshwater", "Supra-littoral rock", 
              "Supra-littoral sediment", "Littoral rock", "Littoral sediment", 
              "Saltmarsh", "Urban", "Suburban")

names(LCM)

head(LCM, 10)

lcm.pca<-princomp(LCM[1:1000,2:24])

screeplot(lcm.pca)


biplot(lcm.pca)

biplot(lcm.pca, c(1,3))

biplot(lcm.pca, c(2,3))

biplot(lcm.pca, c(2,3))

