# Rscript to produce stacked area plots from landclim output files for future scenarios#
########################################################################################

setwd("/Volumes/schwoerer/schwoerer/Documents/modelling/Sardinia")
library("smooth")
library("zoo")

# read in simulation outputs

biomass_BRZ <- read.table("Baratz/BRZ_output/BRZ_044/cohorts_biomass_landtype.csv", header=TRUE, sep=",")
biomass_SaCu <- read.table("SaCurcurica/SaCu_output/SaCu_043/cohorts_biomass_landtype.csv", header=TRUE, sep=",")
biomass_chia <- read.table("Chia/Chia_output/Chia_025/cohorts_biomass_landtype.csv", header=TRUE, sep=",")

fire_BRZ <- read.table("Baratz/BRZ_output/BRZ_044/fire_summary.csv", header=TRUE, sep=",")
fire_SaCu <- read.table("SaCurcurica/SaCu_output/SaCu_043/fire_summary.csv", header=TRUE, sep=",")
fire_chia <- read.table("Chia/Chia_output/Chia_025/fire_summary.csv", header=TRUE, sep=",")

# read in climate data

BRZ_126 <- read.csv("Baratz/BRZ_ssp126.csv")
BRZ_126$smooth_T <- rollmean(BRZ_126$ann.T, k=30)

BRZ_585 <- read.csv("Baratz/BRZ_ssp585.csv")

SaCu_126 <- read.csv("SaCurcurica/SaCu_ssp126.csv")

SaCu_585 <- read.csv("SaCurcurica/SaCu_ssp585.csv")

Chia_126 <- read.csv("Chia/Chia_ssp126.csv")

Chia_585 <- read.csv("Chia/Chia_ssp585.csv")

# set color scheme
speciesNames <- c("querilex", "quercocc", "quersube", "querpube", "oleaeuro", "fraxornu", "ostrcarp", "pinuhale", "pinusilv", "arbuuned", "cistsalv", "phillati", "philangu", "pistlent", "rhamalat", "rhamoleo", "ericarbo", "ericscop", "grass")
speciesColors <- c(querilex = "lightgreen", quercocc = "yellowgreen", quersube = "darkgreen", querpube ="forestgreen", oleaeuro = "olivedrab", pinuhale = "gold", pinusilv = "gold", fraxornu = "olivedrab1", ostrcarp = "olivedrab1", arbuuned = "darkorange", cistsalv = "darkorange",  phillati = "darkorange", philangu = "darkorange", pistlent = "darkorange", rhamalat = "darkorange", rhamoleo = "darkorange", ericarbo = "dodgerblue", ericscop = "dodgerblue", grass = "dimgray")
legendColors <- c("lightgreen", "yellowgreen", "darkgreen","forestgreen", "olivedrab", "olivedrab1",  "gold", "darkorange", "dodgerblue", "dimgray")
legendNames <- c("Quercus ilex","Quercus coccifera", "Quercus suber", "Quercus pubescens", "Olea europaea", "Pinus sp.","other trees","Mediterranean shrubs", "Erica sp.", "grass")

names(speciesColors) <- speciesNames

# prepare data for plotting

times <-  biomass_BRZ$year
times_SaCu <- biomass_SaCu$year
times_chia <- biomass_chia$year

bm_BRZ <- biomass_BRZ[,match(speciesNames, colnames(biomass_BRZ))]
lc_BRZ <- apply(bm_BRZ[,speciesNames], 1, cumsum)

bm_SaCu <- biomass_SaCu[,match(speciesNames, colnames(biomass_SaCu))]
lc_SaCu <- apply(bm_SaCu[,speciesNames], 1, cumsum)

bm_chia <- biomass_chia[,match(speciesNames, colnames(biomass_chia))]
lc_chia <- apply(bm_chia[,speciesNames], 1, cumsum)

#set cell size (in ha)
cell <- 0.0625

#sum up burnt pixels per decade

burn_BRZ <- aggregate(fire_BRZ$cellcount, by=list(decade = fire_BRZ$decade), FUN=sum)
burn_SaCu <- aggregate(fire_SaCu$cellcount, by=list(decade = fire_SaCu$decade), FUN=sum)
burn_chia <- aggregate(fire_chia$cellcount, by=list(decade = fire_chia$decade), FUN=sum)


# calculate area burnt

burn_BRZ$area <- burn_BRZ$x * cell
burn_BRZ$age <- (burn_BRZ$decade*10 + 2000)

burn_SaCu$area <- burn_SaCu$x * cell
burn_SaCu$age <- (burn_SaCu$decade*10 + 2000)

burn_chia$area <- burn_chia$x * cell
burn_chia$age <- (burn_chia$decade*10 + 2000)

#make plots

old.par <- par()

par(mfcol=c(4,3),oma=c(4,6,2,1), mar=c(1,1,1,3))

plot(BRZ_126$yearAD, BRZ_126$ann.T, type="l", col="red", xlim=c(2000,2500), ylim=c(16,24), axes=FALSE)
lines(BRZ_585$yearAD, BRZ_585$ann.T, type="l", col="firebrick")
lines(BRZ_126$yearAD[15:476],rollmean(BRZ_126$ann.T, 30), col="black")
lines(BRZ_585$yearAD[15:476],rollmean(BRZ_585$ann.T, 30), col="black")
axis(side=1, labels=FALSE)
axis(side=4, labels=FALSE)

plot(BRZ_126$yearAD, BRZ_126$ann.P, type="l", col="blue", xlim=c(2000,2500), ylim=c(200,1000), axes=FALSE)
lines(BRZ_585$yearAD, BRZ_585$ann.P, type="l", col="darkblue")
lines(BRZ_126$yearAD[15:476],rollmean(BRZ_126$ann.P, 30), col="black")
lines(BRZ_585$yearAD[15:476],rollmean(BRZ_585$ann.P, 30), col="black")
axis(side=1, labels=FALSE)
axis(side=2)

plot(0,0, type="n", xlim=c(2000,2500), ylim=c(0, 150), ylab="Biomass (t/ha)", axes=FALSE)
 for (i in rev(speciesNames)) {
     polygon(x=c(times[1], times, times[length(times)]), c(0, lc_BRZ[i,], 0), col=speciesColors[i], border=NA)}
 mtext("Baratz", side=3, cex=1.5, line=0, outer =TRUE, adj=0.15)
 mtext("Biomass(t/ha)", side=2, line=2.8)
 axis(side=1, at=seq(2000,2500,100), labels=FALSE)
 axis(side=2)
 box()
 
 plot(rev(burn_BRZ$age), burn_BRZ$area, type="h", axes=FALSE, xlim=c(2000,2500), ylim=c(1,20000))
 axis(side=1, labels=TRUE, at=seq(2000,2500,100))
 mtext("Area burned (ha)", side=2, line=2.8)
 mtext("Age (cal. BP)", side=1, outer=TRUE, line=2.2)
 axis(side=2)
 box()
 
 plot(SaCu_126$yearAD, SaCu_126$ann.T, type="l", col="red", xlim=c(2000,2500), ylim=c(16,24), axes=FALSE)
 lines(SaCu_585$yearAD, SaCu_585$ann.T, type="l", col="firebrick")
 lines(SaCu_126$yearAD[15:476],rollmean(SaCu_126$ann.T, 30), col="black")
 lines(SaCu_585$yearAD[15:476],rollmean(SaCu_585$ann.T, 30), col="black")
 axis(side=1, labels=FALSE)
 axis(side=4, labels=FALSE)
 
 plot(SaCu_126$yearAD, SaCu_126$ann.P, type="l", col="blue", xlim=c(2000,2500), ylim=c(200,1000), axes=FALSE)
 lines(SaCu_585$yearAD, SaCu_585$ann.P, type="l", col="darkblue")
 lines(SaCu_126$yearAD[15:476],rollmean(SaCu_126$ann.P, 30), col="black")
 lines(SaCu_585$yearAD[15:476],rollmean(SaCu_585$ann.P, 30), col="black")
 axis(side=1, labels=FALSE)
 axis(side=2, labels=FALSE)
 
 plot(0,0, type="n", xlim=c(2000,2500), ylim=c(0, 150), axes=FALSE)
 for (i in rev(speciesNames)) {
   polygon(x=c(times_SaCu[1], times_SaCu, times_SaCu[length(times_SaCu)]), c(0, lc_SaCu[i,], 0), col=speciesColors[i], border=NA)}
 mtext("Sa Curcurica", side=3, cex=1.5, line=0, outer =TRUE)
 axis(side=1, labels=FALSE, at=seq(2000,2500,100))
 axis(side=2, labels=FALSE)
 box()
 
 plot(rev(burn_SaCu$age), burn_SaCu$area, type="h", axes=FALSE, xlim=c(2000,2500), ylim=c(1,20000))
 axis(side=1, labels=TRUE, at=seq(2000,2500,100))
 axis(side=2, labels=FALSE)
 box()
 
 plot(Chia_126$yearAD, Chia_126$ann.T, type="l", col="red",  xlim=c(2000,2500), ylim=c(16,24), axes=FALSE)
 lines(Chia_585$yearAD, Chia_585$ann.T, type="l", col="firebrick")
 lines(Chia_126$yearAD[15:476],rollmean(Chia_126$ann.T, 30), col="black")
 lines(Chia_585$yearAD[15:476],rollmean(Chia_585$ann.T, 30), col="black")
 axis(side=1, labels=FALSE)
 axis(side=4)
 
 plot(Chia_126$yearAD, Chia_126$ann.P, type="l", col="blue", xlim=c(2000,2500), ylim=c(200,1000), axes=FALSE)
 lines(Chia_585$yearAD, Chia_585$ann.P, type="l", col="darkblue")
 lines(Chia_126$yearAD[15:476],rollmean(Chia_126$ann.P, 30), col="black")
 lines(Chia_585$yearAD[15:476],rollmean(Chia_585$ann.P, 30), col="black")
 axis(side=1, labels=FALSE)
 axis(side=2, labels=FALSE)
 
 plot(0,0, type="n", xlim=c(2000,2500), ylim=c(0, 150), axes=FALSE)
 for (i in rev(speciesNames)) {
   polygon(x=c(times_chia[1], times_chia, times_chia[length(times_chia)]), c(0, lc_chia[i,], 0), col=speciesColors[i], border=NA)}
 mtext("Chia", side=3, cex=1.5, line=0, outer =TRUE, adj=0.85)
 axis(side=1, labels=FALSE, at=seq(2000,2500,100))
 axis(side=2, labels=FALSE)
 #legend("topright", legend=legendNames, fill=legendColors, cex=1.2, bty="n")
 box()
 
 plot(rev(burn_chia$age), burn_chia$area, type="h", axes=FALSE, xlim=c(2000,2500), ylim=c(1,20000))
 axis(side=1, labels=TRUE, at=seq(2000,2500,100))
 axis(side=2, labels=FALSE)
 box()
 
par(old.par)
  
