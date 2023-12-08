# Rscript to produce stacked area plots from landclim output files and pollen data#
###################################################################################

# read in simulation outputs

biomass_BRZ <- read.table("Baratz/BRZ_output/BRZ_053/cohorts_biomass_landtype.csv", header=TRUE, sep=",")
biomass_SaCu <- read.table("SaCurcurica/SaCu_output/SaCu_054/cohorts_biomass_landtype.csv", header=TRUE, sep=",")
biomass_chia <- read.table("Chia/Chia_output/Chia_036/cohorts_biomass_landtype.csv", header=TRUE, sep=",")

fire_BRZ <- read.table("Baratz/BRZ_output/BRZ_053/fire_summary.csv", header=TRUE, sep=",")
fire_SaCu <- read.table("SaCurcurica/SaCu_output/SaCu_054/fire_summary.csv", header=TRUE, sep=",")
fire_chia <- read.table("Chia/Chia_output/Chia_036/fire_summary.csv", header=TRUE, sep=",")

#read in pollen data

pollen_BRZ <- read.table("Baratz/Pollen_BRZ.csv", header=TRUE, sep=",")
pollen_SaCu <- read.table("SaCurcurica/Pollen_SaCu.csv", header=TRUE, sep=",")
pollen_chia <- read.table("Chia/Pollen_chia.csv", header=TRUE, sep=",")

# read in climate data
T_Verdarolo <- read.table("Baratz/climate_data/T_Verdarolo.txt", header=TRUE)
P_BRZ <- read.csv("Baratz/Baratz_trace_precip.csv", header=TRUE)
P_SaCu <- read.csv("SaCurcurica/SaCurcurica_trace_precip.csv", header=TRUE)
P_Chia <- read.csv("Chia/Chia_trace_precip.csv", header=TRUE)

# summarize according to modelling output

pollen_BRZ$med_shrub <- pollen_BRZ$Abt + pollen_BRZ$Cis + pollen_BRZ$Pfl + pollen_BRZ$Pis
pollen_BRZ$deciduous <- pollen_BRZ$Bet + pollen_BRZ$Fra.or + pollen_BRZ$Ost 
poll_BRZ <- data.frame(pollen_BRZ$Que.il.t, pollen_BRZ$Que.su.t, pollen_BRZ$Que.pu.t, pollen_BRZ$Ole, pollen_BRZ$Fra.or, pollen_BRZ$Ost, pollen_BRZ$Pin, pollen_BRZ$Abt, pollen_BRZ$Cis, pollen_BRZ$Pfl, pollen_BRZ$Pis, pollen_BRZ$Erc.ar.t, pollen_BRZ$Poaeae)
names(poll_BRZ) <- c("querilex", "quersube", "querpube", "oleaeuro", "fraxornu", "ostrcarp", "pinuhale", "arbuuned", "cistsalv", "phillati", "pistlent", "ericarbo", "grass")

pollen_SaCu$med_shrub <- pollen_SaCu$Abt + pollen_SaCu$Cis + pollen_SaCu$Pfl + pollen_SaCu$Pis
pollen_SaCu$deciduous <- pollen_SaCu$Bet + pollen_SaCu$Fra.or + pollen_SaCu$Ost
poll_SaCu <- data.frame(pollen_SaCu$Que.il.t, pollen_SaCu$Que.ce.t, pollen_SaCu$Que.ro.t, pollen_SaCu$Ole, pollen_SaCu$Fra.or, pollen_SaCu$Ost, pollen_SaCu$Pin, pollen_SaCu$Abt, pollen_SaCu$Cis, pollen_SaCu$Pfl, pollen_SaCu$Pis, pollen_SaCu$Erc, pollen_SaCu$Poaeae)
names(poll_SaCu) <- c("querilex", "quersube", "querpube", "oleaeuro", "fraxornu", "ostrcarp", "pinuhale", "arbuuned", "cistsalv", "phillati", "pistlent", "ericarbo", "grass")

pollen_chia$med_shrub <- pollen_chia$Arbutus + pollen_chia$Cistus.albidus.type + pollen_chia$Cistus.monspeliensis.type + pollen_chia$Phillyrea + pollen_chia$Pistacia
pollen_chia$deciduous <- pollen_chia$Betula + pollen_chia$Fraxinus.ornus + pollen_chia$Ostrya.carpinifolia.type
poll_chia <- data.frame(pollen_chia$Quercus.ilex.type, pollen_chia$Quercus.suber.type, pollen_chia$Quercus.pubescens.type, pollen_chia$Olea.europaea, pollen_chia$Fraxinus.ornus, pollen_chia$Ostrya.carpinifolia.type, pollen_chia$Pinus, pollen_chia$Arbutus, pollen_chia$Cistus.albidus.type, pollen_chia$Phillyrea, pollen_chia$Pistacia, pollen_chia$Erica.arborea.type, pollen_chia$Poaceae)
names(poll_chia) <- c("querilex", "quersube", "querpube", "oleaeuro", "fraxornu", "ostrcarp", "pinuhale", "arbuuned", "cistsalv", "phillati", "pistlent", "ericarbo", "grass")

# set color scheme
speciesNames <- c("grass","querilex", "quercocc", "quersube", "querpube", "oleaeuro", "fraxornu", "ostrcarp", "pinuhale", "arbuuned", "cistsalv", "phillati", "philangu", "pistlent", "rhamalat", "rhamoleo", "ericarbo", "ericscop")
speciesPollen <- c("querilex", "quersube", "querpube", "oleaeuro", "fraxornu", "ostrcarp", "pinuhale",  "arbuuned", "cistsalv", "phillati", "pistlent", "ericarbo", "grass")
speciesColors <- c(grass = "dimgray", querilex = "lightgreen", quercocc = "olivedrab1", quersube = "darkgreen", querpube ="forestgreen", oleaeuro = "olivedrab", fraxornu = "purple3", ostrcarp = "purple3", pinuhale = "gold",  arbuuned = "darkorange", cistsalv = "darkorange",  phillati = "darkorange", philangu = "darkorange", pistlent = "darkorange", rhamalat = "darkorange", rhamoleo = "darkorange", ericarbo = "dodgerblue", ericscop = "dodgerblue")
speciesColorsPollen <- c(querilex = "lightgreen", quersube = "darkgreen", querpube ="forestgreen", oleaeuro = "olivedrab", fraxornu = "purple3", ostrcarp = "purple3", pinuhale = "gold", arbuuned = "darkorange", cistsalv = "darkorange",  phillati = "darkorange", pistlent = "darkorange", ericarbo = "dodgerblue", grass = "dimgray")
legendColors <- c("lightgreen", "darkgreen","forestgreen", "olivedrab", "purple3",  "gold", "darkorange", "dodgerblue", "dimgray")
legendNames <- c("Quercus ilex/cocc.", "Quercus suber", "Quercus pubescens", "Olea europaea", "other deciduous", "Pinus spp.", "Mediterranean shrubs", "Erica spp.", "grass")

names(speciesColors) <- speciesNames
names(speciesColorsPollen) <- speciesPollen

# prepare data for plotting

times <-  -8900 + (biomass_BRZ$year)
times_SaCu <- -8900 + (biomass_SaCu$year)
times_chia <- -8900 + (biomass_chia$year)

times_pol_BRZ <- pollen_BRZ$Age_clam20*-1
times_pol_SaCu <- pollen_SaCu$Age_clam20*-1
times_pol_chia <- pollen_chia$Age_clam20*-1

bm_BRZ <- biomass_BRZ[,match(speciesNames, colnames(biomass_BRZ))]
lc_BRZ <- apply(bm_BRZ[,speciesNames], 1, cumsum)

bm_SaCu <- biomass_SaCu[,match(speciesNames, colnames(biomass_SaCu))]
lc_SaCu <- apply(bm_SaCu[,speciesNames], 1, cumsum)

bm_chia <- biomass_chia[,match(speciesNames, colnames(biomass_chia))]
lc_chia <- apply(bm_chia[,speciesNames], 1, cumsum)

pol_BRZ <- apply(poll_BRZ[,speciesPollen], 1, cumsum)
pol_SaCu <- apply(poll_SaCu[,speciesPollen], 1, cumsum)
pol_chia <- apply(poll_chia[,speciesPollen], 1, cumsum)

char_z_BRZ <- (pollen_BRZ$infl - mean(pollen_BRZ$infl)) / sd(pollen_BRZ$infl)
char_z_SaCu <- (pollen_SaCu$Influx - mean(pollen_SaCu$Influx)) / sd(pollen_SaCu$Influx)
char_z_Chia <- (pollen_chia$influx - mean(pollen_chia$influx)) / sd(pollen_chia$influx)

#set cell size (in ha)
cell <- 0.0625

#sum up burnt pixels per decade

burn_BRZ <- aggregate(fire_BRZ$cellcount, by=list(decade = fire_BRZ$decade), FUN=sum)
burn_SaCu <- aggregate(fire_SaCu$cellcount, by=list(decade = fire_SaCu$decade), FUN=sum)
burn_chia <- aggregate(fire_chia$cellcount, by=list(decade = fire_chia$decade), FUN=sum)


# calculate area burnt

burn_BRZ$area <- burn_BRZ$x * cell
burn_BRZ$age <- burn_BRZ$decade*-10

burn_SaCu$area <- burn_SaCu$x * cell
burn_SaCu$age <- burn_SaCu$decade*-10

burn_chia$area <- burn_chia$x * cell
burn_chia$age <- burn_chia$decade*-10


#make plots

old.par <- par()

par(mfcol=c(6,3),oma=c(4,6,2,1), mar=c(1,1,1,1))

plot(T_Verdarolo$Year, T_Verdarolo$Jan+16.2, type="l", xlim=c(-8000,0), ylim=c(15,20), axes=FALSE, col="red")
axis(side=1, at=seq(-8000,0,1000), labels=FALSE)
axis(side=4, labels=FALSE)
box()

plot(P_BRZ$Age, rowSums(P_BRZ[,1:12]), type="l", xlim=c(-8000,0), ylim=c(560,680), axes=FALSE, col="blue")
axis(side=1, at=seq(-8000,0,1000), labels=FALSE)
axis(side=2)
box()

plot(0,0, type="n", xlim=c(-8000,0), ylim=c(0, 150), ylab="Biomass (t/ha)", axes=FALSE)
 for (i in rev(speciesNames)) {
     polygon(x=c(times[1], times, times[length(times)]), c(0, lc_BRZ[i,], 0), col=speciesColors[i], border=NA)}
 mtext("Baratz", side=3, cex=1.5, line=0, outer =TRUE, adj=0.15)
 mtext("Biomass(t/ha)", side=2, line=2.8)
 axis(side=1, at=seq(-8000,0,1000), labels=FALSE)
 axis(side=2)
 box()
 
 plot(rev(burn_BRZ$age), burn_BRZ$area, type="h", axes=FALSE, xlim=c(-8000,0), ylim=c(1,20000))
 axis(side=1, labels=FALSE, at=seq(-8000,0,1000))
 mtext("Area burned (ha)", side=2, line=2.8)
 mtext("Age (cal. BP)", side=1, outer=TRUE, line=2.2)
 axis(side=2)
 box()
 
 plot(0,0, type="n", xlim=c(-8000,0), ylim=c(0, 100), ylab="Pollen (%)", axes=FALSE)
 for (j in rev(speciesPollen)) {
    polygon(x=c(times_pol_BRZ[1], times_pol_BRZ, times_pol_BRZ[length(times_pol_BRZ)]), c(0, pol_BRZ[j,], 0), col=speciesColorsPollen[j], border=NA)}
 mtext("Pollen (%)", side=2, line=2.8)
 axis(side=1,at=seq(-8000,0,1000), labels=FALSE)
 axis(side=2)
 box()
 
 plot(times_pol_BRZ, pollen_BRZ$Influx_clam20z, type="l", xlim=c(-8000,0), ylim=c(-1,5), axes=FALSE)
 mtext("Charcoal influx (part. cm-2 yr-1)", side=2, line=2.8)
 axis(side=1, at=seq(-8000,0,1000), labels=rev(seq(0,8000,1000)))
 axis(side=2)
 box()
 
 plot(T_Verdarolo$Year, T_Verdarolo$Jan+16.54, type="l", xlim=c(-8000,0), ylim=c(15,20), axes=FALSE, col="red")
 axis(side=1, at=seq(-8000,0,1000), labels=FALSE)
 axis(side=4, labels=FALSE)
 box()
 
 plot(P_SaCu$Age, rowSums(P_SaCu[,2:13]), type="l", xlim=c(-8000,0), ylim=c(560,680), axes=FALSE, col="blue")
 axis(side=1, at=seq(-8000,0,1000), labels=FALSE)
 axis(side=2, labels=FALSE)
 box()
 
 plot(0,0, type="n", xlim=c(-8000,0), ylim=c(0, 150), axes=FALSE)
 for (i in rev(speciesNames)) {
   polygon(x=c(times_SaCu[1], times_SaCu, times_SaCu[length(times_SaCu)]), c(0, lc_SaCu[i,], 0), col=speciesColors[i], border=NA)}
 mtext("Sa Curcurica", side=3, cex=1.5, line=0, outer =TRUE)
 axis(side=1, labels=FALSE, at=seq(-8000,0,1000))
 axis(side=2, labels=FALSE)
 box()
 
 plot(rev(burn_SaCu$age), burn_SaCu$area, type="h", axes=FALSE, xlim=c(-8000,0), ylim=c(1,20000))
 axis(side=1, labels=FALSE, at=seq(-8000,0,1000))
 axis(side=2, labels=FALSE)
 box()
 
 plot(0,0, type="n", xlim=c(-8000,0), ylim=c(0, 100), axes=FALSE)
 for (j in rev(speciesPollen)) {
    polygon(x=c(times_pol_SaCu[1], times_pol_SaCu, times_pol_SaCu[length(times_pol_SaCu)]), c(0, pol_SaCu[j,], 0), col=speciesColorsPollen[j], border=NA)}
 axis(side=1, at=seq(-8000,0,1000), labels=FALSE)
 axis(side=2, labels=FALSE)
 box()
 
 plot(times_pol_SaCu, pollen_SaCu$Influx_clam20z, type="l", xlim=c(-8000,0), ylim=c(-1,5), axes=FALSE)
 axis(side=1, at=seq(-8000,0,1000), labels=rev(seq(0,8000,1000)))
 axis(side=2)
 box()
 
 plot(T_Verdarolo$Year, T_Verdarolo$Jan+17.14, type="l", xlim=c(-8000,0), ylim=c(15,20), axes=FALSE, col="red")
 axis(side=1, at=seq(-8000,0,1000), labels=FALSE)
 axis(side=4)
 box()
 
 plot(P_Chia$Age, rowSums(P_Chia[,2:13]), type="l", xlim=c(-8000,0), ylim=c(560,680), axes=FALSE, col="blue")
 axis(side=1, at=seq(-8000,0,1000), labels=FALSE)
 axis(side=2, labels=FALSE)
 box()
 
 plot(0,0, type="n", xlim=c(-8000,0), ylim=c(0, 150), axes=FALSE)
 for (i in rev(speciesNames)) {
   polygon(x=c(times_chia[1], times_chia, times_chia[length(times_chia)]), c(0, lc_chia[i,], 0), col=speciesColors[i], border=NA)}
 mtext("Chia", side=3, cex=1.5, line=0, outer =TRUE, adj=0.85)
 axis(side=1, labels=FALSE, at=seq(-8000,0,1000))
 axis(side=2, labels=FALSE)
 box()
 
 plot(rev(burn_chia$age), burn_chia$area, type="h", axes=FALSE, xlim=c(-8000,0), ylim=c(1,20000))
 axis(side=1, labels=FALSE, at=seq(-8000,0,1000))
 axis(side=2, labels=FALSE)
 box()
 
 plot(0,0, type="n", xlim=c(-8000,0), ylim=c(0, 100), axes=FALSE)
  for (j in rev(speciesPollen)) {
    polygon(x=c(times_pol_chia[1], times_pol_chia, times_pol_chia[length(times_pol_chia)]), c(0, pol_chia[j,], 0), col=speciesColorsPollen[j], border=NA)}
 axis(side=1, at=seq(-8000,0,1000), labels=FALSE)
 axis(side=2, labels=FALSE)
 box()

 plot(times_pol_chia, pollen_chia$Influx_clam20z, type="l", xlim=c(-8000,0), ylim=c(-1,5), axes=FALSE)
 axis(side=1, at=seq(-8000,0,1000), labels=rev(seq(0,8000,1000)))
 axis(side=2)
 #legend("topright", legend=legendNames, fill=legendColors, cex=0.9, bty="n")
 box()
 
par(old.par)
  