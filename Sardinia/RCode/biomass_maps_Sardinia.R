#R-script to plot LandClim data based on the LandClim Tutorial by Timothy Thrippleton, ETH and tweaked by Christoph Schwörer, UniBE#

library("ggplot2")
library("RColorBrewer")
library("dplyr")

# set color scheme
speciesNames <- c("querilex", "quercocc", "quersube", "querpube", "oleaeuro", "fraxornu", "ostrcarp", "pinuhale", "pinusilv", "arbuuned", "cistsalv", "phillati", "philangu", "pistlent", "rhamalat", "rhamoleo", "ericarbo", "ericscop", "grass")
speciesColors <- c(querilex = "lightgreen", quercocc = "yellowgreen", quersube = "darkgreen", querpube ="forestgreen", oleaeuro = "olivedrab", pinuhale = "gold", pinusilv = "gold", fraxornu = "olivedrab1", ostrcarp = "olivedrab1", arbuuned = "darkorange", cistsalv = "darkorange",  phillati = "darkorange", philangu = "darkorange", pistlent = "darkorange", rhamalat = "darkorange", rhamoleo = "darkorange", ericarbo = "dodgerblue", ericscop = "dodgerblue", grass = "dimgray")
legendColors <- c("lightgreen", "yellowgreen", "darkgreen","forestgreen", "olivedrab", "olivedrab1",  "gold", "darkorange", "dodgerblue", "dimgray")
legendNames <- c("Quercus ilex","Quercus coccifera", "Quercus suber", "Quercus pubescens", "Olea europaea", "Pinus sp.","other trees","Mediterranean shrubs", "Erica sp.", "grass")

setwd("/Volumes/schwoerer/schwoerer/Documents")

# make maps from the fullouts showing dominant species per pixel

### Baratz ###

# BRZ low fire, 8000 BP

fullout <- read.csv("modelling/Sardinia/Baratz/BRZ_output/BRZ_033/cohorts_detail_year_900.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/BRZ_lowfire_8000BP.pdf", width=10.63, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# BRZ high fire, 8000 BP

fullout <- read.csv("modelling/Sardinia/Baratz/BRZ_output/BRZ_035/cohorts_detail_year_900.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/BRZ_highfire_8000BP.pdf", width=10.63, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# BRZ ssp126 low fire 2300

fullout <- read.csv("modelling/Sardinia/Baratz/BRZ_output/BRZ_040/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/BRZ_126_low_2300.pdf", width=10.63, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# BRZ ssp126 high fire 2300

fullout <- read.csv("modelling/Sardinia/Baratz/BRZ_output/BRZ_042/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/BRZ_126_high_2300.pdf", width=10.63, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# BRZ ssp370 low fire 2300

fullout <- read.csv("modelling/Sardinia/Baratz/BRZ_output/BRZ_041/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/BRZ_370_low_2300.pdf", width=10.63, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# BRZ ssp370 high fire 2300

fullout <- read.csv("modelling/Sardinia/Baratz/BRZ_output/BRZ_043/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/BRZ_370_high_2300.pdf", width=10.63, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# BRZ ssp585 low fire 2300

fullout <- read.csv("modelling/Sardinia/Baratz/BRZ_output/BRZ_044/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/BRZ_585_low_2300.pdf", width=10.63, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# BRZ ssp585 high fire 2300

fullout <- read.csv("modelling/Sardinia/Baratz/BRZ_output/BRZ_045/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/BRZ_585_high_2300.pdf", width=10.63, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

###SaCurcurica ###

# SaCu low fire, 8000 BP

fullout <- read.csv("modelling/Sardinia/SaCurcurica/SaCu_output/SaCu_029/cohorts_detail_year_900.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/SaCu_lowfire_8000BP.pdf", width=9.33, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# SaCu high fire, 8000 BP

fullout <- read.csv("modelling/Sardinia/SaCurcurica/SaCu_output/SaCu_033/cohorts_detail_year_900.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/SaCu_highfire_8000BP.pdf", width=9.33, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# SaCu ssp126 low fire 2300

fullout <- read.csv("modelling/Sardinia/SaCurcurica/SaCu_output/SaCu_039/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/SaCu_ssp126_low_2300.pdf", width=9.33, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# SaCu ssp126 high fire 2300

fullout <- read.csv("modelling/Sardinia/SaCurcurica/SaCu_output/SaCu_041/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/SaCu_ssp126_high_2300.pdf", width=9.33, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# SaCu ssp370 low fire 2300

fullout <- read.csv("modelling/Sardinia/SaCurcurica/SaCu_output/SaCu_040/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/SaCu_ssp370_low_2300.pdf", width=9.33, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# SaCu ssp370 high fire 2300

fullout <- read.csv("modelling/Sardinia/SaCurcurica/SaCu_output/SaCu_042/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/SaCu_ssp370_high_2300.pdf", width=9.33, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# SaCu ssp585 low fire 2300

fullout <- read.csv("modelling/Sardinia/SaCurcurica/SaCu_output/SaCu_043/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/SaCu_ssp585_low_2300.pdf", width=9.33, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# SaCu ssp585 high fire 2300

fullout <- read.csv("modelling/Sardinia/SaCurcurica/SaCu_output/SaCu_044/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/SaCu_ssp585_high_2300.pdf", width=9.33, height=10.07)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

### Chia ###

# CHIA low fire, 8000 BP

fullout <- read.csv("modelling/Sardinia/Chia/Chia_output/CHIA_013/cohorts_detail_year_900.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/CHIA_lowfire_8000BP.pdf", width=10.05, height=7.13)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# CHIA high fire, 8000 BP

fullout <- read.csv("modelling/Sardinia/Chia/Chia_output/CHIA_018/cohorts_detail_year_900.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/CHIA_highfire_8000BP.pdf", width=10.05, height=7.13)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# CHIA ssp126 low fire 2300

fullout <- read.csv("modelling/Sardinia/Chia/Chia_output/CHIA_021/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/CHIA_ssp126_low_2300.pdf", width=10.05, height=7.13)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# CHIA ssp126 high fire 2300

fullout <- read.csv("modelling/Sardinia/Chia/Chia_output/CHIA_023/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/CHIA_ssp126_high_2300.pdf", width=10.05, height=7.13)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# CHIA ssp370 low fire 2300

fullout <- read.csv("modelling/Sardinia/Chia/Chia_output/CHIA_022/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/CHIA_ssp370_low_2300.pdf", width=10.05, height=7.13)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# CHIA ssp370 high fire 2300

fullout <- read.csv("modelling/Sardinia/Chia/Chia_output/CHIA_024/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/CHIA_ssp370_high_2300.pdf", width=10.05, height=7.13)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# CHIA ssp585 low fire 2300

fullout <- read.csv("modelling/Sardinia/Chia/Chia_output/CHIA_025/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/CHIA_ssp585_low_2300.pdf", width=10.05, height=7.13)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()

# CHIA ssp585 high fire 2300

fullout <- read.csv("modelling/Sardinia/Chia/Chia_output/CHIA_026/cohorts_detail_year_2300.csv", header=TRUE, sep=",")
fullout_aggregate_biomass <- fullout %>% group_by(row, col) %>% summarise(CellBiomass = sum(biomass))
dominantSpecies <- fullout %>% mutate(cohortBiomass = biomass) %>% group_by(row, col, cell, species) %>% summarise(speciesBiomass = sum(cohortBiomass), height = max(height)) %>% group_by(row, col, cell) %>% filter(speciesBiomass == max(speciesBiomass)) %>% filter(height == max(height))
pdf("Sardinia/LandClim_maps/New_color_scheme/CHIA_ssp585_high_2300.pdf", width=10.05, height=7.13)
ggplot(dominantSpecies) + geom_raster(aes(x=col, y=row*-1, fill=species)) + scale_fill_manual(values=speciesColors) + theme_bw()
dev.off()
