# CPC Rainfall index- basic grid stats
# 11/16/18 MAC

library(raster)
library(rasterVis)

# set rasteroptions
rasterOptions(progress = 'text')

# Download States boundaries (might take time)
states <- getData('GADM', country='United States', level=1)
counties<-getData('GADM', country='USA', level=2)
#county<-subset(us,NAME_2==countyName)

# thin out states
SWstates    <- c('California', 'Nevada', 'Utah', 'Colorado', 'Arizona','New Mexico','Texas')
states <- states[states$NAME_1 %in% SWstates,]

# load CPC data layers
cpc.moSumPrecip<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/moSumPrecip_AZNM_cpc_precip_conus_1948_2017.grd")
cpc.moAvg<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/moAvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd")
cpc.twoMoSumClimo<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/mo2AvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd")
cpc.percAvgPrecip<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/mo2percAvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd")
cpc.twoMoSumPrecip<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/mo2sumAvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd")

# load ClimGrid data layers
clim.moSumPrecip<-stack("/scratch/crimmins/climgrid/processed/AZNM_RainIndex/resamp_moSumPrecip_AZNM_climgrid_1948_2017.grd")
clim.moAvg<-stack("/scratch/crimmins/climgrid/processed/AZNM_RainIndex/resamp_moAvgPrecip_AZNM_climgrid_1948_2017.grd")
clim.twoMoSumClimo<-stack("/scratch/crimmins/climgrid/processed/AZNM_RainIndex/resamp_mo2AvgPrecip_AZNM_climgrid_1948_2017.grd")
clim.percAvgPrecip<-stack("/scratch/crimmins/climgrid/processed/AZNM_RainIndex/resamp_mo2percAvgPrecip_AZNM_climgrid_1948_2017.grd")
clim.twoMoSumPrecip<-stack("/scratch/crimmins/climgrid/processed/AZNM_RainIndex/resamp_mo2sumAvgPrecip_AZNM_climgrid_1948_2017.grd")

# load ClimGrid data layers
prism.moSumPrecip<-stack("/scratch/crimmins/PRISM/monthly/processed/AZNM_RainIndex/resamp_moSumPrecip_AZNM_PRISM_1948_2017.grd")
prism.moAvg<-stack("/scratch/crimmins/PRISM/monthly/processed/AZNM_RainIndex/resamp_moAvgPrecip_AZNM_PRISM_1948_2017.grd")
prism.twoMoSumClimo<-stack("/scratch/crimmins/PRISM/monthly/processed/AZNM_RainIndex/resamp_mo2AvgPrecip_AZNM_PRISM_1948_2017.grd")
prism.percAvgPrecip<-stack("/scratch/crimmins/PRISM/monthly/processed/AZNM_RainIndex/resamp_mo2percAvgPrecip_AZNM_PRISM_1948_2017.grd")
prism.twoMoSumPrecip<-stack("/scratch/crimmins/PRISM/monthly/processed/AZNM_RainIndex/resamp_mo2sumAvgPrecip_AZNM_PRISM_1948_2017.grd")

# plot means
library(RColorBrewer)
myTheme<-rasterTheme(region=(brewer.pal(9, "YlGnBu")))

my.at <- seq(0, 10, 1)
p<-levelplot(cpc.twoMoSumClimo/25.4, layout=c(4,3),at=my.at, par.settings = myTheme,
                   margin=FALSE, main="CPC - Mean two-month interval precip (in)", colorkey=list(space="bottom"))  + layer(sp.polygons(states))
p<-levelplot(prism.twoMoSumClimo/25.4, layout=c(4,3),at=my.at, par.settings = myTheme,
          margin=FALSE, main="PRISM - Mean two-month interval precip", colorkey=list(space="bottom"))  + layer(sp.polygons(states))
p<-levelplot(clim.twoMoSumClimo/25.4, layout=c(4,3),at=my.at, par.settings = myTheme,
          margin=FALSE, main="ClimGrid - Mean two-month interval precip", colorkey=list(space="bottom"))  + layer(sp.polygons(states))

# plot to other format
png("/home/crimmins/RProjects/RainfallIndex/figs/ClimGrid_2mo_mean.png", width = 11, height = 7, units = "in", res = 300L)
print(p, newpage = FALSE)
dev.off()


# plot diffs in means
diffTheme<-rasterTheme(region=(brewer.pal(9, "BrBG")))

my.at <- seq(-20, 20, 2)
levelplot(cpc.twoMoSumClimo-prism.twoMoSumClimo, layout=c(4,3),at=my.at, par.settings = diffTheme,
          margin=FALSE, main="CPC-PRISM: Mean two-month interval precip", colorkey=list(space="bottom"))  #+ layer(sp.polygons(states))
my.at <- seq(-20, 20, 2)
levelplot(cpc.twoMoSumClimo-clim.twoMoSumClimo, layout=c(4,3),at=my.at, par.settings = diffTheme,
          margin=FALSE, main="CPC-ClimGrid: Mean two-month interval precip", colorkey=list(space="bottom"))  #+ layer(sp.polygons(states))

# scatterplots of climatologies
plot(cpc.twoMoSumClimo/25.4, prism.twoMoSumClimo/25.4, main="CPC vs PRISM Two-month Avg Precip")
plot(cpc.twoMoSumClimo/25.4, clim.twoMoSumClimo/25.4, main="CPC vs ClimGrid Two-month Avg Precip")


# get skew stats
#get the date from the names of the layers and extract the month
indices <- format(as.Date(names(cpc.twoMoSumPrecip), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)

# diff of median vs mean (skew)
cpc.twoMoSumPrecip.Median<- stackApply(cpc.twoMoSumPrecip, indices, fun = median)
prism.twoMoSumPrecip.Median<- stackApply(prism.twoMoSumPrecip, indices, fun = median)
clim.twoMoSumPrecip.Median<- stackApply(clim.twoMoSumPrecip, indices, fun = median)
# CPC Skew
names(cpc.twoMoSumPrecip.Median) <- month.abb
my.at <- seq(-1.75, 1.75, 0.25)
p<-levelplot(cpc.twoMoSumClimo/25.4-cpc.twoMoSumPrecip.Median/25.4, layout=c(4,3),at=my.at, par.settings = BuRdTheme,
          margin=FALSE, main="CPC Mean-Median TwoMo Climo (in)", colorkey=list(space="bottom"))  + layer(sp.polygons(states))
# plot to other format
png("/home/crimmins/RProjects/RainfallIndex/figs/CPC_2mo_mean_median.png", width = 11, height = 7, units = "in", res = 300L)
print(p, newpage = FALSE)
dev.off()



# PRISM Skew
#names(cpc.twoMoSumPrecip.Median) <- month.abb
my.at <- seq(-40, 40, 5)
levelplot(prism.twoMoSumClimo-prism.twoMoSumPrecip.Median, layout=c(4,3),at=my.at, par.settings = BuRdTheme,
          margin=FALSE, main="PRISM Mean-Median TwoMo Climo", colorkey=list(space="bottom"))  + layer(sp.polygons(states))


#names(cpc.twoMoSumPrecip.Median) <- month.abb
#plot(cpc.twoMoSumPrecip.Median-cpc.twoMoSumClimo, zlim=c(-30,5), main="test")

# plot basic stats, mean, median, diff with state/county boundaries



# time series analysis



