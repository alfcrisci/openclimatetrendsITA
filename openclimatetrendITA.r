library(raster)
library(rgdal)
library(greenbrown)
library(RColorBrewer)

##################################################################################################
# Get bounds 

italia_prov=getData('GADM', country='ITA', level=2)
italia_bound_region=getData('GADM', country='ITA', level=1)


tg_ita=brick("tg_ita_eobs_1950_2015.nc",lvar=1)
rain_ita=brick("rain_ita_eobs_1950_2015.nc",lvar=1)


tg_ita_T <- TrendRaster(tg_ita,start=c(1950,1), freq=12,method="SeasonalAdjusted", breaks=0, funSeasonalCycle=MeanSeasonalCycle)

png("trend_T.png")

plot(tg_ita_T[["SlopeSEG1"]]*736,col=brewer.pal(8,"YlOrRd"))
plot(italia_bound_region,add=T)
dev.off()


rain_ita_T <- TrendRaster(rain_ita,start=c(1950,1), freq=12,method="SeasonalAdjusted", breaks=0, funSeasonalCycle=MeanSeasonalCycle)

png("rain_T.png")
plot(rain_ita_T[["SlopeSEG1"]]*736,col=brewer.pal(8,"RdBu"))
plot(italia_bound_region,add=T)
dev.off()