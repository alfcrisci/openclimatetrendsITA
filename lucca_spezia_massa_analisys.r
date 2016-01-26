library(raster)
library(rts)
library(xts)
library(rgdal)
library(maptools)
library(gdalUtils)
library(greenbrown)
library(RColorBrewer)
library(lubridate)




italia_bound_region=getData('GADM', country='ITA', level=1)
italia_prov=getData('GADM', country='ITA', level=2)



######################################################################################################à
# Load data 
######################################################################################################à

laspezia_spoly=readRDS("laspezia_spoly.rds")
lucca_spoly=readRDS("lucca_spoly.rds")
massa_carrara_spoly=readRDS("massa_carrara_spoly.rds")


tg_ita=brick("tg_ita_eobs_1950_2015.nc",lvar=1)
rain_ita=brick("rain_ita_eobs_1950_2015.nc",lvar=1)


laspezia_temp=raster::extract(tg_ita,laspezia_spoly)
lucca_temp=raster::extract(tg_ita,lucca_spoly)
massa_carrara_temp=raster::extract(tg_ita,massa_carrara_spoly)

laspezia_rain=raster::extract(rain_ita,laspezia_spoly)
lucca_rain=raster::extract(rain_ita,lucca_spoly)
massa_carrara_rain=raster::extract(rain_ita,massa_carrara_spoly)


######################################################################################################à
# Save objects
######################################################################################################à


sapply(grep("temp",ls(),value=T),function(x) saveRDS(get(x),paste0(x,".rds")))
sapply(grep("a_rain",ls(),value=T),function(x) saveRDS(get(x),paste0(x,".rds")))


SPEZIA_R_ts=as.xts(data.frame(rain=as.numeric(apply(laspezia_rain[[1]],2,function(x)  {x[is.na(x)] <- 0;return(mean(x,na.rm=T))})),row.names=as.Date(ymd(gsub("X","",attr(laspezia_rain[[1]],"dimnames")[[2]])))))
LUCCA_R_ts=as.xts(data.frame(rain=as.numeric(apply(lucca_rain[[1]],2,function(x)  {x[is.na(x)] <- 0;return(mean(x,na.rm=T))})),row.names=as.Date(ymd(gsub("X","",attr(lucca_rain[[1]],"dimnames")[[2]])))))
MASSA_R_ts=as.xts(data.frame(rain=as.numeric(apply(massa_carrara_rain[[1]],2,function(x)  {x[is.na(x)] <- 0;return(mean(x,na.rm=T))})),row.names=as.Date(ymd(gsub("X","",attr(massa_carrara_rain[[1]],"dimnames")[[2]])))))


SPEZIA_T_ts=as.xts(data.frame(temp=as.numeric(apply(laspezia_temp[[1]],2,function(x)  {x[is.na(x)] <- 0;return(mean(x,na.rm=T))})),row.names=as.Date(ymd(gsub("X","",attr(laspezia_temp[[1]],"dimnames")[[2]])))))
LUCCA_T_ts=as.xts(data.frame(temp=as.numeric(apply(lucca_temp[[1]],2,function(x)  {x[is.na(x)] <- 0;return(mean(x,na.rm=T))})),row.names=as.Date(ymd(gsub("X","",attr(lucca_temp[[1]],"dimnames")[[2]])))))
MASSA_T_ts=as.xts(data.frame(temp=as.numeric(apply(massa_carrara_temp[[1]],2,function(x)  {x[is.na(x)] <- 0;return(mean(x,na.rm=T))})),row.names=as.Date(ymd(gsub("X","",attr(massa_carrara_temp[[1]],"dimnames")[[2]])))))

######################################################################################################à
# Save objects
######################################################################################################à

sapply(grep("_ts",ls(),value=T),function(x) saveRDS(get(x),paste0(x,".rds")))

SPEZIA_R_ts_81_10=SPEZIA_R_ts["1981/2010"]
LUCCA_R_ts_81_10=LUCCA_R_ts["1981/2010"]
MASSA_R_ts_81_10=MASSA_R_ts["1981/2010"]
  
SPEZIA_T_ts_81_10=SPEZIA_T_ts["1981/2010"]
LUCCA_T_ts_81_10=LUCCA_T_ts["1981/2010"]
MASSA_T_ts_81_10=MASSA_T_ts["1981/2010"]

SPEZIA_R_ts_2012=SPEZIA_R_ts["2012"]
LUCCA_R_ts_2012=LUCCA_R_ts["2012"]
MASSA_R_ts_2012=MASSA_R_ts["2012"]

SPEZIA_T_ts_2012=SPEZIA_T_ts["2012"]
LUCCA_T_ts_2012=LUCCA_T_ts["2012"]
MASSA_T_ts_2012=MASSA_T_ts["2012"]

######################################################################################################à
SPEZIA_R_res=list(mean_year_clim_03_10=mean(apply.yearly(SPEZIA_R_ts_81_10[.indexmon(SPEZIA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum)),
                  sd_year_clim_03_10=sd(apply.yearly(SPEZIA_R_ts_81_10[.indexmon(SPEZIA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum)),
                  high_bound=mean(apply.yearly(SPEZIA_R_ts_81_10[.indexmon(SPEZIA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum))+sd(apply.yearly(SPEZIA_R_ts_81_10[.indexmon(SPEZIA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum)),
                  low_bound=mean(apply.yearly(SPEZIA_R_ts_81_10[.indexmon(SPEZIA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum))-sd(apply.yearly(SPEZIA_R_ts_81_10[.indexmon(SPEZIA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum)),
                  sum_ts_2012_03_10=sum(SPEZIA_R_ts_2012[.indexmon(SPEZIA_R_ts_2012) %in% c(3,4,5,6,7,8,9,10)])
                 )

SPEZIA_T_res=list(mean_year_clim_03_10=mean(apply.yearly(SPEZIA_T_ts_81_10[.indexmon(SPEZIA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean)),
                 sd_year_clim_03_10=sd(apply.yearly(SPEZIA_T_ts_81_10[.indexmon(SPEZIA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean)),
                 high_bound=mean(apply.yearly(SPEZIA_T_ts_81_10[.indexmon(SPEZIA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean))+sd(apply.yearly(SPEZIA_T_ts_81_10[.indexmon(SPEZIA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean)),
                 low_bound=mean(apply.yearly(SPEZIA_T_ts_81_10[.indexmon(SPEZIA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean))-sd(apply.yearly(SPEZIA_T_ts_81_10[.indexmon(SPEZIA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean)),
                 mean_ts_2012_03_10=mean(SPEZIA_T_ts_2012[.indexmon(SPEZIA_T_ts_2012) %in% c(3,4,5,6,7,8,9,10)])
)

LUCCA_R_res=list(mean_year_clim_03_10=mean(apply.yearly(LUCCA_R_ts_81_10[.indexmon(LUCCA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum)),
                 sd_year_clim_03_10=sd(apply.yearly(LUCCA_R_ts_81_10[.indexmon(LUCCA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum)),
                 high_bound=mean(apply.yearly(LUCCA_R_ts_81_10[.indexmon(LUCCA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum))+sd(apply.yearly(LUCCA_R_ts_81_10[.indexmon(LUCCA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum)),
                 low_bound=mean(apply.yearly(LUCCA_R_ts_81_10[.indexmon(LUCCA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum))-sd(apply.yearly(LUCCA_R_ts_81_10[.indexmon(LUCCA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum)),
                 sum_ts_2012_03_10=sum(LUCCA_R_ts_2012[.indexmon(LUCCA_R_ts_2012) %in% c(3,4,5,6,7,8,9,10)])
)

LUCCA_T_res=list(mean_year_clim_03_10=mean(apply.yearly(LUCCA_T_ts_81_10[.indexmon(LUCCA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean)),
                sd_year_clim_03_10=sd(apply.yearly(LUCCA_T_ts_81_10[.indexmon(LUCCA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean)),
                high_bound=mean(apply.yearly(LUCCA_T_ts_81_10[.indexmon(LUCCA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean))+sd(apply.yearly(LUCCA_T_ts_81_10[.indexmon(LUCCA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean)),
                low_bound=mean(apply.yearly(LUCCA_T_ts_81_10[.indexmon(LUCCA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean))-sd(apply.yearly(LUCCA_T_ts_81_10[.indexmon(LUCCA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean)),
                mean_ts_2012_03_10=mean(LUCCA_T_ts_2012[.indexmon(LUCCA_T_ts_2012) %in% c(3,4,5,6,7,8,9,10)])
)

MASSA_R_res=list(mean_year_clim_03_10=mean(apply.yearly(MASSA_R_ts_81_10[.indexmon(MASSA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum)),
                 sd_year_clim_03_10=sd(apply.yearly(MASSA_R_ts_81_10[.indexmon(MASSA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum)),
                 high_bound=mean(apply.yearly(MASSA_R_ts_81_10[.indexmon(MASSA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum))+sd(apply.yearly(MASSA_R_ts_81_10[.indexmon(MASSA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum)),
                 low_bound=mean(apply.yearly(MASSA_R_ts_81_10[.indexmon(MASSA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum))-sd(apply.yearly(MASSA_R_ts_81_10[.indexmon(MASSA_R_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],sum)),
                 sum_ts_2012_03_10=sum(MASSA_R_ts_2012[.indexmon(MASSA_R_ts_2012) %in% c(3,4,5,6,7,8,9,10)])
)

MASSA_T_res=list(mean_year_clim_03_10=mean(apply.yearly(MASSA_T_ts_81_10[.indexmon(MASSA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean)),
                sd_year_clim_03_10=sd(apply.yearly(MASSA_T_ts_81_10[.indexmon(MASSA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean)),
                high_bound=mean(apply.yearly(MASSA_T_ts_81_10[.indexmon(MASSA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean))+sd(apply.yearly(MASSA_T_ts_81_10[.indexmon(MASSA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean)),
                low_bound=mean(apply.yearly(MASSA_T_ts_81_10[.indexmon(MASSA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean))-sd(apply.yearly(MASSA_T_ts_81_10[.indexmon(MASSA_T_ts_81_10) %in% c(3,4,5,6,7,8,9,10)],mean)),
                mean_ts_2012_03_10=mean(MASSA_T_ts_2012[.indexmon(MASSA_T_ts_2012) %in% c(3,4,5,6,7,8,9,10)])
)

################################################################################################################################
# save object 

sapply(grep("_res",ls(),value=T),function(x) saveRDS(get(x),paste0(x,".rds")))
