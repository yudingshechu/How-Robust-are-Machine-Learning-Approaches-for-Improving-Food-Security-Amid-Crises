
library(raster)#install and load this package
library(sf)
library(haven)
library(tidyr)
library(ggplot2)
library(dplyr)
library(rgdal)
library(ncdf4)
library(exactextractr)
library(tidyverse)
library(reshape2)
###############################################################
filename <- "POWER_Regional_monthly_2016_2017_007d2178S_002d7822N_017d0660E_027d0660E_LST.nc"
our_nc_data_test <- nc_open(filename)


our_nc_data <- nc_open("POWER_Regional_monthly_2016_2017_001d7546S_004d3932N_029d0779E_035d6257E_LST.nc")
attributes(our_nc_data$var)
time <- ncvar_get(our_nc_data, "time")
lswt_array <- ncvar_get(our_nc_data, "PRECTOTCORR") 
fillvalue <- ncatt_get(our_nc_data, "PRECTOTCORR", "_FillValue")
lswt_array[lswt_array==fillvalue$value] <- NA
lswt_array[,,2]
###############################################################
uga_shape_2 <- readOGR(dsn = "uga_admbnda_ubos_20200824_shp",
                       layer = "uga_admbnda_adm2_ubos_20200824")
uga_shape_3 <- readOGR(dsn = "uga_admbnda_ubos_20200824_shp",
                       layer = "uga_admbnda_adm3_ubos_20200824")

ncdata <- brick("POWER_Regional_monthly_2016_2017_001d7546S_004d3932N_029d0779E_035d6257E_LST.nc",
                varname = "T2M_RANGE")
uga_sf_2 <- as(uga_shape_2, "sf")
nc_data_uganda <- crop(ncdata, uga_sf_2)
extract_ugadata2 <- raster::extract(nc_data_uganda, uga_shape_2, 
                                    fun=mean, df=TRUE, na.rm = TRUE,weight = TRUE,exact=TRUE)
final_data2 <- cbind(st_drop_geometry(uga_sf_2), extract_ugadata2)
longdata <- final_data2[,c(3,16:41)]
expdata <- melt(longdata,id.vars = "ADM2_EN",variable.name="date",value.name = "value")
expdata["year"] <- substr(expdata$date,2,5)
expdata["month"] <- substr(expdata$date,6,7)
write.csv(expdata,file = "2016T2M_RANGE.csv",row.names = FALSE)
##############################
ncdata <- brick("POWER_Regional_monthly_2016_2017_001d7546S_004d3932N_029d0779E_035d6257E_LST.nc",
                varname = "T2M")
uga_sf_2 <- as(uga_shape_2, "sf")
nc_data_uganda <- crop(ncdata, uga_sf_2)
extract_ugadata2 <- raster::extract(nc_data_uganda, uga_shape_2, 
                                    fun=mean, df=TRUE, na.rm = TRUE,weight = TRUE,exact=TRUE)
final_data2 <- cbind(st_drop_geometry(uga_sf_2), extract_ugadata2)
longdata <- final_data2[,c(3,16:41)]
expdata <- melt(longdata,id.vars = "ADM2_EN",variable.name="date",value.name = "value")
expdata["year"] <- substr(expdata$date,2,5)
expdata["month"] <- substr(expdata$date,6,7)
write.csv(expdata,file = "2016T2M.csv",row.names = FALSE)
################################
ncdata <- brick("POWER_Regional_monthly_2016_2017_001d7546S_004d3932N_029d0779E_035d6257E_LST.nc",
                varname = "PRECTOTCORR_SUM")
uga_sf_2 <- as(uga_shape_2, "sf")
nc_data_uganda <- crop(ncdata, uga_sf_2)
extract_ugadata2 <- raster::extract(nc_data_uganda, uga_shape_2, 
                      fun=mean, df=TRUE, na.rm = TRUE,weight = TRUE,exact=TRUE)
final_data2 <- cbind(st_drop_geometry(uga_sf_2), extract_ugadata2)
longdata <- final_data2[,c(3,16:41)]
expdata <- melt(longdata,id.vars = "ADM2_EN",variable.name="date",value.name = "value")
expdata["year"] <- substr(expdata$date,2,5)
expdata["month"] <- substr(expdata$date,6,7)
write.csv(expdata,file = "2016PRECTOTCORR_SUM.csv",row.names = FALSE)
###############################################################
# 2019
###############################################################
ncdata <- brick("POWER_Regional_monthly_2019_2020_001d7546S_004d3932N_029d0779E_035d6257E_LST.nc",
                varname = "T2M_RANGE")
uga_sf_2 <- as(uga_shape_2, "sf")
nc_data_uganda <- crop(ncdata, uga_sf_2)
extract_ugadata2 <- raster::extract(nc_data_uganda, uga_shape_2, 
                                    fun=mean, df=TRUE, na.rm = TRUE,weight = TRUE,exact=TRUE)
final_data2 <- cbind(st_drop_geometry(uga_sf_2), extract_ugadata2)
longdata <- final_data2[,c(3,16:41)]
expdata <- melt(longdata,id.vars = "ADM2_EN",variable.name="date",value.name = "value")
expdata["year"] <- substr(expdata$date,2,5)
expdata["month"] <- substr(expdata$date,6,7)
write.csv(expdata,file = "2019T2M_RANGE.csv",row.names = FALSE)
##############################
ncdata <- brick("POWER_Regional_monthly_2019_2020_001d7546S_004d3932N_029d0779E_035d6257E_LST.nc",
                varname = "T2M")
uga_sf_2 <- as(uga_shape_2, "sf")
nc_data_uganda <- crop(ncdata, uga_sf_2)
extract_ugadata2 <- raster::extract(nc_data_uganda, uga_shape_2, 
                                    fun=mean, df=TRUE, na.rm = TRUE,weight = TRUE,exact=TRUE)
final_data2 <- cbind(st_drop_geometry(uga_sf_2), extract_ugadata2)
longdata <- final_data2[,c(3,16:41)]
expdata <- melt(longdata,id.vars = "ADM2_EN",variable.name="date",value.name = "value")
expdata["year"] <- substr(expdata$date,2,5)
expdata["month"] <- substr(expdata$date,6,7)
write.csv(expdata,file = "2019T2M.csv",row.names = FALSE)
################################
ncdata <- brick("POWER_Regional_monthly_2019_2020_001d7546S_004d3932N_029d0779E_035d6257E_LST.nc",
                varname = "PRECTOTCORR_SUM")
uga_sf_2 <- as(uga_shape_2, "sf")
nc_data_uganda <- crop(ncdata, uga_sf_2)
extract_ugadata2 <- raster::extract(nc_data_uganda, uga_shape_2, 
                                    fun=mean, df=TRUE, na.rm = TRUE,weight = TRUE,exact=TRUE)
final_data2 <- cbind(st_drop_geometry(uga_sf_2), extract_ugadata2)
longdata <- final_data2[,c(3,16:41)]
expdata <- melt(longdata,id.vars = "ADM2_EN",variable.name="date",value.name = "value")
expdata["year"] <- substr(expdata$date,2,5)
expdata["month"] <- substr(expdata$date,6,7)
write.csv(expdata,file = "2019PRECTOTCORR_SUM.csv",row.names = FALSE)

###############################################################
uganda_shapefile_cropped <- crop(uga_shape_2, nc_data_uganda)
nl_data_df <- data.frame(lon = coordinates(uganda_shapefile_cropped)[, 1], 
                         lat = coordinates(uganda_shapefile_cropped)[, 2],
                         nl_mean = drop_na(extract_ugadata2))
plotdata = fortify(uganda_shapefile_cropped)
plotdata$id = as.integer(plotdata$id)
colnames(nl_data_df)[3] <- "id"

plotdata_final = merge(x=plotdata,y=nl_data_df,all.x=TRUE,by='id')

ggplot() +
  geom_polygon(data = plotdata_final, 
               aes(x = long, y = lat.x, group = group,fill=nl_mean.X202013),color="black") +
  scale_fill_gradient(low = "royalblue4", high = "red1") +
  coord_equal()

