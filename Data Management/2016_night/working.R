
library(raster)#install and load this package
library(sf)
library(haven)
library(tidyr)
library(ggplot2)
library(dplyr)
library(rgdal)
###############################################################
uga_shape_2 <- readOGR(dsn = "uga_admbnda_ubos_20200824_shp",
                       layer = "uga_admbnda_adm2_ubos_20200824")
uga_shape_3 <- readOGR(dsn = "uga_admbnda_ubos_20200824_shp",
                       layer = "uga_admbnda_adm3_ubos_20200824")
uga_shape_4 <- readOGR(dsn = "uga_admbnda_ubos_20200824_shp",
                       layer = "uga_admbnda_adm4_ubos_20200824")

NLExtract <- function(filepath1,filepath2,map,optname){
  data <- raster(filepath1) 
  data2 <- raster(filepath2) 
  uga_sf_2 <- as(map, "sf")
  nl_data_uganda_2 <- crop(data2, uga_sf_2)
  nl_data_uganda_1 <- crop(data, uga_sf_2)
  merged_raster <- merge(nl_data_uganda_1, nl_data_uganda_2)
  extract_ugadata2 <- raster::extract(merged_raster, map, fun=mean, df=TRUE, na.rm = TRUE)
  final_data2 <- cbind(st_drop_geometry(uga_sf_2), extract_ugadata2)
  write.csv(final_data2,file = optname,row.names = FALSE)
}

NLExtract("001Adata/20160701.tif","001Adata/20160702.tif",uga_shape_3,"201607_adm3.csv")
NLExtract("001Adata/20160801.tif","001Adata/20160802.tif",uga_shape_3,"201608_adm3.csv")
NLExtract("001Adata/20160901.tif","001Adata/20160902.tif",uga_shape_3,"201609_adm3.csv")
NLExtract("001Adata/20161001.tif","001Adata/20161002.tif",uga_shape_3,"201610_adm3.csv")
NLExtract("001Adata/20161101.tif","001Adata/20161102.tif",uga_shape_3,"201611_adm3.csv")
NLExtract("001Adata/20161201.tif","001Adata/20161202.tif",uga_shape_3,"201612_adm3.csv")
NLExtract("001Adata/20170101.tif","001Adata/20170102.tif",uga_shape_3,"201701_adm3.csv")
NLExtract("001Adata/20170201.tif","001Adata/20170202.tif",uga_shape_3,"201702_adm3.csv")
NLExtract("001Adata/20170301.tif","001Adata/20170302.tif",uga_shape_3,"201703_adm3.csv")
NLExtract("001Adata/20170401.tif","001Adata/20170402.tif",uga_shape_3,"201704_adm3.csv")
NLExtract("001Adata/20170501.tif","001Adata/20170502.tif",uga_shape_3,"201705_adm3.csv")
NLExtract("001Adata/20170601.tif","001Adata/20170602.tif",uga_shape_3,"201706_adm3.csv")

##############################################################
uganda_shapefile_cropped <- crop(uga_shape_2, merged_raster)
nl_data_df <- data.frame(lon = coordinates(uganda_shapefile_cropped)[, 1], 
                         lat = coordinates(uganda_shapefile_cropped)[, 2],
                         nl_mean = drop_na(extract_ugadata2))

plotdata = fortify(uganda_shapefile_cropped)
plotdata$id = as.integer(plotdata$id)
summary(plotdata$id)
summary(nl_data_df$nl_mean.ID)
colnames(nl_data_df)[3] <- "id"

plotdata_final = merge(x=plotdata,y=nl_data_df,all.x=TRUE,by='id')

ggplot() +
  geom_polygon(data = plotdata_final, 
               aes(x = long, y = lat.x, group = group,fill=nl_mean.layer),color="black") +
  scale_fill_gradient(low = "yellow", high = "red") +
  coord_equal()
