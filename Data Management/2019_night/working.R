
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


NLExtract("001Adata/20201101.tif","001Adata/20201102.tif",uga_shape_3,
          "202011_adm3.csv")
NLExtract("001Adata/20201201.tif","001Adata/20201202.tif",uga_shape_3,
          "202012_adm3.csv")
NLExtract("001Adata/20201001.tif","001Adata/20201002.tif",uga_shape_3,
          "202010_adm3.csv")
NLExtract("001Adata/20200901.tif","001Adata/20200902.tif",uga_shape_3,
          "202009_adm3.csv")
NLExtract("001Adata/20200801.tif","001Adata/20200802.tif",uga_shape_3,
          "202008_adm3.csv")
NLExtract("001Adata/20200701.tif","001Adata/20200702.tif",uga_shape_3,
          "202007_adm3.csv")
NLExtract("001Adata/20200201.tif","001Adata/20200202.tif",uga_shape_3,
          "202002_adm3.csv")
NLExtract("001Adata/20200101.tif","001Adata/20200102.tif",uga_shape_3,
          "202001_adm3.csv")
NLExtract("001Adata/20190901.tif","001Adata/20190902.tif",uga_shape_3,
          "201909_adm3.csv")
NLExtract("001Adata/20191001.tif","001Adata/20191002.tif",uga_shape_3,
          "201910_adm3.csv")
NLExtract("001Adata/20191101.tif","001Adata/20191102.tif",uga_shape_3,
          "201911_adm3.csv")
NLExtract("001Adata/20191201.tif","001Adata/20191202.tif",uga_shape_3,
          "201912_adm3.csv")
########################################################################
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
