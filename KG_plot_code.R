library(raster)
library(wesanderson)

kg <- raster("C:/Users/syanc/Downloads/Beck_KG_V1/Beck_KG_V1_present_0p5.tif")
plot(kg)

plot(kg == 0)

max(kg)
kg_convert_mat <- matrix(c(0, NA,
  1,1,
                               2,1,
                               3,1,
                               4,2,
                               5,2,
                               6,2,
                               7,2,
                               8,3,
                               9,3,
                               10,3,
                               11,3,
                               12,3,
                               13,3,
                               14,3,
                               15,3,
                               16,3,
                               17,4,
                               18,4,
                               19,4,
                               20,4,
                               21,4,
                               22,4,
                               23,4,
                               24,4,
                               25,4,
                               26,4,
                               27,4,
                               28,4,
                               29,5,
                               30,5), byrow = T, ncol = 2)
kg_main <- raster::reclassify(kg, kg_convert_mat)

pal_5 <- wes_palette("Darjeeling1", 5)
plot(kg_main, col = pal_5)

ggplot()+
  geom_raster(data = kg_main)
