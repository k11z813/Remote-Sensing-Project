################################################################################
#            Vegetation Index Plots for Each Growth Stage: Figure 1            #          
################################################################################
# working directory
setwd("")

# load packages
library(sp)
library(raster)
library(ggplot2)

################################################################################
#                             Load Tiff Files                                  #
################################################################################
# Aug 01 
aug01 <- raster('20180801_seq_50m_PC.tif')
aug01

aug01.green <- raster('20180801_seq_50m_PC.tif', band=1)
aug01.red <- raster('20180801_seq_50m_PC.tif', band=2)
aug01.re <- raster('20180801_seq_50m_PC.tif', band=3)
aug01.nir <- raster('20180801_seq_50m_PC.tif',band=4)

# Aug 23 
aug23 <- raster('20180823_seq_50m_PC.tif')
aug23

aug23.green <- raster('20180823_seq_50m_PC.tif', band=1)
aug23.red <- raster('20180823_seq_50m_PC.tif', band=2)
aug23.re <- raster('20180823_seq_50m_PC.tif', band=3)
aug23.nir <- raster('20180823_seq_50m_PC.tif',band=4)

# Sep 29 
sep29 <- raster('20180929_seq_50m_PC.tif')
sep29

sep29.green <- raster('20180929_seq_50m_PC.tif', band=1)
sep29.red <- raster('20180929_seq_50m_PC.tif', band=2)
sep29.re <- raster('20180929_seq_50m_PC.tif', band=3)
sep29.nir <- raster('20180929_seq_50m_PC.tif',band=4)

################################################################################
#                          Plot Vegetation Indices                             #
################################################################################
windows()
par(mfrow = c(4,3), mai = c(0.1, 0.1, 0.1, 0.1))
# NDVI
aug01_ndvi <- plot((aug01.nir - aug01.red) / (aug01.nir + aug01.red), box=FALSE, axes=FALSE, main = "NDVI V6")+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
aug23_ndvi <- plot((aug23.nir - aug23.red) / (aug23.nir + aug23.red), box=FALSE, axes=FALSE, main = "NDVI Three Nodes")+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
sep29_ndvi <- plot((sep29.nir - sep29.red) / (sep29.nir + sep29.red), box=FALSE, axes=FALSE, main = "NDVI Flowering")+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# OSAVI
aug01.OSAVI <- plot((aug01.nir - aug01.red) / (aug01.nir + aug01.red + 0.16), box=FALSE, axes=FALSE, main = "OSAVI V6")+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
aug23.OSAVI <- plot((aug23.nir - aug23.red) / (aug23.nir + aug23.red + 0.16), box=FALSE, axes=FALSE, main = "OSAVI Three Nodes")+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
sep29.OSAVI <- plot((sep29.nir - sep29.red) / (sep29.nir + sep29.red + 0.16), box=FALSE, axes=FALSE, main = "OSAVI Flowering")+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

#NDRE
aug01.NDRE <- plot((((aug01.nir - aug01.re) / (aug01.nir + aug01.re)) - 1), box=FALSE, axes=FALSE, main = "NDRE V6")+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
aug23.NDRE <- plot((((aug23.nir - aug23.re) / (aug23.nir + aug23.re)) - 1), box=FALSE,axes=FALSE, main = "NDRE Three Nodes")+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
sep29.NDRE <- plot((((sep29.nir - sep29.re) / (sep29.nir + sep29.re)) - 1), box=FALSE, axes=FALSE, main = "NDRE Flowering")+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# RECI
aug01.RECI <- plot(((aug01.nir / aug01.red) - 1), box=FALSE, axes=FALSE, main = "RECI V6")+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
aug23.RECI <- plot(((aug23.nir / aug23.red) - 1), box=FALSE, axes=FALSE, main = "RECI Three Nodes")+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
sep29.RECI <- plot(((sep29.nir / sep29.red) - 1), box=FALSE, axes=FALSE, main = "RECI Flowering")+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
