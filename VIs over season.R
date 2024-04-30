################################################################################
# * * * * * * * * * * * * * Directory and Packages  * * * * * * * * * * * * *  #                                    
################################################################################
# working directory
setwd("C:/Users/kaeli/OneDrive - Montana State University/Documents/Spring 2024/Remote Sensing/Project/PC")

# load packages
library(ggplot2)
library(dplyr)
library(gridExtra)

################################################################################
#                            Read in CSV Files                                 #
################################################################################
# jun 27
jun27 <- read.csv("20180627_PC.csv")
jun27.red <- jun27$RED
jun27.re <- jun27$REDEDGE
jun27.nir <- jun27$NIR

# jul 4
jul04 <- read.csv("20180704_PC.csv")
jul04.red <- jul04$RED
jul04.re <- jul04$REDEDGE
jul04.nir <- jul04$NIR

# jul 11
jul11 <- read.csv("20180711_PC.csv")
jul11.red <- jul11$RED
jul11.re <- jul11$REDEDGE
jul11.nir <- jul11$NIR

# jul 18
jul18 <- read.csv("20180718_PC.csv")
jul18.red <- jul18$RED
jul18.re <- jul18$REDEDGE
jul18.nir <- jul18$NIR

# jul 27
jul27 <- read.csv("20180727_PC.csv")
jul27.red <- jul27$RED
jul27.re <- jul27$REDEDGE
jul27.nir <- jul27$NIR

# aug 1
aug01 <- read.csv("20180801_PC.csv")
aug01.red <- aug01$RED
aug01.re <- aug01$REDEDGE
aug01.nir <- aug01$NIR

# aug 10
aug10 <- read.csv("20180810_PC.csv")
aug10.red <- aug10$RED
aug10.re <- aug10$REDEDGE
aug10.nir <- aug10$NIR

# aug 15
aug15 <- read.csv("20180815_PC.csv")
aug15.red <- aug15$RED
aug15.re <- aug15$REDEDGE
aug15.nir <- aug15$NIR

# aug 23
aug23 <- read.csv("20180823_PC.csv")
aug23.red <- aug23$RED
aug23.re <- aug23$REDEDGE
aug23.nir <- aug23$NIR

# aug 29
aug29 <- read.csv("20180829_PC.csv")
aug29.red <- aug29$RED
aug29.re <- aug29$REDEDGE
aug29.nir <- aug29$NIR

# sep 6
sep06 <- read.csv("20180906_PC.csv")
sep06.red <- sep06$RED
sep06.re <- sep06$REDEDGE
sep06.nir <- sep06$NIR

# sep 12
sep12 <- read.csv("20180912_PC.csv")
sep12.red <- sep12$RED
sep12.re <- sep12$REDEDGE
sep12.nir <- sep12$NIR

# sep 19
sep19 <- read.csv("20180919_PC.csv")
sep19.red <- sep19$RED
sep19.re <- sep19$REDEDGE
sep19.nir <- sep19$NIR

# sep 26
sep26 <- read.csv("20180926_PC.csv")
sep26.red <- sep26$RED
sep26.re <- sep26$REDEDGE
sep26.nir <- sep26$NIR

# sep 29
sep29 <- read.csv("20180929_PC.csv")
sep29.red <- sep29$RED
sep29.re <- sep29$REDEDGE
sep29.nir <- sep29$NIR

# oct 4
oct04 <- read.csv("20181004_PC.csv")
oct04.red <- oct04$RED
oct04.re <- oct04$REDEDGE
oct04.nir <- oct04$NIR

# oct 10
oct10 <- read.csv("20181010_PC.csv")
oct10.red <- oct10$RED
oct10.re <- oct10$REDEDGE
oct10.nir <- oct10$NIR

# oct 17
oct17 <- read.csv("20181017_PC.csv")
oct17.red <- oct17$RED
oct17.re <- oct17$REDEDGE
oct17.nir <- oct17$NIR

# oct 24
oct24 <- read.csv("20181024_PC.csv")
oct24.red <- oct24$RED
oct24.re <- oct24$REDEDGE
oct24.nir <- oct24$NIR

################################################################################
#                            Vegetation Indices                                #
################################################################################
# NDVI
jun27.NDVI <- (jun27.nir - jun27.red) / (jun27.nir + jun27.red)
jul04.NDVI <- (jul04.nir - jul04.red) / (jul04.nir + jul04.red)
jul11.NDVI <- (jul11.nir - jul11.red) / (jul11.nir + jul11.red)
jul18.NDVI <- (jul18.nir - jul18.red) / (jul18.nir + jul18.red)
jul27.NDVI <- (jul27.nir - jul27.red) / (jul27.nir + jul27.red)
aug01.NDVI <- (aug01.nir - aug01.red) / (aug01.nir + aug01.red)
aug10.NDVI <- (aug10.nir - aug10.red) / (aug10.nir + aug10.red)
aug15.NDVI <- (aug15.nir - aug15.red) / (aug15.nir + aug15.red)
aug23.NDVI <- (aug23.nir - aug23.red) / (aug23.nir + aug23.red)
aug29.NDVI <- (aug29.nir - aug29.red) / (aug29.nir + aug29.red)
sep06.NDVI <- (sep06.nir - sep06.red) / (sep06.nir + sep06.red)
sep12.NDVI <- (sep12.nir - sep12.red) / (sep12.nir + sep12.red)
sep19.NDVI <- (sep19.nir - sep19.red) / (sep19.nir + sep19.red)
sep26.NDVI <- (sep26.nir - sep26.red) / (sep26.nir + sep26.red)
sep29.NDVI <- (sep29.nir - sep29.red) / (sep29.nir + sep29.red)
oct04.NDVI <- (oct04.nir - oct04.red) / (oct04.nir + oct04.red)
oct10.NDVI <- (oct10.nir - oct10.red) / (oct10.nir + oct10.red)
oct17.NDVI <- (oct17.nir - oct17.red) / (oct17.nir + oct17.red)
oct24.NDVI <- (oct24.nir - oct24.red) / (oct24.nir + oct24.red)

NDVI <- c(mean(jun27.NDVI), mean(jul04.NDVI), mean(jul11.NDVI), mean(jul18.NDVI), 
          mean(jul27.NDVI), mean(aug01.NDVI), mean(aug10.NDVI), mean(aug15.NDVI), 
          mean(aug23.NDVI), mean(aug29.NDVI), mean(sep06.NDVI), mean(sep12.NDVI), 
          mean(sep19.NDVI), mean(sep26.NDVI), mean(sep29.NDVI), mean(oct04.NDVI), 
          mean(oct10.NDVI), mean(oct17.NDVI), mean(oct24.NDVI)) 

# OSAVI
jun27.OSAVI <- (jun27.nir - jun27.red) / (jun27.nir + jun27.red + 0.16)
jul04.OSAVI <- (jul04.nir - jul04.red) / (jul04.nir + jul04.red + 0.16)
jul11.OSAVI <- (jul11.nir - jul11.red) / (jul11.nir + jul11.red + 0.16)
jul18.OSAVI <- (jul18.nir - jul18.red) / (jul18.nir + jul18.red + 0.16)
jul27.OSAVI <- (jul27.nir - jul27.red) / (jul27.nir + jul27.red + 0.16)
aug01.OSAVI <- (aug01.nir - aug01.red) / (aug01.nir + aug01.red + 0.16)
aug10.OSAVI <- (aug10.nir - aug10.red) / (aug10.nir + aug10.red + 0.16)
aug15.OSAVI <- (aug15.nir - aug15.red) / (aug15.nir + aug15.red + 0.16)
aug23.OSAVI <- (aug23.nir - aug23.red) / (aug23.nir + aug23.red + 0.16)
aug29.OSAVI <- (aug29.nir - aug29.red) / (aug29.nir + aug29.red + 0.16)
sep06.OSAVI <- (sep06.nir - sep06.red) / (sep06.nir + sep06.red + 0.16)
sep12.OSAVI <- (sep12.nir - sep12.red) / (sep12.nir + sep12.red + 0.16)
sep19.OSAVI <- (sep19.nir - sep19.red) / (sep19.nir + sep19.red + 0.16)
sep26.OSAVI <- (sep26.nir - sep26.red) / (sep26.nir + sep26.red + 0.16)
sep29.OSAVI <- (sep29.nir - sep29.red) / (sep29.nir + sep29.red + 0.16)
oct04.OSAVI <- (oct04.nir - oct04.red) / (oct04.nir + oct04.red + 0.16)
oct10.OSAVI <- (oct10.nir - oct10.red) / (oct10.nir + oct10.red + 0.16)
oct17.OSAVI <- (oct17.nir - oct17.red) / (oct17.nir + oct17.red + 0.16)
oct24.OSAVI <- (oct24.nir - oct24.red) / (oct24.nir + oct24.red + 0.16)

OSAVI <- c(mean(jun27.OSAVI), mean(jul04.OSAVI), mean(jul11.OSAVI), mean(jul18.OSAVI), 
           mean(jul27.OSAVI), mean(aug01.OSAVI), mean(aug10.OSAVI), mean(aug15.OSAVI), 
           mean(aug23.OSAVI), mean(aug29.OSAVI), mean(sep06.OSAVI), mean(sep12.OSAVI), 
           mean(sep19.OSAVI), mean(sep26.OSAVI), mean(sep29.OSAVI), mean(oct04.OSAVI), 
           mean(oct10.OSAVI), mean(oct17.OSAVI), mean(oct24.OSAVI)) 

# NDRE 
jun27.NDRE <- (jun27.nir - jun27.re) / (jun27.nir + jun27.re)
jul04.NDRE <- (jul04.nir - jul04.re) / (jul04.nir + jul04.re)
jul11.NDRE <- (jul11.nir - jul11.re) / (jul11.nir + jul11.re)
jul18.NDRE <- (jul18.nir - jul18.re) / (jul18.nir + jul18.re)
jul27.NDRE <- (jul27.nir - jul27.re) / (jul27.nir + jul27.re)
aug01.NDRE <- (aug01.nir - aug01.re) / (aug01.nir + aug01.re)
aug10.NDRE <- (aug10.nir - aug10.re) / (aug10.nir + aug10.re)
aug15.NDRE <- (aug15.nir - aug15.re) / (aug15.nir + aug15.re)
aug23.NDRE <- (aug23.nir - aug23.re) / (aug23.nir + aug23.re)
aug29.NDRE <- (aug29.nir - aug29.re) / (aug29.nir + aug29.re)
sep06.NDRE <- (sep06.nir - sep06.re) / (sep06.nir + sep06.re)
sep12.NDRE <- (sep12.nir - sep12.re) / (sep12.nir + sep12.re)
sep19.NDRE <- (sep19.nir - sep19.re) / (sep19.nir + sep19.re)
sep26.NDRE <- (sep26.nir - sep26.re) / (sep26.nir + sep26.re)
sep29.NDRE <- (sep29.nir - sep29.re) / (sep29.nir + sep29.re)
oct04.NDRE <- (oct04.nir - oct04.re) / (oct04.nir + oct04.re)
oct10.NDRE <- (oct10.nir - oct10.re) / (oct10.nir + oct10.re)
oct17.NDRE <- (oct17.nir - oct17.re) / (oct17.nir + oct17.re)
oct24.NDRE <- (oct24.nir - oct24.re) / (oct24.nir + oct24.re)

NDRE <- c(mean(jun27.NDRE), mean(jul04.NDRE), mean(jul11.NDRE), mean(jul18.NDRE), 
          mean(jul27.NDRE), mean(aug01.NDRE), mean(aug10.NDRE), mean(aug15.NDRE), 
          mean(aug23.NDRE), mean(aug29.NDRE), mean(sep06.NDRE), mean(sep12.NDRE), 
          mean(sep19.NDRE), mean(sep26.NDRE), mean(sep29.NDRE), mean(oct04.NDRE), 
          mean(oct10.NDRE), mean(oct17.NDRE), mean(oct24.NDRE)) 

# RECI
jun27.RECI <- (jun27.nir / jun27.red) - 1
jul04.RECI <- (jul04.nir / jul04.red) - 1
jul11.RECI <- (jul11.nir / jul11.red) - 1
jul18.RECI <- (jul18.nir / jul18.red) - 1
jul27.RECI <- (jul27.nir / jul27.red) - 1
aug01.RECI <- (aug01.nir / aug01.red) - 1
aug10.RECI <- (aug10.nir / aug10.red) - 1
aug15.RECI <- (aug15.nir / aug15.red) - 1
aug23.RECI <- (aug23.nir / aug23.red) - 1
aug29.RECI <- (aug29.nir / aug29.red) - 1
sep06.RECI <- (sep06.nir / sep06.red) - 1
sep12.RECI <- (sep12.nir / sep12.red) - 1
sep19.RECI <- (sep19.nir / sep19.red) - 1
sep26.RECI <- (sep26.nir / sep26.red) - 1
sep29.RECI <- (sep29.nir / sep29.red) - 1
oct04.RECI <- (oct04.nir / oct04.red) - 1
oct10.RECI <- (oct10.nir / oct10.red) - 1
oct17.RECI <- (oct17.nir / oct17.red) - 1
oct24.RECI <- (oct24.nir / oct24.red) - 1

RECI <- c(mean(jun27.RECI), mean(jul04.RECI), mean(jul11.RECI), mean(jul18.RECI), 
          mean(jul27.RECI), mean(aug01.RECI), mean(aug10.RECI), mean(aug15.RECI), 
          mean(aug23.RECI), mean(aug29.RECI), mean(sep06.RECI), mean(sep12.RECI), 
          mean(sep19.RECI), mean(sep26.RECI), mean(sep29.RECI), mean(oct04.RECI), 
          mean(oct10.RECI), mean(oct17.RECI), mean(oct24.RECI)) 

################################################################################
#                                   Plots                                      #
################################################################################
# Dates for plots
dates <- c("jun 27", "jul 04", "jul 11", "jul 18", "jul 27", "aug 01",
          "aug 10", "aug 15", "aug 23", "aug 29", "sep 06", "sep 12", 
          "sep 19", "sep 26", "sep 29", "oct 04", "oct 10", "oct 17", "oct 24")

dates <- strptime(dates, format = "%b%d")

# Plots 
windows()
p1 <- ggplot(data.frame(date = dates, NDVI = NDVI), aes(x = date, y = NDVI)) +
       geom_point(aes(color = NDVI)) + 
       geom_line() +
       labs(x = "Date", y = "NDVI")  
  
p2 <- ggplot(data.frame(date = dates, OSAVI = OSAVI), aes(x = date, y = OSAVI)) +
      geom_point(aes(color = OSAVI)) + 
      geom_line() +
      labs(x = "Date", y = "OSAVI") 
  
p3 <- ggplot(data.frame(date = dates, NDRE = NDRE), aes(x = date, y = NDRE)) +
      geom_point(aes(color = NDRE)) + 
      geom_line()
      labs(x = "Date", y = "NDRE")
  
p4 <- ggplot(data.frame(date = dates, RECI = RECI), aes(x = date, y = RECI)) +
      geom_point(aes(color = RECI)) + 
      geom_line()
      labs(x = "Date", y = "RECI") 
grid.arrange (p1, p2, p3, p4, nrow = 2)
