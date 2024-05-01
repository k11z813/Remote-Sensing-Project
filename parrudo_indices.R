################################################################################
# * * * * * * * * * * * * * Directory and Packages  * * * * * * * * * * * * *  #                                    
################################################################################
# working directory
setwd("C:/Users/kaeli/OneDrive - Montana State University/Documents/Spring 2024/Remote Sensing/Project/R_input/parrudo")

# load packages
library(report)
library(insight)

################################################################################
#                            Read in CSV Files                                 #
################################################################################
# v6 = v
parrudo.v6 <- read.csv("V6.csv")
names(parrudo.v6) # get column names

# assign column names to new variables
v.bio <- parrudo.v6$BIOMASS 
v.green <- parrudo.v6$GREEN
v.red <- parrudo.v6$RED
v.re <- parrudo.v6$REDEDGE
v.nir <- parrudo.v6$NIR

# three nodes = n
parrudo.threenodes <- read.csv("threenodes.csv")
names(parrudo.threenodes)

n.bio <- parrudo.threenodes$BIOMASS
n.green <- parrudo.threenodes$GREEN
n.red <- parrudo.threenodes$RED
n.re <- parrudo.threenodes$REDEDGE
n.nir <- parrudo.threenodes$NIR

# flowering = f
parrudo.flowering <- read.csv("flowering.csv")
names(parrudo.flowering)

f.bio <- parrudo.flowering$BIOMASS
f.green <- parrudo.flowering$GREEN
f.red <- parrudo.flowering$RED
f.re <- parrudo.flowering$REDEDGE
f.nir <- parrudo.flowering$NIR

################################################################################
#                            Vegetation Indices                                #
################################################################################
# Calculate vegetation indices by adding band names into index formulas
# NDVI
v.NDVI <- (v.nir - v.red) / (v.nir + v.red)
n.NDVI <- (n.nir - n.red) / (n.nir + n.red)
f.NDVI <- (f.nir - f.red) / (f.nir + f.red)

# OSAVI
v.OSAVI <- (v.nir - v.red) / (v.nir + v.red + 0.16)
n.OSAVI <- (n.nir - n.red) / (n.nir + n.red + 0.16)
f.OSAVI <- (f.nir - f.red) / (f.nir + f.red + 0.16)

# NDRE 
v.NDRE <- (v.nir - v.re) / (v.nir + v.re)
n.NDRE <- (n.nir - n.re) / (n.nir + n.re)
f.NDRE <- (f.nir - f.re) / (f.nir + f.re)

# RECI
v.RECI <- (v.nir / v.red) - 1
n.RECI <- (n.nir / n.red) - 1
f.RECI <- (f.nir / f.red) - 1

################################################################################
#                            Descriptive Analysis                              #
################################################################################
# Calculate minimum, maximum, mean, median, and standard deviation for indices
# NDVI
min(v.NDVI)
max(v.NDVI)
mean(v.NDVI)
median(v.NDVI)
sd(v.NDVI)

min(n.NDVI)
max(n.NDVI)
mean(n.NDVI)
median(n.NDVI)
sd(n.NDVI)

min(f.NDVI)
max(f.NDVI)
mean(f.NDVI)
median(f.NDVI)
sd(f.NDVI)

# OSAVI
min(v.OSAVI)
max(v.OSAVI)
mean(v.OSAVI)
median(v.OSAVI)
sd(v.OSAVI)

min(n.OSAVI)
max(n.OSAVI)
mean(n.OSAVI)
median(n.OSAVI)
sd(n.OSAVI)

min(f.OSAVI)
max(f.OSAVI)
mean(f.OSAVI)
median(f.OSAVI)
sd(f.OSAVI)

# NDRE
min(v.NDRE)
max(v.NDRE)
mean(v.NDRE)
median(v.NDRE)
sd(v.NDRE)

min(n.NDRE)
max(n.NDRE)
mean(n.NDRE)
median(n.NDRE)
sd(n.NDRE)

min(f.NDRE)
max(f.NDRE)
mean(f.NDRE)
median(f.NDRE)
sd(f.NDRE)

# RECI
min(v.RECI)
max(v.RECI)
mean(v.RECI)
median(v.RECI)
sd(v.RECI)

min(n.RECI)
max(n.RECI)
mean(n.RECI)
median(n.RECI)
sd(n.RECI)

min(f.RECI)
max(f.RECI)
mean(f.RECI)
median(f.RECI)
sd(f.RECI)

# Biomass
min(v.bio)
max(v.bio)
mean(v.bio)
median(v.bio)
sd(v.bio)

min(n.bio)
max(n.bio)
mean(n.bio)
median(n.bio)
sd(n.bio)

min(f.bio)
max(f.bio)
mean(f.bio)
median(f.bio)
sd(f.bio)

################################################################################
#                                Correlations                                  #
################################################################################
# correlation test: index vs biomass v6
c1.v <- cor.test(v.NDVI,v.bio, method = "pearson")
c2.v <- cor.test(v.OSAVI,v.bio, method = "pearson")
c3.v <- cor.test(v.NDRE,v.bio, method = "pearson")
c4.v <- cor.test(v.RECI,v.bio, method = "pearson")

cor.results.v <- c(report_table(c1.v),
                 report_table(c2.v),
                 report_table(c3.v),
                 report_table(c4.v))
display(cor.results.v)
export_table(cor.results.v, format="html")

# correlation test: index vs biomass three nodes
c1.n <- cor.test(n.NDVI,n.bio, method = "pearson")
c2.n <- cor.test(n.OSAVI,n.bio, method = "pearson")
c3.n <- cor.test(n.NDRE,n.bio, method = "pearson")
c4.n <- cor.test(n.RECI,n.bio, method = "pearson")

cor.results.n <- c(report_table(c1.n),
                   report_table(c2.n),
                   report_table(c3.n),
                   report_table(c4.n))
display(cor.results.n)
export_table(cor.results.n, format="html")

# correlation test: index vs biomass flowering
c1.f <- cor.test(f.NDVI,f.bio, method = "pearson")
c2.f <- cor.test(f.OSAVI,f.bio, method = "pearson")
c3.f <- cor.test(f.NDRE,f.bio, method = "pearson")
c4.f <- cor.test(f.RECI,f.bio, method = "pearson")

# Combine results 
cor.results.f <- c(report_table(c1.f),
                   report_table(c2.f),
                   report_table(c3.f),
                   report_table(c4.f))
display(cor.results.f) # display results
export_table(cor.results.f, format="html") # export table

################################################################################
#                      Regression: Index vs Growth Stage                       #
################################################################################ 
# assign growth stage to values
bio <- c(v.bio, n.bio, f.bio)
ndvi <- c(v.NDVI, n.NDVI, f.NDVI)
osavi <- c(v.OSAVI, n.OSAVI, f.OSAVI)
ndre <- c(v.NDRE, n.NDRE, f.NDRE)
reci <- c(v.RECI, n.RECI, f.RECI)
growth.stage <- c(rep("Seedling",40),rep("Three Nodes",40),rep("Flowering",40))

#treatment as a factor = categorical
growth.stage <- as.factor(growth.stage) 

# make indicator variables for growth stage
v6 <- ifelse(growth.stage==1,1,0)
three.nodes <- ifelse(growth.stage==2,1,0)
flowering <- ifelse(growth.stage==3,1,0)

####################
# * * * NDVI * * * #
####################
# fit model 
ndvi.model <- lm(bio ~ ndvi*growth.stage)
summary(ndvi.model)

# extract coefficients
beta0.1<- ndvi.model$coeff[1]
beta1.1<- ndvi.model$coeff[2]
beta2.1<- ndvi.model$coeff[3]
beta3.1<- ndvi.model$coeff[4]
beta4.1<- ndvi.model$coeff[5]
beta5.1<- ndvi.model$coeff[6]

#####################
# * * * OSAVI * * * #
##################### 
# fit model
osavi.model <- lm(bio ~ osavi*growth.stage) 
summary(osavi.model) 

# extract coefficients
beta0.2<- osavi.model$coeff[1]
beta1.2<- osavi.model$coeff[2]
beta2.2<- osavi.model$coeff[3]
beta3.2<- osavi.model$coeff[4]
beta4.2<- osavi.model$coeff[5]
beta5.2<- osavi.model$coeff[6]

####################
# * * * NDRE * * * #
#################### 
# fit model
ndre.model <- lm(bio ~ ndre*growth.stage)
summary(ndre.model) 

# extract coefficients
beta0.3<- ndre.model$coeff[1]
beta1.3<- ndre.model$coeff[2]
beta2.3<- ndre.model$coeff[3]
beta3.3<- ndre.model$coeff[4]
beta4.3<- ndre.model$coeff[5]
beta5.3<- ndre.model$coeff[6]

####################
# * * * RECI * * * #
####################
# fit model
reci.model <- lm(bio ~ reci*growth.stage)
summary(reci.model) 

# extract coefficients
beta0.4<- reci.model$coeff[1]
beta1.4<- reci.model$coeff[2]
beta2.4<- reci.model$coeff[3]
beta3.4<- reci.model$coeff[4]
beta4.4<- reci.model$coeff[5]
beta5.4<- reci.model$coeff[6]

################################################################################
#                              Regression: Plots                               #
################################################################################ 
windows()
par(mfrow = c(2,2))
plot(ndvi, bio, type="n", main="NDVI vs. Biomass by Growth Stage", xlab="NDVI", ylab="Biomass (kg/ha)", 
     xlim=c(0.56,0.93), ylim=c(448.8,10444.5))
points(ndvi[growth.stage=="Seedling"], bio[growth.stage=="Seedling"], pch=16, col="darkgoldenrod1", cex=1)
points(ndvi[growth.stage=="Three Nodes"], bio[growth.stage=="Three Nodes"], pch=15, col="palegreen3",cex=1)
points(ndvi[growth.stage=="Flowering"], bio[growth.stage=="Flowering"], pch=17, col="indianred3" ,cex=1)
legend(0.57,10500,legend=c("Seedling", "Three Nodes", "Flowering"),
       pch=c(16,15,17), col=c("darkgoldenrod1","palegreen3","indianred3"), cex=c(0.7,0.7,0.7), bty="n")
abline(a= beta0.1, b=beta1.1, lwd=2, lty=2)
abline(a=(beta0.1 + beta2.1),b=(beta1.1+beta4.1), lwd=2 )
abline(a=(beta0.1 + beta3.1),b=(beta1.1+beta5.1), lwd=2,lty=3)

plot(osavi, bio, type="n", main="OSAVI vs. Biomass by Growth Stage", xlab="OSAVI", ylab="Biomass (kg/ha)", 
     xlim=c(0.374,0.701), ylim=c(448.8,10444.5))
points(osavi[growth.stage=="Seedling"], bio[growth.stage=="Seedling"], pch=16, col="darkgoldenrod1", cex=1)
points(osavi[growth.stage=="Three Nodes"], bio[growth.stage=="Three Nodes"], pch=15, col="palegreen3",cex=1)
points(osavi[growth.stage=="Flowering"], bio[growth.stage=="Flowering"], pch=17, col="indianred3" ,cex=1)
legend(0.37,10500,legend=c("Seedling", "Three Nodes", "Flowering"),
       pch=c(16,15,17), col=c("darkgoldenrod1","palegreen3","indianred3"), cex=c(0.7,0.7,0.7), bty="n")
abline(a= beta0.2, b=beta1.2, lwd=2, lty=2)
abline(a=(beta0.2 + beta2.2),b=(beta1.2+beta4.2), lwd=2 )
abline(a=(beta0.2 + beta3.2),b=(beta1.2+beta5.2), lwd=2,lty=3)

plot(ndre, bio, type="n", main="NDRE vs. Biomass by Growth Stage", xlab="NDRE", ylab="Biomass (kg/ha)", 
     xlim=c(0.1,0.361), ylim=c(448.8,10444.5))
points(ndre[growth.stage=="Seedling"], bio[growth.stage=="Seedling"], pch=16, col="darkgoldenrod1", cex=1)
points(ndre[growth.stage=="Three Nodes"], bio[growth.stage=="Three Nodes"], pch=15, col="palegreen3",cex=1)
points(ndre[growth.stage=="Flowering"], bio[growth.stage=="Flowering"], pch=17, col="indianred3" ,cex=1)
legend(0.09,10500,legend=c("Seedling", "Three Nodes", "Flowering"),
       pch=c(16,15,17), col=c("darkgoldenrod1","palegreen3","indianred3"), cex=c(0.7,0.7,0.7), bty="n")
abline(a= beta0.3, b=beta1.3, lwd=2, lty=2)
abline(a=(beta0.3 + beta2.3),b=(beta1.3+beta4.3), lwd=2 )
abline(a=(beta0.3 + beta3.3),b=(beta1.3+beta5.3), lwd=2,lty=3)

plot(reci, bio, type="n", main="RECI vs. Biomass by Growth Stage", xlab="RECI", ylab="Biomass (kg/ha)", 
     xlim=c(2.7,25.7), ylim=c(448.8,10444.5))
points(reci[growth.stage=="Seedling"], bio[growth.stage=="Seedling"], pch=16, col="darkgoldenrod1", cex=1)
points(reci[growth.stage=="Three Nodes"], bio[growth.stage=="Three Nodes"], pch=15, col="palegreen3",cex=1)
points(reci[growth.stage=="Flowering"], bio[growth.stage=="Flowering"], pch=17, col="indianred3" ,cex=1)
legend(17,9500,legend=c("Seedling", "Three Nodes", "Flowering"),
       pch=c(16,15,17), col=c("darkgoldenrod1","palegreen3","indianred3"), cex=c(0.7,0.7,0.7), bty="n")
abline(a= beta0.4, b=beta1.4, lwd=2, lty=2)
abline(a=(beta0.4 + beta2.4),b=(beta1.4+beta4.4), lwd=2 )
abline(a=(beta0.4 + beta3.4),b=(beta1.4+beta5.4), lwd=2,lty=3)

################################################################################
#                Regression: Index vs Each Growth Stage                        #
################################################################################
####################
# * * * NDVI * * * #
#################### 
#v6
ndvi.v <-  matrix(data = c(ndvi[growth.stage=="Seedling"], bio[growth.stage=="Seedling"]),ncol = 2)
ndvi.v <- as.data.frame(ndvi.v)
summary(lm(V1~V2, data=ndvi.v))

# three nodes
ndvi.n <-  matrix(data = c(ndvi[growth.stage=="Three Nodes"], bio[growth.stage=="Three Nodes"]),ncol = 2)
ndvi.n <- as.data.frame(ndvi.n)
summary(lm(V1~V2, data=ndvi.n))

# flowering
ndvi.f <-  matrix(data = c(ndvi[growth.stage=="Flowering"], bio[growth.stage=="Flowering"]),ncol = 2)
ndvi.f <- as.data.frame(ndvi.f)
summary(lm(V1~V2, data=ndvi.f))

#####################
# * * * OSAVI * * * #
##################### 
#v6
osavi.v <-  matrix(data = c(osavi[growth.stage=="Seedling"], bio[growth.stage=="Seedling"]),ncol = 2)
osavi.v <- as.data.frame(osavi.v)
summary(lm(V1~V2, data=osavi.v))

# three nodes
osavi.n <-  matrix(data = c(osavi[growth.stage=="Three Nodes"], bio[growth.stage=="Three Nodes"]),ncol = 2)
osavi.n <- as.data.frame(osavi.n)
summary(lm(V1~V2, data=osavi.n))

# flowering
osavi.f <-  matrix(data = c(osavi[growth.stage=="Flowering"], bio[growth.stage=="Flowering"]),ncol = 2)
osavi.f <- as.data.frame(osavi.f)
summary(lm(V1~V2, data=osavi.f))

####################
# * * * NDRE * * * #
#################### 
#v6
ndre.v <-  matrix(data = c(ndre[growth.stage=="Seedling"], bio[growth.stage=="Seedling"]),ncol = 2)
ndre.v <- as.data.frame(ndre.v)
summary(lm(V1~V2, data=ndre.v))

# three nodes
ndre.n <-  matrix(data = c(ndre[growth.stage=="Three Nodes"], bio[growth.stage=="Three Nodes"]),ncol = 2)
ndre.n <- as.data.frame(ndre.n)
summary(lm(V1~V2, data=ndre.n))

# flowering
ndre.f <-  matrix(data = c(ndre[growth.stage=="Flowering"], bio[growth.stage=="Flowering"]),ncol = 2)
ndre.f <- as.data.frame(ndre.f)
summary(lm(V1~V2, data=ndre.f))

####################
# * * * RECI * * * #
#################### 
#v6
reci.v <-  matrix(data = c(reci[growth.stage=="Seedling"], bio[growth.stage=="Seedling"]),ncol = 2)
reci.v <- as.data.frame(reci.v)
summary(lm(V1~V2, data=reci.v))

# three nodes
reci.n <-  matrix(data = c(reci[growth.stage=="Three Nodes"], bio[growth.stage=="Three Nodes"]),ncol = 2)
reci.n <- as.data.frame(reci.n)
summary(lm(V1~V2, data=reci.n))

# flowering
reci.f <-  matrix(data = c(reci[growth.stage=="Flowering"], bio[growth.stage=="Flowering"]),ncol = 2)
reci.f <- as.data.frame(reci.f)
summary(lm(V1~V2, data=reci.f))

