#Script for generating Heat Map of Variance/CoV Matrix
#Pulls data from Katze's LabKey install
#
# setwd("C://Users//rasm//Desktop//Heatmap-app//smriti_heatmap-app_gabby_11_20//mice-heritability")
#C:\Users\rasm\Desktop\Heatmap-app\smriti_heatmap-app_gabby_11_20\mice-heritability
dat<-read.csv("Testdat.csv", na.strings=c("","NA","na","n/a","N/A"))
str(dat)
#Can either rename before, or rename now.
#Also can strip out unnecessare columns like Sex for the current flu/sars data

#Can calculate heritabilities as follows
a<-anova(lm(MM.R1.singles~Cross, data=dat))
interclass.corr<-(a$"Mean Sq"[1]-a$"Mean Sq"[2])/(a$"Mean Sq"[1]+(3-1)*a$"Mean Sq"[2])
#0.7908131

###DWB: or from the textbook (Gelman and Hill 2007) definition of ICC:
require("nlme")
me.fit <- lme(fixed=MM.R1.singles~1, random=~1|Cross, data=dat)
temp <- VarCorr(me.fit)
as.numeric(temp["(Intercept)",1])/sum(as.numeric(temp[,"Variance"]))#0.7901959

###end DWB

###DWB:  As this is essentially the same as above except for the 2* correction to the denominator, does this actually provide more info?
coef.genet.det<-(a$"Mean Sq"[1]-a$"Mean Sq"[2])/(a$"Mean Sq"[1]+((2*3)-1)*a$"Mean Sq"[2])

#Where the 3 in (3-1) or ((2*3)-1) is the within-group n (so should be an average).  For this flow set of mocks
#that is fine, but for something like weight, depending on the day, n is different.
#Either estimate values or just go with a ballpark.

#If we want to get fancy, this can be a function
# for i in 1 to Number of phenotypes
# calculate the interclass.corr and coef.genet.det
#write a new table with
#Phenotype/interclass.corr/coef.genet.det
#Then plot phenotypes on the X and the two
# values each on the y...scale is same.
#If this sits underneath the heatmap and
#matches up, then you have covariation between
#phenotypes and a measure of genetic control
#in one easy figure.  Alternately in shiny, the
#heritability metrics could be part of the hover-over?
V<-var(dat[,9:48], na.rm=TRUE)
#phenotypes should probably be appropriately transformed before this...
#Log transform for cell#s, etc
cov.cor<-cov2cor(V)
cov.cor.m<-data.matrix(cov.cor)
hmn<-heatmap(cov.cor.m, col=heat.colors(256), margins=c(10,9))

################################################################

# this is across all 29 unique Crosses, look at covariance heatmap by individual Cross

################################################################

length(unique(dat[,"Cross"]))

################################################################

# All crosses

# dat[,9:48] -> full

# V1 <- var(full, na.rm=TRUE)
# cov.cor1 <- cov2cor(V1)
# cov.cor1.m <- data.matrix(cov.cor1)
# hmn_cross1 <- heatmap(cov.cor1.m, col=heat.colors(256), margins=c(10,9))

cov.cor.m

################################################################

# first cross
dat[as.vector(unlist(sapply(unique(dat[,"Cross"])[1],function(x)which(x==dat[,"Cross"])))),9:48] -> first

# remove full NA columns
first[,!as.vector(unlist(sapply(1:dim(first)[2],function(x)sum(is.na(first[,x])) == dim(first)[1])))] -> first_v2
V1 <- var(first_v2)
cov.cor1 <- cov2cor(V1)
cov.cor1.m <- data.matrix(cov.cor1)
# hmn_cross1 <- heatmap(cov.cor1.m, col=heat.colors(256), margins=c(10,9),na.rm=FALSE)

################################################################

# second cross
dat[as.vector(unlist(sapply(unique(dat[,"Cross"])[2],function(x)which(x==dat[,"Cross"])))),9:48] -> second

# remove full NA columns
second[,!as.vector(unlist(sapply(1:dim(second)[2],function(x)sum(is.na(second[,x])) == dim(second)[1])))] -> second_v2
V1 <- var(second_v2)
cov.cor1 <- cov2cor(V1)
cov.cor1.m <- data.matrix(cov.cor1)
# hmn_cross1 <- heatmap(cov.cor1.m, col=topo.colors(256), margins=c(10,9),na.rm=FALSE)

# legend
plot.new()		
image(as.matrix(1:100), col=topo.colors(256),axes=F)
axis(side=1,at=c(0,.25,.5,.75,1),labels=seq(range(cov.cor1.m)[1],range(cov.cor1.m)[2],length.out=5))
################################################################

# NA issue, "3032x16188", since numbers consist of only -1, 1, shiny heatmap throws error
dat[as.vector(unlist(sapply("3032x16188",function(x)which(x==dat[,"Cross"])))),9:48] -> second

# remove full NA columns
second[,!as.vector(unlist(sapply(1:dim(second)[2],function(x)sum(is.na(second[,x])) == dim(second)[1])))] -> second_v2
V1 <- var(second_v2)
cov.cor1 <- cov2cor(V1)
cov.cor1.m <- data.matrix(cov.cor1)
# hmn_cross1 <- heatmap(cov.cor1.m, col=topo.colors(256), margins=c(10,9),na.rm=FALSE)

# legend
plot.new()		
image(as.matrix(1:100), col=topo.colors(256),axes=F)
axis(side=1,at=c(0,.25,.5,.75,1),labels=seq(range(cov.cor1.m)[1],range(cov.cor1.m)[2],length.out=5))

# NA issue, 
dat[as.vector(unlist(sapply("13140x3015",function(x)which(x==dat[,"Cross"])))),9:48] -> second

# remove full NA columns
second[,!as.vector(unlist(sapply(1:dim(second)[2],function(x)sum(is.na(second[,x])) == dim(second)[1])))] -> second_v2
V1 <- var(second_v2, na.rm=T)
#remove 0's in columns

colnames(V1)[which(sapply(1:dim(V1)[1],function(x)sum(V1[,x] == "0") == dim(V1)[2]))] -> remove_var

V1[-as.vector(unlist(sapply(remove_var,function(x)which(x==row.names(V1))))),] -> V2

V2[,-as.vector(unlist(sapply(remove_var,function(x)which(x==colnames(V2)))))] -> V3

cov.cor1 <- cov2cor(V3)
cov.cor1.m <- data.matrix(cov.cor1)
# hmn_cross1 <- heatmap(cov.cor1.m, col=topo.colors(256), margins=c(10,9),na.rm=FALSE)

# legend
plot.new()		
image(as.matrix(1:100), col=topo.colors(256),axes=F)
axis(side=1,at=c(0,.25,.5,.75,1),labels=seq(range(cov.cor1.m)[1],range(cov.cor1.m)[2],length.out=5))

################################################################



