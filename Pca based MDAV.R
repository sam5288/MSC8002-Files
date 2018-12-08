#Data Sets



library(pander)
#=========================
# data
#=========================

library(sdcMicro)

data("EIA")

str(EIA)

#=========================
# Delete direct dentifier
#=========================

#UTILITYID
UTILITYID <- EIA$UTILITYID

EIA <- EIA[,6:15]
str(EIA)

#Part 1. clustering the data using PCA's


pander(summary(EIA))
EIA.trans <- scale(EIA, center = TRUE, scale = TRUE)
pander(summary(EIA.trans))


# Perform PCA on the data
EIA.pca <- princomp(EIA.trans)

# Plot first two components
biplot(EIA.pca)

summary(EIA.pca)

screeplot(EIA.pca, type="line")

pander(EIA.pca$loadings[,1:4])


#Part 2: replacing the values in clusters with their mean (Aggregation)


EIA.pcadata <- as.matrix(EIA.pca$scores[,1:4])

microData <- as.data.frame(EIA.pcadata)

sdc <- microaggregation(microData, method = "mdav", aggr= 4)

y <- sdc$mx

pander(head(y), caption = "R output for MDAV EIA data")

pander(head(sdc$x), caption = "Original data")

#Evaluation: Calculating Information Loss (a difference between original values and modified values)


dUtility(microData,xm=sdc$mx, method = "IL1s")

#table 9
pander(cbind(k=c(3,4,5,10,15,20,30,50),
IL=c(dUtility(microData,xm=microaggregation(microData, method = "mdav",
aggr= 3)$mx, method = "IL1s"),
dUtility(microData,xm=microaggregation(microData, method = "mdav",
aggr= 4)$mx, method = "IL1s"),
dUtility(microData,xm=microaggregation(microData, method = "mdav", 
aggr= 5)$mx, method = "IL1s"),
dUtility(microData,xm=microaggregation(microData, method = "mdav",
aggr= 10)$mx, method = "IL1s"),
dUtility(microData,xm=microaggregation(microData, method = "mdav",
                                       aggr= 15)$mx, method = "IL1s"),
dUtility(microData,xm=microaggregation(microData, method = "mdav",
                                       aggr= 20)$mx, method = "IL1s"),
dUtility(microData,xm=microaggregation(microData, method = "mdav",
                                       aggr= 30)$mx, method = "IL1s"),
dUtility(microData,xm=microaggregation(microData, method = "mdav",
                                       aggr= 50)$mx, method = "IL1s"))),

caption="PCA based MDAV algorithm evaluation")


