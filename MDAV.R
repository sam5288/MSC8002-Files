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

MDAV.EIA <- EIA[,6:15]

#Part 1. clustering the data using PCA's


pander(summary(MDAV.EIA))
MDAV.EIA.trans <- scale(MDAV.EIA, center = TRUE, scale = TRUE)
pander(summary(MDAV.EIA.trans))

#Part 2: replacing the values in clusters with their mean (Aggregation)

microData <- as.data.frame(MDAV.EIA.trans)
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
       
       caption="MDAV algorithm evaluation")




       