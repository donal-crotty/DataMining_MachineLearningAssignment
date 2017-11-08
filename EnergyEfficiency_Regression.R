engy_eff <- read.csv("data/regression/EnergyEfficiency.csv", stringsAsFactors = FALSE)
#add engy_eff to a dataframe
values <- data.frame(engy_eff)

############## engy_eff Exploration ################

#Plot Heating Load againest Cooling Load
plot(engy_eff$HeatingLoad, engy_eff$CoolingLoad, xlab="Heating Load", ylab="Cooling Load")
#Plot Relative Compactness against the Surface Area
plot(engy_eff$RelativeCompactness, engy_eff$SurfaceArea, xlab="Relative Compactness", ylab="Surface Area")
#Summarize each attribute
attach(values)
summary(RelativeCompactness)
summary(SurfaceArea)
summary(WallArea)
summary(RoofArea)
summary(OverallHeight)
summary(Orientation)
summary(GlazingArea)
summary(GlazingAreaDistribution)
summary(HeatingLoad)
summary(CoolingLoad)
#look at the two predicted characteristics
table(HeatingLoad)
table(CoolingLoad)

######## Training and Testing Sets #############
set.seed(1)
engy_eff_rand <- engy_eff[order(runif(1000)), ]
engy_eff_train <- engy_eff_rand[1:900, ]
engy_eff_test  <- engy_eff_rand[901:1000, ]

########## Create Prediction Model - Heating Load #############

mydata <- data.frame(engy_eff_train,engy_eff_test)
regression.model <- lm(HeatingLoad ~ CoolingLoad)
plot(HeatingLoad, CoolingLoad, xlab="HeatingLoad", ylab="CoolingLoad")
abline(lm(HeatingLoad ~ CoolingLoad, data = mydata))
methods(plot)
?plot.lm
plot(regression.model, which = 1)

########## Create Prediction Model - Cooling Load #############

mydata1 <- data.frame(engy_eff_train,engy_eff_test)
regression.model1 <- lm(CoolingLoad ~ HeatingLoad)
plot(CoolingLoad, HeatingLoad, xlab="x CoolingLoad", ylab="HeatingLoad")
abline(lm(CoolingLoad ~ HeatingLoad, data = mydata))
methods(plot)
?plot.lm
plot(regression.model, which = 1)

