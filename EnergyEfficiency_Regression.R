engy_eff <- read.csv("data/regression/EnergyEfficiency.csv", stringsAsFactors = FALSE)
values <- data.frame(engy_eff)
cor(engy_eff)
pairs(engy_eff)

#disable package warnings for screenshotting purposes
options(warn=-1)

############## engy_eff Exploration ################

#Plot Heating Load againest Cooling Load
plot(engy_eff$HeatingLoad, engy_eff$CoolingLoad, xlab="Heating Load", ylab="Cooling Load")
#Plot Relative Compactness against the Surface Area
plot(engy_eff$RelativeCompactness, engy_eff$SurfaceArea, xlab="Relative Compactness", ylab="Surface Area")

#Summarize each predicted attribute
#attach(values)
summary(HeatingLoad)
summary(CoolingLoad)

#look at the two predicted characteristics
table(HeatingLoad)
table(CoolingLoad)

set.seed(1)

########## Create Prediction Model - Heating Load #############

mydata <- data.frame(engy_eff)
regression.model <- lm(HeatingLoad ~ SurfaceArea + RoofArea + WallArea)

plot(HeatingLoad, CoolingLoad, xlab="HeatingLoad", ylab="CoolingLoad")
abline(lm(HeatingLoad ~ CoolingLoad, data = mydata))
plot(regression.model, which = 1)

#predict Heating load based on Surface Area, RoofArea and WallArea vars
newdata <- data.frame(SurfaceArea=500.0, RoofArea=150.0, WallArea=300.0)
predict(regression.model, newdata)
#summarize the model in relation to r squared
summary(regression.model)$r.squared
predict(regression.model, newdata, interval="confidence")
predict(regression.model, newdata, interval="predict")

########## Create Prediction Model - Cooling Load #############

mydata1 <- data.frame(engy_eff)
regression.model1 <- lm(CoolingLoad ~ SurfaceArea + RoofArea + WallArea)
plot(CoolingLoad, HeatingLoad, xlab="x CoolingLoad", ylab="HeatingLoad")
abline(lm(CoolingLoad ~ HeatingLoad, data = mydata1))
methods(plot)
plot(regression.model1, which = 1)

#predict Cooling load based on Surface Area, RoofArea and WallArea vars
newdata1 <- data.frame(SurfaceArea=500.0, RoofArea=150.0, WallArea=300.0)
predict(regression.model1, newdata1)

summary(regression.model1)$r.squared
predict(regression.model1, newdata1, interval="confidence")
predict(regression.model1, newdata1, interval="predict")

