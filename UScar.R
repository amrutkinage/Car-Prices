library(MASS)
library(car)
library(ggplot2)
library(stringr)
library(dplyr)
library(corrplot)
library(cowplot)
car1 = read.csv('C:/Users/admin/Downloads/CarPrice_Assignment.csv')
car12 = read.csv('C:/Users/admin/Downloads/CarPrice_Assignment.csv')
## Checking for duplicates #
length(unique(car1$car_ID)) != dim(car1)[1]
length(unique(car1$car_ID))
## There are no duplicates in the data set 205 columns of unique data
## available in the data set

## Looking at the data set ##

str(car1)
summary(car1)
##***************************************************************************##
#                                                                             #
#                             Data Cleaning                                   #
#                                                                             #
##***************************************************************************##
# Checking for issues:
#  * NA values
#  * Car name column needs to be seperated. It as 2 delimiter "-" and " "
#  * symboling column needs to be converted into factor
#  * Converting factor varibale to numeric for using them in linear regression
##***************************************************************************##

#  1. NA values
## There are no NA values in the dataset.
colSums(is.na(car1))
#  2. Car name column needs to be seperated from model name. It as 2 delimiter "-" and " "
strsplit()
car1$CarName <- strsplit(car1$CarName, " ", 2)
car1$CarName <- str_split_fixed(car1$CarName, " ", 2)
car1$CarName <- sub('-romero*', '', car1$CarName)
car1$CarName <- car1$CarName['' ,-2]
car1$CarName <- car1$CarName[ ,-2]
## Converting to factor after the split
car1$CarName <- factor(car1$CarName, levels=unique(car1$CarName))
str(car1$CarName)

## want to check as we have lot of levels need to know if any manual error/correction possible.
check <- car1 %>% group_by(CarName) %>% summarise(count = n()) %>% arrange(desc(count))

## Observation 1: There is no car name as "Maxda" based on google search. Guess its a
## manual error and it should be "Mazda"
car1$CarName <- gsub("maxda", "mazda", car1$CarName)
## Observation 2: Nissan has been identified in 2 line items because of the upper case "N"
car1$CarName <- gsub("Nissan", "nissan", car1$CarName)
## Observation 3: Porsche has been miss spelled as "porcshce"
car1$CarName <- gsub("porcshce", "porsche", car1$CarName)
## Observation 4: toyota has been miss spelled as " toyouta"
car1$CarName <- gsub("toyouta", "toyota", car1$CarName)
## Observation 5: volkswagen has been miss spelled as "vokswagen" and has a abriviation as "VW"
car1$CarName <- gsub("vokswagen", "volkswagen", car1$CarName)
car1$CarName <- gsub("vw", "volkswagen", car1$CarName)
## Converting the CarName column into factor
car1$CarName <- factor(car1$CarName, levels=unique(car1$CarName))

## want to check the levels again
check1 <- car1 %>% group_by(CarName) %>% summarise(count = n()) %>% arrange(desc(count))

#  3. symboling column needs to be converted in to factor
car1$symboling <- factor(car1$symboling, levels = unique(car1$symboling))

############### Analysis on Data ##########################################

## Lets visualise the variables and see for relationships.

p1 <- ggplot(car1, aes(x=car1$fueltype)) + geom_bar()
p2 <- ggplot(car1, aes(x=car1$symboling)) + geom_bar()
p3 <- ggplot(car1, aes(x=car1$aspiration)) + geom_bar()
p4 <- ggplot(car1, aes(x=car1$doornumber)) + geom_bar()
p5 <- ggplot(car1, aes(x=car1$carbody)) + geom_bar()
p6 <- ggplot(car1, aes(x=car1$drivewheel)) + geom_bar()
p7 <- ggplot(car1, aes(x=car1$enginelocation)) + geom_bar()
p8 <- ggplot(car1, aes(x=car1$enginetype)) + geom_bar()
p9 <- ggplot(car1, aes(x=car1$cylindernumber)) + geom_bar()
p10 <- ggplot(car1, aes(x=car1$fuelsystem)) + geom_bar()
p11 <- ggplot(car1, aes(x=car1$CarName)) + geom_bar()

plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
## Observations on ploting 10 categorical variable considered in the sample.
## In fuel type - Gas is preferred over diesel
## symboling which has 0 have more count and very few has -2
## car aspiration standard is preferred over turbo
## Car door number 4 is preferred over 2
## Car body Sydan and Hatchback are prefered 
## drive wheel fwd is having the major numbers 
## engine location front is having a significant number 
## Engine type ohc is prefered over others 
## cylinder number 4 is prefered over others 
## fuel system mpfi and 2bbl are prferred 
p11
## in the car name major player is toyota and followed by few playes
## like nissan, mazda, volvo, subaru, volkswagen, honda

## Lets build a Correlation plot for continuous variable and see for variation
corrplot(cor(car1[, c(10:14,17, 19:26)]), method = "circle")
 ## Variables that has significant positive Correlation with price are
## Engine size, Horse Power, curbweight, car width, car length 
## Variables that has negative correlation with price are
## citympg and highway mpg.

#  4. Converting factor varibale to numeric for using them in linear regression

############# Converting column with 2 categorical variable ########################
## Converting fuel type to numeric 
levels(car1$fueltype)<-c(1,0)
## Converting aspiration to numeric 
levels(car1$aspiration)<-c(1,0)
## Converting door number to numeric 
levels(car1$doornumber)<-c(1,0)
## Converting enginelocation to numeric 
levels(car1$enginelocation)<-c(1,0)

############ Converting columns with more than 2 categorical variable ##############

# Converting "carbody" into dummies . 

dummy_1 <- data.frame(model.matrix( ~carbody, data = car1))
# Removing the unwanted column from the data set  
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set
car1 <- cbind(car1,dummy_1)


# Converting "drivewheel" into dummies . 
dummy_1 <- data.frame(model.matrix( ~drivewheel, data = car1))
# Removing the unwanted column from the data set  
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set
car1 <- cbind(car1,dummy_1)


# Converting "cylindernumber" into dummies . 
dummy_1 <- data.frame(model.matrix( ~cylindernumber, data = car1))
# Removing the unwanted column from the data set  
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set
car1 <- cbind(car1,dummy_1)

# Converting "carname" into dummies . 
dummy_1 <- data.frame(model.matrix( ~CarName, data = car1))
# Removing the unwanted column from the data set  
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set
car1 <- cbind(car1,dummy_1)


# Converting "symboling" into dummies . 
dummy_1 <- data.frame(model.matrix( ~symboling, data = car1))
# Removing the unwanted column from the data set  
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set
car1 <- cbind(car1,dummy_1)


# Converting "enginetype" into dummies . 
dummy_1 <- data.frame(model.matrix( ~enginetype, data = car1))
# Removing the unwanted column from the data set  
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set
car1 <- cbind(car1,dummy_1)


# Converting "fuelsystem" into dummies . 
dummy_1 <- data.frame(model.matrix( ~fuelsystem, data = car1))
# Removing the unwanted column from the data set  
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set
car1 <- cbind(car1,dummy_1)


car <- car1[ , -(c(1, 2, 3, 7, 8, 15, 16, 18))]


#################### Divide into training and test data set ##############
#set the seed to 100, let's run it 

set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car), 0.7*nrow(car))
# generate the train data set
train = car[trainindices,]
#Similarly store the rest of the observations into an object "test".
test = car[-trainindices,]

#################### Buid the 1st model ###################################

#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)
# Check the summary of model. 
summary(model_1)

## I am getting 9 not defined because of singularities lets run Step AIC 
## to eliminate the not significant varibales from the model

step <- stepAIC(model_1, direction = "both")
step

## It has eliminated 33 variable from the final model. Let us continue with 
## model building 

model_2 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + peakrpm + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                cylindernumberfive + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + symboling1 + 
                enginetypeohc + fuelsystem2bbl + fuelsystemmpfi, data = train)
summary(model_2)
sort(vif(model_2))
## On seeing the combination of VIF and P-Value we can eliminate - citympg

model_3 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                cylindernumberfive + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + symboling1 + 
                enginetypeohc + fuelsystem2bbl + fuelsystemmpfi, data = train)
summary(model_3)
sort(vif(model_3))
## On seeing the combination of VIF and P-Value we can eliminate - fuelsystemmpfi

model_4 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                cylindernumberfive + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + symboling1 + 
                enginetypeohc + fuelsystem2bbl, data = train)

summary(model_4)
sort(vif(model_4))
## On seeing the combination of VIF and P-Value we can eliminate - fuelsystem2bbl

model_5 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                cylindernumberfive + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + symboling1 + 
                enginetypeohc, data = train)

summary(model_5)
sort(vif(model_5))
## On seeing the combination of VIF and P-Value we can eliminate - carbodyhardtop


model_6 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + peakrpm + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                cylindernumberfive + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + symboling1 + 
                enginetypeohc, data = train)


summary(model_6)
sort(vif(model_6))
## On seeing the combination of VIF and P-Value we can eliminate - carbodysedan

model_7 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + peakrpm + 
                carbodyhatchback + carbodywagon + drivewheelrwd + 
                cylindernumberfive + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + symboling1 + 
                enginetypeohc, data = train)
summary(model_7)
sort(vif(model_7))
## On seeing the combination of VIF and P-Value we can eliminate - carbodyhatchback


model_8 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + peakrpm + 
                carbodywagon + drivewheelrwd + 
                cylindernumberfive + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + symboling1 + 
                enginetypeohc, data = train)

summary(model_8)
sort(vif(model_8))
## On seeing the combination of VIF and P-Value we can eliminate - carbodywagon


model_9 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + peakrpm + drivewheelrwd + 
                cylindernumberfive + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + symboling1 + 
                enginetypeohc, data = train)

summary(model_9)
sort(vif(model_9))
## On seeing the combination of VIF and P-Value we can eliminate - symboling


model_10 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + enginesize + stroke + peakrpm + drivewheelrwd + 
                 cylindernumberfive + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_10)
sort(vif(model_10))
## On seeing the combination of VIF and P-Value we can eliminate - carnamemercury

model_11 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + enginesize + stroke + peakrpm + drivewheelrwd + 
                 cylindernumberfive + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_11)
sort(vif(model_11))

## All the p-Values are significant. So on seeing the VIF table we can eliminate curbweight since it has 2 *

model_12 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + 
                 cylindernumberfive + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_12)
sort(vif(model_12))
## All the P-Values are significant except one so lets remove cylindernumberfive

model_13 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                 peakrpm + drivewheelrwd + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_13)

sort(vif(model_13))
## All the P-values are significant so lets check for multicoliniarity and drop enginesize

model_14 <- lm(formula = price ~ aspiration + enginelocation + carwidth + stroke + 
                 peakrpm + drivewheelrwd + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_14)
sort(vif(model_14))
## On considering the P-Values and VIF combination lets drop drivewheelrwd


model_15 <- lm(formula = price ~ aspiration + enginelocation + carwidth + stroke + 
                 peakrpm + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_15)
sort(vif(model_15))
## On considering the P-Values and VIF combination lets drop stroke

model_16 <- lm(formula = price ~ aspiration + enginelocation + carwidth +
                 peakrpm + cylindernumbertwo + CarNamebmw + CarNamedodge + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_16)
sort(vif(model_16))
## On considering the P-Values and VIF combination lets drop peakrpm

model_17 <- lm(formula = price ~ aspiration + enginelocation + carwidth +
                 cylindernumbertwo + CarNamebmw + CarNamedodge + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNameporsche + CarNamerenault + CarNamesaab + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_17)
sort(vif(model_17))
## On considering the P-Values and VIF combination lets drop carnamesaab

model_18 <- lm(formula = price ~ aspiration + enginelocation + carwidth +
                 cylindernumbertwo + CarNamebmw + CarNamedodge + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNameporsche + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_18)
sort(vif(model_18))
## On considering the P-Values and VIF combination lets drop cylindernumbertwo

model_19 <- lm(formula = price ~ aspiration + enginelocation + carwidth +
                 CarNamebmw + CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNameporsche + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_19)
sort(vif(model_19))
## On considering the P-Values and VIF combination lets drop aspiration

model_20 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamedodge + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNameporsche + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_20)
sort(vif(model_20))
## On considering the P-Values and VIF combination lets drop carnameporsche

model_21 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamedodge + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_21)
sort(vif(model_21))
## On considering the P-Values and VIF combination lets drop carnamedodge

model_22 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_22)
sort(vif(model_22))
## On considering the P-Values and VIF combination lets drop carnameplymouth

model_23 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_23)
sort(vif(model_23))
## Nothing from VIF. But, carnamehonda's P-Value has gone up. SO lets remove it from our next model.


model_24 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_24)
sort(vif(model_24))
## No multicoliniarity. It looks like a good model. byt lets try and take the higher value with one *
## carnamenissan

model_25 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamepeugeot + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + enginetypeohc, data = train)

summary(model_25)
sort(vif(model_25))
## Nothing from VIF. and lets remove one with one * and see if the R values go down
##carnamevolkswagen


model_26 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamemitsubishi + CarNamepeugeot + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + enginetypeohc, data = train)

summary(model_26)
sort(vif(model_26))
## Nothing from VIF. All the P-values are significant. Lets see if we get major variation if we drop
## A variable which is close to 0.05 with one *. carnamemitsubishi

model_27 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamepeugeot + CarNamerenault + CarNamesubaru + CarNametoyota + enginetypeohc, data = train)

summary(model_27)
sort(vif(model_27))
## Nothing from VIF. All the P-values are significant. Lets see if we get major variation if we drop
## A variable which is close to 0.05 with one *. carnametoyota

model_28 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamepeugeot + CarNamerenault + CarNamesubaru + enginetypeohc, data = train)

summary(model_28)
sort(vif(model_28))
## Nothing from VIF. All the P-values are significant. Lets see if we get major variation if we drop
## A variable which is close to 0.05 with one *. carnamerenault

model_29 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamejaguar + CarNamemazda + CarNamebuick + 
                 CarNamepeugeot + CarNamesubaru + enginetypeohc, data = train)

summary(model_29)
sort(vif(model_29))
## This looks like a good model as all the P-values are below 0.05 and there are not VIF scores above 2.
## But lets see if we can drop one of the items having two *. I consider droping carnamemazda and check the results


model_30 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamejaguar + CarNamebuick + 
                 CarNamepeugeot + CarNamesubaru + enginetypeohc, data = train)

summary(model_30)
sort(vif(model_30))
## This looks like a good model as all the P-values are below 0.05 and there are not VIF scores above 2.
## But lets try to drop a variable carnamepeugeot


model_31 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamejaguar + CarNamebuick + 
                 CarNamesubaru + enginetypeohc, data = train)

summary(model_31)
vif(model_31)
## This looks like a good model as all the P-values are below 0.05 and there are not VIF scores above 2.
## But lets see if the values drop one more carnamesubaru

model_32 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamejaguar + CarNamebuick + 
                 enginetypeohc, data = train)

summary(model_32)
vif(model_32)
## which also has a significant P-value. We need to keep this as well in our model

## for further analysis.
##Lets try and test it in our model and check for R.Sq
Predict_1 <- predict(model_32,test[,-18])
test$test_price <- Predict_1
# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

## There is a heauge difference in the R.sq value 0.90 in train and .80 in test. 
## Lets try adding one continuous variable and check for variation and accuracy. 
## In the earlier correlation plot we saw there was high correlation for price and
## horse power so lets try considering it the model and see if we can have a mangeable
## VIF score and test and train data test results on R.Sq value.

model_33 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamejaguar + CarNamebuick + 
                 enginetypeohc + horsepower, data = train)
summary(model_33)
vif(model_33)
## for further analysis.
##Lets try and test it in our model and check for R.Sq
Predict_1 <- predict(model_33,test[,-18])
test$test_price <- Predict_1
# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

## After adding the horsepower variable this model has become more stable and the r.sq from train and test are .93 and .85 
## respectively. 

ggplot(test, aes(x=c(1:nrow(test)))) + geom_line(aes(y=test_price), col = 'red') + geom_line(aes(y=price), col = 'black')
## On comparing the price prediction with prices there seems to be a good overlap. This should be a good model to propose.

## But after seeing the VIF we see two variables are nearing 3. Lets see if we drop carwidth and see what difference 
## It makes to the model.
model_34 <- lm(formula = price ~ enginelocation + CarNamebmw + CarNamejaguar + CarNamebuick + 
                 enginetypeohc + horsepower, data = train)
summary(model_34)
vif(model_34)
## On seeing the P-value of enginetypeohc its above 0.5 and lets remove it and see 
model_34a <- lm(formula = price ~ enginelocation + CarNamebmw + CarNamejaguar + CarNamebuick + 
                  horsepower, data = train)
summary(model_34a)
vif(model_34a)


## for further analysis.
##Lets try and test it in our model and check for R.Sq
Predict_1 <- predict(model_34a,test[,-18])
test$test_price <- Predict_1
# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2
## Droping carwidth and droping enginetypeohc is not doing any good as the variation between the R.sq from test and train is aroud.15
## So the best model to propose is model_33 which has less variation while testing the test data and train data


## In model_33 the enginetypeohc is having one * so lets try to remove it and check for variations

model_35 <- lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamejaguar + CarNamebuick + 
                 horsepower, data = train)
summary(model_35)
vif(model_35)

## for further analysis.
##Lets try and test it in our model and check for R.Sq
Predict_1 <- predict(model_35,test[,-18])
test$test_price <- Predict_1
# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2
## With S.qr from Train and Test beeing 0.93 and 0.86 respectively and also has the P-Values significant 
## for all the variables. We should keep this model and not model_33. 

## lets plot this and see the curves for variations if any
ggplot(test, aes(x=c(1:nrow(test)))) + geom_line(aes(y=test_price), col = 'red') + geom_line(aes(y=price), col = 'black')
## On comparing the price prediction with prices there seems to be a good overlap. This should be a good model to propose.

########################Conclusion############################################
##We have 5 independent variables impacting the prices when it comes to US market
##Engine location rear
##Car width
##BMW
##Jaguar
##Buick
##Horse Power
##Also the intersect is a negative value which means that the slop is steep. A minimal increase
##In variable X will have a significant increase in Y.

# Any results you write to the current directory are saved as output. 