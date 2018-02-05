# ***********************
#       background      *
# ***********************

# the data set only focus on men

# *******************************
#      read & view the data     *
# *******************************
library(tidyverse)
library("MASS")
library(leaps)
library(faraway)
library(glmnet)
library(car)

data<- read.csv("BodyFat.csv", header = TRUE) 
dim(data)
str(data)
names(data)
data<- data[ ,-1] # do not need the index column
head(data)
summary(data) # BODYFAT contains one 0 record and an extremly large record!!!
attach(data)

# find the two "strange" records through summary
(zero <- which(BODYFAT == 0))
(large <- which(BODYFAT == max(BODYFAT)))

## check the 0 BODYFAT record >>> EXCLUDE it
data[zero,]
data[zero+(-2:2),]
# try to fix the issue by calculate BODYFAT using DENSITY
(cal_fat <- 495/DENSITY[zero] - 450)
# got a negative value!!! >>> throw away the bad record T_T
bad <- zero

## check the extreme BODYFAT record >>> INCLUDE it
data[large,]
data[large+(-2:2),]
# check whether the BODYFAT record is wrong or not
(cal_fat <- 495/DENSITY[large] - 450) # calculated value is close to recorded one
ADIPOSITY[large] %in% sort(ADIPOSITY, decreasing = TRUE)[1:5]
CHEST[large] %in% sort(CHEST, decreasing = TRUE)[1:5]
# his ADIPOSITY & CHEST is also large >>> the large BODYFAT seems more reasonable

# ***************************************
#    General demographic information    *
# ***************************************
par(mfrow = c(2,2))
hist(BODYFAT,breaks=30,
     main="Histogram of Body Fat %",xlab="Body Fat %")

hist(AGE,breaks=30,
     main="Histogram of Age",xlab="Age (year) %")

hist(WEIGHT,breaks=30,
     main="Histogram of Weight",xlab="Weight (lbs)")

hist(HEIGHT,breaks=30,
     main="Histogram of Height",xlab="Height (inches)")
par(mfrow = c(1,1))

# **********************
#     data cleaning    *
# **********************

# *** fit full model exclude the zero bodyfat bad point *** #
m1 <- lm(BODYFAT ~ ., data = data[-bad, -2])
summary(m1)
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))

## check cook's distance
plot(m1, which = 4)
abline(h = 4/(nrow(data)-ncol(data)), lty = 2) ## 42, 39, 86 need to check them

## check the abnormal points
data[39,] ## extremely large weight with normal height
# * in detail
WEIGHT[39] %in% sort(WEIGHT, decreasing = TRUE)[1:5]
WEIGHT[39] == max(WEIGHT) # the largest weight 
(cal_fat <- 495/DENSITY[39] - 450) # seems reasonable ...
summary(HEIGHT) # this guy's height is 3rd Qu. >>> normal
summary(ADIPOSITY) # largest BMI
summary(BODYFAT)

data[42,] ## extremely small height with normal weight
summary(HEIGHT)
sort(HEIGHT)[1:5] # it is rare for men to have this height... REMOVE
bad <- c(bad, 42)

data[86,] ## seems normal, keep it.

# *** check siri formula *** #
reverse_de <- 1/DENSITY
plot(BODYFAT ~ reverse_de)
m0 <- lm(BODYFAT ~ reverse_de)

par(mfrow = c(2,2))
plot(m0) ### find three abnormal points: 96, 76, 48
par(mfrow = c(1,1))

check <- c(96, 76, 48)
data[check, ]

# check 96
data[which(WEIGHT > 224 & WEIGHT < 225), ]
495/DENSITY[96]- 450 # too small and his all measurement seems normal, not reasonable >>> use body fat

# check 76
495/DENSITY[76]- 450 # 14.09151 
data[which(WEIGHT > 148 & WEIGHT < 149), ] # compare to 24
### the measurement of YEAR NECK CHEST ABDOMEN  HIP for 76 are larger than 24, it is impossible for him to have a smaller body fat!!! keep body fat !!!

# check 48
495/DENSITY[48]- 450 # 14.13502
data[which(WEIGHT > 148 & WEIGHT < 149), ] # compare to 24
### all of his measurement are quite similar to 24, it is impossible for his to have so small body fat, keep density!!!
data$BODYFAT[48] <- round(495/data$DENSITY[48]- 450, digits = 2)

# *** check the relationship between BMI & HEIGHT WEIGHT *** #
BMI <- (WEIGHT/2.2046226218)/((HEIGHT*0.0254)^2)
boxplot(BMI - ADIPOSITY)
which(abs(BMI - data$ADIPOSITY) > 1) # Find 3 abnormal points  42 163 221
# we havw already dealt with the 42th point

### check 163
data[163, ]
BMI[163]
data[which(WEIGHT > 183 & WEIGHT < 185 & HEIGHT > 67 & HEIGHT < 69), ] 
# compare 19 with 163: They have basically the same HEIGHT and WEIGHT, CHEST 19 >> 163, ABDOMEN 19 < 163, BICEPS 19 >> 163 ---> indicates that 19 is in a better shape than 163, HOWEVER, the BODYFAT of 163 is SMALLER than 19, make no sense! (Unless the 163 guy is skinny without a good shape. We conclude that there is something wrong with the HEIGHT or WEIGHT for 163

### check 221
data[221,]
BMI[221]
data[which(WEIGHT > 153 & WEIGHT < 155 & HEIGHT > 69 & HEIGHT < 71), ]
# compare 220 with 221: WEIGHT 221 < 220, HEIGHT 221 > 220, BODYFAT 221 < 220 WRIST 221 > 220 ANKLE 221 > 220 >> The 220 guy has less fat & larger bone, so he should look thinner than 221 guy. HOWEVER, most measurement of 221 guy is larger than 220 guy, make no sense. We conclude that there is something wrong with the HEIGHT or WEIGHT for 221

## TODO try to make up these 2 records
bad <- c(bad, 221, 163)

### fit the model again after delete 4 points
detach(data)
data <- data[-bad, -2]
attach(data)
names(data)

m1 <- lm(BODYFAT ~ ., data)
summary(m1)
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))

# ******************************
#       variable selection     *
# ******************************

### Mallow-cp
x <- model.matrix(m1)[,-1]
y <- BODYFAT
g <- leaps(x,y, nbest=1)
Cpplot(g) # the selected models seem to complicated

### AIC
m_null <- lm(BODYFAT ~ 1, data)
m_AIC_back <- step(m1, k=2)
m_AIC_for <- step(m_null, direction="forward",
                       scope=list(lower=~1,upper=m1))
m_AIC_both <- step(m_null, direction="both",
                        scope=list(lower=~1, upper=m1))  # the selected models seem to complicated

m_BIC_back <- step(m1, k=log(nrow(data)-1)) # WEIGHT, ABDOMEN, FOREARM, WRIST
m_BIC_for <- step(m_null, direction="forward", 
                       scope=list(lower=~1,upper=m1), k=log(nrow(data)-1))  # WEIGHT, ABDOMEN
m_BIC_both <- step(m_null, direction="both",
                        scope=list(lower=~1,upper= m1), k=log(nrow(data)-1))  # WEIGHT, ABDOMEN

# ********************
#    fit the model   *
# ********************
m2 <- m_BIC_both # keep only ABDOMEN, WEIGHT
(s2 <- summary(m2))
(mse2 <- sum((s2$residuals)^2)/nrow(data))
# round the model to make it eaasier to calculate
fit <- -40 + ABDOMEN - 0.2*WEIGHT 
mse <- sum((fit - BODYFAT)^2)/nrow(data)
res <- fit - BODYFAT

m3 <- lm(BODYFAT ~ ABDOMEN)
summary(m3)

m4 <- lm((BODYFAT)*WEIGHT ~ ABDOMEN + WEIGHT, data) # transform
summary(m4)
(mse4 <- sum((m4$residuals/WEIGHT)^2)/nrow(data)) # worse

m5 <- lm(BODYFAT ~ WEIGHT + ABDOMEN + FOREARM + WRIST, data) # WEIGHT, ABDOMEN, FOREARM, WRIST  
(s5 <- summary(m5))
(mse5 <- sum((s5$residuals)^2)/nrow(data))
# round the model
crPlot(m5)

# LASSO
set.seed(2018)
m6 <- cv.glmnet(x = as.matrix(data[, -1]),y = BODYFAT, alpha = 1, nfolds = 10)
plot(m6) # the model corresponding to minimum is too complicate 
m6$lambda.1se
which(m6$lambda == m6$lambda.1se)
lasso.coef = coef(m6, s = m6$lambda.1se) ## even if we use the 1se model, 5 vars, it still too complicated to use

# RIDGE
set.seed(2018)
m7 = cv.glmnet(x = as.matrix(data[, -1]),y = BODYFAT, alpha = 0, nfolds = 10)
plot(m7)
m7$lambda.1se
which(m7$lambda == m7$lambda.1se)
ridge.coef = coef(m7, s = m7$lambda.1se)

## check model. 
anova(m5, m1) # compare full model and model with 4 vars, not significant
anova(m2, m5) # compare model with 2 vars and model with 4 vars, not significant

# ************************
#     model diagnose     *
# ************************

# Normality
shapiro.test(m2$residuals) # passed

# Multi-collinearity
vif(m2) # no colinearity

# homoscedasticity 
ncvTest(m2) # constant error variance checked

# Nonlinearity
crPlots(m2) # linearty for WEIGHT is not good

# try to transform WEIGHT
bc <- boxcox(WEIGHT~1, data = data, lambda = seq(-10, 10, length = 10))
trans <- bc$x[which.max(bc$y)]
W2 <- WEIGHT^trans
mt<- lm(BODYFAT ~ ABDOMEN + WEIGHT + W2)
(mset <- sum((mt$residuals)^2)/nrow(data)) # 15.59839

crPlots(mt) # good
summary(mt) # mse smaller

par(mfrow = c(2,2))
plot(mt)
par(mfrow = c(1,1))

# Normality
shapiro.test(mt$residuals) # passed

# Multi-collinearity
vif(mt) # no colinearity

# homoscedasticity 
ncvTest(mt) # constant error variance checked

# round model
fit2 <- -55 + 0.9 * ABDOMEN - 0.00036 * (WEIGHT^2)


mse <- sum((fit2 - BODYFAT)^2)/nrow(data)
res <- fit2 - BODYFAT
summary(res)


# Normality
shapiro.test(m5$residuals) # passed

# Multi-collinearity
vif(m5) # no colinearity

# homoscedasticity 
ncvTest(m5) # constant error variance checked

# Nonlinearity
crPlots(m5) # linearty for WEIGHT is not good

mt_2 = lm(BODYFAT~WEIGHT+W2+ABDOMEN+FOREARM+WRIST, data)
summary(mt_2)
crPlots(mt_2)
(mset_2 <- sum((mt_2$residuals)^2)/nrow(data)) # 15.59839

# Normality
shapiro.test(mt_2$residuals) # passed

# Multi-collinearity
vif(mt_2) # no colinearity

# homoscedasticity 
ncvTest(mt_2) # constant error variance checked


# ********************
#    rule of thumb   *
# ********************
m2
# round the model to make it eaasier to calculate
fit <- -40 + ABDOMEN - 0.2*WEIGHT 
(mse <- sum((fit - BODYFAT)^2)/nrow(data))

m5
fit2 <- -50 - 0.1*WEIGHT + ABDOMEN + 0.5*FOREARM - WRIST
(mse2 <- sum((fit2 - BODYFAT)^2)/nrow(data))

sample <- data.frame(WEIGHT = 170, W2 = 170^trans, ABDOMEN = 90, FOREARM = 28, WRIST = 17)
predict.lm(mt_2, newdata = sample, interval = 'confidence')
predict(m_t, newdata = test, interval = 'confidence')

# fit value using rule1
(sample_fit1 <- -40 + 90 - 0.2*170)

# fit value using rule2
(sample_fit2 <- -50 - 0.1*170 + 90 + 0.5*28 - 17)



