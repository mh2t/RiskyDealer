library(dplyr)
library(data.table)
library(lubridate)
library(caret)
library(stringr)
library(ISLR)
library(gam)
library(splines)


# read data
df <- read.csv('./case.csv')

##############################################################################
# compute number of NA
##############################################################################
# return the number of missing values
na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
(na_count <- data.frame(na_count))

##############################################################################
# Imputation
##############################################################################
# impute Autocheck_score
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df <- df %>%
  group_by(CarMake,CarYear,SalePrice) %>%
  mutate(Autocheck_score = ifelse(is.na(Autocheck_score), Mode(Autocheck_score),Autocheck_score))


df <- df %>%
  group_by(Mileage,CarYear,SalePrice) %>%
  mutate(Autocheck_score = ifelse(is.na(Autocheck_score), Mode(Autocheck_score),Autocheck_score))

df <- df %>%
  group_by(CarYear,SalePrice) %>%
  mutate(Autocheck_score = ifelse(is.na(Autocheck_score), Mode(Autocheck_score),Autocheck_score))

df <- df %>%
  group_by(CarMake,SalePrice) %>%
  mutate(Autocheck_score = ifelse(is.na(Autocheck_score), Mode(Autocheck_score),Autocheck_score))

df$Autocheck_score <- ifelse(is.na(df$Autocheck_score), median(df$Autocheck_score,na.rm = TRUE),df$Autocheck_score)

df[df==""] <- NA

# clean ConditionReport column:
# 0 -> 10
# A -> 19
# SL -> 17
# PR -> 23
# RG -> 24
# AV -> 34
# CL -> 48
# EC -> 50
# Blank -> 29
# imputation by t.test. If p >0.05, then imputation is OK
CG <- df[df$ConditionReport == "SL", ]
TG <- df[df$ConditionReport == "17", ]
var.test(CG$SalePrice, TG$SalePrice)
t.test(CG$SalePrice, TG$SalePrice, var.equal = TRUE)


df$ConditionReport <- ifelse(df$ConditionReport=='A',20,df$ConditionReport)
df$ConditionReport <- ifelse(df$ConditionReport=='A3',20,df$ConditionReport)
df$ConditionReport <- ifelse(df$ConditionReport=='SL',16,df$ConditionReport)
df$ConditionReport <- ifelse(df$ConditionReport=='PR',18,df$ConditionReport)
df$ConditionReport <- ifelse(df$ConditionReport=='RG',18,df$ConditionReport)
df$ConditionReport <- ifelse(df$ConditionReport=='AV',25,df$ConditionReport)
df$ConditionReport <- ifelse(df$ConditionReport=='CL',44,df$ConditionReport)
df$ConditionReport <- ifelse(df$ConditionReport=='EC',50,df$ConditionReport)
df$ConditionReport <- ifelse(df$ConditionReport=='Y6',50,df$ConditionReport)
df$ConditionReport <- ifelse(is.na(df$ConditionReport),29,df$ConditionReport)

# convert char to int
df$ConditionReport <- as.numeric(df$ConditionReport)

# impute JDPowersCat
df <- df %>%
  group_by(CarMake,CarYear,SalePrice) %>%
  mutate(JDPowersCat = ifelse(JDPowersCat=="",Mode(JDPowersCat,na.rm = TRUE),JDPowersCat))

df$JDPowersCat <- ifelse(is.na(df$JDPowersCat),"UNDEFINED",df$JDPowersCat)

##############################################################################
# feature engineering
##############################################################################

# create Age variable
df$Age <- 2015 - df$CarYear

# create number of buyers per location
df <- df %>%
  group_by(BuyerID,SellingLocation) %>%
  mutate(BuyerPerLoc = n())

# create RANK by (CarMake, CarYear, SellingLocation,SaleDate)
df <- df %>%
  group_by(CarMake, CarYear, JDPowersCat,SellingLocation,SaleDate) %>%
  mutate(Rank = rank(SalePrice))

# create number of buyers per date
df <- df %>%
  group_by(BuyerID,SaleDate) %>%
  mutate(NumBuyerDate = n())

# create number of Locations per date
df <- df %>%
  group_by(SellingLocation,SaleDate) %>%
  mutate(NumLocationDate = n())

# create number of buyers per date and location
df <- df %>%
  group_by(SellingLocation,SaleDate,BuyerID) %>%
  mutate(NumBuyerLocDate = n())

# create CompetativeSize by (CarMake, CarYear,SaleDate)
df <- df %>%
  group_by(CarMake, CarYear,SaleDate,JDPowersCat) %>%
  mutate(CompetativeSize = n())


# create GlobalRank
df$GlobalRank <- df$Rank/df$CompetativeSize


# create AdjustedSalePrice by (CarMake, SellingLocation,SaleDate)
df <- df %>%
  group_by(CarMake, JDPowersCat,SellingLocation,SaleDate) %>%
  mutate(AdjustedSalePrice = predict(lm(SalePrice~1)) - predict(lm(SalePrice~Mileage)))

# create EffectivePrice
df$EffectivePrice <- df$SalePrice + df$AdjustedSalePrice


# create number of distinct seller per buyer
df <- df %>%
  group_by(BuyerID) %>%
  mutate(NumDistSeller = length(SellerID))

# create number of visit per buyer per date
df <- df %>%
  group_by(BuyerID,SaleDate) %>%
  mutate(NumVisitDate = length(SellingLocation))

# create number of visit per buyer in general
df <- df %>%
  group_by(BuyerID) %>%
  mutate(NumVisit = length(SellingLocation))


# create number of returned per buyerID per month
df <- df %>%
  group_by(BuyerID,month(SaleDate)) %>%
  mutate(NumReturendMonth = sum(Returned,na.rm = TRUE))

# create the number of returned per location
df <- df %>%
  group_by(SellingLocation) %>%
  mutate(NumReturendLoc = sum(Returned, na.rm = TRUE))


# create the AVG of SalePrice per buyer per month
df <- df %>%
  group_by(BuyerID,month(SaleDate)) %>%
  mutate(AVGPriceBuyerMonth = mean(SalePrice))

# create the number of purchase per buyer per month
df <- df %>%
  group_by(BuyerID,month(SaleDate)) %>%
  mutate(NumPurchaseBuyerMonth = n())

# create the number of purchase per buyer
df <- df %>%
  group_by(BuyerID) %>%
  mutate(NumPurchaseBuyer = length(SaleDate))


# create number of days between purchase
df <- df %>%
  mutate(date = ymd(SaleDate)) %>% # convert to Datetime class
  group_by(BuyerID) %>% #group by USERID
  mutate(NumDaysBWBuys = as.integer(difftime(date, # take the difference
                                            lag(date, default = first(date)), unit = 'day')))

# create day of week
df$DayOfWeek <- weekdays(as.Date(df$date))



# sanity check for LIGHTR, LIGHTG and LIGHTY
df$LIGHTG <- ifelse(df$LIGHTG==1 & df$LIGHTY==1 & df$LIGHTR==1,0,df$LIGHTG)
df$LIGHTY <- ifelse(df$LIGHTY==1 & df$LIGHTR==1,0,df$LIGHTY)
df$LIGHTG <- ifelse(df$LIGHTG==1 & df$LIGHTR==1,0,df$LIGHTG)
df$LIGHTG <- ifelse(df$LIGHTG==1 & df$LIGHTY==1,0,df$LIGHTG)
df$LIGHTR <- ifelse(df$LIGHTG==0 & df$LIGHTY==0 & df$LIGHTR==0,1,df$LIGHTR)

# force PSIEligible to be correctly encoded
df$PSIEligible <- ifelse(df$Mileage < 125000 & df$SalePrice > 3000 & df$SalePrice < 40000 & df$LIGHTR == 0, 1,0)


##############################################################################
# create pctReturned by 10 cut-bin in NumPurchaseBuyer
##############################################################################
df <- df %>% 
  mutate(NumPurchase_10 = plyr::round_any(NumPurchaseBuyer,10,floor)) %>% 
  group_by(NumPurchase_10) %>% 
  mutate(pctReturned = mean(Returned,na.rm = TRUE))

##############################################################################
# SalePrice Transformation
##############################################################################

# Polynomial regression transformation
poly_fit <- lm(pctReturned~poly(NumPurchaseBuyer,3), data = df)
poly_pred <- predict(poly_fit, newdata = list(NumPurchaseBuyer=df$NumPurchaseBuyer), SE = FALSE) 


# Cubic Spline transformation. Knots are computed by quartiles
cubic_fit_q <- lm(pctReturned~bs(NumPurchaseBuyer, knots = c(quantile(df$NumPurchaseBuyer,0.25),
                                                  quantile(df$NumPurchaseBuyer,0.5),
                                                  quantile(df$NumPurchaseBuyer,0.75))), data = df)
cubic_pred_q <- predict(cubic_fit_q, newdata = list(NumPurchaseBuyer=df$NumPurchaseBuyer), SE = FALSE)


# Local regression transformation
loess_fit <- loess(pctReturned~NumPurchaseBuyer, data = df, span = 0.2, degree=1)
loess_pred <- predict(loess_fit, newdata = data.frame(NumPurchaseBuyer=df$NumPurchaseBuyer), SE = FALSE)


# GAM
gam_fit <- gam(pctReturned~s(NumPurchaseBuyer,3), data = df)
gam_pred <- predict(gam_fit, newdata = list(NumPurchaseBuyer=df$NumPurchaseBuyer), SE = FALSE) 


# square root
sqrt_fit <- lm(pctReturned~sqrt(NumPurchaseBuyer), data = df)
sqrt_pred <- predict(sqrt_fit, newdata = list(NumPurchaseBuyer=df$NumPurchaseBuyer), SE = FALSE)


# log
log_fit <- lm(pctReturned~log(NumPurchaseBuyer), data = df)
log_pred <- predict(log_fit, newdata = list(NumPurchaseBuyer=df$NumPurchaseBuyer), SE = FALSE) 


# squared
sq_fit <- lm(pctReturned~poly(NumPurchaseBuyer,2), data = df)
sq_pred <- predict(sq_fit, newdata = list(NumPurchaseBuyer=df$NumPurchaseBuyer), SE = FALSE) 

# z-score NumPurchaseBuyer
df$NumPurchaseBuyer_Z <- (df$NumPurchaseBuyer-mean(df$NumPurchaseBuyer))/sd(df$NumPurchaseBuyer)
zprice_fit <- lm(pctReturned~NumPurchaseBuyerZ, data = df)
zprice_pred <- predict(zprice_fit, newdata = list(NumPurchaseBuyerZ=df$NumPurchaseBuyerZ), SE = FALSE) 


# reparametrize single NumPurchaseBuyer by some transformation
df$NumPurchaseBuyer_Z            = zprice_pred
df$NumPurchaseBuyer_P3           = poly_pred
df$NumPurchaseBuyer_SPQ          = cubic_pred_q
df$NumPurchaseBuyer_LOCAL        = loess_pred
df$NumPurchaseBuyer_GAM          = gam_pred
df$NumPurchaseBuyer_SQRT         = sqrt_pred
df$NumPurchaseBuyer_SQ           = sq_pred
df$NumPurchaseBuyer_LOG          = log_pred

# create dummy variables for categorical variables
df_dum <- df
dmy <- dummyVars("~.",data = df_dum[,c('DayOfWeek','SellingLocation','CarMake','JDPowersCat')], fullRank = F)
df_dum <- data.frame(predict(dmy,newdata = df_dum[,c('DayOfWeek','SellingLocation','CarMake','JDPowersCat')]))
df <- data.frame(df,df_dum)

# drop unwanted columns
drops <- c('pctReturned','NumPurchase_10','DayOfWeek','date','Rank','SaleDate','JDPowersCat',
           'CarYear','CarMake','SellingLocation','VIN','SellerID','month.SaleDate.')
df <- df[,!names(df) %in% drops]


# write to CSV
write.csv(df,'./caseCL.csv')


df <- df[complete.cases(df$Returned),]

# Create Risky variable
df <- df %>%
  group_by(BuyerID) %>%
  mutate(Risky = ifelse((sum(Returned)/n())>0.1,1,0))

write.csv(df,'./caseML.csv')




