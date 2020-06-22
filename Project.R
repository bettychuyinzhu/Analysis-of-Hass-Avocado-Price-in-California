#---------------------------------------------------------------------------------------
# Final Project
#---------------------------------------------------------------------------------------

# Data source:https://www.kaggle.com/neuromusic/avocado-prices

library(lubridate)            # as.POSIXct() & strftime() to extract "Month" from "Date"
library(ggplot2)              # ggplot() to create boxplots, scatterplots, density plots

#---------------------------------------------------------------------------------------
# Clean up Three Datasets, Combine Three Datasets Into One Combined Dataset
#---------------------------------------------------------------------------------------
# Conduct clean up of the first dataset (from 2015 to 2017) from Kaggle.com

avocado_raw<-read.csv("/Users/chuyinzhu/Desktop/R Language/Project/avocado_raw.csv")
dim(avocado_raw)                      # [1] 18249    14

# Since this dataset does not have complete records of 2018, only records of 
# 2015, 2016, and 2017 are extracted. Also, Column "X" (row index) is deleted. 
# Last, only California and its cities are kept in this dataset.
avocado_set1<-subset(avocado_raw,
                     select=-X,            
                     avocado_raw$region %in% c("California", "LosAngeles","Sacramento",
                                               "SanFrancisco","SanDiego")
                     & avocado_raw$year!=2018)

# After delettion, there are 1570 rows and 13 variables.
dim(avocado_set1)                          # [1] 1570   13      
names(avocado_set1)
# [1] "Date"         "AveragePrice" "Total.Volume" "X4046"        "X4225"        "X4770"       
# [7] "Total.Bags"   "Small.Bags"   "Large.Bags"   "XLarge.Bags"  "type"         "year"        
# [13] "region"  

# Change the names and orders of the columns for convenience of data merging later.
names(avocado_set1)<-c("Date","AveragePrice",'TotalVolume',"X4046","X4225","X4770",
                       "TotalBags","SmallBags","LargeBags","XLargeBags","Type","Year",
                       "Region")

col_order<-c("Region","Date","Type","AveragePrice",'TotalVolume',"X4046","X4225",
             "X4770","TotalBags","SmallBags","LargeBags","XLargeBags","Year")
avocado_set1<-avocado_set1[,col_order]

# Change the date format of the "Date" column for convenience of data munging later.
avocado_set1$Date<-as.Date(avocado_set1$Date)

#---------------------------------------------------------------------------------------
# Conduct clean up of the second dataset (2018) from www.hassavocadoboard.com, where 
# the original data source is.

avocado_set2<-read.csv("/Users/chuyinzhu/Desktop/R Language/Project/2018.csv")

# Since this dataset has a format that is slightly different from the first dataset, an
# extra column "Timeframe" is deleted. Also, only California and its cities are kept.
levels(avocado_set2$Geography)
avocado_set2<-subset(avocado_set2,
                     select=-2,
                     avocado_set2$Geography %in% c("California", "Los Angeles",
                                                   "Sacramento","San Francisco","San Diego"))

# After delettion, there are 490 rows and 13 variables. All records are 2018.
avocado_set2$Year=2018
dim(avocado_set2)                          # [1] 490   13   

# Change the names and orders of the columns for convenience of data merging later.
names(avocado_set2)
# [1] "Geography"                 "Current.Year.Week.Ending"  "Type"                     
# [4] "ASP.Current.Year"          "Total.Bulk.and.Bags.Units" "X4046.Units"              
# [7] "X4225.Units"               "X4770.Units"               "TotalBagged.Units"        
# [10] "SmlBagged.Units"           "LrgBagged.Units"           "X.LrgBagged.Units" 
# [13] "year"
names(avocado_set2)<-c("Region","Date","Type","AveragePrice",'TotalVolume',"X4046","X4225",
                       "X4770","TotalBags","SmallBags","LargeBags","XLargeBags","Year")

# Change the date format of the "Date" column for convenience of data munging later.
avocado_set2$Date<-as.Date(avocado_set2$Date)

#---------------------------------------------------------------------------------------
# Conduct clean up of the second dataset (2019) from www.hassavocadoboard.com, where 
# the original data source is.
avocado_set3<-read.csv("/Users/chuyinzhu/Desktop/R Language/Project/2019.csv")

# Since this dataset has a format that is slightly different from the first dataset, an
# extra column "Timeframe" is deleted. Also, only California and its cities are kept.
levels(avocado_set3$Geography)
avocado_set3<-subset(avocado_set3,
                     select=-2,
                     avocado_set3$Geography %in% c("California", "Los Angeles",
                                                   "Sacramento","San Francisco","San Diego"))

# After delettion, there are 520 rows and 13 variables. All records are 2019.
avocado_set3$Year=2019
dim(avocado_set3)                          # [1] 520   13   

# Change the names and orders of the columns for convenience of data merging later.
names(avocado_set3)
# [1] "Geography"                 "Current.Year.Week.Ending"  "Type"                     
# [4] "ASP.Current.Year"          "Total.Bulk.and.Bags.Units" "X4046.Units"              
# [7] "X4225.Units"               "X4770.Units"               "TotalBagged.Units"        
# [10] "SmlBagged.Units"           "LrgBagged.Units"           "X.LrgBagged.Units" 
# [13] "year"
names(avocado_set3)<-c("Region","Date","Type","AveragePrice",'TotalVolume',"X4046","X4225",
                       "X4770","TotalBags","SmallBags","LargeBags","XLargeBags","Year")

# Change the date format of the "Date" column for convenience of data munging later.
avocado_set3$Date<-as.Date(avocado_set3$Date)

#---------------------------------------------------------------------------------------
# Stack all three datasets (from 2015-2019) into one, and save it.
avocado<-rbind(avocado_set1,avocado_set2,avocado_set3)
dim(avocado)                              # [1] 2580   13
write.csv(avocado,"/Users/chuyinzhu/Desktop/R Language/Project/avocado.csv",
          row.names=FALSE)


#---------------------------------------------------------------------------------------
# Read the Combined Dataset into R and Conduct Further Clean Up
#---------------------------------------------------------------------------------------

avocado<-read.csv("/Users/chuyinzhu/Desktop/R Language/Project/avocado.csv")

# Overview of the combined dataset
summary(avocado)

# Region            Date                 Type      AveragePrice    TotalVolume      
# California  :516   2015-01-04:  10   conventional :785   Min.   :0.530   Min.   :    3563  
# Sacramento  :516   2015-01-11:  10   Conventional :305   1st Qu.:1.137   1st Qu.:   26033  
# LosAngeles  :314   2015-01-18:  10   Conventional :200   Median :1.480   Median :  262428  
# SanDiego    :314   2015-01-25:  10   organic      :785   Mean   :1.513   Mean   : 1117345  
# SanFrancisco:314   2015-02-01:  10   Organic      :505   3rd Qu.:1.810   3rd Qu.:  816446  
# Los Angeles :202   2015-02-08:  10                       Max.   :3.250   Max.   :11324683  
# (Other)     :404   (Other)   :2520                                                         

# X4046             X4225             X4770            TotalBags         SmallBags      
# Min.   :    264   Min.   :    316   Min.   :     0.0   Min.   :      0   Min.   :      0  
# 1st Qu.:   8448   1st Qu.:  10889   1st Qu.:     0.0   1st Qu.:   7642   1st Qu.:   7521  
# Median :  78998   Median :  92472   Median :   259.6   Median :  66300   Median :  62696  
# Mean   : 396824   Mean   : 348788   Mean   : 30374.8   Mean   : 341358   Mean   : 298122  
# 3rd Qu.: 225224   3rd Qu.: 396038   3rd Qu.: 22522.8   3rd Qu.: 199784   3rd Qu.: 164743  
# Max.   :4794142   Max.   :4097592   Max.   :424389.6   Max.   :4324167   Max.   :4017035  

# LargeBags           XLargeBags          Year     
# Min.   :      0.0   Min.   :     0   Min.   :2015  
# 1st Qu.:      5.9   1st Qu.:     0   1st Qu.:2016  
# Median :    720.0   Median :     0   Median :2017  
# Mean   :  29789.4   Mean   : 13446   Mean   :2017  
# 3rd Qu.:  14627.3   3rd Qu.:  3272   3rd Qu.:2018  
# Max.   :1549350.4   Max.   :479699   Max.   :2019  

#---------------------------------------------------------------------------------------
# Conduct more data clean up

levels(avocado$Region)
# [1] "California"    "Los Angeles"   "LosAngeles"    "Sacramento"    "San Diego"     
# [6] "San Francisco"  "SanDiego"      "SanFrancisco" 

levels(avocado$Type)
# [1] "conventional"  "Conventional"  "Conventional " "organic"   "Organic"  

avocado$Region[avocado$Region=="LosAngeles"]<-"Los Angeles"
avocado$Region[avocado$Region=="SanDiego"]<-"San Diego"
avocado$Region[avocado$Region=="SanFrancisco"]<-"San Francisco"
avocado$Type[avocado$Type=="Conventional"]<-"conventional"
avocado$Type[avocado$Type=="Conventional "]<-"conventional"
avocado$Type[avocado$Type=="Organic"]<-"organic"

# Add a "Month" Column base on the "Date" column
avocado$Month<-as.POSIXct(avocado$Date)
avocado$Month<-strftime(avocado$Month,"%m")


write.csv(avocado,"/Users/chuyinzhu/Desktop/R Language/Project/avocado_df.csv",
          row.names=FALSE)


#---------------------------------------------------------------------------------------
# Test Multicollinearity of the Volume-Related Variables
#---------------------------------------------------------------------------------------

df<-read.csv("/Users/chuyinzhu/Desktop/R Language/Project/avocado_df.csv")

# Since each single volume record is large, so I create new variables using the log (10)
# transformation

# Create an exploratory data frame (df2) 
df2<-df

# Log the volume columes, then run a correlation analysis to test multicollinearity
df2$logTotalVolume<-log(df2$TotalVolume)
df2$logX4046<-log(df2$X4046)
df2$logX4225<-log(df2$X4225)
df2$logX4770<-log(df2$X4770); 
df2$logTotalBags<-log(df2$TotalBags)
df2$logSMBags<-log(df2$SmallBags)
df2$logLBags<-log(df2$LargeBags)
df2$logXLBags<-log(df2$XLargeBags)

corr<-round(cor(df2[,c(15:22)]),2); corr
#                logTotalVolume logX4046 logX4225 logX4770 logTotalBags logSMBags logLBags logXLBags
# logTotalVolume           1.00     0.97     0.96      NaN          NaN       NaN      NaN       NaN
# logX4046                 0.97     1.00     0.93      NaN          NaN       NaN      NaN       NaN
# logX4225                 0.96     0.93     1.00      NaN          NaN       NaN      NaN       NaN
# logX4770                  NaN      NaN      NaN        1          NaN       NaN      NaN       NaN
# logTotalBags              NaN      NaN      NaN      NaN            1       NaN      NaN       NaN
# logSMBags                 NaN      NaN      NaN      NaN          NaN         1      NaN       NaN
# logLBags                  NaN      NaN      NaN      NaN          NaN       NaN        1       NaN
# logXLBags                 NaN      NaN      NaN      NaN          NaN       NaN      NaN         1

# Some volumes, after log, are too small and shown as "-Inf" as results. Thus, there are
# NaNs in the analysis.

#---------------------------------------------------------------------------------------
# Replace "-Inf" with "0", re-run the correlation analysis to test multicollinearity

df2$logX4770[!is.finite(df2$logX4770)]<-0
df2$logTotalBags[!is.finite(df2$logTotalBags)]<-0
df2$logSMBags[!is.finite(df2$logSMBags)]<-0
df2$logLBags[!is.finite(df2$logLBags)]<-0
df2$logXLBags[!is.finite(df2$logXLBags)]<-0

corr<-round(cor(df2[,c(15:22)]),2); corr
#                logTotalVolume logX4046 logX4225 logX4770 logTotalBags logSMBags logLBags logXLBags
# logTotalVolume           1.00     0.97     0.96     0.91         0.90      0.90     0.79      0.84
# logX4046                 0.97     1.00     0.93     0.91         0.85      0.85     0.73      0.84
# logX4225                 0.96     0.93     1.00     0.89         0.82      0.82     0.72      0.81
# logX4770                 0.91     0.91     0.89     1.00         0.77      0.77     0.73      0.85
# logTotalBags             0.90     0.85     0.82     0.77         1.00      1.00     0.80      0.74
# logSMBags                0.90     0.85     0.82     0.77         1.00      1.00     0.77      0.73
# logLBags                 0.79     0.73     0.72     0.73         0.80      0.77     1.00      0.73
# logXLBags                0.84     0.84     0.81     0.85         0.74      0.73     0.73      1.00

# Potential predictors on averag unit price are highly correlated, indicating
# multicollinearity. Therefore, only "logTotalVolume" is kept

# Add the new predictor "logTotalVolume" to the data frame (df)
df$logTotalVolume<-log(df$TotalVolume)


#---------------------------------------------------------------------------------------
# Create "Season" Variable based on Month
#---------------------------------------------------------------------------------------

season=function(x){
  if(x %in% 3:5) return("Spring")
  if(x %in% 6:8) return("Summer")
  if(x %in% 9:11) return("Fall")
  if(x %in% c(12,1,2)) return("Winter")
}

df$Season=sapply(df$Month,season)

#---------------------------------------------------------------------------------------
# Data Summary
#---------------------------------------------------------------------------------------
# Create new dataframe "df_all" for EDA and analysis
df_all<-df[,c(1,3,4,13,15,16)]

# Overview of the "California" data
summary(subset(df_all,df_all$Region=="California"))

#           Region              Type      AveragePrice        Year      logTotalVolume              
# California   :516   conventional:258   Min.   :0.670   Min.   :2015   Min.   :11.16        
# Los Angeles  :  0   organic     :258   1st Qu.:1.117   1st Qu.:2016   1st Qu.:12.11   
# Sacramento   :  0                      Median :1.445   Median :2017   Median :13.75   
# San Diego    :  0                      Mean   :1.440   Mean   :2017   Mean   :13.81                     
# San Francisco:  0                      3rd Qu.:1.710   3rd Qu.:2018   3rd Qu.:15.62                     
#                                        Max.   :2.580   Max.   :2019   Max.   :16.24                     
#     Season
# Length:516  
# Class :character  
# Mode  :character  
#
#
#

#---------------------------------------------------------------------------------------
# Overview of the LA, Sac, SD, SF data
summary(subset(df_all,df_all$Region!="California"))

#           Region              Type       AveragePrice        Year      logTotalVolume            
# California   :  0   conventional:1032   Min.   :0.530   Min.   :2015   Min.   : 8.178         
# Los Angeles  :516   organic     :1032   1st Qu.:1.140   1st Qu.:2016   1st Qu.: 9.903   
# Sacramento   :516                       Median :1.490   Median :2017   Median :12.137   
# San Diego    :516                       Mean   :1.531   Mean   :2017   Mean   :11.858                     
# San Francisco:516                       3rd Qu.:1.850   3rd Qu.:2018   3rd Qu.:13.375                     
#                                         Max.   :3.250   Max.   :2019   Max.   :15.549   
#     Season
# Length:2064  
# Class :character  
# Mode  :character  
#
#
#

#---------------------------------------------------------------------------------------
# Exploratory Data Analysis
#---------------------------------------------------------------------------------------
# First, explore the relationship between volumes sold and average unit price

# Create a scatterplot for "log(TotalVolume)" and "Average Unit Price"
ggplot(df_all, aes(x=logTotalVolume,y=AveragePrice))+
  geom_point(size=0.5)+
  labs(title="Sales Growth vs. Average Unit Price",
       x="Sales Growth",y="Average Unit Price (US $)")  

# Create scatterplots (by region and city)
ggplot(df_all, aes(x=logTotalVolume,y=AveragePrice, color=Region))+
  geom_point(size=0.5)+facet_wrap(~Region)+
  labs(title="Sales Growth vs. Average Unit Price by Region/City",
       x="Sales Growth",y="Average Unit Price (US $)")   

#---------------------------------------------------------------------------------------
# Second, explore the relationship between avocado types and average unit price

# Create price density plotd by region/cities and by type
ggplot(subset(df_all, Type=="conventional"), aes(x=AveragePrice))+
  geom_density(aes(fill=Region), color=NA, alpha=0.35)+
  labs(title="Price Density Plot (Conventional Avocados)",
       x="Average Unit Price (US $)")    
   
ggplot(subset(df_all, Type=="organic"), aes(x=AveragePrice))+
  geom_density(aes(fill=Region), color=NA, alpha=0.35)+
  labs(title="Price Density Plot (Organic Avocados)",
       x="Average Unit Price (US $)")    

#---------------------------------------------------------------------------------------
# Third, explore the relationship between seasones and average unit price

# Create boxplots to show monthly price trend by region/cities and by type
ggplot(subset(df_all, Type=="conventional"), 
       aes(x=factor(Season,levels=c("Spring", "Summer","Fall","Winter")), 
           y=AveragePrice))+
  geom_boxplot()+
  facet_wrap(~Region, nrow=1)+
  labs(title="Seasonal Trend of Average Unit Price (Conventional Avocados)",
       x="Season", y="Average Unit Price (US $)")

ggplot(subset(df_all, Type=="organic"), 
       aes(x=factor(Season,levels=c("Spring", "Summer","Fall","Winter")), 
           y=AveragePrice))+
  geom_boxplot()+
  facet_wrap(~Region, nrow=1)+
  labs(title="Seasonal Trend of Average Unit Price (Organic Avocados)",
       x="Season", y="Average Unit Price (US $)")


#---------------------------------------------------------------------------------------
# Supervised Machine Learning Algorithm (Part 1/2)
#---------------------------------------------------------------------------------------
# Multiple Linear Regression (MLR) for the dataset--"df_Cal" to explore if (sold) volume
# growth, types (of avocadoes sold), cities (where the avocados were sold), and seasons 
# have any (statisticially) significant impact on Average Unit Price.

# Subset from the data frame "df_all"
df_Cal<-subset(df_all,Region!="California")

# Split the data set into training set and test set
n<-nrow(df_Cal)                             # n=2064
n_train<-round(n*0.8)                       # n_train=1651, use 80% data as training set


set.seed(100)
train_i<-sample(n,n_train,replace=FALSE)    # Create an index

trainCal<-df_Cal[train_i,]                  # Create training set
testCal<-df_Cal[-train_i,]                  # Create test set

#---------------------------------------------------------------------------------------
# Train linear model to predict average unit price of overall California. 

lm_Cal<-lm(AveragePrice~logTotalVolume+Type+Region+Season, data=df_Cal)

summary(lm_Cal)

# Call:
# lm(formula = AveragePrice ~ logTotalVolume + Type + Region + Season, data = df_Cal)
#
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.76933 -0.18273 -0.01598  0.17473  0.94830 
#
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          6.45343    0.27670  23.323  < 2e-16 ***
# logTotalVolume      -0.36224    0.01868 -19.387  < 2e-16 ***
# Typeorganic         -0.67180    0.06837  -9.826  < 2e-16 ***
# RegionSacramento    -0.43426    0.04141 -10.488  < 2e-16 ***
# RegionSan Diego     -0.45726    0.03562 -12.835  < 2e-16 ***
# RegionSan Francisco  0.04218    0.02789   1.513 0.130536    
# SeasonSpring        -0.13349    0.01654  -8.072 1.16e-15 ***
# SeasonSummer         0.05453    0.01637   3.330 0.000883 ***
# SeasonWinter        -0.24230    0.01657 -14.623  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.261 on 2055 degrees of freedom
# Multiple R-squared:  0.704,	Adjusted R-squared:  0.7029 
# F-statistic:   611 on 8 and 2055 DF,  p-value: < 2.2e-16

#---------------------------------------------------------------------------------------
# Display linear regression series of 4 diagnostic plots

par(mfrow=c(2,2))
plot(lm_Cal)

par(mfrow=c(1,1))

#---------------------------------------------------------------------------------------
# Use the trained model (lm_Cal) to predict the output of test set
predict_Cal<-predict(lm_Cal, newdata=testCal)

# Check the correlation between predict values vs. test set values
cor(predict_Cal, testCal$AveragePrice)
# [1] 0.8388951                       # Very high correlation

# Plot predicted vs. actual in test set
par(mfrow=c(1,1))
plot(testCal$AveragePrice, predict_Cal,
     xlab="Actual Average Unit Price", ylab="Predicted Average Unit Price",
     main="Predicted vs. Actual Average Unit Price in Test Set (California)")
abline(a=0,b=1)


#---------------------------------------------------------------------------------------
# Supervised Machine Learning Algorithm (Part 2/2)
#---------------------------------------------------------------------------------------
# Multiple Linear Regression (MLR) for the dataset--"df_LA" to explore if (sold) volume
# growth, types (of avocadoes sold), and seasons have any (statisticially) significant 
# impact on Average Unit Price in Los Angeles.

# Subset from the data frame "df_all"
df_LA<-subset(df_all,Region=="Los Angeles")

# Split the data set into training set and test set
m<-nrow(df_LA)                              # m=516
m_train<-round(m*0.8)                       # m_train=413, use 80% data as training set


set.seed(200)
train_k<-sample(m,m_train,replace=FALSE)    # Create an index

trainLA<-df_LA[train_k,]                  # Create training set
testLA<-df_LA[-train_k,]                  # Create test set

#---------------------------------------------------------------------------------------
# Train linear model to predict average unit price in LA. 
lm_LA<-lm(AveragePrice~logTotalVolume+Type+Season, data=df_LA)

summary(lm_LA)

# Call:
# lm(formula = AveragePrice ~ logTotalVolume + Type + Season, data = df_LA)
#
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.47791 -0.18431 -0.04517  0.18696  0.65907 
#
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     3.96103    0.57198   6.925 1.32e-11 ***
# logTotalVolume -0.18802    0.03878  -4.849 1.65e-06 ***
# Typeorganic    -0.14739    0.14020  -1.051    0.294    
# SeasonSpring   -0.21886    0.03286  -6.661 7.08e-11 ***
# SeasonSummer   -0.02422    0.03246  -0.746    0.456    
# SeasonWinter   -0.28161    0.03268  -8.617  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.2557 on 510 degrees of freedom
# Multiple R-squared:  0.5788,	Adjusted R-squared:  0.5746 
# F-statistic: 140.1 on 5 and 510 DF,  p-value: < 2.2e-16

#---------------------------------------------------------------------------------------
# Display linear regression series of 4 diagnostic plots

par(mfrow=c(2,2))
plot(lm_LA)

par(mfrow=c(1,1))

#---------------------------------------------------------------------------------------
# Use the trained model (lm_LA) to predict the output of test set
predict_LA<-predict(lm_LA, newdata=testLA)

# Check the correlation between predict values vs. test set values
cor(predict_LA, testLA$AveragePrice)
# [1] 0.7532892                       # Very high correlation

# Plot predicted vs. actual in test set
par(mfrow=c(1,1))
plot(testLA$AveragePrice, predict_LA,
     xlab="Actual Average Unit Price", ylab="Predicted Average Unit Price",
     main="Predicted vs. Actual Average Unit Price in Test Set (LA)")
abline(a=0,b=1)

#---------------------------------------------------------------------------------------