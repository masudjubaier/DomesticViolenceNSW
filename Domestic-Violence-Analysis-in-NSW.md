Domestic Violence Analysis in NSW
================

## GitHub Documents

``` r
library(dplyr)
library(arm)
library(ggplot2)
library(scales)
library(reshape2)
library(sqldf)
library(ggmap)
library(caret)
library(glmnet)
library(lme4)
library(jtools)
library(ggstance)
library(kableExtra)
library(knitr)
library(dplyr)
```

``` r
#Loading data into datafeame
DV_NSW <- read.csv("data/DV_NSW_by_LGA.csv")

DV_Labels <- read.csv("data/labels.csv")

NSW_LGA <- read.csv("data/NSW_LGA.csv")
```

# Introduction

   Domestic violence (DV) is dark stain in modern society. DV can be
defined as any form of mistreatment including physical, phycological,
financial, sexual take place within domestic circumstances perpetrated
by member of intimate relationship. Any member of household can be
victim of DV, while women and children are most vulnerable to this
crime. The World Health Origination(WHO) has described the violence
against women is a global problem of public health which need immediate
attention\[1\]. Australia is not Immune to this crime, especially NSW
suffer with this crime heavily. Hance it is very important the
understand the nature of this crime and identify the factors having most
influence. In this project we shall try find these factors and try to
prove following hypothesis by implementing a linear model.

**Null Hypothesis (*H*<sub>0</sub>):** There is no relationship between
income, education and employment with DV.  
**Alternative Hypothesis (*H*<sub>*A*</sub>):** There is strong
correlation between income, education and employment with DV.

# Data and methodology

   The dataset contains 3 CSV file. The 1<sup>*s**t*</sup> CSV file
“DV\_NSW\_by\_LGA.csv” contains the count of DV crime in NSW LGA from
Jan-99 to Dec-15. The 2<sup>*n**d*</sup> CSV file “NSW\_LGA.csv” contain
2011 census data of NSW LGA. In the 2<sup>*n**d*</sup> CSV file there
are around 8K features on 2011 NSW LGA census data. The features are in
encoded form, where the description of the features is found in the
3<sup>*r**d*</sup> CSV file “labels.csv”. At first we have performed
some exploratory analysis to understand the dataset. We have generated
some visualization to show the trend of the crime. From the
visualization it is clear the trend is upward. To identify the features
contributes most towards the crime we have implemented Forward stepwise
Feature selection.

   **Forward stepwise Feature selection** is a computationally efficient
algorithm to select the best features that produces best outcome. The
algorithm starts with a null model and add predictors one by one until
all predictors are tried. At each step, the predictor that give the
largest improvement on the model is added to the final predictor list.
The steps of the algorithm are as follows.

1. Let, *M*<sub>0</sub> be a null model, which has no predictor.

2. For *k* = 0, .. , *p* − 1:

> (*a*) Consider all *p* − *k* models that supplement the predictors in
> *M*<sub>*k*</sub> with one added predictor.  
> (*b*) Chose the *best* between these *p* − *k* models, and name it
> *M*<sub>*k* + 1</sub>. Here *best* is considered as the model having
> minimum RSS or maximum *R*<sup>2</sup>.

3. Select the best model from *M*<sub>0</sub>, .., *M*<sub>*p*</sub>
using any one of these metric cross-validated prediction error or
*C*<sub>*p*</sub> (*A**I**C*) or (*B**I**C*) or adjusted
*R*<sup>2</sup>.

Using the features identified we shall build a linear model to predict
DV.

# Results and implications

   The plot below visualizes yearly frequencies of all across NSW. From
the plot it is clear the crime rate has an upward trend.

``` r
#
DV_NSW1 <- melt(DV_NSW, id.vars="LGA") %>%
  dplyr::rename(Month = variable,
                Count = value)

DV_NSW1$Month <- format(as.Date(paste("1-",gsub("\\.", "-", DV_NSW1$Month),sep=""), "%d-%b-%y"),"%b-%y")

DV_NSW1$Year <- format(as.Date(paste("1-",gsub("\\.", "-", DV_NSW1$Month),sep=""), "%d-%b-%y"),"%Y")

#Creating a dataframe with yearly total count.
DV_NSW2 <- sqldf('SELECT Year, SUM(Count) AS YearCount FROM DV_NSW1 GROUP BY Year')

p1 <- ggplot(data=DV_NSW2, aes(x=Year, y=YearCount)) +
  labs( y = "Frequency", title = "Frequency by Year") +
  geom_bar(stat="identity", aes(fill = YearCount)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, vjust = -.25, angle = 90, hjust = 1)) +
  theme(legend.position = "none")
p1
```

<img src="README_figs/README-Yearly Frequency-1.png" style="display: block; margin: auto;" />

   The plot below visualizes the frequency of top 10 LGA in descending
order. Blacktown LGA outcast all other LGA by very clear margin. The
concern authority might need to pay extra attention on Blacktown.

``` r
#Creating a dataframe with yearly count of top 10 LGA.
DV_NSW3 <- sqldf('SELECT LGA, SUM(Count) AS YearCount FROM DV_NSW1 GROUP BY LGA order by YearCount DESC limit 10')

p2 <- ggplot(data=DV_NSW3, aes(x = reorder(LGA, -YearCount), y = YearCount)) +
  labs(x = "LGA", y = "Year Count", title = "Top 10 LGA") +
  geom_bar(stat="identity", aes(fill = YearCount)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, vjust = -.25, angle = 90, hjust = 1)) +
  theme(legend.position = "none")

p2
```

<img src="README_figs/README-Top 10 LGA Frequency-1.png" style="display: block; margin: auto;" />

   The plot below visualizes the frequency timeseries of top 4 LGA. The
timeseries plot shows normal distribution of the frequency, no sudden
abrupt spike or drop of the frequency.

``` r
#DV_NSW1_Top <- dplyr::filter(DV_NSW1, LGA %in% c('Campbelltown', 'Blacktown', 'Penrith', 'Wyong', 'Liverpool', 'Fairfield', 'Parramatta', 'Newcastle', 'Lake Macquarie', 'Wollongong'))
#Creation dataframe with top 4 LGA data
DV_NSW4 <- subset(DV_NSW1, LGA == "Blacktown" | LGA == "Campbelltown" | LGA == "Penrith" | LGA == "Liverpool")


DV_NSW4$Month <- as.Date(paste("1-",DV_NSW4$Month,sep = ""),"%d-%b-%y")
DV_NSW4$Year <- as.numeric(DV_NSW4$Year)


p3 <- ggplot(data = DV_NSW4, aes(x = Month, y = Count,  color=LGA)) +
  geom_point(alpha = 0.01) + geom_line(aes(group = 1)) +
  facet_wrap(~LGA, ncol=1, scales = "free_x") +
  labs( title = "Top 4 LGA Time Series") +
  scale_y_continuous() +
  scale_x_date(labels = date_format("%b-%y"), breaks = date_breaks("5 year")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6)) +
  theme_bw()+
  theme_minimal() +
  theme(axis.text.x.bottom = element_text(angle = 90, vjust = 0.5, size = 6)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(legend.position = "none")

p3
```

<img src="README_figs/README-Top 4 LGA Timeseries-1.png" style="display: block; margin: auto;" />

``` r
# As the  ABS census data packs contains only data of 2011, as shall use only 2011 LGA DV data for model building purpose.
DV_NSW_2011 <- filter(DV_NSW1, Year == '2011')

#Creating dataframe with 2011 total count for each LGA
DV_NSW_2011_sum <- DV_NSW_2011 %>%
  group_by(LGA) %>%
  dplyr::summarise(DV_Count_2011 = sum(Count)) %>%
  arrange(LGA)
#Cleaning the LGA name by removing (C) and (A) form LGA name
NSW_LGA$label <- gsub(" \\(C)", "", NSW_LGA$label)
NSW_LGA$label <- gsub(" \\(A)", "", NSW_LGA$label)

#Joing the DV data with ABS census data
DV_NSW_2011_all <- DV_NSW_2011_sum %>%
  left_join(NSW_LGA %>%
              dplyr::rename(LGA = label)
              )

#Encoding the LGA name to numaric value
DV_NSW_2011_all$LGA <- as.numeric(as.factor(as.character(DV_NSW_2011_all$LGA)))

#Removing the columns region_id from from the dataframe
DV_NSW_2011_all <- DV_NSW_2011_all[-3]

#Scaling the data to standard form
for(i in 4:ncol(DV_NSW_2011_all)){
  current.f <- colnames(DV_NSW_2011_all)[i]
  DV_NSW_2011_all$current.f <- scale(DV_NSW_2011_all[,current.f])
}
```

  
   After applying the Forward stepwise Feature selection algorithm, the
features selected by the algorithm are listed in the table below. From
the feature list we can see, no features on income education and
employment as our hypothesis are selected by the algorithm. Therefore,
we are manually adding feature B115, B126, B5503 and B2847 in the
predictor list covering these aspects. With this combined predictor
list, we shall build a linear model.

``` r
selectFeature <- function(train, test, cls.train, cls.test, features) {
  ## identify a feature to be selected
  curr.min.mse <- Inf
  selected.i <- NULL
  for(i in 1:ncol(train)) {
    current.f <- colnames(train)[i]

    # Skip the features already selectd and the response variable DV_Count_2011
    if(current.f %in% c(features, "DV_Count_2011")) { next }
    model <- lm(data=train[,c(features, current.f, "DV_Count_2011")], DV_Count_2011 ~ .)
    #Checking the MSE with only the features already selected plus ith feature and DV_Count_2011
    test.mse <- mean((cls.test - predict.lm(model, test[,c(features, current.f, "DV_Count_2011")])) ^ 2)

    if(test.mse < curr.min.mse) {
        curr.min.mse <- test.mse
        selected.i <- colnames(train)[i]
    }
  }
  return(selected.i)
}


set.seed(1)
inTrain <- createDataPartition(DV_NSW_2011_all$DV_Count_2011, p = .6)[[1]]

# Split the training and test data
train <- DV_NSW_2011_all[ inTrain,]
test  <- DV_NSW_2011_all[-inTrain,]

# Select the first feature with min error
features <- c()
curr.min.mse <- Inf

# Select the first feature
for(i in 1:ncol(train)) {
  #DV_Count_2011 is response variable y, hance not considering as feature in the dataset
  if(colnames(train)[i] == "DV_Count_2011") { next }
  #Checking the MSE with only the ith feature and DV_Count_2011
  model <- lm(data=train[,c(colnames(train)[i], "DV_Count_2011")], DV_Count_2011 ~ .)
  # Calculate the MSE
  test.mse <- mean((test$DV_Count_2011 - predict.lm(model, test[, c(colnames(train)[i], "DV_Count_2011")])) ^ 2)
  #Finding the feature with minimum MSE in the dataset
  if(test.mse < curr.min.mse) {
    curr.min.mse <- test.mse
    features <- colnames(train)[i]
    }
  }

#The above loop returns the feature that has min MSE
# Now finding the next nine features
for (i in 1:9) {
  selected.i <- selectFeature(train, test, train$DV_Count_2011, test$DV_Count_2011, features)

  # add the best feature from current run
  features <- c(features, selected.i)
}

DV_Labels_Sel <- DV_Labels %>%
  dplyr::filter(Sequential %in% features) %>%
  dplyr::select(Sequential, Long)

#The features are given as a long string concatened with _. Replacing _ with space for beatter readabelity
DV_Labels_Sel$Long <- gsub("_", " ", DV_Labels_Sel$Long)

kable(DV_Labels_Sel) %>%
  kable_styling(full_width = F) %>%
  row_spec(0, bold = T) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "30em")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;font-weight: bold;">
Sequential
</th>
<th style="text-align:left;font-weight: bold;">
Long
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;border-right:1px solid;">
B1390
</td>
<td style="text-align:left;width: 30em; ">
Cambodia Year of arrival 2011
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border-right:1px solid;">
B1758
</td>
<td style="text-align:left;width: 30em; ">
Poland Year of arrival 2011
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border-right:1px solid;">
B2252
</td>
<td style="text-align:left;width: 30em; ">
Dependent children aged 5 9 years female parent Language and proficiency
in English not stated male parent Speaks English only
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border-right:1px solid;">
B2409
</td>
<td style="text-align:left;width: 30em; ">
Dependent children aged 15 17 years female parent Total male parent
Speaks other language and speaks English Proficiency in English not
stated
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border-right:1px solid;">
B2894
</td>
<td style="text-align:left;width: 30em; ">
Males Year 11 or equivalent Age 15 19 years
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border-right:1px solid;">
B3851
</td>
<td style="text-align:left;width: 30em; ">
Females 15 19 years Did unpaid domestic work 30 hours or more
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border-right:1px solid;">
B4258
</td>
<td style="text-align:left;width: 30em; ">
Persons 20 24 years Cared for Total
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border-right:1px solid;">
B4702
</td>
<td style="text-align:left;width: 30em; ">
Age group of parent 25 29 years Number of children ever born Six or more
children
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border-right:1px solid;">
B4849
</td>
<td style="text-align:left;width: 30em; ">
One parent family with children under 15 and no dependent students and
non dependent children Persons
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border-right:1px solid;">
B7452
</td>
<td style="text-align:left;width: 30em; ">
Professional scientific and technical services Occupation Labourers
</td>
</tr>
</tbody>
</table>

   We are building a linear model with the selected features. The
summary of the linear model as presented below.

``` r
lm_fit <- lm(DV_Count_2011 ~ B115 + B126 + B5503+ B2847+ B4849 + B4702 + B2252 + B4258 + B2894 + B1758 + B3851 + B7452 + B2409 + B1390,DV_NSW_2011_all)

summary(lm_fit)
```

    ## 
    ## Call:
    ## lm(formula = DV_Count_2011 ~ B115 + B126 + B5503 + B2847 + B4849 + 
    ##     B4702 + B2252 + B4258 + B2894 + B1758 + B3851 + B7452 + B2409 + 
    ##     B1390, data = DV_NSW_2011_all)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -100.473  -17.105   -1.651   18.293  116.720 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 41.0157167 32.3057487   1.270 0.206582    
    ## B115         0.0015837  0.0141017   0.112 0.910762    
    ## B126        -0.0002338  0.0005186  -0.451 0.652966    
    ## B5503       -0.6446707  0.7519112  -0.857 0.392878    
    ## B2847        0.0306113  0.0207253   1.477 0.142191    
    ## B4849        0.2614197  0.0394617   6.625 9.30e-10 ***
    ## B4702        7.7695686  1.6862621   4.608 9.90e-06 ***
    ## B2252        9.1174831  2.3752840   3.838 0.000196 ***
    ## B4258        0.1956317  0.0460197   4.251 4.13e-05 ***
    ## B2894       -0.8215502  0.1847910  -4.446 1.91e-05 ***
    ## B1758       11.3768055  3.1433709   3.619 0.000427 ***
    ## B3851        2.3630352  0.5996256   3.941 0.000134 ***
    ## B7452        0.8505166  0.3691961   2.304 0.022891 *  
    ## B2409       -3.4327696  1.7094242  -2.008 0.046784 *  
    ## B1390       -2.5787578  0.9509693  -2.712 0.007636 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 34.06 on 125 degrees of freedom
    ## Multiple R-squared:  0.9831, Adjusted R-squared:  0.9812 
    ## F-statistic: 519.3 on 14 and 125 DF,  p-value: < 2.2e-16

   The model summary shows, the features added manually B115, B126,
B5503 and B2847 has P-Value greater than 0.05, hance these are not a
good predictor in the model. Therefore, we cannot reject
*H*<sub>0</sub>, i.e. There is no relationship between income, education
and employment with DV. The features selected by Forward stepwise
Feature selection algorithm are the best predictor to predict DV. The
regression summary we can see these features have very low P-Value
indicating the features having very strong relationship with the
response variable. This fact is marked by \*\*\* in the summary. Also,
the Multiple R-squared of the model returned 0.9831, indicating the
predictors can explain 98.12% of variance from the model.  

``` r
plot(lm_fit)
```

<img src="README_figs/README-LM Model Plot-1.png" style="display: block; margin: auto;" /><img src="README_figs/README-LM Model Plot-2.png" style="display: block; margin: auto;" /><img src="README_figs/README-LM Model Plot-3.png" style="display: block; margin: auto;" /><img src="README_figs/README-LM Model Plot-4.png" style="display: block; margin: auto;" />
   One of the best ways to present finding from a model is to generate
plot\_summs from jtools library. Using the plot, we can show the best
predictor’s coefficient estimations are normally distributed as
presented in plot below.

``` r
plot_summs(lm_fit, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
```

    ## Registered S3 methods overwritten by 'broom':
    ##   method            from  
    ##   tidy.glht         jtools
    ##   tidy.summary.glht jtools

    ## Loading required namespace: broom.mixed
    ## Loading required namespace: broom.mixed

<img src="README_figs/README-LM Model Plot SUMMS-1.png" style="display: block; margin: auto;" />

   Another useful visualization is effect\_plot to show the relationship
between predictor and response variable. The predictors with low P-Value
are visualized below. All these plot display very strong connection with
the response variable.

``` r
effect_plot(lm_fit, pred = "B4849", interval = TRUE, plot.points = TRUE)
```

<img src="README_figs/README-LM Model effect Plot-1.png" style="display: block; margin: auto;" />

``` r
effect_plot(lm_fit, pred = "B4702", interval = TRUE, plot.points = TRUE)
```

<img src="README_figs/README-LM Model effect Plot-2.png" style="display: block; margin: auto;" />

``` r
effect_plot(lm_fit, pred = "B2252", interval = TRUE, plot.points = TRUE)
```

<img src="README_figs/README-LM Model effect Plot-3.png" style="display: block; margin: auto;" />

``` r
effect_plot(lm_fit, pred = "B4258", interval = TRUE, plot.points = TRUE)
```

<img src="README_figs/README-LM Model effect Plot-4.png" style="display: block; margin: auto;" />

``` r
effect_plot(lm_fit, pred = "B2894", interval = TRUE, plot.points = TRUE)
```

<img src="README_figs/README-LM Model effect Plot-5.png" style="display: block; margin: auto;" />

``` r
effect_plot(lm_fit, pred = "B1758", interval = TRUE, plot.points = TRUE)
```

<img src="README_figs/README-LM Model effect Plot-6.png" style="display: block; margin: auto;" />

``` r
effect_plot(lm_fit, pred = "B3851", interval = TRUE, plot.points = TRUE)
```

<img src="README_figs/README-LM Model effect Plot-7.png" style="display: block; margin: auto;" />

``` r
effect_plot(lm_fit, pred = "B1390", interval = TRUE, plot.points = TRUE)
```

<img src="README_figs/README-LM Model effect Plot-8.png" style="display: block; margin: auto;" />

# Conclusion

   DV is a social problem in NSW. The data we have used in this project,
clearly shows the level of crime does not have a downward trend. Policy
maker and law enforcing agencies need to have more clear insight on the
crime, so that they can come up with a policy to reduce this crime. This
project can add a very good value in this regard. We have shown the
trend of the crime and the area where the crime is happening the most.
We have also identified some key predictors and build a regression model
that can predict the crime very with very high level of confidence. The
data we have worked on is not enough to build a complete model. We had
only census data of 2011. In future we shall add more census and DV data
and try to build a complete model that can predict DV with higher
accuracy.
