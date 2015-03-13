# LiquidAssets-Wine: Price regression
bdanalytics  

**  **    
**Date: (Fri) Mar 13, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/wine.csv  
    New:        https://courses.edx.org/c4x/MITx/15.071x_2/asset/wine_test.csv  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
#suppressPackageStartupMessages(require())

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis specific global variables
glb_separate_predict_dataset <- TRUE
glb_predct_var <- "Price"               # or NULL
glb_sel_mdl <- NULL

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/wine.csv", 
    comment="entity_df", print_diagn=TRUE)
```

```
## [1] "Reading file ./data/wine.csv..."
## [1] "dimensions of data in ./data/wine.csv: 25 rows x 7 cols"
##   Year  Price WinterRain    AGST HarvestRain Age FrancePop
## 1 1952 7.4950        600 17.1167         160  31  43183.57
## 2 1953 8.0393        690 16.7333          80  30  43495.03
## 3 1955 7.6858        502 17.1500         130  28  44217.86
## 4 1957 6.9845        420 16.1333         110  26  45152.25
## 5 1958 6.7772        582 16.4167         187  25  45653.81
## 6 1959 8.0757        485 17.4833         187  24  46128.64
##    Year  Price WinterRain    AGST HarvestRain Age FrancePop
## 4  1957 6.9845        420 16.1333         110  26  45152.25
## 10 1963 6.7127        608 15.7167         155  20  48798.99
## 18 1971 7.1934        551 16.7667         112  12  52431.65
## 19 1972 6.2049        536 14.9833         158  11  52894.18
## 20 1973 6.6367        376 17.0667         123  10  53332.81
## 22 1975 7.2920        572 16.9500         171   8  53955.04
##    Year  Price WinterRain    AGST HarvestRain Age FrancePop
## 20 1973 6.6367        376 17.0667         123  10  53332.81
## 21 1974 6.2941        574 16.3000         184   9  53689.61
## 22 1975 7.2920        572 16.9500         171   8  53955.04
## 23 1976 7.1211        418 17.6500         247   7  54159.05
## 24 1977 6.2587        821 15.5833          87   6  54378.36
## 25 1978 7.1860        763 15.8167          51   5  54602.19
## 'data.frame':	25 obs. of  7 variables:
##  $ Year       : int  1952 1953 1955 1957 1958 1959 1960 1961 1962 1963 ...
##  $ Price      : num  7.5 8.04 7.69 6.98 6.78 ...
##  $ WinterRain : int  600 690 502 420 582 485 763 830 697 608 ...
##  $ AGST       : num  17.1 16.7 17.1 16.1 16.4 ...
##  $ HarvestRain: int  160 80 130 110 187 187 290 38 52 155 ...
##  $ Age        : int  31 30 28 26 25 24 23 22 21 20 ...
##  $ FrancePop  : num  43184 43495 44218 45152 45654 ...
##  - attr(*, "comment")= chr "entity_df"
## NULL
```

```r
if (glb_separate_predict_dataset) {
    predct_df <- myimport_data(
        url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/wine_test.csv", 
        comment="predct_df", print_diagn=TRUE)
} else {
    predct_df <- entity_df[sample(1:nrow(entity_df), nrow(entity_df) / 1000),]
    comment(predct_df) <- "predct_df"
    myprint_df(predct_df)
    str(predct_df)
}         
```

```
## [1] "Reading file ./data/wine_test.csv..."
## [1] "dimensions of data in ./data/wine_test.csv: 2 rows x 7 cols"
##   Year  Price WinterRain    AGST HarvestRain Age FrancePop
## 1 1979 6.9541        717 16.1667         122   4  54835.83
## 2 1980 6.4979        578 16.0000          74   3  55110.24
## 'data.frame':	2 obs. of  7 variables:
##  $ Year       : int  1979 1980
##  $ Price      : num  6.95 6.5
##  $ WinterRain : int  717 578
##  $ AGST       : num  16.2 16
##  $ HarvestRain: int  122 74
##  $ Age        : int  4 3
##  $ FrancePop  : num  54836 55110
##  - attr(*, "comment")= chr "predct_df"
## NULL
```

```r
script_df <- rbind(script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 cleanse_data                2                0
```

## Step `2`: cleanse data

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_explore_data", 
                              chunk_step_major=max(script_df$chunk_step_major), 
                              chunk_step_minor=1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(entity_df))
#View(entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Potential Enhancements:
#       One code chunk to cycle thru entity_df & predct_df ?
#           Use with / within ?
#           for (df in c(entity_df, predct_df)) cycles thru column names
#           for (df in list(entity_df, predct_df)) does not change the actual dataframes
#
#       Build splines   require(splines); bsBasis <- bs(training$age, df=3)

# entity_df <- mutate(entity_df,
#     <col_name>.NA=is.na(<col_name>) 
#     <col_name>_fctr=as.factor(<col_name>),
#     
#     Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#     Year=year(Date.my),
#     Month=months(Date.my),
#     Weekday=weekdays(Date.my)
#     
#                     )
# 
# predct_df <- mutate(predct_df, 
#                     )

print(summary(entity_df))
```

```
##       Year          Price         WinterRain         AGST      
##  Min.   :1952   Min.   :6.205   Min.   :376.0   Min.   :14.98  
##  1st Qu.:1960   1st Qu.:6.519   1st Qu.:536.0   1st Qu.:16.20  
##  Median :1966   Median :7.121   Median :600.0   Median :16.53  
##  Mean   :1966   Mean   :7.067   Mean   :605.3   Mean   :16.51  
##  3rd Qu.:1972   3rd Qu.:7.495   3rd Qu.:697.0   3rd Qu.:17.07  
##  Max.   :1978   Max.   :8.494   Max.   :830.0   Max.   :17.65  
##   HarvestRain         Age         FrancePop    
##  Min.   : 38.0   Min.   : 5.0   Min.   :43184  
##  1st Qu.: 89.0   1st Qu.:11.0   1st Qu.:46584  
##  Median :130.0   Median :17.0   Median :50255  
##  Mean   :148.6   Mean   :17.2   Mean   :49694  
##  3rd Qu.:187.0   3rd Qu.:23.0   3rd Qu.:52894  
##  Max.   :292.0   Max.   :31.0   Max.   :54602
```

```r
print(sapply(names(entity_df), function(col) sum(is.na(entity_df[, col]))))
```

```
##        Year       Price  WinterRain        AGST HarvestRain         Age 
##           0           0           0           0           0           0 
##   FrancePop 
##           0
```

```r
print(summary(predct_df))
```

```
##       Year          Price         WinterRain         AGST      
##  Min.   :1979   Min.   :6.498   Min.   :578.0   Min.   :16.00  
##  1st Qu.:1979   1st Qu.:6.612   1st Qu.:612.8   1st Qu.:16.04  
##  Median :1980   Median :6.726   Median :647.5   Median :16.08  
##  Mean   :1980   Mean   :6.726   Mean   :647.5   Mean   :16.08  
##  3rd Qu.:1980   3rd Qu.:6.840   3rd Qu.:682.2   3rd Qu.:16.13  
##  Max.   :1980   Max.   :6.954   Max.   :717.0   Max.   :16.17  
##   HarvestRain       Age         FrancePop    
##  Min.   : 74   Min.   :3.00   Min.   :54836  
##  1st Qu.: 86   1st Qu.:3.25   1st Qu.:54904  
##  Median : 98   Median :3.50   Median :54973  
##  Mean   : 98   Mean   :3.50   Mean   :54973  
##  3rd Qu.:110   3rd Qu.:3.75   3rd Qu.:55042  
##  Max.   :122   Max.   :4.00   Max.   :55110
```

```r
print(sapply(names(predct_df), function(col) sum(is.na(predct_df[, col]))))
```

```
##        Year       Price  WinterRain        AGST HarvestRain         Age 
##           0           0           0           0           0           0 
##   FrancePop 
##           0
```

```r
#pairs(subset(entity_df, select=-c(col_symbol)))

#   Histogram of predictor in entity_df & predct_df
# Check for predct_df & entity_df features range mismatches

# Other diagnostics:
# print(subset(entity_df, <col1_name> == max(entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(entity_df$<col1_name>, na.rm=TRUE)))

# print(<col_name>_freq_entity_df <- mycreate_tbl_df(entity_df, "<col_name>"))
# print(which.min(table(entity_df$<col_name>)))
# print(which.max(table(entity_df$<col_name>)))
# print(which.max(table(entity_df$<col1_name>, entity_df$<col2_name>)[, 2]))
# print(table(entity_df$<col1_name>, entity_df$<col2_name>))
# print(table(is.na(entity_df$<col1_name>), entity_df$<col2_name>))
# print(xtabs(~ <col1_name>, entity_df))
# print(xtabs(~ <col1_name> + <col2_name>, entity_df))
# print(<col1_name>_<col2_name>_xtab_entity_df <- 
#   mycreate_xtab(entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_entity_df[is.na(<col1_name>_<col2_name>_xtab_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(entity_df$<col1_name>, entity_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(entity_df$<col1_name>.NA, entity_df$<col2_name>, mean, na.rm=TRUE)))


# Other plots:
# print(myplot_histogram(entity_df, "<col1_name>"))
# print(myplot_box(df=entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(entity_df, "<col1_name>", "<col2_name>"))

script_df <- rbind(script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                3                1
## 4  manage_missing_data                3                2
```

### Step `3`.`2`: manage missing data

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1)) 
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                3                1
## 4  manage_missing_data                3                2
## 5   encode_retype_data                3                3
```

### Step `3`.`3`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# 
# entity_df <- mymap_codes(entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##           chunk_label chunk_step_major chunk_step_minor
## 1         import_data                1                0
## 2        inspect_data                2                1
## 3 manage_missing_data                2                2
## 4  encode_retype_data                2                3
## 5    extract_features                3                0
```

## Step `3`: extract features

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##           chunk_label chunk_step_major chunk_step_minor
## 1         import_data                1                0
## 2        inspect_data                2                1
## 3 manage_missing_data                2                2
## 4         encode_data                2                2
## 5    extract_features                3                0
## 6     select_features                4                0
```

## Step `4`: select features

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(script_df$chunk_step_major),
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))        
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               inspect_data                2                1
## 3        manage_missing_data                2                2
## 4                encode_data                2                2
## 5           extract_features                3                0
## 6            select_features                4                0
## 7 remove_correlated_features                4                1
```

### Step `4`.`1`: remove correlated features

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="run_models", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               inspect_data                2                1
## 3        manage_missing_data                2                2
## 4                encode_data                2                2
## 5           extract_features                3                0
## 6            select_features                4                0
## 7 remove_correlated_features                4                1
## 8                 run_models                5                0
```

## Step `5`: run models

```r
models_df <- data.frame()

#   Regression:
#       Linear:
ret_lst <- myrun_mdl_lm(indep_vars_vctr=c("AGST"), 
                        models_df=models_df)
print(summary(mdl <- ret_lst$model)); print(models_df <- ret_lst$models_df)
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = entity_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.78450 -0.23882 -0.03727  0.38992  0.90318 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -3.4178     2.4935  -1.371 0.183710    
## AGST          0.6351     0.1509   4.208 0.000335 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4993 on 23 degrees of freedom
## Multiple R-squared:  0.435,	Adjusted R-squared:  0.4105 
## F-statistic: 17.71 on 1 and 23 DF,  p-value: 0.000335
```

```
##   feats Adj.R.sq      SSE
## 1  AGST 0.410459 5.734875
```

```r
ret_lst <- myrun_mdl_lm(indep_vars_vctr=c("AGST", "HarvestRain"), 
                        models_df=models_df)
print(summary(mdl <- ret_lst$model)); print(models_df <- ret_lst$models_df)
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = entity_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.88321 -0.19600  0.06178  0.15379  0.59722 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.20265    1.85443  -1.188 0.247585    
## AGST         0.60262    0.11128   5.415 1.94e-05 ***
## HarvestRain -0.00457    0.00101  -4.525 0.000167 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3674 on 22 degrees of freedom
## Multiple R-squared:  0.7074,	Adjusted R-squared:  0.6808 
## F-statistic: 26.59 on 2 and 22 DF,  p-value: 1.347e-06
```

```
##               feats  Adj.R.sq      SSE
## 1              AGST 0.4104590 5.734875
## 2 AGST, HarvestRain 0.6807681 2.970373
```

```r
ret_lst <- myrun_mdl_lm(indep_vars_vctr=c("HarvestRain", "WinterRain"), 
                        models_df=models_df)
print(summary(mdl <- ret_lst$model)); print(models_df <- ret_lst$models_df)
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = entity_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.0933 -0.3222 -0.1012  0.3871  1.1877 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.865e+00  6.616e-01  11.888 4.76e-11 ***
## HarvestRain -4.971e-03  1.601e-03  -3.105  0.00516 ** 
## WinterRain  -9.848e-05  9.007e-04  -0.109  0.91392    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5611 on 22 degrees of freedom
## Multiple R-squared:  0.3177,	Adjusted R-squared:  0.2557 
## F-statistic: 5.122 on 2 and 22 DF,  p-value: 0.01492
```

```
##                     feats  Adj.R.sq      SSE
## 1                    AGST 0.4104590 5.734875
## 2       AGST, HarvestRain 0.6807681 2.970373
## 3 HarvestRain, WinterRain 0.2556753 6.925756
```

```r
ret_lst <- myrun_mdl_lm(indep_vars_vctr=".", 
                        models_df=models_df)
print(summary(mdl <- ret_lst$model)); print(models_df <- ret_lst$models_df)
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = entity_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.48179 -0.24662 -0.00726  0.22012  0.51987 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.092e-01  1.467e+02   0.005 0.996194    
## Year        -5.847e-04  7.900e-02  -0.007 0.994172    
## WinterRain   1.043e-03  5.310e-04   1.963 0.064416 .  
## AGST         6.012e-01  1.030e-01   5.836 1.27e-05 ***
## HarvestRain -3.958e-03  8.751e-04  -4.523 0.000233 ***
## Age                 NA         NA      NA       NA    
## FrancePop   -4.953e-05  1.667e-04  -0.297 0.769578    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3019 on 19 degrees of freedom
## Multiple R-squared:  0.8294,	Adjusted R-squared:  0.7845 
## F-statistic: 18.47 on 5 and 19 DF,  p-value: 1.044e-06
```

```
##                                                 feats  Adj.R.sq      SSE
## 1                                                AGST 0.4104590 5.734875
## 2                                   AGST, HarvestRain 0.6807681 2.970373
## 3                             HarvestRain, WinterRain 0.2556753 6.925756
## 4 Year, WinterRain, AGST, HarvestRain, Age, FrancePop 0.7844538 1.732113
```

```r
ret_lst <- myrun_mdl_lm(indep_vars_vctr=c("AGST", "HarvestRain", "Age", "WinterRain"), 
                        models_df=models_df)
print(summary(mdl <- ret_lst$model)); print(models_df <- ret_lst$models_df)
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = entity_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.45470 -0.24273  0.00752  0.19773  0.53637 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.4299802  1.7658975  -1.942 0.066311 .  
## AGST         0.6072093  0.0987022   6.152  5.2e-06 ***
## HarvestRain -0.0039715  0.0008538  -4.652 0.000154 ***
## Age          0.0239308  0.0080969   2.956 0.007819 ** 
## WinterRain   0.0010755  0.0005073   2.120 0.046694 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.295 on 20 degrees of freedom
## Multiple R-squared:  0.8286,	Adjusted R-squared:  0.7943 
## F-statistic: 24.17 on 4 and 20 DF,  p-value: 2.036e-07
```

```
##                                                 feats  Adj.R.sq      SSE
## 1                                                AGST 0.4104590 5.734875
## 2                                   AGST, HarvestRain 0.6807681 2.970373
## 3                             HarvestRain, WinterRain 0.2556753 6.925756
## 4 Year, WinterRain, AGST, HarvestRain, Age, FrancePop 0.7844538 1.732113
## 5                  AGST, HarvestRain, Age, WinterRain 0.7942795 1.740162
```

```r
glb_sel_mdl <- mdl

print(myplot_scatter(models_df, "SSE", "Adj.R.sq") + 
          geom_text(aes(label=feats), data=models_df, color="NavyBlue", size=3.5))
```

![](LiquidAssets_Wine_files/figure-html/run_models-1.png) 

```r
# script_df <- rbind(script_df, 
#                    data.frame(chunk_label="run_models", 
#                               chunk_step_major=max(script_df$chunk_step_major)+1, 
#                               chunk_step_minor=0))
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               inspect_data                2                1
## 3        manage_missing_data                2                2
## 4                encode_data                2                2
## 5           extract_features                3                0
## 6            select_features                4                0
## 7 remove_correlated_features                4                1
## 8                 run_models                5                0
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] doBy_4.5-13     survival_2.38-1 ggplot2_1.0.0  
## 
## loaded via a namespace (and not attached):
##  [1] codetools_0.2-10 colorspace_1.2-5 digest_0.6.8     evaluate_0.5.5  
##  [5] formatR_1.0      grid_3.1.2       gtable_0.1.2     htmltools_0.2.6 
##  [9] knitr_1.9        labeling_0.3     lattice_0.20-30  MASS_7.3-39     
## [13] Matrix_1.1-5     munsell_0.4.2    plyr_1.8.1       proto_0.3-10    
## [17] Rcpp_0.11.4      reshape2_1.4.1   rmarkdown_0.5.1  scales_0.2.4    
## [21] splines_3.1.2    stringr_0.6.2    tcltk_3.1.2      tools_3.1.2     
## [25] yaml_2.1.13
```
