# LiquidAssets-Wine: Price regression
bdanalytics  

**  **    
**Date: (Mon) Mar 16, 2015**    

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
suppressPackageStartupMessages(require(reshape2))

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_is_separate_predict_dataset <- TRUE
glb_predct_var <- "Price"               # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_var <- "Year"                    # or NULL
glb_is_id_var_a_feature <- TRUE
glb_exclude_vars_as_features <- "FrancePop" # or NULL; mydelete_cor_features prefers FrancePop over Age

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
glb_entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/wine.csv", 
    comment="glb_entity_df", print_diagn=TRUE)
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
##  - attr(*, "comment")= chr "glb_entity_df"
## NULL
```

```r
if (glb_is_separate_predict_dataset) {
    glb_predct_df <- myimport_data(
        url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/wine_test.csv", 
        comment="glb_predct_df", print_diagn=TRUE)
} else {
    glb_predct_df <- glb_entity_df[sample(1:nrow(glb_entity_df), nrow(glb_entity_df) / 1000),]
    comment(glb_predct_df) <- "glb_predct_df"
    myprint_df(glb_predct_df)
    str(glb_predct_df)
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
##  - attr(*, "comment")= chr "glb_predct_df"
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
#print(str(glb_entity_df))
#View(glb_entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Potential Enhancements:
#       One code chunk to cycle thru glb_entity_df & glb_predct_df ?
#           Use with / within ?
#           for (df in c(glb_entity_df, glb_predct_df)) cycles thru column names
#           for (df in list(glb_entity_df, glb_predct_df)) does not change the actual dataframes
#
#       Build splines   require(splines); bsBasis <- bs(training$age, df=3)

# glb_entity_df <- mutate(glb_entity_df,
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
# glb_predct_df <- mutate(glb_predct_df, 
#                     )

print(summary(glb_entity_df))
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
print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
```

```
##        Year       Price  WinterRain        AGST HarvestRain         Age 
##           0           0           0           0           0           0 
##   FrancePop 
##           0
```

```r
print(summary(glb_predct_df))
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
print(sapply(names(glb_predct_df), function(col) sum(is.na(glb_predct_df[, col]))))
```

```
##        Year       Price  WinterRain        AGST HarvestRain         Age 
##           0           0           0           0           0           0 
##   FrancePop 
##           0
```

```r
#pairs(subset(glb_entity_df, select=-c(col_symbol)))

#   Histogram of predictor in glb_entity_df & glb_predct_df
# Check for glb_predct_df & glb_entity_df features range mismatches

# Other diagnostics:
# print(subset(glb_entity_df, <col1_name> == max(glb_entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_entity_df$<col1_name>, na.rm=TRUE)))

# print(<col_name>_freq_glb_entity_df <- mycreate_tbl_df(glb_entity_df, "<col_name>"))
# print(which.min(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>)[, 2]))
# print(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>))
# print(table(is.na(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(xtabs(~ <col1_name>, glb_entity_df))
# print(xtabs(~ <col1_name> + <col2_name>, glb_entity_df))
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mycreate_xtab(glb_entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_entity_df[is.na(<col1_name>_<col2_name>_xtab_glb_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>.NA, glb_entity_df$<col2_name>, mean, na.rm=TRUE)))


# Other plots:
# print(myplot_histogram(glb_entity_df, "<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>"))

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
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
```

### Step `2`.`2`: manage missing data

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
# glb_entity_df <- mymap_codes(glb_entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                3                1
## 4  manage_missing_data                3                2
## 5   encode_retype_data                3                3
## 6     extract_features                4                0
```

## Step `4`: extract features

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
print(glb_feats_df <- data.frame(id=setdiff(names(glb_entity_df), glb_predct_var),
            cor.y=cor(glb_entity_df[, setdiff(names(glb_entity_df), glb_predct_var)], 
                        y=glb_entity_df[, glb_predct_var])[,1]))
```

```
##                      id      cor.y
## Year               Year -0.4477679
## WinterRain   WinterRain  0.1366505
## AGST               AGST  0.6595629
## HarvestRain HarvestRain -0.5633219
## Age                 Age  0.4477679
## FrancePop     FrancePop -0.4668616
```

```r
print(glb_feats_df <- mydelete_cor_features())
```

```
##                    Year   WinterRain        AGST HarvestRain         Age
## Year         1.00000000  0.016970024 -0.24691585  0.02800907 -1.00000000
## WinterRain   0.01697002  1.000000000 -0.32109061 -0.27544085 -0.01697002
## AGST        -0.24691585 -0.321090611  1.00000000 -0.06449593  0.24691585
## HarvestRain  0.02800907 -0.275440854 -0.06449593  1.00000000 -0.02800907
## Age         -1.00000000 -0.016970024  0.24691585 -0.02800907  1.00000000
## FrancePop    0.99448510 -0.001621627 -0.25916227  0.04126439 -0.99448510
##                FrancePop
## Year         0.994485097
## WinterRain  -0.001621627
## AGST        -0.259162274
## HarvestRain  0.041264394
## Age         -0.994485097
## FrancePop    1.000000000
##                   Year  WinterRain       AGST HarvestRain        Age
## Year        0.00000000 0.016970024 0.24691585  0.02800907 1.00000000
## WinterRain  0.01697002 0.000000000 0.32109061  0.27544085 0.01697002
## AGST        0.24691585 0.321090611 0.00000000  0.06449593 0.24691585
## HarvestRain 0.02800907 0.275440854 0.06449593  0.00000000 0.02800907
## Age         1.00000000 0.016970024 0.24691585  0.02800907 0.00000000
## FrancePop   0.99448510 0.001621627 0.25916227  0.04126439 0.99448510
##               FrancePop
## Year        0.994485097
## WinterRain  0.001621627
## AGST        0.259162274
## HarvestRain 0.041264394
## Age         0.994485097
## FrancePop   0.000000000
## [1] "cor(Year, Age)=-1.0000"
```

![](LiquidAssets_Wine_files/figure-html/remove_correlated_features-1.png) 

```
## [1] "cor(Price, Year)=-0.4478"
## [1] "cor(Price, Age)=0.4478"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in mydelete_cor_features(): Dropping Year as a feature
```

![](LiquidAssets_Wine_files/figure-html/remove_correlated_features-2.png) 

```
##                      id      cor.y
## WinterRain   WinterRain  0.1366505
## AGST               AGST  0.6595629
## HarvestRain HarvestRain -0.5633219
## Age                 Age  0.4477679
## FrancePop     FrancePop -0.4668616
##               WinterRain        AGST HarvestRain         Age    FrancePop
## WinterRain   1.000000000 -0.32109061 -0.27544085 -0.01697002 -0.001621627
## AGST        -0.321090611  1.00000000 -0.06449593  0.24691585 -0.259162274
## HarvestRain -0.275440854 -0.06449593  1.00000000 -0.02800907  0.041264394
## Age         -0.016970024  0.24691585 -0.02800907  1.00000000 -0.994485097
## FrancePop   -0.001621627 -0.25916227  0.04126439 -0.99448510  1.000000000
##              WinterRain       AGST HarvestRain        Age   FrancePop
## WinterRain  0.000000000 0.32109061  0.27544085 0.01697002 0.001621627
## AGST        0.321090611 0.00000000  0.06449593 0.24691585 0.259162274
## HarvestRain 0.275440854 0.06449593  0.00000000 0.02800907 0.041264394
## Age         0.016970024 0.24691585  0.02800907 0.00000000 0.994485097
## FrancePop   0.001621627 0.25916227  0.04126439 0.99448510 0.000000000
## [1] "cor(Age, FrancePop)=-0.9945"
```

![](LiquidAssets_Wine_files/figure-html/remove_correlated_features-3.png) 

```
## [1] "cor(Price, Age)=0.4478"
## [1] "cor(Price, FrancePop)=-0.4669"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in mydelete_cor_features(): Dropping FrancePop as a feature
```

![](LiquidAssets_Wine_files/figure-html/remove_correlated_features-4.png) 

```
##                      id      cor.y
## WinterRain   WinterRain  0.1366505
## AGST               AGST  0.6595629
## HarvestRain HarvestRain -0.5633219
## Age                 Age  0.4477679
##              WinterRain        AGST HarvestRain         Age
## WinterRain   1.00000000 -0.32109061 -0.27544085 -0.01697002
## AGST        -0.32109061  1.00000000 -0.06449593  0.24691585
## HarvestRain -0.27544085 -0.06449593  1.00000000 -0.02800907
## Age         -0.01697002  0.24691585 -0.02800907  1.00000000
##             WinterRain       AGST HarvestRain        Age
## WinterRain  0.00000000 0.32109061  0.27544085 0.01697002
## AGST        0.32109061 0.00000000  0.06449593 0.24691585
## HarvestRain 0.27544085 0.06449593  0.00000000 0.02800907
## Age         0.01697002 0.24691585  0.02800907 0.00000000
##                      id      cor.y
## WinterRain   WinterRain  0.1366505
## AGST               AGST  0.6595629
## HarvestRain HarvestRain -0.5633219
## Age                 Age  0.4477679
```

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
glb_models_df <- data.frame()

#   Regression:
#       Linear:
ret_lst <- myrun_mdl_lm(indep_vars_vctr=c("AGST"), 
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 0.07135757
## [1] 0.7882105
```

```r
print(summary(mdl <- ret_lst$model)); 
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
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

```r
print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
              glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))
```

```
##   feats n.fit  R.sq.fit  R.sq.OOB Adj.R.sq.fit  SSE.fit    SSE.OOB
## 1  AGST    25 0.4350232 0.7882105     0.410459 5.734875 0.07135757
```

```r
ret_lst <- myrun_mdl_lm(indep_vars_vctr=c("AGST", "HarvestRain"), 
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 0.3645547
## [1] -0.08199948
```

```r
print(summary(mdl <- ret_lst$model)); 
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
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

```r
print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
              glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))
```

```
##               feats n.fit  R.sq.fit    R.sq.OOB Adj.R.sq.fit  SSE.fit
## 1              AGST    25 0.4350232  0.78821050    0.4104590 5.734875
## 2 AGST, HarvestRain    25 0.7073708 -0.08199948    0.6807681 2.970373
##      SSE.OOB
## 1 0.07135757
## 2 0.36455468
```

```r
ret_lst <- myrun_mdl_lm(indep_vars_vctr=c("AGST", "HarvestRain", "Age"), 
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 0.1569299
## [1] 0.5342316
```

```r
print(summary(mdl <- ret_lst$model)); 
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.66258 -0.22953 -0.00268  0.27236  0.49391 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.4778196  1.6274142  -0.908  0.37414    
## AGST         0.5322922  0.0995343   5.348 2.65e-05 ***
## HarvestRain -0.0045386  0.0008757  -5.183 3.90e-05 ***
## Age          0.0250875  0.0087249   2.875  0.00905 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3186 on 21 degrees of freedom
## Multiple R-squared:   0.79,	Adjusted R-squared:   0.76 
## F-statistic: 26.34 on 3 and 21 DF,  p-value: 2.596e-07
```

```r
print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
              glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))
```

```
##                    feats n.fit  R.sq.fit    R.sq.OOB Adj.R.sq.fit  SSE.fit
## 1                   AGST    25 0.4350232  0.78821050    0.4104590 5.734875
## 3 AGST, HarvestRain, Age    25 0.7900362  0.53423162    0.7600414 2.131266
## 2      AGST, HarvestRain    25 0.7073708 -0.08199948    0.6807681 2.970373
##      SSE.OOB
## 1 0.07135757
## 3 0.15692988
## 2 0.36455468
```

```r
ret_lst <- myrun_mdl_lm(indep_vars_vctr=c("HarvestRain", "WinterRain"), 
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 0.9434133
## [1] -1.800054
```

```r
print(summary(mdl <- ret_lst$model)); 
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
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

```r
print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
              glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))
```

```
##                     feats n.fit  R.sq.fit    R.sq.OOB Adj.R.sq.fit
## 1                    AGST    25 0.4350232  0.78821050    0.4104590
## 3  AGST, HarvestRain, Age    25 0.7900362  0.53423162    0.7600414
## 2       AGST, HarvestRain    25 0.7073708 -0.08199948    0.6807681
## 4 HarvestRain, WinterRain    25 0.3177024 -1.80005374    0.2556753
##    SSE.fit    SSE.OOB
## 1 5.734875 0.07135757
## 3 2.131266 0.15692988
## 2 2.970373 0.36455468
## 4 6.925756 0.94341330
```

```r
ret_lst <- myrun_mdl_lm(indep_vars_vctr=".", 
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## Warning in predict.lm(mdl, newdata = OOB_df): prediction from a
## rank-deficient fit may be misleading
```

```
## [1] 0.08199167
## [1] 0.7566484
```

```r
print(summary(mdl <- ret_lst$model)); 
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
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

```r
print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
              glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))
```

```
##                                                 feats n.fit  R.sq.fit
## 1                                                AGST    25 0.4350232
## 5 Year, WinterRain, AGST, HarvestRain, Age, FrancePop    25 0.8293592
## 3                              AGST, HarvestRain, Age    25 0.7900362
## 2                                   AGST, HarvestRain    25 0.7073708
## 4                             HarvestRain, WinterRain    25 0.3177024
##      R.sq.OOB Adj.R.sq.fit  SSE.fit    SSE.OOB
## 1  0.78821050    0.4104590 5.734875 0.07135757
## 5  0.75664845    0.7844538 1.732113 0.08199167
## 3  0.53423162    0.7600414 2.131266 0.15692988
## 2 -0.08199948    0.6807681 2.970373 0.36455468
## 4 -1.80005374    0.2556753 6.925756 0.94341330
```

```r
ret_lst <- myrun_mdl_lm(indep_vars_vctr=c("AGST", "HarvestRain", "Age", "WinterRain"),
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 0.06926281
## [1] 0.7944278
```

```r
print(summary(mdl <- ret_lst$model)); 
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
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

```r
print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
              glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))
```

```
##                                                 feats n.fit  R.sq.fit
## 6                  AGST, HarvestRain, Age, WinterRain    25 0.8285662
## 1                                                AGST    25 0.4350232
## 5 Year, WinterRain, AGST, HarvestRain, Age, FrancePop    25 0.8293592
## 3                              AGST, HarvestRain, Age    25 0.7900362
## 2                                   AGST, HarvestRain    25 0.7073708
## 4                             HarvestRain, WinterRain    25 0.3177024
##      R.sq.OOB Adj.R.sq.fit  SSE.fit    SSE.OOB
## 6  0.79442776    0.7942795 1.740162 0.06926281
## 1  0.78821050    0.4104590 5.734875 0.07135757
## 5  0.75664845    0.7844538 1.732113 0.08199167
## 3  0.53423162    0.7600414 2.131266 0.15692988
## 2 -0.08199948    0.6807681 2.970373 0.36455468
## 4 -1.80005374    0.2556753 6.925756 0.94341330
```

```r
glb_sel_mdl <- mdl

print(myplot_scatter(glb_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats), data=glb_models_df, color="NavyBlue", size=3.5))
```

![](LiquidAssets_Wine_files/figure-html/run_models-1.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="fit_trainingall", 
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
## 9            fit_trainingall                6                0
```

## Step `6`: fit trainingall

```r
ret_lst <- myrun_mdl_lm(indep_vars_vctr=glb_feats_df$id, fit_df=glb_entity_df)
print(summary(mdl <- ret_lst$model)); print(glb_sel_mdl_df <- ret_lst$models_df)
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.45470 -0.24273  0.00752  0.19773  0.53637 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.4299802  1.7658975  -1.942 0.066311 .  
## WinterRain   0.0010755  0.0005073   2.120 0.046694 *  
## AGST         0.6072093  0.0987022   6.152  5.2e-06 ***
## HarvestRain -0.0039715  0.0008538  -4.652 0.000154 ***
## Age          0.0239308  0.0080969   2.956 0.007819 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.295 on 20 degrees of freedom
## Multiple R-squared:  0.8286,	Adjusted R-squared:  0.7943 
## F-statistic: 24.17 on 4 and 20 DF,  p-value: 2.036e-07
```

```
##                                feats n.fit  R.sq.fit R.sq.OOB Adj.R.sq.fit
## 1 WinterRain, AGST, HarvestRain, Age    25 0.8285662       NA    0.7942795
##    SSE.fit SSE.OOB
## 1 1.740162      NA
```

```r
glb_sel_mdl <- mdl

script_df <- rbind(script_df, 
                   data.frame(chunk_label="predict_newdata", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                inspect_data                2                1
## 3         manage_missing_data                2                2
## 4                 encode_data                2                2
## 5            extract_features                3                0
## 6             select_features                4                0
## 7  remove_correlated_features                4                1
## 8                  run_models                5                0
## 9             fit_trainingall                6                0
## 10            predict_newdata                7                0
```

## Step `7`: predict newdata

```r
glb_predct_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_predct_df)
print(glb_predct_df)
```

```
##   Year  Price WinterRain    AGST HarvestRain Age FrancePop Price.predict
## 1 1979 6.9541        717 16.1667         122   4  54835.83      6.768925
## 2 1980 6.4979        578 16.0000          74   3  55110.24      6.684910
```

```r
print(myplot_scatter(glb_predct_df, glb_predct_var, glb_predct_var_name))
```

![](LiquidAssets_Wine_files/figure-html/predict_newdata-1.png) 

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
## [1] reshape2_1.4.1  doBy_4.5-13     survival_2.38-1 ggplot2_1.0.0  
## 
## loaded via a namespace (and not attached):
##  [1] codetools_0.2-10 colorspace_1.2-5 digest_0.6.8     evaluate_0.5.5  
##  [5] formatR_1.0      grid_3.1.2       gtable_0.1.2     htmltools_0.2.6 
##  [9] knitr_1.9        labeling_0.3     lattice_0.20-30  MASS_7.3-39     
## [13] Matrix_1.1-5     munsell_0.4.2    plyr_1.8.1       proto_0.3-10    
## [17] Rcpp_0.11.4      rmarkdown_0.5.1  scales_0.2.4     splines_3.1.2   
## [21] stringr_0.6.2    tcltk_3.1.2      tools_3.1.2      yaml_2.1.13
```
