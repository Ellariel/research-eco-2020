---
title: "Экологические установки и ситуативные стимулы в условиях трансформирующейся
  экологической культуры России: эмпирическое исследование"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---
# Введение

Оцениваются 4 поведенческих блока:

- Переработка (Recycling Behavior, RB), 
- Эко-покупки (Eco-Shopping Behavior, ESB), 
- Ресурсосбережение (Resource-Saving Behavior, RSB),
- Эко-мобильность (Eco-Mobility Behavior, EMB).


По каждому из блоков считается аддитивный индекс, представляющий собой оценку склонности респондента к определённому виду эко-поведения. Дополнительно считается индекс склонности к комфорту против склонности к экономии (tendency to be comfortable, TC). 

## Гипотезы

*Гипотеза H1.* Эко-поведение не однородно - склонность к одному из видов эко-поведения не предопределяет склонность к другому.


*Гипотеза H2.* Люди обычно переоценивают собственную склонность к эко-поведению и недооценивают склонность окружающих.


*Гипотеза H3.* Склонность к тому или иному виду эко-поведения ситуативно подвержена влиянию стимулов, как эмоционального, так и рационального характера.


*Гипотеза H3а.* Склонность к более затратным видам поведения менее подвержена ситуативному влиянию стимулов (low-cost hypothesis).

## Подготовка данных

Ниже представленны параметры окружения R. Производится загрузка данных, логарифмирование некоторых признаков (возраст, размер города, время ответа). Вычисляются аддитивные индексы, затем производится их унификация в диапазоне [0...1] для удобства графического отображения и интерпретации.


```r
library(knitr)
library(MatchIt)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gplots)
library(gtable)
library(egg)
library(corrplot)
library(scales)
```



```r
#environment info
sessionInfo()
```

```
## R version 3.6.3 (2020-02-29)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 18363)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=Russian_Russia.1251  LC_CTYPE=Russian_Russia.1251   
## [3] LC_MONETARY=Russian_Russia.1251 LC_NUMERIC=C                   
## [5] LC_TIME=Russian_Russia.1251    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] scales_1.1.1   corrplot_0.84  egg_0.4.5      gridExtra_2.3  gtable_0.3.0  
##  [6] gplots_3.0.3   ggthemes_4.2.0 ggplot2_3.3.1  dplyr_0.8.5    MatchIt_3.0.2 
## [11] knitr_1.28    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.4.6       pillar_1.4.4       compiler_3.6.3     bitops_1.0-6      
##  [5] tools_3.6.3        digest_0.6.25      evaluate_0.14      lifecycle_0.2.0   
##  [9] tibble_3.0.1       pkgconfig_2.0.3    rlang_0.4.6        yaml_2.2.1        
## [13] xfun_0.14          withr_2.2.0        stringr_1.4.0      vctrs_0.3.0       
## [17] gtools_3.8.2       caTools_1.18.0     grid_3.6.3         tidyselect_1.1.0  
## [21] glue_1.4.1         R6_2.4.1           rmarkdown_2.2      gdata_2.18.0      
## [25] purrr_0.3.4        magrittr_1.5       ellipsis_0.3.1     htmltools_0.4.0   
## [29] MASS_7.3-51.6      assertthat_0.2.1   colorspace_1.4-1   KernSmooth_2.23-16
## [33] stringi_1.4.6      munsell_0.5.0      crayon_1.3.4
```

```r
#loading raw data
data<-read.csv("ecodata.csv", sep=";", dec=",")

data<-data[!is.na(data$age),]
data<-data[!is.na(data$income),]

data$group<-factor(data$group, labels = c("Control","Rational","Emotional"))

#log data
data$log_age <- log10(data$age)
data$log_city_size <- log10(data$city_size)
data$log_time_RB <- log10(data$time_12)
data$log_time_ESB <- log10(data$time_34)
data$log_time_RSB <- log10(data$time_56)
data$log_time_EMB <- log10(data$time_78)
data$log_time_TC <- log10(data$time_9)

#make an additive indexes
data$RB  <- (data$case_1_1 + data$case_1_2 + data$case_2_1 + data$case_2_2) #1..24
data$ESB <- (data$case_3_1 + data$case_3_2 + data$case_4_1 + data$case_4_2) #1..24
data$RSB <- (data$case_5_1 + data$case_5_2 + data$case_6_1 + data$case_6_2) #1..24
data$EMB <- (data$case_7_1 + data$case_7_2 + data$case_8_1 + data$case_8_2) #1..24

data<-data[!is.na(data$ESB),]

data$TC  <- (data$case_9_1 + data$case_9_2) #1..12

data$RBo  <- (data$case_1_3 + data$case_2_3) #1..12
data$ESBo <- (data$case_3_3 + data$case_4_3) #1..12
data$RSBo <- (data$case_5_3 + data$case_6_3) #1..12
data$EMBo <- (data$case_7_3 + data$case_8_3) #1..12


#rescale <- function(x){(x-min(x))/(max(x)-min(x))}

data$RB  <- rescale(data$RB)
data$ESB <- rescale(data$ESB)
data$RSB <- rescale(data$RSB)
data$EMB <- rescale(data$EMB)
data$TC  <- rescale(data$TC)
data$RBo  <- rescale(data$RBo)
data$ESBo <- rescale(data$ESBo)
data$RSBo <- rescale(data$RSBo)
data$EMBo <- rescale(data$EMBo)
```


## Сводные статистики и описание данных


```r
summary(data[,c(29,30,32:37,39, 54:62)])
```

```
##       emo              rat              sex              age       
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :14.00  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:20.00  
##  Median :0.0000   Median :0.0000   Median :0.0000   Median :26.00  
##  Mean   :0.4977   Mean   :0.4859   Mean   :0.3354   Mean   :28.88  
##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:35.00  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :67.00  
##  NA's   :103      NA's   :108      NA's   :1                       
##      income        isworking           edu             car        
##  Min.   :1.000   Min.   :0.0000   Min.   :1.000   Min.   :0.0000  
##  1st Qu.:1.000   1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:0.0000  
##  Median :2.000   Median :1.0000   Median :3.000   Median :1.0000  
##  Mean   :2.366   Mean   :0.6509   Mean   :3.065   Mean   :0.7339  
##  3rd Qu.:3.000   3rd Qu.:1.0000   3rd Qu.:3.000   3rd Qu.:1.0000  
##  Max.   :5.000   Max.   :1.0000   Max.   :4.000   Max.   :1.0000  
##                  NA's   :2        NA's   :196     NA's   :196     
##    city_size              RB              ESB              RSB        
##  Min.   :    2552   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:  686430   1st Qu.:0.4500   1st Qu.:0.5000   1st Qu.:0.4000  
##  Median : 1195446   Median :0.5500   Median :0.6500   Median :0.5000  
##  Mean   : 2694228   Mean   :0.5858   Mean   :0.6252   Mean   :0.4959  
##  3rd Qu.: 1483119   3rd Qu.:0.7500   3rd Qu.:0.7500   3rd Qu.:0.5500  
##  Max.   :12692466   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
##  NA's   :17                                                           
##       EMB               TC              RBo              ESBo       
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.4875   1st Qu.:0.5000   1st Qu.:0.2000   1st Qu.:0.5000  
##  Median :0.6000   Median :0.8000   Median :0.3000   Median :0.5000  
##  Mean   :0.5837   Mean   :0.6937   Mean   :0.3197   Mean   :0.5197  
##  3rd Qu.:0.7500   3rd Qu.:1.0000   3rd Qu.:0.5000   3rd Qu.:0.6000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
##                                                                     
##       RSBo             EMBo       
##  Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.4000   1st Qu.:0.3000  
##  Median :0.5000   Median :0.5000  
##  Mean   :0.4928   Mean   :0.4259  
##  3rd Qu.:0.6000   3rd Qu.:0.5000  
##  Max.   :1.0000   Max.   :1.0000  
## 
```

```r
par(mfrow = c(3,4), mar=c(2,1,1,1))
hist(data$RB, breaks = "FD", main = "RB")
hist(data$ESB, breaks = "FD", main = "ESB")
hist(data$RSB, breaks = "FD", main = "RSB")
hist(data$EMB, breaks = "FD", main = "EMB")
hist(data$TC, breaks = "FD", main = "TC")
hist(data$RBo, breaks = "FD", main = "RBo")
hist(data$ESBo, breaks = "FD", main = "ESBo")
hist(data$RSBo, breaks = "FD", main = "RSBo")
hist(data$EMBo, breaks = "FD", main = "EMBo")

hist(data$log_age, breaks = "FD", main = "log_age")
hist(data$log_city_size, breaks = "FD", main = "log_city_size")
hist(data$income, breaks = "FD", main = "income")
```

![](_report_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## Анализ различий между несбалансированными группами



```r
par(mfrow = c(2,2), mar=c(2,2,2,1))
boxplot(RB ~ group, data=data, main="RB", horizontal = T)
boxplot(ESB ~ group, data=data, main="ESB", horizontal = T)
boxplot(RSB ~ group, data=data, main="RSB", horizontal = T)
boxplot(EMB ~ group, data=data, main="EMB", horizontal = T)
```

![](_report_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
boxplot(RBo ~ group, data=data, main="RBo", horizontal = T)
boxplot(ESBo ~ group, data=data, main="ESBo", horizontal = T)
boxplot(RSBo ~ group, data=data, main="RSBo", horizontal = T)
boxplot(EMBo ~ group, data=data, main="EMBo", horizontal = T)
```

![](_report_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
boxplot(RBo ~ group, data=data, main="RBo", horizontal = T)

boxplot(log_age ~ group, data=data, main="log age", horizontal = T)
boxplot(log_city_size ~ group, data=data, main="log city size", horizontal = T)
boxplot(income ~ group, data=data, main="income", horizontal = T)
```

![](_report_files/figure-html/unnamed-chunk-4-3.png)<!-- -->

```r
boxplot(TC ~ group, data=data, main="TC", horizontal = T)
```

![](_report_files/figure-html/unnamed-chunk-4-4.png)<!-- -->


```r
summary(data[data$group=="Control",c(29,30,32:37,39, 54:62)])
```

```
##       emo         rat         sex              age            income    
##  Min.   :0   Min.   :0   Min.   :0.0000   Min.   :15.00   Min.   :1.00  
##  1st Qu.:0   1st Qu.:0   1st Qu.:0.0000   1st Qu.:21.00   1st Qu.:1.00  
##  Median :0   Median :0   Median :0.0000   Median :24.00   Median :2.00  
##  Mean   :0   Mean   :0   Mean   :0.3945   Mean   :28.35   Mean   :2.33  
##  3rd Qu.:0   3rd Qu.:0   3rd Qu.:1.0000   3rd Qu.:35.00   3rd Qu.:3.00  
##  Max.   :0   Max.   :0   Max.   :1.0000   Max.   :67.00   Max.   :5.00  
##                                                                         
##    isworking           edu             car           city_size       
##  Min.   :0.0000   Min.   :1.000   Min.   :0.0000   Min.   :   10293  
##  1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:1.0000   1st Qu.:  952136  
##  Median :1.0000   Median :3.000   Median :1.0000   Median : 1195446  
##  Mean   :0.6389   Mean   :2.955   Mean   :0.8182   Mean   : 2904776  
##  3rd Qu.:1.0000   3rd Qu.:3.000   3rd Qu.:1.0000   3rd Qu.: 1483119  
##  Max.   :1.0000   Max.   :4.000   Max.   :1.0000   Max.   :12692466  
##  NA's   :1        NA's   :65      NA's   :65       NA's   :3         
##        RB              ESB              RSB              EMB        
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.3500   1st Qu.:0.5000   1st Qu.:0.4000   1st Qu.:0.4500  
##  Median :0.5000   Median :0.5500   Median :0.5000   Median :0.5500  
##  Mean   :0.5459   Mean   :0.5766   Mean   :0.4968   Mean   :0.5642  
##  3rd Qu.:0.8000   3rd Qu.:0.7500   3rd Qu.:0.5500   3rd Qu.:0.7500  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
##                                                                     
##        TC              RBo              ESBo             RSBo       
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.5000   1st Qu.:0.2000   1st Qu.:0.4000   1st Qu.:0.4000  
##  Median :0.8000   Median :0.3000   Median :0.5000   Median :0.5000  
##  Mean   :0.6982   Mean   :0.3431   Mean   :0.4927   Mean   :0.4991  
##  3rd Qu.:1.0000   3rd Qu.:0.5000   3rd Qu.:0.6000   3rd Qu.:0.6000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :0.9000  
##                                                                     
##       EMBo       
##  Min.   :0.0000  
##  1st Qu.:0.3000  
##  Median :0.5000  
##  Mean   :0.4147  
##  3rd Qu.:0.5000  
##  Max.   :1.0000  
## 
```

```r
head(data[data$group=="Control",c(29,30,32:37,39, 54:62)])
```

```
##    emo rat sex age income isworking edu car city_size   RB  ESB  RSB  EMB  TC
## 2    0   0   1  18      1         0  NA  NA   1195446 0.00 0.00 0.00 0.00 0.0
## 3    0   0   0  22      2         1  NA  NA   1195446 0.80 0.50 0.55 0.55 1.0
## 4    0   0   0  30      3         1  NA  NA   1195446 0.55 0.95 0.60 0.45 0.8
## 7    0   0   1  35      5         1  NA  NA   1195446 0.40 0.85 0.85 0.75 0.4
## 13   0   0   1  22      2         0  NA  NA   1195446 0.45 0.35 0.30 0.20 0.2
## 22   0   0   0  19      1         0  NA  NA   1195446 0.75 0.50 0.70 1.00 0.6
##    RBo ESBo RSBo EMBo
## 2  0.0  0.0  0.0  0.0
## 3  0.5  0.5  0.5  0.5
## 4  0.4  0.6  0.5  0.4
## 7  0.5  0.6  0.8  0.3
## 13 0.7  0.9  0.4  0.3
## 22 0.3  0.5  0.4  0.8
```

```r
nrow(data[data$group=="Control",c(29,30,32:37,39, 54:62)])
```

```
## [1] 109
```


```r
summary(data[data$group=="Emotional",c(29,30,32:37,39, 54:62)])
```

```
##       emo         rat           sex              age            income     
##  Min.   :1   Min.   : NA   Min.   :0.0000   Min.   :15.00   Min.   :1.000  
##  1st Qu.:1   1st Qu.: NA   1st Qu.:0.0000   1st Qu.:20.00   1st Qu.:1.000  
##  Median :1   Median : NA   Median :0.0000   Median :26.50   Median :2.000  
##  Mean   :1   Mean   :NaN   Mean   :0.2685   Mean   :29.29   Mean   :2.546  
##  3rd Qu.:1   3rd Qu.: NA   3rd Qu.:1.0000   3rd Qu.:36.25   3rd Qu.:3.000  
##  Max.   :1   Max.   : NA   Max.   :1.0000   Max.   :62.00   Max.   :5.000  
##              NA's   :108                                                   
##    isworking           edu             car           city_size       
##  Min.   :0.0000   Min.   :1.000   Min.   :0.0000   Min.   :    2552  
##  1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:0.0000   1st Qu.: 1013468  
##  Median :1.0000   Median :3.000   Median :1.0000   Median : 1195446  
##  Mean   :0.6822   Mean   :3.171   Mean   :0.7073   Mean   : 2746519  
##  3rd Qu.:1.0000   3rd Qu.:3.000   3rd Qu.:1.0000   3rd Qu.: 1618039  
##  Max.   :1.0000   Max.   :4.000   Max.   :1.0000   Max.   :12692466  
##  NA's   :1        NA's   :67      NA's   :67       NA's   :3         
##        RB              ESB              RSB              EMB        
##  Min.   :0.1000   Min.   :0.1500   Min.   :0.0000   Min.   :0.0500  
##  1st Qu.:0.5000   1st Qu.:0.5000   1st Qu.:0.4000   1st Qu.:0.4500  
##  Median :0.6500   Median :0.6500   Median :0.5000   Median :0.6000  
##  Mean   :0.6356   Mean   :0.6343   Mean   :0.4921   Mean   :0.5769  
##  3rd Qu.:0.8000   3rd Qu.:0.7500   3rd Qu.:0.5500   3rd Qu.:0.7500  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
##                                                                     
##        TC              RBo              ESBo            RSBo       
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.000   Min.   :0.0000  
##  1st Qu.:0.5000   1st Qu.:0.1000   1st Qu.:0.500   1st Qu.:0.4000  
##  Median :0.8000   Median :0.3000   Median :0.500   Median :0.5000  
##  Mean   :0.6963   Mean   :0.3009   Mean   :0.538   Mean   :0.4639  
##  3rd Qu.:0.9250   3rd Qu.:0.5000   3rd Qu.:0.600   3rd Qu.:0.6000  
##  Max.   :1.0000   Max.   :0.7000   Max.   :1.000   Max.   :0.9000  
##                                                                    
##       EMBo      
##  Min.   :0.000  
##  1st Qu.:0.300  
##  Median :0.450  
##  Mean   :0.437  
##  3rd Qu.:0.500  
##  Max.   :1.000  
## 
```

```r
head(data[data$group=="Emotional",c(29,30,32:37,39, 54:62)])
```

```
##    emo rat sex age income isworking edu car city_size   RB  ESB  RSB  EMB  TC
## 1    1  NA   0  34      3         1  NA  NA   1195446 0.75 0.50 0.45 0.65 0.7
## 5    1  NA   0  34      3         1  NA  NA   1195446 0.75 0.55 0.45 0.75 0.9
## 6    1  NA   0  25      2         1  NA  NA  12692466 1.00 0.15 0.45 0.70 1.0
## 12   1  NA   0  19      1         0  NA  NA   1195446 0.90 0.45 0.35 0.95 0.8
## 14   1  NA   0  24      1         0  NA  NA   1195446 0.80 0.15 0.50 0.10 0.0
## 15   1  NA   0  17      1         0  NA  NA    151275 0.90 0.65 0.50 0.80 1.0
##    RBo ESBo RSBo EMBo
## 1  0.6  0.5  0.4  0.5
## 5  0.5  0.6  0.5  0.4
## 6  0.1  0.9  0.5  0.3
## 12 0.0  1.0  0.1  0.2
## 14 0.3  0.4  0.5  0.6
## 15 0.0  0.5  0.5  0.0
```

```r
nrow(data[data$group=="Emotional",c(29,30,32:37,39, 54:62)])
```

```
## [1] 108
```



```r
summary(data[data$group=="Rational",c(29,30,32:37,39, 54:62)])
```

```
##       emo           rat         sex              age            income     
##  Min.   : NA   Min.   :1   Min.   :0.0000   Min.   :14.00   Min.   :1.000  
##  1st Qu.: NA   1st Qu.:1   1st Qu.:0.0000   1st Qu.:20.00   1st Qu.:1.000  
##  Median : NA   Median :1   Median :0.0000   Median :27.00   Median :2.000  
##  Mean   :NaN   Mean   :1   Mean   :0.3431   Mean   :29.02   Mean   :2.214  
##  3rd Qu.: NA   3rd Qu.:1   3rd Qu.:1.0000   3rd Qu.:34.00   3rd Qu.:3.000  
##  Max.   : NA   Max.   :1   Max.   :1.0000   Max.   :64.00   Max.   :5.000  
##  NA's   :103               NA's   :1                                       
##    isworking           edu             car           city_size       
##  Min.   :0.0000   Min.   :1.000   Min.   :0.0000   Min.   :   10854  
##  1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:0.0000   1st Qu.:  614367  
##  Median :1.0000   Median :3.000   Median :1.0000   Median : 1195446  
##  Mean   :0.6311   Mean   :3.077   Mean   :0.6667   Mean   : 2391960  
##  3rd Qu.:1.0000   3rd Qu.:3.000   3rd Qu.:1.0000   3rd Qu.: 1483119  
##  Max.   :1.0000   Max.   :4.000   Max.   :1.0000   Max.   :12692466  
##                   NA's   :64      NA's   :64       NA's   :11        
##        RB              ESB             RSB             EMB        
##  Min.   :0.0000   Min.   :0.050   Min.   :0.000   Min.   :0.0000  
##  1st Qu.:0.4500   1st Qu.:0.550   1st Qu.:0.450   1st Qu.:0.5000  
##  Median :0.5500   Median :0.700   Median :0.500   Median :0.6000  
##  Mean   :0.5757   Mean   :0.667   Mean   :0.499   Mean   :0.6117  
##  3rd Qu.:0.7000   3rd Qu.:0.750   3rd Qu.:0.550   3rd Qu.:0.7500  
##  Max.   :1.0000   Max.   :1.000   Max.   :1.000   Max.   :1.0000  
##                                                                   
##        TC              RBo              ESBo             RSBo       
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.1000   Min.   :0.0000  
##  1st Qu.:0.5000   1st Qu.:0.2000   1st Qu.:0.5000   1st Qu.:0.4000  
##  Median :0.8000   Median :0.3000   Median :0.5000   Median :0.5000  
##  Mean   :0.6864   Mean   :0.3146   Mean   :0.5291   Mean   :0.5165  
##  3rd Qu.:1.0000   3rd Qu.:0.4500   3rd Qu.:0.6000   3rd Qu.:0.6000  
##  Max.   :1.0000   Max.   :0.9000   Max.   :1.0000   Max.   :1.0000  
##                                                                     
##       EMBo       
##  Min.   :0.0000  
##  1st Qu.:0.3000  
##  Median :0.4000  
##  Mean   :0.4262  
##  3rd Qu.:0.5000  
##  Max.   :1.0000  
## 
```

```r
head(data[data$group=="Rational",c(29,30,32:37,39, 54:62)])
```

```
##    emo rat sex age income isworking edu car city_size   RB  ESB RSB EMB  TC RBo
## 9   NA   1   0  59      5         0  NA  NA        NA 0.50 0.50 0.5 0.5 0.0 0.0
## 18  NA   1   0  22      1         0  NA  NA   1195446 0.45 0.55 0.5 0.4 0.6 0.5
## 19  NA   1   0  20      1         0  NA  NA   1195446 0.15 0.70 0.2 0.5 0.2 0.2
## 25  NA   1   0  23      2         1  NA  NA   1195446 0.60 0.60 0.3 0.7 0.0 0.1
## 30  NA   1   0  27      3         1  NA  NA   5392992 0.70 0.50 0.5 0.5 0.9 0.2
## 38  NA   1   1  32      3         1  NA  NA   1195446 0.45 0.65 0.1 0.4 0.2 0.5
##    ESBo RSBo EMBo
## 9   0.5  0.5  0.5
## 18  0.6  0.6  0.5
## 19  0.5  0.4  0.7
## 25  0.5  0.5  0.4
## 30  0.5  0.5  0.5
## 38  0.4  0.1  0.1
```

```r
nrow(data[data$group=="Rational",c(29,30,32:37,39, 54:62)])
```

```
## [1] 103
```


```r
library(psych)
describeBy(data[,c(32:37,39,54:62)], data$group, na.rm = T)$Control[,c(2:5,8,9)]
```

```
##             n       mean         sd     median   min        max
## sex       109       0.39       0.49       0.00     0        1.0
## age       109      28.35      10.37      24.00    15       67.0
## income    109       2.33       1.24       2.00     1        5.0
## isworking 108       0.64       0.48       1.00     0        1.0
## edu        44       2.95       0.68       3.00     1        4.0
## car        44       0.82       0.39       1.00     0        1.0
## city_size 106 2904776.10 4255889.26 1195446.00 10293 12692466.0
## RB        109       0.55       0.28       0.50     0        1.0
## ESB       109       0.58       0.23       0.55     0        1.0
## RSB       109       0.50       0.19       0.50     0        1.0
## EMB       109       0.56       0.24       0.55     0        1.0
## TC        109       0.70       0.32       0.80     0        1.0
## RBo       109       0.34       0.21       0.30     0        1.0
## ESBo      109       0.49       0.19       0.50     0        1.0
## RSBo      109       0.50       0.18       0.50     0        0.9
## EMBo      109       0.41       0.21       0.50     0        1.0
```

```r
describeBy(data[,c(32:37,39,54:62)], data$group, na.rm = T)$Rational[,c(2:5,8,9)]
```

```
##             n       mean         sd     median      min        max
## sex       102       0.34       0.48       0.00     0.00        1.0
## age       103      29.02      11.42      27.00    14.00       64.0
## income    103       2.21       1.17       2.00     1.00        5.0
## isworking 103       0.63       0.48       1.00     0.00        1.0
## edu        39       3.08       0.66       3.00     1.00        4.0
## car        39       0.67       0.48       1.00     0.00        1.0
## city_size  92 2391959.93 3488321.68 1195446.00 10854.00 12692466.0
## RB        103       0.58       0.22       0.55     0.00        1.0
## ESB       103       0.67       0.17       0.70     0.05        1.0
## RSB       103       0.50       0.20       0.50     0.00        1.0
## EMB       103       0.61       0.20       0.60     0.00        1.0
## TC        103       0.69       0.33       0.80     0.00        1.0
## RBo       103       0.31       0.21       0.30     0.00        0.9
## ESBo      103       0.53       0.15       0.50     0.10        1.0
## RSBo      103       0.52       0.18       0.50     0.00        1.0
## EMBo      103       0.43       0.19       0.40     0.00        1.0
```

```r
describeBy(data[,c(32:37,39,54:62)], data$group, na.rm = T)$Emotional[,c(2:5,8,9)]
```

```
##             n       mean         sd     median     min        max
## sex       108       0.27       0.45       0.00    0.00        1.0
## age       108      29.29      10.77      26.50   15.00       62.0
## income    108       2.55       1.34       2.00    1.00        5.0
## isworking 107       0.68       0.47       1.00    0.00        1.0
## edu        41       3.17       0.59       3.00    1.00        4.0
## car        41       0.71       0.46       1.00    0.00        1.0
## city_size 105 2746518.50 3836439.26 1195446.00 2552.00 12692466.0
## RB        108       0.64       0.21       0.65    0.10        1.0
## ESB       108       0.63       0.19       0.65    0.15        1.0
## RSB       108       0.49       0.20       0.50    0.00        1.0
## EMB       108       0.58       0.21       0.60    0.05        1.0
## TC        108       0.70       0.29       0.80    0.00        1.0
## RBo       108       0.30       0.19       0.30    0.00        0.7
## ESBo      108       0.54       0.17       0.50    0.00        1.0
## RSBo      108       0.46       0.20       0.50    0.00        0.9
## EMBo      108       0.44       0.17       0.45    0.00        1.0
```

## Балансирование групп

Балансирование групп производится на основе метода PSM по основным характеристикам респондентов (пол, доход, занятость, возраст, размер города, склонность к комфорту).


Ниже представлены результаты балансировки и основные характеристики сбалансированных групп.


## Emotional vs Control



```r
emo_data <- data %>%  # MatchIt does not allow missing values
  select(id, emo, sex, income, isworking, log_age, log_city_size, TC) %>%
    na.omit()
  
match.emo <- matchit(emo ~ sex + income + isworking + log_age + log_city_size + TC, data = emo_data, method="nearest")

a <- summary(match.emo)

emo_data <- match.data(match.emo) %>% 
  select(id, distance, weights) %>% 
    left_join(data, by = c("id"))

plot(match.emo, type = 'jitter', interactive = F)
```

![](_report_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
kable(a$nn, digits = 2, align = 'c', caption = 'Table: Sample sizes')
```



Table: Table: Sample sizes

             Control    Treated 
----------  ---------  ---------
All            105        104   
Matched        104        104   
Unmatched       1          0    
Discarded       0          0    

```r
t <- c("sex","income","isworking","log_age","log_city_size","TC")
t <- lapply(t, function(v) {
  #t.test(emo_data[, v] ~ emo_data[, "rat"])
  w<-wilcox.test(emo_data[, v] ~ emo_data[, "emo"], paired = F, alternative = "two.sided")
  w$p.value
})
t<-unlist(t)

#Wilcoxon rank sum test with continuity correction
#W = 4459, p-value = 0.2775
#alternative hypothesis: true location shift is not equal to 0

a <- cbind(a$sum.matched[-1,c(1,2,4)], t)
names(a)[4]<-"p-value"


kable(a, digits = 3, align = 'c', 
      caption = 'Table: Summary of balance for matched data')
```



Table: Table: Summary of balance for matched data

                 Means Treated    Means Control    Mean Diff    p-value 
--------------  ---------------  ---------------  -----------  ---------
sex                  0.279            0.385         -0.106       0.106  
income               2.567            2.308          0.260       0.172  
isworking            0.683            0.625          0.058       0.384  
log_age              1.441            1.428          0.013       0.480  
log_city_size        6.095            6.081          0.013       0.532  
TC                   0.700            0.693          0.007       0.793  




```r
by<-factor(emo_data$emo, labels = c("Control","Emotional"))

par(mfrow = c(1,4), mar=c(2,2,2,1))
boxplot(RB ~ by, data=emo_data, main="RB", horizontal = F)
boxplot(ESB ~ by, data=emo_data, main="ESB", horizontal = F)
boxplot(RSB ~ by, data=emo_data, main="RSB", horizontal = F)
boxplot(EMB ~ by, data=emo_data, main="EMB", horizontal = F)
```

![](_report_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
boxplot(RBo ~ by, data=emo_data, main="RBo", horizontal = F)
boxplot(ESBo ~ by, data=emo_data, main="ESBo", horizontal = F)
boxplot(RSBo ~ by, data=emo_data, main="RSBo", horizontal = F)
boxplot(EMBo ~ by, data=emo_data, main="EMBo", horizontal = F)
```

![](_report_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

```r
boxplot(RBo ~ by, data=emo_data, main="RBo", horizontal = F)

boxplot(log_age ~ by, data=emo_data, main="log age", horizontal = F)
boxplot(log_city_size ~ by, data=emo_data, main="log city size", horizontal = F)
boxplot(income ~ by, data=emo_data, main="income", horizontal = F)
```

![](_report_files/figure-html/unnamed-chunk-10-3.png)<!-- -->

```r
boxplot(TC ~ by, data=emo_data, main="TC", horizontal = F)
```

![](_report_files/figure-html/unnamed-chunk-10-4.png)<!-- -->


```r
summary(emo_data[emo_data$emo==1,c(34:37,41)])
```

```
##       sex              age            income        isworking     
##  Min.   :0.0000   Min.   :15.00   Min.   :1.000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:20.00   1st Qu.:1.000   1st Qu.:0.0000  
##  Median :0.0000   Median :26.50   Median :2.000   Median :1.0000  
##  Mean   :0.2788   Mean   :29.38   Mean   :2.567   Mean   :0.6827  
##  3rd Qu.:1.0000   3rd Qu.:36.25   3rd Qu.:3.000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :62.00   Max.   :5.000   Max.   :1.0000  
##    city_size       
##  Min.   :    2552  
##  1st Qu.:  944783  
##  Median : 1195446  
##  Mean   : 2761433  
##  3rd Qu.: 1618039  
##  Max.   :12692466
```

```r
summary(emo_data[emo_data$emo==0,c(34:37,41)])
```

```
##       sex              age            income        isworking    
##  Min.   :0.0000   Min.   :15.00   Min.   :1.000   Min.   :0.000  
##  1st Qu.:0.0000   1st Qu.:21.00   1st Qu.:1.000   1st Qu.:0.000  
##  Median :0.0000   Median :24.50   Median :2.000   Median :1.000  
##  Mean   :0.3846   Mean   :28.39   Mean   :2.308   Mean   :0.625  
##  3rd Qu.:1.0000   3rd Qu.:35.00   3rd Qu.:3.000   3rd Qu.:1.000  
##  Max.   :1.0000   Max.   :67.00   Max.   :5.000   Max.   :1.000  
##    city_size       
##  Min.   :   10293  
##  1st Qu.: 1020120  
##  Median : 1195446  
##  Mean   : 2947686  
##  3rd Qu.: 1483119  
##  Max.   :12692466
```


Анализ значимости различий в средних между независимыми группами (u-тест Манна-Уитни).
p-value > 0.05 означает отсутствие значимых различий.





## Rational vs Control



```r
rat_data <- data %>%  # MatchIt does not allow missing values
  select(id, rat, sex, income, isworking, log_age, log_city_size, TC) %>%
    na.omit()
  
match.rat <- matchit(rat ~ sex + income + isworking + log_age + log_city_size + TC, data = rat_data, method="nearest")

a <- summary(match.rat)

rat_data <- match.data(match.rat) %>% 
  select(id, distance, weights) %>% 
    left_join(data, by = c("id"))

plot(match.rat, type = 'jitter', interactive = F)
```

![](_report_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
kable(a$nn, digits = 2, align = 'c', caption = 'Table: Sample sizes')
```



Table: Table: Sample sizes

             Control    Treated 
----------  ---------  ---------
All            105        91    
Matched        91         91    
Unmatched      14          0    
Discarded       0          0    

```r
t <- c("sex","income","isworking","log_age","log_city_size","TC")
t <- lapply(t, function(v) {
  #t.test(rat_data[, v] ~ rat_data[, "rat"])
  w<-wilcox.test(rat_data[, v] ~ rat_data[, "rat"], paired = F, alternative = "two.sided")
  w$p.value
})
t<-unlist(t)

#Wilcoxon rank sum test with continuity correction
#W = 4459, p-value = 0.2775
#alternative hypothesis: true location shift is not equal to 0

a <- cbind(a$sum.matched[-1,c(1,2,4)], t)
names(a)[4]<-"p-value"

kable(a, digits = 3, align = 'c', 
      caption = 'Table: Summary of balance for matched data')
```



Table: Table: Summary of balance for matched data

                 Means Treated    Means Control    Mean Diff    p-value 
--------------  ---------------  ---------------  -----------  ---------
sex                  0.308            0.385         -0.077       0.277  
income               2.165            2.264         -0.099       0.541  
isworking            0.615            0.615          0.000       1.000  
log_age              1.430            1.420          0.010       0.806  
log_city_size        6.041            6.072         -0.031       0.717  
TC                   0.697            0.682          0.014       0.689  




```r
by<-factor(rat_data$rat, labels = c("Control","Rational"))

par(mfrow = c(1,4), mar=c(2,2,2,1))
boxplot(RB ~ by, data=rat_data, main="RB", horizontal = F)
boxplot(ESB ~ by, data=rat_data, main="ESB", horizontal = F)
boxplot(RSB ~ by, data=rat_data, main="RSB", horizontal = F)
boxplot(EMB ~ by, data=rat_data, main="EMB", horizontal = F)
```

![](_report_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
boxplot(RBo ~ by, data=rat_data, main="RBo", horizontal = F)
boxplot(ESBo ~ by, data=rat_data, main="ESBo", horizontal = F)
boxplot(RSBo ~ by, data=rat_data, main="RSBo", horizontal = F)
boxplot(EMBo ~ by, data=rat_data, main="EMBo", horizontal = F)
```

![](_report_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

```r
boxplot(RBo ~ by, data=rat_data, main="RBo", horizontal = F)

boxplot(log_age ~ by, data=rat_data, main="log age", horizontal = F)
boxplot(log_city_size ~ by, data=rat_data, main="log city size", horizontal = F)
boxplot(income ~ by, data=rat_data, main="income", horizontal = F)
```

![](_report_files/figure-html/unnamed-chunk-14-3.png)<!-- -->

```r
boxplot(TC ~ by, data=rat_data, main="TC", horizontal = F)
```

![](_report_files/figure-html/unnamed-chunk-14-4.png)<!-- -->



```r
summary(rat_data[rat_data$rat==1,c(34:37,41)])
```

```
##       sex              age            income        isworking     
##  Min.   :0.0000   Min.   :14.00   Min.   :1.000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:20.00   1st Qu.:1.000   1st Qu.:0.0000  
##  Median :0.0000   Median :27.00   Median :2.000   Median :1.0000  
##  Mean   :0.3077   Mean   :28.87   Mean   :2.165   Mean   :0.6154  
##  3rd Qu.:1.0000   3rd Qu.:34.00   3rd Qu.:3.000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :64.00   Max.   :5.000   Max.   :1.0000  
##    city_size       
##  Min.   :   10854  
##  1st Qu.:  611261  
##  Median : 1195446  
##  Mean   : 2405108  
##  3rd Qu.: 1483119  
##  Max.   :12692466
```

```r
summary(rat_data[rat_data$rat==0,c(34:37,41)])
```

```
##       sex              age            income        isworking     
##  Min.   :0.0000   Min.   :15.00   Min.   :1.000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:21.00   1st Qu.:1.000   1st Qu.:0.0000  
##  Median :0.0000   Median :23.00   Median :2.000   Median :1.0000  
##  Mean   :0.3846   Mean   :27.88   Mean   :2.264   Mean   :0.6154  
##  3rd Qu.:1.0000   3rd Qu.:35.00   3rd Qu.:3.000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :67.00   Max.   :5.000   Max.   :1.0000  
##    city_size       
##  Min.   :   10293  
##  1st Qu.:  986128  
##  Median : 1195446  
##  Mean   : 2827260  
##  3rd Qu.: 1483119  
##  Max.   :12692466
```


Анализ значимости различий в средних между независимыми группами (u-тест Манна-Уитни).
p-value > 0.05 означает отсутствие значимых различий.




# Результаты


## Проверка гипотезы №1


*Гипотеза H1. Эко-поведение не однородно - склонность к одному из видов эко-поведения не предопределяет склонность к другому.*


Для исследования этой гипотезы прибегнем к методу Smallest Space Analyse (SSA). В рамках данного метода в едином пространстве размещаются признаки объекта на основе некоторой метрики близости. Относительно близкие друг к другу признаки считаются однородными. Для построения пространства будем использовать метрику d=1-r, где r - коэффициент корреляции Спирмена. Двумерная диаграмма рассеяния (рис.) на основе d показывает, что чем более скоррелированы индексы склонности к некоторым видам эко-поведения, тем ближе их метки на диаграмме и, соответственно, данные виды поведения относительно более однородны.



```r
#cmdscale(d, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE) #Multidimensional Scaling

cols <- 56:59

ctrl_emo <- cbind(
  na.omit(emo_data[emo_data$emo==0,cols]), #na.omit
  na.omit(emo_data[emo_data$emo==1,cols])
)

names(ctrl_emo)[1:4] <- sapply(names(ctrl_emo)[1:4],function(x) paste0(x,"c"))
names(ctrl_emo)[5:8] <- sapply(names(ctrl_emo)[5:8],function(x) paste0(x,"e"))

#ctrl_emo

mds_emo <- cmdscale(as.dist(apply(cor(ctrl_emo, method = "spearman"),1:2,function(x) 1-x)), k = 2)


p0<-ggplot()+labs(x="DIM 1", y="DIM 2")+xlim(-1,1)+ylim(-1,1)+theme_bw()+
  geom_text(data=as.data.frame(mds_emo), aes(x=mds_emo[,1], y=mds_emo[,2], label=rownames(mds_emo), col="Emotional"))+
  scale_colour_manual(name="",values=c("1","2"))




d<-na.omit(emo_data[emo_data$emo==1,cols])
mds_emo <- cmdscale(as.dist(apply(cor(d, method = "spearman"),1:2,function(x) 1-x)), k = 2)

d<-na.omit(emo_data[emo_data$emo==0,cols])
mds_emo_ctrl <- cmdscale(as.dist(apply(cor(d, method = "spearman"),1:2,function(x) 1-x)), k = 2)

d<-na.omit(rat_data[rat_data$rat==1,cols])
mds_rat <- cmdscale(as.dist(apply(cor(d, method = "spearman"),1:2,function(x) 1-x)), k = 2)

d<-na.omit(rat_data[rat_data$rat==0,cols])
mds_rat_ctrl <- cmdscale(as.dist(apply(cor(d, method = "spearman"),1:2,function(x) 1-x)), k = 2)

p1<-ggplot()+labs(x="DIM 1", y="DIM 2")+xlim(-1,1)+ylim(-1,1)+theme_bw()+
  geom_text(data=as.data.frame(mds_emo), aes(x=mds_emo[,1], y=mds_emo[,2], label=rownames(mds_emo), col="Emotional"))+
  geom_text(data=as.data.frame(mds_emo_ctrl), aes(x=mds_emo_ctrl[,1], y=mds_emo_ctrl[,2], label=rownames(mds_emo_ctrl), col="Control"))+
  scale_colour_manual(name="",values=c("1","2"))
  
p2<-ggplot()+labs(x="DIM 1", y="DIM 2")+xlim(-1,1)+ylim(-1,1)+theme_bw()+
  geom_text(data=as.data.frame(mds_rat), aes(x=mds_rat[,1], y=mds_rat[,2], label=rownames(mds_rat), col="Rational"))+
  geom_text(data=as.data.frame(mds_rat_ctrl), aes(x=mds_rat_ctrl[,1], y=mds_rat_ctrl[,2], label=rownames(mds_rat_ctrl), col="Control"))+
  scale_colour_manual(name="",values=c("1","2"))

p0 <- ggplot()+labs(x=NULL, y=NULL)+xlim(-0.8,0.8)+ylim(-0.8,0.8)+theme_bw()
p1 <- p0 + geom_text(data=as.data.frame(mds_emo), aes(x=mds_emo[,1], y=mds_emo[,2],
                                                      label=rownames(mds_emo)))+
      #labs(title = "Emotional / Control")+
      geom_text(aes(x=0.55, y=0.65,label="Emotional"), fontface = "bold")
p2 <- p0 + geom_text(data=as.data.frame(mds_emo_ctrl), aes(x=mds_emo_ctrl[,1], y=mds_emo_ctrl[,2],
                                                      label=rownames(mds_emo_ctrl)))+
      #labs(title = "")
      geom_text(aes(x=0.6, y=0.65,label="Control"), fontface = "bold")
p3 <- p0 + geom_text(data=as.data.frame(mds_rat), aes(x=mds_rat[,1], y=mds_rat[,2],
                                                      label=rownames(mds_rat)))+
      #labs(title = "Rational / Control") 
      geom_text(aes(x=0.6, y=0.65,label="Rational"), fontface = "bold")
p4 <- p0 + geom_text(data=as.data.frame(mds_rat_ctrl), aes(x=mds_rat_ctrl[,1], y=mds_rat_ctrl[,2],
                                                      label=rownames(mds_rat_ctrl)))+
      #labs(title = "")
      geom_text(aes(x=0.6, y=0.65,label="Control"), fontface = "bold")
g <- grid.arrange(p2, p1, p4, p3, ncol = 2, nrow = 2)
```

![](_report_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
#g<-arrangeGrob(p2, p1, p4, p3, ncol = 2, nrow = 2)

ggsave('H1.pdf', g)
```




На графиках можно заметить, что рассматриваемые виды эко-поведения достаточно далеки друг от друга, как в контрольной так и в экспериментальной группах, т.е. неоднородны. Эта неоднородность достаточно устойчива, воздействие стимулов не приводит к радикальным изменениям. Визуально относительно близкими можно было бы признать ресурсосбережение (RSB) и эко-мобильность (EMB) в контрольных группах, но корреляция между ними не высока ~ 0.33.


## Проверка гипотезы №2


*Гипотеза H2. Люди обычно переоценивают собственную склонность к эко-поведению и недооценивают склонность окружающих.*

Посмотрим в разрезе групп на графики плотности распределения самооцененной склонности (self) к различным видам эко-поведения и оцененной склонности окружающих (other). При построении графиков для наглядности применено сглаживание.



```r
dplot <- function (d, s, o, t){
  plot(density(d[,s], bw = 0.15), type = "l", col = "1", lwd = 1, bty="n", main = "",#main = paste(s,t),
       yaxt="n", ylim=c(0,2.5), xaxt="n") 
  axis(side=1,at=c(0.0,0.5,1.0))
  lines(density(d[,o], bw = 0.15), lty = 2, lwd = 1, col = "2")
  
  #legend("topright",legend = c("self", "other"), 
  #       col = c("1","2"), lwd = c(1,1), 
  #       lty = c(1,2), bty = "n")
  
 #print(paste0(s," ", o, " ", t)) #print label for wilcox data
  
 #t.test(d[,s], d[,o], paired = TRUE, alternative = "two.sided") #Критерий Стьюдента
 w<-wilcox.test(d[,s], d[,o], paired = TRUE, alternative = "greater") #Критерий Уилкоксона
 legend("topright",legend = c(s, t, ifelse(w$p.value<0.01,"p < 0.01", "p > 0.1")), bty = "n")
 #w # print wilcox output
}

#RB
cols <- c(56:59,61:64)

par(mfrow = c(4,4), mar=c(1.8,1.1,1.5,1))
d<-na.omit(rat_data[rat_data$rat==0,cols])
dplot(d,"RB","RBo","Control")
d<-na.omit(rat_data[rat_data$rat==1,cols])
dplot(d,"RB","RBo","Rational")
d<-na.omit(emo_data[emo_data$emo==0,cols])
dplot(d,"RB","RBo","Control")
d<-na.omit(emo_data[emo_data$emo==1,cols])
dplot(d,"RB","RBo","Emotional")
#ESB
#par(mfrow = c(4,4), mar=c(2,2,2,1))
d<-na.omit(rat_data[rat_data$rat==0,cols])
dplot(d,"ESB","ESBo","Control")
d<-na.omit(rat_data[rat_data$rat==1,cols])
dplot(d,"ESB","ESBo","Rational")
d<-na.omit(emo_data[emo_data$emo==0,cols])
dplot(d,"ESB","ESBo","Control")
d<-na.omit(emo_data[emo_data$emo==1,cols])
dplot(d,"ESB","ESBo","Emotional")

#RSB
#par(mfrow = c(2,2), mar=c(2,2,2,1))
d<-na.omit(rat_data[rat_data$rat==0,cols])
dplot(d,"RSB","RSBo","Control")
d<-na.omit(rat_data[rat_data$rat==1,cols])
dplot(d,"RSB","RSBo","Rational")
d<-na.omit(emo_data[emo_data$emo==0,cols])
dplot(d,"RSB","RSBo","Control")
d<-na.omit(emo_data[emo_data$emo==1,cols])
dplot(d,"RSB","RSBo","Emotional")

#EMB
#par(mfrow = c(2,2), mar=c(2,2,2,1))
d<-na.omit(rat_data[rat_data$rat==0,cols])
dplot(d,"EMB","EMBo","Control")
d<-na.omit(rat_data[rat_data$rat==1,cols])
dplot(d,"EMB","EMBo","Rational")
d<-na.omit(emo_data[emo_data$emo==0,cols])
dplot(d,"EMB","EMBo","Control")
d<-na.omit(emo_data[emo_data$emo==1,cols])
dplot(d,"EMB","EMBo","Emotional")
```

![](_report_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
dopdf('H2.pdf')
```

```
## png 
##   2
```

Пунктирной линией показана оценка окружающих, сплошной - самооценка.


## Проверка гипотезы №3

*Гипотеза H3. Склонность к тому или иному виду эко-поведения ситуативно подвержена влиянию стимулов, как эмоционального, так и рационального характера.*


Для проверки этой гипотезы определим различия между средними значениями индекса склонности в контрольной и исследуемой группах по каждому виду эко-поведения. Рассмотрим различия, как в исходных (unmatched), так и в сбалансированных (matched) выборках (рис.).


По левой шкале показан индекс склонности к соответствующему виду эко-поведения, на графике среднее значение по группам. Показано p-value для одностороннего критерия Манна-Уитни.



```r
getmeans <- function (c) {
  emo_all<-data[!is.na(data$emo),]
  rat_all<-data[!is.na(data$rat),]
  
  d<-rbind(
    cbind( mean(emo_data[emo_data$emo==0,c], na.rm = T),
          nrow(emo_data[emo_data$emo==0 & !is.na(emo_data[,c]),]),
          mean(emo_data[emo_data$emo==1,c], na.rm = T),
          nrow(emo_data[emo_data$emo==1 & !is.na(emo_data[,c]),]),
          with(emo_data, wilcox.test(as.formula(paste(c,"~ emo")), 
                                     paired = F, alternative = "less"))$p.value),
    cbind(mean(rat_data[rat_data$rat==0,c], na.rm = T),
          nrow(rat_data[rat_data$rat==0 & !is.na(rat_data[,c]),]),
          mean(rat_data[rat_data$rat==1,c], na.rm = T),
          nrow(rat_data[rat_data$rat==1 & !is.na(rat_data[,c]),]),
          with(rat_data, wilcox.test(as.formula(paste(c,"~ rat")),
                                     paired = F, alternative = "less"))$p.value),
    cbind(mean(emo_all[emo_all$emo==0,c], na.rm = T),
          nrow(emo_all[emo_all$emo==0 & !is.na(emo_all[,c]),]),
          mean(emo_all[emo_all$emo==1,c], na.rm = T),
          nrow(emo_all[emo_all$emo==1 & !is.na(emo_all[,c]),]),
          with(emo_all, wilcox.test(as.formula(paste(c,"~ emo")), 
                                     paired = F, alternative = "less"))$p.value),
    cbind(mean(rat_all[rat_all$rat==0,c], na.rm = T),
          nrow(rat_all[rat_all$rat==0 & !is.na(rat_all[,c]),]),
          mean(rat_all[rat_all$rat==1,c], na.rm = T),
          nrow(rat_all[rat_all$rat==1 & !is.na(rat_all[,c]),]),
          with(rat_all, wilcox.test(as.formula(paste(c,"~ rat")),
                                     paired = F, alternative = "less"))$p.value)
  )
  d<-cbind(d[,-5],d[,3]-d[,1],d[,5]) #reorder and add diff
  d<-data.frame(d)
  d<-cbind(c("Emotional (matched)","Rational (matched)",
             "Emotional (unmatched)", "Rational (unmatched)"),d)
  names(d) <- c("","Means Control","Obs. Control","Means Treated","Obs. Treated",
                "Means Diff","p-value")
  
  table <- kable(d, digits = 3, align = 'l', caption = paste('Table:',c,'u-тест Манна-Уитни $H_{0}: \\mu_{C} < \\mu_{T}$'))
  
  par(mfrow = c(2,2), mar=c(2,2,1.5,1))
  
  by<-factor(emo_data$emo, labels = c("Control","Emotional"))
  with(emo_data, plotmeans(as.formula(paste(c,"~ by")), main = "",#paste(c,"means, matched groups"), 
                           frame = F, mean.labels = F, connect = T, xlab = "", ylim=c(0.4,0.8)))
  par(new=F)
  text(cex=1, font=2, x=2.1, y=0.8, xpd=T, adj=0, paste0(c,"\nmatched\np = ",round(d[1,7],3)))

  by<-factor(rat_data$rat, labels = c("Control","Rational"))
  with(rat_data, plotmeans(as.formula(paste(c,"~ by")), main = "",#paste(c,"means, matched groups"), 
                           frame = F, mean.labels = F, connect = T, xlab = "", ylim=c(0.4,0.8)))
  par(new=F)
  text(cex=1, font=2, x=2.1, y=0.8, xpd=T, adj=0, paste0(c,"\nmatched\np = ",round(d[2,7],3)))
  
  
  
  by<-factor(emo_all$emo, labels = c("Control","Emotional"))
  with(emo_all, plotmeans(as.formula(paste(c,"~ by")), main = "",#paste(c,"means, matched groups"), 
                           frame = F, mean.labels = F, connect = T, xlab = "", ylim=c(0.4,0.8)))
  par(new=F)
  text(cex=1, font=2, x=2.1, y=0.8, xpd=T, adj=0, paste0(c,"\nunmatched\np = ",round(d[3,7],3)))

  by<-factor(rat_all$rat, labels = c("Control","Rational"))
  with(rat_all, plotmeans(as.formula(paste(c,"~ by")), main = "",#paste(c,"means, matched groups"), 
                           frame = F, mean.labels = F, connect = T, xlab = "", ylim=c(0.4,0.8)))
  par(new=F)
  text(cex=1, font=2, x=2.1, y=0.8, xpd=T, adj=0, paste0(c,"\nunmatched\np = ",round(d[4,7],3)))
  
  
  #with(data, plotmeans(as.formula(paste(c,"~ group")), main = "",#paste(c,"means, unmatched groups"), 
  #                         frame = F, mean.labels = F, connect=F, xlab = "", ylim=c(0.4,0.8)))
  #par(new=F)
  #text(cex=1, font=2, x=2.8, y=0.8, xpd=T, adj=0, paste0(c,"\nunmatched\np = 0.00"))
  
  table
}
getmeans("RB")
```

![](_report_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

Table: Table: RB u-тест Манна-Уитни $H_{0}: \mu_{C} < \mu_{T}$

                        Means Control   Obs. Control   Means Treated   Obs. Treated   Means Diff   p-value 
----------------------  --------------  -------------  --------------  -------------  -----------  --------
Emotional (matched)     0.544           104            0.636           104            0.092        0.005   
Rational (matched)      0.541           91             0.590           91             0.049        0.063   
Emotional (unmatched)   0.546           109            0.636           108            0.090        0.007   
Rational (unmatched)    0.546           109            0.576           103            0.030        0.162   

```r
dopdf('H3_1.pdf')
```

```
## png 
##   2
```

```r
getmeans("ESB")
```

![](_report_files/figure-html/unnamed-chunk-21-2.png)<!-- -->

Table: Table: ESB u-тест Манна-Уитни $H_{0}: \mu_{C} < \mu_{T}$

                        Means Control   Obs. Control   Means Treated   Obs. Treated   Means Diff   p-value 
----------------------  --------------  -------------  --------------  -------------  -----------  --------
Emotional (matched)     0.572           104            0.628           104            0.056        0.033   
Rational (matched)      0.566           91             0.668           91             0.102        0.001   
Emotional (unmatched)   0.577           109            0.634           108            0.058        0.027   
Rational (unmatched)    0.577           109            0.667           103            0.090        0.002   

```r
dopdf('H3_2.pdf')
```

```
## png 
##   2
```

```r
getmeans("RSB")
```

![](_report_files/figure-html/unnamed-chunk-21-3.png)<!-- -->

Table: Table: RSB u-тест Манна-Уитни $H_{0}: \mu_{C} < \mu_{T}$

                        Means Control   Obs. Control   Means Treated   Obs. Treated   Means Diff   p-value 
----------------------  --------------  -------------  --------------  -------------  -----------  --------
Emotional (matched)     0.493           104            0.498           104            0.005        0.601   
Rational (matched)      0.492           91             0.508           91             0.016        0.193   
Emotional (unmatched)   0.497           109            0.492           108            -0.005       0.751   
Rational (unmatched)    0.497           109            0.499           103            0.002        0.381   

```r
dopdf('H3_3.pdf')
```

```
## png 
##   2
```

```r
getmeans("EMB")
```

![](_report_files/figure-html/unnamed-chunk-21-4.png)<!-- -->

Table: Table: EMB u-тест Манна-Уитни $H_{0}: \mu_{C} < \mu_{T}$

                        Means Control   Obs. Control   Means Treated   Obs. Treated   Means Diff   p-value 
----------------------  --------------  -------------  --------------  -------------  -----------  --------
Emotional (matched)     0.560           104            0.575           104            0.015        0.274   
Rational (matched)      0.575           91             0.602           91             0.027        0.271   
Emotional (unmatched)   0.564           109            0.577           108            0.013        0.287   
Rational (unmatched)    0.564           109            0.612           103            0.047        0.096   

```r
dopdf('H3_4.pdf')
```

```
## png 
##   2
```


*Гипотеза H3а. Склонность к более затратным видам поведения менее подвержена ситуативному влиянию стимулов (low-cost hypothesis).*


## Анализ роли отдельных признаков


Для этой цели построим регрессионную модель, учитывающую все интересующие признаки и контрольные переменные. Наша базовая спецификация модели выглядит следующим образом:

$Y = \beta_{0} + \beta_{1}s + \gamma X + \varepsilon$,


где $Y$ - зависимая переменная, отражающая индивидуалную оценку склонности к тому или иному виду поведения (RB, ESB, RSB, EMB) в зависимости от модели;


$s$ - бинарная независимая переменная пренадлежности к экспериментальной $(s=1)$ или контрольной группе $(s=0)$, отражающая, соответствено, эмоциональный или рациональный стимул в зависимости от модели;


$X$ - вектор контрольных переменных (пол, возраст и др.).


Далее представлены сводные результаты на основе простой линейной регрессии для каждого стимула. Показано, что в рамках склонности к тому или иному виду поведения ситуативные стимулы влияют по разному, детерминируя вклад тех или иных индивидуальных характеристик респондентов.


```r
library(lmtest)
library(sandwich)
library(DescTools)
library(car)
library(caret)
```



```r
#Функции агрегации информации о модели
fit.summary <- function (x) { #Сводная информация и базовые тесты для lm + графики
  print(summary(x)) #F-stat p < 0.05 -> good model
  #qqnorm(x$residuals)
  #qqline(x$residuals, col = 2)
  print(shapiro.test(x$residuals)) #p > 0.05 -> normal distrib
  print(bptest(x)) # studentized Breusch-Pagan test if p<0.05 -> heteroskedacity is present
  #coeftest(x) #test significance
  print(coeftest(x, vcov = vcovHC(x, "HC1")))  #robust like in Stata
  print(PseudoR2(x, which = "McFaddenAdj"))
  plot(x)
  avPlots(x)
  }

fit.plots <- function (x, v) {
  avPlots(x, as.formula(v), id=F)
  plot(x)
  }

fit.bootstrap <- function (x, r, ...) { #бутстрап
  set.seed(1313)
  mboot <- Boot(x, R = r, ...) # bootstrap samples--too small to be useful
  cbind(summary(mboot),confint(mboot))
  }

fit.coeff <- function (x, boot = F, r = 1000) { #вывод коэффициентов + бутстрап
  df<-cbind(summary(x)$coefficients[,-3], 
            coeftest(x, vcov = vcovHC(x, "HC1"))[,c(-1,-3)])  #robust like in Stata
  df<-data.frame(df)
  names(df) <- c("Estimate","SE","pvalue","Robust.SE","Robust.pvalue")
  if (boot) {
    df<-cbind(df, 
              fit.bootstrap(x, r)[,c(-1,-2)])
  }
  write.table(df, file = paste0('models\\',deparse(substitute(x)),'_coef.csv'), sep=";", dec=",")
  df}

fit.params <- function (x) { #вывод базовых свойств и некоторых тестов
  rname <-deparse(substitute(x))
  s<-summary(x) #F-stat p < 0.05 -> good model
  t<-shapiro.test(x$residuals) #p > 0.05 -> normal distrib
  l<-bptest(x) # studentized Breusch-Pagan test if p<0.05 -> heteroskedacity is present
  f<-PseudoR2(x, which = "McFaddenAdj")
  c<-coeftest(x, vcov = vcovHC(x, "HC1"))  #robust like in Stata
  df<-data.frame(type = as.character(s[["call"]][[1]]),
             src = as.character(s[["call"]][["data"]]),
             group = strsplit(as.character(s[["terms"]][1])," ~ ")[[3]],
             r.squared = ifelse(!is.null(s$r.squared),s$r.squared,NA), 
             adj.r.squared = ifelse(!is.null(s$adj.r.squared),s$adj.r.squared,NA), 
             #adj.r.pseudo = f,
             shapiro.p.value = t$p.value,
             bp.p.value = l$p.value,
             f.p.value = ifelse(!is.null(s[["fstatistic"]]),
                            pf(s[["fstatistic"]][["value"]], 
                              s[["fstatistic"]][["numdf"]], 
                              s[["fstatistic"]][["dendf"]], lower.tail = F), NA))#,
             #aic = ifelse(!is.null(s[["aic"]]),s[["aic"]],NA))
  if (is.null(rname))
    row.names(df)<-s[["terms"]][[2]]
      else row.names(df)<-rname
  df}
```







```r
emo_data$month <- factor(emo_data$month)
dummy <- dummyVars( ~ month, data = emo_data)
df<-predict(dummy, newdata=emo_data)
emo_data<-cbind(emo_data,df)

rat_data$month <- factor(rat_data$month)
dummy <- dummyVars( ~ month, data = rat_data)
df<-predict(dummy, newdata=rat_data)
rat_data<-cbind(rat_data,df)
```



```r
#models  RB
emo_all<-data[!is.na(data$emo),]
emo_all<-na.omit(emo_all[,c(29,32:35,45:58)])

emo_all$month <- factor(emo_all$month)
dummy <- dummyVars( ~ month, data = emo_all)
df<-predict(dummy, newdata=emo_all)
emo_all<-cbind(emo_all,df)

#whole data lm
RB_emo_m0 <- lm(RB ~ emo+sex+income+isworking+log_age+log_city_size+TC+log_time_RB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12, 
         data = emo_all, na.action=na.exclude)
#matched data lm
RB_emo_m1 <- lm(RB ~ emo+sex+income+isworking+log_age+log_city_size+TC+log_time_RB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12, 
         data = emo_data, na.action=na.exclude)
#matched data lm no controls
RB_emo_m2 <- lm(RB ~ emo, data = emo_data, na.action=na.exclude)

rat_all<-data[!is.na(data$rat),]
rat_all<-na.omit(rat_all[,c(30,32:35,45:58)])


rat_all$month <- factor(rat_all$month)
dummy <- dummyVars( ~ month, data = rat_all)
df<-predict(dummy, newdata=rat_all)
rat_all<-cbind(rat_all,df)


#whole data lm
RB_rat_m0 <- lm(RB ~ rat+sex+income+isworking+log_age+log_city_size+TC+log_time_RB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12, 
         data = rat_all, na.action=na.exclude)
#matched data lm
RB_rat_m1 <- lm(RB ~ rat+sex+income+isworking+log_age+log_city_size+TC+log_time_RB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12, 
         data = rat_data, na.action=na.exclude)
#matched data lm no controls
RB_rat_m2 <- lm(RB ~ rat, data = rat_data, na.action=na.exclude)
```


```r
#models  ESB
emo_all<-data[!is.na(data$emo),]
emo_all<-na.omit(emo_all[,c(29,32:35,45:58)])

emo_all$month <- factor(emo_all$month)
dummy <- dummyVars( ~ month, data = emo_all)
df<-predict(dummy, newdata=emo_all)
emo_all<-cbind(emo_all,df)

ESB_emo_m0 <- lm(ESB ~ emo+sex+income+isworking+log_age+log_city_size+TC+log_time_ESB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12, 
         data = emo_all, na.action=na.exclude)
#matched data lm
ESB_emo_m1 <- lm(ESB ~ emo+sex+income+isworking+log_age+log_city_size+TC+log_time_ESB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12, 
         data = emo_data, na.action=na.exclude)
ESB_emo_m2 <- lm(ESB ~ emo, data = emo_data, na.action=na.exclude)

rat_all<-data[!is.na(data$rat),]
rat_all<-na.omit(rat_all[,c(30,32:35,45:58)])


rat_all$month <- factor(rat_all$month)
dummy <- dummyVars( ~ month, data = rat_all)
df<-predict(dummy, newdata=rat_all)
rat_all<-cbind(rat_all,df)


ESB_rat_m0 <- lm(ESB ~ rat+sex+income+isworking+log_age+log_city_size+TC+log_time_ESB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12, 
         data = rat_all, na.action=na.exclude)
#matched data lm
ESB_rat_m1 <- lm(ESB ~ rat+sex+income+isworking+log_age+log_city_size+TC+log_time_ESB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12, 
         data = rat_data, na.action=na.exclude)
ESB_rat_m2 <- lm(ESB ~ rat, data = rat_data, na.action=na.exclude)
```



```r
#models  RSB
emo_all<-data[!is.na(data$emo),]
emo_all<-na.omit(emo_all[,c(29,32:35,45:58)])


emo_all$month <- factor(emo_all$month)
dummy <- dummyVars( ~ month, data = emo_all)
df<-predict(dummy, newdata=emo_all)
emo_all<-cbind(emo_all,df)

#whole data lm
RSB_emo_m0 <- lm(RSB ~ emo+sex+income+isworking+log_age+log_city_size+TC+log_time_RSB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12,  
         data = emo_all, na.action=na.exclude)
#matched data lm
RSB_emo_m1 <- lm(RSB ~ emo+sex+income+isworking+log_age+log_city_size+TC+log_time_RSB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12,  
         data = emo_data, na.action=na.exclude)
RSB_emo_m2 <- lm(RSB ~ emo, data = emo_data, na.action=na.exclude)

rat_all<-data[!is.na(data$rat),]
rat_all<-na.omit(rat_all[,c(30,32:35,45:58)])

rat_all$month <- factor(rat_all$month)
dummy <- dummyVars( ~ month, data = rat_all)
df<-predict(dummy, newdata=rat_all)
rat_all<-cbind(rat_all,df)

#whole data lm
RSB_rat_m0 <- lm(RSB ~ rat+sex+income+isworking+log_age+log_city_size+TC+log_time_RSB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12,  
         data = rat_all, na.action=na.exclude)
#matched data lm
RSB_rat_m1 <- lm(RSB ~ rat+sex+income+isworking+log_age+log_city_size+TC+log_time_RSB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12,  
         data = rat_data, na.action=na.exclude)
RSB_rat_m2 <- lm(RSB ~ rat, data = rat_data, na.action=na.exclude)
```



```r
#models  EMB
emo_all<-data[!is.na(data$emo),]
emo_all<-na.omit(emo_all[,c(29,32:35,45:58)])

emo_all$month <- factor(emo_all$month)
dummy <- dummyVars( ~ month, data = emo_all)
df<-predict(dummy, newdata=emo_all)
emo_all<-cbind(emo_all,df)

#whole data lm
EMB_emo_m0 <- lm(EMB ~ emo+sex+income+isworking+log_age+log_city_size+TC+log_time_EMB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12,  
         data = emo_all, na.action=na.exclude)
#matched data lm
EMB_emo_m1 <- lm(EMB ~ emo+sex+income+isworking+log_age+log_city_size+TC+log_time_EMB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12,  
         data = emo_data, na.action=na.exclude)
EMB_emo_m2 <- lm(EMB ~ emo, data = emo_data, na.action=na.exclude)


rat_all<-data[!is.na(data$rat),]
rat_all<-na.omit(rat_all[,c(30,32:35,45:58)])

rat_all$month <- factor(rat_all$month)
dummy <- dummyVars( ~ month, data = rat_all)
df<-predict(dummy, newdata=rat_all)
rat_all<-cbind(rat_all,df)

#whole data lm
EMB_rat_m0 <- lm(EMB ~ rat+sex+income+isworking+log_age+log_city_size+TC+log_time_EMB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12,  
         data = rat_all, na.action=na.exclude)
#matched data lm
EMB_rat_m1 <- lm(EMB ~ rat+sex+income+isworking+log_age+log_city_size+TC+log_time_EMB+log_time_TC+
  +month.1 +month.2 +month.3 +month.4 +month.6 +month.7 +month.8 +month.9 +month.10 +month.11+month.12,  
         data = rat_data, na.action=na.exclude)
EMB_rat_m2 <- lm(EMB ~ rat, data = rat_data, na.action=na.exclude)
```


## Приложение



```r
#RB
fit.coeff(RB_emo_m0, boot = T)
```

```
##                   Estimate         SE      pvalue  Robust.SE Robust.pvalue
## (Intercept)    0.497066325 0.53235206 0.351641080 0.52051547  3.408227e-01
## emo            0.087720637 0.03344211 0.009425562 0.03380393  1.020176e-02
## sex           -0.030540502 0.03764273 0.418199374 0.03886556  4.329695e-01
## income        -0.047704007 0.01749416 0.006996065 0.01811843  9.166349e-03
## isworking      0.056256932 0.04500055 0.212794564 0.04395180  2.021249e-01
## log_age       -0.130356818 0.15504974 0.401554912 0.17530814  4.580499e-01
## log_city_size  0.057135414 0.02836163 0.045371086 0.02991349  5.764461e-02
## TC             0.145799949 0.05636090 0.010435670 0.06497692  2.600077e-02
## log_time_RB   -0.034335467 0.06094702 0.573853558 0.05306985  5.184258e-01
## log_time_TC    0.018898193 0.06174084 0.759873789 0.07324354  7.966733e-01
## month.1       -0.071052053 0.24025201 0.767753556 0.06074702  2.436194e-01
## month.2       -0.442218381 0.33333909 0.186231056 0.04649958  9.002827e-18
## month.3       -0.057031836 0.24027423 0.812633502 0.05349139  2.876991e-01
## month.4        0.006694217 0.27620825 0.980689834 0.12060463  9.557944e-01
## month.6        0.066139582 0.24609585 0.788411076 0.07756373  3.948979e-01
## month.7        0.072251596 0.24318865 0.766715664 0.05704280  2.068496e-01
## month.8        0.064632949 0.24541419 0.792557918 0.05757866  2.630672e-01
## month.9       -0.195945786 0.24256555 0.420218081 0.06888420  4.937232e-03
## month.10      -0.069062236 0.24120226 0.774942824 0.06164763  2.640186e-01
## month.11      -0.172426750 0.26316328 0.513130368 0.13139271  1.910102e-01
##                    bootBias     bootSE       bootMed       2.5 %      97.5 %
## (Intercept)    3.649447e-02 0.55625162  0.5539807969 -0.77128407  1.51989147
## emo           -1.492403e-03 0.03470395  0.0860343411  0.01934960  0.15580525
## sex            6.778089e-03 0.03953841 -0.0272704899 -0.10231230  0.05139121
## income        -2.958719e-05 0.01820281 -0.0472241679 -0.08612921 -0.01623385
## isworking     -5.014627e-03 0.04593885  0.0527850472 -0.04160157  0.14186982
## log_age       -6.739320e-04 0.17529107 -0.1311960757 -0.45774399  0.20964896
## log_city_size -1.111876e-03 0.03068682  0.0576587709 -0.00387525  0.11301691
## TC             2.047331e-03 0.06515511  0.1485368890  0.02151457  0.28042922
## log_time_RB    3.691607e-03 0.06128512 -0.0329864994 -0.14196831  0.10821435
## log_time_TC   -9.018098e-03 0.07945436  0.0094334605 -0.12213583  0.18816408
## month.1       -5.396975e-03 0.06407719 -0.0752897865 -0.19519057  0.05617239
## month.2        3.653397e-03 0.04941133 -0.4363613648 -0.56225289 -0.35846411
## month.3       -5.041641e-03 0.05736288 -0.0653905153 -0.15541074  0.07234285
## month.4       -1.109464e-02 0.13983100  0.0003680697 -0.29742443  0.26878923
## month.6       -2.303735e-05 0.07804256  0.0672952512 -0.09459953  0.21758199
## month.7       -4.134783e-03 0.06408520  0.0677928094 -0.05828051  0.20177854
## month.8       -2.570478e-03 0.05820089  0.0625191057 -0.05491502  0.19041337
## month.9       -8.394356e-03 0.07200573 -0.2049685632 -0.32641513 -0.01556124
## month.10      -4.499996e-03 0.06405231 -0.0756305424 -0.19330655  0.05826317
## month.11       2.913518e-03 0.14418348 -0.1725443754 -0.43367504  0.15506633
```

```r
fit.plots(RB_emo_m0, "~ emo")
```

![](_report_files/figure-html/unnamed-chunk-26-1.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-2.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-3.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-4.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-5.png)<!-- -->

```r
fit.coeff(RB_rat_m0, boot = T)
```

```
##                   Estimate         SE     pvalue  Robust.SE Robust.pvalue
## (Intercept)    0.569886142 0.58771407 0.33354385 0.48880453   0.245241540
## rat            0.038742024 0.03698480 0.29629989 0.03656070   0.290750034
## sex           -0.076339826 0.04090638 0.06367597 0.04343728   0.080575904
## income        -0.046783587 0.01929257 0.01632118 0.01930354   0.016381205
## isworking      0.055989642 0.04775468 0.24260376 0.04361001   0.200874558
## log_age        0.012342330 0.16400196 0.94009554 0.17815035   0.944845064
## log_city_size  0.058198942 0.03136318 0.06517633 0.02976111   0.052103048
## TC             0.099564836 0.05855969 0.09085439 0.06334315   0.117785339
## log_time_RB   -0.079882964 0.06664674 0.23229383 0.06071333   0.189972540
## log_time_TC    0.036107417 0.07202574 0.61677753 0.07758459   0.642224561
## month.1       -0.155667093 0.25854596 0.54789126 0.05788132   0.007846153
## month.2       -0.008103131 0.36222571 0.98217784 0.06477385   0.900588111
## month.3       -0.105748097 0.25847202 0.68294329 0.06466033   0.103744002
## month.4       -0.011027701 0.29440153 0.97016226 0.07598934   0.884780860
## month.6       -0.103181232 0.26447953 0.69691263 0.09262108   0.266791979
## month.7       -0.111854156 0.26431142 0.67267273 0.08317184   0.180402180
## month.8       -0.215498611 0.27016553 0.42614617 0.08894763   0.016416878
## month.9       -0.268979140 0.26401166 0.30968906 0.09218418   0.003984621
## month.10      -0.125659964 0.25811846 0.62698411 0.06066993   0.039798607
## month.11      -0.191560044 0.27196478 0.48214200 0.09122674   0.037170051
##                    bootBias     bootSE      bootMed       2.5 %       97.5 %
## (Intercept)    0.0248026950 0.54456789  0.573187004 -0.40399357  1.788659815
## rat           -0.0024271220 0.03747732  0.039084751 -0.04617172  0.105459253
## sex           -0.0005282826 0.04504543 -0.078557340 -0.16816856  0.009487591
## income        -0.0002996443 0.01973050 -0.047013180 -0.08634268 -0.007655166
## isworking      0.0034123835 0.04640053  0.063948279 -0.04015638  0.134993008
## log_age        0.0053070717 0.18295429  0.024111088 -0.40429911  0.352347279
## log_city_size -0.0035502366 0.03183898  0.053798856 -0.00183382  0.124765341
## TC            -0.0025788777 0.06142340  0.099706226 -0.03153885  0.220990835
## log_time_RB    0.0068447985 0.06840767 -0.069890863 -0.26227418  0.038517049
## log_time_TC   -0.0088835680 0.08024840  0.033438737 -0.14008804  0.183772564
## month.1       -0.0029893799 0.05868805 -0.157567839 -0.25582530 -0.034022269
## month.2        0.0061015087 0.06923279 -0.004205301 -0.13966333  0.130727564
## month.3        0.0004154134 0.06532554 -0.105752016 -0.22976185  0.025888142
## month.4       -0.0005319742 0.09047037 -0.021174670 -0.14601174  0.264602937
## month.6        0.0052240083 0.09918443 -0.101454239 -0.33365641  0.076492331
## month.7       -0.0069283551 0.08367696 -0.112858464 -0.28951046  0.035671942
## month.8        0.0020774703 0.09108302 -0.210211052 -0.40529946 -0.047746296
## month.9       -0.0108367553 0.10307213 -0.279367010 -0.46363007 -0.070866688
## month.10      -0.0032239630 0.06043017 -0.124640970 -0.24747715 -0.020787096
## month.11      -0.0041916547 0.10478750 -0.191092700 -0.40820475  0.018962726
```

```r
fit.plots(RB_rat_m0, "~ rat")
```

![](_report_files/figure-html/unnamed-chunk-26-6.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-7.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-8.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-9.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-10.png)<!-- -->

```r
fit.coeff(RB_emo_m1, boot = T)
```

```
##                   Estimate         SE      pvalue  Robust.SE Robust.pvalue
## (Intercept)    0.504698239 0.53115241 0.343233212 0.51920212  3.322672e-01
## emo            0.084509365 0.03344720 0.012340125 0.03392347  1.359831e-02
## sex           -0.034999835 0.03769672 0.354360407 0.03882735  3.685173e-01
## income        -0.043630768 0.01770537 0.014627664 0.01817259  1.732916e-02
## isworking      0.053558733 0.04493988 0.234847348 0.04397497  2.247743e-01
## log_age       -0.139531910 0.15483683 0.368659142 0.17593214  4.287189e-01
## log_city_size  0.056385396 0.02830146 0.047783645 0.02979476  5.996643e-02
## TC             0.148106098 0.05625601 0.009174917 0.06479549  2.338466e-02
## log_time_RB   -0.029466640 0.06091017 0.629110613 0.05290047  5.781761e-01
## log_time_TC    0.014974102 0.06166493 0.808401728 0.07325667  8.382574e-01
## month.1       -0.073783401 0.23970571 0.758568965 0.06049398  2.241135e-01
## month.2       -0.442080124 0.33256963 0.185364497 0.04656842  1.040924e-17
## month.3       -0.051425377 0.23975453 0.830396048 0.05360888  3.386549e-01
## month.4        0.002255376 0.27558971 0.993479010 0.12346416  9.854449e-01
## month.6        0.062733746 0.24554035 0.798621958 0.07740710  4.187125e-01
## month.7        0.068387871 0.24264368 0.778372485 0.05715794  2.330192e-01
## month.8        0.062509351 0.24485258 0.798775250 0.05763761  2.795215e-01
## month.9       -0.196481607 0.24200593 0.417882919 0.06877107  4.757771e-03
## month.10      -0.070359605 0.24064733 0.770321253 0.06172385  2.557742e-01
## month.11      -0.173311537 0.26255659 0.510002616 0.13175943  1.899897e-01
##                    bootBias     bootSE     bootMed        2.5 %       97.5 %
## (Intercept)    0.0008136587 0.54921507  0.49673932 -0.544842913  1.597943816
## emo           -0.0020470621 0.03667278  0.08021199  0.016328021  0.169342461
## sex            0.0039061579 0.03899332 -0.03443692 -0.107665229  0.040563826
## income         0.0002095497 0.01819404 -0.04307878 -0.078909071 -0.006452717
## isworking     -0.0038443071 0.04502153  0.05182808 -0.040310798  0.136724540
## log_age        0.0144656205 0.17306392 -0.12220992 -0.509670880  0.174338928
## log_city_size -0.0005853347 0.03100371  0.05626824 -0.004097303  0.114382555
## TC             0.0044328378 0.06121475  0.15746871 -0.009999976  0.242646378
## log_time_RB    0.0049288881 0.05777382 -0.02538247 -0.144280833  0.089151832
## log_time_TC   -0.0089642832 0.07923968  0.01656477 -0.160849640  0.139770086
## month.1       -0.0007257869 0.05712054 -0.07434811 -0.188445824  0.039707059
## month.2        0.0050456164 0.04768491 -0.43857805 -0.526820872 -0.351914474
## month.3       -0.0037289347 0.05082529 -0.05212212 -0.156857016  0.041845481
## month.4       -0.0062536978 0.14597766 -0.01110641 -0.252947819  0.317406399
## month.6        0.0047547046 0.08047900  0.07042881 -0.119429794  0.190857491
## month.7       -0.0051951969 0.05910282  0.06505835 -0.049287245  0.197010535
## month.8       -0.0023999286 0.05951984  0.05645713 -0.051851533  0.186015478
## month.9       -0.0002448515 0.07201880 -0.20075912 -0.327355671 -0.036948562
## month.10       0.0003524571 0.06011898 -0.06692520 -0.210708943  0.040081050
## month.11       0.0036826505 0.15007253 -0.17142592 -0.441346779  0.146668896
```

```r
fit.plots(RB_emo_m1, "~ emo")
```

![](_report_files/figure-html/unnamed-chunk-26-11.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-12.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-13.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-14.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-15.png)<!-- -->

```r
fit.coeff(RB_rat_m1, boot = T)
```

```
##                  Estimate         SE     pvalue  Robust.SE Robust.pvalue
## (Intercept)    0.76817640 0.60524132 0.20618773 0.51467739   0.137502648
## rat            0.03958633 0.03806259 0.29987459 0.03816201   0.301131236
## sex           -0.06567463 0.04194895 0.11939723 0.04415858   0.138893894
## income        -0.03915399 0.02189257 0.07557061 0.02136898   0.068745612
## isworking      0.05593282 0.05049239 0.26961286 0.04672421   0.233022424
## log_age       -0.05970395 0.17179635 0.72864621 0.17933991   0.739633083
## log_city_size  0.05374509 0.03341016 0.10964099 0.03263627   0.101539643
## TC             0.09129415 0.06023920 0.13158771 0.06681884   0.173740322
## log_time_RB   -0.08289121 0.06796808 0.22440402 0.06330757   0.192272822
## log_time_TC    0.02142333 0.07662130 0.78014098 0.08428547   0.799682085
## month.1       -0.16256718 0.25917923 0.53138598 0.05933470   0.006836502
## month.2       -0.01658501 0.36274436 0.96358898 0.06642950   0.803163961
## month.3       -0.11351959 0.25919207 0.66198797 0.06721597   0.093166593
## month.4       -0.01113649 0.29460321 0.96989237 0.07755930   0.886005274
## month.6       -0.09931323 0.26502547 0.70835072 0.09594496   0.302161981
## month.7       -0.11837668 0.26550452 0.65629686 0.08729717   0.176980007
## month.8       -0.22339018 0.27037290 0.40988887 0.09036838   0.014469830
## month.9       -0.26933018 0.26571048 0.31227431 0.10078230   0.008301272
## month.10      -0.10979693 0.25844407 0.67151829 0.05877907   0.063573806
## month.11      -0.18596921 0.27241639 0.49579340 0.09153909   0.043830137
##                    bootBias     bootSE     bootMed        2.5 %      97.5 %
## (Intercept)    3.024398e-02 0.53033528  0.77911156 -0.255242313  1.75110218
## rat           -1.960154e-03 0.03606270  0.03860716 -0.031376043  0.10810851
## sex            5.470743e-03 0.04550204 -0.06043757 -0.172997869  0.01277135
## income        -2.045642e-03 0.02227768 -0.04127930 -0.079257025  0.01198778
## isworking      3.006444e-03 0.04909686  0.05966057 -0.063342034  0.14755527
## log_age        1.372648e-02 0.17261856 -0.05218481 -0.381240354  0.29408061
## log_city_size -5.141236e-03 0.03204407  0.05026570 -0.004020441  0.12281718
## TC             6.562242e-03 0.06732662  0.10096161 -0.073571361  0.20775122
## log_time_RB    6.129427e-05 0.07173980 -0.08361709 -0.228562508  0.05291870
## log_time_TC   -4.064432e-03 0.08600482  0.01498128 -0.141440605  0.18132024
## month.1       -2.809836e-03 0.06129911 -0.16757128 -0.273470755 -0.02462674
## month.2        1.665508e-03 0.06853420 -0.01869660 -0.143378973  0.11695446
## month.3       -2.563859e-03 0.07041575 -0.11682848 -0.243572705  0.02212843
## month.4       -8.261670e-04 0.08827482 -0.01788576 -0.154496538  0.22941782
## month.6        1.799732e-05 0.10420415 -0.10004156 -0.297932126  0.10194883
## month.7       -3.985268e-03 0.08370164 -0.12162708 -0.278550111  0.05565766
## month.8        5.415984e-03 0.09541371 -0.21310769 -0.422217667 -0.05623821
## month.9       -5.692161e-03 0.10595158 -0.27185950 -0.485681586 -0.06144654
## month.10      -3.457850e-03 0.06479684 -0.11353680 -0.228087496  0.02187533
## month.11      -4.224199e-03 0.10775918 -0.19192213 -0.374751562  0.05779791
```

```r
fit.plots(RB_rat_m1, "~ rat")
```

![](_report_files/figure-html/unnamed-chunk-26-16.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-17.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-18.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-19.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-20.png)<!-- -->

```r
fit.coeff(RB_emo_m2, boot = T)
```

```
##               Estimate         SE       pvalue  Robust.SE Robust.pvalue
## (Intercept) 0.54375000 0.02370311 1.213711e-58 0.02655443  1.450389e-51
## emo         0.09182692 0.03352126 6.695645e-03 0.03352126  6.695645e-03
##                  bootBias     bootSE   bootMed      2.5 %    97.5 %
## (Intercept) -0.0004480696 0.02673077 0.5441175 0.48958098 0.5949260
## emo          0.0011145043 0.03428475 0.0940604 0.01987816 0.1557074
```

```r
fit.plots(RB_emo_m2, "~ emo")
```

![](_report_files/figure-html/unnamed-chunk-26-21.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-22.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-23.png)<!-- -->

```
## hat values (leverages) are all = 0.009615385
##  and there are no factor predictors; no plot no. 5
```

![](_report_files/figure-html/unnamed-chunk-26-24.png)<!-- -->

```r
fit.coeff(RB_rat_m2, boot = T)
```

```
##              Estimate         SE       pvalue  Robust.SE Robust.pvalue
## (Intercept) 0.5406593 0.02619797 2.555018e-49 0.02808404  1.433468e-45
## rat         0.0489011 0.03704952 1.885496e-01 0.03704952  1.885496e-01
##                  bootBias     bootSE    bootMed       2.5 %    97.5 %
## (Intercept)  8.115758e-04 0.02808542 0.54070004  0.48577714 0.5964788
## rat         -2.422702e-05 0.03611234 0.04811704 -0.01932489 0.1198964
```

```r
fit.plots(RB_rat_m2, "~ rat")
```

![](_report_files/figure-html/unnamed-chunk-26-25.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-26.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-27.png)<!-- -->

```
## hat values (leverages) are all = 0.01098901
##  and there are no factor predictors; no plot no. 5
```

![](_report_files/figure-html/unnamed-chunk-26-28.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-26-29.png)<!-- -->



```r
#ESB
fit.coeff(ESB_emo_m0, boot = T)
```

```
##                  Estimate         SE     pvalue  Robust.SE Robust.pvalue
## (Intercept)    0.29697563 0.47737313 0.53462338 0.51459486  5.645548e-01
## emo            0.06861565 0.03056519 0.02593381 0.03101581  2.814524e-02
## sex           -0.02162330 0.03404759 0.52613741 0.03581899  5.467784e-01
## income         0.01266986 0.01586614 0.42555528 0.01612736  4.330783e-01
## isworking     -0.03600956 0.04088207 0.37953638 0.03829159  3.482107e-01
## log_age        0.08969994 0.14330903 0.53212278 0.12459022  4.724392e-01
## log_city_size -0.03925533 0.02568794 0.12814413 0.02796928  1.621039e-01
## TC             0.09328203 0.05103309 0.06914526 0.05610761  9.805894e-02
## log_time_ESB  -0.05568663 0.06373407 0.38337189 0.07491348  4.581953e-01
## log_time_TC    0.08727121 0.05676336 0.12585394 0.05187949  9.418322e-02
## month.1        0.22350440 0.21755633 0.30557321 0.04752092  4.929491e-06
## month.2        0.30705357 0.30188458 0.31039547 0.03751318  3.938637e-14
## month.3        0.22941792 0.21749350 0.29285122 0.04803764  3.580576e-06
## month.4        0.22004158 0.24999436 0.37987628 0.15235218  1.503115e-01
## month.6        0.15486328 0.22257053 0.48741242 0.05675361  6.958617e-03
## month.7        0.12294302 0.22010091 0.57711250 0.06159631  4.737545e-02
## month.8        0.12162158 0.22224949 0.58486634 0.07440392  1.037948e-01
## month.9        0.12650122 0.21920490 0.56456461 0.05996501  3.621062e-02
## month.10       0.19478225 0.21842801 0.37366309 0.05447695  4.438595e-04
## month.11       0.01335538 0.23911408 0.95551745 0.11737456  9.095296e-01
##                    bootBias     bootSE     bootMed        2.5 %     97.5 %
## (Intercept)    0.1031941910 0.52700513  0.38181349 -0.791284943 1.30012017
## emo            0.0041363575 0.03195694  0.07262604  0.001713258 0.12694462
## sex            0.0015689823 0.03612187 -0.02334842 -0.085936418 0.05188540
## income         0.0007373683 0.01573914  0.01352331 -0.021737247 0.04315791
## isworking     -0.0013833394 0.03870627 -0.03766256 -0.115402196 0.03534106
## log_age       -0.0147369299 0.12901638  0.07690115 -0.137219618 0.33813860
## log_city_size -0.0025002416 0.02953014 -0.04043778 -0.095504031 0.01500056
## TC            -0.0021669773 0.05208511  0.08790783  0.011743851 0.20502793
## log_time_ESB  -0.0088139539 0.07529924 -0.06680127 -0.186298184 0.11134759
## log_time_TC   -0.0050408011 0.05668716  0.08306569 -0.019686600 0.20987733
## month.1       -0.0044593877 0.04710552  0.22062625  0.120166360 0.31924248
## month.2        0.0011112924 0.03997141  0.31039440  0.225126451 0.38375217
## month.3       -0.0007743204 0.04503061  0.22736022  0.138910082 0.33003898
## month.4       -0.0072810413 0.17456777  0.22321047 -0.172061689 0.50511043
## month.6        0.0014212831 0.05866949  0.16141039  0.022314652 0.24941665
## month.7       -0.0032787322 0.06319777  0.11806124  0.005680676 0.25858958
## month.8       -0.0087977286 0.07342721  0.11017378 -0.006983540 0.28156838
## month.9       -0.0051072222 0.06107283  0.12276577  0.010036887 0.25776083
## month.10      -0.0026637947 0.05410815  0.18931488  0.095500699 0.32377149
## month.11       0.0057488855 0.14300114  0.01096669 -0.268558444 0.27949240
```

```r
fit.plots(ESB_emo_m0, "~ emo")
```

![](_report_files/figure-html/unnamed-chunk-27-1.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-2.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-3.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-4.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-5.png)<!-- -->

```r
fit.coeff(ESB_rat_m0, boot = T)
```

```
##                    Estimate         SE       pvalue  Robust.SE Robust.pvalue
## (Intercept)   -0.4101183164 0.48609113 0.3999786103 0.52253526  4.335894e-01
## rat            0.0990273611 0.03113444 0.0017369595 0.03023618  1.271875e-03
## sex           -0.0208408080 0.03431659 0.5444279085 0.03592118  5.625342e-01
## income         0.0074928449 0.01616186 0.6434980550 0.01806846  6.788724e-01
## isworking     -0.0163988544 0.04028419 0.6844445177 0.03582144  6.476648e-01
## log_age       -0.0756721472 0.13803438 0.5842407834 0.12972132  5.604086e-01
## log_city_size -0.0009416787 0.02648941 0.9716820610 0.02906537  9.741908e-01
## TC             0.0889930051 0.04873029 0.0695091405 0.05051256  7.983937e-02
## log_time_ESB   0.0654885180 0.05577246 0.2418977225 0.06013172  2.776057e-01
## log_time_TC    0.0030018075 0.06346485 0.9623287593 0.06558948  9.635482e-01
## month.1        0.6932670842 0.21945023 0.0018629858 0.06228358  3.971356e-22
## month.2        0.7799689978 0.30460897 0.0112900619 0.06884183  1.069092e-22
## month.3        0.7196494579 0.21876194 0.0012116679 0.05967855  8.537620e-25
## month.4        0.7427661488 0.24868974 0.0032221651 0.17423290  3.282633e-05
## month.6        0.7832477064 0.22394840 0.0005947211 0.07385469  1.245744e-20
## month.7        0.7262880629 0.22393265 0.0014135788 0.07640387  1.486607e-17
## month.8        0.6760050584 0.22887694 0.0035705724 0.09611488  4.299446e-11
## month.9        0.7013532195 0.22311343 0.0019597031 0.06904782  2.283800e-19
## month.10       0.6859756563 0.21829308 0.0019662399 0.06091588  1.683765e-22
## month.11       0.5655021357 0.22972005 0.0147915666 0.06427749  1.260628e-15
##                   bootBias     bootSE       bootMed        2.5 %     97.5 %
## (Intercept)    0.045372610 0.50441373 -0.3701238099 -1.343628227 0.61313332
## rat            0.002425763 0.02858378  0.1019982729  0.038567256 0.15157501
## sex           -0.002889340 0.03474631 -0.0249605220 -0.074842176 0.05687480
## income         0.001399341 0.01827486  0.0085778217 -0.029123824 0.03965059
## isworking     -0.001916675 0.03557204 -0.0208531430 -0.081085628 0.05580827
## log_age       -0.021124144 0.13236264 -0.0941404343 -0.333734083 0.16836670
## log_city_size  0.001578902 0.02821602  0.0002664029 -0.054109933 0.05308418
## TC            -0.004551114 0.04902409  0.0830344507 -0.001779735 0.18706107
## log_time_ESB   0.004103792 0.06173672  0.0705378802 -0.066761982 0.17798500
## log_time_TC   -0.010812842 0.06625355 -0.0083628275 -0.111869316 0.14589621
## month.1        0.003557648 0.06292332  0.6949013773  0.580947007 0.81169141
## month.2        0.002089186 0.07028923  0.7788801911  0.640359821 0.93369901
## month.3        0.008205962 0.05948804  0.7312103368  0.587050766 0.82465404
## month.4        0.009956444 0.19687654  0.7727244183  0.297815018 1.01994962
## month.6        0.008550703 0.07335051  0.7968221381  0.614027361 0.89621972
## month.7        0.008856022 0.07201578  0.7317994414  0.585496341 0.87586757
## month.8        0.004183623 0.10106101  0.6788782733  0.475249165 0.89075793
## month.9        0.004002611 0.06908258  0.7002307044  0.573784900 0.86047321
## month.10       0.004256125 0.06401305  0.6893116184  0.556845755 0.80752818
## month.11       0.005696393 0.07163804  0.5718257301  0.409181585 0.69696949
```

```r
fit.plots(ESB_rat_m0, "~ rat")
```

![](_report_files/figure-html/unnamed-chunk-27-6.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-7.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-8.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-9.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-10.png)<!-- -->

```r
fit.coeff(ESB_emo_m1, boot = T)
```

```
##                  Estimate         SE     pvalue  Robust.SE Robust.pvalue
## (Intercept)    0.28767622 0.47834527 0.54829895 0.51531123  5.773330e-01
## emo            0.07006561 0.03069740 0.02358174 0.03116209  2.571003e-02
## sex           -0.01981310 0.03421883 0.56327456 0.03604224  5.831647e-01
## income         0.01096761 0.01611333 0.49692767 0.01653537  5.079627e-01
## isworking     -0.03480965 0.04098945 0.39683093 0.03832226  3.648614e-01
## log_age        0.09320964 0.14363953 0.51718619 0.12477141  4.559708e-01
## log_city_size -0.03888366 0.02573489 0.13248492 0.02795817  1.659361e-01
## TC             0.09217346 0.05114279 0.07310374 0.05613723  1.022760e-01
## log_time_ESB  -0.05633942 0.06384242 0.37864629 0.07500266  4.534922e-01
## log_time_TC    0.08871033 0.05689726 0.12064698 0.05182725  8.860955e-02
## month.1        0.22503729 0.21791145 0.30307068 0.04762514  4.492286e-06
## month.2        0.30657563 0.30235993 0.31191326 0.03750556  4.307113e-14
## month.3        0.22745627 0.21785698 0.29779718 0.04813069  4.479928e-06
## month.4        0.22239632 0.25041441 0.37561475 0.15233239  1.459753e-01
## month.6        0.15682479 0.22294149 0.48265512 0.05681041  6.343071e-03
## month.7        0.12499467 0.22047024 0.57142731 0.06156214  4.372649e-02
## month.8        0.12284468 0.22260701 0.58170958 0.07462719  1.014115e-01
## month.9        0.12734812 0.21955339 0.56258715 0.06000578  3.512472e-02
## month.10       0.19565849 0.21877558 0.37228659 0.05451482  4.232835e-04
## month.11       0.01380546 0.23949089 0.95409261 0.11747122  9.065718e-01
##                    bootBias     bootSE      bootMed        2.5 %     97.5 %
## (Intercept)    5.924815e-02 0.54200531  0.291388257 -0.782095830 1.40749729
## emo            4.658696e-03 0.03036373  0.073483459  0.012987898 0.12481564
## sex           -2.079631e-03 0.03602321 -0.021365086 -0.089481549 0.05558904
## income         7.896936e-04 0.01709899  0.010723404 -0.020633709 0.04640666
## isworking     -4.612378e-04 0.04224013 -0.033588135 -0.120002733 0.04610167
## log_age       -1.280756e-02 0.13031922  0.082110442 -0.140175909 0.37065964
## log_city_size -4.130468e-03 0.03017699 -0.042774067 -0.093681778 0.02097547
## TC             1.729800e-03 0.05442796  0.094327971 -0.025583147 0.19063835
## log_time_ESB   2.188224e-03 0.07566576 -0.051872158 -0.233105898 0.08483804
## log_time_TC   -6.617782e-03 0.05978908  0.085101210 -0.032322456 0.19960903
## month.1        2.860424e-05 0.04716340  0.227234684  0.124771479 0.31276125
## month.2        2.711531e-04 0.03929598  0.306712080  0.230228604 0.38292709
## month.3       -8.524775e-04 0.04815344  0.230658873  0.127244992 0.31837564
## month.4       -5.646091e-03 0.17509115  0.218272885 -0.163980969 0.50832682
## month.6       -1.044294e-03 0.06587655  0.156836840  0.007431757 0.28006017
## month.7       -1.901749e-03 0.06476391  0.125132213 -0.007835457 0.24543931
## month.8       -1.321211e-03 0.07806367  0.116678855 -0.018338222 0.31062720
## month.9        1.200610e-03 0.06042939  0.131399295  0.003632139 0.22779976
## month.10      -5.010571e-03 0.05660877  0.189811432  0.091220366 0.31791665
## month.11      -5.980675e-03 0.13827139  0.005971785 -0.242447926 0.31081887
```

```r
fit.plots(ESB_emo_m1, "~ emo")
```

![](_report_files/figure-html/unnamed-chunk-27-11.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-12.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-13.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-14.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-15.png)<!-- -->

```r
fit.coeff(ESB_rat_m1, boot = T)
```

```
##                   Estimate         SE       pvalue  Robust.SE Robust.pvalue
## (Intercept)   -0.261465641 0.49423364 0.5975070548 0.56014117  6.412807e-01
## rat            0.109662608 0.03136867 0.0006096211 0.03077678  4.815646e-04
## sex           -0.026203880 0.03436359 0.4468420530 0.03683040  4.778136e-01
## income         0.019229049 0.01793661 0.2852892628 0.01992479  3.359425e-01
## isworking     -0.030327042 0.04158940 0.4669315228 0.03719480  4.160652e-01
## log_age       -0.137439251 0.14079238 0.3304288046 0.13942490  3.257217e-01
## log_city_size  0.008678853 0.02748207 0.7525603319 0.02942611  7.684195e-01
## TC             0.060468081 0.04891027 0.2181341577 0.05092885  2.368453e-01
## log_time_ESB   0.026337931 0.05817954 0.6513697682 0.06498715  6.858071e-01
## log_time_TC    0.020637621 0.06717795 0.7590794049 0.06977173  7.677710e-01
## month.1        0.674686038 0.21499077 0.0020199782 0.06576046  2.418755e-19
## month.2        0.760757469 0.29798807 0.0116040796 0.07041669  7.837168e-21
## month.3        0.702330635 0.21433063 0.0012842440 0.06086362  7.285729e-23
## month.4        0.735968185 0.24301614 0.0028609900 0.17171385  3.112496e-05
## month.6        0.777748404 0.21910491 0.0005050444 0.07686757  5.879750e-19
## month.7        0.744245037 0.21922968 0.0008636487 0.06935634  1.243215e-20
## month.8        0.653711206 0.22376253 0.0039817436 0.09792462  3.729228e-10
## month.9        0.679554433 0.21961226 0.0023241546 0.06842889  1.893375e-18
## month.10       0.670413831 0.21350553 0.0020081876 0.06470818  1.283885e-19
## month.11       0.560689528 0.22456748 0.0135332096 0.06163305  3.234111e-16
##                   bootBias     bootSE      bootMed       2.5 %     97.5 %
## (Intercept)    0.118124123 0.54995161 -0.115466756 -1.34109717 0.68370920
## rat            0.003914520 0.03160971  0.114125036  0.03675127 0.16240657
## sex            0.001528525 0.03873513 -0.024161339 -0.10489364 0.04655950
## income        -0.001704485 0.02074938  0.017625053 -0.02061245 0.05974936
## isworking      0.003083471 0.04019221 -0.027389459 -0.11434078 0.04005457
## log_age       -0.025726737 0.14849034 -0.174593495 -0.37679948 0.24721165
## log_city_size -0.001256876 0.03282016  0.008632136 -0.06273221 0.07535930
## TC            -0.007711389 0.05150303  0.051683992 -0.02993568 0.18331783
## log_time_ESB  -0.002023577 0.06431603  0.028228845 -0.11667325 0.14412939
## log_time_TC   -0.011856380 0.07005557  0.010066577 -0.10083169 0.17715004
## month.1       -0.006768419 0.06549346  0.668615127  0.54795571 0.82944781
## month.2       -0.008586870 0.07951863  0.753284025  0.60441723 0.92370134
## month.3       -0.002545620 0.06525728  0.701496378  0.57787708 0.83844620
## month.4       -0.005089333 0.18791156  0.733864353  0.34294793 1.03171185
## month.6       -0.004158030 0.08195586  0.777057452  0.59349581 0.92801060
## month.7        0.002929773 0.07048542  0.745595401  0.61677366 0.88833740
## month.8       -0.007049286 0.10514936  0.648349434  0.47207525 0.87125783
## month.9       -0.005258615 0.06817752  0.672198330  0.54640249 0.83134725
## month.10      -0.007052514 0.06441708  0.665106414  0.54857189 0.80659097
## month.11      -0.002189000 0.06891701  0.557006149  0.43912461 0.71143717
```

```r
fit.plots(ESB_rat_m1, "~ rat")
```

![](_report_files/figure-html/unnamed-chunk-27-16.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-17.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-18.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-19.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-20.png)<!-- -->

```r
fit.coeff(ESB_emo_m2, boot = T)
```

```
##              Estimate         SE       pvalue  Robust.SE Robust.pvalue
## (Intercept) 0.5716346 0.02093318 2.185229e-70 0.02310564  1.311251e-63
## emo         0.0562500 0.02960399 5.881904e-02 0.02960399  5.881904e-02
##                 bootBias     bootSE   bootMed        2.5 %    97.5 %
## (Intercept) -0.001199995 0.02259470 0.5706545  0.528224407 0.6173243
## emo          0.001385872 0.02952333 0.0572667 -0.001998598 0.1136650
```

```r
fit.plots(ESB_emo_m2, "~ emo")
```

![](_report_files/figure-html/unnamed-chunk-27-21.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-22.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-23.png)<!-- -->

```
## hat values (leverages) are all = 0.009615385
##  and there are no factor predictors; no plot no. 5
```

![](_report_files/figure-html/unnamed-chunk-27-24.png)<!-- -->

```r
fit.coeff(ESB_rat_m2, boot = T)
```

```
##              Estimate         SE       pvalue  Robust.SE Robust.pvalue
## (Intercept) 0.5659341 0.02165506 3.613932e-63 0.02408115  9.694983e-57
## rat         0.1016484 0.03062488 1.092872e-03 0.03062488  1.092872e-03
##                  bootBias     bootSE   bootMed      2.5 %    97.5 %
## (Intercept) -0.0004687915 0.02410911 0.5661977 0.51485518 0.6103261
## rat          0.0008355673 0.03101501 0.1029111 0.03572546 0.1601923
```

```r
fit.plots(ESB_rat_m2, "~ rat")
```

![](_report_files/figure-html/unnamed-chunk-27-25.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-26.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-27.png)<!-- -->

```
## hat values (leverages) are all = 0.01098901
##  and there are no factor predictors; no plot no. 5
```

![](_report_files/figure-html/unnamed-chunk-27-28.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-27-29.png)<!-- -->


```r
#RSB
fit.coeff(RSB_emo_m0, boot = T)
```

```
##                   Estimate         SE     pvalue  Robust.SE Robust.pvalue
## (Intercept)    0.111739749 0.41814580 0.78958594 0.46226747   0.809258429
## emo            0.010328796 0.02719540 0.70452077 0.02778059   0.710458954
## sex            0.039628545 0.03046361 0.19489406 0.03071661   0.198580337
## income        -0.010080647 0.01423847 0.47982588 0.01372881   0.463694543
## isworking     -0.026330842 0.03641592 0.47053771 0.03628279   0.468913184
## log_age        0.279737867 0.12529807 0.02675043 0.12603610   0.027641640
## log_city_size  0.028970645 0.02295190 0.20842026 0.02074449   0.164187908
## TC             0.110563744 0.04600713 0.01721977 0.04508897   0.015109575
## log_time_RSB  -0.035954441 0.04955500 0.46901441 0.04628050   0.438200166
## log_time_TC   -0.001609401 0.05134342 0.97502684 0.07651909   0.983241784
## month.1       -0.077532017 0.19475909 0.69101175 0.04411323   0.080439949
## month.2       -0.144830513 0.27037622 0.59282133 0.04756426   0.002659302
## month.3       -0.032051627 0.19460038 0.86935232 0.04304469   0.457430703
## month.4       -0.205891137 0.22370639 0.35855620 0.10708516   0.056022898
## month.6       -0.087758944 0.19914029 0.65994177 0.05593363   0.118324468
## month.7       -0.007316007 0.19706175 0.97042416 0.05887302   0.901235490
## month.8       -0.081341936 0.19890919 0.68304666 0.04842897   0.094684850
## month.9       -0.052711926 0.19616514 0.78844441 0.05533853   0.342041980
## month.10      -0.105645710 0.19562074 0.58979575 0.05373181   0.050743667
## month.11      -0.116623275 0.21344506 0.58544544 0.05721900   0.042924039
##                    bootBias     bootSE      bootMed        2.5 %       97.5 %
## (Intercept)    0.0125744623 0.48835486  0.120275912 -0.890114500  1.065768689
## emo            0.0029735953 0.02877254  0.011773863 -0.056888997  0.066423552
## sex           -0.0001466025 0.02897972  0.039855739 -0.022019395  0.095979248
## income         0.0005621169 0.01390543 -0.009283218 -0.037499426  0.014685691
## isworking     -0.0032726457 0.03627539 -0.030651967 -0.096505773  0.049292785
## log_age        0.0022323644 0.12192480  0.284403544  0.036050453  0.502509778
## log_city_size -0.0014956167 0.02049532  0.026419880 -0.006856053  0.072664896
## TC            -0.0008648594 0.04211740  0.110237760  0.026948603  0.200764063
## log_time_RSB  -0.0087355311 0.05167687 -0.047161261 -0.114846774  0.105123377
## log_time_TC    0.0082628439 0.07993602  0.004374286 -0.155857640  0.146191900
## month.1       -0.0022532523 0.04637946 -0.081939946 -0.158114570  0.019335557
## month.2       -0.0068878626 0.04974964 -0.148059588 -0.236583856 -0.051528310
## month.3       -0.0027518708 0.04207065 -0.035494000 -0.108958402  0.064643056
## month.4       -0.0060518421 0.12193641 -0.211896007 -0.409217822  0.081301973
## month.6       -0.0007870990 0.05564473 -0.088459351 -0.200572247  0.020097126
## month.7       -0.0035900433 0.05494136 -0.011891997 -0.118500116  0.109710935
## month.8       -0.0032081353 0.04662274 -0.084592406 -0.161311070  0.031656948
## month.9       -0.0002829689 0.05878379 -0.056236431 -0.149978504  0.079179784
## month.10      -0.0006492028 0.05297696 -0.106660059 -0.211479019 -0.002478603
## month.11       0.0028983426 0.06794858 -0.108458567 -0.337402904 -0.021038713
```

```r
fit.plots(RSB_emo_m0, "~ emo")
```

![](_report_files/figure-html/unnamed-chunk-28-1.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-2.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-3.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-4.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-5.png)<!-- -->

```r
fit.coeff(RSB_rat_m0, boot = T)
```

```
##                   Estimate         SE     pvalue  Robust.SE Robust.pvalue
## (Intercept)   -0.408473847 0.43982642 0.35430823 0.41107902    0.32175132
## rat            0.019778345 0.02839546 0.48701470 0.02775025    0.47695878
## sex            0.049297010 0.03153500 0.11979065 0.03343765    0.14218826
## income        -0.018125130 0.01486590 0.22438402 0.01629364    0.26748101
## isworking     -0.019530697 0.03675468 0.59582555 0.03823261    0.61010422
## log_age        0.297845778 0.12624589 0.01940817 0.12612194    0.01929103
## log_city_size  0.016292224 0.02412973 0.50043924 0.01840140    0.37716010
## TC             0.098612616 0.04502243 0.02981729 0.05024205    0.05125127
## log_time_RSB  -0.029313862 0.05700330 0.60772333 0.05358850    0.58505944
## log_time_TC    0.100846914 0.05847247 0.08633882 0.05832271    0.08554123
## month.1       -0.006310103 0.19882928 0.97471832 0.05237640    0.90424345
## month.2       -0.031905304 0.27858562 0.90895137 0.06016458    0.59657209
## month.3        0.034796897 0.19896634 0.86136838 0.06111688    0.56984455
## month.4       -0.085802100 0.22661534 0.70542312 0.07611262    0.26114828
## month.6        0.030151742 0.20346166 0.88235953 0.08388566    0.71969790
## month.7        0.043431936 0.20323689 0.83102755 0.06670308    0.51581603
## month.8        0.012453250 0.20781432 0.95228345 0.08906980    0.88896643
## month.9        0.037374144 0.20324269 0.85431204 0.06494808    0.56572436
## month.10       0.029906001 0.19857864 0.88046349 0.05598173    0.59387037
## month.11      -0.009027340 0.20913148 0.96561825 0.08822428    0.91861708
##                    bootBias     bootSE      bootMed        2.5 %     97.5 %
## (Intercept)    0.0588856632 0.45369417 -0.357261105 -1.284442667 0.51720369
## rat            0.0007619124 0.02590789  0.020291902 -0.028806422 0.07196145
## sex            0.0002294013 0.03348549  0.049512157 -0.016057720 0.12080961
## income        -0.0006909493 0.01653782 -0.019000714 -0.049716455 0.01602531
## isworking     -0.0015321516 0.04041074 -0.021401163 -0.095205438 0.06761613
## log_age        0.0052499474 0.14290998  0.297108418  0.041776583 0.59496963
## log_city_size  0.0001215200 0.02115551  0.016629062 -0.024626927 0.05867764
## TC            -0.0030826334 0.04954470  0.094063895  0.009273425 0.21144898
## log_time_RSB  -0.0070830081 0.06275485 -0.036014082 -0.141716525 0.08965023
## log_time_TC   -0.0064438358 0.05986319  0.096885391 -0.011773822 0.23561917
## month.1       -0.0006694180 0.05292201 -0.007610865 -0.118261609 0.09743766
## month.2        0.0040235262 0.06263895 -0.028777154 -0.164240815 0.08237194
## month.3        0.0021618391 0.06125854  0.035488428 -0.100251994 0.15576534
## month.4        0.0037331778 0.08807696 -0.093849360 -0.215641472 0.15537509
## month.6       -0.0037701507 0.07986212  0.020768551 -0.111879909 0.19056907
## month.7        0.0007200817 0.07057431  0.045513445 -0.088582824 0.19507109
## month.8        0.0013210745 0.08555632  0.012296034 -0.150610082 0.17291333
## month.9        0.0080281832 0.06713701  0.042440427 -0.087258905 0.16959243
## month.10       0.0024849936 0.05702384  0.030529193 -0.099490031 0.13152172
## month.11       0.0015813348 0.09584477 -0.001204909 -0.229066276 0.15091919
```

```r
fit.plots(RSB_rat_m0, "~ rat")
```

![](_report_files/figure-html/unnamed-chunk-28-6.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-7.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-8.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-9.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-10.png)<!-- -->

```r
fit.coeff(RSB_emo_m1, boot = T)
```

```
##                   Estimate         SE     pvalue  Robust.SE Robust.pvalue
## (Intercept)    0.109586231 0.41912165 0.79401908 0.46147496   0.812551159
## emo            0.009426636 0.02735373 0.73076545 0.02797076   0.736479585
## sex            0.038593064 0.03064639 0.20948251 0.03088641   0.213029771
## income        -0.009068206 0.01450284 0.53255132 0.01418112   0.523304439
## isworking     -0.026992056 0.03653680 0.46097195 0.03637386   0.458969290
## log_age        0.277265723 0.12573829 0.02865982 0.12603901   0.029037320
## log_city_size  0.028804252 0.02300741 0.21214140 0.02078881   0.167521337
## TC             0.110852647 0.04611643 0.01719901 0.04516809   0.015029225
## log_time_RSB  -0.033405187 0.05009128 0.50566240 0.04677486   0.476007998
## log_time_TC   -0.003066276 0.05159316 0.95267129 0.07643767   0.968044233
## month.1       -0.077982477 0.19520019 0.68997830 0.04408810   0.078550922
## month.2       -0.143868174 0.27099503 0.59612257 0.04756946   0.002839704
## month.3       -0.030791521 0.19506428 0.87474190 0.04323140   0.477193622
## month.4       -0.206935136 0.22422502 0.35724719 0.10794254   0.056743225
## month.6       -0.088604841 0.19959954 0.65761596 0.05588868   0.114560454
## month.7       -0.008039407 0.19751328 0.96757577 0.05908459   0.891914643
## month.8       -0.081728056 0.19935867 0.68230582 0.04840505   0.092988653
## month.9       -0.052930409 0.19660680 0.78805592 0.05529806   0.339703723
## month.10      -0.105663746 0.19606040 0.59056955 0.05373592   0.050729871
## month.11      -0.116465365 0.21392515 0.58679688 0.05736332   0.043733831
##                   bootBias     bootSE      bootMed        2.5 %       97.5 %
## (Intercept)    0.033333702 0.42727056  0.150989874 -0.743901583  0.896054589
## emo            0.006327046 0.02768117  0.015769334 -0.046902060  0.057577543
## sex            0.001440065 0.03171344  0.038250798 -0.016609647  0.101801357
## income         0.001990272 0.01566993 -0.007933324 -0.039838245  0.021952538
## isworking     -0.005916280 0.03838241 -0.034025096 -0.093330754  0.060413521
## log_age       -0.013853245 0.13091930  0.265007134  0.014035208  0.545034045
## log_city_size -0.004745518 0.02284811  0.024597639 -0.015203678  0.082031397
## TC             0.006734720 0.04548408  0.118087443 -0.003468923  0.186707179
## log_time_RSB  -0.006928723 0.05031524 -0.040884630 -0.120748052  0.086163264
## log_time_TC    0.009180540 0.07935347 -0.002567774 -0.137963372  0.163494702
## month.1       -0.001624928 0.04205391 -0.082370740 -0.152811140  0.019986392
## month.2       -0.008809980 0.05223027 -0.145416001 -0.254506464 -0.046335687
## month.3       -0.004498988 0.04024085 -0.036532416 -0.095485332  0.065420095
## month.4       -0.008777843 0.12898307 -0.219946375 -0.419020639  0.092260889
## month.6       -0.002104278 0.05877932 -0.088674645 -0.224353298  0.020365925
## month.7       -0.004705457 0.06079679 -0.013883830 -0.125127883  0.122419233
## month.8       -0.004350246 0.05309495 -0.085956208 -0.188433872  0.020722842
## month.9       -0.005220834 0.05488930 -0.058007103 -0.147437400  0.075910067
## month.10       0.001794088 0.05373166 -0.101800636 -0.220559369 -0.006207430
## month.11       0.003278384 0.06568942 -0.110365506 -0.276685313 -0.006693031
```

```r
fit.plots(RSB_emo_m1, "~ emo")
```

![](_report_files/figure-html/unnamed-chunk-28-11.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-12.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-13.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-14.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-15.png)<!-- -->

```r
fit.coeff(RSB_rat_m1, boot = T)
```

```
##                    Estimate         SE      pvalue  Robust.SE Robust.pvalue
## (Intercept)   -0.4370041843 0.47258194 0.356488922 0.48745000    0.37131031
## rat            0.0131185651 0.02966033 0.658866867 0.02840698    0.64483870
## sex            0.0419332000 0.03245854 0.198231665 0.03488135    0.23105316
## income        -0.0329703922 0.01684395 0.052018495 0.01887660    0.08259733
## isworking     -0.0069944606 0.03902362 0.857975902 0.04060253    0.86344295
## log_age        0.3625822967 0.13250278 0.006904746 0.13110625    0.00634249
## log_city_size  0.0198833962 0.02576231 0.441356221 0.02084950    0.34167518
## TC             0.1073713204 0.04669001 0.022743730 0.05298432    0.04435656
## log_time_RSB  -0.0416248476 0.07355416 0.572239705 0.08615556    0.62965150
## log_time_TC    0.1038347335 0.06610713 0.118202524 0.07104148    0.14578592
## month.1       -0.0210840335 0.19991362 0.916136746 0.05548876    0.70446579
## month.2       -0.0344915442 0.27993443 0.902091210 0.06465194    0.59442155
## month.3        0.0131414273 0.20020255 0.947744851 0.06522140    0.84056860
## month.4       -0.0998177239 0.22763735 0.661612062 0.07486589    0.18430945
## month.6        0.0099753491 0.20446800 0.961149260 0.08426877    0.90591688
## month.7        0.0276261009 0.20484623 0.892887902 0.07132968    0.69904128
## month.8        0.0009674009 0.20859539 0.996305377 0.09123738    0.99155314
## month.9        0.0488469852 0.20505672 0.812017655 0.06717611    0.46818469
## month.10       0.0316492174 0.19945291 0.874118518 0.05943027    0.59507918
## month.11      -0.0324983347 0.21007091 0.877249030 0.09116404    0.72194354
##                    bootBias     bootSE      bootMed       2.5 %      97.5 %
## (Intercept)    0.0585078684 0.51610245 -0.405964666 -1.43972937 0.517308159
## rat            0.0004921827 0.02970515  0.013077012 -0.04154617 0.073353386
## sex           -0.0029358094 0.03417463  0.038888246 -0.02675173 0.112385799
## income        -0.0002786967 0.01881172 -0.032437913 -0.07318287 0.003103423
## isworking     -0.0023080395 0.04134510 -0.009813261 -0.08950462 0.072165422
## log_age        0.0048323130 0.13935244  0.355245946  0.11002920 0.658444067
## log_city_size -0.0006071906 0.02146286  0.019496706 -0.02017282 0.063981669
## TC            -0.0065696597 0.05221575  0.098696255  0.02350431 0.247344133
## log_time_RSB  -0.0059767261 0.08947770 -0.043018564 -0.22723697 0.133882328
## log_time_TC   -0.0058639975 0.06822301  0.097416389 -0.02713319 0.248290065
## month.1        0.0008081330 0.05453648 -0.021976817 -0.11727542 0.086461312
## month.2        0.0081446389 0.06324746 -0.028883203 -0.16793543 0.083865092
## month.3        0.0009848572 0.06661499  0.018292507 -0.12461337 0.141160815
## month.4        0.0081413969 0.09031717 -0.107129047 -0.22241734 0.140205085
## month.6       -0.0003259708 0.08703587  0.005880736 -0.15423437 0.186593709
## month.7        0.0042847080 0.07279309  0.034712931 -0.12415456 0.159349633
## month.8        0.0040990748 0.09769998  0.003543233 -0.19782416 0.189159197
## month.9        0.0037978002 0.07226587  0.048977467 -0.08364820 0.212397649
## month.10      -0.0002891505 0.05897065  0.027532090 -0.07941686 0.151906042
## month.11       0.0063974372 0.09311498 -0.025926934 -0.26778179 0.127507387
```

```r
fit.plots(RSB_rat_m1, "~ rat")
```

![](_report_files/figure-html/unnamed-chunk-28-16.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-17.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-18.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-19.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-20.png)<!-- -->

```r
fit.coeff(RSB_emo_m2, boot = T)
```

```
##                Estimate         SE       pvalue  Robust.SE Robust.pvalue
## (Intercept) 0.493269231 0.01898874 6.421266e-67 0.01863601  3.273360e-68
## emo         0.004807692 0.02685414 8.580902e-01 0.02685414  8.580902e-01
##                  bootBias     bootSE     bootMed       2.5 %     97.5 %
## (Intercept) -5.761101e-04 0.01881011 0.492727273  0.45575901 0.53219864
## emo         -2.127436e-06 0.02649260 0.004707074 -0.04561947 0.05740483
```

```r
fit.plots(RSB_emo_m2, "~ emo")
```

![](_report_files/figure-html/unnamed-chunk-28-21.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-22.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-23.png)<!-- -->

```
## hat values (leverages) are all = 0.009615385
##  and there are no factor predictors; no plot no. 5
```

![](_report_files/figure-html/unnamed-chunk-28-24.png)<!-- -->

```r
fit.coeff(RSB_rat_m2, boot = T)
```

```
##               Estimate         SE       pvalue  Robust.SE Robust.pvalue
## (Intercept) 0.49230769 0.02048099 4.434911e-58 0.02010235  3.371877e-59
## rat         0.01593407 0.02896449 5.829163e-01 0.02896449  5.829163e-01
##                  bootBias     bootSE    bootMed       2.5 %     97.5 %
## (Intercept) -0.0002652897 0.02025909 0.49232732  0.45324077 0.53260409
## rat         -0.0001409882 0.02906404 0.01671099 -0.04417238 0.06812455
```

```r
fit.plots(RSB_rat_m2, "~ rat")
```

![](_report_files/figure-html/unnamed-chunk-28-25.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-26.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-27.png)<!-- -->

```
## hat values (leverages) are all = 0.01098901
##  and there are no factor predictors; no plot no. 5
```

![](_report_files/figure-html/unnamed-chunk-28-28.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-28-29.png)<!-- -->


```r
#EMB
fit.coeff(EMB_emo_m0, boot = T)
```

```
##                  Estimate         SE     pvalue  Robust.SE Robust.pvalue
## (Intercept)   -0.56860096 0.50392582 0.26060553 0.48648109  0.2439547612
## emo            0.03674972 0.03248805 0.25941407 0.03193332  0.2512573387
## sex            0.07166720 0.03649402 0.05101903 0.03298784  0.0310600481
## income        -0.03961513 0.01697265 0.02064367 0.01768744  0.0262734082
## isworking      0.02521660 0.04376810 0.56520535 0.04503625  0.5761985452
## log_age        0.26916782 0.15222571 0.07863823 0.14627006  0.0673053828
## log_city_size  0.02196186 0.02756247 0.42656532 0.02692152  0.4156568035
## TC             0.08682960 0.05462036 0.11357536 0.05308759  0.1035894307
## log_time_EMB  -0.02390833 0.06242382 0.70215008 0.05882393  0.6848799806
## log_time_TC    0.12285745 0.06100713 0.04544641 0.05501943  0.0267232884
## month.1        0.15063048 0.23316703 0.51905027 0.05823641  0.0104463303
## month.2       -0.05171273 0.32630962 0.87424977 0.06577967  0.4327638180
## month.3        0.19616409 0.23311093 0.40112837 0.05008332  0.0001252858
## month.4        0.16790667 0.26795301 0.53165972 0.12938327  0.1959561196
## month.6        0.02644980 0.23852166 0.91182081 0.08166140  0.7463749485
## month.7        0.17107563 0.23590915 0.46924104 0.06055018  0.0052302434
## month.8        0.17417704 0.23820810 0.46556439 0.07653770  0.0239856294
## month.9        0.12450413 0.23508196 0.59699692 0.06611167  0.0612038762
## month.10       0.09841803 0.23420456 0.67480025 0.06550337  0.1346400933
## month.11       0.08890196 0.25620750 0.72898337 0.08792743  0.3132692081
##                    bootBias     bootSE     bootMed         2.5 %       97.5 %
## (Intercept)    0.0729434380 0.50852561 -0.53288157 -1.5413811858  0.449969087
## emo           -0.0010747513 0.03328592  0.03215076 -0.0256901246  0.111306179
## sex           -0.0002272931 0.03299513  0.07259233  0.0001448952  0.131459367
## income         0.0003071473 0.01740887 -0.03920926 -0.0752339657 -0.007315677
## isworking     -0.0017588431 0.04656319  0.02127219 -0.0701818652  0.116664317
## log_age       -0.0065088033 0.14892813  0.27321843 -0.0474738899  0.551135448
## log_city_size -0.0029260974 0.02804169  0.01953820 -0.0309080731  0.083961759
## TC             0.0065602708 0.05572676  0.09629365 -0.0424761112  0.188470850
## log_time_EMB  -0.0025913836 0.06150287 -0.02737652 -0.1431562400  0.108930115
## log_time_TC   -0.0075552981 0.05707138  0.11364440  0.0181738919  0.239334863
## month.1        0.0002983471 0.05890325  0.15083136  0.0404249306  0.260294442
## month.2        0.0053510900 0.06723994 -0.05104155 -0.1795191098  0.081357756
## month.3       -0.0014612837 0.05487286  0.19200974  0.0864185488  0.306637851
## month.4       -0.0038975882 0.14618153  0.15975411 -0.1330143437  0.463532022
## month.6       -0.0040594997 0.08224958  0.02685523 -0.1399516614  0.198303251
## month.7        0.0042394241 0.06005283  0.17691560  0.0375697395  0.281990195
## month.8       -0.0014311314 0.07578486  0.17335509  0.0072542329  0.326411865
## month.9       -0.0024501770 0.06731587  0.12109273 -0.0185815563  0.270310119
## month.10      -0.0061001530 0.06783845  0.09483063 -0.0347265642  0.228690043
## month.11       0.0073414200 0.10580478  0.09967131 -0.1882983747  0.259871724
```

```r
fit.plots(EMB_emo_m0, "~ emo")
```

![](_report_files/figure-html/unnamed-chunk-29-1.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-2.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-3.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-4.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-5.png)<!-- -->

```r
fit.coeff(EMB_rat_m0, boot = T)
```

```
##                   Estimate         SE     pvalue  Robust.SE Robust.pvalue
## (Intercept)   -0.554270773 0.52285564 0.29055911 0.49124367    0.26072847
## rat            0.055102425 0.03352866 0.10207664 0.03365807    0.10339136
## sex            0.025448929 0.03716314 0.49437762 0.03769650    0.50049781
## income        -0.022525518 0.01746862 0.19892154 0.01869066    0.22975367
## isworking     -0.025063841 0.04336180 0.56399171 0.04563758    0.58356913
## log_age        0.176770252 0.14987164 0.23979993 0.16334137    0.28063812
## log_city_size  0.034041503 0.02862669 0.23598156 0.02909624    0.24359795
## TC             0.037701595 0.05275967 0.47580840 0.05041080    0.45552631
## log_time_EMB   0.008225685 0.06535790 0.89998954 0.06138950    0.89356220
## log_time_TC    0.117379327 0.06765921 0.08451605 0.06526064    0.07379177
## month.1        0.095828403 0.23472151 0.68357594 0.05890146    0.10554152
## month.2        0.007858685 0.33046484 0.98105448 0.07822399    0.92009041
## month.3        0.157593792 0.23476532 0.50292082 0.06581869    0.01769984
## month.4        0.015289185 0.26738287 0.95446598 0.16870294    0.92789136
## month.6        0.024260375 0.24026648 0.91968700 0.07876943    0.75845242
## month.7        0.131520576 0.23991227 0.58424731 0.08118484    0.10701965
## month.8        0.056704495 0.24529952 0.81745567 0.10120268    0.57598306
## month.9        0.153753925 0.23974241 0.52214279 0.06883737    0.02677026
## month.10       0.052124114 0.23442303 0.82429873 0.06057233    0.39066942
## month.11      -0.001737250 0.24697744 0.99439566 0.10249986    0.98649665
##                    bootBias     bootSE      bootMed        2.5 %     97.5 %
## (Intercept)    7.586375e-03 0.52822300 -0.542533015 -1.599360958 0.49591000
## rat            3.499253e-03 0.03154677  0.057020768 -0.008184813 0.11667995
## sex           -1.479922e-03 0.03800319  0.025698824 -0.053063252 0.09898035
## income         5.757785e-05 0.01867728 -0.023158311 -0.055814640 0.01788995
## isworking     -2.089865e-03 0.04352716 -0.026678196 -0.114734425 0.05665405
## log_age       -3.847393e-03 0.16547722  0.164009628 -0.115375130 0.54377838
## log_city_size  1.594475e-04 0.03021990  0.035029837 -0.037552901 0.09170369
## TC            -5.209172e-03 0.04817992  0.029975607 -0.052548950 0.13066547
## log_time_EMB   6.626652e-03 0.06268849  0.010086980 -0.121554145 0.13781398
## log_time_TC   -7.538150e-03 0.07090162  0.115959001 -0.037944752 0.24020509
## month.1       -5.357678e-04 0.05614086  0.096244720 -0.027478709 0.20374355
## month.2       -3.466825e-04 0.07698511  0.010360553 -0.172200895 0.14221856
## month.3        2.245044e-03 0.05916118  0.160096112  0.033616809 0.26161329
## month.4        1.354379e-02 0.20129268  0.002485018 -0.243012943 0.53007531
## month.6        3.418884e-03 0.07280572  0.022928025 -0.104776645 0.19852626
## month.7        3.716445e-03 0.08439043  0.135069293 -0.043483682 0.28741044
## month.8       -2.832362e-03 0.10199511  0.059999503 -0.142307632 0.24210550
## month.9        9.172340e-03 0.06848632  0.163398993 -0.006357444 0.26983417
## month.10       5.652174e-04 0.06193869  0.055643901 -0.083521761 0.16462542
## month.11      -5.002980e-03 0.10571517 -0.008610991 -0.187427148 0.20415856
```

```r
fit.plots(EMB_rat_m0, "~ rat")
```

![](_report_files/figure-html/unnamed-chunk-29-6.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-7.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-8.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-9.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-10.png)<!-- -->

```r
fit.coeff(EMB_emo_m1, boot = T)
```

```
##                  Estimate         SE     pvalue  Robust.SE Robust.pvalue
## (Intercept)   -0.58660058 0.50495632 0.24683586 0.48729669  0.2301861033
## emo            0.03858543 0.03260477 0.23813349 0.03201729  0.2296627330
## sex            0.07405726 0.03665740 0.04477622 0.03317842  0.0267895387
## income        -0.04183965 0.01722408 0.01607541 0.01796867  0.0209501110
## isworking      0.02690141 0.04386489 0.54043318 0.04504886  0.5511185347
## log_age        0.27290286 0.15245410 0.07505268 0.14653346  0.0641080568
## log_city_size  0.02250605 0.02759909 0.41583908 0.02685073  0.4029877486
## TC             0.08526431 0.05471199 0.12081525 0.05312936  0.1102067579
## log_time_EMB  -0.02301931 0.06249733 0.71304548 0.05866692  0.6952271325
## log_time_TC    0.12435466 0.06109867 0.04322350 0.05512644  0.0252346413
## month.1        0.15259188 0.23341668 0.51408349 0.05834155  0.0096332511
## month.2       -0.05299229 0.32664438 0.87129711 0.06572746  0.4211215410
## month.3        0.19355093 0.23337087 0.40794658 0.05000419  0.0001496047
## month.4        0.17106492 0.26825468 0.52444753 0.12752519  0.1814020836
## month.6        0.02905234 0.23878636 0.90329295 0.08166316  0.7224204776
## month.7        0.17388300 0.23617526 0.46249889 0.06050116  0.0045192797
## month.8        0.17581162 0.23845858 0.46186901 0.07676878  0.0231231532
## month.9        0.12574152 0.23532547 0.59374436 0.06630356  0.0594338387
## month.10       0.09967100 0.23444734 0.67122686 0.06550919  0.1298192926
## month.11       0.08901540 0.25646719 0.72891720 0.08779581  0.3119377574
##                    bootBias     bootSE     bootMed         2.5 %       97.5 %
## (Intercept)    0.0792854543 0.46084450 -0.52117476 -1.6068367895  0.305567930
## emo            0.0033436942 0.03149126  0.04073899 -0.0211035306  0.106550597
## sex           -0.0002888063 0.03347990  0.07365564  0.0070412686  0.139994230
## income         0.0016989889 0.01873848 -0.03841363 -0.0820643987 -0.009214026
## isworking     -0.0039179182 0.04447388  0.02325897 -0.0564875022  0.115325174
## log_age       -0.0213709210 0.15135450  0.25102796 -0.0104878663  0.569489311
## log_city_size -0.0033059898 0.02915632  0.02161110 -0.0329973099  0.076642282
## TC             0.0056360940 0.05436786  0.08530482 -0.0088452799  0.205134556
## log_time_EMB  -0.0048161414 0.06151541 -0.02644173 -0.1398770729  0.099849377
## log_time_TC   -0.0022665344 0.05727779  0.12480696 -0.0022789483  0.241299497
## month.1       -0.0036070614 0.05485245  0.14968700  0.0445415139  0.276370821
## month.2        0.0027293523 0.06656698 -0.05422644 -0.1781416562  0.091763249
## month.3       -0.0007049132 0.04890190  0.19434883  0.0986176034  0.284543048
## month.4       -0.0028871633 0.14685066  0.16669007 -0.0964633375  0.495064157
## month.6       -0.0017756003 0.08568507  0.03697953 -0.1604812485  0.167981556
## month.7        0.0046317690 0.06224717  0.17736166  0.0481446061  0.291107971
## month.8       -0.0041743366 0.08296952  0.17354015 -0.0005989996  0.332686869
## month.9       -0.0074680212 0.06463092  0.11652719  0.0041965359  0.260817862
## month.10      -0.0058378374 0.06505110  0.09520181 -0.0292933578  0.219171472
## month.11      -0.0005644666 0.10498154  0.08742287 -0.1621812315  0.274369114
```

```r
fit.plots(EMB_emo_m1, "~ emo")
```

![](_report_files/figure-html/unnamed-chunk-29-11.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-12.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-13.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-14.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-15.png)<!-- -->

```r
fit.coeff(EMB_rat_m1, boot = T)
```

```
##                   Estimate         SE     pvalue  Robust.SE Robust.pvalue
## (Intercept)   -0.722167963 0.53422333 0.17832079 0.49229477   0.144330393
## rat            0.034647302 0.03416835 0.31208656 0.03437096   0.314938509
## sex            0.026844614 0.03768243 0.47724763 0.03918270   0.494251147
## income        -0.023180415 0.01959343 0.23851436 0.02029761   0.255128050
## isworking     -0.033385343 0.04527367 0.46193864 0.04789981   0.486812493
## log_age        0.269378982 0.15469358 0.08351679 0.15692343   0.087958695
## log_city_size  0.032413413 0.03004061 0.28219759 0.03146089   0.304416077
## TC             0.027897854 0.05374741 0.60443085 0.05334939   0.601739266
## log_time_EMB   0.026340247 0.06784718 0.69835621 0.06636937   0.691983057
## log_time_TC    0.114086655 0.07225261 0.11628734 0.07186706   0.114356567
## month.1        0.115971896 0.23253718 0.61865026 0.06194136   0.062969435
## month.2        0.030004370 0.32693341 0.92699006 0.08206627   0.715131974
## month.3        0.150440569 0.23261380 0.51871571 0.06787286   0.028049922
## month.4        0.001532389 0.26446989 0.99538406 0.16451330   0.992579527
## month.6        0.014570040 0.23789951 0.95123997 0.07931535   0.854480414
## month.7        0.114086640 0.23824302 0.63267921 0.08590359   0.186020524
## month.8        0.052389167 0.24262736 0.82931831 0.10195319   0.608053468
## month.9        0.193171017 0.23854449 0.41924856 0.06677192   0.004341007
## month.10       0.060442945 0.23200045 0.79478628 0.06236552   0.333903843
## month.11      -0.011674929 0.24448732 0.96197216 0.10241282   0.909379976
##                    bootBias     bootSE      bootMed        2.5 %     97.5 %
## (Intercept)    0.0935619504 0.49208448 -0.634965066 -1.784566687 0.14151822
## rat            0.0025794842 0.03434822  0.037971145 -0.034471507 0.10127078
## sex           -0.0012703885 0.04017142  0.025425289 -0.053582452 0.09972216
## income         0.0001063334 0.02101354 -0.023134437 -0.063813796 0.01728130
## isworking     -0.0022688936 0.04811565 -0.034556027 -0.123919021 0.05168558
## log_age       -0.0138374669 0.15991210  0.262359604 -0.059766349 0.58679774
## log_city_size -0.0011126394 0.03170871  0.030144369 -0.025639818 0.10010619
## TC            -0.0078736517 0.05303676  0.021372078 -0.069975543 0.15751729
## log_time_EMB  -0.0017663112 0.07438292  0.026022385 -0.112178424 0.17069088
## log_time_TC   -0.0118965639 0.07209124  0.105547692 -0.033430507 0.24530129
## month.1       -0.0001943762 0.06033498  0.119192927 -0.008222683 0.22163174
## month.2        0.0030724050 0.08152798  0.028616657 -0.125750038 0.20875648
## month.3        0.0031339519 0.06859920  0.153093384  0.011135681 0.27719039
## month.4        0.0096568134 0.20123263 -0.019421808 -0.250009456 0.56153577
## month.6        0.0025229295 0.08067284  0.018438944 -0.157776684 0.15808871
## month.7        0.0060745193 0.08913198  0.115286821 -0.047177314 0.30465696
## month.8        0.0046022502 0.10056643  0.056546432 -0.147687286 0.26578051
## month.9        0.0002011243 0.07195330  0.195360161  0.041122482 0.32514229
## month.10      -0.0000840130 0.06326455  0.061039824 -0.072996945 0.18455921
## month.11       0.0067764736 0.11724399  0.001547804 -0.242237782 0.20286934
```

```r
fit.plots(EMB_rat_m1, "~ rat")
```

![](_report_files/figure-html/unnamed-chunk-29-16.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-17.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-18.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-19.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-20.png)<!-- -->

```r
fit.coeff(EMB_emo_m2, boot = T)
```

```
##               Estimate         SE       pvalue  Robust.SE Robust.pvalue
## (Intercept) 0.56009615 0.02258903 9.304637e-64 0.02390605  5.216551e-60
## emo         0.01490385 0.03194571 6.413244e-01 0.03194571  6.413244e-01
##                 bootBias     bootSE   bootMed      2.5 %     97.5 %
## (Intercept) -0.001343322 0.02373836 0.5587549  0.5149849 0.60666318
## emo          0.001623848 0.03176194 0.0170218 -0.0485328 0.07541806
```

```r
fit.plots(EMB_emo_m2, "~ emo")
```

![](_report_files/figure-html/unnamed-chunk-29-21.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-22.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-23.png)<!-- -->

```
## hat values (leverages) are all = 0.009615385
##  and there are no factor predictors; no plot no. 5
```

![](_report_files/figure-html/unnamed-chunk-29-24.png)<!-- -->

```r
fit.coeff(EMB_rat_m2, boot = T)
```

```
##               Estimate         SE       pvalue  Robust.SE Robust.pvalue
## (Intercept) 0.57472527 0.02364965 9.821768e-59 0.02529887  9.396213e-55
## rat         0.02747253 0.03344565 4.125005e-01 0.03344565  4.125005e-01
##                  bootBias     bootSE    bootMed       2.5 %    97.5 %
## (Intercept) -0.0005534032 0.02525750 0.57473401  0.52355215 0.6273632
## rat          0.0003472735 0.03376152 0.02837064 -0.04024144 0.0916640
```

```r
fit.plots(EMB_rat_m2, "~ rat")
```

![](_report_files/figure-html/unnamed-chunk-29-25.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-26.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-27.png)<!-- -->

```
## hat values (leverages) are all = 0.01098901
##  and there are no factor predictors; no plot no. 5
```

![](_report_files/figure-html/unnamed-chunk-29-28.png)<!-- -->![](_report_files/figure-html/unnamed-chunk-29-29.png)<!-- -->

## Приложение



```r
#make summary
df <- rbind(
  fit.params(RB_emo_m0),
  fit.params(RB_emo_m1),

  fit.params(RB_rat_m0),
  fit.params(RB_rat_m1),

  fit.params(ESB_emo_m0),
  fit.params(ESB_emo_m1),

  fit.params(ESB_rat_m0),
  fit.params(ESB_rat_m1),

  fit.params(RSB_emo_m0),
  fit.params(RSB_emo_m1),

  fit.params(RSB_rat_m0),
  fit.params(RSB_rat_m1),

  fit.params(EMB_emo_m0),
  fit.params(EMB_emo_m1),

  fit.params(EMB_rat_m0),
  fit.params(EMB_rat_m1)
)
write.table(df, file = paste0('models\\models_pars.csv'), sep=";", dec=",")
df
```

```
##            type      src group r.squared adj.r.squared shapiro.p.value
## RB_emo_m0    lm  emo_all   emo 0.1854741   0.103590541    5.933183e-01
## RB_emo_m1    lm emo_data   emo 0.1810835   0.098320622    5.209019e-01
## RB_rat_m0    lm  rat_all   rat 0.1277595   0.033597123    1.266375e-01
## RB_rat_m1    lm rat_data   rat 0.1089132   0.004402973    1.478884e-01
## ESB_emo_m0   lm  emo_all   emo 0.1164439   0.027620785    1.897174e-01
## ESB_emo_m1   lm emo_data   emo 0.1146713   0.025196591    1.966178e-01
## ESB_rat_m0   lm  rat_all   rat 0.1496946   0.057900272    1.205193e-01
## ESB_rat_m1   lm rat_data   rat 0.1656386   0.067781431    8.001263e-02
## RSB_emo_m0   lm  emo_all   emo 0.1222804   0.034044014    3.224553e-05
## RSB_emo_m1   lm emo_data   emo 0.1227599   0.034102684    3.836915e-05
## RSB_rat_m0   lm  rat_all   rat 0.1075883   0.011248445    1.360207e-05
## RSB_rat_m1   lm rat_data   rat 0.1246319   0.021965282    3.727486e-05
## EMB_emo_m0   lm  emo_all   emo 0.1117455   0.022450038    1.139032e-01
## EMB_emo_m1   lm emo_data   emo 0.1132409   0.023621655    1.232945e-01
## EMB_rat_m0   lm  rat_all   rat 0.1055124   0.008948425    3.392518e-01
## EMB_rat_m1   lm rat_data   rat 0.1135954   0.009634384    3.013467e-01
##            bp.p.value   f.p.value
## RB_emo_m0  0.04575478 0.002836128
## RB_emo_m1  0.05614150 0.004146493
## RB_rat_m0  0.55106162 0.154221474
## RB_rat_m1  0.43334585 0.416436439
## ESB_emo_m0 0.12327216 0.180156956
## ESB_emo_m1 0.11830000 0.199501419
## ESB_rat_m0 0.24963046 0.053257379
## ESB_rat_m1 0.18287578 0.042070734
## RSB_emo_m0 0.88509428 0.137666064
## RSB_emo_m1 0.89952954 0.138344527
## RSB_rat_m0 0.33492134 0.337748689
## RSB_rat_m1 0.28540558 0.252133574
## EMB_emo_m0 0.29931493 0.220839892
## EMB_emo_m1 0.29793699 0.212091204
## EMB_rat_m0 0.91919484 0.361916850
## EMB_rat_m1 0.85347883 0.362859156
```


```r
#make summary
df <- rbind(
  fit.params(RB_emo_m2),

  fit.params(RB_rat_m2),

  fit.params(ESB_emo_m2),

  fit.params(ESB_rat_m2),

  fit.params(RSB_emo_m2),

  fit.params(RSB_rat_m2),

  fit.params(EMB_emo_m2),

  fit.params(EMB_rat_m2)
)
write.table(df, file = paste0('models\\models_pars_nocontrols.csv'), sep=";", dec=",")
df
```

```
##            type      src group    r.squared adj.r.squared shapiro.p.value
## RB_emo_m2    lm emo_data   emo 0.0351474021   0.030463652    7.021981e-03
## RB_rat_m2    lm rat_data   rat 0.0095855383   0.004083236    1.815040e-03
## ESB_emo_m2   lm emo_data   emo 0.0172239159   0.012453158    2.733900e-03
## ESB_rat_m2   lm rat_data   rat 0.0576740628   0.052438919    8.040261e-03
## RSB_emo_m2   lm emo_data   emo 0.0001555665  -0.004698047    3.912548e-07
## RSB_rat_m2   lm rat_data   rat 0.0016784920  -0.003867739    1.048777e-07
## EMB_emo_m2   lm emo_data   emo 0.0010554698  -0.003793775    1.752698e-02
## EMB_rat_m2   lm rat_data   rat 0.0037343977  -0.001800411    1.716637e-02
##             bp.p.value   f.p.value
## RB_emo_m2  0.001516882 0.006695645
## RB_rat_m2  0.097530432 0.188549619
## ESB_emo_m2 0.026710838 0.058819040
## ESB_rat_m2 0.037629672 0.001092872
## RSB_emo_m2 0.752093750 0.858090235
## RSB_rat_m2 0.781909540 0.582916254
## EMB_emo_m2 0.169012654 0.641324362
## EMB_rat_m2 0.147869707 0.412500479
```





.
