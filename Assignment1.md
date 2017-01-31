# PSYCH 372 - Assignment 1
### Connor Gaspar (20417636)
#### University of Waterloo, February 2017



```r
set.seed(372)
library(xlsx); library(ggplot2); library(ggthemes);
library(psych); library(BayesFactor); library(knitr)
## Reading raw data
datS <- read.xlsx("p372 class data assignment 1.xlsx", sheetName="Sheet1", stringsAsFactors=F)
```


```r
## Organizing raw data
datS[c(41, 82),] <- NA # Removing header rows from data
datS1 <- datS[,1:5] 
datS2 <- datS[,6:10] 
colnames(datS2) <- colnames(datS1)
dat <- rbind(datS1, datS2)
dat <- dat[complete.cases(dat),] # Removing NA rows from data
dat$Unique <- as.factor(seq(1, nrow(dat), 1)) # Adding unique observation ID

## Cleaning raw data
dat$EntryNo <- gl(34, 5) # Providing individual entry ID's
dat <- dat[, c(6, 1:5)] # Reordering columns
dat$Distance <- gsub("'", "", dat$Distance) #Removing apostrophes from data
dat$Distance <- as.numeric(dat$Distance) # Reclassing Distance column
dat$Distance <- dat$Distance*10 # Expanding the values to multiples of 10cm

## Reclassing raw data
dat$Position <- as.factor(dat$Position) 
levels(dat$Position) <- c("Sitting", "Standing")
dat$Sex <- as.factor(dat$Sex) 
levels(dat$Sex) <- c("Women", "Men", "Mixed")
dat$Angle <- as.numeric(dat$Angle)

## Outlier Analysis
scores <- (dat$Distance-mean(dat$Distance))/sd(dat$Distance) # Calculating z-scores
distDat <- dat[scores<=3,] # Removal of rows where distance scores is < 3 (4% of data)
distDat <- distDat[distDat$Angle %in% seq(0, 180, 45),] # Excluding trials where angle was coded incorrectly.
```




```r
## Plotting distributions
hist(dat$Distance, breaks = seq(0, 700, 10), col="red",
     main="Pre-outlier exclusion distribution", xlab = "Distance (cm)") # Pre-outlier analysis
```

![plot of chunk Plotting](figure/Plotting-1.png)

```r
hist(distDat$Distance, breaks = seq(0, 250, 10), col="red",
     main="Post-outlier exclusion distribution", xlab="Distance (cm)") # Post-outlier analysis
```

![plot of chunk Plotting](figure/Plotting-2.png)

```r
## Creating distance plot data #
distPlotDat1 <- aggregate(Distance ~ Sex+Position, data=distDat, mean)
distPlotDat1$SD <- aggregate(Distance ~ Sex+Position, data=distDat, sd)[,"Distance"]
distPlotDat1$n <- aggregate(Distance ~ Sex+Position, data=distDat, length)[,"Distance"]

distPlotDat2 <- aggregate(Distance ~ Sex, data=distDat, mean)
distPlotDat2$SD <- aggregate(Distance ~ Sex, data=distDat, sd)[,"Distance"]
distPlotDat2$n <- aggregate(Distance ~ Sex, data=distDat, length)[,"Distance"]

distPlotDat3 <- aggregate(Distance ~ Position, data=distDat, mean)
distPlotDat3$SD <- aggregate(Distance ~ Position, data=distDat, sd)[,"Distance"]
distPlotDat3$n <- aggregate(Distance ~ Position, data=distDat, length)[,"Distance"]

# Calculating CI's
for(i in 1:nrow(distPlotDat1)){
        
        distError <- qnorm(.975)*distPlotDat1[i, "SD"]/sqrt(distPlotDat1[i, "n"])
        distPlotDat1[i, "Lower"] <- distPlotDat1[i, "Distance"]-distError
        distPlotDat1[i, "Upper"] <- distPlotDat1[i, "Distance"]+distError
}

for(i in 1:nrow(distPlotDat2)) {
        
        distError2 <- qnorm(.975)*distPlotDat2[i, "SD"]/sqrt(distPlotDat2[i, "n"])
        distPlotDat2[i, "Lower"] <- distPlotDat2[i, "Distance"]-distError2
        distPlotDat2[i, "Upper"] <- distPlotDat2[i, "Distance"]+distError2
}

for(i in 1:nrow(distPlotDat3)){
        
        distError3 <- qnorm(.975)*distPlotDat3[i, "SD"]/sqrt(distPlotDat3[i, "n"])
        distPlotDat3[i, "Lower"] <- distPlotDat3[i, "Distance"]-distError3
        distPlotDat3[i, "Upper"] <- distPlotDat3[i, "Distance"]+distError3
}

# Plotting Distance w.r.t Position
ggplot(data=distPlotDat3, aes(x=Position, y=Distance, fill=Position))+
               geom_bar(stat="identity", position=position_dodge(.7), colour="black",
                        size=.2, width=.7)+
               geom_errorbar(aes(ymin=Lower, ymax=Upper),
                             position=position_dodge(.9), width=.2)+
               scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60),
                                  expand=c(0,0))+
               labs(x="Pair Position",
                    y="Distance from Partner (cm)")+
               theme_base()+
               scale_fill_brewer(palette="Dark2")+
               theme(plot.margin=unit(c(.5,.25,.25,.5), "cm"),
                     axis.title.y=element_text(margin=margin(0,15,0,0)),
                     legend.title=element_blank())
```

![plot of chunk Plotting](figure/Plotting-3.png)

```r
# Plotting Distance w.r.t. Sex
ggplot(data=distPlotDat2, aes(x=Sex, y=Distance, fill=Sex))+
               geom_bar(stat="identity", position=position_dodge(), colour="black",
                        size=.2)+
               geom_errorbar(aes(ymin=Lower, ymax=Upper),
                             position=position_dodge(.9), width=.2)+
               scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60),
                                  expand=c(0,0))+
               labs(x="Group Gender Pairs",
                    y="Distance from Partner (cm)")+
               theme_base()+
               scale_fill_brewer(palette="Set1")+
               theme(plot.margin=unit(c(.5,.25,.25,.5), "cm"),
                     axis.title.y=element_text(margin=margin(0,15,0,0)),
                     legend.title=element_blank())
```

![plot of chunk Plotting](figure/Plotting-4.png)

```r
# Plotting Distance w.r.t. Position grouped by Sex 
ggplot(data=distPlotDat1, aes(x=Position, y=Distance, fill=Sex))+
        geom_bar(stat="identity", position=position_dodge(), colour="black",
                 size=.2)+
        geom_errorbar(aes(ymin=Lower, ymax=Upper),
                          position=position_dodge(.9), width=.2)+
        scale_y_continuous(breaks=seq(0, 80, 10), limits=c(0, 80), 
                           expand=c(0,0))+
        labs(x="Group Gender Pairs",
             y="Distance from Partner (cm)")+
        theme_base()+ 
        scale_fill_brewer(palette = "Set1")+
        theme(plot.margin=unit(c(.5,.25,.25,.5), "cm"),
              axis.title.y=element_text(margin=margin(0,15,0,0)),
              legend.title=element_blank())
```

![plot of chunk Plotting](figure/Plotting-5.png)


```r
## Difference in Distance BY Sex

# Men -- Women
t.test(distDat[distDat$Sex=="Men", "Distance"],
       distDat[distDat$Sex=="Women", "Distance"],
       paired=F)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  distDat[distDat$Sex == "Men", "Distance"] and distDat[distDat$Sex == "Women", "Distance"]
## t = 0.13085, df = 96.795, p-value = 0.8962
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -12.16240  13.87919
## sample estimates:
## mean of x mean of y 
##  47.26190  46.40351
```

```r
ttestBF(distDat[distDat$Sex=="Men", "Distance"],
       distDat[distDat$Sex=="Women", "Distance"],
       paired=F)
```

```
## Bayes factor analysis
## --------------
## [1] Alt., r=0.707 : 0.2154242 ±0.03%
## 
## Against denominator:
##   Null, mu1-mu2 = 0 
## ---
## Bayes factor type: BFindepSample, JZS
```

```r
# Men -- Mixed
t.test(distDat[distDat$Sex=="Men", "Distance"],
       distDat[distDat$Sex=="Mixed", "Distance"],
       paired=F)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  distDat[distDat$Sex == "Men", "Distance"] and distDat[distDat$Sex == "Mixed", "Distance"]
## t = 0.51236, df = 95.998, p-value = 0.6096
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -9.837324 16.682562
## sample estimates:
## mean of x mean of y 
##  47.26190  43.83929
```

```r
ttestBF(distDat[distDat$Sex=="Men", "Distance"],
       distDat[distDat$Sex=="Mixed", "Distance"],
       paired=F)
```

```
## Bayes factor analysis
## --------------
## [1] Alt., r=0.707 : 0.2389557 ±0.03%
## 
## Against denominator:
##   Null, mu1-mu2 = 0 
## ---
## Bayes factor type: BFindepSample, JZS
```

```r
## Difference in Distance BY Position

# Standing -- Seated
t.test(distDat[distDat$Position=="Standing", "Distance"],
       distDat[distDat$Position=="Sitting", "Distance"],
       paired=F)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  distDat[distDat$Position == "Standing", "Distance"] and distDat[distDat$Position == "Sitting", "Distance"]
## t = -0.35361, df = 126.14, p-value = 0.7242
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -13.604811   9.479853
## sample estimates:
## mean of x mean of y 
##  44.83146  46.89394
```

```r
ttestBF(distDat[distDat$Sex=="Men", "Distance"],
       distDat[distDat$Sex=="Mixed", "Distance"],
       paired=F)
```

```
## Bayes factor analysis
## --------------
## [1] Alt., r=0.707 : 0.2389557 ±0.03%
## 
## Against denominator:
##   Null, mu1-mu2 = 0 
## ---
## Bayes factor type: BFindepSample, JZS
```

```r
####

## Omnibus ANOVA (Distance ~ Sex+Position)
ezANOVA(data=distDat,
        dv=Distance,
        wid=Unique,
        between=.(Position, Sex),
        detailed=T)
```

```
## $ANOVA
##         Effect DFn DFd       SSn      SSd         F          p p<.05
## 1     Position   1 149  208.0741 181422.9 0.1708883 0.67991793      
## 2          Sex   2 149  371.4117 181422.9 0.1525175 0.85867772      
## 3 Position:Sex   2 149 5966.3990 181422.9 2.4500583 0.08975844      
##           ges
## 1 0.001145587
## 2 0.002043032
## 3 0.031839590
## 
## $`Levene's Test for Homogeneity of Variance`
##   DFn DFd      SSn      SSd        F         p p<.05
## 1   5 149 4821.903 122812.9 1.170013 0.3266291
```

```r
anovaBF(Distance ~ Position*Sex, data=distDat, progress=F)
```

```
## Bayes factor analysis
## --------------
## [1] Position                      : 0.185753   ±0%
## [2] Sex                           : 0.07251301 ±0.02%
## [3] Position + Sex                : 0.01335163 ±1.65%
## [4] Position + Sex + Position:Sex : 0.01075415 ±3.15%
## 
## Against denominator:
##   Intercept only 
## ---
## Bayes factor type: BFlinearModel, JZS
```


```r
knit2html("Assignment1.Rmd", quiet = T)
```

```
## Error in parse_block(g[-1], g[1], params.src): duplicate label 'Setup'
```
