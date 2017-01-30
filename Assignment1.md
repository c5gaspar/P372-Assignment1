###Assignment 1
####PSYCH 372
##### Connor Gaspar (20417636)


```r
library(xlsx); library(ggplot2); library(ggthemes);
library(psych); library(knitr)
## Reading raw data
datS <- read.xlsx("p372 class data assignment 1.xlsx", sheetName="Sheet1", stringsAsFactors=F)

## Organizing raw data
datS[c(41, 82),] <- NA # Removing header rows from data
datS1 <- datS[,1:5]; datS2 <- datS[,6:10]; colnames(datS2) <- colnames(datS1)
dat <- rbind(datS1, datS2)
dat <- dat[complete.cases(dat),] # Removing NA rows from data

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
```




```r
## Plotting distributions
hist(dat$Distance, breaks = seq(0, 700, 10), col="red",
     main="Pre-outlier exclusion distribution") # Pre-outlier analysis
```

![plot of chunk Plotting](figure/Plotting-1.png)

```r
hist(distDat$Distance, breaks = seq(0, 250, 10), col="red",
     main="Post-outlier exclusion distribution") # Post-outlier analysis
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
knit2html("Assignment1.Rmd")
```

```
## Error in parse_block(g[-1], g[1], params.src): duplicate label 'Setup'
```