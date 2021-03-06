# PSYCH 372 - Assignment 1
### Connor Gaspar (20417636)
#### University of Waterloo, February 2017


##### SETUP
set.seed(372)
library(xlsx); library(ggplot2); library(ggthemes); library(psych)
library(ez); library(BayesFactor); library(knitr)
## Reading raw data
datS <- read.xlsx("p372 class data assignment 1.xlsx", sheetName="Sheet1", stringsAsFactors=F)


##### DATA MANIPULATION
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

## Outlier Analysis
scores <- (dat$Distance-mean(dat$Distance))/sd(dat$Distance) # Calculating z-scores
distDat <- dat[scores<=3,] # Removal of rows where distance scores is < 3 (4% of data)
anglDat <- distDat[distDat$Angle %in% seq(0, 180, 45),] # Excluding trials where angle was coded incorrectly.

anglDat$Angle <- as.factor(anglDat$Angle)
anglDat$Angle <- factor(anglDat$Angle, levels=c(0, 45, 90, 135, 180)) #Reordering



##### PLOTTING
## Plotting distributions
hist(dat$Distance, breaks = seq(0, 700, 10), col="red",
     main="Pre-outlier exclusion distribution", xlab = "Distance (cm)") # Pre-outlier analysis

hist(distDat$Distance, breaks = seq(0, 250, 10), col="red",
     main="Post-outlier exclusion distribution", xlab="Distance (cm)") # Post-outlier analysis

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

anglPlotDat1 <- aggregate(Distance ~ Angle, data=anglDat, mean)
anglPlotDat1$SD <- aggregate(Distance ~ Angle, data=anglDat, sd)[,"Distance"]
anglPlotDat1$n <- aggregate(Distance ~ Angle, data=anglDat, length)[,"Distance"]

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

for(i in 1:nrow(anglPlotDat1)){
        
        anglError <- qnorm(.975)*anglPlotDat1[i, "SD"]/sqrt(anglPlotDat1[i, "n"])
        anglPlotDat1[i, "Lower"] <- anglPlotDat1[i, "Distance"]-anglError
        anglPlotDat1[i, "Upper"] <- anglPlotDat1[i, "Distance"]+anglError
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

# Plotting Distance w.r.t. Angle
ggplot(data=anglPlotDat1, aes(x=Angle, y=Distance, fill=Angle))+
        geom_bar(stat="identity", position=position_dodge(), colour="black",
                 size=.2)+
        geom_errorbar(aes(ymin=Lower, ymax=Upper),
                      position=position_dodge(.9), width=.2)+
        scale_y_continuous(breaks=seq(0, 100, 10), limits=c(0, 100),
                           expand=c(0,0))+
        labs(x="Angle between Partners", y="Distance from Partner (cm)")+
        theme_base()+
        scale_fill_brewer(type="seq", palette="Greens")+
        theme(plot.margin=unit(c(.5,.25,.25,.5), "cm"),
              axis.title.y=element_text(margin=margin(0,15,0,0)),
              legend.title=element_blank())


##### QUICK STATS
## Difference in Distance BY Sex

# Men -- Women
t.test(distDat[distDat$Sex=="Men", "Distance"],
       distDat[distDat$Sex=="Women", "Distance"],
       paired=F)

ttestBF(distDat[distDat$Sex=="Men", "Distance"],
        distDat[distDat$Sex=="Women", "Distance"],
        paired=F)
# Men -- Mixed
t.test(distDat[distDat$Sex=="Men", "Distance"],
       distDat[distDat$Sex=="Mixed", "Distance"],
       paired=F)

ttestBF(distDat[distDat$Sex=="Men", "Distance"],
        distDat[distDat$Sex=="Mixed", "Distance"],
        paired=F)

## Difference in Distance BY Position

# Standing -- Seated
t.test(distDat[distDat$Position=="Standing", "Distance"],
       distDat[distDat$Position=="Sitting", "Distance"],
       paired=F)

ttestBF(distDat[distDat$Sex=="Men", "Distance"],
        distDat[distDat$Sex=="Mixed", "Distance"],
        paired=F)

####

## Two-way ANOVAs 

# Distance ~ Sex*Position

ezANOVA(data=distDat,
        dv=Distance,
        wid=Unique,
        between=.(Position, Sex),
        detailed=T)

anovaBF(Distance ~ Position*Sex, data=distDat, progress=F)

# Distance ~ Position*Angle
ezANOVA(data=anglDat,
        dv=Distance,
        wid=Unique,
        between=.(Position, Angle),
        detailed=T)

anovaBF(Distance ~ Position*Angle, data=anglDat, progress=F)

# Distance ~ Angle regression
lm(Distance ~ as.numeric(Angle), data=anglDat)