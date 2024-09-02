pulse<- read.csv("Pulse.csv", header=T)

# more about the data
View(pulse)
names(pulse)

pulse$PULSE1
is.na(pulse$PULSE1)
which(is.na(pulse$PULSE1))
sum(is.na(pulse$PULSE1))


mean(pulse$PULSE1, na.rm = T)
median(pulse$PULSE1, na.rm = T) 
range(pulse$PULSE1, na.rm = T) 
min(pulse$PULSE1, na.rm = T) 
max(pulse$PULSE1, na.rm = T)
max(pulse$PULSE1, na.rm = T) - min(pulse$PULSE1, na.rm = T) ## 98


## function to calculate the range
my.range <- function(x){
  return(max(x, na.rm = T) - min(x, na.rm = T))
}

my.range(pulse$PULSE1)

## dispersion measures
## mean absolute deviation function
mean.abs.dev <- function(x){
  return(mean(abs(x - mean(x, na.rm = T)), na.rm = TRUE))
}

mean.abs.dev(pulse$PULSE1) 
var(pulse$PULSE1 , na.rm = T)
sd(pulse$PULSE1 , na.rm = T)
IQR(pulse$PULSE1, na.rm = T)

## Calculating quartiles/percentiles using the quantile function
quantile(pulse$PULSE1, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)

#overall summaries
summary(pulse$PULSE1)

pulse$RAN == 0

## proportion assigned sit/ran
mean(pulse$RAN == 0)
mean(pulse$RAN == 1)

## the table approach to produce a frequency table
table(pulse$RAN)
table(pulse$RAN, useNA = 'always')

table(pulse$RAN)/sum(table(pulse$RAN))

## dispersion for categorical data
#Variance
mean(pulse$RAN == 0) * (1 - mean(pulse$RAN == 0))/length(pulse$RAN) 
prod((table(pulse$RAN)/sum(table(pulse$RAN)))) / length(pulse$RAN)

#Standard deviation
sqrt(mean(pulse$RAN == 0) * (1 - mean(pulse$RAN == 0))/length(pulse$RAN)) 
sqrt(prod((table(pulse$RAN)/sum(table(pulse$RAN)))) / length(pulse$RAN))


## graphical displays
## boxplot of resting pulse rate, and by sex

boxplot(pulse$PULSE1)
boxplot(pulse$PULSE1, 
        ylab = 'Resting pulse rate (bpm)',
        main = 'Boxplot of Resting Pulse Rates')

boxplot(pulse$PULSE1 ~ pulse$SEX)
boxplot(pulse$PULSE1 ~ pulse$SEX,
        names = c('Male', 'Female'),
        xlab = 'Sex',
        ylab = 'Resting pulse rate (bpm)',
        main = 'Boxplot of Resting Pulse Rates by Sex')

## histograms of resting pulse rate, and by sex

hist(pulse$PULSE1)
hist(pulse$PULSE1,
     xlab = 'Resting pulse rate (bpm)',
     main = 'Histogram of Resting Pulse Rate')

## by sex, overlaid
hist(pulse$PULSE1[pulse$SEX == 0], ## males
     freq = F,
     xlab = 'Resting pulse rate (bpm)',
     main = 'Histogram of Resting Pulse Rates by Sex') 
hist(pulse$PULSE1[pulse$SEX == 1], ## females
     freq = F,
     col = gray(0.5),
     add = T)

##panel
par(mfrow = c(2,1))
hist(pulse$PULSE1[pulse$SEX == 0]) ## males
hist(pulse$PULSE1[pulse$SEX == 1]) ## females

par(mfrow=c(1,1))
## nicer higher quality visuals
hist(pulse$PULSE1[pulse$SEX == 0], ## males
     xlim = range(pulse$PULSE1, na.rm = T),
     ylim = c(0,30),
     xlab = 'Resting pulse rate (bpm)',
     main = 'Histogram of Resting Pulse Rate (Males)'
)
hist(pulse$PULSE1[pulse$SEX == 1], ## females
     xlim = range(pulse$PULSE1, na.rm = T),
     ylim = c(0,30),
     xlab = 'Resting pulse rate (bpm)',
     main = 'Histogram of Resting Pulse Rate (Females)'
)

## Density plots
par(mfrow = c(1,1))
plot(density(pulse$PULSE1,na.rm=TRUE), xlab="Resting pulse rate", main="Density plot of resting pulse rates") 

# Plot density plots of pulse rate by sex on the same graph 
plot(density(pulse$PULSE1[pulse$SEX == 0],na.rm=TRUE), xlab="Resting pulse rate", main="Density plot of resting pulse rates") ## males     
points(density(pulse$PULSE1[pulse$SEX == 1],na.rm=TRUE),col=2,type="l") ## Females     

## qq plots
par(mfrow = c(1,1))
qqnorm(pulse$PULSE1)
qqline(pulse$PULSE1)

##bar plot of whether people sat or ran, prop sat and ran(categorical)
barplot(table(pulse$RAN)/sum(table(pulse$RAN)))
barplot(table(pulse$RAN)/sum(table(pulse$RAN)),
        names.arg = c('Sat', 'Ran'),
        xlab = 'Did student sit or run?',
        main = 'Bar plot of whether a student sat or ran')




#####################################################################
str(pulse) # structure 
# mean and sd of pulse1 for males and females
pulse$SEX<-factor(pulse$SEX)
pulse$SEX
levels(pulse$SEX)<-c("Male", "Female") 
pulse$SEX
mean_female<-mean(pulse$PULSE1[pulse$SEX=="Female"],na.rm=T)
mean_female
mean_male<-mean(pulse$PULSE1[pulse$SEX=="Male"])
mean_male
sd_female<-sd(pulse$PULSE1[pulse$SEX=="Female"],na.rm=T)
sd_female
sd_male<-sd(pulse$PULSE1[pulse$SEX=="Male"])
sd_male

# Excluding the obs > 100bpm
mean_female_less.than.100<-mean(pulse$PULSE1[pulse$SEX=="Female" & pulse$PULSE1<= 100],
                                na.rm=T)


mean_female_less.than.100
mean_male_less.than.100<-mean(pulse$PULSE1[pulse$SEX=="Male" & 
                                             pulse$PULSE1<= 100])#72.931
mean_male_less.than.100
sd_female_less.than.100<-sd(pulse$PULSE1[pulse$SEX=="Female" & 
                                           pulse$PULSE1<= 100],na.rm=T)#9.88
sd_female_less.than.100
sd_male_less.than.100<-sd(pulse$PULSE1[pulse$SEX=="Male" & 
                                         pulse$PULSE1<= 100])#10.151
sd_male_less.than.100


round(sd_female,digits = 2)

Mean_table<-data.frame(Measure=c("Mean_female","Mean_male", "Mean_female_less100",
                                 "Mean_male_less100"),
                       Value= c(mean_female,mean_male, mean_female_less.than.100,
                                mean_male_less.than.100))
Mean_table
sd_table<-data.frame(Measure=c("sd_female","sd_male", "sd_female_less100",
                               "sd_male_less100"),
                     Value= c(sd_female,sd_male, sd_female_less.than.100,
                              sd_male_less.than.100))
sd_table


median(pulse$PULSE1[pulse$SEX=="Female"],na.rm=T)
median(pulse$PULSE1[pulse$SEX=="Male"],na.rm=T)
IQR(pulse$PULSE1[pulse$SEX=="Female"], na.rm = T)
IQR(pulse$PULSE1[pulse$SEX=="Male"], na.rm = T)


length(pulse$PULSE1)
which(pulse$PULSE1>100)


############################################################
# Function to calculate CV (coefficient of variation)
CV<-function(x)
{
  cv=sd(x,na.rm=T)/mean(x,na.rm=T)
  return(cv)
}
# CV for resting pulse rate
CV(pulse$PULSE1)

cv_f<-CV(pulse$PULSE1[pulse$SEX=="Female"])
cv_f
cv_m<-CV(pulse$PULSE1[pulse$SEX=="Male"])
cv_m
data.frame(Sex= c("Male","female"),CV=c(cv_m,cv_f))

###########################################################
## QQ plots for resting pulse rates by sex
par(mfrow=c(2,1))
qqnorm(pulse$PULSE1[pulse$SEX=="Female"],
       main="Pulse rate for females")
qqline(pulse$PULSE1[pulse$SEX=="Female"])
qqnorm(pulse$PULSE1[pulse$SEX=="Male"],
       main="Pulse rate for Males")
qqline(pulse$PULSE1[pulse$SEX=="Male"])

##############################################
pulse.diff<-pulse$PULSE2-pulse$PULSE1

par(mfrow=c(1,1))
boxplot(pulse.diff~pulse$RAN, names=c("Sat", "Ran"), xlab="Did student sit or ran?", main="Boxplot of Difference in pulse rates whether a student Sat or Ran")

hist(pulse.diff[pulse$RAN == 0], 
     xlim = range(pulse.diff, na.rm = T),
     xlab = 'Difference in pulse rate (bpm)',
     main = 'Histogram of difference in Pulse Rate students who Sta'
)


hist(pulse.diff[pulse$RAN == 1], 
     xlim = range(pulse.diff, na.rm = T),
     xlab = 'Difference in pulse rate (bpm)',
     main = 'Histogram of difference in Pulse Rate students who RAN'
)


plot(density(pulse.diff[pulse$RAN == 0],na.rm=TRUE),xlim=range(pulse.diff,na.rm=TRUE), xlab="Difference in pulse rate", main="Density plot of difference in pulse rates") ## males     
points(density(pulse.diff[pulse$RAN == 1],na.rm=TRUE),col=2,type="l")    

## qq plots
par(mfrow = c(1,1))
qqnorm(pulse.diff[pulse$RAN == 1],main="QQ plot of difference in pulse rate \nfor students who Ran")
qqline(pulse.diff[pulse$RAN == 1])

par(mfrow = c(1,1))
qqnorm(pulse.diff[pulse$RAN ==0],main="QQ plot of difference in pulse rate \nfor students who Sat")
qqline(pulse.diff[pulse$RAN ==0])


mean(pulse.diff[pulse$RAN ==0], na.rm=TRUE)
sd(pulse.diff[pulse$RAN ==0], na.rm=TRUE)
mean(pulse.diff[pulse$RAN ==1], na.rm=TRUE)
sd(pulse.diff[pulse$RAN ==1], na.rm=TRUE)
