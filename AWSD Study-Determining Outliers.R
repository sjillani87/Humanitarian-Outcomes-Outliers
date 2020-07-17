library(foreign)
library(stargazer)
library(xtable)
library(ggplot2)


setwd("~/Documents/Humanitarian Outcomes Study")
security_incidents_wounded_total<-na.omit(read.dta("security_incidents_wounded_total.dta"))
security_incidents_killings_total<-na.omit(read.dta("security_incidents_killings_total.dta"))
security_incidents_kidnappings_total<-na.omit(read.dta("security_incidents_kidnappings_total.dta"))

# CREATING HISTOGRAMS TO SHOW THE DATA DISTRIBUTION 

a <- ggplot(security_incidents_wounded_total, aes(x = change_wounded))
b <- ggplot(security_incidents_killings_total, aes(x = change_killed))
c <- ggplot(security_incidents_kidnappings_total, aes(x = change_kidnappings))

bw <- 2 * IQR(security_incidents_wounded_total$change_wounded) / length(security_incidents_wounded_total$change_wounded)^(1/3)
a + geom_histogram(binwidth=bw, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(change_wounded)), 
             linetype = "dashed", size = 0.6)+xlab("Change in Wounded")

bw <- 2 * IQR(security_incidents_killings_total$change_killed) / length(security_incidents_killings_total$change_killed)^(1/3)
b + geom_histogram(binwidth=bw, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(change_killed)), 
             linetype = "dashed", size = 0.6)+xlab("Change in Killings")

bw <- 2 * IQR(security_incidents_kidnappings_total$change_kidnappings) / length(security_incidents_kidnappings_total$change_kidnappings)^(1/3)
c + geom_histogram(binwidth=bw, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(change_kidnappings)), 
             linetype = "dashed", size = 0.6)+xlab("Change in Kidnappings")




# CREATING A FUNCTION TO IDENTIFY OUTLIERS


IQR.outliers <- function(x) {
  if(any(is.na(x)))
    stop("x is missing values")
  if(!is.numeric(x))
    stop("x is not numeric")
  Q3<-quantile(x,0.75)
  Q1<-quantile(x,0.25)
  IQR<-(Q3-Q1)
  left<- (Q1-(1.5*IQR))
  right<- (Q3+(1.5*IQR))
  c(x[x <left],x[x>right])
}

# USING THE FUNCTION TO IDENTIFY  OUTLIERS IN THE DATASETS
# USING XTABLE TO CREATE LATEX TABLES


#WOUNDED

positive_wounded_cutoff<-min(IQR.outliers(security_incidents_wounded_total$change_wounded)[which(IQR.outliers(security_incidents_wounded_total$change_wounded)>0)])
positive_wounded_outliers<-subset(security_incidents_wounded_total, change_wounded>=positive_wounded_cutoff)

keeps <- c("Year", "Country", "change_wounded")
print(xtable(positive_wounded_outliers[keeps][order(-positive_wounded_outliers$change_wounded),], digits=0), include.rownames=FALSE)

#KIDNAPPINGS

positive_kidnappings_cutoff<-min(IQR.outliers(security_incidents_kidnappings_total$change_kidnappings)[which(IQR.outliers(security_incidents_kidnappings_total$change_kidnappings)>0)])
positive_kidnappings_outliers<-subset(security_incidents_kidnappings_total, change_kidnappings>=positive_kidnappings_cutoff)

keeps <- c("Year", "Country", "change_kidnappings")
print(xtable(positive_kidnappings_outliers[keeps][order(-positive_kidnappings_outliers$change_kidnappings),], digits=0), include.rownames=FALSE)

#KILLINGS

positive_killing_cutoff<-min(IQR.outliers(security_incidents_killings_total$change_killed)[which(IQR.outliers(security_incidents_killings_total$change_killed)>0)])
positive_killing_outliers<-subset(security_incidents_killings_total, change_killed>=positive_killing_cutoff)

keeps <- c("Year", "Country", "change_killed")
print(xtable(positive_killing_outliers[keeps][order(-positive_killing_outliers$change_killed),], digits=0), include.rownames=FALSE)



