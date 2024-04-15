

setwd('C:/Users/CarlinML/DACSS-690V/FinalDeliverable/')
library(readxl)

sheet_names <- excel_sheets("DACSS-690V_dataset.xlsx")
sheet_names 

Admissions_df<-read_xlsx("DACSS-690V_dataset.xlsx", "Sheet1")

library(summarytools)
dfSummary(Admissions_df)

## CATEGORICAL DATA.

# Plot 1A: Percent of Applicants by Race
Admissions_df$Race[Admissions_df$Race == "Unknown"] <- "Not Specified"
Admissions_df$Race[Admissions_df$Race == "Hispanics of any Race"] <- "Hispanic"
Admissions_df$Race[Admissions_df$Race == "American Indian or Native Alaskan"] <- "American Indian or Alaskan Native"

RaceFreq=table(Admissions_df$Race)
RaceFreq

RaceProp=prop.table(RaceFreq)*100
RaceProp

# as data frame
(RaceTable=as.data.frame(RaceFreq))
# renaming data frame columns
names(RaceTable)=c("Race","Count")
# adding percents:
RaceTable$Percent=as.vector(RaceProp)
# then, you have:
RaceTable

# specifying data to be included on plot:
library(ggplot2)
library(tidyverse)
library(stringr)
plot1a= ggplot(data=RaceTable, aes(x =Race, y =Percent)) + 
  geom_bar(fill ="navy", stat = 'identity') +
  theme_classic() +
  labs(title='UMass Chan Medical School', subtitle='2019-2023 Applicants by Race - US Permanent Residents',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  scale_y_continuous(breaks=c(0,10,20,30,40,50), limits = c(0, 50)) +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label=paste0(round(Percent, 2), "%") ), position=position_dodge(width=0.9), vjust=-0.5, size=3) +
  scale_x_discrete(limits = c("White", "Asian", "Hispanic", "Black or African American", "Not Specified", "American Indian or Alaskan Native", "Native Hawaiian or Other Pacific Islander"),
                   labels = scales::label_wrap(8)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())
plot1a
saveRDS(plot1a, file = "Plot1A_Univariate_Categorical.rds")


# Plot 1B: Total Number of Applicants by Admit Term
AdmitFreq=table(Admissions_df$`Admit Term`)
AdmitFreq

# as data frame
(AdmitTable=as.data.frame(AdmitFreq))
# renaming data frame columns
names(AdmitTable)=c("AdmitTerm","Count")
AdmitTable

plot1b= ggplot(data=AdmitTable, aes(x =AdmitTerm, y =Count)) + 
  geom_bar(fill ="navy", stat = 'identity') +
  theme_classic() +
  labs(title='UMass Chan Medical School', subtitle='Total Number of Applicants by Admit Term',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  scale_y_continuous(breaks=c(0,1000,2000,3000,4000,5000,6000), limits = c(0, 6000)) +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label=Count), position=position_dodge(width=0.9), vjust=-0.25, size=3)
plot1b
saveRDS(plot1b, file = "Plot1B_Univariate_Categorical.rds")

## NUMERICAL DATA.

# Plot 2: Average Total MCAT Score

# get all the summary values but the count of NAs.
(statVals=summary(Admissions_df$`MCAT Total Score`,digits = 3)[1:6])

library(magrittr)
# the summary values as vector
statVals=statVals%>%as.vector() 

plot2 = ggplot(Admissions_df,aes(y = `MCAT Total Score`)) + 
  geom_boxplot() +
  scale_y_continuous(breaks = statVals) +
  coord_flip() +
  geom_hline(yintercept = 508, color='blue', linetype="dotdash",  size=1) +
  annotate(geom='text', color='blue', label=str_wrap(paste0("Cutoff for Acceptance = 508"), width = 10, indent = 0),
           y = 501, x=-0.25, angle=0, size=2.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 40, size = 8, vjust = 0.5)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()) +
  labs(title='UMass Chan Medical School', subtitle='2019-2023 Applicants: MCAT Total Score',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
plot2
saveRDS(plot2, file = "Plot2_Univariate_Numeric.rds")

## CATEGORICAL/NUMERICAL DATA.

# Plot 3: GPA by Race, MCAT Total Score by Race

tapply(Admissions_df$`GPA-Total`,Admissions_df$Race, summary)
RaceFreq=table(Admissions_df$Race)
RaceFreq

plot3a = ggplot(Admissions_df, aes(x=`MCAT Total Score`, y=Race)) +
  geom_violin() + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "blue") +
  theme_classic() +
  geom_vline(xintercept = 508, color='blue', linetype="dotdash",  size=1) +
  scale_y_discrete(limits = c("White", "Asian", "Native Hawaiian or Other Pacific Islander", "Hispanic", "Black or African American", "American Indian or Alaskan Native")) +
  labs(title='UMass Chan Medical School', subtitle='Average MCAT Total Score by Race',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
plot3a
saveRDS(plot3a, file = "Plot3A_Bivariate_Cat-Numeric.rds")

Admissions_df$URM[Admissions_df$URM == "Y"] <- "URM"
Admissions_df$URM[Admissions_df$URM == "N"] <- "Non-URM"
plot3b = ggplot(Admissions_df, aes(x=`MCAT Total Score`, y=URM)) +
  geom_violin() + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "blue") +
  theme_classic() +
  labs(title='UMass Chan Medical School', subtitle='Average MCAT Total Score by Under-Represented Minority (URM) Status',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
plot3b
saveRDS(plot3b, file = "Plot3B_Bivariate_Cat-Numeric.rds")

## NUMERICAL/NUMERICAL DATA.

# Plot 4: GPA by MCAT Total Score

Race_Aggr=aggregate(data=Admissions_df,cbind(`GPA-Science`,`GPA-NonScience`,`GPA-Total`,`MCAT Total Score`)~Race,median)

library(ggrepel)
cor.test(Race_Aggr$`GPA-Science`,Race_Aggr$`MCAT Total Score`,method = "pearson")
plot4a = ggplot(Race_Aggr, aes(x=`GPA-Science`,y=`MCAT Total Score`)) +
  geom_point() +
  geom_text_repel(aes(label=Race),size=2) +
  xlim(c(3.0, 4.0)) +
  theme_classic() +
  labs(title='UMass Chan Medical School', subtitle='MCAT Total Score and GPA (Science) by URM Status',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
plot4a
saveRDS(plot4a, file = "Plot4A_Bivariate_Numeric-Numeric.rds")

cor.test(Race_Aggr$`GPA-NonScience`,Race_Aggr$`MCAT Total Score`,method = "pearson")
plot4b = ggplot(Race_Aggr, aes(x=`GPA-NonScience`,y=`MCAT Total Score`)) +
  geom_point() +
  geom_text_repel(aes(label=Race),size=2) +
  xlim(c(3.0, 4.0)) +
  labs(title='UMass Chan Medical School', subtitle='MCAT Total Score and GPA (Non-Science) by URM Status',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
plot4b
saveRDS(plot4b, file = "Plot4B_Bivariate_Numeric-Numeric.rds")

cor.test(Race_Aggr$`GPA-Total`,Race_Aggr$`MCAT Total Score`,method = "pearson")
plot4c = ggplot(Race_Aggr, aes(x=`GPA-Total`,y=`MCAT Total Score`)) +
  geom_point() +
  geom_text_repel(aes(label=Race),size=2) +
  xlim(c(3.0, 4.0)) +
  labs(title='UMass Chan Medical School', subtitle='MCAT Total Score and Total GPA by URM Status',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
plot4c
saveRDS(plot4c, file = "Plot4C_Bivariate_Numeric-Numeric.rds")

## Add facet for admitted (y/n) to MCAT v GPA plot