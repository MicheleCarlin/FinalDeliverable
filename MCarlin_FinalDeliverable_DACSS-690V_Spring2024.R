

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
  labs(title='2019-2023 UMass Chan Medical School Applicants', subtitle='by Race (US Permanent Residents Only)',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  scale_y_continuous(breaks=c(0,10,20,30,40,50), limits = c(0, 50)) +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label=paste0(round(Percent, 2), "%") ), position=position_dodge(width=0.9), vjust=-0.5, size=3) +
  scale_x_discrete(limits = c("White", "Asian", "Hispanic", "Black or African American", "Not Specified", "American Indian or Alaskan Native", "Native Hawaiian or Other Pacific Islander"),
                   labels = scales::label_wrap(8)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()) +
  theme(legend.title = element_text(size=8)) 
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
  labs(title='2019-2023 UMass Chan Medical School Applicants', subtitle='Average MCAT Total Score',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.title = element_text(size=8)) 
plot2
saveRDS(plot2, file = "Plot2_Univariate_Numeric.rds")


## CATEGORICAL/NUMERICAL or NUMERICAL/NUMERICAL DATA.

# Plot 3: GPA by Race, MCAT Total Score by Race

tapply(Admissions_df$`GPA-Total`,Admissions_df$Race, summary)
RaceFreq=table(Admissions_df$Race)
RaceFreq

plot3a = ggplot(Admissions_df, aes(x=`MCAT Total Score`, y=Race)) +
  geom_violin() + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "blue") +
  annotate(geom='text', color='blue', label="Cutoff for Acceptance = 508", width = 20, indent = 0,
           y = 2.5, x=495, angle=0, size=2.5) +
  theme_classic() +
  geom_vline(xintercept = 508, color='blue', linetype="dotdash",  size=1) +
  scale_y_discrete(limits = c("White", "Asian", "Native Hawaiian or Other Pacific Islander", "Hispanic", "Black or African American", "American Indian or Alaskan Native")) +
  labs(title='2019-2023 UMass Chan Medical School Applicants', subtitle='Average MCAT Total Score by Race',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  theme(title=element_text(size=8))
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


Race_Aggr=aggregate(data=Admissions_df,cbind(`GPA-Science`,`GPA-NonScience`,`GPA-Total`,`MCAT Total Score`)~Race,median)

library(ggrepel)
cor.test(Race_Aggr$`GPA-Science`,Race_Aggr$`MCAT Total Score`,method = "pearson")
plot3C = ggplot(Race_Aggr, aes(x=`GPA-Science`,y=`MCAT Total Score`)) +
  geom_point() +
  geom_text_repel(aes(label=Race),size=2) +
  xlim(c(3.0, 4.0)) +
  theme_classic() +
  labs(title='MCAT Total Score and Average GPA (Science)', subtitle='by URM Status',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
plot3C
saveRDS(plot3C, file = "Plot3C_Bivariate_Numeric-Numeric.rds")

cor.test(Race_Aggr$`GPA-NonScience`,Race_Aggr$`MCAT Total Score`,method = "pearson")
plot3D = ggplot(Race_Aggr, aes(x=`GPA-NonScience`,y=`MCAT Total Score`)) +
  geom_point() +
  geom_text_repel(aes(label=Race),size=2) +
  xlim(c(3.0, 4.0)) +
  labs(title='UMass Chan Medical School', subtitle='MCAT Total Score and GPA (Non-Science) by URM Status',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
plot3D
saveRDS(plot3D, file = "Plot3D_Bivariate_Numeric-Numeric.rds")

cor.test(Race_Aggr$`GPA-Total`,Race_Aggr$`MCAT Total Score`,method = "pearson")
plot3E = ggplot(Race_Aggr, aes(x=`GPA-Total`,y=`MCAT Total Score`)) +
  geom_point() +
  geom_text_repel(aes(label=Race),size=2) +
  xlim(c(3.0, 4.0)) +
  labs(title='UMass Chan Medical School', subtitle='MCAT Total Score and Total GPA by URM Status',
       x = NULL, y = NULL, caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
plot3E
saveRDS(plot3E, file = "Plot3E_Bivariate_Numeric-Numeric.rds")



## Plot 4: Map 
names(Admissions_df)[names(Admissions_df) == "Legal Residence"] <- "region"
StateFreq=table(Admissions_df$region)
StateFreq

StateProp=prop.table(StateFreq)*100
StateProp

# as data frame
(StateTable=as.data.frame(StateFreq))
# renaming data frame columns
names(StateTable)=c("Code","Percent")
# adding percents:
StateTable$Percent=as.vector(StateProp)
# then, you have:
StateTable


States_abbr <- read.csv("States_abbreviations.csv")
head(States_abbr,10)
Merge_df=merge(StateTable,States_abbr,
               by.x='Code', by.y='Abbreviation')

names(Merge_df)[names(Merge_df) == "State"] <- "region"
Merge_df$region <- tolower(Merge_df$region)



Merge_df <- Merge_df %>%
  mutate(value = case_when(
    Percent < 1.00 ~ "< 1%",
    Percent >= 1.00 & Percent < 4.00 ~ "1 - 3.99%",
    Percent >= 4.00 & Percent < 10.00 ~ "4 - 9.99%",
    Percent >= 10.00 ~ ">= 10%"))
table(select(Merge_df, value))

Merge_df$value <- factor(Merge_df$value, levels = c("< 1%", "1 - 3.99%", "4 - 9.99%", ">= 10%"))

library(choroplethrMaps)
library(choroplethr)




plot4 <- state_choropleth(Merge_df) + 
  scale_fill_brewer(name="Percent of applicants \nfrom each state:", palette="Blues") +
  labs(title='2019-2023 UMass Chan Medical Schools Applicants', subtitle='by Legal Residence State',
      caption = 'Source: UMass Chan Medical School Institutional Research Data') +
  theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
plot4

c = StateChoropleth$new(Merge_df)
c$title = "2019-2023 UMass Chan Medical Schools Applicants \nby Legal Residence State"
c$legend = "Percent of applicants \nfrom each state"
c$set_num_colors(7)
c$set_zoom(NULL)
c$show_labels = FALSE
plot4a = c$render()
plot4a
plot4 <- plot4a + labs(title='2019-2023 UMass Chan Medical School Applicants', subtitle='by Legal Residence State', caption = 'Source: UMass Chan Medical School Institutional Research Data') +
   theme(plot.caption = element_text(hjust = 0), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
   theme(legend.title = element_text(size=8)) 
plot4

saveRDS(plot4, file = "Plot4_Choropleth.rds")

