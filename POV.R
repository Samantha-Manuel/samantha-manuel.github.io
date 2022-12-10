# Setting up the new R environment, starting fresh, click run!
rm(list=ls())

#Set working directory to where data is stored
setwd("~/Documents/Fall 2022/EPPS 6356/POV Final Project")

#Download Excel Data
library(readxl)
POV <- read_excel("percep of vulnerability data 10.27.2022.xlsx", sheet = "clean_stata")
View(POV)                                         #view dataset

POV <- as.data.frame(POV) 

# Installing Packages
knitr::opts_chunk$set(error = TRUE, tidy = TRUE)

.cran_packages <- c("ggplot2", "sjPlot", "sjmisc", "gridExtra", "cowplot", "grid", 
                    "tables", "knitr", "pander", "tables", "knitcitations", "png", "downloader", 
                    "plyr", "data.table", "dplyr", "tidyr", "foreign", "coin", "car", "easyGgplot2", 
                    "likert", "readxl", "FSA", "plotly", "lattice", "rcompanion", "readr", "mvoutlier", 
                    "mvnormtest", "HH")

sapply(.cran_packages, require, character.only = TRUE)

# gender identity percentages and pie chart
POV %>% 
  group_by( genderid ) %>% 
  summarise( percent = 100 * n() / nrow(POV) )

table(POV$genderid)

GenderIDGroups = c("Cis-Female", "Cis-Male", "Non-Binary")
GenderIDValue = c(25, 18, 2)
gender_id_table <- data.frame(GenderIDGroups, GenderIDValue)
colnames(gender_id_table) <- c("Sexual Orientation", "Frequency")
gender_id_table
gender_id_table_percent <- gender_id_table %>%
  mutate(GenderIDGroups = factor(GenderIDGroups, 
                                  levels = GenderIDGroups[length(GenderIDGroups):1]),
         cumulative = cumsum(Frequency),
         midpoint = cumulative - Frequency / 2,
         labels = paste0(round((Frequency/ sum(Frequency)) * 100, 1), "%"))
gender_id_table_percent

ggplot(gender_id_table_percent, aes(x = "", y=GenderIDValue, fill=GenderIDGroups)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("steelblue", "firebrick", "yellowgreen")) +
  labs(x = "", y = "", title = "Gender Identity Distribution Frequencies by Percentage",
       fill = "Gender Identity") + 
  geom_text(aes(x = 1.2, y = midpoint , label = labels), color="black",
            fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10))

POV <- mutate(POV, cis_d=case_when(
  genderid == "Cis-Female" ~ "Cis-Gender",
  genderid == "Cis-Male" ~ "Cis-Gender",
  genderid == "Non-binary" ~ "Non-Binary"))
table(POV$cis_d)

gender_d_Groups = c("Cis-Gender", "Non-Binary")
gender_d_Value = c(43, 2)
gender_d_table <- data.frame(gender_d_Groups, gender_d_Value)
colnames(gender_d_table) <- c("Sexual Orientation", "Frequency")
gender_d_table
gender_d_table_percent <- gender_d_table %>%
  mutate(gender_d_Groups = factor(gender_d_Groups, 
                                 levels = gender_d_Groups[length(gender_d_Groups):1]),
         cumulative = cumsum(Frequency),
         midpoint = cumulative - Frequency / 2,
         labels = paste0(round((Frequency/ sum(Frequency)) * 100, 1), "%"))
gender_d_table_percent

ggplot(gender_d_table_percent, aes(x = "", y=gender_d_Value, fill=gender_d_Groups)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(x = "", y = "", title = "Gender Identity Distribution Frequencies by Percentage",
       fill = "Gender Identity") + 
  geom_text(aes(x = 1.3, y = midpoint , label = labels), color="black",
            fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10))

# sexual orientation percentages and pie chart
POV %>% 
  group_by( sexorient ) %>% 
  summarise( percent = 100 * n() / nrow( POV ) )

table(POV$sexorient)

SexOrientGroups = c("Bisexual (8.9%)", "Gay (2.2%)", "Heterosexual (82.2%)", 
               "Other (4.4%)", "Pansexual (2.2%)")
SexOrientValue = c(4, 1, 37, 2, 1)
sexorient_table <- data.frame(SexOrientGroups, SexOrientValue)
colnames(sexorient_table) <- c("Sexual Orientation", "Frequency")
sexorient_table
sexorient_table_percent <- sexorient_table %>%
  mutate(SexOrientGroups = factor(SexOrientGroups, 
                             levels = SexOrientGroups[length(SexOrientGroups):1]),
         cumulative = cumsum(Frequency),
         midpoint = cumulative - Frequency / 2,
         labels = paste0(round((Frequency/ sum(Frequency)) * 100, 1), "%"))
sexorient_table_percent

ggplot(sexorient_table_percent, aes(x = "", y=SexOrientValue, fill=SexOrientGroups)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("steelblue", "firebrick", "yellowgreen", 
                               "mediumpurple", "darkorange")) +
  labs(x = "", y = "", title = "Sexual Orientation Distribution Frequencies 
       by Percentage",
       fill = "Sexual Orientation") + 
  geom_text(aes(x = 1.63, y = midpoint , label = labels), color="black",
            fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10))

ggplot(sexorient_table_percent, aes(x = "", y=SexOrientValue, fill=SexOrientGroups)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("steelblue", "firebrick", "yellowgreen", 
                               "mediumpurple", "darkorange")) +
  labs(x = "", y = "", title = "Sexual Orientation Distribution Frequencies 
       by Percentage",
       fill = "Sexual Orientation") + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10))

POV <- mutate(POV, het_d=case_when(
  sexorient == "Heterosexual" ~ "Heterosexual",
  sexorient == "Bisexual" ~ "Queer",
  sexorient == "Gay" ~ "Queer",
  sexorient == "Other" ~ "Queer",
  sexorient == "Pansexual" ~ "Queer"))

table(POV$het_d)

het_d_Groups = c("Heterosexual (82.2%)", "Queer (17.8%)")
het_d_Value = c(37, 8)
het_d_table <- data.frame(het_d_Groups, het_d_Value)
colnames(het_d_table) <- c("Sexual Orientation", "Frequency")
het_d_table

ggplot(het_d_table, aes(x = "", y=het_d_Value, fill=het_d_Groups)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(x = "", y = "", title = "Sexual Orientation Distribution Frequencies 
       by Percentage",
       fill = "Sexual Orientation") +  
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10))

# race percentages and pie chart
POV %>% 
  group_by( race ) %>% 
  summarise( percent = 100 * n() / nrow(POV) )

table(POV$race)
POV <- mutate(POV, race_recod=case_when(
  race == "Asian" ~ "Asian",
  race == "Black or African American" ~ "Black or African American",
  race == "Hispanic or Latino" ~ "Hispanic or Latino",
  race == "White or Caucasian" ~ "White or Caucasian",
  race == "Asian,Other" ~ "Biracial",
  race == "White or Caucasian,Asian" ~ "Biracial",
  race == "White or Caucasian,Hispanic or Latino" ~ "Biracial",
  race == "Other" ~ "Asian"))
table(POV$race_recod)

POV %>% 
  group_by( race_recod ) %>% 
  summarise( percent = 100 * n() / nrow(POV) )

RaceGroups = c("Asian", "Biracial", "Black or African American", 
               "Hispanic or Latino", "White or Caucasian")
RaceValue = c(19, 3, 5, 3, 15)
race_table <- data.frame(RaceGroups, RaceValue)
colnames(race_table) <- c("Race", "Frequency")
race_table
race_table_percent <- race_table %>%
  mutate(RaceGroups = factor(RaceGroups, 
                            levels = RaceGroups[length(RaceGroups):1]),
         cumulative = cumsum(Frequency),
         midpoint = cumulative - Frequency / 2,
         labels = paste0(round((Frequency/ sum(Frequency)) * 100, 1), "%"))
race_table_percent

ggplot(race_table_percent, aes(x = "", y=RaceValue, fill=RaceGroups)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("steelblue", "firebrick", "yellowgreen", 
                               "mediumpurple", "darkorange")) +
  labs(x = "", y = "", title = "Race Distribution Frequencies by Percentage",
       fill = "Race Groups") + 
  geom_text(aes(x = 1.2, y = midpoint , label = labels), color="black",
            fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10)) 

# age percentages and pie chart
POV %>% 
  group_by( age ) %>% 
  summarise( percent = 100 * n() / nrow(POV) )

table(POV$age)
AgeGroups = c("18-24", "25-34", "35 and over")
AgeValue = c(18, 15, 12)
age_table <- data.frame(AgeGroups, AgeValue)
colnames(age_table) <- c("Age Groups", "Frequency")
age_table
age_table_percent <- age_table %>%
  mutate(AgeGroups = factor(AgeGroups, 
                       levels = AgeGroups[length(AgeGroups):1]),
         cumulative = cumsum(Frequency),
         midpoint = cumulative - Frequency / 2,
         labels = paste0(round((Frequency/ sum(Frequency)) * 100, 1), "%"))
age_table_percent

ggplot(age_table_percent, aes(x = "", y=AgeValue, fill=AgeGroups)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("steelblue", "firebrick", "yellowgreen")) +
  labs(x = "", y = "", title = "Age Distribution Frequencies by Percentage",
       fill = "Age Groups") + 
  geom_text(aes(x = 1.2, y = midpoint , label = labels), color="black",
            fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10)) 

# Creating a new variable to identify cis-het and non-cis-het participants
table(POV$genderid, POV$het_d)

POV <- mutate(POV, cis_het=case_when(
  cis_d == "Cis-Gender" & het_d == "Heterosexual" ~ "Cis-Gender 
  Heterosexual",
  cis_d == "Cis-Gender" & het_d == "Queer" ~ "Cis-Gender 
  Queer",
  cis_d == "Non-Binary" & het_d == "Heterosexual" ~ "Non-Binary 
  Heterosexual",
  cis_d == "Non-Binary" & het_d == "Queer" ~ "Non-Binary 
  Queer"))

POV <- mutate(POV, cis_het2=case_when(
  genderid == "Cis-Female" & het_d == "Heterosexual" ~ "Cis-Female 
  Heterosexual",
  genderid == "Cis-Male" & het_d == "Heterosexual" ~ "Cis-Male 
  Heterosexual",
  genderid == "Cis-Female" & het_d == "Queer" ~ "Cis-Female
  Queer",
  genderid == "Cis-Male" & het_d == "Queer" ~ "Cis-Male
  Queer",
  genderid == "Non-binary" & het_d == "Heterosexual" ~ "Non-Binary 
  Heterosexual",
  genderid == "Non-binary" & het_d == "Queer" ~ "Non-Binary 
  Queer"))

# barplot campus safety during day
table(POV$campussafe_day)
POV <- mutate(POV, campussafe_day_recod=case_when(
  campussafe_day == "1" ~ "Not safe at all",
  campussafe_day == "2" ~ "Somewhat unsafe",
  campussafe_day == "3" ~ "Somewhat safe",
  campussafe_day == "4" ~ "Very safe"))

p_campus_day <- table(POV$campussafe_day_recod, POV$cis_het)
p_campus_day
# There is no column for "Non-binary Heterosexual" because there were no respondents identifying this way
barplot(p_campus_day, xlab = "Gender Identity and Sexual Orientation", 
        ylab = "Likert Scale Responses",
        main = "How Safe Do You Feel On Campus During the Day?",
        beside = TRUE, legend = TRUE, cex.axis=1, 
        col = c("steelblue", "firebrick"))

# barplot campus safety during night
table(POV$campusssafe_night)
POV <- mutate(POV, campussafe_night_recod=case_when(
  campusssafe_night == "1" ~ "Not safe at all",
  campusssafe_night == "2" ~ "Somewhat unsafe",
  campusssafe_night == "3" ~ "Somewhat safe",
  campusssafe_night == "4" ~ "Very safe"))

p_campus_night <- table(POV$campussafe_night_recod, POV$cis_het)
p_campus_night
barplot(p_campus_night, xlab = "Gender Identity and Sexual Orientation", 
        ylab = "Likert Scale Responses",
        main = "How Safe Do You Feel On Campus During the Night?",
        beside = TRUE, legend = TRUE, cex.axis=1, 
        col = c("steelblue", "firebrick", "yellowgreen"))

# barplot community safety during day
table(POV$commsafe_day)
POV <- mutate(POV, commsafe_day_recod=case_when(
  commsafe_day == "1" ~ "Not safe at all",
  commsafe_day == "2" ~ "Somewhat unsafe",
  commsafe_day == "3" ~ "Somewhat safe",
  commsafe_day == "4" ~ "Very safe"))

p_comm_day <- table(POV$commsafe_day_recod, POV$cis_het)
p_comm_day
barplot(p_comm_day, xlab = "Gender Identity and Sexual Orientation", 
        ylab = "Likert Scale Responses",
        main = "How Safe Do You Feel in the Community 
        Surrounding the Campus During the Day?",
        beside = TRUE, legend = TRUE, cex.axis=1, 
        col = c("steelblue", "firebrick", "yellowgreen"))

# barplot community safety during night
table(POV$commsafe_night)
POV <- mutate(POV, commsafe_night_recod=case_when(
  commsafe_night == "1" ~ "Not safe at all",
  commsafe_night == "2" ~ "Somewhat unsafe",
  commsafe_night == "3" ~ "Somewhat safe",
  commsafe_night == "4" ~ "Very safe"))

p_comm_night <- table(POV$commsafe_night_recod, POV$cis_het)
p_comm_night
barplot(p_comm_night, xlab = "Gender Identity and Sexual Orientation", 
        ylab = "Likert Scale Responses",
        main = "How Safe Do You Feel in the Community 
        Surrounding the Campus During the Night?",
        beside = TRUE, legend = TRUE, cex.axis=1, 
        col = c("steelblue", "firebrick", "yellowgreen", "mediumpurple"))

# barplot of verbal threat
POV <- mutate(POV, verbalthreat_recod=case_when(
  verbalthreat == "1" ~ "Very Unlikely",
  verbalthreat == "2" ~ "Somewhat Unlikely",
  verbalthreat == "3" ~ "Neither Likely nor Unlikely",
  verbalthreat == "4" ~ "Somewhat Likely", 
  verbalthreat == "5" ~ "Very Likely"))

POV$verbalthreat_recod = factor(POV$verbalthreat_recod, 
                                levels=c("Very Unlikely","Somewhat Unlikely",
                                         "Neither Likely nor Unlikely",
                                         "Somewhat Likely", "Very Likely"))
p_verbalthreat <- table(POV$verbalthreat_recod, POV$cis_het)
p_verbalthreat
barplot(p_verbalthreat, xlab = "Gender Identity and Sexual Orientation", 
        ylab = "Likert Scale Responses",
        main = "How Likely Do You Believe a Verbal Threat
        Could Ever Happen To You?",
        beside = TRUE, legend = TRUE, cex.axis=1, 
        col = c("steelblue", "firebrick", "yellowgreen", "mediumpurple", "darkorange"))

POV <- mutate(POV, verbalthreat_recod2=case_when(
  verbalthreat == "1" ~ "Unlikely",
  verbalthreat == "2" ~ "Unlikely",
  verbalthreat == "3" ~ "Neither Likely nor Unlikely",
  verbalthreat == "4" ~ "Likely", 
  verbalthreat == "5" ~ "Likely"))

POV$verbalthreat_recod2 = factor(POV$verbalthreat_recod2, 
                                    levels=c("Unlikely",
                                             "Neither Likely nor Unlikely",
                                             "Likely"))
p_verbalthreat2 <- table(POV$verbalthreat_recod2, POV$cis_het)
p_verbalthreat2
barplot(p_verbalthreat2, xlab = "Gender Identity and Sexual Orientation", 
        ylab = "Likert Scale Responses",
        main = "How Likely Do You Believe a Verbal Threat
        Could Ever Happen To You?",
        beside = TRUE, legend = TRUE, cex.axis=1, 
        col = c("steelblue", "firebrick", "yellowgreen"))

p_verbalthreat3 <- table(POV$verbalthreat_recod2, POV$cis_het2)
p_verbalthreat3
barplot(p_verbalthreat3, xlab = "Gender Identity and Sexual Orientation", 
        ylab = "Likert Scale Responses",
        main = "How Likely Do You Believe a Verbal Threat
        Could Ever Happen To You?",
        beside = TRUE, legend = TRUE,
        args.legend = list(x = "topright", inset = c(- 0.05, 0)), cex.names=0.75, 
        col = c("steelblue", "firebrick", "yellowgreen"))

# barplot of physical assault
POV <- mutate(POV, physicalassault_recod=case_when(
  physicalassault == "1" ~ "Very Unlikely",
  physicalassault == "2" ~ "Somewhat Unlikely",
  physicalassault == "3" ~ "Neither Likely nor Unlikely",
  physicalassault == "4" ~ "Somewhat Likely", 
  physicalassault == "5" ~ "Very Likely"))

POV$physicalassault_recod = factor(POV$physicalassault_recod, 
                                levels=c("Very Unlikely","Somewhat Unlikely",
                                         "Neither Likely nor Unlikely",
                                         "Somewhat Likely", "Very Likely"))
p_physicalassault <- table(POV$physicalassault_recod, POV$cis_het)
p_physicalassault
barplot(p_physicalassault, xlab = "Gender Identity and Sexual Orientation", 
        ylab = "Likert Scale Responses",
        main = "How Likely Do You Believe a Physical Assault 
        (Excluding Sexual Assault) Could Happen To You?",
        beside = TRUE, legend = TRUE, cex.axis=1, 
        col = c("steelblue", "firebrick", "yellowgreen", "mediumpurple", "darkorange"))

  POV <- mutate(POV, physicalassault_recod2=case_when(
  physicalassault == "1" ~ "Unlikely",
  physicalassault == "2" ~ "Unlikely",
  physicalassault == "3" ~ "Neither Likely nor Unlikely",
  physicalassault == "4" ~ "Likely", 
  physicalassault == "5" ~ "Likely"))

POV$physicalassault_recod2 = factor(POV$physicalassault_recod2, 
                                   levels=c("Unlikely",
                                            "Neither Likely nor Unlikely",
                                            "Likely"))
p_physicalassault2 <- table(POV$physicalassault_recod2, POV$cis_het)
p_physicalassault2
barplot(p_physicalassault2, xlab = "Gender Identity and Sexual Orientation", 
        ylab = "Likert Scale Responses",
        main = "How Likely Do You Believe a Physical Assault 
        (Excluding Sexual Assault) Could Happen To You?",
        beside = TRUE, legend = TRUE, cex.axis=1, 
        col = c("steelblue", "firebrick", "yellowgreen"))

p_physicalassault3 <- table(POV$physicalassault_recod2, POV$cis_het2)
p_physicalassault3
barplot(p_physicalassault3, xlab = "Gender Identity and Sexual Orientation", 
        ylab = "Likert Scale Responses",
        main = "How Likely Do You Believe a Physical Assault 
        (Excluding Sexual Assault) Could Happen To You?",
        beside = TRUE, legend = TRUE, 
        args.legend = list(x = "topright", inset = c(- 0.05, 0)),cex.names=0.75, 
        col = c("steelblue", "firebrick", "yellowgreen"))

# Recoding Campus Living Variable
POV <- mutate(POV, campus_live_recod=case_when(
  campus_live == "0" ~ "No",
  campus_live == "1" ~ "Yes"))
table(POV$campus_live_recod)

p_campuslive <- table(POV$campus_live_recod, POV$cis_het2)
barplot(p_campuslive, xlab = "Gender Identity and Sexual Orientation", 
        ylab = "Frequency",
        main = "Do You Live on Campus?",
        beside = TRUE, legend = TRUE, cex.names=0.75, 
        col = c("firebrick", "steelblue"))

# Recoding Sexual Orientation Openness Variable
table(POV$open_sexorient, useNA="always")
POV <- mutate(POV, open_sexorient_recod=case_when(
  open_sexorient == 0 ~ "No",
  open_sexorient == 1 ~ "Yes"))
table(POV$open_sexorient_recod, useNA="always")

p_open_sexorient <- table(POV$open_sexorient_recod, POV$cis_het2)
barplot(p_open_sexorient, xlab = "Gender Identity and Sexual Orientation", 
        ylab = "Frequency",
        main = "Are You Open About your Sexual Orientation?",
        beside = TRUE, legend = TRUE, cex.names=0.75, 
        col = c("firebrick", "steelblue"))

# Recoding gender and sexual orientation dummy variables
POV <- mutate(POV, cisF_d=case_when(
  genderid == "Cis-Male" ~ 0,
  genderid == "Non-binary" ~ 0,
  genderid == "Cis-Female" ~ 1))
table(POV$cisF_d)

POV <- mutate(POV, cisM_d=case_when(
  genderid == "Cis-Male" ~1,
  genderid == "Non-binary" ~ 0,
  genderid == "Cis-Female" ~ 0))
table(POV$cisM_d)

POV <- mutate(POV, NB_d=case_when(
  genderid == "Cis-Male" ~ 0,
  genderid == "Non-binary" ~ 1,
  genderid == "Cis-Female" ~ 0))
table(POV$NB_d)

POV <- mutate(POV, gender_recod=case_when(
  genderid == "Cis-Male" ~ 0,
  genderid == "Cis-Female" ~ 1,
  genderid == "Non-binary" ~ 2))
table(POV$gender_recod)

POV <- mutate(POV, gender_recod2=case_when(
  genderid == "Cis-Male" ~ 0,
  genderid == "Cis-Female" ~ 0,
  genderid == "Non-binary" ~ 1))
table(POV$gender_recod2)

POV <- mutate(POV, sexorient_recod=case_when(
  sexorient == "Heterosexual" ~ 0,
  sexorient == "Bisexual" ~ 1,
  sexorient == "Gay" ~ 1,
  sexorient == "Other" ~ 1,
  sexorient == "Pansexual" ~ 1))
table(POV$sexorient_recod)

# Possible box plots
boxplot(physicalassault~cisF_d,data=POV, main="Box Plot of Cis-Female Gender 
        on Physical Assault Fear", 
        xlab="Gender Identity (0=Non-Cis-Female, 1=Cis-Female)", 
        ylab="Likert Scale")

boxplot(physicalassault~cisM_d,data=POV, main="Box Plot of Cis-Male Gender 
        on Physical Assault Fear", 
        xlab="Gender Identity (0=Non-Cis-Male, 1=Cis-Male)", 
        ylab="Likert Scale")

boxplot(physicalassault~NB_d,data=POV, main="Box Plot of Non-Binary Gender 
        on Physical Assault Fear", 
        xlab="Gender Identity (0=Cis-Gender, 1=Non-Binary)", 
        ylab="Likert Scale")

# Box plots and regressions

# campus safety day and gender
table(POV$campussafe_day, POV$gender_recod)
boxplot(campussafe_day~gender_recod,data=POV, main="Box Plot of Gender Identity and 
        Feeling of Campus Safety During the Day", 
        xlab="Gender Identity", 
        ylab="Likert Scale (1 = Not Safe At All, 4 = Very Safe)", 
        names=c("Cis-Male", "Cis-Female", "Non-Binary"))

ggplot(POV, aes(y=campussafe_day, x=gender_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Gender Identity and 
        Feeling of Campus Safety During the Day") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (1 = Not Safe At All, 4 = Very Safe)")

# campus safety night and gender
table(POV$campusssafe_night, POV$gender_recod)
boxplot(campusssafe_night~gender_recod,data=POV, main="Box Plot of Gender Identity and 
        Feeling of Campus Safety During the Night", 
        xlab="Gender Identity", 
        ylab="Likert Scale (1 = Not Safe At All, 4 = Very Safe)", 
        names=c("Cis-Male", "Cis-Female", "Non-Binary"))

ggplot(POV, aes(y=campusssafe_night, x=gender_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Gender Identity and 
        Feeling of Campus Safety During the Night") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (1 = Not Safe At All, 4 = Very Safe)")

# community safety day and gender
table(POV$commsafe_day, POV$gender_recod)
boxplot(commsafe_day~gender_recod,data=POV, main="Box Plot of Gender Identity and 
        Feeling of Community Safety During the Day", 
        xlab="Gender Identity", 
        ylab="Likert Scale (1 = Not Safe At All, 4 = Very Safe)", 
        names=c("Cis-Male", "Cis-Female", "Non-Binary"))

ggplot(POV, aes(y=commsafe_day, x=gender_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Gender Identity and 
        Feeling of Community Safety During the Day") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (1 = Not Safe At All, 4 = Very Safe)")

# community safety night and gender
table(POV$commsafe_night, POV$gender_recod)
boxplot(commsafe_night~gender_recod,data=POV, main="Box Plot of Gender Identity and 
        Feeling of Community Safety During the Night", 
        xlab="Gender Identity", 
        ylab="Likert Scale (1 = Not Safe At All, 4 = Very Safe)", 
        names=c("Cis-Male", "Cis-Female", "Non-Binary"))

ggplot(POV, aes(y=commsafe_night, x=gender_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Gender Identity and 
        Feeling of Community Safety During the Night") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (1 = Not Safe At All, 4 = Very Safe)")

# physical assault and gender
boxplot(physicalassault~gender_recod,data=POV, main="Box Plot of Gender Identity and Physical Assault Fear", 
        xlab="Gender Identity", 
        ylab="Likert Scale (1 = Very Unlikely, 5 = Very Likely)", 
        names=c("Cis-Male", "Cis-Female", "Non-Binary"))

ggplot(POV, aes(y=physicalassault, x=gender_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Gender Identity on Physical Assault Fear") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (1 = Very Unlikely, 5 = Very Likely)")

# verbal threat and gender
boxplot(verbalthreat~gender_recod,data=POV, main="Box Plot of Gender Identity and Verbal Threat Fear", 
        xlab="Gender Identity", 
        ylab="Likert Scale (1 = Very Unlikely, 5 = Very Likely)", 
        names=c("Cis-Male", "Cis-Female", "Non-Binary"))

ggplot(POV, aes(y=verbalthreat, x=gender_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Gender Identity on Verbal Threat Fear") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (1 = Very Unlikely, 5 = Very Likely)")

# campus safety day and sexual orientation
table(POV$campussafe_day, POV$sexorient_recod)
boxplot(campussafe_day~sexorient_recod,data=POV, main="Box Plot of Sexual Orientation and 
        Feeling of Campus Safety During the Day", 
        xlab="Sexual Orientation", 
        ylab="Likert Scale (1 = Not Safe At All, 4 = Very Safe)", 
        names=c("Heterosexual", "Queer"))

ggplot(POV, aes(y=campussafe_day, x=sexorient_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Sexual Orientation and 
        Feeling of Campus Safety During the Day") + 
  xlab("Gender Identity (0 = Heterosexual, 1 = Queer)") + 
  ylab("Likert Scale (1 = Not Safe At All, 4 = Very Safe)")

# campus safety night and sexual orientation
table(POV$campusssafe_night, POV$sexorient_recod)
boxplot(campusssafe_night~sexorient_recod,data=POV, main="Box Plot of Sexual Orientation and 
        Feeling of Campus Safety During the Night", 
        xlab="Sexual Orientation", 
        ylab="Likert Scale (1 = Not Safe At All, 4 = Very Safe)", 
        names=c("Heterosexual", "Queer"))

ggplot(POV, aes(y=campusssafe_night, x=sexorient_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Sexual Orientation and 
        Feeling of Campus Safety During the Night") + 
  xlab("Gender Identity (0 = Heterosexual, 1 = Queer)") + 
  ylab("Likert Scale (1 = Not Safe At All, 4 = Very Safe)")

# community safety day and sexual orientation
table(POV$commsafe_day, POV$sexorient_recod)
boxplot(commsafe_day~sexorient_recod,data=POV, main="Box Plot of Sexual Orientation and 
        Feeling of Community Safety During the Day", 
        xlab="Sexual Orientation", 
        ylab="Likert Scale (1 = Not Safe At All, 4 = Very Safe)", 
        names=c("Heterosexual", "Queer"))

ggplot(POV, aes(y=commsafe_day, x=sexorient_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Sexual Orientation and 
        Feeling of Community Safety During the Day") + 
  xlab("Gender Identity (0 = Heterosexual, 1 = Queer)") + 
  ylab("Likert Scale (1 = Not Safe At All, 4 = Very Safe)")

# community safety night and sexual orientation
table(POV$commsafe_night, POV$sexorient_recod)
boxplot(commsafe_night~sexorient_recod,data=POV, main="Box Plot of Sexual Orientation and 
        Feeling of Community Safety During the Night", 
        xlab="Sexual Orientation", 
        ylab="Likert Scale (1 = Not Safe At All, 4 = Very Safe)", 
        names=c("Heterosexual", "Queer"))

ggplot(POV, aes(y=commsafe_night, x=sexorient_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Sexual Orientation and 
        Feeling of Community Safety During the Night") + 
  xlab("Gender Identity (0 = Heterosexual, 1 = Queer)") + 
  ylab("Likert Scale (1 = Not Safe At All, 4 = Very Safe)")

# physical assault and sexual orientation
boxplot(physicalassault~sexorient_recod,data=POV, main="Box Plot of Sexual Orientation and 
        Physical Assault Fear", 
        xlab="Sexual Orientation", 
        ylab="Likert Scale (1 = Very Unlikely, 5 = Very Likely)", 
        names=c("Heterosexual", "Queer"))

ggplot(POV, aes(y=physicalassault, x=sexorient_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Sexual Orientation on Physical Assault Fear") + 
  xlab("Sexual Orientation (0 = Heterosexual, 1 = Queer)") + 
  ylab("Likert Scale (1 = Very Unlikely, 5 = Very Likely)")

# verbal threat and sexual orientation
boxplot(verbalthreat~sexorient_recod,data=POV, main="Box Plot of Sexual Orientation and 
        Verbal Threat Fear", 
        xlab="Sexual Orientation", 
        ylab="Likert Scale (1 = Very Unlikely, 5 = Very Likely)", 
        names=c("Heterosexual", "Queer"))

ggplot(POV, aes(y=verbalthreat, x=sexorient_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Sexual Orientation on Verbal Threat Fear") + 
  xlab("Sexual Orientation (0 = Heterosexual, 1 = Queer)") + 
  ylab("Likert Scale (1 = Very Unlikely, 5 = Very Likely)")

# Interaction Plots
POV %>% 
  ggplot() +
  aes(x = gender_recod, color = sexorient_recod, group = sexorient_recod, y = campussafe_day) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  ggtitle("Interaction Plot of Gender Identity, Sexual Orientation, and
          Campus Safety During the Day") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (1 = Not Safe At All, 4 = Very Safe)") +
  guides(col=guide_legend("Sexual Orientation \n(0 = Heterosexual,\n1 = Queer)"))

POV %>% 
  ggplot() +
  aes(x = gender_recod, color = sexorient_recod, group = sexorient_recod, y = campusssafe_night) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  ggtitle("Interaction Plot of Gender Identity, Sexual Orientation, and
          Campus Safety During the Night") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (1 = Not Safe At All, 4 = Very Safe)") +
  guides(col=guide_legend("Sexual Orientation \n(0 = Heterosexual,\n1 = Queer)"))

POV %>% 
  ggplot() +
  aes(x = gender_recod, color = sexorient_recod, group = sexorient_recod, y = commsafe_day) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  ggtitle("Interaction Plot of Gender Identity, Sexual Orientation, and
          Community Safety During the Day") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (1 = Not Safe At All, 4 = Very Safe)") +
  guides(col=guide_legend("Sexual Orientation \n(0 = Heterosexual,\n1 = Queer)"))

POV %>% 
  ggplot() +
  aes(x = gender_recod, color = sexorient_recod, group = sexorient_recod, y = commsafe_night) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  ggtitle("Interaction Plot of Gender Identity, Sexual Orientation, and
          Community Safety During the Night") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (1 = Not Safe At All, 4 = Very Safe)") +
  guides(col=guide_legend("Sexual Orientation \n(0 = Heterosexual,\n1 = Queer)"))

POV %>% 
  ggplot() +
  aes(x = gender_recod, color = sexorient_recod, group = sexorient_recod, y = physicalassault) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  ggtitle("Interaction Plot of Gender Identity, Sexual Orientation, and
          Fear of Physical Assault") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (1 = Very Unlikely, 5 = Very Likely)") +
  guides(col=guide_legend("Sexual Orientation \n(0 = Heterosexual,\n1 = Queer)"))

POV %>% 
  ggplot() +
  aes(x = gender_recod, color = sexorient_recod, group = sexorient_recod, y = verbalthreat) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  ggtitle("Interaction Plot of Gender Identity, Sexual Orientation, and
          Fear of Verbal Threat") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (1 = Very Unlikely, 5 = Very Likely)") +
  guides(col=guide_legend("Sexual Orientation \n(0 = Heterosexual,\n1 = Queer)"))


#Summative Campus & Community Scales

# General Campus Safety
campussafety <- POV$campussafe_day + POV$campusssafe_night
summary(campussafety)

boxplot(campussafety~gender_recod,data=POV, main="Box Plot of Gender Identity and General Campus Safety", 
        xlab="Gender Identity", 
        ylab="Likert Scale (2 = Not Safe At All, 8 = Very Safe)", 
        names=c("Cis-Male", "Cis-Female", "Non-Binary"))

ggplot(POV, aes(y=campussafety, x=gender_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Gender Identity on General Campus Safety") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (2 = Not Safe At All, 8 = Very Safe)")

boxplot(campussafety~sexorient_recod,data=POV, main="Box Plot of Sexual Orientation and General Campus Safety", 
        xlab="Sexual Orientation", 
        ylab="Likert Scale (2 = Not Safe At All, 8 = Very Safe)", 
        names=c("Heterosexual", "Queer"))

ggplot(POV, aes(y=campussafety, x=sexorient_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Sexual Orientation on General Campus Safety") + 
  xlab("Sexual Orientation (0 = Heterosexual, 1 = Queer)") + 
  ylab("Likert Scale (2 = Not Safe At All, 8 = Very Safe)")

POV %>% 
  ggplot() +
  aes(x = gender_recod, color = sexorient_recod, group = sexorient_recod, y = campussafety) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  ggtitle("Interaction Plot of Gender Identity, Sexual Orientation, and
          General Campus Safety") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (2 = Not Safe At All, 8 = Very Safe)") +
  guides(col=guide_legend("Sexual Orientation \n(0 = Heterosexual,\n1 = Queer)"))

# General Community Safety
commsafety <- POV$commsafe_day + POV$commsafe_night
summary(commsafety)

boxplot(commsafety~gender_recod,data=POV, main="Box Plot of Gender Identity and 
        General Community Safety", 
        xlab="Gender Identity", 
        ylab="Likert Scale (2 = Not Safe At All, 8 = Very Safe)", 
        names=c("Cis-Male", "Cis-Female", "Non-Binary"))

ggplot(POV, aes(y=commsafety, x=gender_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Gender Identity on General Community Safety") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (2 = Not Safe At All, 8 = Very Safe)")

boxplot(commsafety~sexorient_recod,data=POV, main="Box Plot of Sexual Orientation and 
        General Community Safety", 
        xlab="Sexual Orientation", 
        ylab="Likert Scale (2 = Not Safe At All, 8 = Very Safe)", 
        names=c("Heterosexual", "Queer"))

ggplot(POV, aes(y=commsafety, x=sexorient_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Sexual Orientation on General Community Safety") + 
  xlab("Sexual Orientation (0 = Heterosexual, 1 = Queer)") + 
  ylab("Likert Scale (2 = Not Safe At All, 8 = Very Safe)")

POV %>% 
  ggplot() +
  aes(x = gender_recod, color = sexorient_recod, group = sexorient_recod, y = commsafety) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  ggtitle("Interaction Plot of Gender Identity, Sexual Orientation, and
          General Community Safety") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (2 = Not Safe At All, 8 = Very Safe)") +
  guides(col=guide_legend("Sexual Orientation \n(0 = Heterosexual,\n1 = Queer)"))

# General Safety 
campuscommsafety <- POV$campussafe_day + POV$campusssafe_night + 
  POV$commsafe_day + POV$commsafe_night
summary(campuscommsafety)

boxplot(campuscommsafety~gender_recod,data=POV, main="Box Plot of Gender Identity and General Safety", 
        xlab="Gender Identity", 
        ylab="Likert Scale (4 = Not Safe At All, 16 = Very Safe)", 
        names=c("Cis-Male", "Cis-Female", "Non-Binary"))

ggplot(POV, aes(y=campuscommsafety, x=gender_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Gender Identity on General Safety") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (4 = Not Safe At All, 16 = Very Safe)")

boxplot(campuscommsafety~sexorient_recod,data=POV, main="Box Plot of Sexual Orientation and General Safety", 
        xlab="Sexual Orientation", 
        ylab="Likert Scale (4 = Not Safe At All, 16 = Very Safe)", 
        names=c("Heterosexual", "Queer"))

ggplot(POV, aes(y=campuscommsafety, x=sexorient_recod)) + 
  geom_point() + geom_smooth(method="lm") + 
  ggtitle("Effect of Sexual Orientation on General Safety") + 
  xlab("Sexual Orientation (0 = Heterosexual, 1 = Queer)") + 
  ylab("Likert Scale (4 = Not Safe At All, 16 = Very Safe)")

POV %>% 
  ggplot() +
  aes(x = gender_recod, color = sexorient_recod, group = sexorient_recod, y = campuscommsafety) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  ggtitle("Interaction Plot of Gender Identity, Sexual Orientation, and
          General Safety") + 
  xlab("Gender Identity (0 = Cis-Male, 1 = Cis-Female, 2 = Non-Binary)") + 
  ylab("Likert Scale (4 = Not Safe At All, 16 = Very Safe)") +
  guides(col=guide_legend("Sexual Orientation \n(0 = Heterosexual,\n1 = Queer)"))

Reg2 <- lm(verbalthreat_recod ~ sexorient_recod + gender_recod + (sexorient_recod*gender_recod), data=POV)
summary(Reg2)
plot_model(Reg2, type = "int", 
           terms = c("sexorient_recod", "gender_recod"), data=POV)


