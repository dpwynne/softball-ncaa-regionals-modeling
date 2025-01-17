library(tidyverse)
library(ggplot2)
library(lattice)
library(caret)
library(tidymodels)
library(parsnip)

#Read in the data
Softball2015 <- read.csv("Data/Softball2015.csv")

Softball2016 <- read.csv("Data/Softball2016.csv")

Softball2017 <- read.csv("Data/Softball2017.csv")

Softball2018 <- read.csv("Data/Softball2018.csv")


#this is the google spreadsheet
Regionals <- read.csv("Data/NCAA Tournament Games 2015-2018 - Sheet1 new.csv")


library(tidyverse)
#library(ggplot2)
library(lattice)
library(caret)

library(tidymodels)
# send link to Tidymodels with R online book

#filters each data set into teams that went to Regionals
NCAA2015 <- filter(Softball2015, NCAA %in% c("At-large","Auto"))
NCAA2016 <- filter(Softball2016, NCAA %in% c("At-large","Auto"))
NCAA2017 <- filter(Softball2017, NCAA %in% c("At-large","Auto"))
NCAA2018 <- filter(Softball2018, NCAA %in% c("At-large","Auto"))

#adds year variable
NCAA2015 <- mutate(NCAA2015, Year=2015)
NCAA2016 <- mutate(NCAA2016, Year=2016)
NCAA2017 <- mutate(NCAA2017, Year=2017)
NCAA2018 <- mutate(NCAA2018, Year=2018)

#cannot row bind unless number of columns are equal; this removes extra columns in 2018 data set
NCAA2018 <- subset(NCAA2018, select=-c(PitcherK, PitcherBB))

#row binds the four datasets
RegionalStats <- data.frame(rbind(NCAA2015,NCAA2016,NCAA2017,NCAA2018))

#reorders columns so year is first
RegionalStats <- RegionalStats[, c(32,1:31)]

#Adds stats: fielding percentage, singles, total bases, double plays per game, slugging percentage, success rate of stolen bases
#second line adds batting average
RegionalStats <- mutate(RegionalStats, FieldingPct = (PO+A)/(PO+A+E), Singles = H - Doubles - Triples - HR, 
                        TB = Singles + 2*Doubles + 3*Triples + 4*HR, DPPerGame = DP/G, 
                        SlgPct = TB/AB, SuccessRate = SB/(SB+CS))
RegionalStats <- mutate(RegionalStats, BA=H/AB)

#Joins Regionals and stats into one data frame by game
RegionalGames <- Regionals %>% left_join(RegionalStats, by = c("Home.Team" = "Name", "Year"),  
                                         suffix = c(".x", ".home")) %>% left_join(RegionalStats, 
                                                                                  by = c("Visiting.Team" = "Name", "Year"), suffix = c(".home",".visit"))

#Adds column indicating home team win
RegionalGames <- mutate(RegionalGames, Home.Win = ifelse(Home.Score>Visiting.Score, "Yes", "No"))

#Adds column indicating run differental (absolute value of Home-Visiting runs)
RegionalGames <- mutate(RegionalGames, Run.Diff = Home.Score-Visiting.Score)

#Creates new data frame that standardizes important variables
RegionalGames_Std <- mutate(RegionalGames, Run.Diff = Home.Score-Visiting.Score,
                            Singles.home = Singles.home/G.home, Singles.visit=Singles.visit/G.visit,
                            TB.home=TB.home/G.home, TB.visit=TB.visit/G.visit, 
                            Doubles.home=Doubles.home/G.home, Doubles.visit=Doubles.visit/G.visit,
                            Triples.home=Triples.home/G.home, Triples.visit=Triples.visit/G.visit,
                            HR.home=HR.home/G.home, HR.visit=HR.visit/G.visit,
                            SB.home=SB.home/G.home, SB.visit=SB.visit/G.visit,
                            CS.home=CS.home/G.home, CS.visit=CS.visit/G.visit,
                            RunsScored.home=RunsScored.home/G.home, RunsScored.visit=RunsScored.visit/G.visit,
                            PO.home=PO.home/G.home, PO.visit=PO.visit/G.visit,
                            A.home=A.home/G.home, A.visit=A.visit/G.visit,
                            E.home=E.home/G.home, E.visit=E.visit/G.visit,
                            RunsAllowed.home=RunsAllowed.home/G.home, RunsAllowed.visit=RunsAllowed.visit/G.visit)

#Adds column indicating if home team or visiting team (or neither) is regional host
RegionalGames_Std <- mutate(RegionalGames_Std, 
                            Host= case_when(Regional.Host==Home.Team~1, 
                                            Regional.Host==Visiting.Team~-1,
                                            Regional.Host != Home.Team & Regional.Host != Visiting.Team ~ 0) )

dim(RegionalGames_Std)
set.seed(1)
obs=seq(1,397,1)
x=sample(obs,300,replace=F)
RegionalGames_Std$Home.Win <- as.factor(RegionalGames_Std$Home.Win)
Games.train=RegionalGames_Std[x,]
Games.test=RegionalGames_Std[-x,]

Differences <- mutate(RegionalGames_Std, Doubles = Doubles.home - Doubles.visit, 
                      Triples = Triples.home - Triples.visit,
                      HR = HR.home - HR.visit, 
                      RunsScored = RunsScored.home - RunsScored.visit,
                      SB = SB.home - SB.visit, 
                      CS = CS.home - CS.visit, 
                      IP = IP.home - IP.visit, 
                      RunsAllowed = RunsAllowed.home - RunsAllowed.visit,
                      ER = ER.home - ER.visit,
                      ERA = ERA.home - ERA.visit,
                      PO = PO.home - PO.visit,
                      A = A.home - A.visit,
                      E = E.home - E.visit,
                      DP = DPPerGame.home - DPPerGame.visit,
                      FieldingPct = FieldingPct.home - FieldingPct.visit, 
                      Singles = Singles.home - Singles.visit,
                      TB = TB.home - TB.visit, 
                      SlgPct = SlgPct.home - SlgPct.visit,
                      SuccessRate = SuccessRate.home - SuccessRate.visit,
                      BA = BA.home - BA.visit,
                      ConferenceDiff = case_when(ConfNumBids.home > 1 & ConfNumBids.visit == 1 ~ 1, ConfNumBids.home == 1 & ConfNumBids.visit > 1 ~ -1, TRUE ~ 0 ))


Differences$Home.Win <- as.factor(Differences$Home.Win)

Diff.split <- initial_split(Differences)
diff.train <- analysis(Diff.split)
diff.test <- assessment(Diff.split)

test <- glm(Home.Win ~ Host*(E + SlgPct + HR + ERA + Doubles), family = binomial, data = diff.train) %>% MASS::stepAIC()

fit.logi <- test %>% predict(diff.test, type="response")

logi.predicted <- ifelse(fit.logi> 0.5, "Yes", "No")

counter=0
for(i in 1:100)
{
  if(logi.predicted[i] !=diff.test$Home.Win[i])
  {counter=counter+1}
}

counter
misclass=counter/100
misclass
accuracy = (100-counter)/100
accuracy


test2 <- glm(Home.Win ~ Host*(HR), family = binomial, data = diff.train) %>% MASS::stepAIC()

fit.logi2 <- test2 %>% predict(diff.test, type="response")

logi.predicted2 <- ifelse(fit.logi2 > 0.5, "Yes", "No")

counter2=0
for(i in 1:100)
{
  if(logi.predicted2[i] !=diff.test$Home.Win[i])
  {counter2=counter2+1}
}

counter2
misclass2=counter2/100
misclass2
accuracy2 = (100-counter2)/100
accuracy2

test3 <- glm(Home.Win ~ Host*(Triples + Singles + Doubles + HR + FieldingPct + SlgPct + SuccessRate), family = binomial, data = diff.train) %>% MASS::stepAIC()

fit.logi3 <- test3 %>% predict(diff.test, type="response")

logi.predicted3 <- ifelse(fit.logi3 > 0.5, "Yes", "No")

counter3=0
for(i in 1:100)
{
  if(logi.predicted3[i] !=diff.test$Home.Win[i])
  {counter3=counter3+1}
}

counter3
misclass3=counter3/100
misclass3
accuracy3 = (100-counter3)/100
accuracy3
#test3 highest accuracy other than just HR, replacing Triples w/ HR ~0.67, w/ Doubles~0.68, w/Singles ~0.69

test4 <- glm(Home.Win ~ Host*(Triples), family = binomial, data = diff.train) %>% MASS::stepAIC()

fit.logi4 <- test4 %>% predict(diff.test, type="response")

logi.predicted4 <- ifelse(fit.logi4 > 0.5, "Yes", "No")

counter4=0
for(i in 1:100)
{
  if(logi.predicted4[i] !=diff.test$Home.Win[i])
  {counter4=counter4+1}
}

counter4
misclass4=counter4/100
misclass4
accuracy4 = (100-counter4)/100
accuracy4

test5 <- glm(Home.Win ~ Host*(HR + 
                                RunsScored + 
                                RunsAllowed +
                                DP + 
                                SlgPct + 
                                FieldingPct + 
                                Singles +  
                                Doubles + 
                                Triples + 
                                SuccessRate + 
                                BA), family = binomial, data = diff.train) %>% MASS::stepAIC()

fit.logi5 <- test5 %>% predict(diff.test, type="response")

modelsummary <- summary(test5)

logi.predicted5 <- ifelse(fit.logi5 > 0.5, "Yes", "No")

counter5=0
for(i in 1:100)
{
  if(logi.predicted5[i] !=diff.test$Home.Win[i])
  {counter5=counter5+1}
}

counter5
misclass5=counter5/100
misclass5
accuracy5 = (100-counter5)/100
accuracy5


#0.76 accuracy with test 5 which kept HR, Runs Allowed, DP, SlgPct, FieldingPct, Singles, Doubles, Triples, and Batting avg
#also Host interaction with HR, Runs Allowed, DP, and FieldingPct

Softball2019 <- read.csv("Data/Softball2019.csv")

Softball2021 <- read.csv("Data/Softball2021.csv")

Softball2022 <- read.csv("Data/Softball2022.csv")



#read in spreadsheets for 2019, 2021, 2022
Games2019 <- read.csv("Data/NCAA 2019 Regional Games.csv")
Games2021 <- read.csv("Data/NCAA 2021 Regional Games.csv")
Games2022 <- read.csv("Data/NCAA 2022 Regional Games.csv")

#filters each data set into teams that went to Regionals
NCAA2019 <- filter(Softball2019, NCAA %in% c("At-large","Auto"))
NCAA2021 <- filter(Softball2021, NCAA %in% c("Yes"))
NCAA2022 <- filter(Softball2022, NCAA %in% c("Yes"))

#gets rid of extra columns in 2022
#NCAA2022 <- subset(NCAA2022, select=-c(X, X.1, X.2, X.3, X.4, X.5, X.6, X.7, X.8, X.9, X.10, X.11, X.12, X.13, X.14, X.15, X.16, X.17, X.18))

#adds year variable
NCAA2019 <- mutate(NCAA2019, Year=2019)
NCAA2021 <- mutate(NCAA2021, Year=2021)
NCAA2022 <- mutate(NCAA2022, Year=2022)

#moves year column to front
NCAA2019 <- NCAA2019[, c(35,1:34)]
NCAA2021 <- NCAA2021[, c(36,1:35)]
NCAA2022 <- NCAA2022[, c(36,1:35)]

#adds stats to match RegionalGames_Std
NCAA2019 <- mutate(NCAA2019, FieldingPct = (PO+A)/(PO+A+E), Singles = H - Doubles - Triples - HR, 
                        TB = Singles + 2*Doubles + 3*Triples + 4*HR, DPPerGame = DP/G, 
                        SlgPct = TB/AB, SuccessRate = SB/(SB+CS), BA=H/AB)
NCAA2021 <- mutate(NCAA2021, FieldingPct = (PO+A)/(PO+A+E), Singles = H - Doubles - Triples - HR, 
                   TB = Singles + 2*Doubles + 3*Triples + 4*HR, DPPerGame = DP/G, 
                   SlgPct = TB/AB, SuccessRate = SB/(SB+CS), BA=H/AB)
NCAA2022 <- mutate(NCAA2022, FieldingPct = (PO+A)/(PO+A+E), Singles = H - Doubles - Triples - HR, 
                   TB = Singles + 2*Doubles + 3*Triples + 4*HR, DPPerGame = DP/G, 
                   SlgPct = TB/AB, SuccessRate = SB/(SB+CS), BA=H/AB)

#joins game data with stat data by team name for both home and visiting
Regionals2019 <- Games2019 %>% left_join(NCAA2019, by = c("Home.Team" = "Name", "Year"),  
                                                          suffix = c(".x", ".home")) %>% left_join(NCAA2019, 
                                                                                                   by = c("Visiting.Team" = "Name", "Year"), suffix = c(".home",".visit"))

Regionals2021 <- Games2021 %>% left_join(NCAA2021, by = c("Home.Team" = "Name", "Year"),  
                                          suffix = c(".x", ".home")) %>% left_join(NCAA2021, 
                                                                                   by = c("Visiting.Team" = "Name", "Year"), suffix = c(".home",".visit"))

Regionals2022 <- Games2022 %>% left_join(NCAA2022, by = c("Home.Team" = "Team", "Year"),  
                                          suffix = c(".x", ".home")) %>% left_join(NCAA2022, 
                                                                                   by = c("Visiting.Team" = "Team", "Year"), suffix = c(".home",".visit"))

#Adds column indicating yes/no for home team win
Regionals2019 <- mutate(Regionals2019, Home.Win = ifelse(Home.Score>Visiting.Score, "Yes", "No"))

Regionals2021 <- mutate(Regionals2021, Home.Win = ifelse(Home.Score>Visiting.Score, "Yes", "No"))

Regionals2022 <- mutate(Regionals2022, Home.Win = ifelse(Home.Score>Visiting.Score, "Yes", "No"))

#Creates new data frame that standardizes important variables
Regionals2019 <- mutate(Regionals2019, Run.Diff = Home.Score-Visiting.Score,
                            Singles.home = Singles.home/G.home, Singles.visit=Singles.visit/G.visit,
                            TB.home=TB.home/G.home, TB.visit=TB.visit/G.visit, 
                            Doubles.home=Doubles.home/G.home, Doubles.visit=Doubles.visit/G.visit,
                            Triples.home=Triples.home/G.home, Triples.visit=Triples.visit/G.visit,
                            HR.home=HR.home/G.home, HR.visit=HR.visit/G.visit,
                            SB.home=SB.home/G.home, SB.visit=SB.visit/G.visit,
                            CS.home=CS.home/G.home, CS.visit=CS.visit/G.visit,
                            RunsScored.home=RunsScored.home/G.home, RunsScored.visit=RunsScored.visit/G.visit,
                            PO.home=PO.home/G.home, PO.visit=PO.visit/G.visit,
                            A.home=A.home/G.home, A.visit=A.visit/G.visit,
                            E.home=E.home/G.home, E.visit=E.visit/G.visit,
                            RunsAllowed.home=RunsAllowed.home/G.home, RunsAllowed.visit=RunsAllowed.visit/G.visit)

Regionals2021 <- mutate(Regionals2021, Run.Diff = Home.Score-Visiting.Score,
                        Singles.home = Singles.home/G.home, Singles.visit=Singles.visit/G.visit,
                        TB.home=TB.home/G.home, TB.visit=TB.visit/G.visit, 
                        Doubles.home=Doubles.home/G.home, Doubles.visit=Doubles.visit/G.visit,
                        Triples.home=Triples.home/G.home, Triples.visit=Triples.visit/G.visit,
                        HR.home=HR.home/G.home, HR.visit=HR.visit/G.visit,
                        SB.home=SB.home/G.home, SB.visit=SB.visit/G.visit,
                        CS.home=CS.home/G.home, CS.visit=CS.visit/G.visit,
                        RunsScored.home=RunsScored.home/G.home, RunsScored.visit=RunsScored.visit/G.visit,
                        PO.home=PO.home/G.home, PO.visit=PO.visit/G.visit,
                        A.home=A.home/G.home, A.visit=A.visit/G.visit,
                        E.home=E.home/G.home, E.visit=E.visit/G.visit,
                        RunsAllowed.home=RunsAllowed.home/G.home, RunsAllowed.visit=RunsAllowed.visit/G.visit)

Regionals2022 <- mutate(Regionals2022, Run.Diff = Home.Score-Visiting.Score,
                        Singles.home = Singles.home/G.home, Singles.visit=Singles.visit/G.visit,
                        TB.home=TB.home/G.home, TB.visit=TB.visit/G.visit, 
                        Doubles.home=Doubles.home/G.home, Doubles.visit=Doubles.visit/G.visit,
                        Triples.home=Triples.home/G.home, Triples.visit=Triples.visit/G.visit,
                        HR.home=HR.home/G.home, HR.visit=HR.visit/G.visit,
                        SB.home=SB.home/G.home, SB.visit=SB.visit/G.visit,
                        CS.home=CS.home/G.home, CS.visit=CS.visit/G.visit,
                        RunsScored.home=RunsScored.home/G.home, RunsScored.visit=RunsScored.visit/G.visit,
                        PO.home=PO.home/G.home, PO.visit=PO.visit/G.visit,
                        A.home=A.home/G.home, A.visit=A.visit/G.visit,
                        E.home=E.home/G.home, E.visit=E.visit/G.visit,
                        RunsAllowed.home=RunsAllowed.home/G.home, RunsAllowed.visit=RunsAllowed.visit/G.visit)

#adds host variable
Regionals2019 <- mutate(Regionals2019, 
                            Host= case_when(Regional.Host==Home.Team~1, 
                                            Regional.Host==Visiting.Team~-1,
                                            Regional.Host != Home.Team & Regional.Host != Visiting.Team ~ 0) )

Regionals2021 <- mutate(Regionals2021, 
                        Host= case_when(Regional.Host==Home.Team~1, 
                                        Regional.Host==Visiting.Team~-1,
                                        Regional.Host != Home.Team & Regional.Host != Visiting.Team ~ 0) )

Regionals2022 <- mutate(Regionals2022, 
                        Host= case_when(Regional.Host==Home.Team~1, 
                                        Regional.Host==Visiting.Team~-1,
                                        Regional.Host != Home.Team & Regional.Host != Visiting.Team ~ 0) )

#creates new dataframe that includes the difference between important stats
Differences2019 <- mutate(Regionals2019, Doubles = Doubles.home - Doubles.visit, 
                      Triples = Triples.home - Triples.visit,
                      HR = HR.home - HR.visit, 
                      RunsScored = RunsScored.home - RunsScored.visit,
                      SB = SB.home - SB.visit, 
                      CS = CS.home - CS.visit, 
                      IP = IP.home - IP.visit, 
                      RunsAllowed = RunsAllowed.home - RunsAllowed.visit,
                      ER = ER.home - ER.visit,
                      ERA = ERA.home - ERA.visit,
                      PO = PO.home - PO.visit,
                      A = A.home - A.visit,
                      E = E.home - E.visit,
                      DP = DPPerGame.home - DPPerGame.visit,
                      FieldingPct = FieldingPct.home - FieldingPct.visit, 
                      Singles = Singles.home - Singles.visit,
                      TB = TB.home - TB.visit, 
                      SlgPct = SlgPct.home - SlgPct.visit,
                      SuccessRate = SuccessRate.home - SuccessRate.visit,
                      BA = BA.home - BA.visit)

Differences2021 <- mutate(Regionals2021, Doubles = Doubles.home - Doubles.visit, 
                          Triples = Triples.home - Triples.visit,
                          HR = HR.home - HR.visit, 
                          RunsScored = RunsScored.home - RunsScored.visit,
                          SB = SB.home - SB.visit, 
                          CS = CS.home - CS.visit, 
                          IP = IP.home - IP.visit, 
                          RunsAllowed = RunsAllowed.home - RunsAllowed.visit,
                          ER = ER.home - ER.visit,
                          ERA = ERA.home - ERA.visit,
                          PO = PO.home - PO.visit,
                          A = A.home - A.visit,
                          E = E.home - E.visit,
                          DP = DPPerGame.home - DPPerGame.visit,
                          FieldingPct = FieldingPct.home - FieldingPct.visit, 
                          Singles = Singles.home - Singles.visit,
                          TB = TB.home - TB.visit, 
                          SlgPct = SlgPct.home - SlgPct.visit,
                          SuccessRate = SuccessRate.home - SuccessRate.visit,
                          BA = BA.home - BA.visit)

Differences2022 <- mutate(Regionals2022, Doubles = Doubles.home - Doubles.visit, 
                          Triples = Triples.home - Triples.visit,
                          HR = HR.home - HR.visit, 
                          RunsScored = RunsScored.home - RunsScored.visit,
                          SB = SB.home - SB.visit, 
                          CS = CS.home - CS.visit, 
                          IP = IP.home - IP.visit, 
                          RunsAllowed = RunsAllowed.home - RunsAllowed.visit,
                          ER = ER.home - ER.visit,
                          ERA = ERA.home - ERA.visit,
                          PO = PO.home - PO.visit,
                          A = A.home - A.visit,
                          E = E.home - E.visit,
                          DP = DPPerGame.home - DPPerGame.visit,
                          FieldingPct = FieldingPct.home - FieldingPct.visit, 
                          Singles = Singles.home - Singles.visit,
                          TB = TB.home - TB.visit, 
                          SlgPct = SlgPct.home - SlgPct.visit,
                          SuccessRate = SuccessRate.home - SuccessRate.visit,
                          BA = BA.home - BA.visit)


#final model
test6 <- glm(Home.Win ~ Host+HR +
                                RunsScored +
                                RunsAllowed +
                                DP +
                                SlgPct +
                                FieldingPct +
                                Singles +
                                Doubles +
                                Triples +
                                SuccessRate +
                                BA + ConferenceDiff, family = binomial, data = Differences) %>% MASS::stepAIC()

Differences2019 <- mutate(Differences2019, ConferenceDiff = case_when(ConfNumBids.home > 1 & ConfNumBids.visit == 1 ~ 1, ConfNumBids.home == 1 & ConfNumBids.visit > 1 ~ -1, TRUE ~ 0 ))
Differences2021 <- mutate(Differences2021, ConferenceDiff = case_when(ConfNumBids.home > 1 & ConfNumBids.visit == 1 ~ 1, ConfNumBids.home == 1 & ConfNumBids.visit > 1 ~ -1, TRUE ~ 0 ))
Differences2022 <- mutate(Differences2022, ConferenceDiff = case_when(ConfNumBids.home > 1 & ConfNumBids.visit == 1 ~ 1, ConfNumBids.home == 1 & ConfNumBids.visit > 1 ~ -1, TRUE ~ 0 ))

#uses 2019 as validation set for test6
validation2019 <- test6 %>% predict(Differences2019, type="response")

predicted2019 <- ifelse(validation2019 > 0.5, "Yes", "No")

counter2019=0
for(i in 1:104)
{
  if(predicted2019[i] != Differences2019$Home.Win[i])
  {counter2019=counter2019+1}
}

counter2019
misclass2019=counter2019/100
misclass2019
accuracy2019 = (100-counter2019)/100
accuracy2019

#formula
prob = -0.01087

#test on 2021
test2021 <- test6 %>% predict(Differences2021, type = "response")

predicted2021 <- ifelse(test2021 > 0.5, "Yes", "No")

counter2021 = 0
for(i in 1:93)
{
  if(predicted2021[i] != Differences2021$Home.Win[i])
  {counter2021 = counter2021 + 1}
}

misclass2021 = counter2021/100
accuracy2021 = 1 - misclass2021
accuracy2021
#accuracy 2021 = 74%

#test on 2022
test2022 <- test6 %>% predict(Differences2022, type = "response")

predicted2022 <- ifelse(test2022 > 0.5, "Yes", "No")

counter2022 = 0
for(i in 1:102)
{
  if(predicted2022[i] != Differences2022$Home.Win[i])
  {counter2022 = counter2022 + 1}
}

misclass2022 = counter2022/100
accuracy2022 = 1 - misclass2022
accuracy2022


csuf2022 <- Differences2022[23, ]
csufprob <- test5 %>% predict(csuf2022, type = "response")
csufprob

pred_table <- Differences2022[, c("Home.Team", "Visiting.Team", "Home.Win")] %>% cbind(test2022)
View(pred_table)

csuf2022_2 <- Differences2022[(Differences2022$Home.Team %in% c("LSU", "San Diego St.", "Cal St. Fullerton", "Arizona St.")), ]
csufprob2 <- test6 %>% predict(csuf2022_2, type = "response")
csufprob2

#filter stats data to only WCWS teams and CSUF
goal_est <- filter(Softball2021, WCWS == "Yes" | Name == "Cal St. Fullerton")

#add other important stats
goal_est <- mutate(goal_est, FieldingPct = (PO+A)/(PO+A+E), Singles = H - Doubles - Triples - HR, 
                   TB = Singles + 2*Doubles + 3*Triples + 4*HR, DPPerGame = DP/G, 
                   SlgPct = TB/AB, SuccessRate = SB/(SB+CS), BA=H/AB)

#standardize
goal_est <- mutate(goal_est,
                   Singles = Singles/G,
                   TB=TB/G,
                   Doubles=Doubles/G,
                   Triples=Triples/G,
                   HR=HR/G,
                   SB=SB/G,
                   CS=CS/G,
                   RunsScored=RunsScored/G,
                   PO=PO/G,
                   A=A/G,
                   E=E/G,
                   RunsAllowed=RunsAllowed/G)


#include only numeric columns to do mean
goal_est_num <- goal_est[,sapply(goal_est, is.numeric)]

#taking average of WCWS columns as first step to estimating team goals for 2022
WCWS <- c(1,2,4,5,6,7,8,9)
avg_WCWS <- colMeans(goal_est_num[WCWS,], na.rm = TRUE)

#second step of goal estimating
csuf_goal <- (avg_WCWS + goal_est_num[3,])/2

#adding the row to numeric columns with goal stats
goal_est_num[nrow(goal_est_num) + 1,] <- csuf_goal

#Arizona St. Regional
actual_2022 <- Regionals2022[19:24,]

Differences_act <- mutate(actual_2022, Doubles = Doubles.home - Doubles.visit, 
                          Triples = Triples.home - Triples.visit,
                          HR = HR.home - HR.visit, 
                          RunsScored = RunsScored.home - RunsScored.visit,
                          SB = SB.home - SB.visit, 
                          CS = CS.home - CS.visit, 
                          IP = IP.home - IP.visit, 
                          RunsAllowed = RunsAllowed.home - RunsAllowed.visit,
                          ER = ER.home - ER.visit,
                          ERA = ERA.home - ERA.visit,
                          PO = PO.home - PO.visit,
                          A = A.home - A.visit,
                          E = E.home - E.visit,
                          DP = DPPerGame.home - DPPerGame.visit,
                          FieldingPct = FieldingPct.home - FieldingPct.visit, 
                          Singles = Singles.home - Singles.visit,
                          TB = TB.home - TB.visit, 
                          SlgPct = SlgPct.home - SlgPct.visit,
                          SuccessRate = SuccessRate.home - SuccessRate.visit,
                          BA = BA.home - BA.visit)
Differences_act <-  mutate(Differences_act, ConferenceDiff = case_when(ConfNumBids.home > 1 & ConfNumBids.visit == 1 ~ 1, ConfNumBids.home == 1 & ConfNumBids.visit > 1 ~ -1, TRUE ~ 0 ))



new_2022 <- Regionals2022[19:24,]
new_2022$RunsAllowed.visit[which(new_2022$Visiting.Team == "Cal St. Fullerton")] <- 2
new_2022$RunsAllowed.home[which(new_2022$Home.Team == "Cal St. Fullerton")] <- 2
new_2022$BA.visit[which(new_2022$Visiting.Team == "Cal St. Fullerton")] <- 0.33
new_2022$BA.home[which(new_2022$Home.Team == "Cal St. Fullerton")] <- 0.33

Differences_new <- mutate(new_2022, Doubles = Doubles.home - Doubles.visit, 
                          Triples = Triples.home - Triples.visit,
                          HR = HR.home - HR.visit, 
                          RunsScored = RunsScored.home - RunsScored.visit,
                          SB = SB.home - SB.visit, 
                          CS = CS.home - CS.visit, 
                          IP = IP.home - IP.visit, 
                          RunsAllowed = RunsAllowed.home - RunsAllowed.visit,
                          ER = ER.home - ER.visit,
                          ERA = ERA.home - ERA.visit,
                          PO = PO.home - PO.visit,
                          A = A.home - A.visit,
                          E = E.home - E.visit,
                          DP = DPPerGame.home - DPPerGame.visit,
                          FieldingPct = FieldingPct.home - FieldingPct.visit, 
                          Singles = Singles.home - Singles.visit,
                          TB = TB.home - TB.visit, 
                          SlgPct = SlgPct.home - SlgPct.visit,
                          SuccessRate = SuccessRate.home - SuccessRate.visit,
                          BA = BA.home - BA.visit)
Differences_new <-  mutate(Differences_new, ConferenceDiff = case_when(ConfNumBids.home > 1 & ConfNumBids.visit == 1 ~ 1, ConfNumBids.home == 1 & ConfNumBids.visit > 1 ~ -1, TRUE ~ 0 ))

est_2022 <- Regionals2022[19:24,]
which(est_2022$Visiting.Team == "Cal St. Fullerton")
which(est_2022$Home.Team == "Cal St. Fullerton")
#replacing CSUF visit data with estimated goals
est_2022$HR.visit[which(est_2022$Visiting.Team == "Cal St. Fullerton")] <- 1.3039820
est_2022$RunsAllowed.visit[which(est_2022$Visiting.Team == "Cal St. Fullerton")] <- 2.631781
est_2022$DPPerGame.visit[which(est_2022$Visiting.Team == "Cal St. Fullerton")] <- 0.2656308
est_2022$SlgPct.visit[which(est_2022$Visiting.Team == "Cal St. Fullerton")] <- 0.5390819
est_2022$Singles.visit[which(est_2022$Visiting.Team == "Cal St. Fullerton")] <- 5.301730
est_2022$Doubles.visit[which(est_2022$Visiting.Team == "Cal St. Fullerton")] <- 1.456112
est_2022$Triples.visit[which(est_2022$Visiting.Team == "Cal St. Fullerton")] <- 0.1414057
est_2022$BA.visit[which(est_2022$Visiting.Team == "Cal St. Fullerton")] <- 0.3195273

#replacing CSUF home data with estimated goals
est_2022$HR.home[which(est_2022$Home.Team == "Cal St. Fullerton")] <- 1.3039820
est_2022$RunsAllowed.home[which(est_2022$Home.Team == "Cal St. Fullerton")] <- 2.631781
est_2022$DPPerGame.home[which(est_2022$Home.Team == "Cal St. Fullerton")] <- 0.2656308
est_2022$SlgPct.home[which(est_2022$Home.Team == "Cal St. Fullerton")] <- 0.5390819
est_2022$Singles.home[which(est_2022$Home.Team == "Cal St. Fullerton")] <- 5.301730
est_2022$Doubles.home[which(est_2022$Home.Team == "Cal St. Fullerton")] <- 1.456112
est_2022$Triples.home[which(est_2022$Home.Team == "Cal St. Fullerton")] <- 0.1414057
est_2022$BA.home[which(est_2022$Home.Team == "Cal St. Fullerton")] <- 0.3195273

#recreating Differences data on regional to apply model
Differences_est <- mutate(est_2022, Doubles = Doubles.home - Doubles.visit, 
                                             Triples = Triples.home - Triples.visit,
                                             HR = HR.home - HR.visit, 
                                             RunsScored = RunsScored.home - RunsScored.visit,
                                             SB = SB.home - SB.visit, 
                                             CS = CS.home - CS.visit, 
                                             IP = IP.home - IP.visit, 
                                             RunsAllowed = RunsAllowed.home - RunsAllowed.visit,
                                             ER = ER.home - ER.visit,
                                             ERA = ERA.home - ERA.visit,
                                             PO = PO.home - PO.visit,
                                             A = A.home - A.visit,
                                             E = E.home - E.visit,
                                             DP = DPPerGame.home - DPPerGame.visit,
                                             FieldingPct = FieldingPct.home - FieldingPct.visit, 
                                             Singles = Singles.home - Singles.visit,
                                             TB = TB.home - TB.visit, 
                                             SlgPct = SlgPct.home - SlgPct.visit,
                                             SuccessRate = SuccessRate.home - SuccessRate.visit,
                                             BA = BA.home - BA.visit)
Differences_est <-  mutate(Differences_est, ConferenceDiff = case_when(ConfNumBids.home > 1 & ConfNumBids.visit == 1 ~ 1, ConfNumBids.home == 1 & ConfNumBids.visit > 1 ~ -1, TRUE ~ 0 ))

#apply model to regional
est.goals.prob <- test6 %>% predict(Differences_est, type = "response")
est.goals.prob

AZRegional <- Regionals2022[19:24,]

ncaa2022_form <- mutate(NCAA2022, Singles = Singles/G,
TB=TB/G,
Doubles=Doubles/G,
Triples=Triples/G,
HR=HR/G,
SB=SB/G,
CS=CS/G,
RunsScored=RunsScored/G,
PO=PO/G,
A=A/G,
E=E/G,
RunsAllowed=RunsAllowed/G)


csuf2022_2 <- Differences2022[(Differences2022$Home.Team %in% c("LSU", "San Diego St.", "Cal St. Fullerton", "Arizona St.")), ]
csufprob2 <- test6 %>% predict(csuf2022_2, type = "response")
csufprob2

csuf2022_3 <- mutate(csuf2022_2, HR = 0)
csufprob3 <- test6 %>% predict(csuf2022_3, type = "response")
csufprob3

View(csuf2022_2)

csuf_actual <- test6 %>% predict(Differences_act, type = "response")
csuf_est <- test6 %>% predict(Differences_est, type = "response")
csuf_new <- test6 %>% predict(Differences_new, type = "response")
Statistic <- c("CSUF 2022 Actual", "Estimated 2022 Goals", "Changing RA in Model")
Chances <- c(0.1151719, 0.2503823, 0.203444)
compare2022AZ <- data.frame(Statistic, Chances)
compareAZ2 <- compare2022AZ[c(1,2),]
View(compare2022)

Chances_LSU <- c(0.2477817, 0.4580774, 0.3925945)
compare2022LSU <- data.frame(Statistic, Chances_LSU)
View(compare2022LSU)

Chances_SD <- c(0.4403116, 0.6687409, 0.6068669)
compare2022SD <- data.frame(Statistic, Chances_SD)
View(compare2022SD)


Arizona.St <- ggplot(data = compare2022AZ[compare2022AZ$Statistic!= "Changing RA in Model",], aes(x = Statistic, y = Chances)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#FF7900") +
  labs(title = "Chances of Winning vs. Arizona St.")+
  scale_y_continuous(labels = percent_format())+
  geom_text(aes(label = percent(Chances, .11)), 
            vjust=1.6, size = 3.5, color = "white")
Arizona.St

Arizona.St2 <- ggplot(data = compare2022AZ, aes(x = Statistic, y = Chances, fill = Statistic)) + 
  geom_bar(stat = "identity", width = 0.75) +
  scale_fill_manual(values = c("#FF7900", "#00274C", "#00274C"))+
  labs(title = "Chances of Winning vs. Arizona St. Using Model")+
  scale_y_continuous(labels = percent_format())+
  geom_text(aes(label = percent(Chances, .11)), 
            vjust=-0.3, size = 3.5, color = "#00274C")
Arizona.St2 + scale_x_discrete(limits = c("CSUF 2022 Actual", "Estimated 2022 Goals", "Changing RA in Model"))


LSU <- ggplot(data = compare2022LSU[compare2022LSU$Statistic!= "Changing RA in Model",], aes(x = Statistic, y = Chances_LSU)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#FF7900") +
  labs(title = "Chances of Winning vs. LSU")+
  scale_y_continuous(labels = percent_format())+
  geom_text(aes(label = percent(Chances_LSU, .11)), 
            vjust=1.6, size = 3.5, color = "white")
LSU

LSU2 <- ggplot(data = compare2022LSU, aes(x = Statistic, y = Chances_LSU, fill = Statistic)) + 
  geom_bar(stat = "identity", width = 0.75) +
  scale_fill_manual(values = c("#FF7900", "#00274C", "#00274C"))+
  labs(title = "Chances of Winning vs. LSU Using Model")+
  scale_y_continuous(labels = percent_format())+
  geom_text(aes(label = percent(Chances_LSU, .11)), 
            vjust=-0.3, size = 3.5, color = "#00274C")
LSU2 + scale_x_discrete(limits = c("CSUF 2022 Actual", "Estimated 2022 Goals", "Changing RA in Model"))


SD <- ggplot(data = compare2022SD[compare2022SD$Statistic!= "Changing RA in Model",], aes(x = Statistic, y = Chances_SD)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#FF7900") +
  labs(title = "Chances of Winning vs. San Diego St.")+
  scale_y_continuous(labels = percent_format())+
  geom_text(aes(label = percent(Chances_SD, .11)), 
            vjust=1.6, size = 3.5, color = "white")
SD

SD2 <- ggplot(data = compare2022SD, aes(x = Statistic, y = Chances_SD, fill = Statistic)) + 
  geom_bar(stat = "identity", width = 0.75) +
  scale_fill_manual(values = c("#FF7900", "#00274C", "#00274C"))+
  labs(title = "Chances of Winning vs. LSU Using Model")+
  scale_y_continuous(labels = percent_format())+
  geom_text(aes(label = percent(Chances_SD, .11)), 
            vjust=-0.3, size = 3.5, color = "#00274C")
SD2 + scale_x_discrete(limits = c("CSUF 2022 Actual", "Estimated 2022 Goals", "Changing RA in Model"))

RegionalsTable <- RegionalGames_Std[c(1,163,229,359), ] %>% rename(`Regional Host` = Regional.Host, `Game Number` = Game.Number, `Home Team` = Home.Team, `Visiting Team` = Visiting.Team, `Home Score` = Home.Score, `Visiting Score` = Visiting.Score, `Home Team Win?` = Home.Win)
RegionalsTable %>%
  gt(caption = "Figure 1: A sample of rows from the dataset. Home team stats are labeled .home and visiting team stats are labeled .visit") %>%
  cols_align(
    align = "center"
  ) %>%
  tab_header(
    title = md("**Regional Games and Team Statistics**"),
    subtitle = "Years 2015 - 2018"
  ) %>%
  tab_options(table.border.top.width=3, table.border.top.color="#00274C", table.border.bottom.width=3,table.border.bottom.color="#00274C", 
              column_labels.border.bottom.color = "#00274C", table_body.hlines.color="#DC8218", column_labels.font.weight="bold")


