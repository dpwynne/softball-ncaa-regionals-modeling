#Read in the data
Softball2015 <- read.csv("C:/Users/hurri/CSUF/Spring 2022/Softball/Softball2015.csv")
View(Softball2015)
Softball2016 <- read.csv("C:/Users/hurri/CSUF/Spring 2022/Softball/Softball2016.csv")
View(Softball2016)
Softball2017 <- read.csv("C:/Users/hurri/CSUF/Spring 2022/Softball/Softball2017.csv")
View(Softball2017)
Softball2018 <- read.csv("C:/Users/hurri/CSUF/Spring 2022/Softball/Softball2018.csv")
View(Softball2018)

#this is the google spreadsheet
Regionals <- read.csv("C:/Users/hurri/CSUF/Spring 2022/Softball/NCAA Tournament Games 2015-2018 - Sheet1.csv")
View(Regionals)

library(tidyverse)
library(ggplot2)

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
RegionalStats <- mutate(RegionalStats, FieldingPct = (PO+A)/(PO+A+E), Singles = H - Doubles - Triples - HR, TB = Singles + 2*Doubles + 3*Triples + 4*HR, DPPerGame = DP/G, SlgPct = TB/AB, SuccessRate = SB/(SB+CS))
RegionalStats <- mutate(RegionalStats, BA=H/AB)
