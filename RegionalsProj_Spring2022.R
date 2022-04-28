#Read in the data
Softball2015 <- read.csv("Data/Softball2015.csv")
View(Softball2015)
Softball2016 <- read.csv("Data/Softball2016.csv")
View(Softball2016)
Softball2017 <- read.csv("Data/Softball2017.csv")
View(Softball2017)
Softball2018 <- read.csv("Data/Softball2018.csv")
View(Softball2018)

#this is the google spreadsheet
Regionals <- read.csv("Data/NCAA Tournament Games 2015-2018 - Sheet1.csv")
View(Regionals)

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

#Plots double plays per game home vs away
DoublePlays <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=DPPerGame.home, y=DPPerGame.visit, color=Home.Win))
print(DoublePlays)
DoublePlaysRuns <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=DPPerGame.home, y=DPPerGame.visit, color=Run.Diff))
print(DoublePlaysRuns)

#Plots fielding percentage home vs away
FieldingPct <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=FieldingPct.home, y=FieldingPct.visit, color=Home.Win))
print(FieldingPct)
FieldingPctRuns <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=FieldingPct.home, y=FieldingPct.visit, color=Run.Diff))
print(FieldingPctRuns)

#Plots batting average home vs away
BattingAvg <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=BA.home, y=BA.visit, color=Home.Win))
print(BattingAvg)

#Plots home runs
HomeRuns <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=HR.home, y=HR.visit, color=Home.Win))
print(HomeRuns)
HomeRuns2 <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=HR.home, y=HR.visit, color=Run.Diff))
print(HomeRuns2)

#plots stolen bases
StolenBases <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=SB.home, y=SB.visit, color=Home.Win))
print(StolenBases)
StolenBases2 <- ggplot(data=RegionalGames_Std)+geom_point(mapping=aes(x=SB.home, y=SB.visit, color=Run.Diff))
print(StolenBases2)

#creates boxplots based on difference b/w visiting and home in particular stat, filters games where host was neither team
RegionalGames_Std %>% mutate(BA=BA.visit-BA.home) -> BA
BA2 <- filter(BA, Regional.Host != Home.Team, Regional.Host != Visiting.Team)
boxplot(BA~Home.Win, data=BA2)
boxplot(BA.home~Home.Win, data=BA2)
boxplot(BA.visit~Home.Win, data=BA2)

RegionalGames_Std %>% mutate(HR=HR.visit-HR.home) -> HR 
HR2 <- filter(HR, Regional.Host != Home.Team, Regional.Host != Visiting.Team)
boxplot(HR~Home.Win, data=HR2)
boxplot(HR.home~Home.Win, data=HR2)
boxplot(HR.visit~Home.Win, data=HR2)


#Adds column indicating if home team or visiting team (or neither) is regional host
RegionalGames_Std <- mutate(RegionalGames_Std, 
                            Host= case_when(Regional.Host==Home.Team~1, 
                                            Regional.Host==Visiting.Team~-1,
                                            Regional.Host != Home.Team & Regional.Host != Visiting.Team ~ 0) )

#filters based on who was host (if any), also changed difference in BA
RegionalGames_Std %>% mutate(BA=BA.home-BA.visit) -> BA
BAhost.home <- filter(BA, Host==1)
BAhost.visit <- filter(BA, Host==-1)
BAhost.0 <- filter(BA, Host==0)
BAbxplt.home <- boxplot(BA~Home.Win, data=BAhost.home, main ="Home team is Regional Host")
#shows that home team won more when they had a lower batting avg, which doesn't make sense
BAbxplt.visit <- boxplot(BA~Home.Win, data=BAhost.visit, main = "Visiting team is Regional Host")
BAbxplt.0 <- boxplot(BA~Home.Win, data=BAhost.0, main = "Neither team is Regional Host")

#Wanted to see if Runs/Hit gave any explanation for BA not making sense
#like lower batting avg but more runs per hit
RegionalGames_Std %>% mutate(Runs.home=RunsScored.home/H.home, Runs.visit=RunsScored.visit/H.visit, Runs=Runs.home-Runs.visit) -> R
Rhost.home <- filter(R, Host==1)
Rhost.visit <- filter(R, Host==-1)
Rhost.0 <- filter(R, Host==0)
Rbxplt.home <- boxplot(Runs~Home.Win, data=Rhost.home, main="Home team is Regional Host")
Rbxplt.visit <- boxplot(Runs~Home.Win, data=Rhost.visit, main = "Visiting team is Regional Host")
Rbxplt.0 <- boxplot(Runs~Home.Win, data=Rhost.0, main = "Neither team is Regional Host")

#repeats home run boxplots using filter
RegionalGames_Std %>% mutate(HR = HR.home-HR.visit) -> HR
HRhost.home <- filter(HR, Host==1)
HRhost.visit <- filter(HR, Host==-1)
HRhost.0 <- filter(HR, Host==0)
HRbxplt.home <- boxplot(HR~Home.Win, data=HRhost.home, main="Home Host")
HRbxplt.visit <- boxplot(HR~Home.Win, data=HRhost.visit, main="Visit Host")
HRbxplt.0 <- boxplot(HR~Home.Win, data=HRhost.0, main="Neither Host")

#again for stolen base success rate
RegionalGames_Std %>% mutate(Success = SuccessRate.home-SuccessRate.visit) -> Success
Successhost.home <- filter(Success, Host==1)
Successhost.visit <- filter(Success, Host==-1)
Successhost.0 <- filter(Success, Host==0)
Successbxplt.home <- boxplot(Success~Home.Win, data=Successhost.home, main="Home Host")
Successbxplt.visit <- boxplot(Success~Home.Win, data=Successhost.visit, main="Visit Host")
Successbxplt.0 <- boxplot(Success~Home.Win, data=Successhost.0, main="Neither Host")

#again for fielding percentage. This seems important!
RegionalGames_Std %>% mutate(Fielding = FieldingPct.home-FieldingPct.visit) -> Fielding
Fieldinghost.home <- filter(Fielding, Host==1)
Fieldinghost.visit <- filter(Fielding, Host==-1)
Fieldinghost.0 <- filter(Fielding, Host==0)
Fieldingbxplt.home <- boxplot(Fielding~Home.Win, data=Fieldinghost.home, main="Home Host")
Fieldingbxplt.visit <- boxplot(Fielding~Home.Win, data=Fieldinghost.visit, main="Visit Host")
Fieldingbxplt.0 <- boxplot(Fielding~Home.Win, data=Fieldinghost.0, main="Neither Host")

#slugging percentage boxplot
#on base percentage

#validation ~skeleton~ v1
#takes a random assortment of test and train data
dim(RegionalGames_Std)
set.seed(19)
obs=seq(1,397,1)
random=sample(obs,300,replace=F)

Train.random <- RegionalGames_Std[random,]
Test.random <- RegionalGames_Std[-random,]

#fill in for stat and desired number of predictors below
model1 <- lm(response ~ predictor1+predictor2+predictor3, data=Train.random)
fit.stat <- predict(model1, newdata=Test.random)

model1 <- lm(Run.Diff ~ (HR.home+HR.visit)*Host, data=Train.random)
fit.model1 <- predict(model1, newdata=Test.random)
predict_df <- Test.random %>% select(Run.Diff, HR.home, HR.visit, Host)%>%
  mutate(predicted = fit.model1, residual=Run.Diff-predicted)
mean(predict_df$residual^2)



#validation ~skeleton~ v2
#I think this is what we talked about doing initially
Years <- filter(RegionalGames_Std, Year!="2018")
Test.2018 <- filter(RegionalGames_Std, Year=="2018")

#fill in for stat and predictors
model2 <- lm(response~predictor1+predictor2+predictor3, data=Years)
fit.model2 <- predict(model2, newdata=Test.2018)

slg.runs <- lm(Run.Diff ~ (SlgPct.home+SlgPct.visit)*Host, data=Train.random)
fit.slg.runs <- predict(slg.runs, newdata=Test.random)
predict.slg.runs <- Test.random %>% select(Run.Diff, SlgPct.home, SlgPct.visit, Host)%>%
  mutate(slg.predicted = fit.slg.runs, slg.residual=Run.Diff-slg.predicted)
mean(predict.slg.runs$slg.residual^2)

bat.runs <- lm(Run.Diff ~ (SlgPct.home + SlgPct.visit + BA.home + BA.visit)*Host, data=Train.random)
fit.bat.runs <- predict(bat.runs, newdata=Test.random)
predict.bat.runs <- Test.random %>% 
  select(Run.Diff, SlgPct.home, SlgPct.visit, BA.home, BA.visit, Host) %>%
  mutate(bat.predicted=fit.bat.runs, bat.residual=Run.Diff-bat.predicted)
mean(predict.bat.runs$bat.residual^2)

#with this you get an RMSE of ~29 without host, ~18 with host
field.runs <- lm(Run.Diff ~ (FieldingPct.home + FieldingPct.visit + DPPerGame.home + DPPerGame.visit)*Host,
                 data=Train.random)
fit.field.runs <- predict(field.runs, newdata=Test.random)
predict.field.runs <- Test.random %>%
  select(Run.Diff, FieldingPct.home, FieldingPct.visit, DPPerGame.home, DPPerGame.visit, Host) %>%
  mutate(field.predicted=fit.field.runs, field.residual=Run.Diff-field.predicted)
sqrt(mean(predict.field.runs$field.residual^2)) # RMSE

field2.runs <- lm(Run.Diff ~ (FieldingPct.home + FieldingPct.visit)*Host, data=Train.random)
fit.field2.runs <- predict(field2.runs, newdata=Test.random)
predict.field2.runs <- Test.random %>% select(Run.Diff, FieldingPct.home, FieldingPct.visit, Host) %>%
  mutate(field2.predicted=fit.field2.runs, field2.residual=Run.Diff-field2.predicted)
mean(predict.field2.runs$field2.residual^2)

##Logistic Regression
#attach(RegionalGames_Std)
Home.Win <- as.factor(RegionalGames_Std[,"Home.Win"])
is.factor(Home.Win)

# Why did we redo the training/test split and call it something else? We don't remember
# All of this seems to be doing the exact same thing as we did earlier!
dim(RegionalGames_Std)
set.seed(19)
obs=seq(1,397,1)
x=sample(obs,300,replace=F)
Games <- data.frame(RegionalGames_Std) # if RegionalGames_Std is not a data frame, we need to do this
#Games$Home.Win <- ifelse(Games$Home.Win=="Yes", 1,0)
Games$Home.Win <- as.factor(Games$Home.Win)
Games.train=Games[x,]
Games.test=Games[-x,]

#fielding percentage and double plays, misclass rate ~0.37
logi.field <- glm(Home.Win ~ FieldingPct.home + FieldingPct.visit + DPPerGame.home + DPPerGame.visit, 
                  family=binomial, data=Games.train)
fit.logi.field <- logi.field %>% predict(Games.test, type="response")


logi.field.predicted <- ifelse(fit.logi.field > 0.5, "Yes","No")

##### tidymodels example
library(tidymodels)
library(parsnip)
log_reg_model <- logistic_reg() %>% set_engine("glm") # Sets up the model we're going to use
logreg1 <- fit(log_reg_model, formula = Home.Win ~ (FieldingPct.home + FieldingPct.visit)*Host, data = Games.train)
logreg_predictions <- predict(logreg1, new_data = Games.test, type = "prob")
head(logreg_predictions) # data frame with .pred_No and .pred_Yes columns

counter.1=0
for(i in 1:97)
{
  if(logi.field.predicted[i] != Games.test$Home.Win[i])
  {counter.1=counter.1+1}
}

counter.1
misclassification.rate=counte.1r/97
misclassification.rate

# lines 261-270 in one line of code
accuracy <- mean(logi.field.predicted == Games.test$Home.Win) # accuracy
# == checks element-by-element to see if the two are equal
# equal, TRUE = 1, not equal, FALSE = 0
# mean adds up all the TRUEs and divides by the total number

1 - accuracy # misclassification rate

#Slugging Percentage misclass ~0.36
logi.slg <- glm(Home.Win ~ SlgPct.home + SlgPct.visit, family=binomial,
                data=Games.train)
fit.logi.slg <- logi.slg %>% predict(Games.test, type="response")

logi.slg.predicted <- ifelse(fit.logi.slg > 0.5, "Yes", "No")

counter.2=0
for(i in 1:97)
{
  if(logi.slg.predicted[i] != Games.test$Home.Win[i])
  {counter.2=counter.2+1}
}

counter.2
misclassification.rate=counter.2/97
misclassification.rate

#slg pct relative to host misclass ~0.27
logi.slg2 <- glm(Home.Win ~ (SlgPct.home + SlgPct.visit)*Host, family=binomial,
                data=Games.train)
fit.logi.slg2 <- logi.slg2 %>% predict(Games.test, type="response")

logi.slg2.predicted <- ifelse(fit.logi.slg2 > 0.5,1,0)

counter.3=0
for(i in 1:97)
{
  if(logi.slg2.predicted[i] != Games.test$Home.Win[i])
  {counter.3=counter.3+1}
}

counter.3
misclassification.rate=counter.3/97
misclassification.rate



######tidymodels stuff
library(tidymodels)
library(parsnip)
tidymodels_prefer()

#this is the tidymodels version, but creates a tibble. How to change to numerical?
Games_split <- initial_split(RegionalGames_Std, prop = 0.75)
Games_train <- training(Games_split)
Games_test <- testing(Games_split)
Games_test_small <- Games_test %>% slice_sample(n=10)

#using previous split instead done in lines 247-249
#MSE: ~24.23
lm_model1 <- linear_reg() %>% set_engine("lm")
lm_model1_fit <- lm_model1 %>%
  fit(Run.Diff ~ RunsAllowed.home + RunsAllowed.visit + PO.home + PO.visit + 
        SlgPct.home + SlgPct.visit, data = Games.train)
model1_results <- lm_model1_fit %>% extract_fit_engine() %>% summary()
model1_results
model1_pred <- predict(lm_model1_fit, new_data = Games.test)
compare1 <- Games.test %>% select(
  Run.Diff, RunsAllowed.home, RunsAllowed.visit, PO.home, PO.visit, 
  SlgPct.home, SlgPct.visit)%>%
  mutate(model = model1_pred, resid = Run.Diff-model)
mean(as.numeric(unlist(compare1$resid^2))) #why do I need to do this now?


#MSE: ~26.78
lm_model2 <- linear_reg() %>% set_engine("lm")
lm_model2_fit <- lm_model2 %>%
  fit(Run.Diff ~ RunsAllowed.home + PO.home + SlgPct.home, data = Games.train)
model2_results <- lm_model2_fit %>% extract_fit_engine() %>% summary()
model2_results
model2_pred <- predict(lm_model2_fit, new_data = Games.test)
compare2 <- Games.test %>% select(
  Run.Diff, RunsAllowed.home, PO.home, 
  SlgPct.home)%>%
  mutate(model = model2_pred, resid = Run.Diff-model)
mean(as.numeric(unlist(compare2$resid^2)))

#MSE: ~18.24
lm_model3 <- linear_reg() %>% set_engine("lm")
lm_model3_fit <- lm_model3 %>%
  fit(Run.Diff ~ (RunsAllowed.home + RunsAllowed.visit + PO.home + PO.visit + 
                    SlgPct.home + SlgPct.visit)*Host, data = Games.train)
model3_results <- lm_model3_fit %>% extract_fit_engine() %>% summary()
model3_results
model3_pred <- predict(lm_model3_fit, new_data = Games.test)
compare3 <- Games.test %>% select(
  Run.Diff, RunsAllowed.home, PO.home, 
  SlgPct.home, Host)%>%
  mutate(model = model3_pred, resid = Run.Diff-model)
mean(as.numeric(unlist(compare3$resid^2)))

##Logistic Regression

log_reg_model1 <- logistic_reg() %>% set_engine("glm")
logreg1 <- fit(log_reg_model1, formula = Home.Win ~ RunsAllowed.home + RunsAllowed.visit + 
                 PO.home + PO.visit + 
                 SlgPct.home + SlgPct.visit, 
               data=Games_train)
logreg_predictions <- predict(logreg1, new_data = Games_test_small, type = "prob")
head(logreg_predictions)

logi_vector <- ifelse(logreg_predictions$.pred_Yes > 0.5, "Yes", "No")
accuracy_model1 <- mean(logi_vector == Games_test_small$Home.Win)

#accuracy ~ 0.74
logreg_predictions_v2 <- predict(logreg1, new_data = Games.test, type = "prob")
logi_vector_v2 <- ifelse(logreg_predictions_v2$.pred_Yes > 0.5, "Yes", "No")
accuracy_model1_v2 <- mean(logi_vector_v2 == Games.test$Home.Win)
table(logi_vector_v2, Games.test$Home.Win) #textbook

#accuracy ~ 0.77
log_reg_model2 <- logistic_reg() %>% set_engine("glm")
logreg2 <- fit(log_reg_model2, formula = Home.Win ~ (RunsAllowed.home + RunsAllowed.visit +
                                                       PO.home + PO.visit +
                                                       SlgPct.home + SlgPct.visit)*Host, data=Games.train)
logreg_predictions2 <- predict(logreg2, new_data = Games.test, type = "prob")

logi_vector2 <- ifelse(logreg_predictions2$.pred_Yes > 0.5, "Yes", "No")
accuracy_model2 <- mean(logi_vector2 == Games.test$Home.Win)
accuracy_model2

#accuracy ~ 0.71 (w/o host ~ 0.69)
log_reg_model3 <- logistic_reg() %>% set_engine("glm")
logreg3 <- fit(log_reg_model3, formula = Home.Win ~ (RunsAllowed.home + RunsAllowed.visit + 
                 PO.home + PO.visit + 
                 ERA.home + ERA.visit)*Host, 
               data=Games.train)
logreg_predictions3 <- predict(logreg3, new_data = Games.test, type = "prob")
logi_vector3 <- ifelse(logreg_predictions3$.pred_Yes > 0.5, "Yes", "No")
accuracy_model3 <- mean(logi_vector3 == Games.test$Home.Win)
accuracy_model3

log_reg_model4 <-logistic_reg() %>% set_engine("glm")
logreg4 <- fit(log_reg_model4, formula = Home.Win ~ (SlgPct.home + SlgPct.visit + 
                 PO.home + PO.visit + 
                 ERA.home + ERA.visit +
                 DPPerGame.home + DPPerGame.visit)*Host, 
               data=Games.train)
logreg_predictions4 <- predict(logreg4, new_data = Games.test, type = "prob")
logi_vector4 <- ifelse(logreg_predictions4$.pred_Yes > 0.5, "Yes", "No")
accuracy_model4 <- mean(logi_vector4 == Games.test$Home.Win)
accuracy_model4

log_reg_model5 <- logistic_reg() %>% set_engine("glm")

log_reg_model2 <- logistic_reg() %>% set_engine("glm")
logreg2 <- fit(log_reg_model2, formula = Home.Win ~ (I(RunsAllowed.home - RunsAllowed.visit) +
                                                       I(A.home - A.visit) +
                                                       I(SlgPct.home - SlgPct.visit))*Host, data=Games.train)
logreg_predictions2 <- predict(logreg2, new_data = Games.test, type = "prob")

logi_vector2 <- ifelse(logreg_predictions2$.pred_Yes > 0.5, "Yes", "No")
accuracy_model2 <- mean(logi_vector2 == Games.test$Home.Win)
accuracy_model2

plot(RunsScored.home ~ PO.home, data=RegionalGames_Std)
table(RegionalGames_Std$Host, RegionalGames_Std$Home.Win)






##attempting other methods

#LDA 

library(MASS)

#allows for assumption of same variance in predictors
scaled_train <- scale(select(Games.train, RunsAllowed.home, RunsAllowed.visit, 
                             PO.home, PO.visit, SlgPct.home, SlgPct.visit))

lda.fit <- lda(Home.Win ~ RunsAllowed.home + RunsAllowed.visit + PO.home + PO.visit +
                 SlgPct.home + SlgPct.visit, data = scaled.train)
lda.fit
#~40% of the time, home team did not win (comes from training set)
#shows average for each predictor in each category

lda_predict <- predict(lda.fit, Games.test)
mean(lda_predict$class == Games.test$Home.Win)

#naive Bayes
library(e1071)
nb.model <- naive_Bayes() %>% set_engine("klaR")
nb.fit <- fit(nb.model, formula = Home.Win ~ RunsAllowed.home + RunsAllowed.visit + 
                                       PO.home + PO.visit + 
                                       ERA.home + ERA.visit,
              data = Games.train)
nb.predict <- predict(nb.fit, Games.test)

#Knn
knn_spec <- nearest_neighbor %>% 
  set_engine("kknn") %>% 
  set_mode("classification")
knn_fit <- knn_spec %>%
  fit(Home.Win ~ (RunsAllowed.home + RunsAllowed.visit + 
                             PO.home + PO.visit + 
                             ERA.home + ERA.visit)*Host, data = Games_train)

#look at 2018 arizona regional and verify that mississippi valley state was a home team in mult games