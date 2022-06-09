library(dplyr)


# First: function to predict all 12 possible regionals games based on a model
predict_regionals_games <- function(teams, stats, game_model){
  # teams: a vector of four teams, seeded 1-4 with Team 1 = Host
  # stats: a data frame of all the season stats for teams
  # game_model: the glm containing the coefficients for the model
  stats <- mutate(stats, Singles = Singles/G,
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
  regionals_stats <- stats %>% filter(Team %in% teams)  # filter the dataset to get the host
  
  # Step 1: find the chance of each team winning under each home vs. visitor
  # 12 total games - 4 home x 3 visiting
  # E.g. CSUF @ ASU, CSUF @ LSU, CSUF @ SDSU, ASU @ CSUF, LSU @ CSUF, SDSU @ CSUF,
  # ASU @ SDSU, LSU @ SDSU, SDSU @ LSU, SDSU @ ASU, LSU @ ASU, ASU @ LSU
  # Step 1a: Set up a 12 row data frame with 1 row per possible game
  # Step 1b: mutate this data frame the same way we got the Differences data frame
  # Step 1c: predict on this data frame using game_model and cbind the probabilities
  # At the end of Step 1, we have the chance of each team winning against each other team
  # with each possible home vs. visitor
  
  games_df <- data.frame(
    Home.Team = rep(teams, each = 3),
    Visiting.Team = c(teams[2:4], teams[c(1,3:4)], teams[c(1:2, 4)], teams[1:3])
  ) 
  
  games_stats <- games_df %>%
    left_join(regionals_stats, by = c("Home.Team" = "Team"),  
                                             suffix = c(".x", ".home")) %>% left_join(regionals_stats, 
                                                                                      by = c("Visiting.Team" = "Team"), suffix = c(".home",".visit")) %>%
    mutate(
      Host = case_when(Home.Team == teams[1] ~ 1,
                       Visiting.Team == teams[1] ~ -1,
                       TRUE ~ 0),
      Doubles = Doubles.home - Doubles.visit, 
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
                                ConferenceDiff = case_when(ConfNumBids.home > 1 & ConfNumBids.visit == 1 ~ 1, ConfNumBids.home == 1 & ConfNumBids.visit > 1 ~ -1, TRUE ~ 0))
  
  games_predictions <- predict(game_model, newdata = games_stats, type = "response")
  
  return(games_predictions)
}


# Now we simulate the entire regionals and find the probability of each team advancing

simulate_regionals <- function(teams, winprobs){
  
  games_df2 <- data.frame(
    Home.Team = rep(teams, each = 3),
    Visiting.Team = c(teams[2:4], teams[c(1,3:4)], teams[c(1:2, 4)], teams[1:3]),
    home_win = winprobs,
    visiting_win = 1 - winprobs
  ) 

  # Step 2: Simulate first round
  # Chance of each team reaching 1-0 game, chance of each team reaching 0-1 game
  # 1 home vs 4 visitor, 2 home vs 3 visitor
  # Step 2a: Set up a df with 4 rows (1 per team) x 2 columns
  # column 1: chance of being in 1-0 game, column 2: chance of being in 0-1 game
  
  # Step 3: Simulate second round
  # possibilities for each game: 1 home vs 2 visitor, 1 home vs 3 visitor, 2 home vs 4 visitor, 3 home vs 4 visitor
  # 8 games (4 possibilities * 2 brackets - winner and loser bracket)
  # For each of these games, we need P(it happens) = multiply probabilities from Step 2
  # For each game, P(it happens)*P(team X wins | it happens)
  # Add up probabilities across games to get P(WW), P(WL), P(LW)
  # After Round 2, we should have 4 x 3 matrix
  # Columns: P(WW), P(WL), P(LW)
  # E.g., P(WL) for Team 1 = P(1 vs 2 in Winners)*P(1 loses to 2) + P(1 vs 3 in Winners)*P(1 loses to 3)
  
  # Step 4: Simulate 1-1 game
  # Any of the 12 games
  # LW is home, WL is visitor
  # For each of 12 games, we need P(it happens)
  R3 <- c(
      games_df2$home_win[5]*games_df2$visiting_win[3]*games_df2$home_win[2]*games_df2$visiting_win[6], # 1H 2V 4 WW
      games_df2$visiting_win[5]*games_df2$visiting_win[3]*games_df2$home_win[1]*games_df2$visiting_win[9], # 1H 3V 4 WW
      games_df2$visiting_win[3]*games_df2$home_win[5]*games_df2$home_win[2]*games_df2$home_win[6], # 1H 4V 2WW
      games_df2$visiting_win[3]*games_df2$visiting_win[5]*games_df2$home_win[1]*games_df2$home_win[9], # 1H 4V 3 WW
      games_df2$home_win[3]*games_df2$visiting_win[5]*games_df2$home_win[6]*games_df2$visiting_win[2], # 2H 1V 3 WW
      games_df2$visiting_win[5]*games_df2$home_win[3]*games_df2$home_win[6]*games_df2$home_win[2], # 2H 3V 1 WW
      games_df2$visiting_win[5]*games_df2$visiting_win[3]*games_df2$visiting_win[1]*games_df2$visiting_win[9], # 2H 3V 4 WW
      games_df2$visiting_win[3]*games_df2$visiting_win[5]*games_df2$visiting_win[1]*games_df2$home_win[9], # 2H 4V 3 WW
      games_df2$home_win[3]*games_df2$home_win[5]*games_df2$home_win[9]*games_df2$visiting_win[1], # 3H 1V 2 WW
      games_df2$home_win[5]*games_df2$home_win[3]*games_df2$home_win[9]*games_df2$home_win[1], # 3H 2V 1 WW 
      games_df2$home_win[5]*games_df2$visiting_win[3]*games_df2$visiting_win[2]*games_df2$visiting_win[6], # 3H 2V 4 WW
      games_df2$visiting_win[3]*games_df2$home_win[5]*games_df2$visiting_win[2]*games_df2$home_win[6], # 3H 4V 2 WW
      games_df2$home_win[3]*games_df2$home_win[5]*games_df2$visiting_win[9]*games_df2$visiting_win[1], # 4H 1V 2 WW
      games_df2$home_win[3]*games_df2$visiting_win[5]*games_df2$visiting_win[6]*games_df2$visiting_win[2], # 4H 1V 3 WW
      games_df2$home_win[3]*games_df2$home_win[5]*games_df2$visiting_win[9]*games_df2$home_win[1], # 4H 2V 1 WW
      games_df2$home_win[3]*games_df2$visiting_win[5]*games_df2$visiting_win[6]*games_df2$home_win[2] # 4H 3V 2 1 WW
    )
  
  # P(Each team wins | game happens) * P(it happens)
  # Add up probabilities across games to get P(2-1)
  # P(2-1) for Team 1 = P(1 LW, 4 WL)*P(1 beats 4 when home) + P(1 WL, 4 LW)*P(1 beats 4 when visiting) + [same thing for playing 2 and 3]
  # P(2-1) for Team = P(2 LW, 4 WL)*P(2 beats 4 when home) + P(2 WL, 4 LW)*P(2 beats 4 when visiting) + [same idea for opponents 1 and 3]  

  # round3_df <- data.frame(Team = teams,
  #                         W21 = c(
  #                           sum(R3[c(1, 2, 3, 4)]*games_df3$home_win[c(1, 2, 3, 3)] + R3[c(5, 9, 13, 14)]*games_df3$visiting_win[c(4, 7, 10, 10)]),
  #                           sum(R3[c(5, 6, 7, 8)]*games_df3$home_win[c(4, 5, 5, 6)] + R3[c(1, 10, 11, 15)]*games_df3$visiting_win[c(1, 8, 8, 11)]),
  #                           sum(R3[c(9, 10, 11, 12)]*games_df3$home_win[c(7, 8, 8, 9)] + R3[c(2, 6, 7, 16)]*games_df3$visiting_win[c(2, 5, 5, 12)]),
  #                           sum(R3[c(13, 14, 15, 16)]*games_df3$home_win[c(10, 10, 11, 12)] + R3[c(3, 4, 8, 12)]*games_df3$visiting_win[c(3, 3, 6, 9)])
  #                         ))
  
  # Step 5: Simulate 2-0 vs. 2-1 game
  # 2-0 team (WW from Step 3) is visitor, 2-1 (from Step 4) is home
  # If necessary (loser's bracket team wins forces winner take all Game 7), 2-0 team is home, 2-1 is visitor in Game 7
  # Any of the 12 games, we need P(it happens)
  # P(Each team wins | game happens) * P(it happens)
  # P(3-0) for Team 1 = P(1 WW, 4 2-1)*P(1 beats 4 when visting) + [same thing for playing 2 & 3]
  # P(3-1) for Team 1 = P(1 WW, 4 2-1)*P(4 beats 1 when home)*P(1 beats 4 when home) + [same thing for playing 2 and 3]
  # P(4-1) for Team 1 = P(1 2-1, 4 WW)*P(1 beats 4 when home)*P(1 beats 4 when visiting) + [same thing for playing 2 and 3]
  # Probability of winning regional = P(3-0) + P(3-1) + P(4-1)

  R4 <- c(
    R3[6]*games_df2$home_win[5] + R3[10]*games_df2$visiting_win[8] + R3[15]*games_df2$visiting_win[11], # 1 H 2 V
    R3[6]*games_df2$visiting_win[5] + R3[10]*games_df2$home_win[8] + R3[16]*games_df2$visiting_win[12], # 1 H 3 V
    R3[15]*games_df2$home_win[11] + R3[16]*games_df2$home_win[12], # 1 H 4 V
    R3[3]*games_df2$home_win[3] + R3[9]*games_df2$visiting_win[7] + R3[13]*games_df2$visiting_win[10], # 2 H 1 V
    R3[9]*games_df2$home_win[7] + R3[12]*games_df2$home_win[9], # 2 H 3 V
    R3[3]*games_df2$visiting_win[3] + R3[12]*games_df2$visiting_win[9] + R3[13]*games_df2$home_win[10], # 2 H 4 V
    R3[4]*games_df2$home_win[3] + R3[5]*games_df2$visiting_win[4] + R3[14]*games_df2$visiting_win[10], # 3 H 1 V
    R3[5]*games_df2$home_win[4] + R3[8]*games_df2$home_win[6], # 3 H 2 V
    R3[4]*games_df2$visiting_win[3] + R3[8]*games_df2$visiting_win[6] + R3[14]*games_df2$home_win[10], # 3 H 4 V
    R3[2]*games_df2$home_win[2] + R3[1]*games_df2$home_win[1], # 4 H 1 V
    R3[7]*games_df2$home_win[5] + R3[11]*games_df2$visiting_win[8] + R3[1]*games_df2$visiting_win[1], # 4 H 2 V
    R3[2]*games_df2$visiting_win[2] + R3[7]*games_df2$visiting_win[5] + R3[11]*games_df2$home_win[8] 
  )
  
  round4_df <- data.frame(Team = teams,
                          W30 = c(
                            R4[1]*games_df2$visiting_win[4] + R4[2]*games_df2$visiting_win[7] + R4[3]*games_df2$visiting_win[10], # 1W
                          R4[4]*games_df2$visiting_win[1] + R4[5]*games_df2$visiting_win[8]+ R4[6]*games_df2$visiting_win[11], # 2W
                          R4[7]*games_df2$visiting_win[2] + R4[8]*games_df2$visiting_win[5] + R4[9]*games_df2$visiting_win[12], # 3W
                          R4[10]*games_df2$visiting_win[3] + R4[11]*games_df2$visiting_win[6] + R4[12]*games_df2$visiting_win[9] # 4W
                            ),
                          W31 = c(
                            R4[1]*games_df2$home_win[4]*games_df2$home_win[1] + R4[2]*games_df2$home_win[7]*games_df2$home_win[2] + R4[3]*games_df2$home_win[10]*games_df2$home_win[3], # 1W
                            R4[4]*games_df2$home_win[1]*games_df2$home_win[4] + R4[5]*games_df2$home_win[8]*games_df2$home_win[5] + R4[6]*games_df2$home_win[11]*games_df2$home_win[6], # 2W
                            R4[7]*games_df2$home_win[2]*games_df2$home_win[7] + R4[8]*games_df2$home_win[5]*games_df2$home_win[8] + R4[9]*games_df2$home_win[12]*games_df2$home_win[9], # 3W
                            R4[10]*games_df2$home_win[3]*games_df2$home_win[10] + R4[11]*games_df2$home_win[6]*games_df2$home_win[11] + R4[12]*games_df2$home_win[9]*games_df2$home_win[12] # 4W
                          ),
                          W41 = c(
                            R4[4]*games_df2$home_win[1]*games_df2$visiting_win[4] + R4[7]*games_df2$home_win[2]*games_df2$visiting_win[7] + R4[10]*games_df2$home_win[3]*games_df2$visiting_win[10], # 1W
                            R4[1]*games_df2$home_win[4]*games_df2$visiting_win[1] + R4[8]*games_df2$home_win[5]*games_df2$visiting_win[8] + R4[11]*games_df2$home_win[6]*games_df2$visiting_win[11], # 2W
                            R4[2]*games_df2$home_win[7]*games_df2$visiting_win[2] + R4[5]*games_df2$home_win[8]*games_df2$visiting_win[5] + R4[12]*games_df2$home_win[9]*games_df2$visiting_win[12], # 3W
                            R4[3]*games_df2$home_win[10]*games_df2$visiting_win[3] + R4[6]*games_df2$home_win[11]*games_df2$visiting_win[6] + R4[9]*games_df2$home_win[12]*games_df2$visiting_win[9] # 4 W
                          )) %>%
    transmute(Team, Advance = W30 + W31 + W41)
  
  return(round4_df) # data frame with 4 teams and each team's chance of winning regional  

}
