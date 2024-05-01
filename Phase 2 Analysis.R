library(tidyverse)
library(readxl)
options(scipen = 999, digits = 10)

##Merging, filtering, and manipulating data
savant_collection_pitch_2021 <- read.csv("savant_collection_pitch_2021.csv") %>%
  select(-1)
savant_collection_pitch_2022 <- read.csv("savant_collection_pitch_2022.csv") %>%
  select(-1)
savant_collection_pitch_2023 <- read.csv("savant_collection_pitch_2023.csv") %>%
  select(-1)
savant_collection <- rbind(savant_collection_pitch_2021, savant_collection_pitch_2022, savant_collection_pitch_2023)
  

savant_collection$game_pk <- as.numeric(savant_collection$game_pk)
savant_collection$events[savant_collection$events == ""] <- NA
pk_df <- savant_collection %>%
  group_by(game_pk) %>%
  mutate(max_at_bat = max(at_bat_number)) %>%
  mutate(home_final = ifelse(at_bat_number == max_at_bat & !is.na(events), post_home_score, NA),
         away_final = ifelse(at_bat_number == max_at_bat & !is.na(events), post_away_score, NA)) %>%
  select(game_pk, home_final, away_final) %>%
  distinct() %>%
  filter(!is.na(home_final))
savant_collection <- savant_collection %>%
  left_join(pk_df, by=c("game_pk" = "game_pk"))

write.csv(savant_collection, "savant_collection.csv")
savant_collection <- read.csv("savant_collection.csv") %>%
  select(-1)

savant_collection$game_date <- as.Date(savant_collection$game_date)

Teams_Plus_Sim <- read_csv("Team_Plus_Sim.csv")
Teams_Plus_Sim$Date <- strptime(Teams_Plus_Sim$Date, "%m/%d/%Y")
Teams_Plus_Sim$Date <- format(Teams_Plus_Sim$Date, format="%Y-%m-%d")
Teams_Plus_Sim$Date <- as.Date(Teams_Plus_Sim$Date, format="%Y-%m-%d")
Teams_Plus_Sim$Year <- as.numeric(Teams_Plus_Sim$Year)
Teams_Plus_Sim_Merge <- Teams_Plus_Sim %>%
  filter(Year == "2023" | Year == "2022" | Year == "2021")

pitches_plus_umpires <- savant_collection %>%
  full_join(Teams_Plus_Sim_Merge, by=c("home_team" = "Home", "away_team" = "Away", "game_date" = "Date", "game_year" = "Year", "home_final" = "R_H", "away_final" = "R_A")) %>%  
  mutate(Vertical_Movement = pfx_z * 12,
         Horizontal_Movement = pfx_x * 12)
pitches_plus_umpires <- pitches_plus_umpires %>%
  select(-7:-8, -11:-14, -17, -23, -24, -28:-29, -38:-50, -53:-55, -60:-76, -82:-89, -96:-97, -100:-101, -104:-106)
pitches_plus_umpires <- distinct(pitches_plus_umpires)

write.csv(pitches_plus_umpires, "pitches_plus_umpires.csv")

test <- pitches_plus_umpires %>%
  select(2, 32, 43) %>%
  filter(is.na(Umpire)) %>%
  distinct()

pitches_plus_missedcalls <- pitches_plus_umpires %>%
  mutate(Incorrect_Ball = ifelse(description == "ball" & 
                                  plate_z < (sz_top + (1.45/12)) & 
                                  plate_z > (sz_bot - (1.45/12)) & 
                                  plate_x > -(((1.45*2 + 17) / 12) / 2) & 
                                  plate_x < (((1.45*2 + 17) / 12) / 2), 1, 0)) %>%
  mutate(Incorrect_Strike = ifelse(description == "called_strike" & plate_z > (sz_top + (1.45/12)) | 
                                description == "called_strike" & plate_z < (sz_bot + -(1.45/12)) | 
                                description == "called_strike" & plate_x < -(((1.45/12*2 + 17) / 12) / 2) | 
                                description == "called_strike" & plate_x > (((1.45/12 + 17) / 12) / 2), 1, 0)) %>%
  mutate(Incorrect_Call = ifelse(Incorrect_Ball == 1 | Incorrect_Strike == 1, 1, 0))

Incorrect_Calls <- pitches_plus_missedcalls %>%
  filter(Incorrect_Call == 1)

`0_Outs` <- read.csv("0_Outs.csv", header = TRUE)
`1_Out` <- read.csv("1_Out.csv", header = TRUE)
`2_Outs` <- read.csv("2_Outs.csv", header = TRUE)

pitches_plus_missedcalls <- pitches_plus_missedcalls %>%
  mutate(count = ifelse(balls == 0 & strikes == 0, "0.0",
                 ifelse(balls == 1 & strikes == 0, "1.0",
                 ifelse(balls == 2 & strikes == 0, "2.0",
                 ifelse(balls == 3 & strikes == 0, "3.0",
                 ifelse(balls == 0 & strikes == 1, "0.1",
                 ifelse(balls == 1 & strikes == 1, "1.1",
                 ifelse(balls == 2 & strikes == 1, "2.1",
                 ifelse(balls == 3 & strikes == 1, "3.1",
                 ifelse(balls == 0 & strikes == 2, "0.2",
                 ifelse(balls == 1 & strikes == 2, "1.2",
                 ifelse(balls == 2 & strikes == 2, "2.2",
                 ifelse(balls == 3 & strikes == 2, "3.2", "N/A"
                 )))))))))))))
pitches_plus_missedcalls <- pitches_plus_missedcalls %>%
  drop_na(outs_when_up) %>%
  drop_na(Incorrect_Call)
pitches_plus_missedcalls$difference <- NA

#Loop to creating the missed call statistics
for (row in 1:nrow(pitches_plus_missedcalls)) {
  if (pitches_plus_missedcalls$outs_when_up[row] == 0) 
  { #OUTS CHUNK
    if (!is.na(pitches_plus_missedcalls$on_1b[row]) == F & 
        !is.na(pitches_plus_missedcalls$on_2b[row]) == F &
        !is.na(pitches_plus_missedcalls$on_3b[row]) == F) 
    { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.0`[1] - `0_Outs`$`X0.1`[1]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.1`[1] - `0_Outs`$`X1.0`[1]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.0`[1] - `0_Outs`$`X1.1`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[1] - `0_Outs`$`X2.0`[1])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.0`[1] - `0_Outs`$`X2.1`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[1] - `0_Outs`$`X3.0`[1])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[2] - `0_Outs`$`X3.1`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[1] - `0_Outs`$`X0.0`[2])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[1] - `0_Outs`$`X0.2`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.2`[1] - `0_Outs`$`X1.1`[1])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[1] - `0_Outs`$`X1.2`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[1] - `0_Outs`$`X2.1`[1])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[1] - `0_Outs`$`X2.2`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[1] - `0_Outs`$`X2.1`[1])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[2] - `0_Outs`$`X3.2`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[1] - `0_Outs`$`X0.0`[2])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[1] - `1_Out`$`X0.0`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[1] - `0_Outs`$`X1.2`[1])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[1] - `1_Out`$`X0.0`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[1] - `0_Outs`$`X2.2`[1])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[1] - `1_Out`$`X0.0`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[1] - `0_Outs`$`X3.2`[1])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[2] - `1_Out`$`X0.0`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[1] - `0_Outs`$`X0.0`[2])
        } else {
          invisible()
        }
      }
    } else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == T & 
               !is.na(pitches_plus_missedcalls$on_2b[row]) == F &
               !is.na(pitches_plus_missedcalls$on_3b[row]) == F) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.0`[2] - `0_Outs`$`X0.1`[2]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.1`[2] - `0_Outs`$`X1.0`[2]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.0`[2] - `0_Outs`$`X1.1`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[2] - `0_Outs`$`X2.0`[2])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.0`[2] - `0_Outs`$`X2.1`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[2] - `0_Outs`$`X3.0`[2])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[4] - `0_Outs`$`X3.1`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[2] - `0_Outs`$`X0.0`[4])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[2] - `0_Outs`$`X0.2`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.2`[2] - `0_Outs`$`X1.1`[2])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[2] - `0_Outs`$`X1.2`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[2] - `0_Outs`$`X2.1`[2])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[2] - `0_Outs`$`X2.2`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[2] - `0_Outs`$`X2.1`[2])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[4] - `0_Outs`$`X3.2`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[2] - `0_Outs`$`X0.0`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[2] - `1_Out`$`X0.0`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[2] - `0_Outs`$`X1.2`[2])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[2] - `1_Out`$`X0.0`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[2] - `0_Outs`$`X2.2`[2])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[2] - `1_Out`$`X0.0`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[2] - `0_Outs`$`X3.2`[2])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[4] - `1_Out`$`X0.0`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[2] - `0_Outs`$`X0.0`[4])
        } else {
          invisible()
        }
      }
    }
    else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == F & 
             !is.na(pitches_plus_missedcalls$on_2b[row]) == T &
             !is.na(pitches_plus_missedcalls$on_3b[row]) == F) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.0`[3] - `0_Outs`$`X0.1`[3]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.1`[3] - `0_Outs`$`X1.0`[3]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.0`[3] - `0_Outs`$`X1.1`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[3] - `0_Outs`$`X2.0`[3])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.0`[3] - `0_Outs`$`X2.1`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[3] - `0_Outs`$`X3.0`[3])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[4] - `0_Outs`$`X3.1`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[3] - `0_Outs`$`X0.0`[4])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[3] - `0_Outs`$`X0.2`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.2`[3] - `0_Outs`$`X1.1`[3])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[3] - `0_Outs`$`X1.2`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[3] - `0_Outs`$`X2.1`[3])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[3] - `0_Outs`$`X2.2`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[3] - `0_Outs`$`X2.1`[3])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[4] - `0_Outs`$`X3.2`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[3] - `0_Outs`$`X0.0`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[3] - `1_Out`$`X0.0`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[3] - `0_Outs`$`X1.2`[3])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[3] - `1_Out`$`X0.0`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[3] - `0_Outs`$`X2.2`[3])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[3] - `1_Out`$`X0.0`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[3] - `0_Outs`$`X3.2`[3])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[4] - `1_Out`$`X0.0`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[3] - `0_Outs`$`X0.0`[4])
        } else {
          invisible()
        }
      }
    } else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == T & 
               !is.na(pitches_plus_missedcalls$on_2b[row]) == T &
               !is.na(pitches_plus_missedcalls$on_3b[row]) == F) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.0`[4] - `0_Outs`$`X0.1`[4]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.1`[4] - `0_Outs`$`X1.0`[4]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.0`[4] - `0_Outs`$`X1.1`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[4] - `0_Outs`$`X2.0`[4])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.0`[4] - `0_Outs`$`X2.1`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[4] - `0_Outs`$`X3.0`[4])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[8] - `0_Outs`$`X3.1`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[4] - `0_Outs`$`X0.0`[8])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[4] - `0_Outs`$`X0.2`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.2`[4] - `0_Outs`$`X1.1`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[4] - `0_Outs`$`X1.2`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[4] - `0_Outs`$`X2.1`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[4] - `0_Outs`$`X2.2`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[4] - `0_Outs`$`X2.1`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[8] - `0_Outs`$`X3.2`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[4] - `0_Outs`$`X0.0`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[4] - `1_Out`$`X0.0`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[4] - `0_Outs`$`X1.2`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[4] - `1_Out`$`X0.0`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[4] - `0_Outs`$`X2.2`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[4] - `1_Out`$`X0.0`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[4] - `0_Outs`$`X3.2`[4])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[8] - `1_Out`$`X0.0`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[4] - `0_Outs`$`X0.0`[8])
        } else {
          invisible()
        }
      }
    }
    else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == F & 
             !is.na(pitches_plus_missedcalls$on_2b[row]) == F &
             !is.na(pitches_plus_missedcalls$on_3b[row]) == T) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.0`[5] - `0_Outs`$`X0.1`[5]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.1`[5] - `0_Outs`$`X1.0`[5]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.0`[5] - `0_Outs`$`X1.1`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[5] - `0_Outs`$`X2.0`[5])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.0`[5] - `0_Outs`$`X2.1`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[5] - `0_Outs`$`X3.0`[5])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[6] - `0_Outs`$`X3.1`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[5] - `0_Outs`$`X0.0`[6])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[5] - `0_Outs`$`X0.2`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.2`[5] - `0_Outs`$`X1.1`[5])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[5] - `0_Outs`$`X1.2`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[5] - `0_Outs`$`X2.1`[5])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[5] - `0_Outs`$`X2.2`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[5] - `0_Outs`$`X2.1`[5])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[6] - `0_Outs`$`X3.2`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[5] - `0_Outs`$`X0.0`[6])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[5] - `1_Out`$`X0.0`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[5] - `0_Outs`$`X1.2`[5])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[5] - `1_Out`$`X0.0`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[5] - `0_Outs`$`X2.2`[5])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[5] - `1_Out`$`X0.0`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[5] - `0_Outs`$`X3.2`[5])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[6] - `1_Out`$`X0.0`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[5] - `0_Outs`$`X0.0`[6])
        } else {
          invisible()
        }
      }
    }
    else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == T & 
             !is.na(pitches_plus_missedcalls$on_2b[row]) == F &
             !is.na(pitches_plus_missedcalls$on_3b[row]) == T) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.0`[6] - `0_Outs`$`X0.1`[6]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.1`[6] - `0_Outs`$`X1.0`[6]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.0`[6] - `0_Outs`$`X1.1`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[6] - `0_Outs`$`X2.0`[6])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.0`[6] - `0_Outs`$`X2.1`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[6] - `0_Outs`$`X3.0`[6])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[8] - `0_Outs`$`X3.1`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[6] - `0_Outs`$`X0.0`[8])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[6] - `0_Outs`$`X0.2`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.2`[6] - `0_Outs`$`X1.1`[6])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[6] - `0_Outs`$`X1.2`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[6] - `0_Outs`$`X2.1`[6])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[6] - `0_Outs`$`X2.2`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[6] - `0_Outs`$`X2.1`[6])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[8] - `0_Outs`$`X3.2`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[6] - `0_Outs`$`X0.0`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[6] - `1_Out`$`X0.0`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[6] - `0_Outs`$`X1.2`[6])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[6] - `1_Out`$`X0.0`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[6] - `0_Outs`$`X2.2`[6])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[6] - `1_Out`$`X0.0`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[6] - `0_Outs`$`X3.2`[6])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[8] - `1_Out`$`X0.0`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[6] - `0_Outs`$`X0.0`[8])
        } else {
          invisible()
        }
      }
    }
    else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == F & 
             !is.na(pitches_plus_missedcalls$on_2b[row]) == T &
             !is.na(pitches_plus_missedcalls$on_3b[row]) == T) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.0`[7] - `0_Outs`$`X0.1`[7]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.1`[7] - `0_Outs`$`X1.0`[7]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.0`[7] - `0_Outs`$`X1.1`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[7] - `0_Outs`$`X2.0`[7])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.0`[7] - `0_Outs`$`X2.1`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[7] - `0_Outs`$`X3.0`[7])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[8] - `0_Outs`$`X3.1`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[7] - `0_Outs`$`X0.0`[8])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[7] - `0_Outs`$`X0.2`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.2`[7] - `0_Outs`$`X1.1`[7])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[7] - `0_Outs`$`X1.2`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[7] - `0_Outs`$`X2.1`[7])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[7] - `0_Outs`$`X2.2`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[7] - `0_Outs`$`X2.1`[7])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[8] - `0_Outs`$`X3.2`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[7] - `0_Outs`$`X0.0`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[7] - `1_Out`$`X0.0`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[7] - `0_Outs`$`X1.2`[7])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[7] - `1_Out`$`X0.0`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[7] - `0_Outs`$`X2.2`[7])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[7] - `1_Out`$`X0.0`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[7] - `0_Outs`$`X3.2`[7])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[8] - `1_Out`$`X0.0`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[7] - `0_Outs`$`X0.0`[8])
        } else {
          invisible()
        }
      }
    }
    else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == T & 
             !is.na(pitches_plus_missedcalls$on_2b[row]) == T &
             !is.na(pitches_plus_missedcalls$on_3b[row]) == T) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.0`[8] - `0_Outs`$`X0.1`[8]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.1`[8] - `0_Outs`$`X1.0`[8]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.0`[8] - `0_Outs`$`X1.1`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[8] - `0_Outs`$`X2.0`[8])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.0`[8] - `0_Outs`$`X2.1`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[8] - `0_Outs`$`X3.0`[8])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[8] - `0_Outs`$`X3.1`[8]) +1
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[8] - `0_Outs`$`X0.0`[8]) -1
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.1`[8] - `0_Outs`$`X0.2`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.2`[8] - `0_Outs`$`X1.1`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.1`[8] - `0_Outs`$`X1.2`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[8] - `0_Outs`$`X2.1`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.1`[8] - `0_Outs`$`X2.2`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[8] - `0_Outs`$`X2.1`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[8] - `0_Outs`$`X3.2`[8]) +1
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[8] - `0_Outs`$`X0.0`[8]) -1
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X1.2`[8] - `1_Out`$`X0.0`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `0_Outs`$`X1.2`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X2.2`[8] - `1_Out`$`X0.0`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `0_Outs`$`X2.2`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X3.2`[8] - `1_Out`$`X0.0`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `0_Outs`$`X3.2`[8])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`0_Outs`$`X0.0`[8] - `1_Out`$`X0.0`[8]) +1
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `0_Outs`$`X0.0`[8]) -1
        } else {
          invisible()
        }
      }
    }
    
    
    
    
  } 
  else if (pitches_plus_missedcalls$outs_when_up[row] == 1) 
  {
    #OUTS CHUNK
    if (!is.na(pitches_plus_missedcalls$on_1b[row]) == F & 
        !is.na(pitches_plus_missedcalls$on_2b[row]) == F &
        !is.na(pitches_plus_missedcalls$on_3b[row]) == F) 
    { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.0`[1] - `1_Out`$`X0.1`[1]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.1`[1] - `1_Out`$`X1.0`[1]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.0`[1] - `1_Out`$`X1.1`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[1] - `1_Out`$`X2.0`[1])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.0`[1] - `1_Out`$`X2.1`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[1] - `1_Out`$`X3.0`[1])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[2] - `1_Out`$`X3.1`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[1] - `1_Out`$`X0.0`[2])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[1] - `1_Out`$`X0.2`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.2`[1] - `1_Out`$`X1.1`[1])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[1] - `1_Out`$`X1.2`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[1] - `1_Out`$`X2.1`[1])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[1] - `1_Out`$`X2.2`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[1] - `1_Out`$`X2.1`[1])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[2] - `1_Out`$`X3.2`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[1] - `1_Out`$`X0.0`[2])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[1] - `2_Outs`$`X0.0`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[1] - `1_Out`$`X1.2`[1])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[1] - `2_Outs`$`X0.0`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[1] - `1_Out`$`X2.2`[1])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[1] - `2_Outs`$`X0.0`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[1] - `1_Out`$`X3.2`[1])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[2] - `2_Outs`$`X0.0`[1])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[1] - `1_Out`$`X0.0`[2])
        } else {
          invisible()
        }
      }
    } else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == T & 
               !is.na(pitches_plus_missedcalls$on_2b[row]) == F &
               !is.na(pitches_plus_missedcalls$on_3b[row]) == F) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.0`[2] - `1_Out`$`X0.1`[2]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.1`[2] - `1_Out`$`X1.0`[2]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.0`[2] - `1_Out`$`X1.1`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[2] - `1_Out`$`X2.0`[2])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.0`[2] - `1_Out`$`X2.1`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[2] - `1_Out`$`X3.0`[2])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[4] - `1_Out`$`X3.1`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[2] - `1_Out`$`X0.0`[4])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[2] - `1_Out`$`X0.2`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.2`[2] - `1_Out`$`X1.1`[2])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[2] - `1_Out`$`X1.2`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[2] - `1_Out`$`X2.1`[2])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[2] - `1_Out`$`X2.2`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[2] - `1_Out`$`X2.1`[2])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[4] - `1_Out`$`X3.2`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[2] - `1_Out`$`X0.0`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[2] - `2_Outs`$`X0.0`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[2] - `1_Out`$`X1.2`[2])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[2] - `2_Outs`$`X0.0`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[2] - `1_Out`$`X2.2`[2])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[2] - `2_Outs`$`X0.0`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[2] - `1_Out`$`X3.2`[2])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[4] - `2_Outs`$`X0.0`[2])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[2] - `1_Out`$`X0.0`[4])
        } else {
          invisible()
        }
      }
    }
    else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == F & 
             !is.na(pitches_plus_missedcalls$on_2b[row]) == T &
             !is.na(pitches_plus_missedcalls$on_3b[row]) == F) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.0`[3] - `1_Out`$`X0.1`[3]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.1`[3] - `1_Out`$`X1.0`[3]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.0`[3] - `1_Out`$`X1.1`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[3] - `1_Out`$`X2.0`[3])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.0`[3] - `1_Out`$`X2.1`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[3] - `1_Out`$`X3.0`[3])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[4] - `1_Out`$`X3.1`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[3] - `1_Out`$`X0.0`[4])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[3] - `1_Out`$`X0.2`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.2`[3] - `1_Out`$`X1.1`[3])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[3] - `1_Out`$`X1.2`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[3] - `1_Out`$`X2.1`[3])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[3] - `1_Out`$`X2.2`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[3] - `1_Out`$`X2.1`[3])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[4] - `1_Out`$`X3.2`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[3] - `1_Out`$`X0.0`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[3] - `2_Outs`$`X0.0`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[3] - `1_Out`$`X1.2`[3])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[3] - `2_Outs`$`X0.0`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[3] - `1_Out`$`X2.2`[3])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[3] - `2_Outs`$`X0.0`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[3] - `1_Out`$`X3.2`[3])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[4] - `2_Outs`$`X0.0`[3])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[3] - `1_Out`$`X0.0`[4])
        } else {
          invisible()
        }
      }
    } else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == T & 
               !is.na(pitches_plus_missedcalls$on_2b[row]) == T &
               !is.na(pitches_plus_missedcalls$on_3b[row]) == F) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.0`[4] - `1_Out`$`X0.1`[4]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.1`[4] - `1_Out`$`X1.0`[4]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.0`[4] - `1_Out`$`X1.1`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[4] - `1_Out`$`X2.0`[4])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.0`[4] - `1_Out`$`X2.1`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[4] - `1_Out`$`X3.0`[4])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `1_Out`$`X3.1`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[4] - `1_Out`$`X0.0`[8])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[4] - `1_Out`$`X0.2`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.2`[4] - `1_Out`$`X1.1`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[4] - `1_Out`$`X1.2`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[4] - `1_Out`$`X2.1`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[4] - `1_Out`$`X2.2`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[4] - `1_Out`$`X2.1`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `1_Out`$`X3.2`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[4] - `1_Out`$`X0.0`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[4] - `2_Outs`$`X0.0`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[4] - `1_Out`$`X1.2`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[4] - `2_Outs`$`X0.0`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[4] - `1_Out`$`X2.2`[4])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[4] - `2_Outs`$`X0.0`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[4] - `1_Out`$`X3.2`[4])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `2_Outs`$`X0.0`[4])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[4] - `1_Out`$`X0.0`[8])
        } else {
          invisible()
        }
      }
    }
    else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == F & 
             !is.na(pitches_plus_missedcalls$on_2b[row]) == F &
             !is.na(pitches_plus_missedcalls$on_3b[row]) == T) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.0`[5] - `1_Out`$`X0.1`[5]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.1`[5] - `1_Out`$`X1.0`[5]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.0`[5] - `1_Out`$`X1.1`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[5] - `1_Out`$`X2.0`[5])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.0`[5] - `1_Out`$`X2.1`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[5] - `1_Out`$`X3.0`[5])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[6] - `1_Out`$`X3.1`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[5] - `1_Out`$`X0.0`[6])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[5] - `1_Out`$`X0.2`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.2`[5] - `1_Out`$`X1.1`[5])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[5] - `1_Out`$`X1.2`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[5] - `1_Out`$`X2.1`[5])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[5] - `1_Out`$`X2.2`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[5] - `1_Out`$`X2.1`[5])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[6] - `1_Out`$`X3.2`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[5] - `1_Out`$`X0.0`[6])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[5] - `2_Outs`$`X0.0`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[5] - `1_Out`$`X1.2`[5])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[5] - `2_Outs`$`X0.0`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[5] - `1_Out`$`X2.2`[5])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[5] - `2_Outs`$`X0.0`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[5] - `1_Out`$`X3.2`[5])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[6] - `2_Outs`$`X0.0`[5])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[5] - `1_Out`$`X0.0`[6])
        } else {
          invisible()
        }
      }
    }
    else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == T & 
             !is.na(pitches_plus_missedcalls$on_2b[row]) == F &
             !is.na(pitches_plus_missedcalls$on_3b[row]) == T) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.0`[6] - `1_Out`$`X0.1`[6]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.1`[6] - `1_Out`$`X1.0`[6]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.0`[6] - `1_Out`$`X1.1`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[6] - `1_Out`$`X2.0`[6])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.0`[6] - `1_Out`$`X2.1`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[6] - `1_Out`$`X3.0`[6])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `1_Out`$`X3.1`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[6] - `1_Out`$`X0.0`[8])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[6] - `1_Out`$`X0.2`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.2`[6] - `1_Out`$`X1.1`[6])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[6] - `1_Out`$`X1.2`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[6] - `1_Out`$`X2.1`[6])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[6] - `1_Out`$`X2.2`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[6] - `1_Out`$`X2.1`[6])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `1_Out`$`X3.2`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[6] - `1_Out`$`X0.0`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[6] - `2_Outs`$`X0.0`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[6] - `1_Out`$`X1.2`[6])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[6] - `2_Outs`$`X0.0`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[6] - `1_Out`$`X2.2`[6])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[6] - `2_Outs`$`X0.0`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[6] - `1_Out`$`X3.2`[6])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `2_Outs`$`X0.0`[6])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[6] - `1_Out`$`X0.0`[8])
        } else {
          invisible()
        }
      }
    }
    else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == F & 
             !is.na(pitches_plus_missedcalls$on_2b[row]) == T &
             !is.na(pitches_plus_missedcalls$on_3b[row]) == T) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.0`[7] - `1_Out`$`X0.1`[7]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.1`[7] - `1_Out`$`X1.0`[7]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.0`[7] - `1_Out`$`X1.1`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[7] - `1_Out`$`X2.0`[7])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.0`[7] - `1_Out`$`X2.1`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[7] - `1_Out`$`X3.0`[7])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `1_Out`$`X3.1`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[7] - `1_Out`$`X0.0`[8])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[7] - `1_Out`$`X0.2`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.2`[7] - `1_Out`$`X1.1`[7])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[7] - `1_Out`$`X1.2`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[7] - `1_Out`$`X2.1`[7])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[7] - `1_Out`$`X2.2`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[7] - `1_Out`$`X2.1`[7])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `1_Out`$`X3.2`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[7] - `1_Out`$`X0.0`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[7] - `2_Outs`$`X0.0`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[7] - `1_Out`$`X1.2`[7])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[7] - `2_Outs`$`X0.0`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[7] - `1_Out`$`X2.2`[7])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[7] - `2_Outs`$`X0.0`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[7] - `1_Out`$`X3.2`[7])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `2_Outs`$`X0.0`[7])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[7] - `1_Out`$`X0.0`[8])
        } else {
          invisible()
        }
      }
    }
    else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == T & 
             !is.na(pitches_plus_missedcalls$on_2b[row]) == T &
             !is.na(pitches_plus_missedcalls$on_3b[row]) == T) { #RUNNERS ON CHUNK
      if (pitches_plus_missedcalls$count[row] == "0.0") 
      { #COUNT CHUNK
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.0`[8] - `1_Out`$`X0.1`[8]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.1`[8] - `1_Out`$`X1.0`[8]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
        } else {
          invisible()
        }
      } else if (pitches_plus_missedcalls$count[row] == "1.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.0`[8] - `1_Out`$`X1.1`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[8] - `1_Out`$`X2.0`[8])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.0`[8] - `1_Out`$`X2.1`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[8] - `1_Out`$`X3.0`[8])
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `1_Out`$`X3.1`[8]) +1
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[8] - `1_Out`$`X0.0`[8]) -1
        } else {
          invisible()
        }
      }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.1`[8] - `1_Out`$`X0.2`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.2`[8] - `1_Out`$`X1.1`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.1`[8] - `1_Out`$`X1.2`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[8] - `1_Out`$`X2.1`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.1`[8] - `1_Out`$`X2.2`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[8] - `1_Out`$`X2.1`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `1_Out`$`X3.2`[8]) +1
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[8] - `1_Out`$`X0.0`[8]) -1
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X1.2`[8] - `2_Outs`$`X0.0`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `1_Out`$`X1.2`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X2.2`[8] - `2_Outs`$`X0.0`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `1_Out`$`X2.2`[8])
        } else {
          invisible()
        }
      }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X3.2`[8] - `2_Outs`$`X0.0`[8])
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `1_Out`$`X3.2`[8])
        } else {
          invisible()
        }
      }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
        #copy the count chunk with appropriate values and do another else if for each count
        if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`1_Out`$`X0.0`[8] - `2_Outs`$`X0.0`[8]) +1
        } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
          pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `1_Out`$`X0.0`[8]) -1
        } else {
          invisible()
        }
      }
    }
  }
  else if (pitches_plus_missedcalls$outs_when_up[row] == 2) 
  {
    { #OUTS CHUNK
      if (!is.na(pitches_plus_missedcalls$on_1b[row]) == F & 
          !is.na(pitches_plus_missedcalls$on_2b[row]) == F &
          !is.na(pitches_plus_missedcalls$on_3b[row]) == F) 
      { #RUNNERS ON CHUNK
        if (pitches_plus_missedcalls$count[row] == "0.0") 
        { #COUNT CHUNK
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.0`[1] - `2_Outs`$`X0.1`[1]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.1`[1] - `2_Outs`$`X1.0`[1]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
          } else {
            invisible()
          }
        } else if (pitches_plus_missedcalls$count[row] == "1.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.0`[1] - `2_Outs`$`X1.1`[1])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[1] - `2_Outs`$`X2.0`[1])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.0`[1] - `2_Outs`$`X2.1`[1])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[1] - `2_Outs`$`X3.0`[1])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[2] - `2_Outs`$`X3.1`[1])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[1] - `2_Outs`$`X0.0`[2])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[1] - `2_Outs`$`X0.2`[1])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.2`[1] - `2_Outs`$`X1.1`[1])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[1] - `2_Outs`$`X1.2`[1])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[1] - `2_Outs`$`X2.1`[1])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[1] - `2_Outs`$`X2.2`[1])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[1] - `2_Outs`$`X2.1`[1])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[2] - `2_Outs`$`X3.2`[1])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[1] - `2_Outs`$`X0.0`[2])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[1] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X1.2`[1])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[1] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X2.2`[1])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[1] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X3.2`[1])
          } else {
            invisible()
          }
        }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[2] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X0.0`[2])
          } else {
            invisible()
          }
        }
      } else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == T & 
                 !is.na(pitches_plus_missedcalls$on_2b[row]) == F &
                 !is.na(pitches_plus_missedcalls$on_3b[row]) == F) { #RUNNERS ON CHUNK
        if (pitches_plus_missedcalls$count[row] == "0.0") 
        { #COUNT CHUNK
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.0`[2] - `2_Outs`$`X0.1`[2]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.1`[2] - `2_Outs`$`X1.0`[2]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
          } else {
            invisible()
          }
        } else if (pitches_plus_missedcalls$count[row] == "1.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.0`[2] - `2_Outs`$`X1.1`[2])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[2] - `2_Outs`$`X2.0`[2])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.0`[2] - `2_Outs`$`X2.1`[2])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[2] - `2_Outs`$`X3.0`[2])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[4] - `2_Outs`$`X3.1`[2])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[2] - `2_Outs`$`X0.0`[4])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[2] - `2_Outs`$`X0.2`[2])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.2`[2] - `2_Outs`$`X1.1`[2])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[2] - `2_Outs`$`X1.2`[2])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[2] - `2_Outs`$`X2.1`[2])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[2] - `2_Outs`$`X2.2`[2])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[2] - `2_Outs`$`X2.1`[2])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[4] - `2_Outs`$`X3.2`[2])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[2] - `2_Outs`$`X0.0`[4])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[2] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X1.2`[2])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[2] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X2.2`[2])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[2] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X3.2`[2])
          } else {
            invisible()
          }
        }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[4] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X0.0`[4])
          } else {
            invisible()
          }
        }
      }
      else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == F & 
               !is.na(pitches_plus_missedcalls$on_2b[row]) == T &
               !is.na(pitches_plus_missedcalls$on_3b[row]) == F) { #RUNNERS ON CHUNK
        if (pitches_plus_missedcalls$count[row] == "0.0") 
        { #COUNT CHUNK
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.0`[3] - `2_Outs`$`X0.1`[3]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.1`[3] - `2_Outs`$`X1.0`[3]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
          } else {
            invisible()
          }
        } else if (pitches_plus_missedcalls$count[row] == "1.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.0`[3] - `2_Outs`$`X1.1`[3])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[3] - `2_Outs`$`X2.0`[3])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.0`[3] - `2_Outs`$`X2.1`[3])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[3] - `2_Outs`$`X3.0`[3])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[4] - `2_Outs`$`X3.1`[3])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[3] - `2_Outs`$`X0.0`[4])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[3] - `2_Outs`$`X0.2`[3])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.2`[3] - `2_Outs`$`X1.1`[3])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[3] - `2_Outs`$`X1.2`[3])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[3] - `2_Outs`$`X2.1`[3])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[3] - `2_Outs`$`X2.2`[3])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[3] - `2_Outs`$`X2.1`[3])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[4] - `2_Outs`$`X3.2`[3])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[3] - `2_Outs`$`X0.0`[4])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[3] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X1.2`[3])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[3] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X2.2`[3])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[3] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X3.2`[3])
          } else {
            invisible()
          }
        }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[4] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X0.0`[4])
          } else {
            invisible()
          }
        }
      } else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == T & 
                 !is.na(pitches_plus_missedcalls$on_2b[row]) == T &
                 !is.na(pitches_plus_missedcalls$on_3b[row]) == F) { #RUNNERS ON CHUNK
        if (pitches_plus_missedcalls$count[row] == "0.0") 
        { #COUNT CHUNK
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.0`[4] - `2_Outs`$`X0.1`[4]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.1`[4] - `2_Outs`$`X1.0`[4]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
          } else {
            invisible()
          }
        } else if (pitches_plus_missedcalls$count[row] == "1.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.0`[4] - `2_Outs`$`X1.1`[4])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[4] - `2_Outs`$`X2.0`[4])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.0`[4] - `2_Outs`$`X2.1`[4])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[4] - `2_Outs`$`X3.0`[4])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `2_Outs`$`X3.1`[4])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[4] - `2_Outs`$`X0.0`[8])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[4] - `2_Outs`$`X0.2`[4])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.2`[4] - `2_Outs`$`X1.1`[4])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[4] - `2_Outs`$`X1.2`[4])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[4] - `2_Outs`$`X2.1`[4])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[4] - `2_Outs`$`X2.2`[4])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[4] - `2_Outs`$`X2.1`[4])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `2_Outs`$`X3.2`[4])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[4] - `2_Outs`$`X0.0`[8])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[4] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X1.2`[4])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[4] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X2.2`[4])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[4] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X3.2`[4])
          } else {
            invisible()
          }
        }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X0.0`[8])
          } else {
            invisible()
          }
        }
      }
      else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == F & 
               !is.na(pitches_plus_missedcalls$on_2b[row]) == F &
               !is.na(pitches_plus_missedcalls$on_3b[row]) == T) { #RUNNERS ON CHUNK
        if (pitches_plus_missedcalls$count[row] == "0.0") 
        { #COUNT CHUNK
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.0`[5] - `2_Outs`$`X0.1`[5]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.1`[5] - `2_Outs`$`X1.0`[5]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
          } else {
            invisible()
          }
        } else if (pitches_plus_missedcalls$count[row] == "1.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.0`[5] - `2_Outs`$`X1.1`[5])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[5] - `2_Outs`$`X2.0`[5])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.0`[5] - `2_Outs`$`X2.1`[5])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[5] - `2_Outs`$`X3.0`[5])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[6] - `2_Outs`$`X3.1`[5])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[5] - `2_Outs`$`X0.0`[6])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[5] - `2_Outs`$`X0.2`[5])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.2`[5] - `2_Outs`$`X1.1`[5])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[5] - `2_Outs`$`X1.2`[5])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[5] - `2_Outs`$`X2.1`[5])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[5] - `2_Outs`$`X2.2`[5])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[5] - `2_Outs`$`X2.1`[5])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[6] - `2_Outs`$`X3.2`[5])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[5] - `2_Outs`$`X0.0`[6])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[5] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X1.2`[5])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[5] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X2.2`[5])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[5] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X3.2`[5])
          } else {
            invisible()
          }
        }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[6] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X0.0`[6])
          } else {
            invisible()
          }
        }
      }
      else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == T & 
               !is.na(pitches_plus_missedcalls$on_2b[row]) == F &
               !is.na(pitches_plus_missedcalls$on_3b[row]) == T) { #RUNNERS ON CHUNK
        if (pitches_plus_missedcalls$count[row] == "0.0") 
        { #COUNT CHUNK
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.0`[6] - `2_Outs`$`X0.1`[6]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.1`[6] - `2_Outs`$`X1.0`[6]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
          } else {
            invisible()
          }
        } else if (pitches_plus_missedcalls$count[row] == "1.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.0`[6] - `2_Outs`$`X1.1`[6])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[6] - `2_Outs`$`X2.0`[6])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.0`[6] - `2_Outs`$`X2.1`[6])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[6] - `2_Outs`$`X3.0`[6])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `2_Outs`$`X3.1`[6])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[6] - `2_Outs`$`X0.0`[8])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[6] - `2_Outs`$`X0.2`[6])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.2`[6] - `2_Outs`$`X1.1`[6])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[6] - `2_Outs`$`X1.2`[6])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[6] - `2_Outs`$`X2.1`[6])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[6] - `2_Outs`$`X2.2`[6])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[6] - `2_Outs`$`X2.1`[6])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `2_Outs`$`X3.2`[6])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[6] - `2_Outs`$`X0.0`[8])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[6] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X1.2`[6])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[6] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X2.2`[6])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[6] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X3.2`[6])
          } else {
            invisible()
          }
        }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X0.0`[8])
          } else {
            invisible()
          }
        }
      }
      else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == F & 
               !is.na(pitches_plus_missedcalls$on_2b[row]) == T &
               !is.na(pitches_plus_missedcalls$on_3b[row]) == T) { #RUNNERS ON CHUNK
        if (pitches_plus_missedcalls$count[row] == "0.0") 
        { #COUNT CHUNK
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.0`[7] - `2_Outs`$`X0.1`[7]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.1`[7] - `2_Outs`$`X1.0`[7]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
          } else {
            invisible()
          }
        } else if (pitches_plus_missedcalls$count[row] == "1.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.0`[7] - `2_Outs`$`X1.1`[7])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[7] - `2_Outs`$`X2.0`[7])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.0`[7] - `2_Outs`$`X2.1`[7])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[7] - `2_Outs`$`X3.0`[7])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `2_Outs`$`X3.1`[7])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[7] - `2_Outs`$`X0.0`[8])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[7] - `2_Outs`$`X0.2`[7])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.2`[7] - `2_Outs`$`X1.1`[7])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[7] - `2_Outs`$`X1.2`[7])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[7] - `2_Outs`$`X2.1`[7])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[7] - `2_Outs`$`X2.2`[7])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[7] - `2_Outs`$`X2.1`[7])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `2_Outs`$`X3.2`[7])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[7] - `2_Outs`$`X0.0`[8])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[7] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X1.2`[7])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[7] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X2.2`[7])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[7] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X3.2`[7])
          } else {
            invisible()
          }
        }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X0.0`[8])
          } else {
            invisible()
          }
        }
      }
      else if (!is.na(pitches_plus_missedcalls$on_1b[row]) == T & 
               !is.na(pitches_plus_missedcalls$on_2b[row]) == T &
               !is.na(pitches_plus_missedcalls$on_3b[row]) == T) { #RUNNERS ON CHUNK
        if (pitches_plus_missedcalls$count[row] == "0.0") 
        { #COUNT CHUNK
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.0`[8] - `2_Outs`$`X0.1`[8]) #0 out df filtered to no one on: 1 0 count value - 0 1 count
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.1`[8] - `2_Outs`$`X1.0`[8]) #0 out df filtered to no one on: 0 1 count value - 1 0 count
          } else {
            invisible()
          }
        } else if (pitches_plus_missedcalls$count[row] == "1.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.0`[8] - `2_Outs`$`X1.1`[8])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[8] - `2_Outs`$`X2.0`[8])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "2.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.0`[8] - `2_Outs`$`X2.1`[8])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[8] - `2_Outs`$`X3.0`[8])
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "3.0") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `2_Outs`$`X3.1`[8]) +1
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[8] - `2_Outs`$`X0.0`[8]) -1
          } else {
            invisible()
          }
        }  else if (pitches_plus_missedcalls$count[row] == "0.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.1`[8] - `2_Outs`$`X0.2`[8])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.2`[8] - `2_Outs`$`X1.1`[8])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.1`[8] - `2_Outs`$`X1.2`[8])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[8] - `2_Outs`$`X2.1`[8])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.1`[8] - `2_Outs`$`X2.2`[8])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[8] - `2_Outs`$`X2.1`[8])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "3.1") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `2_Outs`$`X3.2`[8]) +1
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[8] - `2_Outs`$`X0.0`[8]) -1
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "0.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[8] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X1.2`[8])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "1.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X2.2`[8] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X2.2`[8])
          } else {
            invisible()
          }
        }   else if (pitches_plus_missedcalls$count[row] == "2.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X3.2`[8] - `2_Outs`$`X1.2`[9])
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X3.2`[8])
          } else {
            invisible()
          }
        }    else if (pitches_plus_missedcalls$count[row] == "3.2") {
          #copy the count chunk with appropriate values and do another else if for each count
          if (pitches_plus_missedcalls$Incorrect_Strike[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X0.0`[8] - `2_Outs`$`X1.2`[9]) +1
          } else if (pitches_plus_missedcalls$Incorrect_Ball[row] == 1) {
            pitches_plus_missedcalls$difference[row] = (`2_Outs`$`X1.2`[9] - `2_Outs`$`X0.0`[8]) -1
          } else {
            invisible()
          }
        }
      }
    }
  }
}

pitches_plus_missedcalls <- distinct(pitches_plus_missedcalls)
write.csv(pitches_plus_missedcalls, "pitches_plus_missedcalls.csv")

pitches_plus_missedcalls <- read.csv("pitches_plus_missedcalls.csv") %>%
  select(-1)

##Profiling and Gathering Impact information for Umpires
Umpire_Impact <- pitches_plus_missedcalls %>%
  group_by(Umpire) %>%
  summarise(Total_Impact = sum(abs(difference), na.rm = TRUE))
Umpire_Bias <- pitches_plus_missedcalls %>%
  group_by(Umpire) %>%
  summarise(Total_Favor = sum(difference, na.rm = TRUE))
Umpire_Impact <- Umpire_Impact %>%
  left_join(Umpire_Bias, by=c("Umpire" = "Umpire"))

Umpire_Count <- pitches_plus_missedcalls %>%
  select(2, 13, 14, 32, 43) %>%
  distinct() %>%
  group_by(Umpire) %>%
  count(name = "Games")

Umpire_Impact <- Umpire_Impact %>%
  left_join(Umpire_Count, by=c("Umpire" = "Umpire")) %>%
  mutate(Impact_Per_Game = (Total_Impact / Games)) %>%
  mutate(Favor_Per_Game = (Total_Favor / Games))

Umpires <- pitches_plus_missedcalls %>%
  select(43) %>%
  distinct()

Umpires_2023 <- read_xlsx("MLB Umpires (2015-2023).xlsx", 1) %>% select(-1) %>% mutate(year = 2023) %>% select(1:3, 12:15)
Umpires_2022 <- read_xlsx("MLB Umpires (2015-2023).xlsx", 2) %>% mutate(year = 2022) %>% select(1:3, 12:15)
Umpires_2021 <- read_xlsx("MLB Umpires (2015-2023).xlsx", 3) %>% mutate(year = 2021) %>% select(1:3, 12:15)
Umpires_2020 <- read_xlsx("MLB Umpires (2015-2023).xlsx", 4) %>% mutate(year = 2020) %>% select(1:3, 12:15)
Umpires_2019 <- read_xlsx("MLB Umpires (2015-2023).xlsx", 5) %>% mutate(year = 2019) %>% select(1:3, 12:15)
Umpires_2018 <- read_xlsx("MLB Umpires (2015-2023).xlsx", 6) %>% mutate(year = 2018) %>% select(1:3, 12:15)
Umpires_2017 <- read_xlsx("MLB Umpires (2015-2023).xlsx", 7) %>% mutate(year = 2017) %>% select(1:3, 12:15)
Umpires_2016 <- read_xlsx("MLB Umpires (2015-2023).xlsx", 8) %>% mutate(year = 2016) %>% select(1:3, 12:15)
Umpires_2015 <- read_xlsx("MLB Umpires (2015-2023).xlsx", 9) %>% mutate(year = 2015) %>% select(1:3, 12:15)
Umpires_2 <- rbind(Umpires_2015, Umpires_2016, Umpires_2017, Umpires_2018, Umpires_2019, Umpires_2020, Umpires_2021, Umpires_2022, Umpires_2023)

Umpire_Join <- Umpires_2 %>%
  group_by(`umpire name`) %>%
  slice_max(order_by = year, n = 1)

Umpires <- Umpires %>%
  left_join(Umpire_Join, by=c("Umpire" = "umpire name"))
Umpire_Impact <- Umpire_Impact %>%
  left_join(Umpires, by=c("Umpire" = "Umpire")) %>% select(-9,-10)


##Creating umpire missed call statistics
pitches_plus_missedcalls <- read_csv("pitches_plus_missedcalls.csv") %>% select(-1)
pitches_plus_missedcalls_zones <- pitches_plus_missedcalls %>%
  group_by(Umpire) %>%
  mutate(Incorrect_Call_Zones = 
           ifelse(plate_z >= (sz_bot + 2*(1.57/12)) & plate_z <= (sz_top - 2*(1.57/12)) & plate_x >= (-(((1.57*2 + 17) / 12) / 2) + 2*(1.57/12)) & plate_x <= ((((1.57*2 + 17) / 12) / 2) - 2*(1.57/12)), 1,
           ifelse(plate_z >= (sz_bot - 2*(1.57/12)) & plate_z < (sz_bot + 2*(1.57/12)) & plate_x > (-(((1.57*2 + 17) / 12) / 2) + 2*(1.57/12)) & plate_x < ((((1.57*2 + 17) / 12) / 2) - 2*(1.57/12)), 2,
           ifelse(plate_z > (sz_bot - 2*(1.57/12)) & plate_z < (sz_bot + 2*(1.57/12)) & plate_x >= (-(((1.57*2 + 17) / 12) / 2) - 2*(1.57/12)) & plate_x < (-(((1.57*2 + 17) / 12) / 2) + 2*(1.57/12)), 3,
           ifelse(plate_z > (sz_bot + 2*(1.57/12)) & plate_z < (sz_top - 2*(1.57/12)) & plate_x >= (-(((1.57*2 + 17) / 12) / 2) - 2*(1.57/12)) & plate_x < (-(((1.57*2 + 17) / 12) / 2) + 2*(1.57/12)), 4,
           ifelse(plate_z > (sz_top - 2*(1.57/12)) & plate_z < (sz_top + 2*(1.57/12)) & plate_x >= (-(((1.57*2 + 17) / 12) / 2) - 2*(1.57/12)) & plate_x < (-(((1.57*2 + 17) / 12) / 2) + 2*(1.57/12)), 5,
           ifelse(plate_z > (sz_top - 2*(1.57/12)) & plate_z <= (sz_top + 2*(1.57/12)) & plate_x > (-(((1.57*2 + 17) / 12) / 2) + 2*(1.57/12)) & plate_x < ((((1.57*2 + 17) / 12) / 2) - 2*(1.57/12)), 6,
           ifelse(plate_z > (sz_bot - 2*(1.57/12)) & plate_z < (sz_bot + 2*(1.57/12)) & plate_x > ((((1.57*2 + 17) / 12) / 2) - 2*(1.57/12)) & plate_x <= ((((1.57*2 + 17) / 12) / 2) + 2*(1.57/12)), 9,
           ifelse(plate_z > (sz_bot + 2*(1.57/12)) & plate_z < (sz_top - 2*(1.57/12)) & plate_x > ((((1.57*2 + 17) / 12) / 2) - 2*(1.57/12)) & plate_x <= ((((1.57*2 + 17) / 12) / 2) + 2*(1.57/12)), 8,
           ifelse(plate_z > (sz_top - 2*(1.57/12)) & plate_z < (sz_top + 2*(1.57/12)) & plate_x > ((((1.57*2 + 17) / 12) / 2) - 2*(1.57/12)) & plate_x <= ((((1.57*2 + 17) / 12) / 2) + 2*(1.57/12)), 7,
           ifelse(plate_z < (sz_bot - 2*(1.57/12)) & plate_x > (-(((1.57*2 + 17) / 12) / 2) - 2*(1.57/12)) & plate_x < ((((1.57*2 + 17) / 12) / 2) + 2*(1.57/12)), 10,
           ifelse(plate_z < (sz_top + 2*(1.57/12)) & plate_z > (sz_bot - 2*(1.57/12)) & plate_x < (-(((1.57*2 + 17) / 12) / 2) - 2*(1.57/12)), 11,
           ifelse(plate_z > (sz_top + 2*(1.57/12)) & plate_x > (-(((1.57*2 + 17) / 12) / 2) - 2*(1.57/12)) & plate_x < ((((1.57*2 + 17) / 12) / 2) + 2*(1.57/12)), 12,
           ifelse(plate_z < (sz_top + 2*(1.57/12)) & plate_z > (sz_bot - 2*(1.57/12)) & plate_x > ((((1.57*2 + 17) / 12) / 2) + 2*(1.57/12)), 13,
   NA)))))))))))))) %>%
   mutate(Incorrect_Call_Zones_Names = 
         ifelse(Incorrect_Call_Zones == 1, "The Heart", 
         ifelse(Incorrect_Call_Zones == 2, "Low Middle",
         ifelse(Incorrect_Call_Zones == 3, "Low Left",
         ifelse(Incorrect_Call_Zones == 4, "Middle Left",
         ifelse(Incorrect_Call_Zones == 5, "High Left",
         ifelse(Incorrect_Call_Zones == 6, "High Middle",
         ifelse(Incorrect_Call_Zones == 7, "High Right",
         ifelse(Incorrect_Call_Zones == 8, "Middle Right",
         ifelse(Incorrect_Call_Zones == 9, "Low Right",                                   
         ifelse(Incorrect_Call_Zones == 10, "Far Low",
         ifelse(Incorrect_Call_Zones == 11, "Far Left",  
         ifelse(Incorrect_Call_Zones == 12, "Far High",
         ifelse(Incorrect_Call_Zones == 13, "Far Right",
                NA))))))))))))))
Umpire_Stats <- pitches_plus_missedcalls_zones %>%
  group_by(Umpire) %>%
  summarise(
    Total_Incorrect_Calls = sum(Incorrect_Call, na.rm = TRUE),
    Total_Pitches = n(),
    Incorrect_Call_Percentage = Total_Incorrect_Calls / Total_Pitches,
    Umpire_Impact_per_Missed_Call = sum(abs(difference), na.rm = TRUE) / Total_Incorrect_Calls,
    Umpire_Favor_per_Missed_Call = sum(difference, na.rm = TRUE) / Total_Incorrect_Calls,
    Incorrect_Strike_Up_In = sum((Incorrect_Strike == 1 & stand == "R" & Incorrect_Call_Zones == 5) +
                                 (Incorrect_Strike == 1 & stand == "L" & Incorrect_Call_Zones == 7), na.rm = TRUE) /
                             sum((stand == "R" & Incorrect_Call_Zones == 5) +
                                 (stand == "L" & Incorrect_Call_Zones == 7), na.rm = TRUE),
    Incorrect_Ball_Up_In = sum((Incorrect_Strike == 1 & stand == "R" & Incorrect_Call_Zones == 5) +
                                 (Incorrect_Strike == 1 & stand == "L" & Incorrect_Call_Zones == 7), na.rm = TRUE) /
                             sum((stand == "R" & Incorrect_Call_Zones == 5) +
                                 (stand == "L" & Incorrect_Call_Zones == 7), na.rm = TRUE),
    Incorrect_Strike_Up = sum((Incorrect_Strike == 1 & stand == "R" & Incorrect_Call_Zones == 6), na.rm = TRUE) /
                          sum((stand == "R" & Incorrect_Call_Zones == 6), na.rm = TRUE),
    Incorrect_Ball_Up = sum((Incorrect_Ball == 1 & stand == "R" & Incorrect_Call_Zones == 6), na.rm = TRUE) /
                        sum((stand == "R" & Incorrect_Call_Zones == 6), na.rm = TRUE),
    Incorrect_Strike_Up_Away = sum((Incorrect_Strike == 1 & stand == "L" & Incorrect_Call_Zones == 5) +
                                   (Incorrect_Strike == 1 & stand == "R" & Incorrect_Call_Zones == 7), na.rm = TRUE) /
                             sum((stand == "L" & Incorrect_Call_Zones == 5) +
                                 (stand == "R" & Incorrect_Call_Zones == 7), na.rm = TRUE),
    Incorrect_Ball_Up_Away = sum((Incorrect_Ball == 1 & stand == "L" & Incorrect_Call_Zones == 5) +
                                 (Incorrect_Ball == 1 & stand == "R" & Incorrect_Call_Zones == 7), na.rm = TRUE) /
                           sum((stand == "L" & Incorrect_Call_Zones == 5) +
                               (stand == "R" & Incorrect_Call_Zones == 7), na.rm = TRUE),
    Incorrect_Strike_Low_In = sum((Incorrect_Strike == 1 & stand == "R" & Incorrect_Call_Zones == 3) +
                                  (Incorrect_Strike == 1 & stand == "L" & Incorrect_Call_Zones == 9), na.rm = TRUE) /
                             sum((stand == "R" & Incorrect_Call_Zones == 3) +
                                 (stand == "L" & Incorrect_Call_Zones == 9), na.rm = TRUE),
    Incorrect_Ball_Low_In = sum((Incorrect_Ball == 1 & stand == "R" & Incorrect_Call_Zones == 3) +
                                (Incorrect_Ball == 1 & stand == "L" & Incorrect_Call_Zones == 9), na.rm = TRUE) /
                           sum((stand == "R" & Incorrect_Call_Zones == 3) +
                               (stand == "L" & Incorrect_Call_Zones == 9), na.rm = TRUE),
    Incorrect_Strike_Low = sum(Incorrect_Strike == 1 & stand == "R" & Incorrect_Call_Zones == 2, na.rm = TRUE) /
                           sum(stand == "R" & Incorrect_Call_Zones == 2, na.rm = TRUE),
    Incorrect_Ball_Low = sum(Incorrect_Ball == 1 & stand == "R" & Incorrect_Call_Zones == 2, na.rm = TRUE) /
                         sum(stand == "R" & Incorrect_Call_Zones == 2, na.rm = TRUE),
    Incorrect_Strike_Low_Away = sum((Incorrect_Strike == 1 & stand == "L" & Incorrect_Call_Zones == 3) +
                                    (Incorrect_Strike == 1 & stand == "R" & Incorrect_Call_Zones == 9), na.rm = TRUE) /
                             sum((stand == "L" & Incorrect_Call_Zones == 3) +
                                 (stand == "R" & Incorrect_Call_Zones == 9), na.rm = TRUE),
    Incorrect_Ball_Low_Away = sum((Incorrect_Ball == 1 & stand == "L" & Incorrect_Call_Zones == 3) +
                                  (Incorrect_Ball == 1 & stand == "R" & Incorrect_Call_Zones == 9), na.rm = TRUE) /
                           sum((stand == "L" & Incorrect_Call_Zones == 3) +
                               (stand == "R" & Incorrect_Call_Zones == 9), na.rm = TRUE),
    Incorrect_Strike_In = sum((Incorrect_Strike == 1 & stand == "L" & Incorrect_Call_Zones == 8) +
                              (Incorrect_Strike == 1 & stand == "R" & Incorrect_Call_Zones == 4), na.rm = TRUE) /
                             sum((stand == "L" & Incorrect_Call_Zones == 8) +
                                 (stand == "R" & Incorrect_Call_Zones == 4), na.rm = TRUE),
    Incorrect_Ball_In = sum((Incorrect_Ball == 1 & stand == "L" & Incorrect_Call_Zones == 8) +
                            (Incorrect_Ball == 1 & stand == "R" & Incorrect_Call_Zones == 4), na.rm = TRUE) /
                           sum((stand == "L" & Incorrect_Call_Zones == 8) +
                               (stand == "R" & Incorrect_Call_Zones == 4), na.rm = TRUE),
    Incorrect_Strike_Away = sum((Incorrect_Strike == 1 & stand == "L" & Incorrect_Call_Zones == 4) +
                                (Incorrect_Strike == 1 & stand == "R" & Incorrect_Call_Zones == 8), na.rm = TRUE) /
                             sum((stand == "L" & Incorrect_Call_Zones == 4) +
                                 (stand == "R" & Incorrect_Call_Zones == 8), na.rm = TRUE),
    Incorrect_Ball_Away = sum((Incorrect_Ball == 1 & stand == "L" & Incorrect_Call_Zones == 4) +
                              (Incorrect_Ball == 1 & stand == "R" & Incorrect_Call_Zones == 8), na.rm = TRUE) /
                           sum((stand == "L" & Incorrect_Call_Zones == 4) + 
                               (stand == "R" & Incorrect_Call_Zones == 8), na.rm = TRUE),
    Incorrect_Strike_Far = sum((Incorrect_Strike == 1 & Incorrect_Call_Zones == 10) +
                               (Incorrect_Strike == 1 & Incorrect_Call_Zones == 11) +
                               (Incorrect_Strike == 1 & Incorrect_Call_Zones == 12) +
                               (Incorrect_Strike == 1 & Incorrect_Call_Zones == 13), na.rm = TRUE) / 
                           sum((Incorrect_Call_Zones == 10) +
                               (Incorrect_Call_Zones == 11) +
                               (Incorrect_Call_Zones == 12) +
                               (Incorrect_Call_Zones == 13), na.rm = TRUE),
    Incorrect_Ball_Heart = sum(Incorrect_Ball == 1 & Incorrect_Call_Zones == 1, na.rm = TRUE) /
                           sum(Incorrect_Call_Zones == 1, na.rm = TRUE),
    Incorrect_Call_Fastball = sum(Incorrect_Call == 1 & pitch_name == "4-Seam Fastball", na.rm = TRUE) / sum(pitch_name == "4-Seam Fastball", na.rm = TRUE),
    Incorrect_Call_Cutter = sum(Incorrect_Call == 1 & pitch_name == "Cutter", na.rm = TRUE) / sum(pitch_name == "Cutter", na.rm = TRUE),
    Incorrect_Call_Sinker = sum(Incorrect_Call == 1 & pitch_name == "Sinker", na.rm = TRUE) / sum(pitch_name == "Sinker", na.rm = TRUE),
    Incorrect_Call_Changeup = sum(Incorrect_Call == 1 & pitch_name == "Changeup", na.rm = TRUE) / sum(pitch_name == "Changeup", na.rm = TRUE),
    Incorrect_Call_Curveball = sum((Incorrect_Call == 1 & pitch_name == "Curveball") +
                                   (Incorrect_Call == 1 & pitch_name == "Knuckle Curve") +
                                   (Incorrect_Call == 1 & pitch_name == "Slow Curve"), na.rm = TRUE) / 
                               sum((pitch_name == "Curveball") +
                                   (pitch_name == "Knuckle Curve") +
                                   (pitch_name == "Slow Curve"), na.rm = TRUE),
    Incorrect_Call_Slider = sum(Incorrect_Call == 1 & pitch_name == "Slider", na.rm = TRUE) / sum(pitch_name == "Slider", na.rm = TRUE),
    Incorrect_Call_Slurve = ifelse(is.nan(sum(Incorrect_Call == 1 & pitch_name == "Slurve", na.rm = TRUE) / sum(pitch_name == "Slurve", na.rm = TRUE)), NA,
                                          sum(Incorrect_Call == 1 & pitch_name == "Slurve", na.rm = TRUE) / sum(pitch_name == "Slurve", na.rm = TRUE)),
    Incorrect_Call_Sweeper = ifelse(is.nan(sum(Incorrect_Call == 1 & pitch_name == "Sweeper", na.rm = TRUE) / sum(pitch_name == "Sweeper", na.rm = TRUE)), NA,
                                           sum(Incorrect_Call == 1 & pitch_name == "Sweeper", na.rm = TRUE) / sum(pitch_name == "Sweeper", na.rm = TRUE)),
    Incorrect_Call_Screwball = ifelse(is.nan(sum(Incorrect_Call == 1 & pitch_name == "Screwball", na.rm = TRUE) / sum(pitch_name == "Screwball", na.rm = TRUE)), NA,
                                             sum(Incorrect_Call == 1 & pitch_name == "Screwball", na.rm = TRUE) / sum(pitch_name == "Screwball", na.rm = TRUE)),
    Incorrect_Call_Knuckleball = ifelse(is.nan(sum(Incorrect_Call == 1 & pitch_name == "Knuckleball", na.rm = TRUE) / sum(pitch_name == "Knuckleball", na.rm = TRUE)), NA,
                                               sum(Incorrect_Call == 1 & pitch_name == "Knuckleball", na.rm = TRUE) / sum(pitch_name == "Knuckleball", na.rm = TRUE)),
    Incorrect_Call_Splitter = sum((Incorrect_Call == 1 & pitch_name == "Split-Finger") +
                                   (Incorrect_Call == 1 & pitch_name == "Forkball"), na.rm = TRUE) / 
                               sum((pitch_name == "Split-Finger") +
                                   (pitch_name == "Forkball"), na.rm = TRUE)
    ) 
unique_pitch_names <- unique(pitches_plus_missedcalls_zones$pitch_name)
print(unique_pitch_names)

Umpire_Stats_Impact <- Umpire_Stats %>%
  left_join(Umpire_Impact, by=c("Umpire" = "Umpire"))


##Creating Pitcher Arsenal Statistics
Pitcher_Stats_Count <- pitches_plus_missedcalls_zones %>%
  group_by(player_name) %>%
  summarise(
    Total_Pitches_Thrown = n()
  ) %>%
  ungroup()
Pitcher_Stats <- pitches_plus_missedcalls_zones %>%
  left_join(Pitcher_Stats_Count, by=c("player_name" = "player_name")) %>%
  group_by(player_name, pitch_name) %>%
  summarise(
    Pitches_Thrown = n(),
    Usage_PCT = Pitches_Thrown / Total_Pitches_Thrown,
    Spin_Rate = mean(release_spin_rate, na.rm = TRUE),
    Velocity = mean(release_speed, na.rm = TRUE),
    Veritcal_Break = mean(Vertical_Movement, na.rm = TRUE),
    Horizonal_Break = mean(Horizontal_Movement, na.rm = TRUE),
    Impact_Per_Missed_Call = mean(abs(difference), na.rm = TRUE),
    Favor_Per_Missed_Call = mean(difference, na.rm = TRUE),
    p_Total_Impact = sum(abs(difference), na.rm = TRUE),
    p_Total_Favor = sum(difference, na.rm = TRUE),
    p_Total_Incorrect_Calls = sum(Incorrect_Call, na.rm = TRUE),
    p_Incorrect_Call_Percentage = p_Total_Incorrect_Calls / Pitches_Thrown,
    Pitch_Up_In_PCT = sum((stand == "R" & Incorrect_Call_Zones == 5) +
                   (stand == "L" & Incorrect_Call_Zones == 7), na.rm = TRUE) / Pitches_Thrown,
    Pitch_Up_PCT = sum((stand == "R" & Incorrect_Call_Zones == 6), na.rm = TRUE) / Pitches_Thrown,
    Pitch_Up_Away_PCT = sum((stand == "L" & Incorrect_Call_Zones == 5) +
                     (stand == "R" & Incorrect_Call_Zones == 7), na.rm = TRUE) / Pitches_Thrown,
    Pitch_Low_In_PCT = sum((stand == "R" & Incorrect_Call_Zones == 3) +
                     (stand == "L" & Incorrect_Call_Zones == 9), na.rm = TRUE) / Pitches_Thrown,
    Pitch_Low_PCT = sum(stand == "R" & Incorrect_Call_Zones == 2, na.rm = TRUE) / Pitches_Thrown,
    Pitch_Low_Away_PCT = sum((stand == "L" & Incorrect_Call_Zones == 3) +
                       (stand == "R" & Incorrect_Call_Zones == 9), na.rm = TRUE) / Pitches_Thrown,
    Pitch_In_PCT = sum((stand == "L" & Incorrect_Call_Zones == 8) +
                       (stand == "R" & Incorrect_Call_Zones == 4), na.rm = TRUE) / Pitches_Thrown,
    Pitch_Away_PCT = sum((stand == "L" & Incorrect_Call_Zones == 4) +
                         (stand == "R" & Incorrect_Call_Zones == 8), na.rm = TRUE) / Pitches_Thrown,
    Pitch_Far_PCT = sum((Incorrect_Call_Zones == 10 | Incorrect_Call_Zones == 11 | Incorrect_Call_Zones == 12 | Incorrect_Call_Zones == 13), na.rm = TRUE) / Pitches_Thrown,
    Pitch_Heart_PCT = sum(Incorrect_Call_Zones == 1, na.rm = TRUE) / Pitches_Thrown
  ) %>%
  distinct()

Pitcher_Stats[, 4:24] <- round(Pitcher_Stats[, 4:24], digits = 3)


##Merging Pitch and Umpires to get MC Probabilities
Umpire_Stats_Impact_merge <- Umpire_Stats_Impact %>%
  mutate(merge = 1)

Pitcher_Stats_merge <- Pitcher_Stats %>%
  mutate(merge = 1)

Umpire_Pitcher <- Pitcher_Stats_merge %>%
  left_join(Umpire_Stats_Impact_merge, by=c("merge" = "merge"))

Umpire_Pitcher <- Umpire_Pitcher %>%
  mutate(Probability_Incorrect_Strike = case_when(
    pitch_name == "4-Seam Fastball" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Strike_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Strike_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Strike_Low_In) + 
      (Pitch_In_PCT * Incorrect_Strike_In) + 
      (Pitch_Away_PCT * Incorrect_Strike_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Strike_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Strike_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Strike_Up_In) + 
      (Pitch_Far_PCT * Incorrect_Strike_Far) +
      (Incorrect_Call_Fastball),
    
    
    pitch_name == "Cutter" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Strike_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Strike_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Strike_Low_In) + 
      (Pitch_In_PCT * Incorrect_Strike_In) + 
      (Pitch_Away_PCT * Incorrect_Strike_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Strike_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Strike_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Strike_Up_In) + 
      (Pitch_Far_PCT * Incorrect_Strike_Far) +
      (Incorrect_Call_Cutter),
   
     
    pitch_name == "Sinker" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Strike_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Strike_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Strike_Low_In) + 
      (Pitch_In_PCT * Incorrect_Strike_In) + 
      (Pitch_Away_PCT * Incorrect_Strike_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Strike_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Strike_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Strike_Up_In) + 
      (Pitch_Far_PCT * Incorrect_Strike_Far) +
      (Incorrect_Call_Sinker),
    
    
    pitch_name == "Changeup" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Strike_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Strike_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Strike_Low_In) + 
      (Pitch_In_PCT * Incorrect_Strike_In) + 
      (Pitch_Away_PCT * Incorrect_Strike_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Strike_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Strike_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Strike_Up_In) + 
      (Pitch_Far_PCT * Incorrect_Strike_Far) +
      (Incorrect_Call_Changeup),
    
    
    pitch_name == "Curveball" | pitch_name == "Knuckle Curve" | pitch_name == "Slow Curve" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Strike_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Strike_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Strike_Low_In) + 
      (Pitch_In_PCT * Incorrect_Strike_In) + 
      (Pitch_Away_PCT * Incorrect_Strike_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Strike_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Strike_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Strike_Up_In) + 
      (Pitch_Far_PCT * Incorrect_Strike_Far) +
      (Incorrect_Call_Curveball),    
    
    
    pitch_name == "Slider" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Strike_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Strike_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Strike_Low_In) + 
      (Pitch_In_PCT * Incorrect_Strike_In) + 
      (Pitch_Away_PCT * Incorrect_Strike_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Strike_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Strike_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Strike_Up_In) + 
      (Pitch_Far_PCT * Incorrect_Strike_Far) +
      (Incorrect_Call_Slider),    
    
    
    pitch_name == "Slurve" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Strike_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Strike_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Strike_Low_In) + 
      (Pitch_In_PCT * Incorrect_Strike_In) + 
      (Pitch_Away_PCT * Incorrect_Strike_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Strike_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Strike_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Strike_Up_In) + 
      (Pitch_Far_PCT * Incorrect_Strike_Far) +
      (Incorrect_Call_Slurve),    
    
   
    pitch_name == "Sweeper" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Strike_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Strike_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Strike_Low_In) + 
      (Pitch_In_PCT * Incorrect_Strike_In) + 
      (Pitch_Away_PCT * Incorrect_Strike_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Strike_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Strike_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Strike_Up_In) + 
      (Pitch_Far_PCT * Incorrect_Strike_Far) +
      (Incorrect_Call_Sweeper),  
    
    
    pitch_name == "Screwball" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Strike_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Strike_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Strike_Low_In) + 
      (Pitch_In_PCT * Incorrect_Strike_In) + 
      (Pitch_Away_PCT * Incorrect_Strike_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Strike_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Strike_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Strike_Up_In) + 
      (Pitch_Far_PCT * Incorrect_Strike_Far) +
      (Incorrect_Call_Screwball), 
    
    
    pitch_name == "Split-Finger" | pitch_name == "Forkball"  ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Strike_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Strike_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Strike_Low_In) + 
      (Pitch_In_PCT * Incorrect_Strike_In) + 
      (Pitch_Away_PCT * Incorrect_Strike_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Strike_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Strike_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Strike_Up_In) + 
      (Pitch_Far_PCT * Incorrect_Strike_Far) +
      (Incorrect_Call_Splitter), 
    
    
    TRUE ~ NA_real_
  ),
  Probability_Incorrect_Ball = case_when(
    pitch_name == "4-Seam Fastball" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Ball_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Ball_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Ball_Low_In) + 
      (Pitch_In_PCT * Incorrect_Ball_In) + 
      (Pitch_Away_PCT * Incorrect_Ball_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Ball_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Ball_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Ball_Up_In) + 
      (Pitch_Heart_PCT * Incorrect_Ball_Heart) +
      (Incorrect_Call_Fastball),
    
    
    pitch_name == "Cutter" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Ball_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Ball_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Ball_Low_In) + 
      (Pitch_In_PCT * Incorrect_Ball_In) + 
      (Pitch_Away_PCT * Incorrect_Ball_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Ball_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Ball_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Ball_Up_In) + 
      (Pitch_Heart_PCT * Incorrect_Ball_Heart) +
      (Incorrect_Call_Cutter),
   
     
    pitch_name == "Sinker" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Ball_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Ball_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Ball_Low_In) + 
      (Pitch_In_PCT * Incorrect_Ball_In) + 
      (Pitch_Away_PCT * Incorrect_Ball_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Ball_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Ball_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Ball_Up_In) + 
      (Pitch_Heart_PCT * Incorrect_Ball_Heart) +
      (Incorrect_Call_Sinker),
    
    
    pitch_name == "Changeup" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Ball_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Ball_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Ball_Low_In) + 
      (Pitch_In_PCT * Incorrect_Ball_In) + 
      (Pitch_Away_PCT * Incorrect_Ball_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Ball_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Ball_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Ball_Up_In) + 
      (Pitch_Heart_PCT * Incorrect_Ball_Heart) +
      (Incorrect_Call_Changeup),
    
    
    pitch_name == "Curveball" | pitch_name == "Knuckle Curve" | pitch_name == "Slow Curve" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Ball_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Ball_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Ball_Low_In) + 
      (Pitch_In_PCT * Incorrect_Ball_In) + 
      (Pitch_Away_PCT * Incorrect_Ball_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Ball_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Ball_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Ball_Up_In) + 
      (Pitch_Heart_PCT * Incorrect_Ball_Heart) +
      (Incorrect_Call_Curveball),    
    
    
    pitch_name == "Slider" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Ball_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Ball_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Ball_Low_In) + 
      (Pitch_In_PCT * Incorrect_Ball_In) + 
      (Pitch_Away_PCT * Incorrect_Ball_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Ball_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Ball_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Ball_Up_In) + 
      (Pitch_Heart_PCT * Incorrect_Ball_Heart) +
      (Incorrect_Call_Slider),    
    
    
    pitch_name == "Slurve" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Ball_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Ball_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Ball_Low_In) + 
      (Pitch_In_PCT * Incorrect_Ball_In) + 
      (Pitch_Away_PCT * Incorrect_Ball_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Ball_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Ball_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Ball_Up_In) + 
      (Pitch_Heart_PCT * Incorrect_Ball_Heart) +
      (Incorrect_Call_Slurve),    
    
   
    pitch_name == "Sweeper" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Ball_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Ball_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Ball_Low_In) + 
      (Pitch_In_PCT * Incorrect_Ball_In) + 
      (Pitch_Away_PCT * Incorrect_Ball_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Ball_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Ball_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Ball_Up_In) + 
      (Pitch_Heart_PCT * Incorrect_Ball_Heart) +
      (Incorrect_Call_Sweeper),  
    
    
    pitch_name == "Screwball" ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Ball_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Ball_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Ball_Low_In) + 
      (Pitch_In_PCT * Incorrect_Ball_In) + 
      (Pitch_Away_PCT * Incorrect_Ball_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Ball_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Ball_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Ball_Up_In) + 
      (Pitch_Heart_PCT * Incorrect_Ball_Heart) +
      (Incorrect_Call_Screwball), 
    
    
    pitch_name == "Split-Finger" | pitch_name == "Forkball"  ~ 
      (((p_Incorrect_Call_Percentage * Pitches_Thrown) / Pitches_Thrown) *
      ((Incorrect_Call_Percentage * Total_Pitches) / (Total_Pitches))) +
      (Pitch_Low_Away_PCT * Incorrect_Ball_Low_Away) + 
      (Pitch_Low_PCT * Incorrect_Ball_Low) + 
      (Pitch_Low_In_PCT * Incorrect_Ball_Low_In) + 
      (Pitch_In_PCT * Incorrect_Ball_In) + 
      (Pitch_Away_PCT * Incorrect_Ball_Away) + 
      (Pitch_Up_Away_PCT * Incorrect_Ball_Up_Away) + 
      (Pitch_Up_PCT * Incorrect_Ball_Up) + 
      (Pitch_Up_In_PCT * Incorrect_Ball_Up_In) + 
      (Pitch_Heart_PCT * Incorrect_Ball_Heart) +
      (Incorrect_Call_Splitter), 
    
    
    TRUE ~ NA_real_
    ), 
  Incorrect_Strikes_per_100 = Probability_Incorrect_Strike * 100,
  Incorrect_Balls_per_100 = Probability_Incorrect_Ball * 100,
  Delta_Missed_Call = Incorrect_Strikes_per_100 - Incorrect_Balls_per_100,
  Avg_Impact_Per_Missed_Call = 
    (Umpire_Impact_per_Missed_Call), 
  Total_Pitch_Run_Impact = (Incorrect_Strikes_per_100 + Incorrect_Balls_per_100) * Avg_Impact_Per_Missed_Call,
  Total_Pitch_Favor = Delta_Missed_Call * Avg_Impact_Per_Missed_Call,
  Usage_Incorrect_Strikes_per_100 = Probability_Incorrect_Strike * 100 * Usage_PCT, 
  Usage_Incorrect_Balls_per_100 = Probability_Incorrect_Ball * 100 * Usage_PCT,
  Usage_Delta_Missed_Call = Usage_Incorrect_Strikes_per_100 - Usage_Incorrect_Balls_per_100,
  Usage_Total_Pitch_Run_Impact = (Usage_Incorrect_Strikes_per_100 + Usage_Incorrect_Balls_per_100) * Avg_Impact_Per_Missed_Call,
  Usage_Total_Pitch_Favor = Usage_Delta_Missed_Call * Avg_Impact_Per_Missed_Call
)

Umpire_Pitcher_Sort <- Umpire_Pitcher %>%
  select(1:13, 26:31, 61:68, 70:78) %>%
  filter(Pitches_Thrown >= 100, Games >= 50) %>%
  arrange(Total_Pitch_Favor)

impact <- Umpire_Pitcher_Sort %>%
  select(player_name, pitch_name, Umpire, Total_Pitch_Run_Impact) %>%
  arrange(Total_Pitch_Run_Impact)

favor <- Umpire_Pitcher_Sort %>%
  select(player_name, pitch_name, Umpire, Total_Pitch_Favor) %>%
  arrange(Total_Pitch_Favor)


##Creating Graphics
total_games <- pitches_plus_missedcalls %>%
  select(game_pk) %>%
  distinct()

sum(pitches_plus_missedcalls$Incorrect_Call) / 
sum(pitches_plus_missedcalls$difference, na.rm = TRUE) / 6895
sum(abs(pitches_plus_missedcalls$difference), na.rm = TRUE) / 6895
 