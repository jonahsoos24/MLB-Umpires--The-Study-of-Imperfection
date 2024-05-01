library(baseballr)
library(tidyverse)
library(lubridate)
library(ggplot2)



# Getting a vector of possible week start dates
Dates <- seq.Date(
  from = as.Date("2023-03-30"),
  to = Sys.Date(),
  by = 4
)

# Cutting out dates outside of baseball season
Dates <- Dates[between(month(Dates), 3, 10)]

# Creating object for data to be binded into
savant_collection_pitch_2023<- NULL

for(StartDate in 1:length(Dates)){
  
  savant_collection_pitch_2023 <- savant_collection_pitch_2023 %>%
    rbind(
      suppressMessages(
        scrape_statcast_savant_pitcher_all(
          start_date = Dates[StartDate],
          end_date = Dates[StartDate] + 4
        )
      )
    )
  
  # printing loop progress
  print(StartDate/length(Dates))
}

write.csv(savant_collection_pitch_2023, "savant_collection_pitch_2023.csv")





