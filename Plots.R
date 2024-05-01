library(ggplot2)
theme_set(theme_bw())

##Umpire Impact GGplot

##Creating  Clusters
Umpire_Impact <- Umpire_Impact %>%
  mutate(cluster = ifelse(exp <= 5, 1,
                   ifelse(exp > 5 & exp <= 10, 2,
                   ifelse(exp > 10 & exp <= 15, 3,
                   ifelse(exp > 15 & exp <= 20, 4,
                   ifelse(exp > 20, 5, 0)      
                         )))))
Umpire_Impact$cluster <- as.factor(Umpire_Impact$cluster)
##Plot
ggplot(data = Umpire_Impact, aes(x = Favor_Per_Game, y = Impact_Per_Game, color = cluster)) +
  geom_point() +
  scale_fill_discrete(labels = c(
    '0-5 years',
    '6-10 years',
    '11-15 years',
    '16-20 years',
    '20+ years')) +
  scale_color_manual(values = c("1" = "orange",
                                "2" = "blue",
                                "3" = "red",
                                "4" = "forestgreen", 
                                "5" = "purple")) +
  labs(title = "Total Impact versus Favor", 
  color = "Experience Clusters")

ggplot(data = Umpire_Impact, aes(x = exp, y = Impact_Per_Game)) +
  geom_point(aes(color = cluster)) +
  scale_fill_discrete(labels = c(
    '0-5 years',
    '6-10 years',
    '11-15 years',
    '16-20 years',
    '20+ years')) +
  scale_color_manual(values = c("1" = "orange",
                                "2" = "blue",
                                "3" = "red",
                                "4" = "forestgreen", 
                                "5" = "purple")) +
  labs(title = "Experience vs Impact", 
       color = "Experience Clusters") +
  geom_smooth(method = lm)

cor(Umpire_Impact$exp, Umpire_Impact$Impact_Per_Game, use = "complete.obs")

  
##Missed_Calls GGPlot
X <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
sz <- data.frame(X,z)
ggplot() +
  geom_path(data = sz, aes(X=X, y=z)) +
  coord_equal() +
  geom_point(data = Missed_Calls, aes(X = plate_X, y = plate_z, size = release_speed, color = type)) +
  scale_size(range = c(-1.0,2.5)) +
  scale_color_discrete() +
  labs(size = "Speed",
       color = "Pitch Type",
       title = "Missed Calls from 2022",
       subtitle = "Based on PitchFX Data") +
  ylab("Feet Above Homeplate") +
  Xlab("Feet From Homeplate")


##GGPlot Test
Trevor_Bauer <- pitches_plus_missedcalls %>%
  filter(player_name == "Bauer, Trevor") %>%
  filter(Missed_Call == 1)

##Drawing The Strike Zone
x <- c(-(((1.57*2 + 17) / 12) / 2),(((1.57*2 + 17) / 12) / 2),(((1.57*2 + 17) / 12) / 2),-(((1.57*2 + 17) / 12) / 2),-(((1.57*2 + 17) / 12) / 2))
z <- c((mean(pitches_plus_missedcalls$sz_bot, na.rm = TRUE)),(mean(pitches_plus_missedcalls$sz_bot, na.rm = TRUE)),(mean(pitches_plus_missedcalls$sz_top, na.rm = TRUE)),(mean(pitches_plus_missedcalls$sz_top, na.rm = TRUE)),(mean(pitches_plus_missedcalls$sz_bot, na.rm = TRUE)))
#store in dataframe
sz <- data.frame(x,z)

ggplot() +
  geom_path(data = sz, aes(x=x, y=z)) +
  coord_equal() +
  geom_point(data = pitches_plus_missedcalls_zones, aes(x = plate_x, y = plate_z, size = release_speed, color = type)) +
  scale_size(range = c(-1.0, 2.5)) +
  scale_color_discrete() +
  labs(size = "Speed",
       color = "Pitch Type",
       title = "Missed Strikes - Pitch Chart",
       subtitle = "Missed Calls") +
  ylab("Feet Above Homeplate") +
  xlab("Feet From Homeplate") +
  scale_shape_manual(values = c(15, 16, 17, 18, 19, 20)) +
  scale_size_continuous(range = c(0.5, 2)) +
  theme(plot.title = element_text(face = "bold", hjust = -0.015, vjust = 0, colour = "#3C3C3C", size = 20),
        plot.subtitle = element_text(face = "plain", hjust = -0.015, vjust = 0.09, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.x = element_text(vjust = 0.5, size = 11, colour = "#535353", face = "bold")) +
  theme(axis.text.y = element_text(size = 11, colour = "#535353", face = "bold")) +
  theme(axis.title.y = element_text(size = 11, colour = "#535353", face = "bold", vjust = 1.5)) +
  theme(axis.title.x = element_text(size = 11, colour = "#535353", face = "bold", vjust = 0)) +
  theme(panel.grid.major.y = element_line(color = "#bad2d4", linewidth = 0.5)) +
  theme(panel.grid.major.x = element_line(color = "#bdd2d4", linewidth = 0.5)) +
  theme(panel.background = element_rect(fill = "white"))

  