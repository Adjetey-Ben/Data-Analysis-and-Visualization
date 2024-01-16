library(tidyverse)
library(readxl)
library(ggplot2)
library(gganimate)
library(imputeTS)


#change the months to character
read_excel("Redefined.xlsx") %>% 
  mutate(Month = month.abb[as.numeric(Month)]) -> Data

#Equate all other values to NA
Data <- Data %>% 
  mutate(Tmax = ifelse(Tmax >= -99.9 & Tmax <= 6, NA, Tmax),
         Tmin = ifelse(Tmin >= -99.9 & Tmin <= 6, NA ,Tmin))

na_kalman(
  Data, 
  model = "StructTS", 
  smooth = TRUE, 
  nit = -1
) -> Data

#plot graph (Maximum) ####
#Avg_Tmax
#find the monthly mean
Monthly_mean <- Data %>% 
  mutate(Month = factor(Month, levels = month.abb)) %>% 
  group_by(Year, Month) %>% 
  summarize(
    Avg_Tmax = mean(Tmax, na.rm = TRUE),
    Avg_Tmin = mean(Tmin, na.rm = TRUE)
  ) %>% 
 group_by(Year) %>% 
  mutate(Legend = mean(Avg_Tmax)) %>% 
  drop_na()

Max <- Monthly_mean %>% 
  ggplot(aes(x= Month, y=Avg_Tmax, group= Year, color=Legend)) +
  geom_line(linewidth = 1) +
  scale_color_gradient2(low = "darkgreen", mid = "yellow", high = "red", midpoint = 31.15) +
  scale_y_continuous(breaks = seq(27,35)) +
  labs( x = NULL,
        y = NULL,
        title = "Average Maximum Temperature Data (\u00B0 C)",
        subtitle = "Daytime temperature from 1960-2022") +
  theme( 
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", linetype = "dotted", linewidth = 0.25),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "black", size = 10)
    
  ) 
#Animation 
Gif_Max <- Max +
  geom_label(aes(x = 8, y = 33.5, label = Year), fontface = "bold", label.size = 2) +
  transition_manual(Year, cumulative = TRUE)

animate(Gif_Max, width = 6, height = 4, unit = "in", res = 300) +
  anim_save("Gif_1Max.gif")



#plot graph (Minimum)####
#Avg_Tmin
#find the monthly mean
Monthly_mean <- Data %>% 
  mutate(Month = factor(Month, levels = month.abb)) %>% 
  group_by(Year, Month) %>% 
  summarize(
    Avg_Tmax = mean(Tmax, na.rm = TRUE),
    Avg_Tmin = mean(Tmin, na.rm = TRUE)
  ) %>% 
  group_by(Year) %>% 
  mutate(Legend = mean(Avg_Tmin)) %>% 
  drop_na()


Min <- Monthly_mean %>% 
  ggplot(aes(x= Month, y=Avg_Tmin, group= Year, color=Legend)) +
  geom_line(linewidth = 1) +
  scale_color_gradient2(low = "blue", mid = "green", high = "red", midpoint = 24) +
  scale_y_continuous(breaks = seq(21, 27)) +
  labs(x = NULL,
       y = NULL,
       title = "Average Minimum Temperature Data (\u00B0 C)",
       subtitle = "Nighttime temperature from 1960-2022") +
  theme( 
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", linetype = "dotted", linewidth = 0.25),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "black", size = 10)
    
  ) 

#Animation
Gif_Min <- Min +
  geom_label(aes(x = 8, y = 25.5, label = Year), fontface = "bold", label.size = 2) +
  transition_manual(Year, cumulative = TRUE)

animate(Gif_Min, width = 6, height = 4, unit = "in", res = 300) +
  anim_save("Gif_Min.gif")