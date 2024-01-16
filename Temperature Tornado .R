library(tidyverse)
library(readxl)
library(ggplot2)


#change the months to character
read_excel("Redefined.xlsx") %>% 
  mutate(Month = month.abb[as.numeric(Month)]) -> Data

#Equate all other values to NA
Data <- Data %>% 
  mutate(Tmax = ifelse(Tmax >= -99.9 & Tmax <= 6, NA, Tmax),
         Tmin = ifelse(Tmin >= -99.9 & Tmin <= 6, NA,Tmin))


mm <- Data %>% 
  mutate(Month = factor(Month, levels = month.abb)) %>% 
  group_by(Year, Month) %>% 
  summarize(Avg_Tmin = mean(Tmin, na.rm = TRUE),
            Avg_Tmax = mean(Tmax, na.rm = TRUE))


mm <- mm %>% 
  mutate(Tmean = mean(Avg_Tmax + Avg_Tmin) / 2) %>% 
  select(Year, Month, Tmean)

#
mm <- mm %>%
  filter(Month == "Aug" | Month == "Apr") %>% 
  pivot_wider(names_from = Month, values_from = Tmean) 


grid_lines <- tibble(
  x = c(-30, -20, 20, 30),
  y = 2030,
  labels = c("+1\u00B0 C", "0\u00B0 C", "0\u00B0 C", "+1\u00B0 C")
)

years_label <- tibble(
  x = -2,
  y = c(seq(1960, 2020, by=10), 2022)
)


mm %>%
  mutate(Legend = (Aug + Apr) / 2) %>% 
  ggplot(aes(x = Aug, xend = Apr, y = Year, yend = Year, color = Legend)) +
  geom_vline(xintercept = c(-30, -20, 20, 30), color = "red") +
  geom_label(data = grid_lines, aes(x=x, y=y, label=labels),
             inherit.aes = FALSE, fill = "black", color= "gold", label.size = 0, fontface = "bold") +
  geom_segment(lwd = 1.2, lineend = "round") +
  geom_text(data = years_label, aes(x=x, y=y, label=y),
            inherit.aes = FALSE, color= "gold", size= 3, fontface= "bold") +
  scale_color_gradient2(low = "blue", mid = "white", high = "darkred", midpoint = 27.5) +
  theme(
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black",),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) 

ggsave("tornado.png", width = 4.5, height = 3.5, units = "in")