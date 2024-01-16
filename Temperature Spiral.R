library(tidyverse)
library(readxl)
library(gganimate)


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



next_jan <- mm %>%  
  filter(Month == "Jan") %>%
  mutate(Year = Year,
         Month = "next_Jan")

t_data <- bind_rows(next_jan, mm) %>%
  mutate(Month = factor(Month, levels = c(month.abb, "next_Jan")),
         Month_number = as.numeric(Month),
  )

temp_lines <- tibble(
  x = 1,
  y = c(27.5, 28.0),
  labels = c("1.0\u00B0C", "2.0\u00B0C")
)

gridlines <- tibble(
  x =  1.4,
  xend = 12.4,
  y = c(27.5, 28.0), 
  yend = y
)

a <- t_data %>% 
  ggplot(aes(x = Month_number, y = Tmean, group = Year,
             color = Tmean)) +  
  geom_line() +
  geom_segment(data = gridlines, aes(x = x, y = y, xend = xend, yend = yend),
               color="red", lwd = 1.2,
               inherit.aes = FALSE) +
  geom_text(data = temp_lines, aes(x=x, y=y, label = labels), 
            color = "gold", fontface = "bold", size = 3, inherit.aes = FALSE) + 
  geom_label(aes(x = 10, y =26 , label = Year), label.size = 0, fill = "black", size = 4) +
  scale_x_continuous(breaks=1:12,
                     labels=month.abb) +
  scale_y_continuous(
    limits = c(26, 28)) + 
  scale_color_gradient(low = "darkgreen",
                       high = "red",
                       limits = c(26.0, 29.0)
                       ) +
  #scale_color_gradient2(low = "darkgreen", high = "red", mid = "yellow", midpoint = 27.6) +
  coord_polar() + 
  labs(x = NULL,
       y = NULL,
       title = "Temperature changes since 1960 - 2022", fontface = "bold") +
  theme(
    panel.background = element_rect(fill="black", color="black"),
    plot.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(color="white", size=13),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.title = element_text(color="white", size=13),
    plot.title = element_text(color="white", hjust = 0.5,size = 15),
    legend.title = element_text(NULL),

  ) +
  transition_manual(frames = Year, cumulative = TRUE)

ggsave("figures/temperature_lines 5.png", width=8, height=4.5)
a <- a + labs(color = "")
animate(a, width = 7.155, height = 4.5, unit = "in", res = 300)
anim_save("Temperature_Spiral_11100.gif")
