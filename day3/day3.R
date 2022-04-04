library(tidyverse); library(magrittr); library(lubridate); library(here);
library(skimr)
library(MetBrewer)
maternal_data <- read_csv("day3/IHME-GBD_2019_DATA-9cc83da6-1.csv")
skim(maternal_data)

names = sort(names)
se_asia = c("Cambodia","Indonesia","Thailand","Viet Nam",
            "Brunei Darussalam","Singapore","Philippines",
            "Myanmar","Malaysia","Myanmar","Lao People's Democratic Republic")

sea_df = maternal_data %>% filter(location_name %in% se_asia)
sea_df_m = sea_df %>% filter(cause_name == "Maternal and neonatal disorders",
                  metric_name == "Rate")
sea_df_m$location_name = recode(sea_df_m$location_name, 
                    "Lao People's Democratic Republic" = "Laos",
                    "Brunei Darussalam" = "Brunei")

a = ggplot(sea_df_m) +
 aes(x = year, y = val, colour = location_name) +
 geom_point(shape = "circle", size = 1.5) +
  geom_line()+
 theme_minimal()+
  labs(y="Maternal Mortality Rate per 100,000",x="Year",
       title="Maternal Mortality Trends in South East Asia",
       color="Country",
       subtitle = "1990 - 2019",
       caption="Created by Matthew Kusen - @mkusen 
       
       #30DayChartChallenge - Day 3 Historical
       Data Source: IHME 2020 - Global Burden of Disease Study 2019
       http://ghdx.healthdata.org/")+
  scale_color_manual(values=met.brewer("Homer1", 10))+
  theme_light()+
  theme(legend.key.size = unit(.1, 'cm'),
        legend.position = c(.85, .75),
        legend.box.background = element_rect(color="black", size=.5),
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        axis.title.x = element_text(hjust = .5,
                                    size=10,vjust = .5, 
                                    face="bold"),
        axis.title.y = element_text(hjust = .5,
                                    size=10,vjust = .5, 
                                    face="bold"),
        plot.title = element_text(hjust = 0.5, 
                                  vjust = 0.1,
                                  size = 16, 
                                  face = "bold"),
        plot.subtitle = element_text(hjust = .5,
                                     size = 12),
        plot.caption = element_text(size = 8.5,
                                    hjust = 1,
                                    family = "mono"))
a

ggsave(a,filename = "day3/day3.jpg")
