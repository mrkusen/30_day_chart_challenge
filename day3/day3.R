library(tidyverse); library(magrittr); library(lubridate); library(here);
library(skimr)
library(MetBrewer)
library(scales)
maternal_data <- read_csv("day3/IHME-GBD_2019_DATA-9cc83da6-1.csv")
skim(maternal_data)
all_cause <- read_csv("day3/allcountries_mortality.csv")

unique(all_cause$cause_name)
all_self_harm = all_cause %>% filter(cause_name == "Self-harm and interpersonal violence",
                                    metric_id == "1")

names = sort(names)
se_asia = c("Cambodia","Indonesia","Thailand","Viet Nam","Brunei Darussalam","Singapore","Philippines","Myanmar","Malaysia","Myanmar","Lao People's Democratic Republic")

change_df = all_self_harm %>% filter(location_name %in% se_asia)
change_df

change_df$location_name = recode(change_df$location_name,
                                 "Lao People's Democratic Republic" = "Laos",
                                 "Brunei Darussalam" = "Brunei")

a = ggplot(change_df) +
 aes(x = year, y = val, colour = location_name) +
 geom_point(shape = "circle", size = 1.5) +
  geom_line()+
 theme_minimal()+
  labs(y="# of Deaths due to Self Harm and Interpersonal Violence",x="Year",
       title="Deaths due to Self-harm and Interpersonal Violence",
       color="Country",
       subtitle = "In South East Asia - 1990 - 2019",
       caption="Created by Matthew Kusen - @mkusen 
       
       #30DayChartChallenge - Day 3 Historical
       Data Source: IHME 2020 - Global Burden of Disease Study 2019
       http://ghdx.healthdata.org/")+
  scale_color_manual(values=met.brewer("Signac", 10))+
  theme_light()+
  theme(legend.key = element_rect(linetype = "dotted"),
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
                                    family = "mono"))+
  scale_y_continuous(label=comma)+
  geom_label(data = change_df %>% filter(val > 3000 & year == "2005"), 
             aes(label=location_name), nudge_y = 1700)

a

