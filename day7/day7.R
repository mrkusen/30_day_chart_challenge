library(tidyverse);library(here); library(skimr);library(janitor)
library(MetBrewer)

df = read_csv("day7/7_renewable-share-energy.csv") %>% clean_names()
skim(df)

unique(df$entity)
names(df)
#Top 10 countries using the most energy
country = c("China","United States","India","Japan","Russia",
            "Germany","Brazil","South Korea","Canada","France")
name = unique(df$entity)
#check country names are in the list
country %in% name

#top 5
country = c("China","United States","India","Japan","Russia")

#filter those countries
top = df %>% filter(entity %in% country)
day7 = ggplot(top,aes(x= year,y= renewables_percent_sub_energy, color=entity))+
  geom_line(alpha=.5,size=1)+
  geom_point(size=.8,shape=5)+
  ylim(0,15)+
  theme_classic()+
  labs(x="Year",y="Percentage (%) of Renewables",
       title="Share of Renewable Energy Use",
       subtitle="Top 5 Biggest Global Energy Consumers",
       caption="Created by: Matthew Kusen - @mkusen 
       
       #30DayChartChallenge - Day 7 - OWID
       Data Source: Our World in Data -  https://tinyurl.com/2uwbayt9",
       color="Country")+
  theme(text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "grey98", color = "grey98"),
        panel.background = element_rect(fill = "grey98", color = "grey98"),
        legend.background = element_rect(fill = "grey98", color = "grey98"),
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
  scale_color_manual(values=met.brewer("Peru1", 5))
day7

ggsave(day7,filename="day7/day_7.png",
       width=5.5,height=3.78)
