library(rtweet)
library(tidyverse)
library(lubridate)
library(colorspace)
library(ggpattern)
library(ggTimeSeries)
library(patchwork)


pepito<- rtweet::get_timeline(user = "@PepitoTheCat",n=3200)


pepito_home<-
  pepito %>%
  filter(str_detect(text,"back home"))



g1<-                  
pepito_home %>%
  mutate(rec_date = lubridate::date(created_at),
         rec_time = str_sub(text,22,23)) %>%
  group_by(rec_date, rec_time) %>%
  summarise(
    quantity = n()
  )%>%
  ungroup()%>%
  ggplot()+
  geom_point(aes(x= rec_date, 
                 y = rec_time, 
                 fill=quantity), 
             pch=21, 
             color = "#575750",
             size = 2,
             alpha = 0.7)+
  scale_fill_continuous_sequential(palette= "Reds 3") +
  theme_light() + 
  theme(
    panel.background = element_rect(fill = "#575756"), 
    panel.grid = element_blank()
  ) +
  labs(
    title = "Pépito is back home",
    x= "date",
    y= "time"
  )



g2<-
  pepito_home %>%
  mutate(rec_time = str_sub(text,22,23)) %>%
  group_by(rec_time) %>%
  summarise(
    quantity = n()
  )%>%
  ungroup()%>%
  ggplot()+
  geom_col_pattern(aes(x= rec_time,y=quantity), 
                               color = "#575756", pattern = "placeholder", pattern_type= "kitten")+
  scale_fill_continuous_sequential(palette= "Reds 3") +
  theme_light() + 
  theme(
    panel.background = element_rect(fill = "#575756"), 
    panel.grid = element_blank(),
    axis.title.y = element_blank()
  )+
  labs(
    title = "Pépito is back home",
    x= "time",
    y= "quantity"
  )


g3<-
pepito_home %>%
  mutate(rec_time = str_sub(text,22,23)) %>%
  group_by(rec_time) %>%
  summarise(
    quantity = n()
  )%>%
  ungroup()%>%
  ggplot()+
  geom_col(aes(x= rec_time,y=quantity, fill=quantity),
            color = "#575756")+
  scale_fill_continuous_sequential(palette= "Reds 3") +
  theme_light() + 
  theme(
    panel.background = element_rect(fill = "#575756"), 
    panel.grid = element_blank(),
    axis.title.y = element_blank()
  )+
  labs(
    title = "Pépito is back home",
    x= "time",
    y= "quantity"
  )



pepito_calendar<-
pepito_home %>%
  mutate(rec_date = date(created_at)) %>%
  group_by(rec_date) %>%
  summarise(
    quantity = n()
  )%>%
  ungroup()


g4<-
  ggplot_calendar_heatmap(pepito_calendar,
                          "rec_date",
                          "quantity")+
  scale_fill_continuous_sequential(palette= "Reds 3") +
  theme_light() + 
  theme(
    panel.background = element_rect(fill = "#575756"), 
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    strip.background = element_rect(fill="white"),
    strip.text = element_text(color = "black")
  ) +
    facet_wrap(~Year, ncol = 1)+
  labs(
    title = "Pépito is back home"
  )


g1/(g2|g3)/g4

