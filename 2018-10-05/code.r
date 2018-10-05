library(tidyverse)
library(lubridate)

data <- read_csv("avocado.csv")


#do weighted average of prices across whole data cut by convetional vs not

data %>% group_by(Year,Date) %>% 
  summarise(Weight=sum(`Total Volume`),WeightedPrice=sum(AveragePrice*`Total Volume`)) %>%
  mutate(`Average Price`=WeightedPrice/Weight) %>%
  ungroup() %>%
  mutate(Date=dmy(Date)) %>%
  ggplot() +
  geom_point(aes(y=Weight,x=`Average Price`,colour=as.factor(Year)),size=3,alpha=0.5) +
  geom_smooth(aes(y=Weight,x=`Average Price`,colour=as.factor(Year)),se=FALSE,method=lm) +
  annotate("text",x=1.1,y=160000000,label="The biggest selling weeks are around\nthe superbowl and prices fall at this key time",colour="grey50")+
  annotate("text",x=1.50,y=90000000,label="2017 saw the highest\n prices for avocados",colour="grey50")+
  scale_colour_discrete("Year")+
  scale_x_continuous("Average price per avocado",labels=scales::dollar)+
  scale_y_continuous("Total avocados sold per week",labels=scales::comma)+
  theme_minimal() +
  theme(panel.grid=element_blank(),
        text=element_text(colour="grey50"),
        axis.ticks=element_line(colour="grey50"),
        axis.text=element_text(colour="grey50"),
        axis.title = element_text(colour="grey50")) +
  labs(title="Savy shoppers? - when the price of avocados goes up, their sales fall across the USA",
       subtitle="Each data point represents total weekly sales across a 54 major US cities",
       caption="Design by @stevejburr - Data source= Hass Avocado Board") 

#labell the top 3 sales weeks + comment on what they are

ggsave("plot.png",width=9,height=9)

             