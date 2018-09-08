#swd challenge 08/09/2018

library(tidyverse)
library(readxl)
library(grid)
library(gridExtra)

#read in data
data <- read_xlsx("DATA+for+Sept18+SWDchallenge.xlsx")

#default read-in of file isn't quite right, reformat the data
colnames(data) <- c("Region","2000","2016")
data <- data[3:8,]

#create nice text labels before splitting data

data %>% mutate(LHlabel = paste0(Region," - ",scales::percent(`2000`))) %>%
  mutate(RHlabel = scales::percent(round(`2016`,2))) -> data

data.other <- data%>%filter(Region!="Asia Pacific")
data.ap <- data%>%filter(Region=="Asia Pacific")

highlightCol <- "blue"
lowlightCol <- "grey50"

#colour the words in the title to match the colours in the graph
#see: https://stackoverflow.com/questions/49735290/ggplot2-color-individual-words-in-title

t1 <- textGrob(expression(phantom(bold("Asia Pacific")) * " is now the largest global market for tourism - it should be the focus for businesses"),
               x = 0.05, y = 1.06, gp = gpar(col = lowlightCol),just="left")

t2 <- textGrob(expression(bold("Asia Pacific") * phantom(" is now the largest global market for tourism - it should be the focus for businesses")),
               x = 0.05, y = 1.06, gp = gpar(col = highlightCol),just="left")
t3 <- textGrob("Region share of total tourism contribution to GDP",
               x = 0.05, y = 1.03, gp = gpar(col = lowlightCol,fontsize=10),just="left")

ggplot() + geom_segment(data=data.other,aes(x=0,xend=1,y=`2000`,yend=`2016`),col=lowlightCol,size=1) +
  geom_segment(data=data.ap,aes(x=0,xend=1,y=`2000`,yend=`2016`), col=highlightCol,size=1) +
  geom_point(data=data.ap,aes(x=0,y=`2000`),col=highlightCol,size=2) +
  geom_point(data=data.ap,aes(x=1,y=`2016`),col=highlightCol,size=2) +
  geom_point(data=data.other,aes(x=0,y=`2000`),col=lowlightCol,size=2) +
  geom_point(data=data.other,aes(x=1,y=`2016`),col=lowlightCol,size=2) +
  geom_text(data=data.other,aes(x=0,y=`2000`,label=LHlabel),hjust=1,col=lowlightCol,nudge_x=-0.1) +
  geom_text(data=data.ap,aes(x=0,y=`2000`,label=LHlabel),hjust=1,col=highlightCol,nudge_x=-0.1) +
  geom_text(data=data.other,aes(x=1,y=`2016`,label=RHlabel),hjust=-1,col=lowlightCol)+
  geom_text(data=data.ap,aes(x=1,y=`2016`,label=RHlabel),hjust=-1,col=highlightCol)+
  scale_x_continuous(expand=c(0.3,0.3),breaks=c(0,1),labels=c("2000","2016")) +
  theme_minimal() +
  theme(panel.grid=element_blank(),
        text= element_text(colour="grey50"),
        axis.text.y=element_blank(),
        axis.title=element_blank(),
        plot.margin=unit(c(3.5,1,1,1),"lines"),
        plot.caption=element_text(hjust=1,mar=unit(c(3,1,1,1),"lines"))) +
  labs(caption="Source: World Travel and Tourism Council - #SWDchallenge - Design by @stevejburr") +
    annotation_custom(grobTree(t1, t2,t3)) -> p

g <- ggplot_gtable(ggplot_build(p))
g$layout$clip[g$layout$name == "panel"] <- "off"


ggsave("plot.png",plot=g, height=8, width=8)




                        