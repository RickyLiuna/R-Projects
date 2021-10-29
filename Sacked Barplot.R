library(dplyr)
library(reshape2)
library(ggplot2)
library(plotly)

#change this path according to ur arrangement !!!!!!!!!!
data=read.csv("~/Desktop/Jaxie/Sleep_Quality.csv")

#set up data frame
Before_lockdown=data$Alarm_before_lockdown
During_lockdown=data$Alarm_during_lockdown
df=data.frame(Before_lockdown,During_lockdown)
m1 <- melt(df, measure.vars=c("Before_lockdown","During_lockdown"))
m1[,"varval"]<-paste(m1$variable, m1$value, sep="-")


#side by side bar barplot
varp=m1 %>%
  ggplot()+
  aes(varval, fill=value)+
  labs(x = "Alarm usage type",
       y = "Count")+
  geom_bar(stat="count")+
  ggtitle("Alarm usage before and during lockdown")+
  theme(plot.title = element_text(hjust = 0.5))
plotly::ggplotly(varp)


#stacked barplot
myplot=qplot(variable, data=m1, fill=value, ylab="Count", xlab="Alarm usage type")+ 
ggtitle("Alarm usage before and during lockdown")+
  theme(plot.title = element_text(hjust = 0.5))
#facet_wrap( facets= ~variable, scale="free_x")
plotly::ggplotly(myplot)

