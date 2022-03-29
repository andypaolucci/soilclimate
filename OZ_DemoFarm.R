
# Libraries. Probably more than what is required. Use install.packages("packagename") if they are missing
library(tidyverse)
library(hrbrthemes)
library(kableExtra)

options(knitr.table.format = "html")
library(ggstream)
library(viridis)
library(DT)
library(plotly)

library(soilDB)
library(sharpshootR)
library(latticeExtra)
library(RColorBrewer)
library(plyr)
library(weathermetrics)
library(sp)
library(dplyr)
library(devtools)

# get soil temperature, soil moisture, and air temperature data
x <- fetchHenry(project='WI SE Soil Health', what ="soiltemp", gran ='day', soiltemp.summaries=FALSE)
head(x)

# convert celsius to fahrenheit and store in new column
x$soiltemp$sensor_value_f <- celsius.to.fahrenheit(x$soiltemp$sensor_value)

# filter Spring temps 
spring.2 <- dplyr::filter(x$soiltemp, date_time >'2021-04-15 14:00:00'& date_time < '2021-05-12 06:00:00'& sensor_depth=='5')
spring.4 <- dplyr::filter(x$soiltemp, date_time >'2021-04-15 14:00:00'& date_time < '2021-05-12 06:00:00'& sensor_depth=='10')

summer.2 <- dplyr::filter(x$soiltemp, date_time >'2021-07-01 02:00:00'& date_time < '2021-08-01 02:00:00'& sensor_depth=='5')
summer.4 <- dplyr::filter(x$soiltemp, date_time >'2021-07-01 02:00:00'& date_time < '2021-08-01 02:00:00'& sensor_depth=='10')

fall.2 <- dplyr::filter(x$soiltemp, date_time >'2021-09-01 02:00:00'& date_time < '2021-10-06 06:00:00'& sensor_depth=='5')
fall.4 <- dplyr::filter(x$soiltemp, date_time >'2021-09-01 02:00:00'& date_time < '2021-10-06 06:00:00'& sensor_depth=='10')


options(repr.plot.width=6, repr.plot.height=3)
# Plot spring 2" Spaghetti chart

spring.2 %>%
  ggplot(aes(x=date_time, y=sensor_value_f, group=sensor_name, color=sensor_name)) +
  geom_line(size=1, alpha=0.5) +
  scale_color_viridis(discrete = TRUE) +
  theme(
  legend.title=element_blank(),
  legend.key=element_blank(),
  legend.background = element_rect(size = 0.5, colour = "#6D9EC1"),
  legend.position = c(0.88, 0.1),
  plot.margin = margin(4,2,2,3, "cm"),
  plot.title = element_text(size=14),
  panel.background = element_rect(fill = "white", colour = "#6D9EC1",
                                  size = 2, linetype = "solid"),
  panel.grid.major=element_line(colour="grey")
  #panel.grid.minor=element_line(colour="grey")
   ) +
  scale_y_continuous(breaks = c(38, 40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70))+
  scale_x_datetime(breaks = "2 day", expand = c(0,0), date_labels = "%b-%d")+
  ggtitle("Average Daily Spring 2\" Temperature") +
  ylab("Degrees Fahrenheit")+
  xlab("Date")+
  geom_line(y=50, col='black', size=0.5, linetype=2)

spring.4 %>%
  ggplot(aes(x=date_time, y=sensor_value_f, group=sensor_name, color=sensor_name)) +
  geom_line(size=1, alpha=0.5) +
  scale_color_hp(discrete = TRUE) +
  theme(
    legend.title=element_blank(),
    legend.key=element_blank(),
    legend.background = element_rect(size = 0.5, colour = "#6D9EC1"),
    legend.position = c(0.88, 0.1),
    plot.margin = margin(4,2,2,3, "cm"),
    plot.title = element_text(size=14),
    panel.background = element_rect(fill = "white", colour = "#6D9EC1",
                                    size = 2, linetype = "solid"),
    panel.grid.major=element_line(colour="grey")
    #panel.grid.minor=element_line(colour="grey")
  ) +
  scale_y_continuous(breaks = c(38, 40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70))+
  scale_x_datetime(breaks = "2 day", expand = c(0,0), date_labels = "%b-%d")+
  ggtitle("Average Daily Spring 4\" Temperature") +
  ylab("Degrees Fahrenheit")+
  xlab("Date")+
  geom_line(y=50, col='black', size=0.5, linetype=2)

# manually ordering the factor levels
spring.4$sensor_name <- factor(spring.4$sensor_name, levels=c("Soil Health Corn 4 inch", "Conventional Soybean 4 inch", "Conventional Corn 4 inch","Soil Health Soybean 4 inch"))
levels(spring.4$sensor_name)
str(spring.4)

# install dutchmasters package with color palletes 
devtools::install_github("EdwinTh/dutchmasters")
library(dutchmasters)
palette()

install.packages("harrypotter")
library(harrypotter)

#area plot
spring.4 %>%
  ggplot(aes(x=date_time, y=sensor_value_f, fill=sensor_name)) +
  geom_area(alpha=0.7, size=0.1, colour="white",position="dodge") +
  #scale_fill_hp(discrete = TRUE, option = "HarryPotter") +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.title=element_blank(),
    legend.key=element_blank(),
    legend.background = element_rect(size = 0.5, colour = "#6D9EC1"),
    legend.position = c(0.88, 0.1),
    plot.margin = margin(7,2,6,3, "cm"),
    plot.title = element_text(size=14),
    panel.background = element_rect(fill = "white", colour = "#6D9EC1",
                                    size = 2, linetype = "solid"),
    panel.grid.major=element_line(colour="grey"),
    #panel.ontop = TRUE,
    #panel.grid.minor=element_line(colour="grey")
  ) +
  coord_cartesian(ylim=c(10,80))+
  scale_y_continuous(breaks = c(20,35,40,45,50,55,60,65,80), expand = c(0,0), name="Degrees Fahrenheit")+
  scale_x_datetime(breaks = "2 day", expand = c(0,0), date_labels = "%b-%d", name="Date")+
  ggtitle("Average Daily Spring 4\" Temperature") +
  geom_line(y=50, col='red', size=1, linetype=2)

# plot spring 2" area charts 
spring.2 %>%
  ggplot(aes(x=date_time, y=sensor_value_f, group=sensor_name, fill=sensor_name)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Spring 2 inch Temperature") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=14)
  ) +
  facet_wrap(~sensor_name)

# plot spring 2" area charts 
spring.4 %>%
  ggplot(aes(x=date_time, y=sensor_value_f, group=sensor_name, fill=sensor_name)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Spring 4 inch Temperature") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=14)
  ) +
  facet_wrap(~sensor_name)


# plot spring 2" Spaghetti alternate
tmp <- spring.2 %>%
  mutate(name2=sensor_name)

tmp %>%
  ggplot(aes(x=date_time, y=sensor_value_f)) +
  geom_line(data=tmp %>% dplyr::select(-sensor_name), aes(group=name2), color="grey", size=1, alpha=0.5) +
  geom_line(aes(color=sensor_name), color="#69b3a2", size=1.2 )+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  ggtitle("Spring 2 inch Temperature") +
  facet_wrap(~sensor_name)

# Layered Area Charts (in progress)

library(cowplot)
library(paletteer)
library(dplyr)
library(colorspace)

ggplot(spring.2, aes(x=date_time, y=sensor_value_f, fill=sensor_name)) + 
  geom_area(alpha=1, size=.5, colour="white", position=position_dodge()) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  #coord_cartesian(ylim=c(0,70), expand = c(0,0)) +
  scale_y_sqrt() +
  ggtitle("Spring 2 inch Temperature")

spring.2$variable <- factor(spring.2$sensor_name, levels=rev(levels(as.factor(spring.2$sensor_name))))
rev(lebels)

ggplot(spring.2, aes(x=date_time, y=sensor_value_f, fill=sensor_name)) + 
  geom_area(alpha=0.5, size=.5, colour="white",position="dodge")+
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  coord_cartesian(ylim=c(10,80), expand = c(0,0)) +
  scale_y_continuous(breaks = c(20,30,40,50,60,70)) +
  ggtitle("Spring 2 inch Temperature")



