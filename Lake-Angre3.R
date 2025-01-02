#Packages####
library(tidyverse)
library(gsheet)
library(ggpubr)
library(corrplot)
library(MuMIn)

#LandSat data ####
df<-gsheet2tbl('https://docs.google.com/spreadsheets/d/15hyMHrjEkowzHuhVVMfAU_PwPCB7PmadSsEqakbW02w/edit?gid=0#gid=0')
df$date<-as.Date.character(df$date, '%d.%m.%Y')

ggplot(df, aes(date, temp, colour = sattelit))+
  geom_point()+
  geom_line()+
  theme_classic()

landsat57<-subset(df, sattelit == 'LandSat 5-7')
landsat89<-subset(df, sattelit == 'Landsat8-9')

#| We have found no reason for using LandSat 8-9 data
#| as it covers much shorter perion of time than LandSat 5-7

#Logger data ####
df<-gsheet2tbl('https://docs.google.com/spreadsheets/d/15hyMHrjEkowzHuhVVMfAU_PwPCB7PmadSsEqakbW02w/edit?gid=1711426879#gid=1711426879')
df$date<-as.Date.character(df$date, '%d.%m.%Y')

logger<-df
logger$month<-as.character(logger$date)
logger$month<-str_split_i(logger$month, '-', 2)
logger_summer<-logger
logger_summer$T1[!str_detect(logger_summer$month, '06|07|08|09|10')]<-NA

#| Water temperature measured with 'surface' data logger
#| Time intervals when tle lake is covered with ice are shaded
ggplot(logger, aes(date, T1))+ # T1 is a temperature measured with 'surface' data logger
  geom_point(shape = 1)+
  geom_line()+
  geom_rect(aes(xmin = as.Date('2019-11-01'), xmax = as.Date('2020-05-31'), ymin = -1, ymax = 18),
            alpha = 0.005)+
  geom_rect(aes(xmin = as.Date('2018-11-01'), xmax = as.Date('2019-05-31'), ymin = -1, ymax = 18),
            alpha = 0.005)+
  theme_classic()+
  xlab('Date')+
  ylab('Temperature')

#| Overlay of the 'surface' logger data and 
#| the lake surface temperature obtained from the LandSat 5-7 data.
#| It is clearly seen that the LandSat 5-7 temperature (in blue)
#| is much higher than than the data logger temperature (in red) in 2020 and 2021.
#| It may indicate that the data logger sank in these two high-flood years
#| Again, winters are represented by shaded areas

ggplot()+
  geom_point(data=logger_summer, aes(date, T1), colour = '#ee6655')+
  geom_line(data=logger_summer, aes(date, T1), colour = '#ee6655')+
  geom_point(data=landsat57, aes(date, temp), colour = '#4488ee')+
  geom_line(data=landsat57, aes(date, temp), colour = '#4488ee')+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2019-11-01'), xmax = as.Date('2020-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2018-11-01'), xmax = as.Date('2019-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2020-11-01'), xmax = as.Date('2021-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  theme_classic()+
  xlab('Date')+
  ylab('Temperature')

#Lake area data####
area <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1vPUWXa1ZdKpTTTWg_HGvypZjArvRPSfObzt0sv3WyWw/edit?gid=1849254114#gid=1849254114")
area<-area[,c(2:3,6)]

area<-area %>%
  mutate(year = 0,
         month = 0,
         day = 0)

for(i in 1:nrow(area)){
  area$year[i]<-str_split(area$date[i], '-')[[1]][1]
  area$month[i]<-str_split(area$date[i], '-')[[1]][2]
  area$day[i]<-str_split(area$date[i], '-')[[1]][3]
}

#| Overlay of the 'surface' logger data and 
#| the lake area.
#| Summer data logger temperature (in red) is comparatively low 
#| when water level is high - 
#| probably because the logger sank in high-flood years
#| Winters are represented by shaded areas
ggplot()+
  geom_point(data=logger, aes(date, T1), colour = '#ee6655', shape = 1)+
  geom_line(data=logger, aes(date, T1), colour = '#ee6655')+
  geom_point(data=area, aes(date, area/30000), colour = '#4488ee')+
  geom_line(data=area, aes(date, area/30000), colour = '#4488ee')+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2019-11-01'), xmax = as.Date('2020-05-31'), ymin = -1, ymax = 18),
            alpha = 0.005)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2018-11-01'), xmax = as.Date('2019-05-31'), ymin = -1, ymax = 18),
            alpha = 0.005)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2020-11-01'), xmax = as.Date('2021-05-31'), ymin = -1, ymax = 18),
            alpha = 0.005)+
  theme_classic()+
  xlab('Date')+
  ylab('Temperature / Area')+
  theme(axis.text.y = element_blank())+
  xlim(as.Date('2018-07-01'), as.Date('2021-09-15'))

#Plotting temperatures and area together####
plot1<-ggplot()+
  geom_point(data=logger_summer, aes(date, T1), colour = '#ee6655', size = 3, shape=1)+
  geom_line(data=logger_summer, aes(date, T1), colour = '#ee6655', linewidth = 1.5, alpha = .7)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2019-11-01'), xmax = as.Date('2020-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2018-11-01'), xmax = as.Date('2019-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2020-11-01'), xmax = as.Date('2021-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  theme_classic()+
  xlim(as.Date('2018-07-01'), as.Date('2021-09-15'))+
  xlab('Date')+
  ylab('Logger emperature')

plot2<-ggplot()+
  geom_point(data=landsat57, aes(date, temp), colour = '#4488ee', size = 3)+
  geom_line(data=landsat57, aes(date, temp), colour = '#4488ee', linewidth = 1.5, alpha = .7)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2019-11-01'), xmax = as.Date('2020-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2018-11-01'), xmax = as.Date('2019-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2020-11-01'), xmax = as.Date('2021-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  theme_classic()+
  xlim(as.Date('2018-07-01'), as.Date('2021-09-15'))+
  xlab('Date')+
  ylab('LandSat 5-7 temperature')

plot3<-ggplot()+
  geom_point(data=area, aes(date, area), colour = '#44ee55', size = 3)+
  geom_line(data=area, aes(date, area), colour = '#44ee55', linewidth = 1.5, alpha = .7)+
  geom_rect(data=area, aes(date, area, xmin = as.Date('2019-11-01'), xmax = as.Date('2020-05-31'), ymin = -1, ymax = 300000),
            alpha = 0.005)+
  geom_rect(data=area, aes(date, area, xmin = as.Date('2018-11-01'), xmax = as.Date('2019-05-31'), ymin = -1, ymax = 300000),
            alpha = 0.005)+
  geom_rect(data=area, aes(date, area, xmin = as.Date('2020-11-01'), xmax = as.Date('2021-05-31'), ymin = -1, ymax = 300000),
            alpha = 0.005)+
  theme_classic()+
  xlim(as.Date('2018-07-01'), as.Date('2021-09-15'))+
  xlab('Date')+
  ylab('Area')

#| Aligned plots of the 
#| 'surface' logger temperature (upper panel)
#| 'LandSat 5-7 temperature (middle panel)
#| and the lake area (lower panel)
ggarrange(plot1, plot2, plot3, nrow = 3, align = 'v')


#Predict the logger data with LandSat 5-7 data####

pred.df<-left_join(landsat57, logger_summer, by = 'date')
pred.df<-subset(pred.df, !is.na(T1) & !is.na(temp))

cor.test(pred.df$temp, pred.df$T1)
ggplot(pred.df, aes(temp, T1))+
  geom_point(colour = '#4488ee', size = 3)+
  geom_smooth(colour = '#4488ee', se = F, method = 'lm')+
  theme_classic()+
  xlab('LST')+
  ylab('Surface logger temperature')

pred.df$date_dif<-NA
pred.df$area<-NA
for(i in 1:nrow(pred.df)){
  dif<-area$date-pred.df$date[i]
  closest_area<-mean(area$area[abs(dif) == min(abs(dif))])
  pred.df$date_dif[i]<- min(abs(dif))
  pred.df$area[i]<-closest_area
}

model<-lm(T1~0+temp, data = pred.df)
model<-lm(T1~0+temp+area, data = pred.df)

pred.df$is.toomuch<-case_when(pred.df$area > 210000 ~ 1,
                              .default = 0)
#| adjusted R-squared = 0.356
#| AIC = 38.63
model<-lm(T1~temp+is.toomuch, data = pred.df)
#| adjusted R-squared = 0.9001
#| AIC = 20.46
#| Intercept is not significant
model<-lm(T1~is.toomuch*temp, data = pred.df)
#| adjusted R-squared = 0.9891
#| AIC = 19.14
#| This is the best model
model<-lm(T1~0+is.toomuch*temp, data = pred.df)
best.model<-model

#|Logger temperature may be pedicted well with LandSat57 surface temperature
#|and the lake area.
#|However, LandSat 5-7 data seems to be a better source of data,
#|as data logger sinks underestimates surface temperature when
#|water level is high.
