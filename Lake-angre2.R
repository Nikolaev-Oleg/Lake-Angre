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

#Plotting temperatures and area all together####
plot1<-ggplot()+
  geom_point(data=logger_summer, aes(date, T1), colour = '#ee6655')+
  geom_line(data=logger_summer, aes(date, T1), colour = '#ee6655')+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2019-11-01'), xmax = as.Date('2020-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2018-11-01'), xmax = as.Date('2019-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2020-11-01'), xmax = as.Date('2021-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  theme_classic()+
  xlim(as.Date('2018-07-01'), as.Date('2021-09-15'))+
  xlab('Date')+
  ylab('Logger temperature')

plot2<-ggplot()+
  geom_point(data=landsat57, aes(date, temp), colour = '#4488ee')+
  geom_line(data=landsat57, aes(date, temp), colour = '#4488ee')+
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
  geom_point(data=area, aes(date, area), colour = '#44ee55')+
  geom_line(data=area, aes(date, area), colour = '#44ee55')+
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

pred.df$date_dif<-NA
pred.df$area<-NA
for(i in 1:nrow(pred.df)){
  dif<-area$date-pred.df$date[i]
  closest_area<-mean(area$area[abs(dif) == min(abs(dif))])
  pred.df$date_dif[i]<- min(abs(dif))
  pred.df$area[i]<-closest_area
}

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
model<-lm(T1~0+is.toomuch*temp, data = pred.df) #best model, R2adj = 0.99
best.model<-model

pred.df_long<-landsat57
pred.df_long$date_dif<-NA
pred.df_long$area<-NA

for(i in 1:nrow(pred.df_long)){
  dif<-area$date-pred.df_long$date[i]
  closest_area<-mean(area$area[abs(dif) == min(abs(dif))])
  pred.df_long$date_dif[i]<- min(abs(dif))
  pred.df_long$area[i]<-closest_area
}
pred.df_long$is.toomuch<-case_when(pred.df_long$area > 210000 ~ 1,
                                   .default = 0)


predicted_T1<-predict(best.model, newdata = pred.df_long)
pred.df_long$predicted_T1<-predicted_T1

#| Overlay of the real 'surface' logger temperature data (red)
#| and predicted logger temperature data (blue)
#| Overestimation of the logger temperature was obviously corrected
#| with a dummy-encoded lake area

ggplot()+
  geom_point(data=logger_summer, aes(date, T1), colour = '#ee6655')+
  geom_line(data=logger_summer, aes(date, T1), colour = '#ee6655')+
  geom_point(data=pred.df_long, aes(date, predicted_T1), colour = '#4488ee')+
  geom_line(data=pred.df_long, aes(date, predicted_T1), colour = '#4488ee')+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2019-11-01'), xmax = as.Date('2020-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2018-11-01'), xmax = as.Date('2019-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  geom_rect(data=logger, aes(date, T1, xmin = as.Date('2020-11-01'), xmax = as.Date('2021-05-31'), ymin = -10, ymax = 28),
            alpha = 0.005)+
  theme_classic()+
  xlim(as.Date('2018-07-01'), as.Date('2021-09-15'))+
  xlab('Date')+
  ylab('Temperature')

#Save all 'winter' time coordinates into a single dataframe
xmin<-paste0(1980:2023, '-11-01')
xmax<-paste0(1981:2024, '-05-31')
xmin<-as.Date(xmin)
xmax<-as.Date(xmax)
winters<-data.frame(x1 = xmin, x2=xmax, y1 = -Inf, y2 = Inf)

#LARC climatological data####
climat<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1vPUWXa1ZdKpTTTWg_HGvypZjArvRPSfObzt0sv3WyWw/edit?gid=0#gid=0')
df<-matrix(ncol=length(unique(climat$param)),
           nrow=nrow(area))
colnames(df)<-unique(climat$param)

for(i in 1:nrow(df)){
  for(j in 1:ncol(df)){
    yr<-area$year[i]
    month<-area$month[i]
    par<-colnames(df)[j]
    temp<-subset(climat, year==yr & param==par)
    temp<-temp[[month]][1]
    df[i,j]<-as.numeric(temp)
  }
}


big_df<-cbind(df, area[, c(3, 2, 1, 4, 5)])
big_df<-as.data.frame(big_df)
big_df<-na.omit(big_df)
big_df$sin_n_days<-abs(sin(big_df$n_days*pi/365.25))
larc<-big_df

larc_wo_area<-climat[, -length(climat)]
larc_wo_area<-as.data.frame(larc_wo_area)
larc_wo_area<-pivot_longer(larc_wo_area, 
                           cols = c('01',
                                    '02',
                                    '03',
                                    '04',
                                    '05',
                                    '06',
                                    '07',
                                    '08',
                                    '09',
                                    '10',
                                    '11',
                                    '12'),
                           names_to = 'month', values_to = 'value')
larc_wo_area<-spread(larc_wo_area,
                     key = param,
                     value = value)

#Predict LARC data with LandSat 5-7 data####
landsat57$year<-str_split_i(landsat57$date, '-', 1)
landsat57$month<-str_split_i(landsat57$date, '-', 2)
larc_wo_area$year<-as.character(larc_wo_area$year)
larc_wo_area$month<-as.character(larc_wo_area$month)
larc_landsat<-left_join(landsat57, larc_wo_area)
larc_landsat$TS<-as.numeric(larc_landsat$TS)
larc_landsat_summer<-subset(larc_landsat, month == '06' |
                              month == '07' |
                              month == '08' |
                              month == '09' |
                              month == '10')

#| We use only data for June - October here as we are
#| exclusively interested in 'summer' data calibration
#| and prediction

#| R-squared = 0.6369
model<-lm(temp~TS, data = larc_landsat_summer)

larc_landsat_summer<-larc_landsat
larc_landsat_summer$TS[!str_detect(larc_landsat_summer$month, '06|07|08|09|10')]<-NA
prediction<-predict(model, newdata = larc_landsat_summer)
larc_landsat_summer$predicted_temp<-prediction

#| Overlay of the real LARC temperature data (red)
#| and LARC temperature data predicted with LandSat 5-7 data (blue)
ggplot(winters)+
  geom_point(data=larc_landsat_summer, aes(date, TS), colour = '#ee6655')+
  geom_line(data=larc_landsat_summer, aes(date, TS), colour = '#ee6655')+
  geom_point(data=larc_landsat_summer, aes(date, predicted_temp), colour = '#4488ee')+
  geom_line(data=larc_landsat_summer, aes(date, predicted_temp), colour = '#4488ee')+
  geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), alpha = 0.2)+
  theme_classic()+
  xlab('Date')+
  ylab('Temperature')

larc$year<-as.character(larc$year)
larc$month<-as.character(larc$month)
larc_landsat_area<-left_join(landsat57, larc, by=join_by(year, month))
larc_landsat_area$TS<-as.numeric(larc_landsat_area$TS)

larc_landsat_area_summer<-larc_landsat_area
larc_landsat_area_summer$TS[!str_detect(larc_landsat_area_summer$month, '06|07|08|09|10')]<-NA

#Meteorological data####
#Precipitation
meteo_prectotcorr<-gsheet2tbl('https://docs.google.com/spreadsheets/d/15hyMHrjEkowzHuhVVMfAU_PwPCB7PmadSsEqakbW02w/edit?gid=1410359462#gid=1410359462')
prectotcorr_cor<-cor(na.omit(meteo_prectotcorr[,3:6]))
corrplot(prectotcorr_cor)
meteo_prectotcorr$year<-as.character(meteo_prectotcorr$year)
meteo_prectotcorr$month<-meteo_prectotcorr$month %>%
  str_replace('^1$', '01') %>%
  str_replace('^2$', '02') %>%
  str_replace('^3$', '03') %>%
  str_replace('^4$', '04') %>%
  str_replace('^5$', '05') %>%
  str_replace('^6$', '06') %>%
  str_replace('^7$', '07') %>%
  str_replace('^8$', '08') %>%
  str_replace('^9$', '09')
meteo_larc_prectotcorr<-left_join(meteo_prectotcorr, larc, by = join_by(year, month))
meteo_larc_prectotcorr_cor<-cor(na.omit(meteo_larc_prectotcorr[, c(3:6, 19)]))
corrplot(meteo_larc_prectotcorr_cor)

#| In the initial linear model we included the data 
#| from all stations without interactions
#| adjusted R-squared = 0.6313
#| AIC = 292.31
#| Effects of precipitation measurements in Icha and Dolnovka
#| we non-significant
model<-lm(PRECTOTCORR~icha+esso+dolinovka+sobolevo, data=meteo_larc_prectotcorr)

#| Removing non-significant effects improved adjusted R-squared slightly
#| but AIC dropped down
#| adjusted R-squared = 0.6376
#| AIC = 295.897
model<-lm(PRECTOTCORR~esso+sobolevo, data=meteo_larc_prectotcorr)

#Temperature
meteo_ts<-gsheet2tbl('https://docs.google.com/spreadsheets/d/15hyMHrjEkowzHuhVVMfAU_PwPCB7PmadSsEqakbW02w/edit?gid=1634183496#gid=1634183496')
ts_cor<-cor(na.omit(meteo_ts[,3:6]))
corrplot(ts_cor)
meteo_ts$year<-as.character(meteo_ts$year)
meteo_ts$month<-meteo_ts$month %>%
  str_replace('^1$', '01') %>%
  str_replace('^2$', '02') %>%
  str_replace('^3$', '03') %>%
  str_replace('^4$', '04') %>%
  str_replace('^5$', '05') %>%
  str_replace('^6$', '06') %>%
  str_replace('^7$', '07') %>%
  str_replace('^8$', '08') %>%
  str_replace('^9$', '09')
meteo_larc_ts<-left_join(meteo_ts, larc, by = join_by(year, month))
meteo_larc_ts_cor<-cor(na.omit(meteo_larc_ts[, c(3:6, 8)]))
corrplot(meteo_larc_ts_cor)

#| Simple linear model coordinated LARC temperature data
#| with the meteorological stations temperature date well
#| adjusted R-squared = 0.9862
model<-lm(TS~esso+sobolevo, data=meteo_larc_ts) # R2 = .9858. Nice

#Predict GWETTOP####
#| As far as LARC GWETTOP parameter was included in the 
#| model predicting the lake area from LARC data,
#| but was unavailable from meteorological stations data,
#| we searched for combinations of meteorological parameters
#| which predict LARC GWETTOP well enough

df<-gsheet2tbl('https://docs.google.com/spreadsheets/d/15hyMHrjEkowzHuhVVMfAU_PwPCB7PmadSsEqakbW02w/edit?gid=1048431354#gid=1048431354')
df$station <- df$station %>%
  as.character() %>%
  str_replace('32363', 'esso') %>%
  str_replace('32447', 'dolinovka') %>%
  str_replace('32411', 'icha') %>%
  str_replace('32477', 'sobolevo')

meteo_rel.hum<-pivot_longer(df, 
                            cols = c('01',
                                     '02',
                                     '03',
                                     '04',
                                     '05',
                                     '06',
                                     '07',
                                     '08',
                                     '09',
                                     '10',
                                     '11',
                                     '12'),
                            names_to = 'month', values_to = 'RH')

meteo_rel.hum<-spread(meteo_rel.hum, key = station, value = RH)
df<-subset(climat, param == 'RH2M')
df$param<-NULL
df$anual<-NULL
larc_rel.hum<-pivot_longer(df, 
                           cols = c('01',
                                    '02',
                                    '03',
                                    '04',
                                    '05',
                                    '06',
                                    '07',
                                    '08',
                                    '09',
                                    '10',
                                    '11',
                                    '12'),
                           names_to = 'month', values_to = 'RH_larc')

rel.hum<-left_join(meteo_rel.hum, larc_rel.hum,
                   by = join_by(year, month))
rel.hum$RH_larc<-as.numeric(rel.hum$RH_larc)

meteo_prectotcorr$year<-as.numeric(meteo_prectotcorr$year)
meteo_ts$year<-as.numeric(meteo_ts$year)

rel.hum1<-left_join(rel.hum, meteo_prectotcorr,
                    by = join_by(year, month))
rel.hum1<-left_join(rel.hum1, meteo_ts,
                    by = join_by(year, month))
rel.hum1$RH_larc<-NULL
colnames(rel.hum1)<-c('year',
                      'month',
                      'dolinovka.rh',
                      'esso.rh',
                      'icha.rh',
                      'sobolevo.rh',
                      'icha.prec',
                      'esso.prec',
                      'dolinovka.prec',
                      'sobolevo.prec',
                      'icha.ts',
                      'esso.ts',
                      'dolinovka.ts',
                      'sobolevo.ts')

df<-subset(climat, param == 'GWETTOP')
df$param<-NULL
df$anual<-NULL
larc_gwettop<-pivot_longer(df, 
                           cols = c('01',
                                    '02',
                                    '03',
                                    '04',
                                    '05',
                                    '06',
                                    '07',
                                    '08',
                                    '09',
                                    '10',
                                    '11',
                                    '12'),
                           names_to = 'month', values_to = 'GWETTOP')

rel.hum<-left_join(rel.hum, larc_gwettop,
                   by = join_by(year, month))
rel.hum$GWETTOP<-as.numeric(rel.hum$GWETTOP)

rel.hum1<-left_join(rel.hum1, larc_gwettop,
                   by = join_by(year, month))
rel.hum1$GWETTOP<-as.numeric(rel.hum1$GWETTOP)

GWETTOP_shift1up<-rel.hum1[c(1,2, length(rel.hum1))]
for(i in 1:nrow(GWETTOP_shift1up)){
  GWETTOP_shift1up$year[i]<-modify_date_up(n=1,
                                                 year = GWETTOP_shift1up$year[i],
                                                 month = GWETTOP_shift1up$month[i])[1]
  GWETTOP_shift1up$month[i]<-modify_date_up(n=1,
                                                  year = GWETTOP_shift1up$year[i],
                                                  month = GWETTOP_shift1up$month[i])[2]
}
GWETTOP_shift1up$year<-as.numeric(GWETTOP_shift1up$year)

rel.hum2<-left_join(rel.hum1, GWETTOP_shift1up,
                    by = join_by(year, month))
rel.hum1$year<-NULL
rel.hum1$month<-NULL

rel.hum2$year<-NULL
rel.hum2$month<-NULL

#| model shows week but significant relation between
#| LARC GWETTOP and relative humidity measured with
#| meteorological stations
#| R-squared = 0.12

model<-lm(GWETTOP~., data = rel.hum1) 
a<-predict(model, newdata = rel.hum1)
b<-data.frame(real = rel.hum1$GWETTOP, pred = a)
b<-na.omit(b)
ggplot(b, aes(real, pred))+
  geom_point(colour = '#4488ee', size = 3)+
  geom_smooth(colour = '#4488ee', se = F, method = 'lm')+
  theme_classic()+
  xlab('LARC GWETTOP')+
  ylab('Predicted GWETTOP')

#| When we shifted predictors one month back
#| R-squared increased to 0.31
model<-lm(GWETTOP.y~., data = rel.hum2) 
a<-predict(model, newdata = rel.hum2)
b<-data.frame(real = rel.hum2$GWETTOP.y, pred = a)
b<-na.omit(b)
ggplot(b, aes(real, pred))+
  geom_point(colour = '#4488ee', size = 3)+
  geom_smooth(colour = '#4488ee', se = F, method = 'lm')+
  theme_classic()+
  xlab('LARC GWETTOP')+
  ylab('Predicted GWETTOP')

#Predict the lake area: data preparation####
RH<-rel.hum[,1:6]
colnames(RH)<-c("year", "month",
                       "dolinovka.RH",
                       "esso.RH",
                       "icha.RH",
                       "sobolevo.RH")
RH_shift1<-RH
for(i in 1:nrow(RH_shift1)){
  RH_shift1$year[i]<-modify_date_up(n=1,
                                  year = RH_shift1$year[i],
                                  month = RH_shift1$month[i])[1]
  RH_shift1$month[i]<-modify_date_up(n=1,
                                   year = RH_shift1$year[i],
                                   month = RH_shift1$month[i])[2]
}
colnames(RH_shift1)<-c("year", "month",
                "dolinovka.RH_shift1",
                "esso.RH_shift1",
                "icha.RH_shift1",
                "sobolevo.RH_shift1")
RH_shift1$year<-as.numeric(RH_shift1$year)


TS<-meteo_ts
colnames(TS)<-c("year", "month",
                "icha.TS",
                "esso.TS",
                "dolinovka.TS",
                "sobolevo.TS")

TS_shift1<-TS
for(i in 1:nrow(TS_shift1)){
  TS_shift1$year[i]<-modify_date_up(n=1,
                                    year = TS_shift1$year[i],
                                    month = TS_shift1$month[i])[1]
  TS_shift1$month[i]<-modify_date_up(n=1,
                                     year = TS_shift1$year[i],
                                     month = TS_shift1$month[i])[2]
}
colnames(TS_shift1)<-c("year", "month",
                "icha.TS_shift1",
                "esso.TS_shift1",
                "dolinovka.TS_shift1",
                "sobolevo.TS_shift1")
TS_shift1$year<-as.numeric(TS_shift1$year)

PRECTOTCORR_SUM<-meteo_prectotcorr
colnames(PRECTOTCORR_SUM)<-c("year", "month",
                                    "icha.PRECTOTCORR_SUM",
                                    "esso.PRECTOTCORR_SUM",
                                    "dolinovka.PRECTOTCORR_SUM",
                                    "sobolevo.PRECTOTCORR_SUM")

PRECTOTCORR_SUM_shift1<-PRECTOTCORR_SUM
for(i in 1:nrow(PRECTOTCORR_SUM_shift1)){
  PRECTOTCORR_SUM_shift1$year[i]<-modify_date_up(n=1,
                                                 year = PRECTOTCORR_SUM_shift1$year[i],
                                                 month = PRECTOTCORR_SUM_shift1$month[i])[1]
  PRECTOTCORR_SUM_shift1$month[i]<-modify_date_up(n=1,
                                                  year = PRECTOTCORR_SUM_shift1$year[i],
                                                  month = PRECTOTCORR_SUM_shift1$month[i])[2]
}
colnames(PRECTOTCORR_SUM_shift1)<-c("year", "month",
                             "icha.PRECTOTCORR_SUM_shift1",
                             "esso.PRECTOTCORR_SUM_shift1",
                             "dolinovka.PRECTOTCORR_SUM_shift1",
                             "sobolevo.PRECTOTCORR_SUM_shift1")
PRECTOTCORR_SUM_shift1$year<-as.numeric(PRECTOTCORR_SUM_shift1$year)

dec_prec<-meteo_prectotcorr
dec_prec$icha<-NA
dec_prec$dolinovka<-NA
dec_prec$esso<-NA
dec_prec$sobolevo<-NA
dec_prec$year<-as.numeric(dec_prec$year)

for(i in 1:nrow(dec_prec)){
  yr<-dec_prec$year[i]-1
  if(length(subset(meteo_prectotcorr, year == yr & month == '12')$icha)>0){
    dec_prec$icha[i]<-subset(meteo_prectotcorr, year == yr & month == '12')$icha
  }
  if(length(subset(meteo_prectotcorr, year == yr & month == '12')$dolinovka)>0){
    dec_prec$dolinovka[i]<-subset(meteo_prectotcorr, year == yr & month == '12')$dolinovka
  }
  if(length(subset(meteo_prectotcorr, year == yr & month == '12')$esso)>0){
    dec_prec$esso[i]<-subset(meteo_prectotcorr, year == yr & month == '12')$esso
  }
  if(length(subset(meteo_prectotcorr, year == yr & month == '12')$sobolevo)>0){
    dec_prec$sobolevo[i]<-subset(meteo_prectotcorr, year == yr & month == '12')$sobolevo
  }
}
colnames(dec_prec)<-c("year", "month",
                      "icha.dec_prec",
                      "esso.dec_prec",
                      "dolinovka.dec_prec",
                      "sobolevo.dec_prec")

TS$year<-as.numeric(TS$year)
PRECTOTCORR_SUM$year<-as.numeric(PRECTOTCORR_SUM$year)

predict_df<-full_join(RH, TS,
                      by = c('year', 'month'))
#predict_df<-full_join(predict_df, PRECTOTCORR_SUM_shift1,
#                      by = c('year', 'month'))
predict_df<-full_join(predict_df, dec_prec,
                      by = c('year', 'month'))
#predict_df<-full_join(predict_df, RH_shift1,
#                      by = c('year', 'month'))
predict_df<-full_join(predict_df, PRECTOTCORR_SUM,
                      by = c('year', 'month'))

area$year<-as.numeric(area$year)
predict_area<-right_join(predict_df, area, by=c('year', 'month'))

#Model 2.1####
data<-na.omit(predict_area)
data<-data[, -c(1,2, 19, 21, 22)]

model<-lm(area~.,
          data = data,
          na.action = 'na.fail')
best_model<-dredge(model)

#| Model with the lowest AIC accoding to MuMIn::dredge had
#| an AIC = 1565
#| and included TS in Dolinovka and Sobolevo as well as
#| precipitation in Sobolevo as predictors
#| adjusted R-squared was 0.44 for this model
model<-lm(area~
            dolinovka.TS+
            sobolevo.dec_prec+
            sobolevo.TS,
          data = data,
          na.action = 'na.fail')

ggplot(winters)+
  geom_line(data=predict_area, aes(date, area), colour = '#ee7766',
            linewidth = 1,
            alpha = .7)+
  geom_point(data=predict_area, aes(date, area), fill = '#ee7766',
             shape = 21,
             size = 2.5)+
  geom_line(data=b, aes(predict_area.date, a), colour = '#2266ee',
            linewidth = 1,
            alpha = .9)+
  geom_point(data=b, aes(predict_area.date, a), fill = '#2266ee',
             shape = 21,
             size = 2.5)+
  theme_classic()+
  xlim(as.Date('1988-01-01'), as.Date('2024-12-31'))+
  xlab('Date')+
  ylab('Area')

#Model 2.2####
#| In order to improve the performance of the model
#| we applied the architecture without the intercept
model<-lm(area~0+.,
          data = data,
          na.action = 'na.fail')
best_model<-dredge(model)


#| Model with the lowest AIC accoding to MuMIn::dredge had
#| an AIC = 1564
#| and adjusted R-squared = 0.99
model<-lm(area~0+
            dolinovka.TS+
            icha.RH+
            sobolevo.dec_prec+
            sobolevo.TS,
          data = data,
          na.action = 'na.fail')

a<-predict(model, newdata = predict_area)
b<-data.frame(predict_area$date, a)

ggplot(winters)+
  geom_line(data=predict_area, aes(date, area), colour = '#ee7766',
            linewidth = 1,
            alpha = .7)+
  geom_point(data=predict_area, aes(date, area), fill = '#ee7766',
             shape = 21,
             size = 2.5)+
  geom_line(data=b, aes(predict_area.date, a), colour = '#2266ee',
            linewidth = 1,
            alpha = .9)+
  geom_point(data=b, aes(predict_area.date, a), fill = '#2266ee',
             shape = 21,
             size = 2.5)+
  theme_classic()+
  xlim(as.Date('1988-01-01'), as.Date('2024-12-31'))+
  xlab('Date')+
  ylab('Area')

#Model 2.3####
#| We further used meteorological data shifted one month back
#| to predict the lake area, as GWETTOP correlated better
#| with recent rather than current climatology data
PRECTOTCORR_SUM_shift1$year<-as.numeric(PRECTOTCORR_SUM_shift1$year)

predict_df_shift1<-full_join(RH_shift1, TS_shift1,
                      by = c('year', 'month'))
predict_df_shift1<-full_join(predict_df_shift1, PRECTOTCORR_SUM_shift1,
                      by = c('year', 'month'))
predict_df_shift1<-full_join(predict_df_shift1, dec_prec,
                      by = c('year', 'month'))


area$year<-as.numeric(area$year)
predict_area<-right_join(predict_df_shift1, area, by=c('year', 'month'))


data<-na.omit(predict_area)
data<-data[, -c(1,2, 19, 21, 22)]

model<-lm(area~.,
          data = data,
          na.action = 'na.fail')
best_model<-dredge(model)

#| Model with the lowest AIC accoding to MuMIn::dredge had
#| an AIC = 1602
#| and adjusted R-squared = 0.5033
model<-lm(area~
            dolinovka.TS_shift1+
            esso.TS_shift1+
            icha.TS_shift1+
            icha.RH_shift1+
            sobolevo.dec_prec,
          data = data,
          na.action = 'na.fail')

a<-predict(model, newdata = predict_area)
b<-data.frame(predict_area$date, a)

ggplot(winters)+
  geom_line(data=predict_area, aes(date, area), colour = '#ee7766',
            linewidth = 1,
            alpha = .7)+
  geom_point(data=predict_area, aes(date, area), fill = '#ee7766',
             shape = 21,
             size = 2.5)+
  geom_line(data=b, aes(predict_area.date, a), colour = '#2266ee',
            linewidth = 1,
            alpha = .9)+
  geom_point(data=b, aes(predict_area.date, a), fill = '#2266ee',
             shape = 21,
             size = 2.5)+
  theme_classic()+
  xlim(as.Date('1988-01-01'), as.Date('2024-12-31'))+
  xlab('Date')+
  ylab('Area')

#Model 2.4####
#| Model including only significant or almost significant effects 
#| from the previous model
#| AIC = 1599
#| adjusted R-squared = 0.99
model<-lm(area~0+
            dolinovka.TS_shift1+
            esso.TS_shift1+
            icha.RH_shift1+
            sobolevo.dec_prec,
          data = data,
          na.action = 'na.fail')

a<-predict(model, newdata = predict_area)
b<-data.frame(predict_area$date, a)

ggplot(winters)+
  geom_line(data=predict_area, aes(date, area), colour = '#ee7766',
            linewidth = 1,
            alpha = .7)+
  geom_point(data=predict_area, aes(date, area), fill = '#ee7766',
             shape = 21,
             size = 2.5)+
  geom_line(data=b, aes(predict_area.date, a), colour = '#2266ee',
            linewidth = 1,
            alpha = .9)+
  geom_point(data=b, aes(predict_area.date, a), fill = '#2266ee',
             shape = 21,
             size = 2.5)+
  theme_classic()+
  xlim(as.Date('1988-01-01'), as.Date('2024-12-31'))+
  xlab('Date')+
  ylab('Area')

#Model 2.5####
#| Finally, we mixed predictors of the best model based on current data
#| and the data of the previous month
predict_area<-left_join(predict_df, predict_df_shift1,
                        by = c('year', 'month'))
predict_area<-right_join(predict_area, area, by=c('year', 'month'))
data<-na.omit(predict_area)
data<-data[, -c(1,2, 35, 37, 38)]

#| adjusted R-squared = 0.9872
#| AIC = 1560.58
model<-lm(area~0+dolinovka.TS+
            icha.RH+
            sobolevo.dec_prec.x+
            sobolevo.TS+
            dolinovka.TS_shift1+
            esso.TS_shift1+
            icha.RH_shift1,
          data = data,
          na.action = 'na.fail')

a<-predict(model, newdata = predict_area)
b<-data.frame(predict_area$date, a)

ggplot(winters)+
  geom_line(data=predict_area, aes(date, area), colour = '#ee7766',
            linewidth = 1,
            alpha = .7)+
  geom_point(data=predict_area, aes(date, area), fill = '#ee7766',
             shape = 21,
             size = 2.5)+
  geom_line(data=b, aes(predict_area.date, a), colour = '#2266ee',
            linewidth = 1,
            alpha = .9)+
  geom_point(data=b, aes(predict_area.date, a), fill = '#2266ee',
             shape = 21,
             size = 2.5)+
  theme_classic()+
  xlim(as.Date('1988-01-01'), as.Date('2024-12-31'))+
  xlab('Date')+
  ylab('Area')

#Model 2.6####
#| Only significant and almost significant effects
#| from the previous model
#| adjusted R-squared = 0.9868
#| AIC = 1559
model<-lm(area~0+
            sobolevo.dec_prec.x+
            esso.TS_shift1+
            icha.RH_shift1,
          data = data,
          na.action = 'na.fail')

a<-predict(model, newdata = predict_area)
b<-data.frame(predict_area$date, a)

ggplot(winters)+
  geom_line(data=predict_area, aes(date, area), colour = '#ee7766',
            linewidth = 1,
            alpha = .7)+
  geom_point(data=predict_area, aes(date, area), fill = '#ee7766',
             shape = 21,
             size = 2.5)+
  geom_line(data=b, aes(predict_area.date, a), colour = '#2266ee',
            linewidth = 1,
            alpha = .9)+
  geom_point(data=b, aes(predict_area.date, a), fill = '#2266ee',
             shape = 21,
             size = 2.5)+
  theme_classic()+
  xlim(as.Date('1988-01-01'), as.Date('2024-12-31'))+
  xlab('Date')+
  ylab('Area')

