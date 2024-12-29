#Packages####
library(tidyverse)
library(gsheet)
library(MuMIn)
library(corrplot)
#Data upload####
climat<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1vPUWXa1ZdKpTTTWg_HGvypZjArvRPSfObzt0sv3WyWw/edit?gid=0#gid=0')

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

#Data preparation####
#Add '_shift1' predictors and n'

df_shift1<-matrix(ncol=length(unique(climat$param)),
                  nrow=nrow(area))
colnames(df_shift1)<-unique(climat$param)

for(i in 1:nrow(df_shift1)){
  for(j in 1:ncol(df_shift1)){
    yr<-area$year[i]
    month<-area$month[i]
    yr<-modify_date(1, yr, month)[1]
    month<-modify_date(1, yr, month)[2]
    par<-colnames(df_shift1)[j]
    temp<-subset(climat, year==yr & param==par)
    temp<-temp[[month]][1]
    df_shift1[i,j]<-as.numeric(temp)
  }
}
colnames(df_shift1)<-paste0(colnames(df_shift1), '_shift1')

big_df<-cbind(df, df_shift1)
big_df<-cbind(big_df, area[, c(3, 2, 1)])
big_df<-as.data.frame(big_df) %>%
  column_to_rownames('date')
big_df<-na.omit(big_df)
big_df$sin_n_days<-abs(sin(big_df$n_days*pi/365.25))

#Add winter precipitation data
big_df$year<-str_sub(rownames(big_df), 1, 4)
big_df$year<-as.numeric(big_df$year)

nov_prec<-c()
dec_prec<-c()
jan_prec<-c()
feb_prec<-c()
mar_prec<-c()
apr_prec<-c()
may_prec<-c()

for(i in 1:nrow(big_df)){
  temp<-subset(climat, year == big_df$year[i]-1 & param == 'PRECTOTCORR_SUM')
  temp<-temp$`11`
  nov_prec<-append(nov_prec, temp)
  
  temp<-subset(climat, year == big_df$year[i]-1 & param == 'PRECTOTCORR_SUM')
  temp<-temp$`12`
  dec_prec<-append(dec_prec, temp)
  
  temp<-subset(climat, year == big_df$year[i] & param == 'PRECTOTCORR_SUM')
  temp<-temp$`01`
  jan_prec<-append(jan_prec, temp)
  
  temp<-subset(climat, year == big_df$year[i] & param == 'PRECTOTCORR_SUM')
  temp<-temp$`02`
  feb_prec<-append(feb_prec, temp)
  
  temp<-subset(climat, year == big_df$year[i] & param == 'PRECTOTCORR_SUM')
  temp<-temp$`03`
  mar_prec<-append(mar_prec, temp)
  
  temp<-subset(climat, year == big_df$year[i] & param == 'PRECTOTCORR_SUM')
  temp<-temp$`04`
  apr_prec<-append(apr_prec, temp)
  
  temp<-subset(climat, year == big_df$year[i] & param == 'PRECTOTCORR_SUM')
  temp<-temp$`05`
  may_prec<-append(may_prec, temp)
}

big_df$nov_prec<-nov_prec
big_df$dec_prec<-dec_prec
big_df$jan_prec<-jan_prec
big_df$feb_prec<-feb_prec
big_df$mar_prec<-mar_prec
big_df$apr_prec<-apr_prec
big_df$may_prec<-may_prec

big_df$nov_prec<-as.numeric(big_df$nov_prec)
big_df$dec_prec<-as.numeric(big_df$dec_prec)
big_df$jan_prec<-as.numeric(big_df$jan_prec)
big_df$feb_prec<-as.numeric(big_df$feb_prec)
big_df$mar_prec<-as.numeric(big_df$mar_prec)
big_df$apr_prec<-as.numeric(big_df$apr_prec)
big_df$may_prec<-as.numeric(big_df$may_prec)

a<-colnames(big_df)[c(1:32, 35, 37:43)]
b<-c()
for(i in a){
  temp<-cor(big_df[,i], big_df$area)
  b<-append(b, temp)
}
cor_df<-data.frame(a,b)
good_predictors<-subset(cor_df, abs(b)>=0.35)$a

predictors<-big_df[, colnames(big_df) %in% good_predictors]
predictors<-as.matrix(predictors)
a<-cor(predictors)
b<-a
b[abs(b)<.95]<-NA
colnames(a)<-c(1:ncol(a))
rownames(a)<-c(1:nrow(a))
corrplot(a)

predict_df<-big_df[, colnames(big_df) %in% good_predictors]
predict_df<-predict_df[,-c(3, 4, 10, 7, 13, 14, 22, 19)]
predict_df$area<-big_df$area
#Model 1####
#| Includes all predictors except highly correlated with other predictors
#| and purely correlated with the response variable
#| AIC = 1887.164
#| adjusted R-squared = 0.536
model<-lm(area~., data = predict_df, na.action = 'na.fail') #AIC 1887
a<-predict(model, newdata = big_df)
b<-data.frame(rownames(big_df), a)
b$rownames.big_df.<-as.Date(b$rownames.big_df.)

big_df$date<-rownames(big_df)
big_df$date<-as.Date(big_df$date)

ggplot()+
  geom_line(data=big_df, aes(date, area), colour = '#ee7766',
            linewidth = 1,
            alpha = .7)+
  geom_point(data=big_df, aes(date, area), fill = '#ee7766',
             shape = 21,
             size = 2.5)+
  geom_line(data=b, aes(rownames.big_df., a), colour = '#2266ee',
            linewidth = 1,
            alpha = .9)+
  geom_point(data=b, aes(rownames.big_df., a), fill = '#2266ee',
             shape = 21,
             size = 2.5)+
  theme_classic()+
  xlim(as.Date('1988-01-01'), as.Date('2024-12-31'))+
  xlab('Date')+
  ylab('Area')

#Model 2####
#| Includes all variables of Model 1
#| which are included into Best-AIC model as estimated with MuMin::dredge
#| AIC = 1870.153
#| adjusted R-squared = 0.5829
model<-lm(area~dec_prec+
            GWETTOP+
            PRECTOTCORR+
            PRECTOTCORR_SUM_shift1+
            TS+
            TS_shift1, data = predict_df, na.action = 'na.fail') # AIC 1870.153

a<-predict(model, newdata = big_df)
b<-data.frame(rownames(big_df), a)
b$rownames.big_df.<-as.Date(b$rownames.big_df.)

big_df$date<-rownames(big_df)
big_df$date<-as.Date(big_df$date)

ggplot()+
  geom_line(data=big_df, aes(date, area), colour = '#ee7766',
            linewidth = 1,
            alpha = .7)+
  geom_point(data=big_df, aes(date, area), fill = '#ee7766',
             shape = 21,
             size = 2.5)+
  geom_line(data=b, aes(rownames.big_df., a), colour = '#2266ee',
            linewidth = 1,
            alpha = .9)+
  geom_point(data=b, aes(rownames.big_df., a), fill = '#2266ee',
             shape = 21,
             size = 2.5)+
  theme_classic()+
  xlim(as.Date('1988-01-01'), as.Date('2024-12-31'))+
  xlab('Date')+
  ylab('Area')

#Model 3####
#| Includes all variables of Model 2
#| But also takes their interactions into account
#| AIC = 1877.069
#| adjusted R-squared = 0.6066
model<-lm(area~(dec_prec+
                  GWETTOP+
                  PRECTOTCORR+
                  PRECTOTCORR_SUM_shift1+
                  TS+
                  TS_shift1)^2, data = predict_df, na.action = 'na.fail')
a<-predict(model, newdata = big_df)
b<-data.frame(rownames(big_df), a)
b$rownames.big_df.<-as.Date(b$rownames.big_df.)

big_df$date<-rownames(big_df)
big_df$date<-as.Date(big_df$date)

ggplot()+
  geom_line(data=big_df, aes(date, area), colour = '#ee7766',
            linewidth = 1,
            alpha = .7)+
  geom_point(data=big_df, aes(date, area), fill = '#ee7766',
             shape = 21,
             size = 2.5)+
  geom_line(data=b, aes(rownames.big_df., a), colour = '#2266ee',
            linewidth = 1,
            alpha = .9)+
  geom_point(data=b, aes(rownames.big_df., a), fill = '#2266ee',
             shape = 21,
             size = 2.5)+
  theme_classic()+
  xlim(as.Date('1988-01-01'), as.Date('2024-12-31'))+
  xlab('Date')+
  ylab('Area')
#Model 4####
#| Represents the best-AIC model with model 3 architecture
#| as estimated with MuMin::dredge
#| AIC = 1866.58
#| adjusted R-squared = 0.6141
model<-lm(area~dec_prec+
                      GWETTOP+
                      PRECTOTCORR+
                      PRECTOTCORR_SUM_shift1+
                      TS+
                      dec_prec:PRECTOTCORR+
                      PRECTOTCORR:TS,
                      data = predict_df, na.action = 'na.fail')

a<-predict(model, newdata = big_df)
b<-data.frame(rownames(big_df), a)
b$rownames.big_df.<-as.Date(b$rownames.big_df.)

big_df$date<-rownames(big_df)
big_df$date<-as.Date(big_df$date)

ggplot()+
  geom_line(data=big_df, aes(date, area), colour = '#ee7766',
            linewidth = 1,
            alpha = .7)+
  geom_point(data=big_df, aes(date, area), fill = '#ee7766',
             shape = 21,
             size = 2.5)+
  geom_line(data=b, aes(rownames.big_df., a), colour = '#2266ee',
            linewidth = 1,
            alpha = .9)+
  geom_point(data=b, aes(rownames.big_df., a), fill = '#2266ee',
             shape = 21,
             size = 2.5)+
  theme_classic()+
  xlim(as.Date('1988-01-01'), as.Date('2024-12-31'))+
  xlab('Date')+
  ylab('Area')
#Model 5####
#| Includes significant effects of Model 4
#| AIC = 1863
#| adjusted R-squared = 0.6141
#| This is the best model
model<-lm(area~
            GWETTOP+
            PRECTOTCORR_SUM_shift1+
            TS+
            dec_prec:PRECTOTCORR+
            PRECTOTCORR:TS,
          data = predict_df, na.action = 'na.fail')

a<-predict(model, newdata = big_df)
b<-data.frame(rownames(big_df), a)
b$rownames.big_df.<-as.Date(b$rownames.big_df.)

big_df$date<-rownames(big_df)
big_df$date<-as.Date(big_df$date)

ggplot()+
  geom_line(data=big_df, aes(date, area), colour = '#ee7766',
            linewidth = 1,
            alpha = .7)+
  geom_point(data=big_df, aes(date, area), fill = '#ee7766',
             shape = 21,
             size = 2.5)+
  geom_line(data=b, aes(rownames.big_df., a), colour = '#2266ee',
            linewidth = 1,
            alpha = .9)+
  geom_point(data=b, aes(rownames.big_df., a), fill = '#2266ee',
             shape = 21,
             size = 2.5)+
  theme_classic()+
  xlim(as.Date('1988-01-01'), as.Date('2024-12-31'))+
  xlab('Date')+
  ylab('Area')
#Model 6####
#| Includes significant effects of Model 2
#| ACC = 1868.55
#| adjusted R-squared = 0.5817
model<-lm(area~
            GWETTOP+
            PRECTOTCORR_SUM_shift1+
            TS+
            dec_prec,
          data = big_df, na.action = 'na.fail') # AIC 1868.55
a<-predict(model, newdata = big_df)
b<-data.frame(rownames(big_df), a)
b$rownames.big_df.<-as.Date(b$rownames.big_df.)

big_df$date<-rownames(big_df)
big_df$date<-as.Date(big_df$date)

ggplot()+
  geom_line(data=big_df, aes(date, area), colour = '#ee7766',
            linewidth = 1,
            alpha = .7)+
  geom_point(data=big_df, aes(date, area), fill = '#ee7766',
             shape = 21,
             size = 2.5)+
  geom_line(data=b, aes(rownames.big_df., a), colour = '#2266ee',
            linewidth = 1,
            alpha = .9)+
  geom_point(data=b, aes(rownames.big_df., a), fill = '#2266ee',
             shape = 21,
             size = 2.5)+
  theme_classic()+
  xlim(as.Date('1988-01-01'), as.Date('2024-12-31'))+
  xlab('Date')+
  ylab('Area')