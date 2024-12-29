modify_date<-function(n, year, month){
  year_temp<-as.numeric(year)
  month_temp<-str_remove(month, '^0')
  month_temp<-as.numeric(month_temp)
  
  if(month_temp > n){
    month_temp<-month_temp-n
    if(month_temp < 10){
      month_temp<-paste0('0', as.character(month_temp))
    }
    month_temp<-as.character(month_temp)
  }
  else{
    dif<-n-month_temp
    month_temp<-12-dif
    if(month_temp < 10){
      month_temp<-paste0('0', as.character(month_temp))
    }
    month_temp<-as.character(month_temp)
    year_temp<-year_temp-1
  }
  year_temp<-as.character(year_temp)
  return(c(year_temp, month_temp))
}