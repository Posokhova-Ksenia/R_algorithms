fi<-function(i){
  if(i<10){
      i= paste0("00",i)
  }else{
    if(i<100){
      i=paste0("0",i)}
  }
 return(i) 
}

#функция, необохдимая для создания корректного пути к файлу, считывания, очистки от NA

getmonitor <- function (folder,id,summar=F) {
  
  for(i in 1: length(id)){
    id<-id[i]
    if((id*10)%%10!=0){
      stop('id должен быть целым числом')
    }
    if((id<=0) | (id>332)){
      stop("введите верный id")
    }
    if(!is.logical (summar)){
      stop('summar не является логической переменной')
    }
    x<-read.csv(paste0(folder,fi(id), ".csv"))  #корректный путь/считывание
  }
  x$Date <- as.Date(x$Date)
  l1<-apply(!is.na(x[c("sulfate",'nitrate')]),1,any)    #apply это скрытый цикл который выделяет одну строку и в ней выполняет функцию
  x<-x[l1,]
  x$year<-format(x$Date,"%Y")    #добавляем столбцы года и месяца
  x$month<-format(x$Date,"%m")
   if(summar==T){                #данные для печати
     summary<-summary(x)
     print(summary)
   }
  
 return(x) 
}
folder<-'E:/datchiki/specdata/'
func1<-getmonitor(folder,1,T)

library(dplyr)

getseasons<-function(id, folder, factor, byid=F){
  x<-data.frame()
  for(i in 1: length(id)){
  if(is.na(id[i])){
    stop("введите верный id")
   }  
  }
  if(!is.numeric(id)){
    if(length(id)>1){
      stop("id должен быть числовой или ALL")
    }else{
      if(id=="ALL"){
        id<-c(1:332)
      }else{
        stop("id должна быть числовой или ALL")
      }
    }
    
  } else{
    if((id[i]*10)%%10!=0){
      stop('id должен быть целым числом')
    }
  }
  if(length(id)>1){
    if(sum(duplicated(id))!=0){
      stop("повтор значений id")
    }
  }
  if(!is.logical(byid)){
    stop('byid должна быть логической переменной')
  }
  for(i in 1: length(id)){
    x<-rbind(x,getmonitor(folder,id[i], summar=F)) #склейка 
  }
  
  if(factor=="sulfate"){
  x$factor<-x$sulfate
  }else{
    x$factor<-x$nitrate
  }
  x$Date <- as.Date(x$Date)
  x$number_m<-as.numeric(format(x$Date, "%m")) #номер месяца
  
  plot(factor~number_m, x, col=c("red","pink",'blue','green'))
  
  if(byid==T){
    summarise<- x %>% group_by(month, ID)%>%
      summarise(factor_m=mean(factor, na.rm=T)) #Если na.rm имеет значение TRUE, функция пропускает все значения NA. 
    
    summarise$number_m<- as.numeric(summarise$month)
     boxplot(factor_m~number_m, summarise, add=T)
    }else{
    summarise<- x %>% group_by(month)%>%
      summarise(factor_m=mean(factor, na.rm=T))
  }
  
  summarise$number_m<- as.numeric(summarise$month)
         lines(factor_m~number_m, summarise, type="l", col="blue", lwd=3)                
           return(summarise)       
         }
folder<-'E:/datchiki/specdata/'
func2<-getseasons(c(1,2,3,4), folder, factor='sulfate',byid=T)

#задача вторая
library(dplyr)
#install.packages("lubridate")
library(lubridate)
#описательная статистика для сульфатов и нитратов 
getStats<-function(id, folder, bymonth=F){
 x<-data.frame() 
 for(i in 1:length(id)){
   if(is.na(id[i])){
     stop("id не может быть NA")
   }
 }
 if(!is.numeric(id)){
   if(length(id)>1){
     stop("id должен быть числовой или ALL")
   }else{
     if(id=="ALL"){
       id<-c(1:332)
     }else{
       stop("id должна быть числовой или ALL")
     }
   }
   
 } else{
   if((id[i]*10)%%10!=0){
     stop('id должен быть целым числом')
   }
 }
 if(length(id)>1){
   if(sum(duplicated(id))!=0){
     stop("повтор значений id")
   }
 }
 if(!is.logical(bymonth)){
   stop('bymonth должна быть логической переменной')
 }
 for(i in 1: length(id)){
   x<-rbind(x,getmonitor(folder,id[i], summar=F))
 } 
   x<- x %>%
     mutate(
       Year_month=format(Date, "%Y.%m") %>% as.numeric,
       MonthEnd= ceiling_date(Date, 'month')- days(1))  #6 апреля-> 1 мая
   if (bymonth==F){
     x<-x%>%
       group_by(ID) %>%
       summarise(
         n_sulfate = sum(!is.na(sulfate)),
         min_sulfate = sulfate %>% min(na.rm = T),
         max_sulfate = sulfate %>% max(na.rm = T),
         mean_sulfate = mean(sulfate, na.rm=T),
         
         n_nitrate = sum(!is.na(nitrate)),
         min_nitrate = nitrate %>% min(na.rm = T),
         max_nitrate=  nitrate %>% max(na.rm = T),
         mean_nitrate = mean(nitrate, na.rm=T),
         
         dateStart= Date %>% min (na.rm=T),
         DateEnd = Date %>% max (na.rm=T)
       )
     
   }else{
     x<-x%>%
       group_by(ID, MonthEnd) %>%
       summarise(
         n_sulfate = sum(!is.na(sulfate)),
         min_sulfate = sulfate %>% min(na.rm = T),
         max_sulfate = sulfate %>% max(na.rm = T),
         mean_sulfate = mean(sulfate, na.rm=T),
         
         n_nitrate = sum(!is.na(nitrate)),
         min_nitrate = nitrate %>% min(na.rm = T),
         max_nitrate=  nitrate %>% max(na.rm = T),
         mean_nitrate = mean(nitrate, na.rm=T))}
   return(x)
 }

 folder<-'E:/datchiki/specdata/'
 Func3<- getStats(5, folder, bymonth = F)
 
 library(dplyr)
 
 getCorr<-function(folder, threshold){
    if(!is.numeric(threshold)){
           stop("threshold должен быть числовым")
    }
   if((threshold*10)%%10!=0){
     stop('threshold должен быть целым числом')
   }
      if(threshold<0){
           stop("threshold должно быть положительным или нулём")
         }
       t<-data.frame()
       id<-c()
       Corr<-c()
       for (i in 1:332){
           t<- rbind(t, getmonitor(folder,i, summar=F ))
         }
x_n<- t %>%
           group_by(ID) %>%
           summarise(n = sum(!is.na(sulfate|nitrate))) #количество наблюдений для каждого датчика
       for(i in 1:332){
           if(x_n$n[i]>threshold){
               if(getStats(i, folder)$n_nitrate==0|getStats(i,folder)$n_sulfate==0){
                   next
                 }
              t<-getmonitor(folder,i,summar=F)
               cor<-cor(t$sulfate,t$nitrate, use="complete.obs")             #использует все доступные наблюдения для создания корреляционной матрицы. Без него корреляции рассчитываются только тогда, когда данные отсутствуют.
               id<-c(id,i)
               Corr<-c(Corr,cor)
               
               }
         }
      x<-data.frame(id, Corr)
      return(x)
    
      }
  
     folder<-'E:/datchiki/specdata/'
 func4<-getCorr(folder, 1000)
