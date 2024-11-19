#задаем функцию и начальную точку
ff<- function(x,y){+x^2+y^2+10}

#f<- function(a){a[1]^2+a[2]^2-10} 
a<-c(-7, -9) #вектор, в котором сначала знач x затем знач у, начальная точка





HookeJeeves<-function(a,f,h,mx){
  eps<- 0.01 #погрешность
  ex<-c(1,0) #сдвиг по х
  ey<-c(0,1) #сдвиг по у
  #находим значения
  x<- seq(-20,20,1)
  y<-seq(-20,20,1)
  z<- outer(x,y,ff)
  s<-0
  
  #строим графики
  par(mfcol=c(1,2))
  persp(x,y,z) #трехмерная функция
  contour(x,y,z,nlevels=20) # как выглядит на плоскости
  points(a[1], a[2], col="black", pch=20, cex=2) #рисуем точку
  
  a1<-a
  if(mx==T){
    while(h>eps){
      if(f(a1+h*ex)>f(a1)){
        a1<-a+h*ex
        s<- s+1
      }
      if(f(a1-h*ex)>f(a1)){
        a1<-a-h*ex
        s<- s+1
      }
      if(f(a1+h*ey)>f(a1)){
        a1<-a+h*ey
        s<- s+1
      }
      if(f(a1-h*ey)>f(a1)){
        a1<-a-h*ey
        s<- s+1
      }
      
      
      if(any(a1 != a)){                           #нашли ли мы точку в которую можем сдвинуться
        segments(a[1], a[2], a1[1],a1[2], col="green")         #рисует линию
        points(a1[1],a1[2], col="brown", pch=20)
        while(f(2*a1-a)>f(a1)){
          a<-a1
          s<- s+1
          a1<-2*a1-a
          segments(a[1], a[2], a1[1],a1[2])
          points(a1[1],a1[2], col="blue", pch=20)
        }
      }else{
        h<-h/2
      }
      a<-a1
     
      
    }
  }else{
    while(h>eps){
      if(f(a1+h*ex)<f(a1)){
        a1<-a+h*ex
        s<- s+1
      }
      if(f(a1-h*ex)<f(a1)){
        a1<-a-h*ex
        s<- s+1
      }
      if(f(a1+h*ey)<f(a1)){
        a1<-a+h*ey
        s<- s+1
      }
      if(f(a1-h*ey)<f(a1)){
        a1<-a-h*ey
        s<- s+1
      }
      
      
      if(any(a1 != a)){                           #нашли ли мы точку в которую можем сдвинуться
        segments(a[1], a[2], a1[1],a1[2], col = "green")         #рисует линию
        points(a1[1],a1[2], col="brown", pch=20)
        while(f(2*a1-a)<f(a1)){
          a<-a1
          s<- s+1
          a1<-2*a1-a
          segments(a[1], a[2], a1[1],a1[2])
          points(a1[1],a1[2], col="blue", pch=20)
        }
      }else{
        h<-h/2
      }
      a<-a1
      
      
    }
  }
  
  #вывод
  ax<- a[1]
  ay<- a[2]
  znach<- ff(ax,ay)
  return(c(ax,ay,znach,s))
}
HookeJeeves(c(-7, -9),function(a){a[1]^2+a[2]^2+10}, 17,F)
