fib <- function(n){
  if(!is.numeric(n)){
    return("n должно быть числовым")
  }
  if (n<=0){
    return("n должно быть положительным")
  }else if(n%%1!=0){
    return("n должно быть целым")
  }
  
  if (n==1){
    return(1)
  }else if(n==2){
    return(1)
  }
  else{
    return(fib(n-1)+fib(n-2))  
      }
}

fib(6)


#fff = function(x) {cos(x)*x} 
#fff = function(x) {-x^2 + 5*x +10}
#fff= function(x)  {1/x^3} #сломанная история (0, 0.1)
#z=fib(15)/fib(14) 

  zoloto<- function(fff,a,b,eps,mx, pl) {
     
    fff = function(x) {-x^2 + 5*x +10}    
    z<-fib(15)/fib(14)
    
    xl= b- abs(b-a)/z
    xr= a+ abs(b-a)/z
    
    if(!is.numeric(a)){
      return("n должно быть числовым")
    }
    if(!is.numeric(b)){
      return("n должно быть числовым")
    }
  if(a==Inf){
    return("a должно быть конечным")
  }
    if(b==Inf){
      return("a должно быть конечным")
    }
    if(b<=a){
      return("должно быть b>a")
    }
    if(eps<=0){
      return("должно быть eps>0")
    }
    if(!is.logical(mx)){
      stop('mx должна быть логической переменной')
    }
    if( !is.numeric(pl)||pl==Inf || pl%%1!=0 ){
      return("pl должно быть числовым,конечным, целым")
    }
  if (is.na(xl)){
    stop("xl должен быть не NA")
  }else if (is.na(xr)){
    stop("xr должен быть не NA")
  }
    #рисуем начальное положение
    par(mfcol=c(ceiling((pl+1)/2),2)) 
    OX<-seq(a,b,by=0.1) 
    plot(OX, fff(OX), type='l',lwd=3, main='Step 0')
    abline(v=a, col= "red", lwd=3)
    abline(v=b, col="red")
    abline(v=xl, col="orange")
    abline(v=xr, col= "green")

  if(mx==T){
    #поиск max

    step<-0
    shagi<-0
    
    while((b-a)>eps){
      if(fff(xl)>fff(xr)){
        
        b <- xr 
        xr <- xl
        xl <- b- (b-a)/z
        
        
      }else{
        
        a <- xl 
        xl <- xr
        xr <- a+ (b-a)/z
      }
        step <- step+1
        if(step<=pl){
          plot(OX, fff(OX), type="l", main=paste0("step", step))
          
          abline(v=a, col= "red", lwd=3)
          abline(v=b, col="red")
          abline(v=xl, col="orange")
          abline(v=xr, col= "green")
      }
      
        shagi=shagi+1
    } 
    m<-max(c(fff(a), fff(b), fff(xl), fff(xr)))
    nznach<- which(c(fff(a), fff(b), fff(xl), fff(xr)) == max(c(fff(a), fff(b), fff(xl), fff(xr))))
    if (nznach==1) {
      x<-a
    } else if(nznach==2) {
      x<-b
    } else if(nznach==3) {
      x<-xl
    } else if(nznach==4) {
      x<-xr
    }
    
  
    }  else{
       #поиск min
      step<-0
      shagi<-0
      while((b-a)>eps){
        if (fff(xl)>fff(xr)) {
          a<-xl
          xl<-xr
          xr<-a+abs(b-a)/z
          
        } else {
          b<-xr
          xr<-xl
          xl<-b-abs(b-a)/z
        }
          step <- step+1
          if(step<=pl){
            plot(OX, fff(OX), type="l", main=paste0("step", step))
            
            abline(v=a, col= "red", lwd=3)
            abline(v=b, col="red")
            abline(v=xl, col="orange")
            abline(v=xr, col= "green")
          }
          
        
        shagi<-shagi+1
        
      } 
     m<- min(c(fff(a), fff(b), fff(xl), fff(xr)))
      nznach<- which(c(fff(a), fff(b), fff(xl), fff(xr)) == min(c(fff(a), fff(b), fff(xl), fff(xr))))
      if (nznach==1) {
        x<-a
      } else if(nznach==2) {
        x<-b
      } else if(nznach==3) {
        x<-xl
      } else if(nznach==4) {
        x<-xr
      }
      
   }
    print(shagi)
    return(c(x,m))
  }
     

 zoloto(fff,2,8, 0.01, T, 6)


