set.seed(42)
n<- matrix(c(sample(-100:100,100,replace=F)), nrow= 10,ncol=10)
print(n)

determinant<-function(n){
  
 
  if (is.matrix(n)==FALSE){
    stop("необходимо ввести матрицу")
  }
  if(nrow(n)!= ncol(n)){
    stop("матрица должна быть квадратной")
  }
  if (is.numeric(n)==FALSE){
    stop("необходимо ввести числовые значения")
  }
  if(sum(is.na(n)) !=0){
    stop("необходимо устранить пропущенные значения")
  }
  if(any(is.finite(n)==FALSE)){
    stop("устраните бесконечность в вводимых значениях")
  }
  if (any(apply (n, 1, function(n){all(n==0)}))) {
    stop('В матрице есть нулевая строка. Определить равен 0.')
  }
  if (any(duplicated(n))) {
    stop('В матрице есть одинаковые строчки. Определитель равен 0.')
  }
s<-0
row<- nrow(n)
if(row==1){
  return(n[1,1])
} else if (row==2){
  return((n[1,1]*n[2,2])-(n[1,2]*n[2,1]))
}else if (row==3){
  return((n[1,1]*n[2,2]*n[3,3])+(n[1,3]*n[2,1]*n[3,2])+(n[1,2]*n[3,1]*n[2,3])
         -((n[1,3]*n[2,2]*n[3,1])+(n[2,1]*n[1,2]*n[3,3])+(n[1,1]*n[3,2]*n[2,3])))
}else{
  for (i in 1: row){
    s<-s+(-1)^(1+i)*n[i,1]*determinant(as.matrix(n[-i,-1]))
  }
  return(s)
}
}
determinant(n)

#проверка работы алгоритма с помощью встроенной программы

det(n)