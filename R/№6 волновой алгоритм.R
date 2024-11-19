n<- nrow(m)

for(i in 1:(n-1)){
  for(j in (1+i):n){
    if(m[i,j]!=m[j,i]){
      stop('матрица должна быть симметричной')
    }
  }
}



#install.packages("igraph")
library(igraph)

#задаем матрицу и отрисовываем её
m<-matrix(c(0,1,1,0,0,0,0,0,0,
            1,0,0,1,1,0,0,0,0,
            1,0,0,0,1,1,0,0,0,
            0,1,0,0,1,0,1,0,0,
            0,1,1,1,0,0,1,0,0,
            0,0,1,0,0,0,0,1,0,
            0,0,0,1,1,0,0,0,1,
            0,0,0,0,0,1,0,0,1,
            0,0,0,0,0,0,1,1,0), nrow = 9, byrow = T)

set.seed(40)

g <- graph.adjacency(m, mode = 'undirected')    #неориентированный граф
plot.igraph(g)

level <- function(v,m){
  
  #проверки
  if (is.matrix(m)==FALSE){
    stop("необходимо ввести матрицу")
  }
  if(nrow(m)!= ncol(m)){
    stop("матрица должна быть квадратной")
  }
  if(sum(is.na(m)) !=0){
    stop("необходимо устранить пропущенные значения")
  }
  if (is.numeric(m)==FALSE){
    stop("необходимо ввести числовые значения")
  }
  if(isSymmetric(m)==FALSE){
    stop("матрица должна быть симметричной")
  }
  
  n<-nrow(m)
  L<-m[v, ]                                                                      #берем строку
  
for(lev in 2:(n-1)){                                                             #проходимся по разным вариантам уровней от 2 до 8
  for(i in 1:n){                                                                 #проходим по всем вершинам
    if(sum(L = 0)==1){                                                          #все ли l заполнены, только один ноль
      return(L) 
    }
    if(L[i] == lev-1){                                                           #является ли уровень не на один ли меньше
      for(j in 1:n){                                                             #присвоение уровня вершинам
        if((j!=v)&(L[j]==0)&(m[i,j]==1)){                                        #не равно v, нет разметки, смежная с i
          L[j]<- lev
        }
      }
    }
  }
}
  return(L)
}
level(2,m)

Wave<-function(v1,v2,m){
  
  #проверки
  if (is.numeric(v1)==FALSE){
    stop("необходимо ввести числовые значения v1")
  }
  if (is.numeric(v2)==FALSE){
    stop("необходимо ввести числовые значения v2")
  }
  n<-ncol(m)
  if((v1 %in% 1:n)==FALSE){
    stop('такой вершины v1 не существует в графе')
  }
  if((v2 %in% 1:n)==FALSE){
    stop('такой вершины v2 не существует в графе')
  }
  
  n<-ncol(m)
  L<-level(v1, m)
  dlina<- L[v2]            
 road<-c(v1, rep(NA, (dlina-1)), v2)                                             #массив где между v1 and v2 NA
  #условие если dlina=1
  if(dlina==1){
    return(list(c(v1,v2),dlina))
  }
  while(any(is.na(road))){
    for(i in 1:n){
      if((L[i] == (dlina-1))&(m[i,v2]==1)){                                      #имеет уровень финиша минус один и соединяется с финиш (смежность)
        road[dlina]<-i                                                           #на место этого уровня ставим подходящую вершину
        v2<-i                                                                    #конечная вершина меняется на неё
        dlina<-L[v2]                                                             #уровень уменьшается на 1
        
      }
    }
  }
  return(list(road, dlina))
}

Wave(2,8,m)