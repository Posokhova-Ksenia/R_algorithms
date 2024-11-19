#срез графа- это удаление такого количества ребер, чтобы граф перестал быть связным.
#строим наш граф

library(igraph)
g<-matrix(c(0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0), nrow=5,byrow= TRUE)
#g<-matrix(c(0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0), nrow=5,byrow= TRUE)

set.seed(2)

G<- graph.adjacency(g, mode= 'undirected')
plot.igraph(G)
#разорванный граф
g<-matrix(c(0,1,1,0,0,0,
            1,0,1,0,0,0,
            1,1,0,0,0,0,
            0,0,0,0,1,1,
            0,0,0,1,0,1,
            0,0,0,1,1,0
            ),nrow=6,byrow= TRUE)
g<-matrix(c(0,1,0,1,1,
            1,6,7,1,9,
            1,1,0,1,1,
            1,1,8,0,1,
            1,1,1,1,0), nrow=5,byrow= TRUE)

G<- graph.adjacency(g, mode= 'undirected')
plot.igraph(G)

check_mat<-function(g){
  n<- nrow(g)
  if (is.matrix(g)==FALSE){
    stop("необходимо ввести матрицу")
  }
  if(nrow(g)!= ncol(g)){
    stop("матрица должна быть квадратной")
  }
  if (is.numeric(g)==FALSE){
    stop("необходимо ввести числовые значения")
  }

if(any(g<0)){
  stop("убери отрицательные веса")
}
for (i in 1:(n-1)){
  for (j in (i+1):n){
    if((g[i,j] %in% c(0,1)) == F){
      print('не только 0 и 1')
    }
    
  }
}
  for(i in 1:(n-1)){
    for(j in (1+i):n){
      if(g[i,j]!=g[j,i]){
        print('матрица должна быть симметричной')
      }
    }
  }
for(i in (1:n)){
  if (g[i,i] != 0){
    print('петли')
  }
}
  
}
check_mat(g)

#проверяем связанный ли наш граф и если нет, выдаем подграфы и говорим, что срез не нужен

cc<-function(g){
  n<-nrow(g)
  A<-rep(0 , n)
  v<-1
  pg<-list()
  L<-g[v, ] 
  k<-1
  pg[[1]]<-c(v, which(L!=0))
  while(any(A==0)){
    k<-k+1
    for(i in 1:n){
      if(A[i]==0 & L[i]==0){
        v<-i
        L<-g[v, ]
        
        for(lev in 2:(n-1)){                                                             #проходимся по разным вариантам уровней от 2 до 8
          for(i in 1:n){                                                                 #проходим по всем вершинам
            if(sum(L == 0)==1){                                                          #все ли l заполнены, только один ноль
              pg1<-list("граф связанный")
              return (pg1)
            }
            #print(L)
            if(L[i] == lev-1){                                                           #является ли уровень не на один ли меньше
              for(j in 1:n){                                                             #присвоение уровня вершинам
                if((j!=v)&(L[j]==0)&(g[i,j]==1)){                                        #не равно v, нет разметки, смежная с i
                  L[j]<- lev
                }
              }
            }
            if(sum(L == 0)==1){                                                          #все ли l заполнены, только один ноль
              pg1<-list("граф связанный")
              return (pg1)
            }
          }
        }
      }
      else{
        if(L[i]!=0){
          A[v]<-1
          A[which(L!=0)]<-1
          pg[[k]]<-c(v,which(L!=0))
        }
      } 
      pg1<-list("срез не требуется", pg)
     }
    }
return(pg1) 
}
  cc(g)
  
# убираем веса,  делает симметрик (делаем граф неориентированным),убираем петли
  
  goodgraph<-function(g){
    n<-nrow(g)
    for(i in 1:(n-1)){
      for(j in (i+1):n){
        if(g[i,j]!=0 & g[i,j]!=1){
          g[i,j]<-1
        }
        if(g[i,j]!=g[j,i]){
          g[i,j]<-1
          g[j,i]<-1
        }
      }
      g[i,i]<-0
      
      
    }
    print(g)
    
  }
  G<- graph.adjacency(goodgraph(g), mode= 'undirected')
  plot.igraph(G)
  g<-goodgraph(g)
  
  # находим ab
chose_edge<-function(g,x){
  n_edges<-sum(g)/2            #количество ребер, которое есть
  x<-sample(1:n_edges, size=1) #случайным образом выбирается одно
  n<-nrow(g)
  s<-0
  for(i in (1:(n-1))){         
    for(j in (i+1):n){
      s<-s + g[i,j]
      if(s>=x){
        return(c(i,j))
      }
    }
  }
}

cut_graf<-function(g){
  n<-nrow(g)
  i<-0
    while(sum(apply(g, 1, function(x){any(x!=0)}))>2){ #пока более двух ненулевых строк (пока две неизолированных вершины)
      i<-i+1                                           #счетчик запусков цикла
      k<-sample(1:(sum(g)/2),1)
      temp<-chose_edge(g,k)
      a<-temp[1]
      b<-temp[2]
      #сжатие графа по a,b
      t<-g[a,]+g[b, ]
      g[a,]<-g[,a]<-t
      g[b,]<-g[,b]<-0
      g[a,a]<-0
      #print(g) если очень хочется увидеть весь путь)))))))))
    }
  
  return(c(i, sum(g)/2))
}

#много раз запускает cut по-разному, а потом выбирает минимальный срез\

mincut <- function(g){
  mn<-+Inf
  n<-nrow(g)
  for(i in 1:n^2){          #сколько размы повторяем всю задачу
    temp<-cut_graf(g)        #cколько срезов и какое минимальное количество ребер
    if(temp[2]==1){         
      stop("кол-во срезов = 1")
    }
    else if(temp[2]<mn){      #две переменные чтобы не потерять количество срезов, которые мы сделали
      
      mn<-temp[2]
      otvet<-temp
    }
  }
  return(otvet)
  
}
mincut(g)  
    
  
