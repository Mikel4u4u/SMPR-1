#функция преобразования фрейма в матрицу
f_to_M=function(Xl,v_p,v_c){
  Xl = Xl[,append(v_p,dim(Xl)[2])]#"обрезание" "лишних" столбцов выборки
  classes = levels(Xl[,dim(Xl)[2]])#вектор названий классов 
  classes = classes[Nc]#"обрезание" "лишних" названий классов
  #применяем сравнение столбца классов с вектором названий нужных классов получим логическую матрицу
  #построчно применим операцию any получим логический вектор(true если хоть один элемент в строке true)
  #с помощью логического вектора оставим в выборке только данные с нужными классами
  Xl=Xl[Xl[,dim(Xl)[2]]%in%classes,]#описанные выше действия в одну строку
  #преобразование названий 2х классов в 1 и -1
  Xl[,dim(Xl)[2]]=ifelse(Xl[,dim(Xl)[2]]==classes[1],1,-1)
  as.matrix(Xl)
}
#max и min во всех столбцах кроме последнего
max_elements=function(Xl){
  apply(Xl[,-dim(Xl)[2]],2,"max")
}
min_elements=function(Xl){
  apply(Xl[,-dim(Xl)[2]],2,"min")
}
#нормировка столбцов матрицы кроме последнего
norm=function(Xl,ma,mi){
  l=dim(Xl)[1]
  n=dim(Xl)[2]-1
  for(i in 1:l){
    for(j in 1:n){
      Xl[i,j]=(2*Xl[i,j]-mi[j]-ma[j])/(ma[j]-mi[j])
      #Xl[i,j]=(Xl[i,j]-mi[j])/(ma[j]-mi[j])
      #Xl[i,j]=(Xl[i,j]-mi[j])/ma[j]
    }
  }
  Xl
}
#Зависимость темпа обучения от итераций
Eta = function(I){
  # if(I){
  #   if(I>10){
  #     1/log(I,10)
  #   }else{
  #     1/I
  #   }
  # }else{1/2}
  0.5
}
#Параметр сглаживания
lyambda=function(l){
  1/l
}
#процент ошибок
Q=function(Xl,w,w0,f=a){
  row=dim(Xl)[1]
  col=dim(Xl)[2]
  q=0
  for(i in 1:row){
    x=as.double(Xl[i,][-col])
    y=Xl[i,col]
    q=q+ifelse(y==f(x,w,w0),0,1)
  }
  return(100*q/row)
}
#функция выбора обучающих элементов
start_next_N=function(XL=Xl){
  set.seed(round(proc.time()[3]))
  pre_class<<-Xl[sample(1:dim(XL)[1],1,replace=FALSE),dim(Xl)[2]]
}
next_N=function(XL=Xl){
  l=dim(XL)[1]
  v=1:l
  v=v[Xl[,dim(Xl)[2]]!=pre_class]
  pre_class<<- -1*pre_class
  return(sample(v,1,replace=FALSE))
}
# start_next_N=function(XL=Xl){
#   set.seed(round(proc.time()[3]))
#   number_in_order<<-append(dim(XL)[1],sample(1:dim(XL)[1],dim(XL)[1],replace=FALSE))
# }
# next_N=function(XL=Xl){
#   l=dim(XL)[1]
#   if(number_in_order[1]==1){
#     n=number_in_order[2]
#     start_next_N(XL)
#   }else{
#     if(max(number_in_order)>l){
#       start_next_N(XL)
#       n=number_in_order[2]
#       number_in_order<<-number_in_order[-2]
#       number_in_order[1]<<-number_in_order[1]-1
#     }else{
#       n=number_in_order[2]
#       number_in_order<<-number_in_order[-2]
#       number_in_order[1]<<-number_in_order[1]-1
#     }
#   }
#   return(n)
# }
#алгоритм классификации
a=function(x,w,w0){
  sign(as.double(x%*%w)-w0)
}
vQ<-0
start_Q<-function(n){
  vQ<<-append(1,seq(1,n))
}
stub_Q<-function(){
  v=vQ[-1]
  sum(abs(v-rep(mean(v),length(v))))
}
app_Q<-function(n){
  if(vQ[1]==length(vQ)){
    vQ=vQ(-2)
    vQ=append(vQ,n)
  }else{
    vQ[1]=vQ[2]+1
    vQ[vQ[1]]=n
  }
}