#Павлюков В.В. 401и
#source("plug_in.R")
#Для классификации нужны:
#обучающая выборка
D_to_M=function(D){
  n=dim(D)[1]
  m=dim(D)[2]
  M=matrix(0,n,m)
  for(i in 1:n){
    for(j in 1:m){
      M[i,j]=D[i,j]
    }
  }
  M
}
Xl = D_to_M(iris)#ирисы фишера: обучающая выборка
N = c(1,3)#вектор признаков, содержит номера признаков учавствующих в обучении
Xl = Xl[,append(N,5)]#"обрезание" "лишних" столбцов выборки
n<-length(N)#число признаков
nomer_stolbtca_classov <- dim(Xl)[2]#номер столбца с названиями классов(в нашем случае последний 3-й)
classes = levels(iris[,dim(iris)[2]])#вектор названий классов
Ny<-length(classes)#количество классов
#эмпирические оценки априорных вероятностей появления объектов классов(частоты в выборке)
Py<-table(Xl[,nomer_stolbtca_classov])/dim(Xl)[1]

#Ишем эмпирические оценки функций правдоподобия

#матрица матожиданий
myus=matrix(ncol=n,nrow=Ny)
for(i in 1:Ny){
  myus[i,]=apply(Xl[,-nomer_stolbtca_classov][which(Xl[,nomer_stolbtca_classov]==i),],2,"mean")
}

#общая ковариационная матрица
E=matrix(0,n,n)

for(i in 1:Ny){
  Lx=Xl[,-nomer_stolbtca_classov][which(Xl[,nomer_stolbtca_classov]==i),]
  Lx=sweep(Lx,2,myus[i,],"-")
  for(j in 1:dim(Lx)[1]){
    E=E+Lx[j,]%*%t(Lx[j,])
  }
}
E=E/(dim(Xl)[1]-length(classes))
#вычисление альфа и бета параметров алгоритма
a=matrix(0,Ny,n)
b=rep(0,Ny)
Lyambda=rep(1,Ny)
for(i in 1:Ny){
  a[i,]=solve(E)%*%myus[i,]
  b[i]=log(Lyambda[i]*Py[i])-((t(myus[i,])%*%solve(E))%*%myus[i,])/2
}
#классификатор
alg = function(x, a, b) {
  Y = length(b)#количество классов
  n = dim(x)[2]#количество признаков
  scores = rep(0, Y)#вектор плотностей
  for (i in 1:Y) {
    scores[i] = x%*%a[i,]+b[i]
  }
  which.max(scores)#порядковый номер максимальной вероятности
}

#процент ошибки классификации
l<-dim(Xl)[1]
e<-0.0
for(i in 1:l){
  e<-e+100*(Xl[,nomer_stolbtca_classov][i]!=alg(c(Xl[,1][i],Xl[,2][i]), a, b ))/l
}

tcveta = c("red", "green", "blue")#создает вектор цветов
# график
plot(Xl[,1], Xl[,2], col = tcveta[Xl[,nomer_stolbtca_classov]], xlab = "", ylab = "",
     main = paste("fisher\n(error rate ",round(e,digits=2),"%)"),pch = 20)
#классифицируем все точки видимой области первого графика с шагом (0.1, 0.1)    
w_min<-min(Xl[,1])-0.1
w_max<-max(Xl[,1])+0.1
h_min<-min(Xl[,2])-0.1
h_max<-max(Xl[,2])+0.1
w<-w_min
while(w<w_max){
  h=h_min
  while(h<h_max){
    points(w, h, col = tcveta[alg(c(w,h), a, b )], pch = 1)
    h=h+0.1
  }
  w=w+0.1
}
