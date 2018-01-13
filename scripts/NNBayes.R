#Павлюков В.В. 401и
#source("NNBayes.R")
#Для классификации нужны:
#обучающая выборка
Xl = iris#ирисы фишера: обучающая выборка
N = c(1,3)#вектор признаков, содержит номера признаков учавствующих в обучении
Xl = Xl[,append(N,5)]#"обрезание" "лишних" столбцов выборки
n<-length(N)#число признаков
nomer_stolbtca_classov <- dim(Xl)[2]#номер столбца с названиями классов(в нашем случае последний 5-й)
classes = levels(Xl[,nomer_stolbtca_classov])#вектор названий классов
Ny<-length(classes)#количество классов
#эмпирические оценки априорных вероятностей появления объектов классов(частоты в выборке)
Py<-table(Xl[,nomer_stolbtca_classov])/dim(Xl)[1]

#Ишем эмпирические оценки функций правдоподобия

#матрица матожиданий
myus = matrix(0, nrow=Ny, ncol=n)
#матрица среднеквадратичных отклонений
sigmas = matrix(0, nrow=Ny, ncol=n)

for(i in 1:Ny){
  for(j in 1:n){
    temp=Xl[Xl[,nomer_stolbtca_classov]==classes[i],][,j]#j-й столбец содержащий только признаки i-го класса
    myus[i,j]<-mean(temp)
    sigmas[i,j]<-sqrt(var(temp))
  }
}

#Функция вычисляющая нормальную плотность распределения величины
#на основании матожидания и среднеквадратичного отклонения
N = function(x, myu, sigma) {
  1/sqrt(2*pi)/sigma*exp(-1/2*(x-myu)^2/sigma^2)
}

#Наивный Байесовский классификатор
a = function(x, classes, Py, myus, sigmas) {
  Y = length(classes)#количество классов
  n = dim(myus)[2]#количество признаков
  scores = rep(0, Y)#вектор плотностей
  for (i in 1:Y) {
    scores[i] = Py[i]#
    for (j in 1:n) {#найдет произведение вероятности класса на плотности признаков
      scores[i] = scores[i] * N(x[j], myus[i,j], sigmas[i,j])
    }
  }
  which.max(scores)#порядковый номер максимальной вероятности
}

#процент ошибки классификации
l<-dim(Xl)[1]
e<-0.0
for(i in 1:l){
  e<-e+100*(Xl[,nomer_stolbtca_classov][i]!=classes[a(c(Xl[,1][i],Xl[,2][i]), classes, Py, myus, sigmas )])/l
}

tcveta = c("red", "green", "blue")#создает вектор цветов
# график
plot(Xl[,1], Xl[,2], col = tcveta[Xl[,nomer_stolbtca_classov]], xlab = "", ylab = "",main = paste("Наивный Байесовский классификатор (процент ошибки ",round(e,digits=2),"%)"),pch = 20)
#классифицируем все точки видимой области первого графика с шагом (0.1, 0.1)    
w_min<-min(Xl[,1])-0.1
w_max<-max(Xl[,1])+0.1
h_min<-min(Xl[,2])-0.1
h_max<-max(Xl[,2])+0.1
w<-w_min
while(w<w_max){
  h=h_min
  while(h<h_max){
    points(w, h, col = tcveta[a(c(w,h), classes, Py, myus, sigmas )], pch = 1)
    h=h+0.1
  }
  w=w+0.1
}
