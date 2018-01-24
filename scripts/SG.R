#Павлюков В.В. 401и
#source("SG.R")
N = c(1,3)#вектор признаков, содержит номера признаков учавствующих в обучении
Nc = c(1,2)#порядковые номера классов учавствующих в классификации
source("SG_funktcii.R")
#Вход:
  #обучающая выборка
    Xl=f_to_M(iris,N,Nc)
    row=dim(Xl)[1]
    col=dim(Xl)[2]
  #нормировка признаков
    ma=max_elements(Xl)
    mi=min_elements(Xl)
    X0<-Xl#копия выборки,чтобы не делать обратный пересчет из нормированой
    Xl=norm(Xl,ma,mi)#нормировка признаков(по столбцам кроме последнего)
#получение весов
  #1:инициализировать начальные веса
    w1<<-rnorm(col-1)/(2*row)#предыдущее значение параметров алгоритма
  #2:инициализировать текущую оценку функционала
    q1<<-Q(Xl,w1,L_ADALINE)#предыдущая оценка функционала
  #подготовка к циклу
    iteration<-1#счетчик итераций
    wi=w1#текущее Значение параметров алгоритма
    qi=q1#текущая оценка функционала
    start_next_N()#подготовка функции выбора элементов
    w0=0#w0 в нормированной выборке равен нулю
  repeat{
    #4:выбрать объект х
      i={
        b=c(0)#вектор номеров неправильно классифицированных элементов
        for(k in 1:row){
          if((abs(Xl[k,col]-a(as.double(Xl[k,][-col]),wi,w0))/2)!=0){
            b=append(b,k)
          }
        }
        if(length(b)==1){
          #break#выборка разделена
          next_N()
        }else{
          sample(b[-1],1,replace=FALSE)#извлечение случайного элемента
        }
      }
      xi<-as.double(Xl[i,][-col])#признаки k-го элемента
      yi<-Xl[i,col]
    #5:вычислить выходное значение алгоритма a(xi,w) и ошибку ei=L(Mi)
      y<-a( xi,w1,0 )
      ei<-L_ADALINE(xi,y,w1)
    #6:сделать шаг градиентного спуска:w-xi*Eta*L'*yi
      #wi=w1-xi*as.double(Eta(iteration)*dL_ADALINE(xi,yi,w1)*yi)
      wi=w1-Eta(iteration)*(as.double(w1%*%xi)-yi)*xi
    #7:оценить значение функционала:
      qi=(1-lyambda(row))*q1+lyambda(row)*ei
      iteration<-iteration+1
    #8:пока значение Q не стабилизируется и/или веса не перестанут изменяться
      #sq<-sum((qi-q1)^2)
      sw<-sum((w1-wi)^2)
      #if((sq<1e-5)||(sw<1e-10)){
      if(sw<1e-8){
        break
      }else{
        w1=wi
        q1<<-qi
        next
      }
  }
# график
  Xl=X0#возврат выборки в исходное состояние
  #пересчет параметров алгоритма
  wi=wi/((ma-mi)/2)
  w0=wi%*%((mi+ma)/2)
#процент ошибок
  e=0
  for(n in 1:row){
    e=e+abs(Xl[n,col]-a(as.double(Xl[n,][-col]),wi,w0))/2
  }
  e=100*e/row
  #график распределения
  tcveta = c("red", "blue", "green")#создает вектор цветов
  #функция проходящая через точки (1,1),(0,3),(-1,2)
  Ntc=function(k){
    ifelse(k==1,1,3+k)
  }
  plot(Xl[,1], Xl[,2], col = tcveta[Ntc(Xl[,col])], xlab = "", ylab = "",
       main = paste("ADALINE\n(error rate ",round(e,digits=2),"%, iterations ",iteration,")"),pch = 20)
  #классифицируем все точки видимой области первого графика с шагом (0.1, 0.1)    
  w_min<-min(Xl[,1])-0.1
  w_max<-max(Xl[,1])+0.1
  h_min<-min(Xl[,2])-0.1
  h_max<-max(Xl[,2])+0.1
  we<-w_min
  while(we<w_max){
    h=h_min
    while(h<h_max){
      points(we, h, col = tcveta[Ntc(a(c(we,h),wi,w0 ))], pch = 1)
      h=h+0.1
    }
    we=we+0.1
  }
