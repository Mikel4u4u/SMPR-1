#Павлюков В.В. 401и source("Loo_kNN.R")

#функция потерь оценивает ошибку алгоритма при классификации элемента, простейший случай это бинарная принимающая значения 0 или 1

#функционал качества, это среднее значение функции потерь на элементах выборки

#если разбить выборку N различными способами на две непересекающиеся подвыборки — обучающую длины l и контрольную длины k = L−l
#Среднее значение функционала качества от алгоритма на обучающих выборках, примененого к контрольным выборкам на этих разбиениях,
#называется оценкой скользящего контроля

#простейший случяй Loo
#   Суть Loo состоит в том, что из выборки удаляется элемент, алгоритм обучается на полученой выборке
#затем обученый алгоритм проверяется на исключенном элементе 
#Поступив таким образом со всеми элементами выборки, можно судить о методе обучения по
#частоте ошибочных классификаций.

#алгоритм kNN относит классифицируемый обьект к тому же классу, к которому принадлежат большинство из k ближайших элементов выборки
t1 = proc.time()

Xl = iris#ирисы фишера: обучающая выборка
N = c(1,3)#вектор признаков, содержит номера признаков учавствующих в обучении
nomer_stolbtca_classov <- dim(Xl)[2]#номер столбца с названиями классов(в нашем случае последний 5-й)

poschitat_rasstoyanie = function(t1, t2) {#функция расстояния, вычисляет евклидово расстояние между элементами,
  sqrt(sum((t1 - t2)^2))                  #как будто значения параметров являются координатами
}

otsortirovat_vyborky = function(klassyfitcyruyemyy_element,vyborka,vector_priznakov, po_ubyvaniyu = FALSE,
                                funktciya_vesov = poschitat_rasstoyanie){
#возвращает отсортированную выборку по возрастанию(по умолчанию подставив TRUE отсортируем по убыванию)
#весов(расстояний) ее элементов относительно классифицируемого элемента
  dlinna_vyborki = dim(vyborka)[1]
  matritca_rasstoyaniy = matrix(0,dlinna_vyborki,2)#содержит нули,нужна для хранения весов выборки относительно u
  for (i in 1:dlinna_vyborki) {
    matritca_rasstoyaniy[i,] = c(funktciya_vesov(klassyfitcyruyemyy_element, vyborka[i,][vector_priznakov]), i)
  }
  vyborka[order(matritca_rasstoyaniy[,1],decreasing = po_ubyvaniyu),]
}

poluchit_klassy = function(vyborka,nomer_stolbtca_classov){#если я прав, то обьединение вектора названий классов
                                                           #с самим собой должно удалить одинаковые элементы
  union(vyborka[,nomer_stolbtca_classov],vyborka[,nomer_stolbtca_classov])
}

kNN = function(klassyfitcyruyemyy_element,vyborka, vector_priznakov,nomer_stolbtca_classov,k,
               funktciya_rasstoyaniy=poschitat_rasstoyanie) {
  sortirovannaya_vyborka = otsortirovat_vyborky(klassyfitcyruyemyy_element,vyborka,vector_priznakov,FALSE,funktciya_rasstoyaniy)
  klassy = poluchit_klassy(vyborka,nomer_stolbtca_classov)
  blizost <- rep(0,times = length(klassy))
  names(blizost) <- klassy
  for (i in 1:k) {
    blizost[sortirovannaya_vyborka[i,nomer_stolbtca_classov]] <- (blizost[sortirovannaya_vyborka[i,nomer_stolbtca_classov]] + 1)
  }
  max = 1
  for (i in 1:length(blizost)) {
    if (blizost[max] < blizost[i]) {
      max <- i
    }
  }
  vyborka[vyborka$Species == klassy[max],][1,nomer_stolbtca_classov]#такие сложности для того, чтобы возвращался фрейм, а не строка
  #klassy[max] вернет строку с названием наиболее часто встречающегося класса
  #vyborka[vyborka$Species == klassy[max],] фрейм содержащий только 1 класс
  #vyborka[vyborka$Species == klassy[max],][1,nomer_stolbtca_classov] фрейм содержащий название класса
}

Loo = function(viborka, vector_priznakov, nomer_stolbtca_classov, parametr, metod = kNN){
  l = dim(viborka)[1]#длинна выборки
  k <- 0
  for (i in 1:l) {
    Xl <- viborka[-i,]#выборка без i-го элемента
    k <- k + (viborka[i,nomer_stolbtca_classov] != metod( viborka[i,N],Xl,vector_priznakov,nomer_stolbtca_classov,parametr) )
  }
  k/l
}
l = dim(Xl)[1]
k = rep(0,tiam = l)
for (i in 1:l) {
  k[i] <- Loo(Xl, N, nomer_stolbtca_classov, i)
}

t <- (proc.time() - t1)[3]/60
# график гистограма, на ней удобно отсчитывать точки от начала
plot(1:l, k, type = 'h', col = "black", xlab = "k", ylab = "Q",main = paste("Loo время выполнения: ", t, " мин.", sep = '') )
#как видно из графика минимум ошибок достигается при k = 3 или 4
