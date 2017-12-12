#Павлюков В.В. 401и
#source("script_1NN_kNN.R")
#   Метрический алгоритм классификации с обучающей выборкой Xl использует
#ВЕСОВУЮ ФУНКЦИЮ w(i,u), которая оценивает степень важности i-го соседа для классификации обьекта u и 
#ОЦЕНКУ БЛИЗОСТИ( Гy(u,Xl) = sum([yui=y]w(i,u)) i меняется от 1 до l ) обьекта u к классу y
#для того чтобы отнести классифицируемый обьект u к тому классу y,
#для которого суммарный вес ближайших обучающих объектов(оценка близости) максимален.

#алгоритм 1NN относит классифицируемый обьект к тому же классу, что и ближайший обьект выборки
	#весовая функция равна единице на ближайшем элементе и равна нулю на остальных
	#соответственно и оценка близости равна 1 на ближайшем соседе и равна нулю на всех остальных
#алгоритм kNN относит классифицируемый обьект к тому же классу, к которому принадлежат большинство из k ближайших обьектов выборки
	#весовая функция равна 1 на k ближайших соседях и 0 на остальных
#оба алгоритма отнесут обьект к первому встретившемуся в выборке классу в случае одинакового расстояния(1NN)
	или одинкового числа обьектов среди k соседей, принадлежащих разным классам(kNN)

#Для классификации нужны:
	#обучающая выборка
		Xl = iris#ирисы фишера: обучающая выборка
		N = c(1,3)#вектор признаков, содержит номера признаков учавствующих в обучении
		nomer_stolbtca_classov <- dim(Xl)[2]#номер столбца с названиями классов(в нашем случае последний 5-й)
	#функция расстояния
		poschitat_rasstoyanie = function(t1, t2) {#функция расстояния, вычисляет евклидово расстояние между элементами,
			sqrt(sum((t1 - t2)^2))                  #как будто значения параметров являются координатами
		}
	#метод сортировки
		otsortirovat_vyborky = function(klassyfitcyruyemyy_element,vyborka,vector_priznakov, po_ubyvaniyu = FALSE, funktciya_vesov = poschitat_rasstoyanie){
		#возвращает отсортированную выборку по возрастанию(по умолчанию подставив TRUE отсортируем по убыванию) весов(расстояний) ее элементов относительно классифицируемого элемента
			dlinna_vyborki = dim(vyborka)[1]
			matritca_rasstoyaniy = matrix(0,dlinna_vyborki,2)#содержит нули,нужна для хранения весов выборки относительно u
			for (i in 1:dlinna_vyborki) {
				matritca_rasstoyaniy[i,] = c(funktciya_vesov(klassyfitcyruyemyy_element, vyborka[i,][vector_priznakov]), i)
			}
			vyborka[order(matritca_rasstoyaniy[,1],decreasing = po_ubyvaniyu),]
		}
	#методы классификации:
		NN = function(klassyfitcyruyemyy_element,vyborka, vector_priznakov,nomer_stolbtca_classov,funktciya_rasstoyaniy=poschitat_rasstoyanie) {
			sortirovannaya_vyborka = otsortirovat_vyborky(klassyfitcyruyemyy_element,vyborka,vector_priznakov,FALSE,funktciya_rasstoyaniy)
			sortirovannaya_vyborka[1,nomer_stolbtca_classov]
		}

		kNN = function(klassyfitcyruyemyy_element,k,vyborka, vector_priznakov,nomer_stolbtca_classov,funktciya_rasstoyaniy=poschitat_rasstoyanie) {
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
#Визуализация:
	par(mfrow = c(2,1))#   Функция par() изменяет постоянные графические параметры,
		#то есть последующие графики будут строиться относительно
		#параметров, указанных пользователем.mfrow вектор формы c (nr, nc), который делит графическое окно
		#как матрицу из nr строк и nc столбцов, графики тогда рисуются в строках

	poluchit_klassy = function(vyborka,nomer_stolbtca_classov){#если я прав, то обьединение вектора названий классов
															   #с самим собой должно удалить одинаковые элементы
		union(vyborka[,nomer_stolbtca_classov],vyborka[,nomer_stolbtca_classov])
	}
	klassy <- poluchit_klassy(Xl,nomer_stolbtca_classov) #фрейм содержащий названия классов
	
	dlinna_vyborki <- dim(Xl)[1]

	tcveta = topo.colors( length(klassy) )#создает вектор цветов
	points = rbind( #несколько точек для классификации
		c(5, 3),
		c(6.3, 5),
		c(8, 3.5),
		c(5, 2.5),
		c(7.5,2),
		c(6,5.25)
	)

	# график 1NN
		plot(Xl[,N[1]], Xl[,N[2]], col = tcveta[Xl[,nomer_stolbtca_classov]], xlab = "", ylab = "",main = "1NN")
		for (i in 1:dim(points)[1]) {
			pt = points[i,]
			points(pt[1], pt[2], col = tcveta[NN(pt, Xl, N, nomer_stolbtca_classov )], pch = 15) 
		}

	# график kNN
		plot(Xl[,N[1]], Xl[,N[2]], col = tcveta[Xl[,nomer_stolbtca_classov]], xlab = "", ylab = "",main = "kNN")
		for (i in 1:dim(points)[1]) {
			pt = points[i,]
			points(pt[1], pt[2], col = tcveta[kNN(pt, 7, Xl, N, nomer_stolbtca_classov )], pch = 15) 
		}
