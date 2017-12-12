<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <base href="https://github.com/PavlyukovVladimir/SMPR/blob/master/"></base>
</head>
<body>
  
  # Классифицирующие алгоритмы
  ## Навигация
  <a href="#Vvonyye_opredeleniya">Вводные определения</a>
  ## Вводные определения <a name="Vvonyye_opredeleniya"></a>
  <p>Задано множество объектов X, и множество допустимых ответов Y, и существует целевая функция y*: X -> Y, значения которой y<sub>i</sub> = y*(x<sub>i</sub>) известны только на конечном подмножестве объектов {x<sub>1</sub>, …, x<sub>ℓ</sub>} ⊂ X.</p>
  <p>Пары «объект-ответ» (x<sub>i</sub>, y<sub>i</sub>) называются <i>прецедентами</i>. Совокупность пар 𝑋<sup>ℓ</sup>=(𝑥<sub>𝑖</sub>,𝑦<sub>𝑖</sub>)<sup>ℓ</sup><sub>𝑖=1</sub> называется <i>обучающей выборкой</i>.</p>
  <p>Задача <i>обучения по прецедентам</i> заключается в том, чтобы по выборке 𝑋<sup>ℓ</sup> <i>восстановить зависимость</i> y*, то есть построить <i>решающую функцию</i> a: X -> Y, которая приближала бы целевую функцию y*(x), причем не только на объектах обучающей выборки, но и на всем множестве X.</p>
  <p>Решающая функция a должна допускать эффективную компьютерную реализацию; по этой причине её называют <i>классифицирующим алгоритмом</i> .</p>
  <p><i>Признак f</i> объекта x – это результат измерения некоторой характеристики объекта. Формально признаком называется отображение <i>f</i>: X -> D<i>f</i>, где D<i>f</i> – множество допустимых значений признака. В частности, любой алгоритм a: X -> Y также можно рассматривать как признак.</p>
  <p>Набор признаков <i>f</i><sub>1</sub>,…, <i>f</i><sub>n</sub>. Вектор (<i>f</i><sub>1</sub>(x),…, <i>f</i><sub>n</sub>(x)) называют <i>признаковым описанием</i> объекта x ∈ X. В дальнейшем будем полагать, что X = 𝐷<sub><i>𝑓</i><sub>1</sub></sub> × … × 𝐷<sub><i>𝑓</i><sub>𝑛</sub></sub>.</p>
  <p>Совокупность признаковых описаний всех объектов выборки записанная в виде таблицы размера ℓ × n, называют <i>матрицей объектов-признаков</i>:</p>
  <p><img src="img/matrix.jpg" alt="матрица объектов-признаков"></p>
  <p><i>Моделью алгоритмов</i> называется параметрическое семейство отображений A = {g(x, <i>θ</i>) | <i>θ</i> ∈ <i>Θ</i>}, где g: X × <i>Θ</i> -> Y – некоторая фиксированная функция, <i>Θ</i> – множество допустимых значений параметра <i>θ</i>, называемое <i>пространством параметров</i> или <i>пространством поиска</i>.</p>
  <p></p>
  <p></p>
</body>
</html>
