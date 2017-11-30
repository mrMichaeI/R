#Загрузите данные в датафрейм. Адрес: github    https://raw???путь_к_файлу_найдите_сами???/data/gmp.dat
gmp <- read.table(file="https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/gmp.dat")

gmp$pop <- gmp$gmp/gmp$pcgmp

estimate.scaling.exponent <- function(a, y0=6611, response=gmp$pcgmp,
                                      predictor = gmp$pop, maximum.iterations=100, deriv.step = 1/100,
                                      step.scale = 1e-12, stopping.deriv = 1/100) {
  mse <- function(a) { mean((response - y0*predictor^a)^2) }
  for (iteration in 1:maximum.iterations) {
    deriv <- (mse(a+deriv.step) - mse(a))/deriv.step
    a <- a - step.scale*deriv
    if (abs(deriv) <= stopping.deriv) { break() }
  }
  fit <- list(a=a,iterations=iteration,
              converged=(iteration < maximum.iterations))
  return(fit)
}

#Пример вызова с начальным занчением a

a0 <- (estimate.scaling.exponent(0.15))
a0

#С помошью полученного коэффициента постройте кривую (функция curve) зависимости
y0 = 6611
curve(y0*x^estimate.scaling.exponent(0.15)$a,gmp$pop)

#Удалите точку из набора исходных данных случайным образом, как изменилось статистическая оценка коэффициента a?
gmp <- gmp[-c(round(runif(1, 1, nrow(gmp))))]
a1 <- estimate.scaling.exponent(0.15)
a1$a - a0$a

#Статистическая оценка коэффициента а не изменилась


#Запустите оценку несколько раз с разных стартовых точек. Как изменилось значение a?
a2 <- estimate.scaling.exponent(0)
a3 <- estimate.scaling.exponent(0.11)
a4 <- estimate.scaling.exponent(0.24)
a5 <- estimate.scaling.exponent(0.38)
a6 <- estimate.scaling.exponent(0.95)
a7 <- estimate.scaling.exponent(1.22)
a2$a
a3$a
a4$a
a5$a
a6$a
a7$a

#Чем больше значение стартовой точки тем меньше значение а