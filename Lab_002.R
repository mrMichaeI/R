library(MASS)
data("Cars93")

########## ЗАДАНИЕ 1

# 1
summary(Cars93)
# Можно, например по свойству Model (93 строки)

# 2
mean(Cars93[Cars93[ , "DriveTrain"] == "Rear", "Price"])

# 3
min(Cars93[Cars93[ , "Passengers"] == 7, "Horsepower"])
min(Cars93[Cars93[ , "Passengers"] == 6, "Horsepower"])

# 4
# Понадобятся колонки MPG.highway и Fuel.tank.capacity
distances = Cars93["MPG.highway"] * Cars93["Fuel.tank.capacity"]
as.character(Cars93$Make[which(distances == max(distances))])
as.character(Cars93$Make[which(distances == min(distances))])
as.character(Cars93$Make[which(distances == median(distances$MPG.highway))])


########## ЗАДАНИЕ 2

factory.run <- function (o.cars=1, o.trucks=1) {
  factory <- matrix(c(40,1,60,3),nrow=2, dimnames=list(c("трудодни","сталь"),c("автомобили","грузовики")))
  warehouse <- c(1600,70) #Доступно материалов на складе
  names(warehouse) <- rownames(factory)
  reserve <- c(8,1)
  names(reserve) <- rownames(factory)
  output <- c(o.cars, o.trucks)
  names(output) <- colnames(factory)
  
  steps <- 0 # Счётчик числа шагов цикла
  repeat {
    steps <- steps + 1
    needed <- factory %*% output # Подсчитаем ресурсы, которые нам нужны для производства требуемого кол-ва машин
    message(steps)
    print(needed)
    # Если ресурсов достаточно и остаток меньше или равен резерву, то мы произвели максимум возможного.
    # Нужно прекращать
    if (all(needed <= warehouse) && all((warehouse - needed) <= reserve)) {
      break()
    }
    # Если заявка слишком большая и ресурсов недостаточно, уменьшим её на 10%
    if (all(needed > warehouse)) {
      output <- output * 0.9
      next()
    }
    # Если всё наоброт, то увеличим на 10%
    if (all(needed < warehouse)) {
      output <- output * 1.1
      next()
    }
    # Если мы потребили одного ресурса слишком много, а другого недостаточно,
    # то увеличим план на случайную величину
    output <- output * (1+runif(length(output),min=-0.1,max=0.1))
  }
  
  return(output)
}

# 1
factory.run()

# 2
# Функция вызывается со стандартными значениями (1 автомобиль и 1 грузовик). 
# В ходе выполнения каждого шага выводятся затрачиваемые ресурсы и номер шага цикла, 
# а в результате функция выводит количество автомобилей и грузовиков, которое выгодно 
# выпустить (9 автомобилей и 20 грузовиков).

# 3
# Результаты различаются из-за используемых случайных величин

# 4
factory.run <- function(o.cars = 1, o.trucks = 1) {
       factory <- matrix(c(40, 1, 60, 3), nrow = 2, dimnames = list(c("workdays", "steel"), c("cars", "trucks")))
       warehouse <- c(1600, 70) #Доступно материалов на складе
       names(warehouse) <- rownames(factory)
       reserve <- c(8, 1)
       names(reserve) <- rownames(factory)
       output <- c(o.cars, o.trucks)
       names(output) <- colnames(factory)
   
         steps <- 0 # Счётчик числа шагов цикла
         repeat {
               steps <- steps + 1
               needed <- factory %*% output # Подсчитаем ресурсы, которые нам нужны для производства требуемого кол-ва машин
               # Если ресурсов достаточно и остаток меньше или равен резерву, то мы произвели максимум возможного.
               # Нужно прекращать
                 if (all(needed <= warehouse) && all((warehouse - needed) <= reserve)) {
                       break ()
                   }
               # Если заявка слишком большая и ресурсов недостаточно, уменьшим её на 10%
                 if (all(needed > warehouse)) {
                       output <- output * 0.9
                       next ()
                   }
               # Если всё наоброт, то увеличим на 10%
                 if (all(needed < warehouse)) {
                       output <- output * 1.1
                       next ()
                   }
               # Если мы потребили одного ресурса слишком много, а другого недостаточно,
               # то увеличим план на случайную величину
                 output <- output * (1 + runif(length(output), min = -0.1, max = 0.1))
             }
         print(needed)
         message(steps)
         return(output)
    }
factory.run()

# 5
factory.run(o.cars = 30, o.trucks = 20)