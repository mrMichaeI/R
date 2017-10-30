########## ЗАДАНИЕ 1


# 1

html <- readLines('https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/forbes.htm')

length(html) # 1991 строка
sum(nchar(html)) # 80380 символов


# 2 Bill Gates 72 B Larry Ellison 41 B
# 3

finance <- grep("\\$\\d{1,},\\d{1,} B|\\$\\d{1,} B", html)
finance

length(finance)


# 4

finance.p <- regexpr("\\$\\d{1,},\\d{1,} B|\\$\\d{1,} B", html)
finance.m <- regmatches(html,finance.p)
finance.m

#
length(finance.m)

#
finance.m[1]

#
finance.m[finance.m == "$72 B"]

#
finance.m[finance.m == "$41 B"]

#
length(unique(finance.m))




########## ЗАДАНИЕ 2


# 5

worths <- (as.numeric(gsub(",",".",regmatches(finance.m,regexpr("\\d{1,},\\d{1,} |\\d{1,}",finance.m)))))*10^9
worths

#
is.vector(worths)
length(worths)
typeof(worths)

#
length(worths[worths > 10^9])

#
worths[worths==max(worths)]


# 6

#
median(worths)

#
mean(worths)

#
length(worths[worths>5*10^9])
length(worths[worths>10*10^9])
length(worths[worths>25*10^9])

#
sum(worths)

#
sum(worths[1:5])/sum(worths)

#
sum(worths[1:20])/sum(worths)

#
sum(worths)/(78.536*10^9)