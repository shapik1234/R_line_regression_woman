require(stats)
require(datasets)
require(ggplot2)
require(nortest)
require("car")
require(corrplot)
require(readxl)
source("Functions.R")

# рост и вес
# 
#
data.woman <- women
##тест на нормальность данных
##
normality3.extend(women$height)
normality3.extend(women$weight)
#
cor.obj <- cor(data.woman,method= "pearson") 
# зависят на 99 процентов вес-рост
View(cor.obj)
# 0 hipotisa cor = 0 
# alternative hipotisa corelyaziya != 0
# проверим а наша зависимость случайна или нет, проведем тест при p =0.05
#  принимаем альтернативную гипотезу (данные зависят и не случайны)
cor.test(data.woman$height,data.woman$weight,method = "pearson")
# alternativnaya gipotisa p = 1.000^e-15
# нарисуем наши зависимости в виде таблице, сначала зададим цвета:
#с помощью colorRampPalette
# corrplot - выведет наши зависимости по нашему методу
col4 <- colorRampPalette(c("#7F0000","red","#FF7F00",
                           "yellow","#7FFF7F","cyan",
                           "#007FFF","blue","#00007F"))
# нарисуем график зависимости
corrplot(cor.obj,method="circle",col = col4(8),cl.length=6,
         order="AOE",addCoef.col = "red")
ggp <-  ggplot(data = as.data.frame(data.woman))+
   geom_point(aes(y=data.woman$weight,x=data.woman$height))
# продесонстрируем 
ggp
#  линейная моделб построим
#
#
#
#
w.fit <- lm(data.woman$weight~data.woman$height,data=data.woman)
ggp+geom_line(aes(x=data.woman$height,y=w.fit$fitted.values),color="red")
# w.fit$fitted.values сглаженые значения
# 
#
#

summary(w.fit)
# кофиеценты
# Multiple R-squared - коф. детерминации( обьясненная дисперсия(разброса))
#           Adjusted    R-squared - коф. детерминации подкорректированный с 
#                       ошибкой
# F-statistic критерий Фишера дисперсия <0.05 мы работаем с ГС
#
#
#
#
w.fit <- lm(data.woman$weight~data.woman$height+I(data.woman$height^2),data=data.woman)
ggp+geom_line(aes(x=data.woman$height,y=w.fit$fitted.values),color="red")
# w.fit$fitted.values сглаженые значения
# I - изолянта + (независемая переменная)
#
#

summary(w.fit)
