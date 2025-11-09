data(swiss)

# Среднее значение
mean_Catholic = mean(swiss$Catholic)
mean_Fertility =  mean(swiss$Fertility)

# Дисперсия
var_Catholic = var(swiss$Catholic)
var_Fertility = var(swiss$Fertility)

# Стандартное отклонение (СКО)
sd_Catholic = sd(swiss$Catholic)
sd_Fertility = sd(swiss$Fertility)

#Ответ №1
cat("Catholic: Mean =", mean_Catholic, ", Var =", var_Catholic, ", SD =", sd_Catholic, "\n")
cat("Fertility: Mean =", mean_Fertility, ", Var =", var_Fertility, ", SD =", sd_Fertility, "\n")

#Вариант №23: y = Catholic; x = Infant.Mortality | Fertility

# Catholic ~ Infant.Mortality
model_Infant.Mortality = lm(Catholic ~ Infant.Mortality, data = swiss)
summary(model_Infant.Mortality)

# Catholic ~ Fertility
model_examination = lm(Catholic ~ Fertility, data = swiss)
summary(model_examination)

# Ответ №2:
# Catholic = -8.968 - 2.513*Infant.Mortality
# Catholic = -67.441 - 1.548*Fertility

#Оценка:

# Catholic ~ Infant.Mortality:
# R^2: 0.0308
# p-value: 0.238
# 
# Модель плохая; слабая взаимосвязь между регрессором и объясняемой переменной.

# Catholic ~ Fertility:
# R^2: 0.215
# p-value: 0.001029
# **
# Модель плохая; слабая взаимосвязь между регрессором и объясняемой переменной.

#В обоих случаях связь между переменными выявлена отрицательной:
#Увеличение уровня младенческой смертности (Infant.Mortality) связано с уменьшением доли католического населения (Catholic).
#Увеличение уровня фертильности (Fertility) также связано с уменьшением доли католического населения (Catholic).

