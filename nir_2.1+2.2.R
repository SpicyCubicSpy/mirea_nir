# Подключение библиотек
library(lmtest)
library(car)

# Загрузка данных и удаление пропущенных значений
data <- na.omit(attitude)

### Задание 1: Проверка линейной зависимости регрессоров
check_complaints <- lm(complaints ~ privileges + learning, data)
check_privileges <- lm(privileges ~ complaints + learning, data)check_learning <- lm(learning ~ complaints + privileges, data)

summary(check_complaints)$r.squared
summary(check_privileges)$r.squared
summary(check_learning)$r.squared

### Задание 2: Построение базовой модели
base_model <- lm(rating ~ complaints + privileges + learning, data)
summary(base_model)
vif(base_model)

### Улучшенная модель без privileges
reduced_model <- lm(rating ~ complaints + learning, data)
summary(reduced_model)

### Задание 3: Логарифмические модели
model_log1 <- lm(rating ~ log(complaints) + privileges + learning, data)
model_log2 <- lm(rating ~ complaints + log(privileges) + learning, data)
model_log3 <- lm(rating ~ complaints + privileges + log(learning), data)

summary(model_log1)$r.squared
summary(model_log2)$r.squared
summary(model_log3)$r.squared

### Задание 4: Полиномиальная модель с взаимодействиями
full_model <- lm(rating ~ complaints + privileges + learning + 
                   I(complaints^2) + I(privileges^2) + I(learning^2) +
                   complaints:privileges + complaints:learning + privileges:learning,
                 data = data)

# Отбор признаков
best_model <- step(full_model, direction = "backward", trace = 0)
summary(best_model)

### Задание 2.2 — Доверительные интервалы
coef_summary <- summary(best_model)$coefficients
df <- best_model$df.residual
t_critical <- qt(0.975, df)

conf_intervals <- data.frame(
  Estimate = coef_summary[, "Estimate"],
  Std.Error = coef_summary[, "Std. Error"],
  CI_lower = coef_summary[, "Estimate"] - t_critical * coef_summary[, "Std. Error"],
  CI_upper = coef_summary[, "Estimate"] + t_critical * coef_summary[, "Std. Error"]
)

# Гипотеза β = 0
conf_intervals$Significant <- !(conf_intervals$CI_lower <= 0 & conf_intervals$CI_upper >= 0)

### Задание 5: Прогноз
new_data <- data.frame(complaints = 30, learning = 50)
prediction <- predict(best_model, new_data, se.fit = TRUE, interval = "confidence")

### Парные регрессии
predictors <- c("complaints", "I(learning^2)")
for (p in predictors) {
  formula <- as.formula(paste("rating ~", p))
  model <- lm(formula, data)
  coef <- coef(model)[2]
  se <- summary(model)$coefficients[2, 2]
  df <- model$df.residual
  t_crit <- qt(0.975, df)
  ci_lower <- coef - t_crit * se
  ci_upper <- coef + t_crit * se
  print(paste("Регрессор:", p))
  print(paste("Коэффициент:", round(coef, 3)))
  print(paste("Доверительный интервал:", round(ci_lower, 3), "-", round(ci_upper, 3)))
}

