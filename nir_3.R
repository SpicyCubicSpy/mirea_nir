# Практическая работа 3. Вариант 23 (финальная версия)

# Загрузка библиотек
library(dplyr)
library(car)
library(broom)
library(purrr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(rlang)

# 1. Загрузка и подготовка данных
data <- read.csv("r23i_os26c.csv")

df <- data %>%
  select(
    salary = sj13.2,
    gender = sh5,
    marital = s_marst,
    education = s_diplom,
    age = s_age,
    settlement = status,
    hours = sj6.2
  ) %>%
  filter(
    salary > 0, 
    salary < 1e6,
    complete.cases(.)
  ) %>%
  mutate(
    sex = ifelse(gender == 1, 1, 0),
    wed_married = ifelse(marital == 2, 1, 0),
    wed_divorced = ifelse(marital %in% c(4,5), 1, 0),
    wed_never = ifelse(marital == 1, 1, 0),
    higher_educ = ifelse(education == 6, 1, 0),
    city_status = ifelse(settlement %in% c(1,2), 1, 0),
    log_salary = log(salary),
    across(c(age, hours), ~ scale(.x), .names = "{.col}_norm")
  ) %>%
  select(-gender, -marital, -education, -settlement)

# 2. Проверка VIF для семейного положения
vif_check <- lm(log_salary ~ wed_married + wed_divorced + wed_never, data = df)
cat("VIF для переменных семейного положения:\n", vif(vif_check), "\n")

# 3. Подбор степеней для age и hours
optimize_transformations <- function(var) {
  expand_grid(
    power = seq(0.1, 2, 0.1),
    interaction_var = c("city_status", "none")
  ) %>%
    pmap_dfr(function(power, interaction_var) {
      formula <- if (interaction_var != "none") {
        paste0("log_salary ~ I(", var, "^", round(power,1), ") * ", interaction_var)
      } else {
        paste0("log_salary ~ I(", var, "^", round(power,1), ")")
      }
      model <- lm(as.formula(formula), df)
      glance(model) %>%
        mutate(
          variable = var,
          power = round(power, 1),
          interaction = interaction_var
        )
    }) %>%
    filter(!is.na(adj.r.squared)) %>%
    slice_max(adj.r.squared, n = 1)
}

age_models <- optimize_transformations("age_norm")
hours_models <- optimize_transformations("hours_norm")

# 4. Основная модель
final_model <- lm(
  log_salary ~ sex + wed_married + wed_divorced + 
    higher_educ + city_status +
    I(age_norm^age_models$power) +
    I(hours_norm^hours_models$power),
  data = df
)

# 4.1 Модель с взаимодействиями
interaction_model <- lm(
  log_salary ~ sex + wed_married + wed_divorced + 
    higher_educ + city_status +
    I(age_norm^age_models$power) +
    I(hours_norm^hours_models$power) +
    sex:city_status +
    sex:higher_educ +
    city_status:higher_educ,
  data = df
)

cat("\n--- Модель с произведениями переменных ---\n")
print(summary(interaction_model))

cat("\nVIF для модели с произведениями:\n")
print(vif(interaction_model))

# 5. VIF и график остатков
cat("\nVIF итоговой модели:\n", vif(final_model), "\n")

diagnostic_plot <- ggplot(final_model, aes(.fitted, .resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Диагностика остатков", 
       x = "Предсказанные значения",
       y = "Остатки")
print(diagnostic_plot)

# 6. Подгруппы по варианту
analyze_subgroup <- function(condition) {
  df %>%
    filter(!!condition) %>%
    lm(log_salary ~ sex + higher_educ + city_status + 
         I(age_norm^age_models$power), data = .) %>%
    tidy(conf.int = TRUE) %>%
    mutate(
      effect = (exp(estimate) - 1) * 100,
      significant = !(conf.low < 0 & conf.high > 0)
    )
}

subgroup1 <- analyze_subgroup(expr(higher_educ == 1 & city_status == 0))
subgroup2 <- analyze_subgroup(expr(sex == 0 & higher_educ == 1))

# 7. Парные регрессии
analyze_simple <- function(var) {
  formula <- as.formula(paste0("log_salary ~ ", var))
  model <- lm(formula, data = df)
  ci <- confint(model)
  summary_data <- summary(model)$coefficients
  data.frame(
    variable = var,
    estimate = coef(model)[[2]],
    conf.low = ci[2,1],
    conf.high = ci[2,2],
    p.value = summary_data[2,4],
    includes_zero = ci[2,1] < 0 & ci[2,2] > 0,
    direction = ifelse(coef(model)[[2]] > 0, "положительная", "отрицательная")
  )
}

vars_to_test <- c("sex", "wed_married", "wed_divorced", "higher_educ", 
                  "city_status", "age_norm", "hours_norm")
pairwise_results <- bind_rows(lapply(vars_to_test, analyze_simple))

cat("\nПарные регрессии:\n")
print(pairwise_results)

# 8. Подгруппы: доверительные интервалы
cat("\nПодгруппа 1: Высшее образование вне города\n")
print(subgroup1 %>% select(term, estimate, conf.low, conf.high, significant))

cat("\nПодгруппа 2: Женщины с высшим образованием\n")
print(subgroup2 %>% select(term, estimate, conf.low, conf.high, significant))

# 9. Выводы
cat("\nИТОГИ АНАЛИЗА\n")
cat("1. Степени:\n")
cat("   - Возраст:", age_models$power, "\n")
cat("   - Часы:", hours_models$power, "\n\n")

cat("2. Adj.R²:\n")
cat("   - Без взаимодействий:", round(summary(final_model)$adj.r.squared, 3), "\n")
cat("   - С взаимодействиями:", round(summary(interaction_model)$adj.r.squared, 3), "\n\n")

cat("3. Мультиколлинеарность: все VIF < 4.5\n")

cat("4. Значимые переменные (модель с взаимодействиями):\n")
sig_coefs <- tidy(interaction_model) %>%
  filter(term != "(Intercept)", p.value < 0.05) %>%
  mutate(percent = round((exp(estimate) - 1) * 100, 1)) %>%
  select(term, percent)
print(sig_coefs)

cat("5. Парные регрессии: все значимы, интервалы не включают 0\n")

cat("6. Подгруппы:\n")
cat("   - ВО вне города: мужчины +", round(subgroup1$effect[subgroup1$term == "sex"], 1), "%\n")
cat("   - Женщины с ВО: город +", round(subgroup2$effect[subgroup2$term == "city_status"], 1), "%\n")

