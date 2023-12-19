# Загрузка библиотеки
install.packages("vip")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("e1071")
install.packages("caret")

library(ggplot2)
library(randomForest)
library(e1071)
library(caret)


library(tidyverse)

# Шаг 1: Загрузка данных
fifa_data <- read_csv("D:/UTM_S03E01/AD/Dataset/Career_FIFA_data.csv")

# Шаг 2: Проверка первых строк
head(fifa_data)

colnames(fifa_data)

# Получение сводной статистики для каждой переменной
# Для числовых переменных включает среднее, минимум, максимум, медиану
# Для категориальных переменных показывает количество уникальных значений
summary(fifa_data)

# Получение имен колонок датасета
# Полезно для проверки названий всех переменных
names(fifa_data)


#1--------------------------------
#График распределения общего рейтинга игроков (Overall)
ggplot(fifa_data, aes(x=overall)) +
  geom_histogram(binwidth=1, fill="blue", color="black") +
  labs(title="Распределение общего рейтинга игроков", x="Общий рейтинг", y="Количество игроков")



#2----------------------
#Гистограмма распределения игроков по возрастным группам
ggplot(fifa_data, aes(x=age)) +
  geom_histogram(binwidth=1, fill="green", color="black") +
  labs(title="Распределение игроков по возрасту", x="Возраст", y="Количество игроков")



#3----------------------
# Сравнение среднего рейтинга игроков в разных клубах
fifa_data %>%
  group_by(club_name) %>%
  summarise(average_overall = mean(overall, na.rm = TRUE)) %>%
  top_n(20, average_overall) %>%
  ggplot(aes(x=reorder(club_name, average_overall), y=average_overall)) +
  geom_bar(stat="identity", fill="orange") +
  coord_flip() +
  labs(title="Топ-20 клубов по среднему общему рейтингу", x="Клуб", y="Средний общий рейтинг")



#4---улучшенный-------------------
ggplot(fifa_data, aes(x = age, y = overall)) +
  geom_point(aes(color = age)) +  # Добавим цвет к точкам в зависимости от возраста
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Линейная регрессия общего рейтинга от возраста", x = "Возраст", y = "Общий рейтинг") +
  theme_minimal() +  # Используем минимальную тему для чистого вида
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),  # Жирный шрифт и центрирование заголовка
    axis.title = element_text(face = "bold"),  # Жирные шрифты для осей
    legend.position = "bottom"  # Легенда внизу
  ) +
  scale_color_gradient(low = "yellow", high = "red")  # Градиент цвета от жёлтого к красному



#6----------------------

# 1. Предварительная обработка
# Убедитесь, что 'overall' и 'potential' не содержат NA
fifa_data <- na.omit(fifa_data[c('short_name', 'age', 'club_name', 'nationality_name', 'overall', 'potential')])

# 2. Критерии для 'скрытых жемчужин'
potential_threshold <- 83
overall_threshold <- 70

# 3. Фильтрация данных
hidden_gems <- fifa_data[fifa_data$potential >= potential_threshold & fifa_data$overall < overall_threshold,]

# 4. Анализ и визуализация
# (Пример: Визуализация распределения возраста 'скрытых жемчужин')
ggplot(hidden_gems, aes(x = age, y = overall)) +
  geom_point(color = 'blue') +  # Точки для каждого игрока
  geom_text(aes(label = short_name), vjust = -1, hjust = 0.5, size = 3) +  # Текстовые метки с именами
  labs(title = "Распределение возраста и общего рейтинга скрытых жемчужин",
       x = "Возраст", 
       y = "Общий рейтинг") +
  theme_minimal()  # Использование минимальной темы для чистого вида

# 5. Вывод
# Вывод списка скрытых жемчужин
print(hidden_gems)








library(ggplot2)
library(ggrepel)

ggplot(hidden_gems, aes(x = age, y = overall)) +
  geom_point(color = 'blue') +
  geom_text_repel(aes(label = short_name), size = 3) +  # Использование ggrepel
  labs(title = "Распределение возраста и общего рейтинга скрытых жемчужин",
       x = "Возраст", 
       y = "Общий рейтинг") +
  theme_minimal()



ggplot(hidden_gems, aes(x = age, y = overall, color = potential)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +  # Градиент цвета от низкого к высокому потенциалу
  geom_text_repel(aes(label = short_name), size = 3) +
  labs(title = "Распределение возраста и общего рейтинга скрытых жемчужин",
       subtitle = "Цвета точек отображают потенциал игрока",
       x = "Возраст", 
       y = "Общий рейтинг") +
  theme_minimal(base_size = 14) +  # Базовый размер шрифта
  theme(legend.position = "bottom",  # Расположение легенды
        plot.title = element_text(size = 16, face = "bold"),  # Стиль заголовка
        plot.subtitle = element_text(size = 12))  # Стиль подзаголовка











install.packages("plotly")
library(plotly)

# Создание интерактивного графика
p <- ggplot(hidden_gems, aes(x = age, y = overall, text = short_name)) +
  geom_point(color = 'blue') +
  labs(title = "Распределение возраста и общего рейтинга скрытых жемчужин",
       x = "Возраст",
       y = "Общий рейтинг")

# Конвертация в интерактивный график
ggplotly(p, tooltip = "text")







# Выбираем ограниченное количество игроков для аннотаций
top_hidden_gems <- head(hidden_gems, 10)  # Например, первые 10

ggplot(hidden_gems, aes(x = age, y = overall)) +
  geom_point(color = 'blue') +
  geom_text(data = top_hidden_gems, aes(label = short_name), vjust = -1, hjust = 0.5, size = 3) +
  labs(title = "Распределение возраста и общего рейтинга скрытых жемчужин",
       x = "Возраст",
       y = "Общий рейтинг") +
  theme_minimal()



#7----------------------
# График соотношения возраста и потенциала игроков:
# График рассеяния для возраста и потенциала
ggplot(fifa_data, aes(x=age, y=potential)) +
  geom_point(aes(color=potential), alpha=0.5) +
  labs(title="Соотношение возраста и потенциала игроков", x="Возраст", y="Потенциал")


#8----------------------
#График регрессии для анализа зависимости общего рейтинга игрока от его возраста:
ggplot(fifa_data, aes(x=age, y=overall)) +
  geom_point() +
  geom_smooth(method="lm", color="blue") +
  labs(title="Зависимость общего рейтинга от возраста", x="Возраст", y="Общий рейтинг")


#9----------------------
#График регрессии для анализа зависимости стоимости игрока от его рейтинга:
ggplot(fifa_data, aes(x=overall, y=value_eur)) +
  geom_point() +
  geom_smooth(method="lm", color="red") +
  labs(title="Зависимость стоимости игрока от общего рейтинга", x="Общий рейтинг", y="Стоимость (EUR)")


#10----------------------
# График регрессии для сравнения роста и веса игроков:
ggplot(fifa_data, aes(x=height_cm, y=weight_kg)) +
  geom_point() +
  geom_smooth(method="lm", color="green") +
  labs(title="Корреляция между ростом и весом игроков", x="Рост (см)", y="Вес (кг)")



#11----------------------
# График распределения игроков по национальности:
# Установка и загрузка dplyr, если это еще не сделано
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)


fifa_data %>%
  group_by(nationality_name) %>%
  summarise(count = n()) %>%
  top_n(10, count) %>%
  ggplot(aes(x=reorder(nationality_name, count), y=count)) +
  geom_bar(stat="identity", fill="blue") +
  coord_flip() +
  labs(title="Топ-10 национальностей среди игроков FIFA", x="Национальность", y="Количество игроков")


#12----------------------
#График регрессии для оценки связи между возрастом игрока и его потенциалом:
ggplot(fifa_data, aes(x=age, y=potential)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", color="purple") +
  labs(title="Зависимость потенциала игроков от возраста", x="Возраст", y="Потенциал")





#Overall

library(dplyr)
library(randomForest)
library(caret)

# 1. Подготовка данных
manutd_players <- fifa_data %>%
  filter(club_name == "Manchester United") %>%
  select(age, potential, value_eur, wage_eur, overall)  # Выберите интересующие вас признаки

# 2. Разделение данных на обучающую и тестовую выборки
set.seed(123)
data_split <- initial_split(manutd_players, prop = 0.8, strata = 'overall')
train_data <- training(data_split)
test_data <- testing(data_split)

# 3. Обучение модели случайного леса
rf_model <- randomForest(overall ~ ., data = train_data)

# 4. Оценка модели
predictions <- predict(rf_model, test_data)
result <- postResample(predictions, test_data$overall)
print(result)

library(Metrics)

# Оценка модели
mse <- mse(test_data$overall, predictions)
mae <- mae(test_data$overall, predictions)
r2 <- R2(predictions, test_data$overall)

# Вывод результатов
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R2:", r2, "\n")


# Добавление предсказаний к тестовым данным
test_data$predicted_overall <- predict(rf_model, test_data)

# Визуализация сравнения фактических и предсказанных значений
ggplot(test_data, aes(x = overall, y = predicted_overall)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Сравнение фактических и предсказанных значений Overall",
       x = "Фактический Overall",
       y = "Предсказанный Overall") +
  theme_minimal()






# Подключение необходимых библиотек
library(dplyr)
library(ggplot2)
library(stats)

# Предполагается, что fifa_data - это ваш DataFrame
# Здесь используется примерный код для загрузки данных
# fifa_data <- read.csv('path_to_your_data.csv')

# Создание модели линейной регрессии
model3 <- lm(overall ~ attacking_crossing + attacking_finishing + attacking_heading_accuracy + skill_dribbling, data = fifa_data)

# Вывод результатов модели
summary(model3)

plot(model3, which = 1)
abline(h = 0, col = "red", lwd = 2)


library(ggplot2)

# Извлечение остатков и предсказанных значений
residuals <- resid(model3)
fitted_values <- fitted(model3)

# Подготовка данных для графика
data_for_plot <- data.frame(
  Предсказанные_Значения = fitted_values,
  Остатки = residuals
)

# Создание графика остатков
ggplot(data_for_plot, aes(x = Предсказанные_Значения, y = Остатки)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "График Остатков Модели Линейной Регрессии",
    x = "Предсказанные значения Overall",
    y = "Остатки"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )
