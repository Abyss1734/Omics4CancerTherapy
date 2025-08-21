library(tidyverse)
library(ggrepel)
library(openxlsx)
library(glue)
library(ggplot2)


# Загрузка данных
data <- read.csv("./bosutinib_boxplot_data.csv")

# Для этого примера нет столбца T-клеток, так что будем использовать только данные по заболеваниям
# Создание переменной для зависимости
data$Dependency <- ifelse(data$area_under_curve <= 10, "Sensitive (<10)", "Not sensitive")

# Убираем пустые значения
data <- na.omit(data)

# Построение графика boxplot
ggplot(data, aes(x=reorder(primary_disease_renamed, area_under_curve, median), y=area_under_curve, fill=primary_disease_renamed)) + 
  geom_boxplot(outlier.shape=NA, show.legend = FALSE) + # Boxplot
  geom_jitter(aes(color=Dependency), size=1, alpha=0.9) + # Индивидуальные точки
  scale_color_manual(values=c('black', '#A31621')) + # Устанавливаем цвета
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 14),
    legend.title = element_text(size=16),
    legend.text = element_text(size=14),
    plot.title = element_text(size=24)
  ) +
  ylim(7, 15) + # Ограничение оси Y для более квадратного вида
  labs(title= 'Bosutinib Boxplot for Сancer Types', y='Area Under the Curve (AUC)', x='', color= 'Sensitivity') + 
  guides(fill= 'none',
         color= guide_legend(override.aes= list(size=3))) +
  scale_x_discrete(expand = c(0, 1)) # Заполнение пустого пространства на оси X

# Сохранение графика
ggsave(glue('./bosutinib_boxplot.png'), 
       width = 12, height = 8, dpi = 300, units = "in")
