# Подключение библиотек
library(tidyverse)
library(ggrepel)
library(openxlsx)
library(glue)

# Загрузка данных
data <- read.csv("./pim-447_boxplot_data.csv")

# Убираем пустые значения
data <- na.omit(data)

# Построение графика boxplot
ggplot(data, aes(x=reorder(primary_disease_renamed, log2_fold_change, median), y=log2_fold_change, fill=primary_disease_renamed)) + 
  geom_boxplot(outlier.shape=NA, show.legend = FALSE) + # Boxplot
  geom_jitter(size=1, alpha=0.9, show.legend = FALSE) + # Индивидуальные точки без окрашивания
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 14),
    legend.title = element_blank(), # Убираем заголовок легенды (на случай, если остается)
    plot.title = element_text(size=24)
  ) +
  ylim(min(data$log2_fold_change, na.rm = TRUE), max(data$log2_fold_change, na.rm = TRUE)) + # Динамическое ограничение Y
  labs(title= 'PIM-447 Boxplot for Cancer Types', y='Log2 Fold Change (LFC)', x='') + 
  guides(fill= 'none') + # Отключение легенды для fill
  scale_x_discrete(expand = c(0, 1)) # Заполнение пустого пространства на оси X

# Сохранение графика
ggsave(glue('./pim-447_boxplot.png'), 
       width = 12, height = 8, dpi = 300, units = "in")
