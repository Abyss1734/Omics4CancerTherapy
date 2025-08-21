# Загрузка необходимых библиотек
library(ComplexHeatmap)
library(circlize)

# Пути к файлам с результатами
correlation_file <- "./mean_depscores_by_cancer_type_data.csv"

# Проверка существования файлов
if (!file.exists(correlation_file)) stop("Файл с корреляциями не найден: ", correlation_file)

# Чтение данных
correlation_data <- read.csv(correlation_file, row.names = 1)

# Настройка цветовой шкалы
correlation_colors <- colorRamp2(c(-0.2, 0, 0.2), c("blue", "white", "red"))

# Тепловая карта для корреляций с отключенной дендрограммой
ht_correlation <- Heatmap(
  as.matrix(correlation_data), 
  name = "Mean depscore", 
  column_title = "Genes", 
  row_title = "Cancer Types", 
  show_row_names = TRUE, 
  show_column_names = TRUE, 
  clustering_distance_rows = "euclidean", 
  clustering_distance_columns = "euclidean", 
  clustering_method_rows = "complete", 
  clustering_method_columns = "complete",
  col = correlation_colors,
  show_row_dend = TRUE,    # Отключение дендрограммы по строкам
  show_column_dend = TRUE  # Отключение дендрограммы по столбцам
)

# Извлечение порядка строк и столбцов после кластеризации
row_order <- row_order(ht_correlation)
column_order <- column_order(ht_correlation)

# Сохранение тепловой карты
png("mean_depscores_by_cancer_type.png", width = 10, height = 10, units = "in", res = 600)
draw(ht_correlation)
dev.off()

message("Тепловая карта сохранена.")

