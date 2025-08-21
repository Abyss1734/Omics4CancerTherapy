# Загрузка необходимых библиотек
library(ComplexHeatmap)
library(circlize)

# Пути к файлам с результатами
correlation_file <- "./correlation_results_bosutinib_auc_vs_expression_by_cancer_type.csv"
p_value_file <- "./p_value_results_bosutinib_auc_vs_expression_by_cancer_type.csv"

# Проверка существования файлов
if (!file.exists(correlation_file)) stop("Файл с корреляциями не найден: ", correlation_file)
if (!file.exists(p_value_file)) stop("Файл с p-значениями не найден: ", p_value_file)

# Чтение данных
correlation_data <- read.csv(correlation_file, row.names = 1)
p_value_data <- read.csv(p_value_file, row.names = 1)

# Преобразование p-значений в матрицу
p_value_matrix <- as.matrix(p_value_data)

# Преобразование p-значений: отрицательный логарифм
p_value_log_data <- -log10(p_value_matrix)

# Ограничение значений p-значений, чтобы избежать слишком больших значений из-за нулевых
p_value_log_data[is.infinite(p_value_log_data)] <- max(p_value_log_data[!is.infinite(p_value_log_data)], na.rm = TRUE)

# Настройка цветовой шкалы
correlation_colors <- colorRamp2(c(-0.5, 0, 0.5), c("blue", "white", "red"))
p_value_colors <- colorRamp2(c(0, 1.1, 2), c("grey", "white", "blue"))

# Тепловая карта для корреляций с отключенной дендрограммой
ht_correlation <- Heatmap(
  as.matrix(correlation_data), 
  name = "Spearman Correlation", 
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

# Применение порядка к p-value
p_value_log_data <- p_value_log_data[row_order, column_order]

# Тепловая карта для p-значений с новым порядком
ht_p_value <- Heatmap(
  p_value_log_data, 
  name = "-Log10(P-value)", 
  column_title = "Genes", 
  row_title = "Cancer Types", 
  show_row_names = TRUE, 
  show_column_names = TRUE, 
  cluster_rows = FALSE,    # Использование заданного порядка строк
  cluster_columns = FALSE, # Использование заданного порядка столбцов
  col = p_value_colors
)

# Сохранение тепловых карт
output_dir <- "./"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Сохранение тепловой карты для корреляций
png(file = paste0(output_dir, "correlation_heatmap_bosutinib_auc_vs_expression_by_cancer_type.png"), width = 10, height = 10, units = "in", res = 600)
draw(ht_correlation)
dev.off()

# Сохранение тепловой карты для p-значений
png(file = paste0(output_dir, "p_value_heatmap_bosutinib_auc_vs_expression_by_cancer_type.png"), width = 10, height = 10, units = "in", res = 600)
draw(ht_p_value)
dev.off()

message("Тепловые карты успешно сохранены.")
