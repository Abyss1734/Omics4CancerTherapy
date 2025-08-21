# Загрузка необходимых библиотек
library(ComplexHeatmap)
library(readr)
library(circlize)

# Пути к файлам
correlation_file <- "./correlation_depscore_vs_expression_data.csv"
pvalue_file <- "./p_value_depscore_vs_expression_data.csv"

# --- Загрузка и подготовка корреляций ---
data_corr <- read.csv(correlation_file, row.names = 1, check.names = FALSE)
rownames(data_corr) <- gsub("_expression$|_depscore$", "", rownames(data_corr))
colnames(data_corr) <- gsub("_expression$|_depscore$", "", colnames(data_corr))
data_corr <- data_corr[order(rownames(data_corr)), order(colnames(data_corr))]
mat_corr <- as.matrix(data_corr)
class(mat_corr) <- "numeric"

# --- Загрузка и подготовка p-value ---
data_pval <- read.csv(pvalue_file, row.names = 1, check.names = FALSE)
rownames(data_pval) <- gsub("_expression$|_depscore$", "", rownames(data_pval))
colnames(data_pval) <- gsub("_expression$|_depscore$", "", colnames(data_pval))
data_pval <- data_pval[order(rownames(data_pval)), order(colnames(data_pval))]
mat_pval <- as.matrix(data_pval)
class(mat_pval) <- "numeric"

# Преобразование p-value в -log10(p)
pval_log <- -log10(mat_pval)

# --- Цветовые схемы ---
correlation_colors <- colorRamp2(c(-0.4, 0, 0.4), c("blue", "white", "red"))
p_value_colors <- colorRamp2(c(0, 1.1, 2), c("grey", "white", "blue"))

# --- Тепловая карта корреляций ---
ht_corr <- Heatmap(
  mat_corr, 
  name = "Correlation", 
  column_title = "Genes (Expression)", 
  row_title = "Genes (DepScore)", 
  show_row_names = TRUE, 
  show_column_names = TRUE, 
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  col = correlation_colors
)

# --- Тепловая карта p-value (-log10) ---
ht_pval <- Heatmap(
  pval_log, 
  name = "-log10(p-value)", 
  column_title = "Genes (Expression)", 
  row_title = "Genes (DepScore)", 
  show_row_names = TRUE, 
  show_column_names = TRUE, 
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  col = p_value_colors
)

# --- Сохранение тепловой карты корреляций ---
output_corr_path <- "./correlation_depscore_vs_expression.png"
png(file = output_corr_path, width = 10, height = 10, units = "in", res = 600)
draw(ht_corr)
dev.off()
cat("✅ Тепловая карта корреляций сохранена по пути:", output_corr_path, "\n")

# --- Сохранение тепловой карты p-value ---
output_pval_path <- "./p_value_depscore_vs_expression.png"
png(file = output_pval_path, width = 10, height = 10, units = "in", res = 600)
draw(ht_pval)
dev.off()
cat("✅ Тепловая карта p-value сохранена по пути:", output_pval_path, "\n")
