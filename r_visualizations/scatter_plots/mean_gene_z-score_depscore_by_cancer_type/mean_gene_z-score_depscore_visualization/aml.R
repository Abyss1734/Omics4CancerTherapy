library(ggplot2)
library(readr)
library(dplyr)
library(ggrepel)

# Загрузка объединённого файла
data <- read_csv("../mean_gene_z-score_depscore_data/mean_gene_z-score_depscore_aml.csv")

# Удаляем строки с NA
data <- na.omit(data)

# Переименование колонок, если нужно
colnames(data) <- c("Gene", "Depscore", "Zscore")

# Выделяем гены для подсветки
highlight_genes <- c("FLT3", "BCL2", "RUNX1", "GATA1", "KIT", "JAK2","NOTCH1", "TAL1","TLX3",
                     "HOXA9", "HOXA10", "TLX1", "MEF2D", "KMT2A", "IL3RA", "ETV1", "ETV4", "ETV5")

# Отмечаем гены для подсветки
data$highlight <- ifelse(data$Gene %in% highlight_genes, "highlight", "normal")

# Построение графика
plot <- ggplot(data, aes(x = Zscore, y = Depscore)) +
  geom_point(data = subset(data, highlight == "normal"),
             color = "#A6CEE3", size = 5, shape = 16, alpha = 0.9) +
  geom_point(data = subset(data, highlight == "highlight"),
             color = "red", size = 5, shape = 16, alpha = 0.9) +
  geom_label_repel(data = subset(data, highlight == "highlight"),
                   aes(label = Gene), 
                   color = "red", size = 4, box.padding = 0.5, max.overlaps = 50) +
  scale_x_continuous(breaks = seq(-2.5, 2, by = 0.5)) +
  scale_y_continuous(breaks = seq(-2.6, 0.6, by = 0.2)) +
  labs(x = "Z-score", y = "Depscore") +
  ggtitle("AML") +
  theme_minimal(base_size = 15) +
  theme(
    axis.line = element_line(size = 1.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 24, hjust = 0.5),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Сохраняем график
ggsave("./aml.png",
       plot = plot, width = 8, height = 8, dpi = 300, units = "in")
