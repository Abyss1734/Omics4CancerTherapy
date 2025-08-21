library(ggplot2)
library(readr)
library(glue)
library(dplyr)
library(ggrepel)
library(stringr)

# Загрузка данных
df <- read_csv("./cervical_cancer_sik3_pim-447_data.csv")

# Автоматическое определение названий
gene_name <- colnames(df)[2]  
drug_full <- colnames(df)[3]  
drug_name <- str_split(drug_full, "_")[[1]][1]  
cancer_type <- df$Cancer_Type[1]  # Оригинальный тип рака с слешами
cancer_type_filename <- str_replace_all(cancer_type, "/", "_")  # Для имени файла

# Переименование колонок
df <- df %>%
  select(1:5) %>%  # Берём первые 5 колонок
  rename(
    CellLine = 1,
    Depscore = 2,
    LFC = 3,
    CellName = 4
  )

# Автоматический расчёт размера шрифта для заголовка
plot_title <- glue("{drug_name} LFC vs {gene_name} Depscore for {cancer_type}")  # Используем оригинальное название
title_width <- nchar(plot_title)
title_size <- ifelse(title_width > 50, 18, 
                     ifelse(title_width > 30, 20, 24))

# Расчёт корреляции
corr <- cor.test(df$Depscore, df$LFC, method = "spearman")
r_value <- sprintf(corr$estimate, fmt = '%#.2f')
p_value <- sprintf(corr$p.value, fmt = '%#.4f')
if (corr$p.value < 0.0001) p_value <- "p<0.0001"

# Фильтрация данных (можно настроить под свои нужды)
highlighted_points <- df %>% filter(LFC <= -3)
highlight_cells <- df %>% filter(CellLine %in% c("ACH-000510", "ACH-000681"))

# Построение графика с фиксированным смещением аннотации
plot <- ggplot(df, aes(x = LFC, y = Depscore)) +
  geom_point(color = "black", fill = "#00BFFF", size = 5, shape = 21, stroke = 0.5, alpha = 0.9) +
  geom_point(data = highlighted_points, aes(x = LFC, y = Depscore), 
             color = "black", fill = "#50C878", size = 5, shape = 21, stroke = 0.5, alpha = 0.9) +
  geom_point(data = highlight_cells, aes(x = LFC, y = Depscore), 
             color = "black", fill = "#8A2BE2", size = 6, shape = 21, stroke = 0.5, alpha = 0.9) +
  geom_smooth(method = "lm", color = "black", linetype = "dotted", size = 1) +
  labs(
    title = plot_title,
    x = glue("{drug_name} LFC"), 
    y = glue("{gene_name} Depscore")
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.line = element_line(size = 1.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = title_size, hjust = 0.5),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  # Фиксированное смещение от верхнего правого края с рамкой
  annotate("label",  # Используем 'label' вместо 'text' для автоматической рамки
           label = glue("R = {r_value}, p = {p_value}"),
           x = Inf, y = Inf,  # Правый верхний угол
           vjust = 1.4,       # 1.3
           hjust = 2.9,     # 2.9, 2.45, 2, 1.55, 1.1
           size = 6,
           color = "black",
           fill = "white",          # Цвет фона рамки
           alpha = 1,            # Непрозрачность фона
           label.padding = unit(0.3, "lines"),  # Отступы внутри рамки
           label.r = unit(0.4, "lines"),       # Закругление углов
           label.size = 0.3) +     # Толщина границы рамки
  geom_text_repel(
    data = highlighted_points,
    aes(label = CellName),
    color = "black",
    bg.color = "white",
    bg.r = 0.2,
    size = 4,
    box.padding = 0.5,
    segment.color = NA
  ) +
  geom_text_repel(
    data = highlight_cells,
    aes(label = CellName),
    color = "black",
    bg.color = "white",
    bg.r = 0.2,
    size = 4,
    box.padding = 0.5,
    segment.color = NA
  )

# Генерация имени файла (в нижнем регистре, с тире вместо слешей)
file_name <- glue("{drug_name}_LFC_vs_{gene_name}_Depscore_for_{str_replace_all(cancer_type_filename, ' ', '_')}.png") %>% 
  str_to_lower()

# Сохранение графика
ggsave(
  file_name,
  plot = plot,
  width = 8,
  height = 8,
  dpi = 300
)

# Выводим информацию о сохранении
message(glue("График сохранён как: {file_name}"))