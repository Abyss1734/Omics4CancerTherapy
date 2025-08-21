library(readr)
library(dplyr)
library(ggplot2)

# Загрузка данных
file_path <- "C:/Users/rusla/OneDrive/Рабочий стол/Диплом/Гены/chembl_uniprot_ttd/chembl/threshold_selection/Проверка отсечки pChEMBL_8.csv"
df <- read_csv(file_path)

# Преобразование в числовой формат
df <- df %>%
  mutate(`pChEMBL value` = as.numeric(`pChEMBL value`))

# Разделение по метке "Да"/"Нет"
df_yes <- df %>% filter(`Drug Mechanism ChEMBL` == "Да")
df_no <- df %>% filter(`Drug Mechanism ChEMBL` == "Нет")

# Подсчитаем, сколько "Да" на каждый ген
gene_counts <- df_yes %>%
  count(Gene, name = "n_yes")

# Присоединим это к "Нет", чтобы знать сколько выбрать
df_no_selected <- df_no %>%
  left_join(gene_counts, by = "Gene") %>%
  filter(!is.na(n_yes)) %>%  # оставляем только те гены, которые есть в "Да"
  group_by(Gene) %>%
  arrange(desc(`pChEMBL value`)) %>%
  mutate(row = row_number()) %>%
  filter(row <= n_yes) %>%
  ungroup() %>%
  select(-row)

# Добавим колонку для "Да" и "Нет"
df_yes <- df_yes %>% mutate(Mechanism = "Да")
df_no_selected <- df_no_selected %>% mutate(Mechanism = "Нет")

# Объединяем данные
df_grouped <- bind_rows(df_yes, df_no_selected)

# Границы оси Y по данным из "Да"
ymin <- min(df_yes$`pChEMBL value`, na.rm = TRUE)
ymax <- max(df_yes$`pChEMBL value`, na.rm = TRUE)

# График
ggplot(df_grouped, aes(x = Gene, y = `pChEMBL value`, fill = Mechanism)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  
  # Точки с цветной заливкой
  geom_jitter(aes(color = Mechanism), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              alpha = 1, size = 1) +
  
  scale_fill_manual(values = c("Да" = "#00bfff", "Нет" = "#00ff00"),
                    labels = c("Да" = "Drug Mechanism", "Нет" = "Activity")) +
  scale_color_manual(values = c("Да" = "black", "Нет" = "red"),
                     labels = c("Да" = "Drug Mechanism", "Нет" = "Activity")) +
  ylim(ymin, ymax) +
  labs(title = "Dynamic threshold selection",
       x = "Gene", y = "pChEMBL value", fill = "Drug Type", color = "Drug Type") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    legend.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(hjust = 0.5)
  )

# Сохранение графика
ggsave("threshold_selection_boxplot.png", width = 12, height = 6, dpi = 300)