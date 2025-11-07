# 使用R画abundance柱状图

library(readr)
library(dplyr)
library(ggplot2)

# 1) 读文件
df <- read_tsv("/Users/luolin/Downloads/sample1_coverm_bins.tsv")

# 2) 自动识别并重命名关键列
rel_col   <- grep("Relative Abundance", names(df), value = TRUE, ignore.case = TRUE)
count_col <- grep("Read Count|\\bcount\\b", names(df), value = TRUE, ignore.case = TRUE)
mean_col  <- grep("\\bMean\\b", names(df), value = TRUE, ignore.case = TRUE)

df <- df %>%
  rename(
    genome = 1,
    relative_abundance = all_of(rel_col),
    count = all_of(count_col),
    mean  = all_of(mean_col)
  )

# 3) 如果已经是百分比（列名里有 %），就直接用；否则把 0~1 乘以 100
if (grepl("%", rel_col)) {
  df$rel_pct <- df$relative_abundance
} else {
  df$rel_pct <- ifelse(df$relative_abundance <= 1, df$relative_abundance * 100, df$relative_abundance)
}

# 4) 取前 20 个 MAG，按丰度从高到低
topN <- df %>%
  select(genome, rel_pct) %>%
  arrange(desc(rel_pct)) %>%
  slice_head(n = 20) %>%
  mutate(genome = factor(genome, levels = rev(genome)))

# 5) 画横向柱状图
p <- ggplot(topN, aes(x = genome, y = rel_pct)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 MAGs by Relative Abundance (sample1)",
    x = "MAG (bin)",
    y = "Relative abundance (%)"
  ) +
  theme_minimal(base_size = 12)

print(p)
