rm(list = ls())

setwd("E:\\Rworkspace\\TPM")
library(ggplot2) 
library(dplyr)
library(tidyr)

df <- read.table("ik-TPM-YR.txt", header = 1, check.names = F, sep = "\t")
> df
    group tissue    value
1    cold  brain 95.31329
2    cold  brain 96.89150
3    cold  heart 19.58245
4    cold  heart 21.06911
5 control  brain 66.12971
6 control  brain 64.72437
7 control  heart 14.04700
8 control  heart 13.49484

summary_df <- df %>%
  group_by(group, tissue) %>%
  summarise(
    mean_value = mean(value),
    se_value = sd(value) / sqrt(n()),
    .groups = 'drop'
  )

# 执行t检验来比较冷组和对照组在不同组织上的差异
t_test_results <- df %>%
  group_by(tissue) %>%
  summarise(
    t_test_pvalue = t.test(value ~ group)$p.value,
    .groups = 'drop'
  )

# 合并计算结果
summary_df <- left_join(summary_df, t_test_results, by = "tissue")

# 绘制折线图
p1 <- ggplot(summary_df, aes(x = group, y = mean_value, color = tissue, group = tissue)) +
  geom_line(aes(linetype = tissue)) +  # 添加折线
  geom_point(size = 3) +  # 添加数据点
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2) +  # 添加误差条
  theme_minimal() +  # 使用简洁主题
  labs(
    title = "Mean Values with SE and Tissue Comparison",
    x = "Group",  # x轴标签
    y = "Mean Value",  # y轴标签
    color = "Tissue",  # 图例标题
    linetype = "Tissue"  # 图例标题
  ) +
  theme(panel.background = element_rect(fill = "white"))+coord_cartesian(ylim = c(0, 115))

# 计算cold-brain与control-brain的显著性和cold-heart与control-heart的显著性，保存到变量sign中
sign <- df %>%
  filter(tissue %in% c("brain", "heart")) %>%
  group_by(tissue) %>%
  summarise(
    p_value = t.test(value ~ group)$p.value,  # 计算每个组织的t检验p值
    significance = case_when(
      p_value < 0.001 ~ "***",  # p值小于0.001则标记为 ***
      p_value < 0.01 ~ "**",    # p值小于0.01则标记为 **
      p_value < 0.05 ~ "*",     # p值小于0.05则标记为 *
      TRUE ~ "ns"               # p值大于0.05则标记为 ns（不显著）
    ),
    .groups = 'drop'
  )

# 查看显著性结果，添加到图中
print(sign)
p1

pdf("TPM-YR.pdf",width=4,height=4)
  p1
dev.off()
