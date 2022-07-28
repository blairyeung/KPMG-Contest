library(ggplot2)
library(ggrepel)
library(tidyverse)
library(ggsci)

figure_theme <- theme(
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 12),
  )


file_path <- 'D:/Github/KPMG-Contest/Tables/Figure_1.csv'
df <- read.csv(file_path)

col_names <- c('Game','Abb','Category', 'Operator', 'Users', 'Income', 'Percent', 'Users_GZ','TIGZ','Count')
colnames(df) <- col_names

percent_population_gen_z <- 0.181218652

mypal <- colorRampPalette(brewer.pal(6, "PuBu"))
mypal2 <- colorRampPalette(brewer.pal(5, "YlOrRd"))
mypal3 <- colorRampPalette(brewer.pal(6, "YlOrBr"))
mypal4 <- colorRampPalette(brewer.pal(6, "YlOrRd"))
mypal5 <- colorRampPalette(brewer.pal(6, "YlOrBr"))

main <- ggplot(data = df) +
  geom_point(aes(x = as.factor(Category), y = Income, size = Users_GZ, color = Percent/0.181218652, alpha = 0.7)) +
  geom_text_repel(aes(x = as.factor(Category), y = Income, label = Game, color = Percent/0.181218652), hjust=2, vjust=1) +
  scale_color_gradient(trans = 'pseudo_log',high = '#b30838',low = '#e0c000', oob = scales::squish) +
  scale_y_continuous(trans = 'log10', limits = c(0.01, 1000)) +
  scale_size(
    name = waiver(),
    breaks = waiver(),
    labels = waiver(),
    limits = NULL,
    range = c(5, 60),
    trans = "identity",
    guide = "legend"
  ) + 
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         alpha = FALSE, 
         size = FALSE)  

main

main2 <- ggplot(data = df) +
  geom_jitter(aes(x = Users, y = Income, size = Users_GZ, color = fct_inorder(Category), alpha = TIGZ / 4)) +
  geom_text_repel(aes(x = Users, y = Income, label = Game), hjust=2, vjust=1) +
  scale_y_continuous(trans = 'log10', limits = c(0.01, 2000), breaks = c(0.01,0.1,1,10,100,1000), labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(trans = 'log10', limits = c(0.01, 20)) +
  scale_alpha_continuous(limits = c(0.4,1)) +
  scale_color_nejm() +
  scale_size(
    name = waiver(),
    breaks = waiver(),
    labels = waiver(),
    limits = NULL,
    range = c(5, 60),
    trans = "identity",
    guide = "legend"
  ) + 
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         size = FALSE) +
  xlab('Monthly downloads (million)') +
  ylab('Monthly income (million dollars)') +
  labs(color = "Category", alpha = "TIGZ") +
  figure_theme

main2



main3 <- ggplot(data = df) +
  geom_point(aes(x = Users, y = Income, size = Users_GZ, color = Category, alpha = 0.7)) +
  geom_text_repel(aes(x = Users, y = Income, label = Game), hjust=2, vjust=1) +
  scale_y_continuous(trans = 'log10', limits = c(0.01, 2000)) +
  scale_x_continuous(trans = 'log10', limits = c(0.01, 20)) +
  scale_color_lancet() +
  scale_size(
    name = waiver(),
    breaks = waiver(),
    labels = waiver(),
    limits = NULL,
    range = c(5, 60),
    trans = "identity",
    guide = "legend"
  ) + 
  theme_minimal() +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         alpha = FALSE, 
         size = FALSE)

main3


F1b_path <- paste('D:/Github/KPMG-Contest/Figures/','Figure_1b_3','.pdf', sep = '')
ggsave(
  F1b_path,
  plot = main2,
  scale = 1,
  width = 12,
  height = 6,
)

main4 <- ggplot(data = df) + 
  geom_bar(aes(x = Count, y = Users_GZ, fill = Operator), stat = 'identity') +
  scale_fill_nejm() +
  figure_theme +
  xlab('Category') +
  ylab('Generation Z Users') +
  coord_polar(theta = "y") +
  figure_theme

main4
