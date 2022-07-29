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
  legend.text = element_text(size = 12)
  )


setwd('D:/Github/KPMG-Contest')

cur_path <- getwd()

file_path <- 'D:/Github/KPMG-Contest/Tables/Figure_2.csv'
df <- read.csv(file_path)

col_names <- c('Game','Abb','Category', 'Operator', 'Users', 'Percent', 'Users_GZ','TGIZ','Genre_MAU', 'Genre TGIZ')
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
  geom_abline(intercept = 0, slope = 1, color = '#2A2A2A') +
  geom_jitter(aes(x = Users, y = Users_GZ, size = Users_GZ, color = fct_inorder(Category), alpha = TGIZ / 4)) +
  geom_text_repel(aes(x = Users, y = Users_GZ, label = Game), hjust=0.5, vjust=0.5) +
  scale_x_continuous(trans = 'log10', limits = c(0.2, 20)) +
  scale_y_continuous(trans = 'log10', limits = c(0.2, 20)) +
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
  xlab('MAU') +
  ylab('MAUZ') +
  labs(color = "Category", alpha = "TGIZ")

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


F1b_path <- paste('D:/Github/KPMG-Contest/Figures/','Figure_2b_3','.pdf', sep = '')
ggsave(
  F1b_path,
  plot = main2,
  scale = 1,
  width = 12,
  height = 6,
)

order <- c("MOBA", "FPS", "RTS","RPG","TBRPG")

order <-  c("TBRPG", "RPG", "RTS","FPS", "MOBA")

main4 <- ggplot(data = df) + 
  geom_bar(aes(x = Category, y = Users_GZ, fill = Category), stat = 'identity', alpha = 0.8) +
  scale_fill_nejm() +
  xlab('Genre') +
  ylab('MAUZ') + figure_theme

main4

F1c_path <- paste('D:/Github/KPMG-Contest/Figures/','Figure_2b_4','.pdf', sep = '')
ggsave(
  F1c_path,
  plot = main4,
  scale = 1,
  width = 8,
  height = 6,
)

main5 <- ggplot() + 
  geom_point(data = subset(df, Category  %in% c("MOBA")), aes(x = fct_inorder(Category), y = TGIZ, alpha = Genre_MAU),color = '#d48f28', size = 15, shape = 'square') +
  geom_violin(data = df, aes(x = fct_inorder(Category), y = TGIZ, fill =  Genre_MAU, alpha = Genre_MAU)) +
  geom_boxplot(data = df, aes(x = fct_inorder(Category), y = TGIZ,fill = Genre_MAU, alpha = Genre_MAU), width = 0.5) +
  scale_size(
    name = waiver(),
    breaks = waiver(),
    labels = waiver(),
    limits = NULL,
    range = c(10, 30),
    trans = "identity",
    guide = "legend"
  ) +
  scale_alpha(range = c(0.5,0.9)) +
  scale_fill_gradient(limits = c(0, 15), low = '#e0c000',high = '#b30838') +
  xlab('Genre') +
  ylab('TGIZ')+
  figure_theme +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         alpha = FALSE, 
         size = FALSE)  +
  labs(fill = 'Genre MAUZ')
#  scale_x_discrete(limit = order)

main5

F1c_path <- paste('D:/Github/KPMG-Contest/Figures/','Figure_2b_5','.pdf', sep = '')
ggsave(
  F1c_path,
  plot = main5,
  scale = 1,
  width = 8,
  height = 6,
)

