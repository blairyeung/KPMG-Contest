library(ggplot2)
library(ggrepel)
library(tidyverse)
library(ggsci)

figure_theme <- theme(
  axis.title.x = element_text(size = 24),
  axis.title.y = element_text(size = 24),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 16),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 16),
  strip.text = element_text(size = 16),
)

file_path <- 'D:/Github/KPMG-Contest/Tables/Figure_1.csv'
df <- read.csv(file_path)

col_names <- c('Game','Abb','Category', 'Operator', 'Users', 'Income', 'Percent', 'Users_GZ','TGIZ','Count','Genre_MAU', 'Genre TGIZ')
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
  geom_jitter(aes(x = Users, y = Income, size = Users_GZ, color = fct_inorder(Category), alpha = TGIZ / 4)) +
  geom_text_repel(aes(x = Users, y = Income, label = Game), hjust=2, vjust=1) +
  scale_y_continuous(trans = 'log10', limits = c(0.01, 2000), breaks = c(0.01,0.1,1,10,100,1000), labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(trans = 'log10', limits = c(0.01, 20)) +
  scale_alpha_continuous(limits = c(0.4,1), label = c('160', '200', '240', '280', '320', '360', '400')) +
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
  ylab('Monthly income (million dollars)') +
  labs(color = "Genre", alpha = "TGIZ") +
  figure_theme

main2


main3  <- ggplot(data = df) +
  geom_jitter(aes(x = Users, y = Users_GZ, size = Income, color = fct_inorder(Category), alpha = TGIZ / 4)) +
  geom_text_repel(aes(x = Users, y = Users_GZ, label = Game), hjust=2, vjust=1) +
  scale_y_continuous(trans = 'log10', limits = c(0.01, 20), breaks = c(0.01,0.1,1,10,100), labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(trans = 'log10', limits = c(0.01, 20)) +
  scale_size_continuous(limits = c(0.4,1)) +
  scale_alpha_continuous(limits = c(0.4,1), label = c('160', '200', '240', '280', '320', '360', '400')) +
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
  labs(color = "Genre", alpha = "TGIZ") +
  
  figure_theme

main3


F1b_path <- paste('D:/Github/KPMG-Contest/Figures/','Figure_1b_3','.pdf', sep = '')
ggsave(
  F1b_path,
  plot = main2,
  scale = 1,
  width = 12,
  height = 8,
)

order <- c("MOBA", "FPS", "RTS","RPG","TBRPG")

order <-  c("TBRPG", "RPG", "RTS","FPS", "MOBA")

main4 <- ggplot(data = df) + 
  geom_bar(aes(x = fct_inorder(Category), y = Users_GZ, fill  = Operator), stat = 'identity') +
  scale_fill_nejm() +
  xlab('Genre') +
  ylab('MAUZ') +
  figure_theme
#  scale_x_discrete(limit = order)
 

main4


main5 <- ggplot(data = df) + 
  geom_violin(aes(x = fct_inorder(Category), y = TGIZ, fill =  Genre_MAU, alpha = Genre_MAU)) +
  geom_boxplot(aes(x = fct_inorder(Category), y = TGIZ,fill = Genre_MAU, alpha = Genre_MAU), width = 0.5) +
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
  scale_fill_gradient(limits = c(0, 4.5), low = '#e0c000',high = '#b30838') +
  xlab('Genre') +
  ylab('TGIZ')+
  figure_theme +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         alpha = FALSE, 
         size = FALSE)  +
  labs(fill = 'Genre MAUZ')
#  scale_x_discrete(limit = order)

main5

F1c_path <- paste('D:/Github/KPMG-Contest/Figures/','Figure_1b_5','.pdf', sep = '')
ggsave(
  F1c_path,
  plot = main5,
  scale = 1,
  width = 8,
  height = 8,
)

F1c_path <- paste('D:/Github/KPMG-Contest/Figures/','Figure_1b_4','.pdf', sep = '')
ggsave(
  F1c_path,
  plot = main4,
  scale = 1,
  width = 8,
  height = 8,
)

