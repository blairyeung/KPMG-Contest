library(ggplot2)
library(ggrepel)
library(tidyverse)
library(ggsci)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

my_colors = c('#00549E', '#009FC2', '#936FB1', '#850C70', '#B30838', '#EE2E24', '#FFAD59', '#E0C000', '#91BDA6', '#6CB33F', '#00703C')
less_colors = c('#009FC2', '#936FB1' , '#D9839D' ,'#FFAD59', '#91BDA6', '#6CB33F')
              
m_share <- read.csv('D:/Github/KPMG-Contest/Tables/mobile_game_market_share.csv')

colnames(m_share) <- c('Game_type', 'Market_share', 'Order')




m_share2 <- m_share %>% 
  mutate(csum = rev(cumsum(rev(Market_share))), 
         pos = Market_share/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Market_share/2, pos))

m_plot <- ggplot(m_share2, aes(x = "" , y = Market_share, fill = fct_inorder(Game_type))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = Market_share),
            position = position_stack(vjust = 0.5) , color="white") +
  scale_fill_nejm() +
  blank_theme

m_plot
