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
  panel.background = element_blank(),
  strip.background = element_blank(),
  strip.text = element_text(size = 16),
  axis.line.x.bottom=element_line(color="grey"),
  axis.line.y.left=element_line(color="grey")
)


num_games = 3
num_types = 5
num_total = num_games * num_types

sup_path = 'D:/Github/KPMG-Contest/Tables/Raw_'
sub_path = '.csv'

mb_org_file <- read.csv('D:/Github/KPMG-Contest/Tables/Figure_1.csv')

col_names <- c('Game','Abb','Category', 'Operator', 'Users', 'Percent', 'Users_GZ','TGIZ','Genre_MAU', 'Genre TGIZ')
colnames(mb_org_file) <- col_names

MOBA_abb = c('wzry', 'mjzpaj', 'mlol')
MOBA_games = c('Honor_of_Kings', 'Onmyoji_Arena', 'Mobile_League')


FPS_abb = c('mcf', 'mpubg', 'mwpzs')
FPS_games = c('Mobile_Crossfire', 'Mobile_PUBG', 'Ace_Force')

ARPG_abb = c('impact', 'mbh3', 'mmrfz')
ARPG_games = c('Genshin_Impact', 'Honkai_Impact', 'Arknights')

RTS_abb = c('mhszz', 'mblct', 'mbwlb')
RTS_games = c('Clash_Royale', 'Clash_of_Clans', 'Carrot_Fantasy')

TBRPG_abb = c('myys', 'mmhxy', 'mlscs')
TBRPG_games = c('Onmyoji', 'Fantasy_Westward_Journey', 'Heartstone')

Game_types = c('MOBA','FPS', 'RPG', 'RTS', 'TBRPG')
Game_abb = rbind(MOBA_abb, FPS_abb, ARPG_abb, RTS_abb, TBRPG_abb)
Game_names = rbind(MOBA_games, rbind(FPS_games, rbind(ARPG_games, rbind(RTS_games, TBRPG_games))))


percentage_gen_z = c(1:15)
name_genz = c(1:15)

index = 0

games = c(MOBA_games, ARPG_games)

final_expected = c(1: 5 * num_total)
final_actual = c(1: 5 * num_total)
game_name = c(1: 5 * num_total)
game_name_full = c(1: 60 * num_total)
game_player_full = c(1: 60 * num_total)
# Raw Data INPUT

# variance_vect = c(0.008, 0.008, 0.008)
variance_vect = c(0.01, 0.01, 0.01)
variance_vect_2 = c(0.8,0.8,0.8)

print(Game_abb)
game_abb <- Game_abb[15]
print(game_abb)

for (w in 1:num_types){
  
  # Initialize paths
  vec_start <- w
  vec_mid <- w + 5
  vec_end <- w + 10
  games <- c(Game_names[vec_start], Game_names[vec_mid], Game_names[vec_end])
  game_abb <- c(Game_abb[vec_start], Game_abb[vec_mid], Game_abb[vec_end])
  print(game_abb)
  print(game_name)
  paths = c(1:3)
  for (i in 1:num_games){
    paths[i] = paste(sup_path ,game_abb[i],sub_path, sep = '')
  }
  
  # Main loop
  for (f in c(1:num_games)){
    
    index <- index +1
    
    path <- paths[f]
    # read file
    df <- read.csv(path)
    colnames(df) <- c('age', 'value')
    
    
    fi = 0
    
    var_1 = variance_vect[f]
    var_2 = variance_vect_2[f]
    
    # mean_age = c(17, 25, 35, 45, 55)
    mean_age = c(17, 21, 31, 42, 60)
    const = 500
    
    sqrtpi = sqrt(2 * 3.14159)
    
    e = 2.71828
    
    vect <- c(1:55)
    
    for (v in c(1:12)){
      vect[v] = 0
    }
    
    
    for (i in c(5:55)){
      fi = 0
      
      for (ind in 1:nrow(df)){
        data <- df$value[ind]
        # data <- 1
        # print(data)
        diff = (i - mean_age[ind]) / const
        # beta = i^0.2
        beta = 1
        add = (beta * e^( - (diff^2)/ (2 * (var_1^2) ) ) )/ (var_1 * sqrtpi) * data
        substract = (beta * e^( - (diff^2)/ (2 * (var_2^2) ) ) )/ (var_2 * sqrtpi) * data
        # substract = 0
        fi = fi + add - substract
        # print(diff)
        # print(add)
      }
      
      vect[i] = fi
      # print(fi)
    }
    
    for (i in 1:55){
      if (vect[i] < 0){
        vect[i] = 0
      }
    }
    
    
    expceted_data <- data.frame(age = c(1:55),
                                val = vect
    )
    
    median_age  = c(17, 25, 35, 45, 55)
    # median_age = mean_age
    
    raw_data <- data.frame(age = median_age, 
                           val = df$value)
    
    # Compare raw
    
    unnormalized <- c(0,0,0,0,0)
    
    
    for (k in c(1:4)){
      start = median_age[k]
      end = median_age[k+1]
      unnormalized[k+1] =  sum(vect[start:end])
    }
    
    unnormalized[1] = sum(vect[1:17])
    
    
    
    unnormalized_data <- data.frame(age = median_age,
                                    val = unnormalized
    )
    
    norm_fact <- sum(unnormalized)
    
    normalized <- c(1:5)
    
    for (i in c(1:5)){
      normalized[i] = unnormalized[i] / norm_fact
    }
    
    
    normalized_data <- data.frame(age = median_age,
                                  val = normalized
    )
    
    for (v in c(1:5)){
      final_expected[5 * (f-1) + v] <- normalized[v]
    }
    
    for (v in c(1:5)){
      final_actual[5 * (f-1) + v] <- df$value[v]
    }
    
    for(v in c(1:5)){
      game_name[5 * (f-1) + v] <- games[f]
    }
    
    game_name_full = c(1:55)
    game_player_full = c(1:55)
    for(v in c(1:55)){
      game_name_full[v] <- games[f]
      game_player_full[v] <- mb_org_file$Users[index]
    }
    
    types = c(1:5)
    types_new = c(1:55)
    
    for (v in c(1:5)){
      types[v] = Game_types[w]
    }
    
    for (v in c(1:55)){
      types_new[v] = Game_types[w]
    }
    
    
    compare_data <- data.frame(actual = df$value,
                               expected = normalized,
                               game = game_name,
                               type = types
    )
    
    nom <- sum(vect)
    
    for (i in 1:55){
      vect[i] = vect[i]/nom
    }
    
    augmented_data <- data.frame(age = c(1:55),
                                 val = vect,
                                 game = game_name_full,
                                 type = types_new,
                                 players = game_player_full
    )
    
    
    # out_path <- paste('D:/Github/KPMG-Contest/Tables/','Processed_',games[f],'.csv', sep = '')
    # print(out_path)
    # write.csv(compare_data, out_path)
    position <- w + f
    if (position == 2){
      total <- compare_data
      augment_total <- augmented_data
    } else{
      total <- rbind(total, compare_data)
      augment_total <- rbind(augment_total, augmented_data)
    }
    # print(f)
    
    gz <- 0
    
    for (i in 12:27){
      gz = gz + vect[i]  
    }
    print(index)
    name_genz[index] <- games[f]
    percentage_gen_z[index] <- gz
  }
}


out_path <- paste('D:/Github/KPMG-Contest/Tables/','Processed_',total,'.csv', sep = '')
print(out_path)
write.csv(compare_data, out_path)


out_path2 <- paste('D:/Github/KPMG-Contest/Tables/','Subtotal','.csv', sep = '')
print(out_path)
write.csv(augment_total, out_path2)


# Augmented data plot

augmented <- ggplot(data = augment_total, aes(x = age, y = val, color = fct_inorder(game), fill = fct_inorder(game))) +
  geom_line() +
  geom_area(alpha = 0.1, position = 'dodge') +
  annotate(geom = "rect", xmin = 12, xmax = 27, ymin = 0, ymax = 0.065,
           fill = "orange", alpha = 0.2) +
  facet_grid(cols = vars(fct_inorder(type))) + 
  xlab('Age') +
  ylab('Frequency') +
  labs(color = 'Game', fill = 'Game') +
  figure_theme

augmented

augmented_unnorm <- ggplot() +
  geom_area(data = subset(augment_total, type == 'MOBA'), aes(x = age, y = val * players, alpha = 0.2, fill = 'red'), position = 'stack') +
  geom_area(data = subset(augment_total, type == 'FPS'), aes(x = age, y = val * players,  fill = fct_inorder(game)), alpha = 0.2, position = 'stack') +
  geom_area(data = subset(augment_total, type == 'RPG'), aes(x = age, y = val * players, fill = fct_inorder(game)), alpha = 0.2, position = 'stack') +
  geom_area(data = subset(augment_total, type == 'RTS'), aes(x = age, y = val * players, , fill = fct_inorder(game)), alpha = 0.2, position = 'stack') +
  geom_area(data = subset(augment_total, type == 'TBRPG'), aes(x = age, y = val * players,  fill = fct_inorder(game)), alpha = 0.2, position = 'stack') +
  annotate(geom = "rect", xmin = 12, xmax = 27, ymin = 0, ymax = 0.35,
           fill = "orange", alpha = 0.2) +
  xlab('Age') +
  ylab('Frequency') +
  labs(color = 'Game', fill = 'Game') +
  figure_theme

augmented_unnorm

# Unaugmented data plot

ggplot() +
  geom_line(data = normalized_data, aes(x = age, y = val*100)) +
  geom_line(data = raw_data, aes(x = age, y = val, color = 'red'))

compare_data_full <- data.frame(actual = final_actual,
                                expected = normalized,
                                game = game_name
)

# Model robustness

ggplot(total, aes(x = actual, y = expected*100, color = fct_inorder(game))) +
  stat_summary(fun.compare_data= mean_cl_normal) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(size = expected)) +
  geom_smooth(method='lm', linetype=0, aes(fill = fct_inorder(game))) +
  xlim(0, 60) +
  ylim(0, 60) +
  facet_wrap(vars(type))
# Model

r2 <- data.frame(x = total$actual, y= total$expected*100)

rss <- sum((total$expected- total$actual) ^ 2)  ## residual sum of squares
tss <- sum((total$actual - mean(total$actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
print(rsq)

ggplot() +
  geom_line(data = normalized_data, aes(x = age, y = val*100)) +
  geom_line(data = raw_data, aes(x = age, y = val, color = 'red'))


F1a_path <- paste('D:/Github/KPMG-Contest/Figures/','Figure_1augment','.pdf', sep = '')
ggsave(
  F1a_path,
  plot = augmented,
  scale = 1,
  width = 22,
  height = 5,
)

out_path <- paste('D:/Github/KPMG-Contest/Tables/','Processed_data','.csv', sep = '')
write.csv(data.frame(game = name_genz, percentage_gen_z), out_path)

