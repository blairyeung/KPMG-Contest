library(ggplot2)
library(ggrepel)
library(tidyverse)
library(ggsci)

num_games = 3
num_types = 5
num_total = num_games * num_types

sup_path = 'D:/Github/KPMG-Contest/Tables/Raw_'
sub_path = '.csv'


MOBA_abb = c('wzry', 'mjzpaj', 'mlol')
MOBA_games = c('Honor_of_Kings', 'Onmyoji_Arena', 'Mobile_League')


FPS_abb = c('mcf', 'mpubg', 'mwpzs')
FPS_games = c('Mobile_Crossfire', 'Mobile_PUBG', 'Ace_Force')

ARPG_abb = c('impact', 'mbh3', 'mgzlj')
ARPG_games = c('Genshin_Impact', 'Honkai_Impact', 'Princess_Connect')
# R^2 game

RTS_abb = c('mhszz', 'mblct', 'mbwlb')
RTS_games = c('Clash_Royale', 'Clash_of_Clans', 'Carrot_Fantasy')

TBRGB_abb = c('myys', 'mmhxy', 'myyzy')
TBRGB_games = c('Onmyoji', 'Fantasy_Westward_Journey', 'Night_of_Full_Moon')

Game_types = c('MOBA','FPS', 'ARPG', 'RTS', 'TBRGB')
Game_abb = rbind(MOBA_abb, FPS_abb, ARPG_abb, RTS_abb, TBRGB_abb)
Game_names = rbind(MOBA_games, rbind(FPS_games, rbind(ARPG_games, rbind(RTS_games, TBRGB_games))))


games = c(MOBA_games, ARPG_games)

final_expected = c(1: 5 * num_total)
final_actual = c(1: 5 * num_total)
game_name = c(1: 5 * num_total)
# Raw Data INPUT

variance_vect = c(0.009, 0.0065, 0.0072)

print(Game_abb)
game_abb <- Game_abb[15]
print(game_abb)

for (w in 1:num_types){
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
  for (f in c(1:num_games)){
    # (paths[f])
    path <- paths[f]
    # print(path)
    df <- read.csv(path)
    # print(df)
    colnames(df) <- c('age', 'value')
    
    
    fi = 0
    
    var_1 = variance_vect[f]
    # var_2 = 0.1001
    
    # mean_age = c(17, 25, 35, 45, 55)
    mean_age = c(18, 21, 31, 42, 60)
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
        # substract = (e^( - (diff^2)/ (2 * (var_2^2) ) ) )/ (var_2 * sqrtpi) * data
        substract = 0
        fi = fi + add - substract
        # print(diff)
        # print(add)
      }
      
      vect[i] = fi
      # print(fi)
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
    
    compare_data <- data.frame(actual = df$value,
                               expected = normalized,
                               game = game_name,
                               type = Game_types[w]
    )
    # out_path <- paste('D:/Github/KPMG-Contest/Tables/','Processed_',games[f],'.csv', sep = '')
    # print(out_path)
    # write.csv(compare_data, out_path)
    position <- w + f
    if (position == 2){
      total <- compare_data
    } else{
      total <- rbind(total, compare_data)
    }
    # print(f)
  }
}


# Augmented data plot

ggplot() +
  geom_line(data = expceted_data, aes(x = age, y = val))


# Unaugmented data plot

ggplot() +
  geom_line(data = normalized_data, aes(x = age, y = val*100)) +
  geom_line(data = raw_data, aes(x = age, y = val, color = 'red'))

compare_data_full <- data.frame(actual = final_actual,
                              expected = normalized,
                           game = game_name
)

# Model robustness

ggplot(total, aes(x = actual, y = expected*100, fill = fct_inorder(game), color = fct_inorder(game))) +
  stat_summary(fun.compare_data= mean_cl_normal) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  geom_smooth(method='lm', linetype=0) +
  xlim(0, 60) +
  ylim(0, 60) +
  facet_wrap(vars(type))
# Model

r2 <- data.frame(x = total$actual, y= total$expected)

rss <- sum((total$expected- total$actual) ^ 2)  ## residual sum of squares
tss <- sum((total$actual - mean(total$actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
print(rsq)

ggplot() +
  geom_line(data = normalized_data, aes(x = age, y = val*100)) +
  geom_line(data = raw_data, aes(x = age, y = val, color = 'red'))



