library(ggplot2)
library(ggrepel)
library(tidyverse)
library(ggsci)

num_games = 3

games = c('Honor_of_Kings', 'League_of_Legends', 'Mobile_League')
paths = c('D:/Github/KPMG-Contest/Tables/Raw_wzry.csv', 'D:/Github/KPMG-Contest/Tables/Raw_lol.csv' , 'D:/Github/KPMG-Contest/Tables/Raw_mlol.csv')

final_expected = c(1: 5 * num_games)
final_actual = c(1: 5 * num_games)
game_name = c(1: 5 * num_games)
# Raw Data INPUT

variance_vect = c(0.009, 0.0065, 0.0072)


for (f in c(1:3)){
  # (paths[f])
  df <- read.csv(paths[f])
  # print(df)
  colnames(df) <- c('age', 'value')
  
  i = 22
  
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
                             game = game_name
  )
  out_path <- paste('D:/Github/KPMG-Contest/Tables/','Processed_',games[f],'.csv', sep = '')
  print(out_path)
  write.csv(compare_data, out_path)
  if (f == 1){
    total <- compare_data
  } else{
    total <- rbind(total, compare_data)
  }
  print(f)
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

ggplot(total, aes(x = actual, y = expected*100, fill = game, color = game)) + 
  geom_point() +
  stat_summary(fun.compare_data= mean_cl_normal) + 
  geom_smooth(method='lm', linetype=0) +
  geom_abline(intercept = 0, slope = 1) +
  scale_fill_nejm() + 
  scale_color_nejm() +
  xlim(0, 60) +
  ylim(0, 60) +
  theme_minimal()

# Model

ggplot() +
  geom_line(data = normalized_data, aes(x = age, y = val*100)) +
  geom_line(data = raw_data, aes(x = age, y = val, color = 'red'))



