df <- read.csv('D:/Github/KPMG-Contest/Tables/Raw_wzry.csv')

colnames(df) <- c('age', 'value')

i = 22

fi = 0

var_1 = 0.015
# var_2 = 0.1001

mean_age = c(17, 25, 35, 45, 55)
const = 500

sqrtpi = sqrt(2 * 3.14159)

e = 2.71828

fi = 0

for (ind in 1:nrow(df)){
  print(i)
  data = df$value[ind]
  print(data)
  diff = (i - mean_age[ind]) / const
  add =  data * (e^( - (diff^2)/ (2 * (var_1^2) ) ) )/ (var_1 * sqrtpi)
  substract = data * (e^( - (diff^2)/ (2 * (var_2^2) ) ) )/ (var_2 * sqrtpi)
  fi = fi + add - substract
  # print(diff)
  # print(add)
  # print(diff^2)
  # print(((diff^2)/ (2 * (var_2^2) ) ))
}


print(fi)


print (df)

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
    add = (e^( - (diff^2)/ (2 * (var_1^2) ) ) )/ (var_1 * sqrtpi) * data
    substract = (e^( - (diff^2)/ (2 * (var_2^2) ) ) )/ (var_2 * sqrtpi) * data
    substract = 0
    fi = fi + add - substract
    # print(diff)
    # print(add)
  }
  
  vect[i] = fi
  print(fi)
}

expceted_data <- data.frame(age = c(1:55),
                 val = vect
)

raw_data <- data.frame(age = mean_age, 
                       val = df$value)

# Compare raw

unnormalized <- c(0,0,0,0,0)


for (k in c(1:4)){
    start = mean_age[k]
    end = mean_age[k+1]
    print(start)
    print(end)
    unnormalized[k+1] =  sum(vect[start:end])
}

unnormalized[1] = sum(vect[1:17])

print(unnormalized)


unnormalized_data <- data.frame(age = mean_age,
                            val = unnormalized
)

norm_fact <- sum(unnormalized)

normalized <- c(1:5)

for (i in c(1:5)){
  normalized[i] = unnormalized[i] / norm_fact
}


normalized_data <- data.frame(age = mean_age,
                                val = normalized
)
print(normalized)

# Unaugmented data plot


ggplot() +
  geom_line(data = normalized_data, aes(x = age, y = val*100)) +
  geom_line(data = raw_data, aes(x = age, y = val, color = 'red'))

# Augmented data plot

ggplot() +
  geom_line(data = expceted_data, aes(x = age, y = val))

