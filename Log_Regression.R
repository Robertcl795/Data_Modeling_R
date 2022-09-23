# Import libraries
library(rsample)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(car)

# Read CSV
accessibility_dataset = read_csv("accessibility_dataset.csv")

# Set random seed
set.seed(7)

# Randomize and shuffle dataset
cured_dataset <- accessibility_dataset[accessibility_dataset$excellence != 0,]

split_sample <- initial_split(cured_dataset, prop = .70)

# Assign groups
train_data <- training(split_sample)
test_data <- testing(split_sample)

#Verify dimensions of sets
dim(train_data)
dim(test_data)

# Create log and exp model piping data into model function, assign to variable 
log.model <- lm(log(ranking_latin_america) ~ excellence, train_data)
summary(log.model)

log.model.df <- data.frame(x = test_data$excellence,
                           y = exp(predict(log.model, test_data)))

ggplot(train_data, aes(x=excellence,y=ranking_latin_america)) +
  geom_point() + 
  geom_line(data = log.model.df, aes(x, y, color = "Log Model"), size = 1, linetype = 2) + 
  guides(color = guide_legend("Model Type"))


  # Create prediction model and actual/predicted dataframe
  prediction <- predict(log.model, test_data)
  prediction.df <- data.frame(Observado = test_data$ranking_latin_america,
                              Predecido = exp(prediction))
  # Graficar regresión y dataframe
  ggplot(prediction.df, aes(Observado,Predecido)) +
    geom_point(shape = 1) + geom_smooth(method = "glm", formula = y~ log(x),
                                        method.args = list(family = gaussian(link = 'log')))


#Plot predicted vs actual values, and add log fit
ggplot(prediction.df, aes(Actual,Predicted)) +
  geom_point(shape = 1) + stat_smooth(method = "lm", formula = y ~ log(x))

ggplot(test_data, aes(x=excellence,y=ranking_latin_america)) +
  geom_point() + 
  geom_line(data = prediction.df, aes(x, y, color = "Log Model"), size = 1, linetype = 2) + 
  guides(color = guide_legend("Model Type"))

ggplot(test_data, aes(excellence, ranking_latin_america))+
  geom_point(shape = 1) + stat_smooth(method = "lm", formula = log(y) ~ x)

