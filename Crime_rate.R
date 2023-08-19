library(h2o)
library(tidyverse)
library(data.table)
library(rstudioapi)
library(skimr)
library(faraway)
library(inspectdf)
library(mice)
library(plotly)
library(highcharter)
library(recipes)
library(caret)
library(purrr)
library(graphics)
library(Hmisc)
library(glue)

crime_data <- fread('crimes.csv')
crime_data %>%  skim() %>% view()

lapply(crime_data, range)

colnames(crime_data)

target <- 'ViolentCrimesPerPop'
features <- crime_data %>% select(-ViolentCrimesPerPop) %>% names()

formula <- as.formula(paste(target, paste(features, collapse = ' + '), sep = ' ~ '))
glm <- glm(formula = formula, data = crime_data)
glm %>% summary()

coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
features <- features[!features %in% coef_na]

formula <- as.formula(paste(target, paste(features, collapse = ' + '), sep = ' ~ '))
glm <- glm(formula = formula, data = crime_data)
glm %>% summary()


while (glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[1] >= 3){
  afterVIF <- glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[-1] %>% names()
  formula <- as.formula(paste(target, paste(afterVIF, collapse = ' + '), sep = ' ~ '))
  glm <- glm(formula, data = crime_data)
}

glm %>% faraway::vif() %>% sort(decreasing = T) %>% names() -> features

crime_data <- crime_data %>% select(ViolentCrimesPerPop, features)
crime_data %>% glimpse()
crime_data <- crime_data %>% as.data.frame

# crime_data[,-1] <- crime_data[,-1] %>% scale() %>% as.data.frame()


h2o.init()
h2o_data <- crime_data %>% as.h2o

h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'ViolentCrimesPerPop'
features <- crime_data %>% select(-ViolentCrimesPerPop) %>% names()

model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  seed = 123, lambda = 0,
  compute_p_values = T )

model@model$coefficients_table %>% as.data.frame() %>% 
  dplyr::select(names, p_value) %>% 
  mutate(p_value = round(p_value,3)) %>% .[-1,] %>% arrange(desc(p_value))

while(model@model$coefficients_table %>% as.data.frame() %>% 
      dplyr::select(names, p_value) %>% 
      mutate(p_value = round(p_value,3)) %>% .[-1,] %>% arrange(desc(p_value)) %>% 
      .[1,2] > 0.05){
  
    model@model$coefficients_table %>% as.data.frame() %>% 
    dplyr::select(names, p_value) %>% 
    mutate(p_value = round(p_value,3)) %>% 
    filter(!is.nan(p_value)) %>% .[-1,] %>% arrange(desc(p_value)) %>% .[1,1] -> v
  
  features <- features[features != v]
  
  train_h2o <- train %>% as.data.frame() %>% select(target, features) %>% as.h2o()
  test_h2o <- test %>% as.data.frame() %>% select(target, features) %>% as.h2o()
  
  model <- h2o.glm(
    x = features, y = target,
    training_frame = train,
    validation_frame = test,
    seed = 123, lambda = 0,
    compute_p_values = T )
}

model@model$coefficients_table %>% as.data.frame() %>% 
  dplyr::select(names, p_value) %>% 
  mutate(p_value = round(p_value,3))

y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict

test_set <- test %>% as.data.frame()
residuals = test_set$ViolentCrimesPerPop - y_pred$predict


RMSE <- sqrt(mean(residuals^2))

y_test_mean <- mean(test_set$ViolentCrimesPerPop)

tss <- sum((test_set$ViolentCrimesPerPop - y_test_mean)^2)
rss <- sum(residuals^2)

R2 <- 1 - (rss/tss)

n <- test_set %>% nrow()
k <- features %>% length()
Adjusted_R2 <- 1 - (1-R2)*((n-1)/(n-k-1))

tibble(RMSE = round(RMSE,1), R2, Adjusted_R2)

my_data <- cbind(predicted = y_pred$predict,
                 observed = test_set$ViolentCrimesPerPop) %>% as.data.frame() %>% view()



plot <- my_data %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color= "darkred")+
  geom_smooth(method=lm) +
  labs(x="Predicted Crimes",
       y="Actual Crimes",
       title=glue('Test:Adjusted R2={round(enexpr(Adjusted_R2),2)}')) +
  theme(plot.title = element_text(color='darkgreen',size=16,hjust=0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14))



plot %>% ggplotly()

y_pred_train <- model %>% h2o.predict(newdata = train) %>% as.data.frame()

train_set <- train %>% as.data.frame()
residuals = train_set$ViolentCrimesPerPop - y_pred_train$predict


RMSE_train <- sqrt(mean(residuals^2))

y_train_mean <- mean(train_set$ViolentCrimesPerPop)

tss <- sum((train_set$ViolentCrimesPerPop - y_train_mean)^2)
rss <- sum(residuals^2)

R2_train <- 1 - (rss/tss); R2_train

n <- train_set %>% nrow()
k <- features %>% length()
Adjusted_R2_train <- 1 - (1-R2_train)*((n-1)/(n-k-1))

tibble(RMSE_train = round(RMSE_train,1), R2_train, Adjusted_R2_train)



my_data_train <- cbind(predicted = y_pred_train$predict,
                 observed = train_set$ViolentCrimesPerPop) %>% as.data.frame() %>% view()


plot_train <- my_data_train %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color= "darkred")+
  geom_smooth(method=lm) +
  labs(x="Predicted Crimes",
       y="Actual Crimes",
       title=glue('Train:Adjusted R2={round(enexpr(Adjusted_R2_train),2)}')) +
  theme(plot.title = element_text(color='darkgreen',size=16,hjust=0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14))

plot_train %>% ggplotly()

library(patchwork)
plot_train + plot


tibble(RMSE_train = round(RMSE_train,1),
       RMSE_test = round(RMSE,1),
       
       Adjusted_R2_train , 
       Adjusted_R2_test = Adjusted_R2)
       

