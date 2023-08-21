library(tidyverse) 
library(data.table)
library(rstudioapi)
library(skimr)
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
library(h2o) 
#1. Add ggplot2::mpg dataset.
df<-mpg

df %>% skimr::skim()

library(inspectdf)
df %>% inspect_na()

df %>% glimpse()

target <- 'cty'
features <- df %>%  select(displ,year,cyl) %>%  names()

f<-as.formula(paste(target,paste(features, collapse = "+"), sep = "~")) 


glm <- glm(f,data = df)
glm %>% summary()


df<-df %>% select(target,features)





#df.chr <- df %>%
 # select_if(is.character)%>%view()

#df.num <- df %>%
 # select_if(is.numeric)

#df.chr %>% inspect_na()
#df.chr <- dummyVars(" ~ .", data = df.chr) %>% 
 # predict(newdata = df.chr) %>% 
  #as.data.frame()%>%view()

#df <- cbind(df.chr,df.num) %>%
 # select(cty,everything())




df %>% glimpse()
df<-df %>% scale() %>% as.data.frame()
df %>% glimpse

library(h2o)
h2o.init()


h2o_data<-df %>% as.h2o()

h2o_data<-h2o_data %>% h2o.splitFrame(ratios=0.8, seed =123)
train<-h2o_data[[1]]
test<-h2o_data[[2]]

target <- 'cty'
features <- df %>%  select(-cty) %>%  names()

df%>%view()
model<-h2o.glm(
  x=features, y= target,
  training_frame = train,
  validation_frame = test,
  nfolds=10, seed=123,
  lambda = 0, compute_p_values = T)

model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0,
  compute_p_values = TRUE,
  remove_collinear_columns = TRUE
)



model@model$coefficients_table %>% 
  as.data.frame() %>% 
  select(names, p_value) %>% 
  mutate(p_value = round(p_value,3)) %>% 
  .[-1,] %>% 
  arrange(desc(p_value))


while(model@model$coefficients_table %>% 
      as.data.frame() %>% 
      select(names, p_value) %>% 
      mutate(p_value = round(p_value,3)) %>% 
      .[-1,] %>% 
      arrange(desc(p_value)) %>% 
      .[1,2]> 0.05) {
  model@model$coefficients_table %>% 
    as.data.frame() %>%
    dplyr::select(names,p_value) %>% 
    mutate(p_value = round(p_value,3)) %>% 
    filter(! is.nan(p_value)) %>% 
    .[-1,] %>% 
    arrange(desc(p_value)) %>% 
    .[1,1] -> v
  features <- features[features!=v]
  train_h2o <- train %>% as.data.frame() %>%  select(target,features) %>% as.h2o()
  test_h2o <- test %>% as.data.frame() %>%  select(target,features) %>% as.h2o()
  
  model<-h2o.glm(
    x=features, y= target,
    training_frame = train,
    validation_frame = test,
    nfolds=10, seed=123,
    lambda = 0, compute_p_value = T)}


model@model$coefficients_table %>% 
  as.data.frame() %>%
  dplyr::select(names,p_value) %>% 
  mutate(p_value = round(p_value,3))



y_pred<- model %>%  h2o.predict(newdata = test) %>%  as.data.frame()


test_set <- test %>% as.data.frame()
residuals = test_set$ViolentCrimesPerPop - y_pred$predict

RMSE = sqrt(mean(residuals^2))

MAE = mean(abs(residuals))

y_test_mean = mean(test_set$ViolentCrimesPerPop)

tss = sum((test_set$ViolentCrimesPerPop - y_test_mean)^2)

rss = sum(residuals^2)
R2 = 1- (rss/tss)
R2

n<-test_set %>% nrow()

k<-features %>%  length()
Adjusted_R2 = 1 - (1-R2)*((n-1)/(n-k-1))

tibble(RMSE =round(RMSE,1),
       R2, Adjusted_R2)


my_data <- cbind(predicted=y_pred$predict,
                 observed = test_set$cty) %>% 
  as.data.frame()

g <- my_data %>% 
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

g %>% ggplotly()


y_pred_train <- model %>%  h2o.predict(newdata = train) %>%  as.data.frame()

train_set <- train %>%  as.data.frame()
residuals = train_set$ViolentCrimesPerPop - y_pred_train$predict

RMSE_train = sqrt(mean(residuals^2))

y_train_mean = mean(train_set$ViolentCrimesPerPop)

tss = sum((train_set$ViolentCrimesPerPop - y_train_mean)^2)
rss = sum(residuals^2)

R2_train = 1- (rss/tss);
R2_train

n<- train_set %>%  nrow()
k<- features %>%  length()

Adjusted_R2_train = 1 - (1-R2_train)*((n-1)/(n-k-1))


my_data_train <- cbind(predicted=y_pred_train$predict,
                       observed = train_set$cty) %>% 
  as.data.frame()

g_train <- my_data_train %>% 
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

g_train %>%  ggplotly()

library(patchwork)
g_train + g


