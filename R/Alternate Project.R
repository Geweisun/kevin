## ----setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,message = F,warning = F)


## ----------------------------------------------------------------------------
library(tidyverse)
library(psych)
library(randomForest)

theme_set(theme_minimal())

df = read_csv("data/train_clean.csv")

df %>% mutate(Survived = ifelse(Survived ==1, 'Survived', 
                                       'NonSurvived')) ->df



## ----------------------------------------------------------------------------
library(pander)
num_df = df %>% select(
  Age, Fare, Parch, SibSp,  Family_Size
)

cat_df = df %>%
  select(-c(Age, Fare, Parch, SibSp,  Family_Size)) %>%
  select(-c(Cabin, Name, PassengerId,Ticket))


for(col in colnames(cat_df)){
  cat_df[,col] = factor(cat_df %>% pull(col))
}


uni_cat = function(col){
  
  cat_df %>%
    group_by(!!sym(col)) %>%
    count() %>%
    ungroup() %>%
    mutate(proportion = n /sum(n) )-> ct
  
  ct %>%
    ggplot(aes(!!sym(col), proportion, fill = !!sym(col))) +
    geom_col() +
    geom_text(aes(label = n),vjust = -0.5) +
    theme(legend.position = "none") + 
    labs(title = "col distribution" %>%
           gsub('col', col, .)) ->fig
  
  return(fig)
}






num_df %>% pivot_longer(cols = num_df %>%
                          colnames()) %>%
  group_by(name) %>%
  summarise(n_levels = length(unique(value))) -> values_ct


values_ct %>% filter(n_levels <10) %>%
  pull(name) -> may_cat


num_df %>%
  describe() %>%
  select( mean:median, min:kurtosis) %>%
  pander()
  



## ----------------------------------------------------------------------------
num_df %>% pivot_longer(cols = num_df %>%
                          colnames()) %>%
  ggplot(aes(y = value, fill = name)) +
  geom_histogram() +
  facet_grid(vars(name), scales = 'free') +
  theme(legend.position = "none") +
  labs(title = "Distribution of numerical features")


## ----------------------------------------------------------------------------
uni_cat('Pclass')
uni_cat('Embarked')
uni_cat('Sex')
uni_cat('Survived')
uni_cat('Title')


## ----------------------------------------------------------------------------

bi_num = function(col){
  
  stat = df %>%
    group_by(Survived) %>%
    summarise(describe(!!sym(col)))  %>%
    select(Survived, mean, median, sd, skew, kurtosis)
  
  df %>%
    ggplot(aes(x = Survived, y = !!sym(col) , fill = Survived)) +
    geom_boxplot()+
    theme(legend.position = "none") +
    labs(title = "comparing col by survived" %>%
           gsub("col", col, .)) ->f
  
  print(f)
  
  return(list(s = stat, f = f))
  
  
}

tmp = bi_num('Age')
tmp$s %>% pander()


## ----------------------------------------------------------------------------
tmp = bi_num('Fare')
tmp$s %>% pander()


## ----------------------------------------------------------------------------
tmp = bi_num('Parch')
tmp$s %>% pander()


## ----------------------------------------------------------------------------
tmp = bi_num('SibSp')
tmp$s %>% pander()


## ----------------------------------------------------------------------------

tmp = bi_num('Family_Size')
tmp$s %>% pander()


## ----------------------------------------------------------------------------
bi_cat = function(col){
  df %>% group_by(
    Survived, !!sym(col)
  ) %>%
    summarise(n = n()) %>%
    mutate(proportion = (n /sum(n)) %>%
             round(3)) ->ct
  
  ct %>%
    ggplot(aes(Survived, proportion, fill = !!sym(col) %>%
                 as.factor())) +
    geom_col(position = "dodge") +
    labs(title = "comparing col by survived" %>%
           gsub("col", col, .), fill = col) ->f
  
  return(list(s = ct, f = f))
  
}


bi_cat('Pclass')$f


## ----------------------------------------------------------------------------
bi_cat('Embarked')$f


## ----------------------------------------------------------------------------
bi_cat('Sex')$f


## ----------------------------------------------------------------------------
bi_cat('Title')$f


## ----------------------------------------------------------------------------
bi_cat('Parch')$f


## ----------------------------------------------------------------------------
bi_cat('SibSp')$f



## ----------------------------------------------------------------------------
bi_cat('Family_Size')$f


## ----------------------------------------------------------------------------
mdf = cbind(num_df, cat_df)

mdf$Survived = as.factor(mdf$Survived)

glm(Survived ~ ., data = mdf, family = binomial()) -> lgr

step(lgr, trace = F) -> back

back %>% summary() %>%
  coef()-> backs

backs %>%
  pander()




## ----------------------------------------------------------------------------
backs %>%
  as.data.frame() %>%
  mutate(Features = rownames(backs),
         abs = abs(`z value`))  %>%
  filter(`Pr(>|z|)` < 0.05) %>%
  ggplot(aes(`z value` ,Features %>%
               reorder(abs), fill = `z value`)) +
  geom_col() +theme(legend.position = "none" )+
  labs(title = "Impact of Features (Logistic Regression)",  y = 'Features')


## ----------------------------------------------------------------------------
randomForest(Survived ~ ., data = mdf) -> rf
importance(rf) %>% as.data.frame() %>%
  arrange(-MeanDecreaseGini) ->rf_imp
rf_imp$Variable = rownames(rf_imp)
rf_imp %>% ggplot(aes(Variable %>%reorder(-MeanDecreaseGini), 
                      MeanDecreaseGini, fill = -MeanDecreaseGini)) +
  geom_col() +
  theme(legend.position = "none" )+
  labs(title = "Importance of Features (Random Forest)",  x = 'Features')

