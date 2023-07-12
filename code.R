# --------------------------------------------------------
# 4-1. Data cleaning and structuring
# --------------------------------------------------------
library(dplyr)

# read each csv file
df_2014 <- read.csv('Data/Essays/2022AUT_IntroToDS/2014_Financial_Data.csv', header = T, encoding = 'UTF-8')
df_2015 <- read.csv('Data/Essays/2022AUT_IntroToDS/2015_Financial_Data.csv', header = T, encoding = 'UTF-8')
df_2016 <- read.csv('Data/Essays/2022AUT_IntroToDS/2016_Financial_Data.csv', header = T, encoding = 'UTF-8')
df_2017 <- read.csv('Data/Essays/2022AUT_IntroToDS/2017_Financial_Data.csv', header = T, encoding = 'UTF-8')
df_2018 <- read.csv('Data/Essays/2022AUT_IntroToDS/2018_Financial_Data.csv', header = T, encoding = 'UTF-8')

# add years to each file
df_2014$Year = 2014
df_2015$Year = 2015
df_2016$Year = 2016
df_2017$Year = 2017
df_2018$Year = 2018

# check whether there are different variables between the files
col_2014 <- colnames(df_2014)
col_2015 <- colnames(df_2015)
col_2016 <- colnames(df_2016)
col_2017 <- colnames(df_2017)
col_2018 <- colnames(df_2018)
col <- data.frame(cbind(col_2014, col_2015, col_2016, col_2017, col_2018))
col$diff <- if_else(
  (col$col_2014 != col$col_2015) & 
    (col$col_2015 != col$col_2016) & 
    (col$col_2016 != col$col_2017) & 
    (col$col_2017 != col$col_2018)
  , 'Different', 'Same')

# unify different variable names
df_2014 <- df_2014 %>% rename('Price.Var' = 'X2015.PRICE.VAR....')
df_2015 <- df_2015 %>% rename('Price.Var' = 'X2016.PRICE.VAR....')
df_2016 <- df_2016 %>% rename('Price.Var' = 'X2017.PRICE.VAR....')
df_2017 <- df_2017 %>% rename('Price.Var' = 'X2018.PRICE.VAR....')
df_2018 <- df_2018 %>% rename('Price.Var' = 'X2019.PRICE.VAR....')

# merge all the files and reorder the variables
df <- rbind(df_2014, df_2015, df_2016, df_2017, df_2018)
df <- df %>% select(X, Year, everything())

# check data structure
str(df)

# check head and tail data
head(df)
tail(df)

# check the number of null data 
colSums(is.na(df))

# visualize null data using (divide into four parts to see clearly)
library(visdat)
vis_miss(df[1:60], warn_large_data = F)
vis_miss(df[61:120], warn_large_data = F)
vis_miss(df[121:180], warn_large_data = F)
vis_miss(df[181:226], warn_large_data = F)

# visualize null data of operatingCycle, cashConversionCycle variables as they seemed to have many null data above 
library(naniar)
gg_miss_var(df[,c('operatingCycle', 'cashConversionCycle')], show_pct = T)

# delete operatingCycle, cashConversionCycle variables as they contain too many null data
df <- df %>% select(!c(operatingCycle, cashConversionCycle))

# change zero (0) value to null (NA) except for Class variable
df <- df %>% mutate(across(.cols = !Class, everything(), .fns = ~ replace(.x, .x %in% 0, NA)))

# create new dataframe of median values of each industry sector (Table 3)
df_median_sector <- df %>% 
  select(!c('X', 'Year')) %>% 
  group_by(Sector) %>% 
  summarise(across(.cols = everything(), .fns = ~ median(., na.rm = T)))

# fill null (NA) data by median values of each industry sector
for (variable in colnames(df_median_sector)) {
  for (sector in unique(df_median_sector$Sector)) {
    df[(is.na(df[variable])) & (df$Sector == sector), variable] <- df_median_sector[df_median_sector$Sector == sector, variable]
  }
}


# --------------------------------------------------------
# 4-2. Data transformation
#   4-2-1. PCA for 3-1. Clustering similar industry sectors 
# --------------------------------------------------------
# delete constant columns
library(janitor)
df_median_sector_without_constant <- remove_constant(df_median_sector, quiet = T)

# conduct PCA 
library(stats)
df_pca_4.2.1 <- prcomp(df_median_sector_without_constant[,2:221], scale. = T)


# --------------------------------------------------------
#   4-2-2. PCA for 5. Modelling
# --------------------------------------------------------
# delete X, Sector, Price.Var variables
df_4.2.2 <- df %>% select(!c('X','Sector','Price.Var'))

# delete constant columns
df_4.2.2 <- remove_constant(df_4.2.2, quiet = T)

# conduct PCA
df_pca_4.2.2 <- prcomp(df_4.2.2, scale. = T)


# --------------------------------------------------------
# 5. Modelling
#   5-1. Algorithm
# --------------------------------------------------------
library(randomForest)


# --------------------------------------------------------
#   5-2. Models
# --------------------------------------------------------
# function to get training and test data 
get_df_train <- function(data){return(data[1:(round(nrow(data)*0.5)), ])}
get_df_test <- function(data){return(data[(round(nrow(data)*0.5)+1):nrow(data), ])}

# -------- Model A --------------------------------
# exclude Price Var variable and convert Class variable as factor
df_model_a <- df %>% 
  select(!Price.Var) %>% 
  mutate(Class = as.factor(Class))

# get Model A training and test data
df_model_a_train <- get_df_train(df_model_a) 
df_model_a_test <- get_df_test(df_model_a)


# -------- Model B --------------------------------
# get core variables names
core_var <- c('Revenue','Net.Profit.Margin','ROIC','Working.Capital')

# select only core variables and convert Class variable as factor
df_model_b <- df %>% 
  select(all_of(core_var), Class) %>% 
  mutate(Class = as.factor(Class))

# get Model B training and test data
df_model_b_train <- get_df_train(df_model_b) 
df_model_b_test <- get_df_test(df_model_b)


# -------- Model C --------------------------------
# merge Class variable and 4-2-2 PCA x data 
df_model_c <- data.frame(df$Class, df_pca_4.2.2$x)

# rename Class variable and convert it as factor
df_model_c <- df_model_c %>% 
  rename('Class' = 'df.Class') %>% 
  mutate(Class = as.factor(Class))

# get Model C training and test data
df_model_c_train <- get_df_train(df_model_c) 
df_model_c_test <- get_df_test(df_model_c)

# use only 100 PC
df_model_c_train <- df_model_c_train[,1:101]


# --------------------------------------------------------
# 6. Evaluation
# --------------------------------------------------------
library(Metrics)


# Results and discussion
# -----------------------------------------------------------------
# 1. Result of EDA 
#   Figure 2. Clustering industry sectors and finding a correlation with core variables
# -----------------------------------------------------------------
library(ggplot2)

# get sector names which should be below when labeling
text_below <- c('Energy','Basic Materials','Industrials','Communication Services')

# visualize core variables and industry sectors in PC1 and PC2 using 4-2-1 data
library(factoextra)
fviz_pca_biplot(
  df_pca_4.2.1
  , label = "var"
  , col.var = 'red'
  , geom.ind = "point"
  , fill.ind = 'black'
  , col.ind = 'white'
  , pointshape = 22
  , pointsize = 3
  , select.var = list(name = c(core_var,'Price.Var'))
  # , geom.var = c('arrow','text')
) +
  xlim(-20,20) +
  ylim(-12,12) +
  geom_text(
    aes(label = df_median_sector_without_constant$Sector)
    , vjust = ifelse(df_median_sector_without_constant$Sector %in% text_below, -1, 1.5) 
    , size = 3.5
  ) +
  labs(
    title = ''
    , x = 'PC 1 (37.1%)'
    , y = 'PC 2 (18.8%)'
  ) +
  theme(
    panel.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
    , plot.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
    , axis.ticks.y = element_blank()
    , axis.text.y = element_blank()
    , axis.ticks.x = element_blank()
    , axis.text.x = element_blank()
  )


# -----------------------------------------------------------------
#   Figure 3. Trends of price variation based on clustered industry sectors
# -----------------------------------------------------------------
# group the data according to Figure 2.
df_figure3 <- df %>% 
  select(X, Sector, Year, Price.Var) %>% 
  mutate(
    Group = case_when(
      Sector %in% c('Communication Services','Utilities') ~ 'Group 1'
      , Sector %in% c('Energy','Real Estate') ~ 'Group 2'
      , Sector %in% c('Basic Materials','Consumer Cyclical','Consumer Defensive','Industrials','Technology') ~ 'Group 3'
      , Sector == 'Financial Services' ~ 'Group 4'
      , Sector == 'Healthcare' ~ 'Group 5'
    )
  ) %>% 
  group_by(Group, Year) %>% 
  summarise(median_Price.Var = median(Price.Var))

# visualize df_figure3 data
library(stringr)
ggplot(df_figure3, aes(Year, median_Price.Var, color = Group)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), se = FALSE, linewidth = 1.5) +
  geom_hline(yintercept = 0, size = 0.8, linetype = 'solid', color = 'black', linewidth = 0.8) +
  geom_hline(yintercept = c(-30,30), size = 0.5, linetype = 'dashed', color = 'grey', linewidth = 0.5) +
  geom_vline(xintercept = c(2014,2015,2016,2017,2018), size = 0.5, linetype = 'dashed', color = 'grey', linewidth = 0.5) +
  scale_y_continuous(breaks = c(-30, 0, 30), labels = function(x) str_c(x,'%')) +
  scale_color_manual(
    name = ''
    , labels = c(
      'Group A (Communication Services, Utilities)'
      , 'Group B (Energy, Real Estate)'
      , 'Group C (Basic Materials, Technology, Industrials, Consumer Defensive, Consumer Cyclical)'
      , 'Group D (Financial Services)'
      , 'Group E (Healthcare)'
    )
    , values = c('red','blue','orange','green','purple')
  ) +
  labs(title = '') +
  theme(
    panel.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
    , plot.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
    , legend.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
    , legend.key.size = unit(0.4, 'cm')
    , legend.direction = 'vertical'
    , legend.text = element_text(size = 11)
    , legend.position = 'bottom'
    , axis.title.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.text.y = element_text(size = 10)
    , axis.title.x = element_blank()
    , axis.ticks.x = element_blank()
    , axis.text.x = element_text(size = 10)
  ) 


# -----------------------------------------------------------------
#   Figure 4. Proportion of Class variable in each industry sector
# -----------------------------------------------------------------
# convert Class variable as factor
# , group the data according to industry sectors and Class variable
# , calculate the Class 0/1 percentages
# , and reorder according to the Class 1 percentage
df_figure4 <- df %>% 
  mutate(Class = as.factor(Class)) %>% 
  group_by(Sector, Class) %>% 
  tally() %>% 
  mutate(Percent= n/sum(n)) %>% 
  arrange(desc(Class), Percent)

# fix Sector variable order according to the Class 1 percentage
df_figure4$Sector <- factor(df_figure4$Sector, levels = unique(df_figure4$Sector))

# visualize df_figure4 data
ggplot(df_figure4, aes(Sector, Percent, fill = Class)) + 
  geom_bar(stat="identity", position ="fill") + 
  geom_hline(yintercept = 0.5, linewidth = 0.8) +
  scale_y_continuous(breaks = 0.5, labels = function(x) str_c(x*100,'%')) +
  geom_text(aes(label=paste0(sprintf("%1.1f", Percent*100),"%")), position=position_fill(vjust=0.5), colour="white", size = 5) +
  coord_flip() +
  scale_fill_manual(values = c("#FC8D62","#66C2A5"), guide = guide_legend(reverse = T)) +
  labs(title = '') +
  theme(
    panel.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
    , plot.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
    , legend.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
    , legend.key.size = unit(0.4, 'cm')
    , legend.text = element_text(size = 13)
    , legend.position = 'bottom'
    , axis.title.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.text.y = element_text(size = 11)
    , axis.title.x = element_blank()
    , axis.ticks.x = element_blank()
  ) 


# -----------------------------------------------------------------
# 2. Results of building prediction models 
#   2-1. Model A 
# -----------------------------------------------------------------
# train a RF model using Model A training data
df_model_a_rf <- randomForest(Class ~ ., data = df_model_a_train)

# predict Class variable values of Model A test data using above model
df_model_a_pred <- predict(df_model_a_rf, newdata = df_model_a_test[,-223])

# convert as a dataframe
df_model_a_pred <- data.frame(df_model_a_pred)

# merge predicted values and actual values  
df_model_a_cm <- cbind(df_model_a_test[, 223], df_model_a_pred) %>% 
  rename(
    'actual' = 'df_model_a_test[, 223]'
    , 'pred' = 'df_model_a_pred'
  )

# evaluate the performance by calculating accuracy
df_model_a_accuracy <- accuracy(df_model_a_cm$actual, df_model_a_cm$pred)
df_model_a_accuracy

# check overfitting
df_model_a_overfitting <- predict(df_model_a_rf, newdata = df_model_a_train[,-223])
df_model_a_overfitting <- data.frame(df_model_a_overfitting)
df_model_a_overfitting_cm <- cbind(df_model_a_train[, 223], df_model_a_overfitting) %>% 
  rename(
    'actual' = 'df_model_a_train[, 223]'
    , 'pred' = 'df_model_a_overfitting'
  )
df_model_a_overfitting_accuracy <- accuracy(df_model_a_overfitting_cm$actual, df_model_a_overfitting_cm$pred)
df_model_a_overfitting_accuracy


# -----------------------------------------------------------------
#   2-2. Model B 
# -----------------------------------------------------------------
# train a RF model using Model B training data
df_model_b_rf <- randomForest(Class ~ ., data = df_model_b_train)

# predict Class variable values of Model B test data using above model
df_model_b_pred <- predict(df_model_b_rf, newdata = df_model_b_test[,-5])

# convert as a dataframe
df_model_b_pred <- data.frame(df_model_b_pred)

# merge predicted values and actual values
df_model_b_cm <- cbind(df_model_b_test[, 5], df_model_b_pred) %>% 
  rename(
    'actual' = 'df_model_b_test[, 5]'
    , 'pred' = 'df_model_b_pred'
  )

# evaluate the performance by calculating accuracy
df_model_b_accuracy <- accuracy(df_model_b_cm$actual, df_model_b_cm$pred)
df_model_b_accuracy

# check overfitting
df_model_b_overfitting <- predict(df_model_b_rf, newdata = df_model_b_train[,-5])
df_model_b_overfitting <- data.frame(df_model_b_overfitting)
df_model_b_overfitting_cm <- cbind(df_model_b_train[, 5], df_model_b_overfitting) %>% 
  rename(
    'actual' = 'df_model_b_train[, 5]'
    , 'pred' = 'df_model_b_overfitting'
  )
df_model_b_overfitting_accuracy <- accuracy(df_model_b_overfitting_cm$actual, df_model_b_overfitting_cm$pred)
df_model_b_overfitting_accuracy

# -----------------------------------------------------------------
#   2-3. Model C 
# -----------------------------------------------------------------
# train a RF model using Model C training data 
df_model_c_rf <- randomForest(Class ~ ., data = df_model_c_train)

# predict Class variable values of Model C test data using above model
df_model_c_pred <- predict(df_model_c_rf, newdata = df_model_c_test[,-1])

# convert as a dataframe
df_model_c_pred <- data.frame(df_model_c_pred)

# merge predicted values and actual values
df_model_c_cm <- cbind(df_model_c_test[, 1], df_model_c_pred) %>% 
  rename(
    'actual' = 'df_model_c_test[, 1]'
    , 'pred' = 'df_model_c_pred'
  )

# evaluate the performance by calculating accuracy
df_model_c_accuracy <- accuracy(df_model_c_cm$actual, df_model_c_cm$pred)
df_model_c_accuracy

# check overfitting
df_model_c_overfitting <- predict(df_model_c_rf, newdata = df_model_c_train[,-1])
df_model_c_overfitting <- data.frame(df_model_c_overfitting)
df_model_c_overfitting_cm <- cbind(df_model_c_train[, 1], df_model_c_overfitting) %>% 
  rename(
    'actual' = 'df_model_c_train[, 1]'
    , 'pred' = 'df_model_c_overfitting'
  )
df_model_c_overfitting_accuracy <- accuracy(df_model_c_overfitting_cm$actual, df_model_c_overfitting_cm$pred)
df_model_c_overfitting_accuracy

# -----------------------------------------------------------------
#   Figure 5. Cumulative variance by number of PC
# -----------------------------------------------------------------
# visualize the cumulative variance by number of PC
pr_var <- df_pca_4.2.2$sdev^2
pr_var <- pr_var/sum(pr_var)
plot(
  cumsum(pr_var)
  , xlab = 'Number of Principal Components'
  , ylab = 'Cumulative Variance (%)'
  , type = 'o'
)



