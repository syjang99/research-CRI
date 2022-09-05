library(caret)
library(broom)
library(tidyverse)

kmeans_df = foodg_sum_table %>% select(-CAN_ID, -total) 
kmeans_df_sc = foodg_sum_table %>% select(-CAN_ID, -total) %>% scale()

ks = tibble(k = 1:9)
kmeans = ks %>% 
  mutate(kmeans = map(k, ~kmeans(kmeans_df, centers = .x))) %>%
  mutate(glanced = map(kmeans, glance)) 
cluster_stats <- kmeans %>%
  unnest(glanced)
head(cluster_stats)
elbow_plot <- ggplot(cluster_stats, aes(x = k, y = tot.withinss)) +
  geom_point() +
  geom_line() +
  labs(title = "Choosing k", x= "Number of clusters(k)", y= "Total WSSD")+
  scale_x_continuous(breaks = 1:9)
elbow_plot


result = kmeans(kmeans_df, centers = 3)
head(glance(result))

clustered_data = augment(result, kmeans_df)
glimpse(clustered_data)


gathered_data = clustered_data %>% gather(key='variable', value= 'value', -.cluster) 
gathered_data = gathered_data %>% 
  mutate(food_group = as.numeric(str_extract(variable, "[[:digit:]]+"))) %>%
  left_join(distinct(select(ffq_sm, contains("group"))))
ggplot(gathered_data, aes(x=value, fill=.cluster, alpha=0.9)) + geom_density() +
  labs(title = 'Density plot for each variable')+ facet_wrap(~food_group_name, scales='free')


