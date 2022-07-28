setwd('C:/Users/syjan/Desktop/암연구소 인턴/CRC_research')
library(tidyverse)
library(survival)
library(psych)
library(lavaan)
library(semPlot)
library(stringr)
library(EFAutilities)
library(corrplot)


full_dat = read_csv('full_dat.csv')
ffq = read_csv('ffq.csv')
diet = read_csv('nutrients_sum.csv')
ffq_protocol = read_csv('ffq_protocol.csv')
foodg_sum = read_csv('foodg_sum.csv')


foodg_sum_table = foodg_sum %>% 
  pivot_wider(id_cols=c('CAN_ID', 'energy_total'), names_from='food_group', names_prefix="g",
              values_from = "energy") %>%
  rename('total' = 'energy_total') %>%
  ungroup()
View(foodg_sum_table)
summary(foodg_sum_table)

corrplot(cor(select(foodg_sum_table,-CAN_ID)), method="circle")

# attempt 1
pca <- principal(select(foodg_sum_table, -total, -CAN_ID), nfactors=2, rotate='promax')
pca

plot(pca$values, type="b")
capture.output(print(pca), file="pca.txt")

pca_matrix = read_delim('pca.txt', delim='   ',skip=4) 
colnames(pca_matrix) = c('group', 'RC2', 'RC1', 'h2', 'u2','com')

pca_matrix = pca_matrix %>%
  mutate_at(c('RC2', 'RC1', 'h2', 'u2'), 
            ~as.numeric(str_extract(., "[[:digit:]]+\\.*[[:digit:]]*")))

pca_result = pca_matrix[1:36,] %>% 
  mutate(food_group = as.numeric(str_extract(group, "[[:digit:]]+"))) %>%
  left_join(distinct(select(ffq_sm, contains("group")))) %>%
  select(food_group, food_group_name, RC1, RC2, h2, u2)

pca_result %>% View()  

write_csv(pca_result,'pca_N_3_promax.csv')

#attempt 2
pca <- principal(select(foodg_sum_table, -total, -CAN_ID), nfactors=3)
pca

plot(pca$values, type="b")
capture.output(print(pca), file="pca2.txt")

pca_matrix2 = read_delim('pca2.txt', delim='   ',skip=3) 
colnames(pca_matrix2) = c('group', 'RC3','RC2', 'RC1', 'h2', 'u2','com')

pca_matrix2 = pca_matrix2 %>%
  mutate_at(c('RC3','RC2', 'RC1', 'h2', 'u2'), 
            ~as.numeric(str_extract(., "[[:digit:]]+\\.*[[:digit:]]*")))

pca_result2 = pca_matrix2[1:36,] %>% 
  mutate(food_group = as.numeric(str_extract(group, "[[:digit:]]+"))) %>%
  left_join(distinct(select(ffq_sm, contains("group")))) %>%
  select(food_group, food_group_name, RC1, RC2, h2, u2)

pca_result2 %>% View()  

write_csv(pca_result,'pca_N_3_promax.csv')

## biplot
biplot(pca)
fa.diagram(pca)

# tung's code
facload <- as.data.frame(round(data.pca$rotation,2)) #factor loading
facload[abs(facload) <= 0.2] <- ""

pca.score <- data.frame(get_pca_ind(data.pca)$coord[,1:4])



