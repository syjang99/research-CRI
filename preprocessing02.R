setwd('C:/Users/syjan/Desktop/암연구소 인턴/CRC_research')
library(tidyverse)

full_dat = read_csv('full_dat.csv')
ffq_sm = read_csv('ffq_sm.csv')
#diet_n = read_csv('nutrients_sum.csv')

#sum by nutrient for each ID
sum_by_nutrient<- function(df, cols){
  full_dat %>% 
    select(c(CAN_ID, cols))%>%
    group_by(CAN_ID) %>%
    mutate_at(cols, ~sum(.,na.rm=TRUE)) %>%
    distinct(CAN_ID, .keep_all=TRUE) %>%
    filter(energy >=500 & energy <= 4000) %>%
    mutate_at(nutrient_cols, ~ replace(., which(.==0), 5e-324)) %>%
    ungroup()
}
nutrient_cols = colnames(full_dat)[207:235]
diet = sum_by_nutrient(full_dat, nutrient_cols)
View(diet)
dim(diet) #531   31
write_csv(diet, 'nutrients_sum.csv')

#sum by food group
foodg_sum = ffq_sm %>% group_by(CAN_ID, food_group, food_group_name) %>%
  summarize(energy =sum(energy)) %>% ungroup() %>%
  group_by(CAN_ID) %>%
  mutate(energy_total = sum(energy))
foodg_sum %>% View()
write_csv(foodg_sum,'foodg_sum.csv')


#standardize across row (adjust for individual difference)

## why?
foodg_sum %>% filter(food_group==1, energy>10, energy_total<=4000) %>%
  ggplot(aes(energy_total, energy)) +
  geom_point(alpha=.3) + geom_smooth(method='lm', se=FALSE, col='red', alpha=.9)+
  labs(title="Energy obtained by eating whole grains vs. Total sum of energy",
       subtitle="(Outlier rows with total sum under 10 or over 8000 kcal were removed.)",
       y= "Energy from whole grains (kcal)",
       x = "Total sum of energy (kcal)")+
  theme(title = element_text(size=15),
        axis.text= element_text(size=13))



#energy total by id histogram
foodg_sum %>% select(CAN_ID, energy_total) %>% distinct() %>%
  ggplot(aes(energy_total))+ 
  geom_histogram(aes(y = ..density..), alpha=.2, fill='dodgerblue')+
  geom_density(color='red', size=1)+
  labs(title = "Distribution of total energy sum per patient ID",
       x="Total energy (kcal)")+
  theme(title = element_text(size=15),
        axis.text= element_text(size=13)) #skewed

foodg_sum_w %>% select(CAN_ID, weight_total) %>% distinct() %>%
  ggplot(aes(weight_total))+ 
  geom_histogram(aes(y = ..density..), alpha=.2, fill='dodgerblue')+
  geom_density(color='red', size=1)+
  labs(title = "Distribution of total weight sum per patient ID",
       x="Total weight (kcal)")+
  theme(title = element_text(size=15),
        axis.text= element_text(size=13)) #skewed

foodg_sum %>% select(CAN_ID, energy_total) %>% distinct() %>%
  ungroup() %>%
  select(energy_total) %>% unlist() %>%
  shapiro.test() #Shapiro test


## 1. residual method
# My code
foodg_sum_new = foodg_sum %>%
  #filter(energy>0, energy<4000) %>%
  group_by(food_group, food_group_name) %>%
  mutate(res = resid(lm(log(energy) ~ energy_total)),
         b0 = coef(lm(log(energy) ~ energy_total))[[1]],
         b1 = coef(lm(log(energy) ~ energy_total))[[2]],
         mu_hat = mean(energy),
         energy_new = exp(res+ b0 + b1*mu_hat))

foodg_sum_new2 = foodg_sum %>%
  filter(energy>0, energy<4000) %>%
  group_by(food_group, food_group_name) %>%
  mutate(res = resid(lm(log(energy) ~ log(energy_total))),
         b0 = coef(lm(log(energy) ~ log(energy_total)))[[1]],
         b1 = coef(lm(log(energy) ~ log(energy_total)))[[2]],
         mu_hat = mean(energy),
         energy_new = exp(b0 + b1*log(mu_hat)))

#
par(mfrow=c(1,2))
foodg_sum %>% filter(food_group==1) %>%
  ggplot(aes(energy_total, energy)) +
  geom_point(alpha=.3) + geom_smooth(method='lm', col='red', alpha=.9)+
  labs(title="Energy obtained by eating whole grains vs. Total sum of energy",
       subtitle="(Outlier rows with total sum under 10 or over 8000 kcal were removed.)",
       y= "Energy from whole grains (kcal)",
       x = "Total sum of energy (kcal)")+
  theme(title = element_text(size=15),
        axis.text= element_text(size=13))

foodg_sum_new %>% filter(food_group==1) %>%
  ggplot(aes(energy_total, energy_new)) +
  geom_point(alpha=.3) + geom_smooth(method='lm', col='red', alpha=.9)+
  labs(title="Energy obtained by eating whole grains vs. Total sum of energy",
       subtitle="(Outlier rows with total sum under 10 or over 8000 kcal were removed.)",
       y= "Energy from whole grains (kcal)",
       x = "Total sum of energy (kcal)")+
  theme(title = element_text(size=15),
        axis.text= element_text(size=13))

write_csv(foodg_sum_new, 'sum_by_food_group.csv')

foodg_sum_new2 %>% filter(food_group==1, energy_total<10000) %>%
  ggplot(aes(energy_total, energy_new)) +
  geom_point(alpha=.3) + geom_smooth(method='lm', col='red', alpha=.9)+
  labs(title="Energy obtained by eating whole grains vs. Total sum of energy",
       subtitle="(Outlier rows with total sum under 10 or over 8000 kcal were removed.)",
       y= "Energy from whole grains (kcal)",
       x = "Total sum of energy (kcal)")+
  theme(title = element_text(size=15),
        axis.text= element_text(size=13))


## row by standardization

foodg_sum %>% 
  group_by(CAN_ID) %>% mutate(weight_new = scale(weight)) %>%
  filter(food_group==1, energy_total<5000) %>%
  ggplot(aes(energy_total, energy_new)) +
  geom_point(alpha=.3) + geom_smooth(method='lm', col='red', alpha=.9)
  

# assumption tests for using residual method
## visualization
par(mfrow=c(2,2))
plot(lm(folate ~ energy, data=diet))
plot(lm(log(folate) ~ log(energy), data=diet)) 


## 1. normality
#log변환하면 그래프 예뻐지기는 하는데,,
hist(diet$Folate)
hist(log(diet$Folate))
hist(diet$Fiber)
hist(log(diet$Fiber))

shapiro.test(diet$folate) #불만족
shapiro.test(diet$Fiber) #불만족
shapiro.test(log(diet$Folate))  #불만족(!!)
shapiro.test(log(diet$Fiber))  #만족

## 2. 
#descriptive statistics by sex
summary(diet[diet$X00_SEX==1,])
apply(diet[diet$X00_SEX==1,],2,sd)


beta_female = as.numeric(lm(log(fiber) ~ log(energy), data = diet[diet$X00_SEX==1,])$coefficients[2])
beta_male = as.numeric(lm(log(fiber) ~ log(energy), data = diet[diet$X00_SEX==2,])$coefficients[2])
lm(log(Fiber) ~ log(Kcal), data = diet[diet$X00_SEX==2,]) %>% summary()


# 한번에 잔차법 결과 내보내기

