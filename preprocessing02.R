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

foodg_sum %>% group_by()


#energy total by id histogram
foodg_sum %>% select(CAN_ID, energy_total) %>% distinct() %>%
  ggplot(aes(energy_total))+ 
  geom_histogram(aes(y = ..density..), alpha=.2, fill='dodgerblue')+
  geom_density(color='red', size=1)+
  labs(title = "Distribution of total energy sum per patient ID",
       x="Total energy (kcal)")+
  theme(title = element_text(size=15),
        axis.text= element_text(size=13)) #skewed

foodg_sum %>% select(CAN_ID, energy_total) %>% distinct() %>%
  ungroup() %>%
  select(energy_total) %>% unlist() %>%
  shapiro.test() #Shapiro test


## 1. residual method
res_models = foodg_sum %>% 
  filter(energy>0) %>%
  group_by(food_group) %>%
  do(lm_mod = lm(energy ~ energy_total, data=.),
     ll_mod = lm(log(energy) ~ log(energy_total), data=.),
     log_mod = lm(log(energy) ~ energy_total, data=.)) %>%
  pivot_longer(cols=ends_with("mod"), names_to = "model_type",values_to = "model") 


## 2. normalization by row



#Tung's code
a.ffq <- cbind(ffq[,1:2], sapply(which(colnames(ffq)=='var2'):which(colnames(ffq)=='Food106'), function(i){
  fit <- lm(ffq[,i]~var1, data=ffq)
  fit$residuals + fit$coefficients[1] + fit$coefficients[2]*mean(ffq$var1)
})); colnames(a.ffq) <- colnames(ffq)


# My code
ffq %>%
  group_by('CAN_ID') %>%
  mutate_at(nutrient_cols, function(i) {fit <- lm(ffq[,i] ~ energy)})

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
