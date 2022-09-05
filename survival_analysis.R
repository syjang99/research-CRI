fin = read_csv('pca_added.csv') 
fin = fin%>%mutate(sex = ifelse(sex==2,0,sex))

library(survival)
library(leaps)

colnames(fin)

temp = fin

bTert = quantile(fin$Balanced, c(0:3/3))
mTert = quantile(fin$Meat_Carbs, c(0:3/3))
fTert = quantile(fin$Fruits_Dairy, c(0:3/3))
# classify values
temp$Balanced = with(fin, 
               cut(Balanced, 
                   bTert, 
                   include.lowest = T, 
                   labels = c("Low", "Medium", "High")))
temp$Meat_Carbs = with(fin, 
                     cut(Meat_Carbs, 
                         mTert, 
                         include.lowest = T, 
                         labels = c("Low", "Medium", "High")))
temp$Fruits_Dairy = with(fin, 
                     cut(Fruits_Dairy, 
                         fTert, 
                         include.lowest = T, 
                         labels = c("Low", "Medium", "High")))
  

#age, bmi, 
coxph(formula = Surv(OS_time, OS) ~ Drinking, data=fin)

              Comorbid+CRClocation+Prudent+Western+Cafe_lover, data=fin)


OS_dat = fin %>% dplyr::select(-contains("PFS"), -CAN_ID)
PFS_dat = fin %>% dplyr::select(-contains("OS"), -CAN_ID)

PFS_mod = coxph(formula = Surv(PFS_time, event=PFS) ~ ., data=PFS_dat)

c = stepAIC(PFS_mod, method='forward', scope=list(lower= Surv(PFS_time,PFS) ~ Balanced+Meat_Carbs+Fruits_Dairy, upper=PFS_mod))

cox.zph(c)

OS_mod= coxph(formula = Surv(OS_time, event=OS) ~ ., data=OS_dat)

a = stepAIC(OS_mod, method='both', scope=list(lower= Surv(OS_time,OS) ~ Meat_Carbs, upper=OS_mod))
aqacox.zph(a)

weibull_mod = WeibullReg(Surv(OS_time, OS) ~ age + BMI + AJCCstage + Balanced + 
             Balanced+Western, data = OS_dat)
weibull_mod
weibull_mod$coef

full_log = glm(PFS ~ ., PFS_dat %>% dplyr::select(-PFS_time), family='binomial')
glm(PFS ~ Balanced+Meat_Carbs+Fruits_Dairy, data=PFS_dat,family='binomial') %>% summary()

s_log = stepAIC(full_log, method='forward', scope=list(lower= PFS ~ Balanced+Meat_Carbs+Fruits_Dairy, upper=full_log))
summary(s_log)

full_log = glm(OS ~ ., OS_dat %>% dplyr::select(-OS_time), family='binomial')

s_log = stepAIC(full_log, method='forward', scope=list(lower= OS ~ Balanced+Meat_Carbs+Fruits_Dairy, upper=full_log))
summary(s_log)

mod3

library(cars)
vif(mod3)

