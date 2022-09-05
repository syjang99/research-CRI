ffq_sm = read_csv('ffq_preprocessed.csv')
View(ffq_sm)

temp= ffq_sm %>%
  filter(!CAN_ID %in% c('A160', 'A254', 'A277', 'A307', 'A371', 'A523')) %>%
  filter(food_group_name != 'Alcohol') %>%
  group_by(CAN_ID, food_group, food_group_name) %>%
  summarize(energy = sum(energy),
          weight = sum(weight)) %>%
  group_by(CAN_ID) %>%
  mutate(energy_total = sum(energy)) %>%
  filter(energy_total >= 500 & energy_total<=4000)
  
dim(temp) # 130733      7
colSums(is.na(temp)) #None

ffq_res = temp %>%
  group_by(food_group, food_group_name) %>%
  mutate(weight=ifelse(weight==0, 0.0000001, weight)) %>%
  mutate(res = resid(lm(log(weight) ~ log(energy_total))),
         b0 = coef(lm(log(weight) ~ log(energy_total)))[[1]],
         b1 = coef(lm(log(weight) ~ log(energy_total)))[[2]],
         mu_hat = mean(energy_total),
         weight_new = exp(res+b0 + b1*log(mu_hat)))
ffq_res
colSums(is.na(ffq_res)) #None

ffq_table = ffq_res %>% 
  select(CAN_ID, food_group, food_group_name, weight_new) %>%
  pivot_wider(id_cols='CAN_ID', names_from='food_group', names_prefix='food',
              values_from = "weight_new") %>%
  ungroup() %>% replace(is.na(.), 0)
ffq_table

ffq_table_wo_res = temp %>% 
  select(CAN_ID, food_group, food_group_name, weight) %>%
  pivot_wider(id_cols='CAN_ID', names_from='food_group', names_prefix='food',
              values_from = "weight") %>%
  ungroup() %>% replace(is.na(.), 0)

ffq_table_wo_res

write_csv(ffq_table_wo_res,'ffq_table_wo_res.csv')
write_csv(ffq_table,'ffq_table.csv')
write_csv(ffq_res, 'ffq_res.csv')


#_---------------------------------------------------------------------------

patient_info = read_xlsx('dat_0802.xlsx')
colnames(patient_info)
patient_info2 = patient_info %>%
  rename('CAN_ID'='ffq_NO') %>%
  filter(!CAN_ID %in% c('A160', 'A254', 'A277', 'A307', 'A371', 'A523')) %>%
  dplyr::select(CAN_ID, age, sex,  BMI, smoking, Drinking, CRClocationmain, Comorbid, postopcomplication, AJCCstage, OS, OS_time, PFS, PFS_time)%>%
  mutate(CRClocationmain = case_when(
           CRClocationmain %in% 2:8 ~ 'Colon',
           CRClocationmain %in% c(0:1, '%') ~ 'Other',
           CRClocationmain %in% 9:10 ~ 'Rectum'))
View(patient_info)
dim(patient_info2)

\write_csv(patient_info2,'patient_info.csv')

#_---------------------------------------------------------------------------

fin_table = patient_info %>% inner_join(ffq_table)

View(fin_table)

write_csv(fin_table,'fin_table.csv')

fin_table_wo_res = patient_info %>% inner_join(ffq_table_wo_res)
write_csv(fin_table_wo_res,'fin_table_wo_res.csv')


fin_df = patient_info %>% inner_join(ffq_res)
fin_df
write_csv(fin_df,'fin_df.csv')

fin_df_wo_res= patient_info %>% inner_join(temp)
write_csv(fin_df, 'fin_df_wo_res.csv')
