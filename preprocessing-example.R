#setwd('C:/Users/syjan/Desktop/internship/research-CRC')
library(tidyverse)
library(readxl)
#library(sas7bdat) #read sas file -> 느림
library(haven) #이게 더 빠르다
library(stringr)


#(1) 환자정보데이터
patient_info = read_xlsx('DATA_na_removed.xlsx')
head(patient_info)
colnames(patient_info)
View(patient_info)
dim(patient_info) #540, 200

#(2) 식이데이터 변수명
ffq_protocol = read_xlsx('ffq_protocol.xlsx', sheet=1) %>%
  select(1:5) %>% rename('content' = '...5') 
View(ffq_protocol)

#(3) 환자 식이데이터
ffq = read_sas('FFQ540.sas7bdat')
head(ffq)
View(ffq)
dim(ffq) #362340, 103


#(4) 변수명 예쁘게 저장하기
ffq_protocol = ffq_protocol %>%
  mutate(var_name = str_to_lower(content),
         var_name = str_replae(var_name,"\\([^()]+\\)", ''),
         var_name = str_replace_all(var_name, ' ', '_'),
         var_name = str_replace(var_name, 'β', 'beta'),
         var_name = str_replace(var_name, "_$", "")) 
ffq_protocol$var_name[62:63] = c('clupanodonic_acid', 'osbond_acid')
View(ffq_protocol)

colnames(ffq)[8:103] = ffq_protocol$var_name[7:102]
View(ffq)


#(5) 106가지 식품 -> 35가지 식품군으로 재분류하기 (code from Tung)
#food_group: group index, food_group_name: group name

#food group
ffq = ffq %>% 
  mutate(food_group = case_when(
    foodc %in% c(11003010,11011010,11018010,12001000,12006000) ~ 1,
    #~~
    TRUE ~ NA_real_
  ))

#food group name 저장
food_group_names <- tribble(
  ~food_group, ~food_group_name,
  1, "Refined grains",
  2, "Whole grains",
  3, "Tubers and roots",
  4, "Noodles",
  5, "Rice cakes",
  6, "Bread",
  7, "Cereals and snacks",
  8, "Pizza and hamburger",
  9, "Cakes and sweets",
  10, "Legumes",
  11, "Tofu and soy milk",
  12, "Nuts and seeds",
  13, "Red meat",
  14, "Meat by-products",
  15, "Processed meat",
  16, "Poultry",
  17, "Fish",
  18, "Seafood products",
  19, "Other seafood",
  20, "Salted fermented seafood",
  21, "Seaweeds",
  22, "Eggs",
  23, "Milk",
  24, "Dairy Products",
  25, "Fruits",
  26, "Fruit products",
  27, "Green and yellow vegetables",
  28, "Light-colored vegetables",
  29, "Pickled vegetables",
  30, "Kimchi",
  31, "Mushrooms",
  32, "Oil and fat",
  33, "Condiments and seasonings",
  34, "Carbonated beverages",
  35, "Coffee and tea",
  36, "Alcohol"
)

#merge 
ffq = ffq %>% left_join(food_group_names, by='food_group') 
colSums(is.na(ffq))

#확인
ffq %>% select(contains("food")) %>% distinct() %>% View()

#(6) 필요한 column만 추출, 변수명 변경해 저장, 영양소 데이터 제거
ffq = ffq %>% 
  arrange(CAN_ID) %>%
  rename('food_type_code' = 'way',
         'food_name_code' =  'foodc',
         'weight' = 'wet',
         'food_name' = 'foodn',
         'food_type' = 'food') 
ffq_sm = ffq %>% select('CAN_ID':'energy', contains('food'), -'foodg') 
View(ffq_sm)
colnames(ffq_sm)
colSums(is.na(ffq_sm)) #0
dim(ffq_sm) #362340,9


#(7)환자정보+식이정보 full data 생성 후 저장
full_dat = patient_info %>% inner_join(ffq, by='CAN_ID') %>%
  arrange(CAN_ID)
dim(full_dat) #204149, 304
head(full_dat)
colnames(full_dat) 

write_csv(ffq_protocol, 'ffq_protocol.csv')
write_csv(ffq,'ffq.csv')
write_csv(ffq_sm, 'ffq_sm.csv')
write_csv(full_dat,'full_dat.csv')
