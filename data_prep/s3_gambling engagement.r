library(dplyr)
# year 2016
data_2016 = read.csv('../output/data_cleaned/HarmSurvey2016.csv', row =1)
data_gambled = data_2016 %>% select(subjectkey, starts_with('freq')) %>% tibble::column_to_rownames('subjectkey')
# 8 is an un-used code 
data_gambled[is.na(data_gambled)] = 8
# 1-7 from 'daily' to 'not at all'
data_gambled$max_activity = apply(data_gambled, 1, min)
data_gambled$I_gambled = ifelse(data_gambled$max_activity < 7, 1, 0)

data_gambled_2016 = data_gambled %>% filter(I_gambled == 1) %>% select(max_activity) %>% tibble::rownames_to_column('subjectkey')
dim(data_gambled_2016)
head(data_gambled_2016,2)

# year 2017
data_2017 = read.csv('../output/data_cleaned/HarmSurvey2017.csv', row =1)
data_gambled = data_2017 %>% select(subjectkey, starts_with('freq')) %>% tibble::column_to_rownames('subjectkey')
# 8 is an un-used code 
data_gambled[is.na(data_gambled)] = 8
# 1-7 from 'daily' to 'not at all'
data_gambled$max_activity = apply(data_gambled, 1, min)
data_gambled$I_gambled = ifelse(data_gambled$max_activity < 7, 1, 0)

data_gambled_2017 = data_gambled %>% filter(I_gambled == 1) %>% select(max_activity) %>% tibble::rownames_to_column('subjectkey')
dim(data_gambled_2017)

#########2016

activity_numeric = data_2016 %>% select(subjectkey,starts_with('freq_')) %>% tidyr::drop_na()

library(tidyverse)
# reverse coding, original: daily 1; to not at all: 7; cannot say 9
rownames(activity_numeric) <- NULL
activity_numeric_new = activity_numeric %>% tibble::column_to_rownames('subjectkey')
freq_df_r = 8 - activity_numeric_new

freq_df_r[freq_df_r == -1] = NA
freq_df = na.omit(freq_df_r)

data_pca = freq_df %>% select(-freq_Lotto)
pca_model = prcomp(data_pca, scale = TRUE)

freq_df_pca = as.data.frame(pca_model$x) %>% 
            select(PC1) %>%
            tibble::rownames_to_column('subjectkey') %>%
            rename(sum_freq_all_No_lottery = PC1)

head(freq_df_pca)
dim(freq_df_pca)

write.csv(freq_df_pca, '../output/data_cleaned/freq_df_summary_2016_PCA.csv')

#########2017
library(dplyr)

activity_numeric = data_2017 %>% select(subjectkey,starts_with('freq_')) %>% tidyr::drop_na()

library(tidyverse)
# reverse coding, original: daily 1; to not at all: 7; cannot say 9
rownames(activity_numeric) <- NULL
activity_numeric_new = activity_numeric %>% tibble::column_to_rownames('subjectkey')
freq_df_r = 8 - activity_numeric_new

freq_df_r[freq_df_r == -1] = NA
freq_df = na.omit(freq_df_r)

data_pca = freq_df %>% select(-freq_Lotto)
pca_model = prcomp(data_pca, scale = TRUE)

freq_df_pca = as.data.frame(pca_model$x) %>% 
            select(PC1) %>%
            tibble::rownames_to_column('subjectkey') %>%
            rename(sum_freq_all_No_lottery = PC1)

head(freq_df_pca)
dim(freq_df_pca)

write.csv(freq_df_pca, '../output/data_cleaned/freq_df_summary_2017_PCA.csv')

######2016
data_2016_recode = read.csv('../output/data_cleaned/HarmSurvey2016_recode.csv',row = 1)

library(dplyr)

# select who gambled last year
who_gambled_2016 = data_gambled_2016$subjectkey 

data_expense = data_2016_recode %>% filter(subjectkey %in% who_gambled_2016) %>%
            select(subjectkey,starts_with('spent')) %>% 
            mutate(spent_per_w = as.numeric(spent_per_w),
                   spent_per_m = as.numeric(spent_per_m),
                   spent_the_y = as.numeric(spent_the_y)
                  ) %>%
                mutate(annual_expenses = 
                       ifelse(!is.na(spent_the_y), spent_the_y, 
                              ifelse(!is.na(spent_per_m), spent_per_m * 12, spent_per_w * 52))) %>%
                select(subjectkey, annual_expenses) %>% tidyr::drop_na()

cat('all gamblers without missing data: ', nrow(data_expense), '\n')
# clean 0 and 100000; and replace outliers higher than three sd with upper limit
spent_drop_negative = data_expense %>% filter(annual_expenses > 0)
cat('non-positive expenditures : ', nrow(data_expense) - nrow(spent_drop_negative), '\n')

spent_drop_toomuch = spent_drop_negative %>% filter(annual_expenses < 100000)
cat('exceeding 100,000 euros: ', nrow(spent_drop_negative) - nrow(spent_drop_toomuch), '\n')

uppper_limit = mean(spent_drop_toomuch$annual_expenses) + 3* sd(spent_drop_toomuch$annual_expenses)
spent_drop_toomuch$annual_expenses[spent_drop_toomuch$annual_expenses > uppper_limit] = uppper_limit
spent_2016 = spent_drop_toomuch
head(spent_2016,2)
dim(spent_2016)
write.csv(spent_2016,'../output/data_cleaned/wmy_expenses_df_2016.csv')

# -------------------2017---------------------------------
data_2017_recode = read.csv('../output/data_cleaned/HarmSurvey2017_recode.csv',row = 1)

library(dplyr)

# select who gambled last year
who_gambled_2017 = data_gambled_2017$subjectkey 

data_expense = data_2017_recode %>% filter(subjectkey %in% who_gambled_2017) %>%
            select(subjectkey,starts_with('spent')) %>% 
            mutate(spent_per_w = as.numeric(spent_per_w),
                   spent_per_m = as.numeric(spent_per_m),
                   spent_the_y = as.numeric(spent_the_y)
                  ) %>%
                mutate(annual_expenses = 
                       ifelse(!is.na(spent_the_y), spent_the_y, 
                              ifelse(!is.na(spent_per_m), spent_per_m * 12, spent_per_w * 52))) %>%
                select(subjectkey, annual_expenses) %>% tidyr::drop_na()

cat('all gamblers without missing data: ', nrow(data_expense), '\n')
# clean 0 and 100000; and replace outliers higher than three sd with upper limit
spent_drop_negative = data_expense %>% filter(annual_expenses > 0)
cat('non-positive expenditures : ', nrow(data_expense) - nrow(spent_drop_negative), '\n')

spent_drop_toomuch = spent_drop_negative %>% filter(annual_expenses < 100000)
cat('exceeding 100,000 euros: ', nrow(spent_drop_negative) - nrow(spent_drop_toomuch), '\n')

uppper_limit = mean(spent_drop_toomuch$annual_expenses) + 3* sd(spent_drop_toomuch$annual_expenses)
spent_drop_toomuch$annual_expenses[spent_drop_toomuch$annual_expenses > uppper_limit] = uppper_limit
spent_2017 = spent_drop_toomuch
head(spent_2017,2)
dim(spent_2017)
write.csv(spent_2017,'../output/data_cleaned/wmy_expenses_df_2017.csv')

########2016##############
# dichotomize vlues
activity_numeric = data_2016 %>% select(subjectkey,starts_with('freq_')) %>% tidyr::drop_na()
rownames(activity_numeric) <- NULL
width_raw = activity_numeric %>% tibble::column_to_rownames('subjectkey')

width_raw[width_raw == 9] = NA
width_raw[width_raw == 7] = 0
width_raw[width_raw > 0] = 1

# summerize
width_df_summary = width_raw %>% mutate(diverity = rowSums(across(where(is.numeric))))%>%
                                    mutate(diverity_No_lottery = diverity - freq_Lotto) %>%
                                    tibble::rownames_to_column('subjectkey') %>% tidyr::drop_na()

head(width_df_summary,2)
dim(width_df_summary)
write.csv(width_df_summary, '../output/data_cleaned/width_df_summary_2016.csv')

#########2017###########
activity_numeric = data_2017 %>% select(subjectkey,starts_with('freq_')) %>% tidyr::drop_na()
rownames(activity_numeric) <- NULL
width_raw = activity_numeric %>% tibble::column_to_rownames('subjectkey')

width_raw[width_raw == 9] = NA
width_raw[width_raw == 7] = 0
width_raw[width_raw > 0] = 1

# summerize
width_df_summary = width_raw %>% mutate(diverity = rowSums(across(where(is.numeric))))%>%
                                    mutate(diverity_No_lottery = diverity - freq_Lotto) %>%
                                    tibble::rownames_to_column('subjectkey') %>% tidyr::drop_na()

dim(width_df_summary)
write.csv(width_df_summary, '../output/data_cleaned/width_df_summary_2017.csv')


