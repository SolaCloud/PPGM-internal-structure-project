library(dplyr)

data_2016 = read.csv('../output/data_cleaned/HarmSurvey2016.csv')
ppgm_raw_df = data_2016 %>% select(subjectkey, starts_with('ppgm'))
ppgm_raw_df[2:19][ppgm_raw_df[2:19] == 9] = NA
# library(visdat)
# vis_miss(ppgm_raw)

# PROBLEMS SCORE
ppgm_df = ppgm_raw_df %>% mutate(PPGM_1 = case_when(
    PPGM_1a == 1 | PPGM_1b ==1 ~ 1,
    PPGM_1a == 0 & PPGM_1b ==0 ~ 0,
    is.na(PPGM_1a) & PPGM_1b ==0 ~ 0,
    PPGM_1a == 0 & is.na(PPGM_1b) ~ 0,
    is.na(PPGM_1a) & is.na(PPGM_1b) ~ NA)) %>% select(-PPGM_1a, - PPGM_1b)

ppgm_df = ppgm_df %>% mutate(PPGM_3 = case_when(
    PPGM_3a == 1 | PPGM_3b ==1 ~ 1,
    PPGM_3a == 0 & PPGM_3b ==0 ~ 0,
    PPGM_3a == 0 & is.na(PPGM_3b) ~ 0,
    is.na(PPGM_3a) & PPGM_3b ==0 ~ 0,
    is.na(PPGM_3a) & is.na(PPGM_3b) ~ NA)) %>% select(-PPGM_3a, - PPGM_3b)

ppgm_df = ppgm_df %>% mutate(PPGM_5 = case_when(
    PPGM_5a == 1 | PPGM_5b ==1~ 1,
    PPGM_5a == 0 & PPGM_5b ==0 ~ 0,
    PPGM_5a == 0 & is.na(PPGM_5b) ~ 0,
    is.na(PPGM_5a) & PPGM_5b ==0 ~ 0,
    is.na(PPGM_5a) & is.na(PPGM_5b) ~ NA)) %>% select(-PPGM_5a, - PPGM_5b)

# IMPAIRED CONTROL SCORE
ppgm_df = ppgm_df %>% mutate(PPGM_10 = case_when(
    is.na(PPGM_10a)~ NA,
    PPGM_10a == 0  ~ 0,
    PPGM_10a == 1 & PPGM_10b==0 ~ 0,
    PPGM_10a == 1 & PPGM_10b==1 ~ 1,
    PPGM_10a == 1 & is.na(PPGM_10b) ~ NA)) %>% select(-PPGM_10a, - PPGM_10b) 

# OTHER ISSUES SCORE - no special treatment

ppgm_df_csv = na.omit(ppgm_df) %>% select(subjectkey, PPGM_1,PPGM_2,PPGM_3,PPGM_4,PPGM_5,PPGM_6,PPGM_7,PPGM_8,
                                     PPGM_9,PPGM_10,PPGM_11,PPGM_12,PPGM_13,PPGM_14)
dim(ppgm_df_csv)
head(ppgm_df_csv, 2)
write.csv(ppgm_df_csv, '../output/data_cleaned/ppgm_2016.csv')


# diagnosis - PPGM labels
ppgm_df_sub_0 = ppgm_df_csv %>% 
                mutate(subScore_Problems = PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7,
                      subScore_ImpairedControl = PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11,
                      subScore_OtherIssues = PPGM_12 + PPGM_13 + PPGM_14,
                      TotalScore = subScore_Problems + subScore_ImpairedControl + subScore_OtherIssues) %>%
                mutate(diagnosis_ppgm = case_when(
                    (subScore_Problems >= 1 )&(subScore_ImpairedControl >= 1)&(TotalScore >= 5) ~ 'pathological_gambler',
                    (subScore_Problems >= 1)&(subScore_ImpairedControl >= 1)&((TotalScore<=4) & (TotalScore>=2)) ~ 'problem_gambler')
                )
ppgm_df_sub_2 = ppgm_df_sub_0 %>% 
                filter(is.na(diagnosis_ppgm)) %>% 
                mutate(diagnosis_ppgm = case_when((TotalScore) >= 1 ~ 'atRisk_gambler',
                                                 (TotalScore) < 1 ~  'recreational_gambler')) 
ppgm_df_sub_1 = ppgm_df_sub_0 %>%  filter(!is.na(diagnosis_ppgm)) 

ppgm_diagnosis = rbind(ppgm_df_sub_1,ppgm_df_sub_2) %>% select(subjectkey,diagnosis_ppgm)
head(ppgm_diagnosis,2)
dim(ppgm_diagnosis)
table(ppgm_diagnosis$diagnosis_ppgm)
# write.csv(ppgm_diagnosis, 'output/ppgm_label_2016.csv')

library(dplyr)

data_2016 = read.csv('../output/data_cleaned/HarmSurvey2017.csv')
ppgm_raw_df = data_2016 %>% select(subjectkey, starts_with('ppgm'))
ppgm_raw_df[2:19][ppgm_raw_df[2:19] == 9] = NA
# library(visdat)
# vis_miss(ppgm_raw)

# PROBLEMS SCORE
ppgm_df = ppgm_raw_df %>% mutate(PPGM_1 = case_when(
    PPGM_1a == 1 | PPGM_1b ==1 ~ 1,
    PPGM_1a == 0 & PPGM_1b ==0 ~ 0,
    is.na(PPGM_1a) & PPGM_1b ==0 ~ 0,
    PPGM_1a == 0 & is.na(PPGM_1b) ~ 0,
    is.na(PPGM_1a) & is.na(PPGM_1b) ~ NA)) %>% select(-PPGM_1a, - PPGM_1b)

ppgm_df = ppgm_df %>% mutate(PPGM_3 = case_when(
    PPGM_3a == 1 | PPGM_3b ==1 ~ 1,
    PPGM_3a == 0 & PPGM_3b ==0 ~ 0,
    PPGM_3a == 0 & is.na(PPGM_3b) ~ 0,
    is.na(PPGM_3a) & PPGM_3b ==0 ~ 0,
    is.na(PPGM_3a) & is.na(PPGM_3b) ~ NA)) %>% select(-PPGM_3a, - PPGM_3b)

ppgm_df = ppgm_df %>% mutate(PPGM_5 = case_when(
    PPGM_5a == 1 | PPGM_5b ==1~ 1,
    PPGM_5a == 0 & PPGM_5b ==0 ~ 0,
    PPGM_5a == 0 & is.na(PPGM_5b) ~ 0,
    is.na(PPGM_5a) & PPGM_5b ==0 ~ 0,
    is.na(PPGM_5a) & is.na(PPGM_5b) ~ NA)) %>% select(-PPGM_5a, - PPGM_5b)

# IMPAIRED CONTROL SCORE
ppgm_df = ppgm_df %>% mutate(PPGM_10 = case_when(
    is.na(PPGM_10a)~ NA,
    PPGM_10a == 0  ~ 0,
    PPGM_10a == 1 & PPGM_10b==0 ~ 0,
    PPGM_10a == 1 & PPGM_10b==1 ~ 1,
    PPGM_10a == 1 & is.na(PPGM_10b) ~ NA)) %>% select(-PPGM_10a, - PPGM_10b) 

# OTHER ISSUES SCORE - no special treatment

ppgm_df_csv = na.omit(ppgm_df) %>% select(subjectkey, PPGM_1,PPGM_2,PPGM_3,PPGM_4,PPGM_5,PPGM_6,PPGM_7,PPGM_8,
                                     PPGM_9,PPGM_10,PPGM_11,PPGM_12,PPGM_13,PPGM_14)
dim(ppgm_df_csv)
head(ppgm_df_csv, 2)
write.csv(ppgm_df_csv, '../output/data_cleaned/ppgm_2017.csv')

# diagnosis - PPGM labels
ppgm_df_sub_0 = ppgm_df_csv %>% 
                mutate(subScore_Problems = PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7,
                      subScore_ImpairedControl = PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11,
                      subScore_OtherIssues = PPGM_12 + PPGM_13 + PPGM_14,
                      TotalScore = subScore_Problems + subScore_ImpairedControl + subScore_OtherIssues) %>%
                mutate(diagnosis_ppgm = case_when(
                    (subScore_Problems >= 1 )&(subScore_ImpairedControl >= 1)&(TotalScore >= 5) ~ 'pathological_gambler',
                    (subScore_Problems >= 1)&(subScore_ImpairedControl >= 1)&((TotalScore<=4) & (TotalScore>=2)) ~ 'problem_gambler')
                )
ppgm_df_sub_2 = ppgm_df_sub_0 %>% 
                filter(is.na(diagnosis_ppgm)) %>% 
                mutate(diagnosis_ppgm = case_when((TotalScore) >= 1 ~ 'atRisk_gambler',
                                                 (TotalScore) < 1 ~  'recreational_gambler')) 
ppgm_df_sub_1 = ppgm_df_sub_0 %>%  filter(!is.na(diagnosis_ppgm)) 

ppgm_diagnosis = rbind(ppgm_df_sub_1,ppgm_df_sub_2) %>% select(subjectkey,diagnosis_ppgm)
head(ppgm_diagnosis,2)
dim(ppgm_diagnosis)
table(ppgm_diagnosis$diagnosis_ppgm)
# write.csv(ppgm_diagnosis, 'output/ppgm_label_2017.csv')


