library(dplyr)

### FINNISH DATA ####
# FSD3384 - 2017 Gambling Harm Survey
# FSD3261 - 2016 Gambling Harm Survey

# import finnish data
GHS2017 <- read.csv("../raw_data/daF3384e.csv", header = T, stringsAsFactors = F, sep=";")
GHS2016 <- read.csv("../raw_data/daF3261e.csv", header = T, stringsAsFactors = F, sep=";")
# merge two datasets
pool_fin = plyr::rbind.fill(GHS2016, GHS2017)

# rename columns
pool_fin_rename = pool_fin %>% 
                rename(
                    # dataset_id; case-id
                    SN = fsd_no,
                    subjectkey = fsd_id, 
                    
                    # gambling advertisement exposure frequency 15 items; 1-never; 2-occasionally; 3-often; 4-constantly; 9-not apply
                    ads_printed = a3a_a,
                    ads_vehicle = a3a_b,
                    ads_socialmedia = a3a_c,
                    ads_internet = a3a_d,
                    ads_television = a3a_e,
                    ads_publicplace = a3a_f,
                    ads_email = a3a_g,
                    ads_SMS = a3a_h,
                    ads_newsarticle = a3a_i,
                    ads_scratch = a3a_j,
                    ads_onlinegamble = a3a_k,
                    ads_promotion = a3a_l,
                    ads_freegames = a3a_m,
                    ads_bonuses = a3b_n,
                    ads_playerbenefit = a3b_o,
                    
                    # gambling participation
                    # daily 1; to not at all: 7; cannot say 9
                    freq_Lotto = b1a,
                    freq_OthLotA = b1b,
                    freq_OthLotB = b1c,
                    freq_Scratch = b1d,
                    freq_VSbet = b1e,
                    freq_VHorse = b1f,
                    freq_Casino = b1g,
                    freq_Slots = b1h,
                    freq_Table = b1i,
                    freq_PVei = b1j,
                    freq_OVei = b1k,
                    freq_FHorse = b1l,
                    freq_Private = b1m,
                    freq_Cruise = b1n,
                    freq_PPaf = b1o,
                    freq_OPaf = b1p,
                    freq_PFor = b1q,
                    freq_OFor = b1r,
                    freq_Other = b1s,
                    
                    # life-time gambling (huge system missing data)

                    # reasons for gambling, numerical code
                    r_primary = b3,
                    # reasons for gambling, not mentioned-0; mentioned-1
                    r_excitement_fun = b4_1,
                    r_win_money =  b4_2,
                    r_escape_distract =  b4_3,
                    r_socialize =  b4_4,
                    r_worthy_causes =  b4_5,
                    r_feel_good =  b4_6,
                    r_other_reason =  b4_7,
                    r_cannot_say =  b4_9,

                    # self-awareness
                    # 1-no 2-yes 3-cannot say
                    felt_gamble_a_problem = b5a,
                    # 1-4: never-occasionally-often-almost always; 9-cannot say
                    felt_gamble_a_problem_frq = b8,

                    # knowledge 1-5 very little to very much; 9-cannot say
                    k_effect_of_chance = b6a,
                    k_probability_of_winning = b6b,
                    k_warning_signs_PG = b6c,
                    k_means_for_controlling = b6d,
                    k_health_financial_effects_PG = b6e,
                    k_support_when_lost_control = b6f,
                    k_addictive_game_type = b6g,

                    # self-management measures,  
                    # a-aware; 1- yes; 2-no;
                    a_Overview_account_activity = b7a_t,
                    a_Setting_a_spending_limit = b7b_t,
                    a_Providing_information_pg = b7c_t,
                    a_Tests_recognising_pg = b7d_t,
                    a_Electronic_authentication = b7e_t,
                    a_Restrictions_time_Ofday = b7f_t,
                    a_Self_imposed_limitation = b7g_t,
                    a_Self_imposed_ban = b7h_t,
                    a_Shut_internet_account = b7i_t,
                    a_Conditional_self_exclusion = b7j_t,
                    a_Unconditional_self_exclusion = b7k_t,
                    a_Gambling_diary = b7l_t,
                    a_Not_credit_card = b7m_t,
                    a_Advice_croupier = b7n_t,
                    a_Prevention_croupier = b7o_t,
                    a_Info_after_restriction = b7p_t,
                    # a_self_management_Other = b7q_t,# too many missing.more than other items

                    # u-used; 3- yes; 4-no;
                    u_Overview_account_activity = b7a_k,
                    u_Setting_a_spending_limit = b7b_k,
                    u_Providing_information_pg = b7c_k,
                    u_Tests_recognising_pg = b7d_k,
                    u_Electronic_authentication = b7e_k,
                    u_Restrictions_time_Ofday = b7f_k,
                    u_Self_imposed_limitation = b7g_k,
                    u_Self_imposed_ban = b7h_k,
                    u_Shut_internet_account = b7i_k,
                    u_Conditional_self_exclusion = b7j_k,
                    u_Unconditional_self_exclusion = b7k_k,
                    u_Gambling_diary = b7l_k,
                    u_Not_credit_card = b7m_k,
                    u_Advice_croupier = b7n_k,
                    u_Prevention_croupier = b7o_k,
                    u_Info_after_restriction = b7p_k,
                    # u_self_management_Other = b7q_k,   # too many missing.more than other items

                    # money spent
                    spent_per_w = b9a, 
                    spent_per_m = b9b,
                    spent_the_y = b9c,
                    
                    
                    # social env, 0-never; 1-occasionally; 2-most of the time; 3-almost always; 9-cannot say
                    socioEnv_alone = b11a,
                    socioEnv_acquaintances = b11b,
                    socioEnv_strangers = b11c,

                    # gambling harms; 0-no 1-yes 9-cannot say
                    
                    # effect on life (many NA; but this is the Problem and Pathological Gambling Measure (PPGM))
                    
                    ## PROBLEMS SCORE
                    PPGM_1a = c0a,
                    PPGM_1b = c0b, # (Note: do not score 1 for this if thelast item has already been scored)
                    PPGM_2 = c0c,
                    PPGM_3a = c0d,
                    PPGM_3b = c0e, # (Note: do not score 1 for this if thelast item has already been scored)
                    PPGM_4 = c0f,
                    PPGM_5a = c0g,
                    PPGM_5b = c0h, # (Note: do not score 1 for this if thelast item has already been scored)
                    PPGM_6 = c0i,
                    PPGM_7 = c0j,
                    
                    ## IMPAIRED CONTROL SCORE
                    PPGM_8 = c0k,
                    PPGM_9 = c0l,
                    PPGM_10a = c0m, # (Note: this item IS NOT scored, SKIP NEXT ITEM IF "NO")
                    PPGM_10b = c0n, # (Note: need to reverse score BY HAND!!!, YES = 0; NO = 1)
                    PPGM_11 = c0o,
                    
                    ## OTHER ISSUES SCORE
                    PPGM_12 = c0p,
                    PPGM_13 = c0q,
                    PPGM_14 = c0r,

#                     PPGM_borrow_or_sell = c0a,
#                     PPGM_financial_concerns = c0b, # (Note: do not score 1 for this if thelast item has already been scored)
#                     PPGM_mental_stress = c0c,
#                     PPGM_relationship = c0d,
#                     PPGM_neglect_family = c0e, # (Note: do not score 1 for this if thelast item has already been scored)
#                     PPGM_health_or_injury = c0f,
#                     PPGM_work_or_school = c0g,
#                     PPGM_miss_workstudytime = c0h, # (Note: do not score 1 for this if thelast item has already been scored)
#                     PPGM_illegal_acts = c0i,
#                     PPGM_others_say_problems = c0j,
#                     PPGM_more_than_intended = c0k,
#                     PPGM_win_back_lost = c0l,
#                     PPGM_attempts_to_control = c0m, # (Note: this item IS NOT scored, SKIP NEXT ITEM IF "NO")
#                     PPGM_success_to_control = c0n, # (Note: need to reverse score BY HAND!!!, YES = 0; NO = 1)
#                     PPGM_others_say_losecontrol = c0o,
#                     PPGM_preoccupied_withgamble = c0p,
#                     PPGM_withdrawal_symptoms = c0q,
#                     PPGM_increasing_tolerance = c0r,
                    
                    # financial harm
                    hf_reduction_Available_money = c1a,
                    hf_reduction_Saving = c1b,
                    hf_reduction_recreation_expenses = c1c,
                    hf_Increased_credit_card_debt = c1d,
                    hf_Sold_personal_items = c1e,
                    hf_reduction_essential_expenses = c1f,
                    hf_reduction_beneficial_expenses = c1g,
                    hf_late_bill_payments = c1h,
                    hf_additional_employment = c1i,
                    hf_need_welfare_organizations= c1j,
                    hf_need_temporary_accommodation= c1k,
                    hf_Loss_assets = c1l,
                    hf_Loss_utility_supply = c1m,
                    hf_Bankruptcy = c1n,
                    #hf_None_of_above_finance = c1o,
                    # harm on study or work
                    hw_Reduced_performance = c2a,
                    hw_Was_late = c2b,
                    hw_Used_job_time_gamble = c2c,
                    hw_Used_job_resources_gamble = c2d,
                    hw_Was_absent = c2e,
                    hw_Conflict_colleagues = c2f,
                    hw_Lack_progression = c2g,
                    hw_Excluded_from_study = c2h,
                    hw_Hindered_job_seeking = c2i,
                    hw_Lost_job = c2j,
                    #hw_None_of_above_work = c2k,
                    # health harm
                    hh_Reduced_physical_activity = c3a,
                    hh_eat_less = c3b,
                    hh_eat_much = c3c,
                    hh_less_time_for_sleep= c3d,
                    hh_Neglected_hygiene_self_care = c3e,
                    hh_Neglected_medical_needs = c3f,
                    hh_Increased_tobacco = c3g,
                    hh_Increased_alcohol= c3h,
                    hh_loss_sleep_stress= c3i,
                    hh_Increased_depression = c3j,
                    hh_Unhygienic_living = c3k,
                    hh_Increased_health_services = c3l,
                    hh_Required_emergency_treatment = c3m,
                    hh_self_harm = c3n,
                    hh_Attempted_suicide = c3o,
                    hh_Stress_related_health_problems= c3p,
                    #hh_None_of_above_health = c3q,
                    # emotional harm
                    he_distressed = c4a,
                    he_ashamed = c4b,
                    he_regrets = c4c,
                    he_Felt_like_a_failure = c4d,
                    he_insecure = c4e,
                    he_worthless = c4f,
                    he_hopeless = c4g,
                    he_angry = c4h,
                    he_extreme_distress = c4i,
                    he_thoughts_escape = c4j,
                    #he_None_of_above_emotion = c4k,
                    # relationship harm
                    hr_less_close_time = c5a,
                    hr_Neglected_responsibilities = c5b,
                    hr_Felt_belittled = c5c,
                    hr_less_social_time = c5d,
                    hr_greater_relation_tension = c5e,
                    hr_less_enjoyment_SigOther = c5f,
                    hr_greater_relation_conflict = c5g,
                    hr_Social_isolation = c5h,
                    hr_Threat_separation = c5i,
                    hr_Actual_separation = c5j,
                    #hr_None_of_above_relationship = c5k,
                    # other harms OR "social deviance"
                    ho_children_unsupervised = c6a,
                    ho_Arrested_unsafe_driving = c6b,
                    ho_shamed_family_name = c6c,
                    ho_violence = c6d,
                    ho_Petty_theft_dishonesty = c6e,
                    ho_children_neglected = c6f,
                    ho_less_connected_community = c6g,
                    ho_outcast_community = c6h,
                    ho_Reduced_contribution_community = c6i,
                    ho_crime = c6j,
                    ho_fake_promise_payback = c6k,
                    ho_steal_ownhome = c6l,
                    #ho_None_of_above_other = c6m,
                    
                    # second-hand experience or affacted by others' gambling 
                    # yes-1; no-2
                    other_gamble_toomuch = d1a,
                    other_gamble_spouse =  d1b_1,
                    other_gamble_friend =  d1b_8,
                    other_gamble_child =  d1b_3,
                    # not mentioned -0; mentioned -1
                    affectme_emoStrain = d2a,
                    affectme_health = d2b,
                    affectme_relatMarrige = d2c,
                    affectme_relatFriendship = d2d,
                    affectme_loseHome = d2e,
                    affectme_debt = d2f,
                    affectme_kids = d2g,
                    affectme_ClosePersons = d2h,
                    affectme_emoAbuse = d2i,
                    affectme_physiViolence = d2j,
                    affectme_victimOfCrime = d2k,
                    affectme_workStudy = d2l,
                    affectme_otherWays = d2m,
                    affectme_noAffect = d2n,

                    # demographic, 
                    demog_education = e1, # 1-9; 99-cannot say
                    demog_income = e2_lk, # 1-7, 7-unknown, 1-less than 500 euros; 6-over 2500 euros
                    demog_employment = e3, # 1-11; 12-don't want to say
                    demog_gender = bv2, # 1-male, 2-female
                    demog_age_group = bv3, #1=18~24; 2=25~34;... 6=65~74; 7 =over 75
                )

# select the columns
data_fin = pool_fin_rename %>% 
            select(starts_with(c("SN", "subjectkey", "ads_", "freq_", "r_", "felt_", "k_", "a_", "u_", "spent_", "socioEnv_", "PPGM_", "hf_", "hw_", "hh_", "he_", "hr_", "ho_", "other_gamble_", "affectme_", "demog_")))

# create dataset name column
data_fin$DatasetC <- NA
data_fin$DatasetC[data_fin$SN == 3261] <- "Finnish Gambling Harms Survey 2016"
data_fin$DatasetC[data_fin$SN == 3384] <- "Finnish Gambling Harms Survey 2017"

# merge primary reasons to all reasons
data_fin <- data_fin %>%
  rowwise() %>%
  mutate(r_excitement_fun = ifelse(r_primary == 1, 1, r_excitement_fun),
         r_win_money = ifelse(r_primary == 2, 1, r_win_money),
         r_escape_distract = ifelse(r_primary == 3, r_escape_distract + 1, r_escape_distract),
         r_socialize = ifelse(r_primary == 4, 1, r_socialize),
         r_worthy_causes = ifelse(r_primary == 5, 1, r_worthy_causes),
         r_feel_good = ifelse(r_primary == 6, 1, r_feel_good),
         r_other_reason = ifelse(r_primary == 7, 1, r_other_reason),
         r_cannot_say = ifelse(r_primary == 9, 1, r_cannot_say)) %>%ungroup() %>%select(-r_primary)

head(data_fin,2)
dim(data_fin)
# write.csv(data_fin, 'output/HarmSurveyCombine.csv')

mydata_fct = data_fin
# recode data
col_names = names(mydata_fct)
col_names_fct = col_names[!col_names %in% c('SN','subjectkey','spent_the_y','spent_per_m','spent_per_w')]
mydata_fct[,col_names_fct] = lapply(mydata_fct[,col_names_fct], factor)

col_ads = colnames(select(mydata_fct, starts_with('ads_')))
col_Gamfreq = colnames(select(mydata_fct, starts_with('freq_')))
col_reasons = colnames(select(mydata_fct, starts_with('r_')))
col_feltProblem = colnames(select(mydata_fct, starts_with('felt_gamble_a_problem')))
col_knowledge = colnames(select(mydata_fct, starts_with('k_')))
col_se_aware = colnames(select(mydata_fct, starts_with('a_')))
col_se_use = colnames(select(mydata_fct, starts_with('u_')))
col_socioEnv = colnames(select(mydata_fct, starts_with('socioEnv_')))
col_PPGM = colnames(select(mydata_fct, starts_with('PPGM_')))
# col_harm_finance = colnames(select(mydata_fct, starts_with('hf_')))
# col_harm_workstudy = colnames(select(mydata_fct, starts_with('hw_')))
# col_harm_health = colnames(select(mydata_fct, starts_with('hh_')))
# col_harm_emotion = colnames(select(mydata_fct, starts_with('he_')))
# col_harm_relationship = colnames(select(mydata_fct, starts_with('hr_')))
# col_harm_other = colnames(select(mydata_fct, starts_with('ho_')))

col_harm_ALL = colnames(select(mydata_fct, starts_with('h') | starts_with('affectme_')))
col_demo = colnames(select(mydata_fct, starts_with('demog_')))


data_fin_recode = mydata_fct %>%
    mutate_at(col_ads, list(~recode(., `1`= 'Never', `2`= 'Occasionally', `3`='Often', `4`='Constantly', `9`='Cannot say'))) %>%
    mutate_at(col_Gamfreq, list(~recode(., `1`= 'Daily or almost daily', 
                                        `2`= 'Several times a week',
                                        `3`= 'Once a week',
                                        `4`= 'Two or three times a month',
                                        `5`= 'Once a month',
                                        `6`= 'Less frequently',
                                        `7`= 'Not at all in last year',
                                        `9`= 'Cannot say'))) %>%
    mutate_at(col_reasons, list(~recode(., `0`= 'NotMentioned', `1`= 'Mentioned'))) %>%
    mutate_at(col_feltProblem, list(~recode(., `1`= 'No', `2`= 'Yes', `3`='CannotSay'))) %>%
    mutate_at(col_knowledge, list(~recode(., `1`= 'Very little', `2`= 'A little', `3`='Moderately', `4`='A fair amount', `5`='Very much',`9`='Cannot say'))) %>%
    mutate_at(col_se_aware, list(~recode(., `1`= 'Yes', `2`= 'No'))) %>%
    mutate_at(col_se_use, list(~recode(., `3`= 'Yes', `4`= 'No'))) %>%
    mutate_at(col_socioEnv, list(~recode(., `0`= 'Never', `1`= 'Occasionally', `2`='Most of the time', `3`='Almost always', `9`='Cannot say'))) %>%
    mutate_at(col_PPGM, list(~recode(., `2`= 'No', `1`= 'Yes', `9`='CannotSay'))) %>%
    mutate_at(col_harm_ALL, list(~recode(., `0`= 'NotMentioned', `1`= 'Mentioned'))) %>%
    mutate_at('other_gamble_toomuch', list(~recode(., `0`= 'No', `1`= 'Yes', `9`='CannotSay'))) %>%
    mutate_at('demog_gender', list(~recode(., `1`= 'Male', `2`= 'Female')))  %>%
    mutate_at('demog_age_group', list(~recode(., `1`= '18~24', 
                                              `2`= '25~34',
                                              `3`= '35~44',
                                              `4`= '45~54',
                                              `5`= '55~64',
                                              `6`= '65~74',
                                              `7`= 'over75' )))  %>%
    mutate_at('demog_income', list(~recode(., `1`= 'Less than 500 euros', 
                                              `2`= '501-1000 euros',
                                              `3`= '1001-1500 euros',
                                              `4`= '1501-2000 euros',
                                              `5`= '2001-2500 euros',
                                              `6`= 'Over 2500 euros',
                                              `7`= 'Unknown' )))  %>%
    mutate_at('demog_education', list(~recode(., `1`= 'Lower secondary education', 
                                              `2`= 'General upper secondary qualification',
                                              `3`= 'Vocational upper secondary qualification',
                                              `4`= 'Vocational college',
                                              `5`= 'Bachelor',
                                              `6`= 'Higher academic degree',
                                              `7`= 'doctoral degree',
                                              `8`= 'Other',
                                              `9`= 'Have not obtained a degree or diploma',
                                              `99`= 'prefer not to say')))  %>%
    mutate_at('demog_employment', list(~recode(., `1`= 'Employed full time', 
                                              `2`= 'Employed part time',
                                              `3`= 'Farmer, agricultural entrepreneur',
                                              `4`= 'Other entrepreneur or self employed',
                                              `5`= 'Student or pupil',
                                              `6`= 'Unemployed or laid off',
                                              `7`= 'Pensioner/retired',
                                              `8`= 'disability pension or permanently ill',
                                              `9`= 'Conscript or in civilian service',
                                              `10`= 'Homemaker / caring for children or dependents',
                                              `11`= 'Something else',
                                              `12`= 'prefer not to say',
                                              ))) 
head(data_fin_recode,2)
dim(data_fin_recode)

# write.csv(data_fin_recode, 'output/HarmSurveyCombine_recode.csv')

data_2016 = data_fin %>% filter(DatasetC == 'Finnish Gambling Harms Survey 2016')
head(data_2016,2)
dim(data_2016)
write.csv(data_2016, '../output/data_cleaned/HarmSurvey2016.csv')

data_2016_recode = data_fin_recode %>% filter(DatasetC == 'Finnish Gambling Harms Survey 2016')
write.csv(data_2016_recode, '../output/data_cleaned/HarmSurvey2016_recode.csv')
dim(data_2016_recode)

data_2017 = data_fin %>% filter(DatasetC == 'Finnish Gambling Harms Survey 2017')
head(data_2017,2)
dim(data_2017)
write.csv(data_2017, '../output/data_cleaned/HarmSurvey2017.csv')

data_2017_recode = data_fin_recode %>% filter(DatasetC == 'Finnish Gambling Harms Survey 2017')
write.csv(data_2017_recode, '../output/data_cleaned/HarmSurvey2017_recode.csv')
dim(data_2017_recode)


