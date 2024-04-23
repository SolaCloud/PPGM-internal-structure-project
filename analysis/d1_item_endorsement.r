library(dplyr)
ppgm_df_csv = read.csv( '../output/data_cleaned/ppgm_2016.csv', row = 1) 
dat = ppgm_df_csv %>% select(-subjectkey)
head(dat,2)
dim(dat)

endorse_rate = round(colSums(dat)/nrow(dat),3)
df_1 = as.data.frame(endorse_rate) %>% tibble::rownames_to_column('items') %>% mutate(order = seq(1,14))
df_1

ppgm_df_csv = read.csv( '../output/data_cleaned/ppgm_2017.csv', row = 1)
dat = ppgm_df_csv %>% select(-subjectkey)
head(dat,2)
dim(dat)

endorse_rate = round(colSums(dat)/nrow(dat),3)
df_2 = as.data.frame(endorse_rate) %>% tibble::rownames_to_column('items') 
df_2

df_both = merge(df_1, df_2, by = 'items', suffixes = c('_w1','_w2')) %>% arrange(order) %>% select(-order)
df_both$abbrev = c('H1fin', 'H2emo','H3rel','H4hea','H5wor','H6law','H7oth','I1lim','I2cha','I3imp','I4oth','A1pre','A2wit','A3tol')
df_both$contents = c('financial concerns','mental stress','relationship issues','health problems',
                        'work or school problems','illegal acts','others see harms','excessive gambling',
                        'chasing losses','failed control attempts','others see impaired control',
                        'preoccupation','withdrawal','tolerance')
df_both = df_both %>% select(items, abbrev, contents, endorse_rate_w1, endorse_rate_w2)
df_both
write.csv(df_both, '../output/tables/endorsement/endorsement_rate_bothwaves.csv')


