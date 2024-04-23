library(dplyr)
library(corrplot)
# year 2016
ppgm_df_csv = read.csv( '../output/data_cleaned/ppgm_2016.csv', row = 1)

dat = ppgm_df_csv %>% select(-subjectkey)
head(dat,2)
dim(dat)

# check correlated resid error pairs
thres = 0.3

cor_mat = cor(dat)
cor_mat[abs(cor_mat) <thres] = 0
cor_mat[1:7,1:7] = 0
cor_mat[8:14,8:14] = 0

png(filename = paste0("../output/figures/correlated_resid_w1.png"), height = 6, width =6, units = 'in', res =400)
corrplot(cor_mat, type='lower',tl.col = 'black')
dev.off()
corrplot(cor_mat, type='lower',tl.col = 'black')

ppgm_df_csv = read.csv('../output/data_cleaned/ppgm_2017.csv', row = 1)

dat = ppgm_df_csv %>% select(-subjectkey)
head(dat,2)
dim(dat)

thres = 0.3
cor_mat = cor(dat)
cor_mat[abs(cor_mat) < thres] = 0
cor_mat[1:7,1:7] = 0
cor_mat[8:14,8:14] = 0

png(filename = paste0("../output/figures/correlated_resid_w2.png"), height = 6, width =6, units = 'in', res =400)
corrplot(cor_mat, type='lower',tl.col = 'black')
dev.off()
corrplot(cor_mat, type='lower',tl.col = 'black')


