library(dplyr)
ppgm_df_csv = read.csv( '../output/data_cleaned/ppgm_2016.csv', row = 1)
subjectkey_w1 = ppgm_df_csv$subjectkey
dat = ppgm_df_csv %>% select(-subjectkey)
head(dat,2)
dim(dat)

# specify models
library(lavaan)
# general factor model
mod_1 <- "
f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7 + PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 + PPGM_12 + PPGM_13 + PPGM_14
"

# correlated factor model (k=2）
mod_2 <- "
f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7
f2 =~ PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 + PPGM_12 + PPGM_13 + PPGM_14
f1 ~~ f2
"

# correlated factor model (k=3）
mod_3 <- "
f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7
f2 =~ PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 
f3 =~ PPGM_12 + PPGM_13 + PPGM_14
f1 ~~ f2
f1 ~~ f3
f2 ~~ f3
"

mod_bifactor <- "
f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7
f2 =~ PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 + PPGM_12 + PPGM_13 + PPGM_14
g =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7 + PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 + PPGM_12 + PPGM_13 + PPGM_14 
g ~~ 0*f1
g ~~ 0*f2
f1 ~~ 0*f2

PPGM_1 ~~ a*PPGM_1
PPGM_2 ~~ b*PPGM_2
PPGM_3 ~~ c*PPGM_3
PPGM_4 ~~ d*PPGM_4
PPGM_5 ~~ e*PPGM_5
PPGM_6 ~~ f*PPGM_6
PPGM_7 ~~ g*PPGM_7
PPGM_8 ~~ h*PPGM_8
PPGM_9 ~~ i*PPGM_9
PPGM_10 ~~ j*PPGM_10
PPGM_11 ~~ k*PPGM_11
PPGM_12 ~~ l*PPGM_12
PPGM_13 ~~ m*PPGM_13
PPGM_14 ~~ n*PPGM_14

a>0
b>0
c>0
d>0
e>0
f>0
g>0
h>0
i>0
j>0
k>0
l>0
m>0
n>0

"

mod_2_re <- "
f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7
f2 =~ PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 + PPGM_12 + PPGM_13 + PPGM_14
f1 ~~ f2

PPGM_7 ~~ PPGM_11
"

cfa_1 <- lavaan::cfa(model = mod_1, data=dat, ordered =T, estimator = 'WLSMV')
cfa_2 <- lavaan::cfa(model = mod_2, data=dat, ordered =T, estimator = 'WLSMV')
cfa_3 <- lavaan::cfa(model = mod_3, data=dat, ordered =T, estimator = 'WLSMV')
cfa_bi <- lavaan::cfa(model = mod_bifactor, data=dat, ordered =T, estimator = 'WLSMV',parameterization='theta')

cfa_2_re <- lavaan::cfa(model = mod_2_re, data=dat, ordered =T, estimator = 'WLSMV')

# cfa_1
raw_loading = inspect(cfa_1,what="std")$lambda 
loading_df = as.data.frame(raw_loading) 
cfa_1_wave1 = loading_df %>% mutate(cfa_1_wave1 = rowSums(loading_df)) %>% select(cfa_1_wave1)

# cfa_2
raw_loading = inspect(cfa_2,what="std")$lambda 
loading_df = as.data.frame(raw_loading) 
cfa_2_wave1 = loading_df %>% mutate(cfa_2_wave1 = rowSums(loading_df)) %>% select(cfa_2_wave1)

# cfa_3
raw_loading = inspect(cfa_3,what="std")$lambda 
loading_df = as.data.frame(raw_loading) 
cfa_3_wave1 = loading_df %>% mutate(cfa_3_wave1 = rowSums(loading_df)) %>% select(cfa_3_wave1)

# cfa_bi
raw_loading = inspect(cfa_bi,what="std")$lambda 
loading_df = as.data.frame(raw_loading) %>% select(-g) 
cfa_bi_s_wave1 = loading_df %>% mutate(cfa_bi_s_wave1 = rowSums(loading_df)) %>% select(cfa_bi_s_wave1)

raw_loading = inspect(cfa_bi,what="std")$lambda 
loading_df = as.data.frame(raw_loading) %>% select(g) 
cfa_bi_g_wave1 = loading_df %>% mutate(cfa_bi_g_wave1 = rowSums(loading_df)) %>% select(cfa_bi_g_wave1)

# cfa_2_re
raw_loading = inspect(cfa_2_re,what="std")$lambda 
loading_df = as.data.frame(raw_loading) 
cfa_2_re_wave1 = loading_df %>% mutate(cfa_2_re_wave1 = rowSums(loading_df)) %>% select(cfa_2_re_wave1)



loading_w1 = round(cbind(cfa_1_wave1, cfa_2_wave1, cfa_2_re_wave1, cfa_3_wave1, cfa_bi_g_wave1, cfa_bi_s_wave1),2)
loading_w1

df_cfa_1 = standardizedSolution(cfa_1) %>% filter(op == '=~') %>% select(lhs,rhs,se) %>% mutate(model = 'One.Factor')
df_cfa_2 = standardizedSolution(cfa_2) %>% filter(op == '=~') %>% select(lhs,rhs,se) %>% mutate(model = 'Two.Factor')
df_cfa_3 = standardizedSolution(cfa_3) %>% filter(op == '=~') %>% select(lhs,rhs,se) %>% mutate(model = 'Three.Factor')
df_cfa_3[df_cfa_3$lhs =='f2','lhs'] = 'f4'
df_cfa_3[df_cfa_3$lhs =='f3','lhs'] = 'f5'
df_cfa_bi = standardizedSolution(cfa_bi) %>% filter(op == '=~') %>% select(lhs, rhs, se) %>% mutate(model = 'BiFactor')
df_cfa_2_re = standardizedSolution(cfa_2_re) %>% filter(op == '=~') %>% select(lhs,rhs,se) %>% mutate(model = 'Two.Factor.Re')

df_error = rbind(df_cfa_1, df_cfa_2, df_cfa_3, df_cfa_bi, df_cfa_2_re) %>% 
            mutate(model = factor(model, levels = c('One.Factor', 'Two.Factor', 'Two.Factor.Re', 'Three.Factor', 'BiFactor')),
                   lhs = factor(lhs, levels = c('f1', 'f2', 'f4', 'f5','g'), 
                                labels = c('harm', 'dependence', 'impaired control', 'other issues','general')),
                   rhs = factor(rhs, levels = rev(df_cfa_1$rhs))
                  ) %>% 
            rename(standard.errors = se,
                   latent.factors = lhs
                  )
df_error_wave1 = df_error %>% mutate(year = 'wave1')

library(dplyr)
ppgm_df_csv = read.csv( '../output/data_cleaned/ppgm_2017.csv', row = 1)
subjectkey_w2 = ppgm_df_csv$subjectkey
dat = ppgm_df_csv %>% select(-subjectkey)
head(dat,2)
dim(dat)

# e>0.0001: or it will be a negative number near 0; to estimate se, the item 7 and 11 residuals are correlated
mod_bifactor <- "
f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7
f2 =~ PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 + PPGM_12 + PPGM_13 + PPGM_14
g =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7 + PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 + PPGM_12 + PPGM_13 + PPGM_14 
g ~~ 0*f1
g ~~ 0*f2
f1 ~~ 0*f2

PPGM_7 ~~ PPGM_11

PPGM_1 ~~ a*PPGM_1
PPGM_2 ~~ b*PPGM_2
PPGM_3 ~~ c*PPGM_3
PPGM_4 ~~ d*PPGM_4
PPGM_5 ~~ e*PPGM_5
PPGM_6 ~~ f*PPGM_6
PPGM_7 ~~ g*PPGM_7
PPGM_8 ~~ h*PPGM_8
PPGM_9 ~~ i*PPGM_9
PPGM_10 ~~ j*PPGM_10
PPGM_11 ~~ k*PPGM_11
PPGM_12 ~~ l*PPGM_12
PPGM_13 ~~ m*PPGM_13
PPGM_14 ~~ n*PPGM_14
a>0
b>0
c>0
d>0
e>0.0001
f>0
g>0
h>0
i>0
j>0
k>0
l>0
m>0
n>0
"

cfa_1 <- lavaan::cfa(model = mod_1, data=dat, ordered =T, estimator = 'WLSMV')
cfa_2 <- lavaan::cfa(model = mod_2, data=dat, ordered =T, estimator = 'WLSMV')
cfa_3 <- lavaan::cfa(model = mod_3, data=dat, ordered =T, estimator = 'WLSMV')
cfa_bi <- lavaan::cfa(model = mod_bifactor, data=dat, ordered =T, estimator = 'WLSMV',parameterization='theta')

cfa_2_re <- lavaan::cfa(model = mod_2_re, data=dat, ordered =T, estimator = 'WLSMV')

# cfa_1
raw_loading = inspect(cfa_1,what="std")$lambda 
loading_df = as.data.frame(raw_loading) 
cfa_1_wave1 = loading_df %>% mutate(cfa_1_wave1 = rowSums(loading_df)) %>% select(cfa_1_wave1)

# cfa_2
raw_loading = inspect(cfa_2,what="std")$lambda 
loading_df = as.data.frame(raw_loading) 
cfa_2_wave1 = loading_df %>% mutate(cfa_2_wave1 = rowSums(loading_df)) %>% select(cfa_2_wave1)

# cfa_3
raw_loading = inspect(cfa_3,what="std")$lambda 
loading_df = as.data.frame(raw_loading) 
cfa_3_wave1 = loading_df %>% mutate(cfa_3_wave1 = rowSums(loading_df)) %>% select(cfa_3_wave1)

# cfa_bi
raw_loading = inspect(cfa_bi,what="std")$lambda 
loading_df = as.data.frame(raw_loading) %>% select(-g) 
cfa_bi_s_wave1 = loading_df %>% mutate(cfa_bi_s_wave1 = rowSums(loading_df)) %>% select(cfa_bi_s_wave1)

raw_loading = inspect(cfa_bi,what="std")$lambda 
loading_df = as.data.frame(raw_loading) %>% select(g) 
cfa_bi_g_wave1 = loading_df %>% mutate(cfa_bi_g_wave1 = rowSums(loading_df)) %>% select(cfa_bi_g_wave1)

# cfa_2_re
raw_loading = inspect(cfa_2_re,what="std")$lambda 
loading_df = as.data.frame(raw_loading) 
cfa_2_re_wave1 = loading_df %>% mutate(cfa_2_re_wave1 = rowSums(loading_df)) %>% select(cfa_2_re_wave1)



loading_w2 = round(cbind(cfa_1_wave1, cfa_2_wave1, cfa_2_re_wave1, cfa_3_wave1, cfa_bi_g_wave1, cfa_bi_s_wave1),2)
colnames(loading_w2) = c('cfa_1_wave2', 'cfa_2_wave2', 'cfa_2_re_wave2', 'cfa_3_wave2', 'cfa_bi_g_wave2', 'cfa_bi_s_wave2')
loading_w2

df_cfa_1 = standardizedSolution(cfa_1) %>% filter(op == '=~') %>% select(lhs,rhs,se) %>% mutate(model = 'One.Factor')
df_cfa_2 = standardizedSolution(cfa_2) %>% filter(op == '=~') %>% select(lhs,rhs,se) %>% mutate(model = 'Two.Factor')
df_cfa_3 = standardizedSolution(cfa_3) %>% filter(op == '=~') %>% select(lhs,rhs,se) %>% mutate(model = 'Three.Factor')
df_cfa_3[df_cfa_3$lhs =='f2','lhs'] = 'f4'
df_cfa_3[df_cfa_3$lhs =='f3','lhs'] = 'f5'
df_cfa_bi = standardizedSolution(cfa_bi) %>% filter(op == '=~') %>% select(lhs, rhs, se) %>% mutate(model = 'BiFactor')
df_cfa_2_re = standardizedSolution(cfa_2_re) %>% filter(op == '=~') %>% select(lhs,rhs,se) %>% mutate(model = 'Two.Factor.Re')

df_error = rbind(df_cfa_1, df_cfa_2, df_cfa_3, df_cfa_bi, df_cfa_2_re) %>% 
            mutate(model = factor(model, levels = c('One.Factor', 'Two.Factor', 'Two.Factor.Re', 'Three.Factor', 'BiFactor')),
                   lhs = factor(lhs, levels = c('f1', 'f2', 'f4', 'f5','g'), 
                                labels = c('harm', 'dependence', 'impaired control', 'other issues','general')),
                   rhs = factor(rhs, levels = rev(df_cfa_1$rhs))
                  ) %>% 
            rename(standard.errors = se,
                   latent.factors = lhs
                  )
df_error_wave2 = df_error %>% mutate(year = 'wave2')

df_loadings_2waves = cbind(loading_w1, loading_w2) %>% 
                        select('cfa_1_wave1','cfa_2_wave1','cfa_2_re_wave1','cfa_bi_g_wave1','cfa_bi_s_wave1','cfa_3_wave1',
                               'cfa_1_wave2','cfa_2_wave2','cfa_2_re_wave2','cfa_bi_g_wave2','cfa_bi_s_wave2','cfa_3_wave2')
write.csv(df_loadings_2waves, '../output/tables/loading/df_loadings_2waves.csv')
df_loadings_2waves

# combine
library(ggplot2)
df_error_merged = rbind(df_error_wave1, df_error_wave2)

df_error_merged = df_error_merged %>% 
                    mutate(rhs = factor(rhs, 
                                        levels = c('PPGM_14','PPGM_13','PPGM_12','PPGM_11','PPGM_10','PPGM_9','PPGM_8','PPGM_7','PPGM_6','PPGM_5','PPGM_4','PPGM_3','PPGM_2','PPGM_1'), 
                                        labels = c('A3tol', 'A2wit', 'A1pre', 'I4oth', 'I3imp', 'I2cha', 'I1lim', 'H7oth', 'H6law', 'H5wor', 'H4hea', 'H3rel', 'H2emo', 'H1fin')))

df_error_merged$latent.factors = as.character(df_error_merged$latent.factors)
df_error_merged = df_error_merged %>% 
                    mutate( latent.factors = factor(latent.factors, levels = c('general', 'harm', 'dependence', 'impaired control', 'other issues')))

pal = palette.colors(palette = "Okabe-Ito")
ggplot(df_error_merged, aes(x = standard.errors, y = rhs, fill = latent.factors)) + 
    geom_bar(stat = "identity", width = .6, position="dodge") +
    scale_fill_manual(values = c(
      'general' = pal[[9]],
      'harm' = pal[[7]],
      'dependence' = pal[[4]], 
      'impaired control'  = pal[[2]], 
      'other issues' = pal[[3]])) +
    facet_grid(year~model) +
    theme_bw() +
    theme(axis.title.y = element_blank(),text = element_text(size=18), strip.text.x = element_text(size = 13), axis.text.x = element_text(size = 9))

ggsave('../output/figures/LoadingSE_combined.png',  width=12, height=7)


