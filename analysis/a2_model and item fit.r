library(dplyr)
ppgm_df_csv = read.csv( '../output/data_cleaned/ppgm_2016.csv', row = 1)

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

single.factor = fitMeasures(cfa_1, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))

two.factor = fitMeasures(cfa_2, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))

two.factor.re = fitMeasures(cfa_2_re, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))

bifactor = fitMeasures(cfa_bi, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))

three.factor = fitMeasures(cfa_3, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))

df_fit = round(as.data.frame(rbind(single.factor, two.factor,  two.factor.re, bifactor, three.factor)),3)
df_fit$npar = c(summary(cfa_1)$optim$npar,  summary(cfa_2)$optim$npar, summary(cfa_2_re)$optim$npar, summary(cfa_bi)$optim$npar, summary(cfa_3)$optim$npar)
df_fit = df_fit %>% select(npar, chisq, srmr, rmsea.scaled, tli.scaled, cfi.scaled) %>% rename(TLI = tli.scaled, CFI = cfi.scaled, RMSEA = rmsea.scaled, SRMR = srmr)
df_fit_w1 = df_fit
df_fit_w1

# explained variance R-square r2
One.Factor = lavInspect(cfa_1, 'rsquare')
Two.Factor = lavInspect(cfa_2, 'rsquare')
Two.Factor.Respecified = lavInspect(cfa_2_re, 'rsquare')
Bifactor = lavInspect(cfa_bi, 'rsquare')
Three.Factor = lavInspect(cfa_3, 'rsquare')

df_r2 = as.data.frame(cbind(One.Factor, Two.Factor, Two.Factor.Respecified, Bifactor, Three.Factor)) %>% tibble::rownames_to_column('items')

df_r2_long = df_r2 %>% 
    tidyr::pivot_longer(cols = 2:ncol(df_r2), names_to ="models", values_to = "r2") %>% 
    mutate(models = factor(models, levels = c('One.Factor',  'Two.Factor',  'Two.Factor.Respecified', 'Bifactor', 'Three.Factor')),
           items = factor(items, levels= df_r2$items, 
                          labels = rev(c('A3tol', 'A2wit', 'A1pre', 'I4oth', 'I3imp', 'I2cha', 'I1lim', 'H7oth', 'H6law', 'H5wor', 'H4hea', 'H3rel', 'H2emo', 'H1fin'))))


library(ggplot2)
#color-blind friendly
mypal <- c("#000000", "#E69F00", "#CC79A7", "#56B4E9", "#009E73")
               
p= ggplot(df_r2_long, aes(x=items, y=r2, group = models)) + 
    geom_point(aes(color=models), size=4)+ 
    theme_bw()+
    geom_line(aes(color=models, linetype = models),size = 4 )+
    scale_colour_manual(values = c('One.Factor' = mypal[1], 
                                   'Two.Factor' = mypal[2],
                                   'Two.Factor.Respecified' = mypal[3],
                                   'Bifactor' = mypal[4], 
                                   'Three.Factor' = mypal[5]))+
    # scale_color_brewer(palette = cbPalette)+ 
    scale_linetype_manual(values=c(3,1,1,4,5))+
    theme(axis.text.x = element_text(angle=90),
         text = element_text(size=40),         
         legend.key.size = unit(7, 'cm'),
         legend.title = element_text(size=40),
         legend.text = element_text(size=35))+
         xlab("") + 
         ylab("r2") +
         ggtitle("Wave 1")    
ggsave('../output/figures/item_r2_w1.png', plot = p,  width=22, height=16)

library(dplyr)
ppgm_df_csv = read.csv( '../output/data_cleaned/ppgm_2017.csv', row = 1)

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

single.factor = fitMeasures(cfa_1, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))

two.factor = fitMeasures(cfa_2, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))

two.factor.re = fitMeasures(cfa_2_re, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))

bifactor = fitMeasures(cfa_bi, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))

three.factor = fitMeasures(cfa_3, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))

df_fit = round(as.data.frame(rbind(single.factor, two.factor,  two.factor.re, bifactor, three.factor)),3)
df_fit$npar = c(summary(cfa_1)$optim$npar,  summary(cfa_2)$optim$npar, summary(cfa_2_re)$optim$npar, summary(cfa_bi)$optim$npar, summary(cfa_3)$optim$npar)
df_fit = df_fit %>% select(npar, chisq, srmr, rmsea.scaled, tli.scaled, cfi.scaled) %>% rename(TLI = tli.scaled, CFI = cfi.scaled, RMSEA = rmsea.scaled, SRMR = srmr)
df_fit_w2 = df_fit
df_fit_w2



# combine two waves
df_both = cbind(df_fit_w1, df_fit_w2) 
df_both 
write.csv(df_fit_w1, '../output/tables/model fit/model_fit_w1.csv')
write.csv(df_fit_w2, '../output/tables/model fit/model_fit_w2.csv')

# explained variance R-square r2
One.Factor = lavInspect(cfa_1, 'rsquare')
Two.Factor = lavInspect(cfa_2, 'rsquare')
Two.Factor.Respecified = lavInspect(cfa_2_re, 'rsquare')
Bifactor = lavInspect(cfa_bi, 'rsquare')
Three.Factor = lavInspect(cfa_3, 'rsquare')

df_r2 = as.data.frame(cbind(One.Factor, Two.Factor, Two.Factor.Respecified, Bifactor, Three.Factor)) %>% tibble::rownames_to_column('items')

df_r2_long = df_r2 %>% 
    tidyr::pivot_longer(cols = 2:ncol(df_r2), names_to ="models", values_to = "r2") %>% 
    mutate(models = factor(models, levels = c('One.Factor',  'Two.Factor',  'Two.Factor.Respecified', 'Bifactor', 'Three.Factor')),
           items = factor(items, levels= df_r2$items, 
                          labels = rev(c('A3tol', 'A2wit', 'A1pre', 'I4oth', 'I3imp', 'I2cha', 'I1lim', 'H7oth', 'H6law', 'H5wor', 'H4hea', 'H3rel', 'H2emo', 'H1fin'))))


library(ggplot2)
#color-blind friendly
mypal <- c("#000000", "#E69F00", "#CC79A7", "#56B4E9", "#009E73")
               
p= ggplot(df_r2_long, aes(x=items, y=r2, group = models)) + 
    geom_point(aes(color=models), size=4)+ 
    theme_bw()+
    geom_line(aes(color=models, linetype = models),size = 4 )+
    scale_colour_manual(values = c('One.Factor' = mypal[1], 
                                   'Two.Factor' = mypal[2],
                                   'Two.Factor.Respecified' = mypal[3],
                                   'Bifactor' = mypal[4], 
                                   'Three.Factor' = mypal[5]))+
    # scale_color_brewer(palette = cbPalette)+ 
    scale_linetype_manual(values=c(3,1,1,4,5))+
    theme(axis.text.x = element_text(angle=90),
         text = element_text(size=40),         
         legend.key.size = unit(7, 'cm'),
         legend.title = element_text(size=40),
         legend.text = element_text(size=35))+
         xlab("") + 
         ylab("r2") +
         ggtitle("Wave 2")    
ggsave('../output/figures/item_r2_w2.png', plot = p,  width=22, height=16)


