# prepare data
library(dplyr)
library(lavaan)

ppgm_df_csv = read.csv( '../output/data_cleaned/ppgm_2016.csv', row = 1)

dat = ppgm_df_csv %>% select(-subjectkey)
head(dat,2)
dim(dat)

pool = c('PPGM_1', 'PPGM_2' , 'PPGM_3' , 'PPGM_4', 'PPGM_5' ,'PPGM_6' , 'PPGM_7' , 'PPGM_8' , 'PPGM_9' , 'PPGM_10' , 'PPGM_11' , 'PPGM_12' ,'PPGM_13' , 'PPGM_14')

# for two-factor model
n_repete = 200
two.factor_df = NULL
for (i in 1:n_repete){
    set.seed(i)
    the_sample = sample(pool, size=7, replace = FALSE)

    f1_items = the_sample
    f2_items = pool[!pool %in% the_sample] 

    part1 = paste0('f1 =~ ', paste(f1_items, collapse = ' + '))
    part2 = paste0('f2 =~ ', paste(f2_items, collapse = ' + '))
    part3 = 'f1 ~~ f2'
    mod_2 = c(part1, part2, part3)
    cfa_2 <- lavaan::cfa(model = mod_2, data=dat, ordered =T, estimator = 'WLSMV', warn = F)
    two.factor = fitMeasures(cfa_2, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled","srmr"))
    two.factor_df = rbind(two.factor_df, two.factor)    
}

write.csv(two.factor_df, '../output/tables/permutation/two_factor_mixing.csv')

part_fixed = "
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
# for bifactor model
n_repete = 200
Bifactor_df = NULL
for (i in 1:n_repete){
    set.seed(i)
    the_sample = sample(pool, size=7, replace = FALSE)

    f1_items = the_sample
    f2_items = pool[!pool %in% the_sample] 

    part1 = paste0('f1 =~ ', paste(f1_items, collapse = ' + '))
    part2 = paste0('f2 =~ ', paste(f2_items, collapse = ' + '))
    part3 = part_fixed
    mod_bifactor = c(part1, part2, part3)
    cfa_bi <- lavaan::cfa(model = mod_bifactor, data=dat, ordered =T, estimator = 'WLSMV', parameterization = 'theta')
    Bifactor = fitMeasures(cfa_bi, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))
    Bifactor_df = rbind(Bifactor_df, Bifactor)    
}
Bifactor_df
# model convergence is a problem

write.csv(Bifactor_df, '../output/tables/permutation/Bifactor_mixing.csv')

# for three-factor model
n_repete = 200
three.factor_df = NULL
for (i in 1:n_repete){
    set.seed(i)
    the_sample = sample(pool, size=7, replace = FALSE)

    f1_items = the_sample
    f23_items = pool[!pool %in% the_sample] 
    f2_items = sample(f23_items, size=4, replace = FALSE)
    f3_items = f23_items[!f23_items %in% f2_items]
    
    part1 = paste0('f1 =~ ', paste(f1_items, collapse = ' + '))
    part2 = paste0('f2 =~ ', paste(f2_items, collapse = ' + '))
    part3 = paste0('f3 =~ ', paste(f3_items, collapse = ' + '))
    part4 = 'f1 ~~ f2'
    part5 = 'f1 ~~ f3'
    part6 = 'f2 ~~ f3'
    mod_3 = c(part1, part2, part3, part4, part5, part6)
    cfa_3 <- lavaan::cfa(model = mod_3, data=dat, ordered =T, estimator = 'WLSMV', warn = FALSE)
    three.factor = fitMeasures(cfa_3, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))
    three.factor_df = rbind(three.factor_df, three.factor)    
}
three.factor_df_fullShuffle = three.factor_df
## this is full-shuffling; however, we only care about if the second and the third factor could be merged to one; thus half shuffling among the last 7 items would suffice!
write.csv(three.factor_df_fullShuffle, '../output/tables/permutation/three_factor_mixing_fullShuffle.csv')

# half shuffling
n_repete = 200
three.factor_df = NULL

pool_23 = c( 'PPGM_8' , 'PPGM_9' , 'PPGM_10' , 'PPGM_11' , 'PPGM_12' ,'PPGM_13' , 'PPGM_14')
for (i in 1:n_repete){
    set.seed(i)
    the_sample = sample(pool_23, size=4, replace = FALSE)

    f2_items = the_sample
    f3_items = pool_23[!pool_23 %in% the_sample] 
    
    part1 = 'f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7'
    part2 = paste0('f2 =~ ', paste(f2_items, collapse = ' + '))
    part3 = paste0('f3 =~ ', paste(f3_items, collapse = ' + '))
    part4 = 'f1 ~~ f2'
    part5 = 'f1 ~~ f3'
    part6 = 'f2 ~~ f3'
    mod_3 = c(part1, part2, part3, part4, part5, part6)
    cfa_3 <- lavaan::cfa(model = mod_3, data=dat, ordered =T, estimator = 'WLSMV', warn = FALSE)
    three.factor = fitMeasures(cfa_3, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))
    three.factor_df = rbind(three.factor_df, three.factor)    
}
three.factor_df_halfShuffle = three.factor_df

write.csv(three.factor_df_halfShuffle, '../output/tables/permutation/three_factor_mixing_halfShuffle.csv')

# prepare data
library(dplyr)
library(lavaan)

ppgm_df_csv = read.csv( '../output/data_cleaned/ppgm_2017.csv', row = 1)

dat = ppgm_df_csv %>% select(-subjectkey)
head(dat,2)
dim(dat)

# for two-factor model
n_repete = 200
two.factor_df = NULL


for (i in 1:n_repete){
    set.seed(i)
    the_sample = sample(pool, size=7, replace = FALSE)

    f1_items = the_sample
    f2_items = pool[!pool %in% the_sample] 

    part1 = paste0('f1 =~ ', paste(f1_items, collapse = ' + '))
    part2 = paste0('f2 =~ ', paste(f2_items, collapse = ' + '))
    part3 = 'f1 ~~ f2'
    mod_2 = c(part1, part2, part3)
    cfa_2 <- lavaan::cfa(model = mod_2, data=dat, ordered =T, estimator = 'WLSMV', warn = F)
    two.factor = fitMeasures(cfa_2, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled","srmr"))
    two.factor_df = rbind(two.factor_df, two.factor)    
}

write.csv(two.factor_df, '../output/tables/permutation/two_factor_mixing_w2.csv')

part_fixed = "
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

# for bifactor model
n_repete = 200
Bifactor_df = NULL
for (i in 1:n_repete){
    set.seed(i)
    the_sample = sample(pool, size=7, replace = FALSE)

    f1_items = the_sample
    f2_items = pool[!pool %in% the_sample] 

    part1 = paste0('f1 =~ ', paste(f1_items, collapse = ' + '))
    part2 = paste0('f2 =~ ', paste(f2_items, collapse = ' + '))
    part3 = part_fixed
    mod_bifactor = c(part1, part2, part3)
    cfa_bi <- lavaan::cfa(model = mod_bifactor, data=dat, ordered =T, estimator = 'WLSMV', parameterization = 'theta')
    Bifactor = fitMeasures(cfa_bi, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))
    Bifactor_df = rbind(Bifactor_df, Bifactor)    
}
# Bifactor_df
# model convergence is a problem

write.csv(Bifactor_df, '../output/tables/permutation/Bifactor_mixing_w2.csv')

# for three-factor model
n_repete = 200
three.factor_df = NULL
for (i in 1:n_repete){
    set.seed(i)
    the_sample = sample(pool, size=7, replace = FALSE)

    f1_items = the_sample
    f23_items = pool[!pool %in% the_sample] 
    f2_items = sample(f23_items, size=4, replace = FALSE)
    f3_items = f23_items[!f23_items %in% f2_items]
    
    part1 = paste0('f1 =~ ', paste(f1_items, collapse = ' + '))
    part2 = paste0('f2 =~ ', paste(f2_items, collapse = ' + '))
    part3 = paste0('f3 =~ ', paste(f3_items, collapse = ' + '))
    part4 = 'f1 ~~ f2'
    part5 = 'f1 ~~ f3'
    part6 = 'f2 ~~ f3'
    mod_3 = c(part1, part2, part3, part4, part5, part6)
    cfa_3 <- lavaan::cfa(model = mod_3, data=dat, ordered =T, estimator = 'WLSMV', warn = FALSE)
    three.factor = fitMeasures(cfa_3, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))
    three.factor_df = rbind(three.factor_df, three.factor)    
}
three.factor_df_fullShuffle = three.factor_df
## this is full-shuffling; however, we only care about if the second and the third factor could be merged to one; thus half shuffling among the last 7 items would suffice!
write.csv(three.factor_df_fullShuffle, '../output/tables/permutation/three_factor_mixing_fullShuffle_w2.csv')

# half shuffling
n_repete = 200
three.factor_df = NULL

pool_23 = c( 'PPGM_8' , 'PPGM_9' , 'PPGM_10' , 'PPGM_11' , 'PPGM_12' ,'PPGM_13' , 'PPGM_14')
for (i in 1:n_repete){
    set.seed(i)
    the_sample = sample(pool_23, size=4, replace = FALSE)

    f2_items = the_sample
    f3_items = pool_23[!pool_23 %in% the_sample] 
    
    part1 = 'f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7'
    part2 = paste0('f2 =~ ', paste(f2_items, collapse = ' + '))
    part3 = paste0('f3 =~ ', paste(f3_items, collapse = ' + '))
    part4 = 'f1 ~~ f2'
    part5 = 'f1 ~~ f3'
    part6 = 'f2 ~~ f3'
    mod_3 = c(part1, part2, part3, part4, part5, part6)
    cfa_3 <- lavaan::cfa(model = mod_3, data=dat, ordered =T, estimator = 'WLSMV', warn = FALSE)
    three.factor = fitMeasures(cfa_3, c("chisq","df","tli.scaled", "rmsea.scaled","cfi.scaled", "srmr"))
    three.factor_df = rbind(three.factor_df, three.factor)    
}
three.factor_df_halfShuffle = three.factor_df

write.csv(three.factor_df_halfShuffle, '../output/tables/permutation/three_factor_mixing_halfShuffle_w2.csv')


