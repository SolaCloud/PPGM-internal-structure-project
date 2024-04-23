# import data
library(dplyr)
ppgm_df_csv = read.csv( '../output/data_cleaned/ppgm_2016.csv', row = 1)

dat = ppgm_df_csv %>% select(-subjectkey)
head(dat,2)
dim(dat)

# specify models
library(lavaan)
# general factor model
mod_1 <- "
g =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7 + PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 + PPGM_12 + PPGM_13 + PPGM_14
"
# # correlated factor model (k=2）
# mod_2 <- "
# f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7
# f2 =~ PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 + PPGM_12 + PPGM_13 + PPGM_14
# f1 ~~ f2
# "

# # correlated factor model (k=3）
# mod_3 <- "
# f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7
# f2 =~ PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 
# f3 =~ PPGM_12 + PPGM_13 + PPGM_14
# f1 ~~ f2
# f1 ~~ f3
# f2 ~~ f3
# "


#their equivalent higher-oder models
# higher-order model 
mod_secondOrder_2 <- "
f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7
f2 =~ PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 + PPGM_12 + PPGM_13 + PPGM_14

g =~ 1*f1 + 1*f2 
f1 ~~ 0*f2 
"

mod_secondOrder_3 <- "
f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7
f2 =~ PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 
f3 =~ PPGM_12 + PPGM_13 + PPGM_14

g =~ f1 + f2 + f3
f1 ~~ 0*f2 + 0*f3
f2 ~~ 0*f3
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

mod_secondOrder_re <- "
f1 =~ PPGM_1 + PPGM_2 + PPGM_3 + PPGM_4 + PPGM_5 + PPGM_6 + PPGM_7
f2 =~ PPGM_8 + PPGM_9 + PPGM_10 + PPGM_11 + PPGM_12 + PPGM_13 + PPGM_14
g =~ 1*f1 + 1*f2
f1 ~~ 0*f2

PPGM_7 ~~ PPGM_11
"

cfa_1 <- lavaan::cfa(model = mod_1, data=dat, ordered =T, estimator = 'WLSMV')
cfa_2 <- lavaan::cfa(model = mod_secondOrder_2, data=dat, ordered =T, estimator = 'WLSMV')
cfa_3 <- lavaan::cfa(model = mod_secondOrder_3, data=dat, ordered =T, estimator = 'WLSMV')
cfa_bi <- lavaan::cfa(model = mod_bifactor, data=dat, ordered =T, estimator = 'WLSMV', parameterization = 'theta')
cfa_2_re <- lavaan::cfa(model = mod_secondOrder_re, data=dat, ordered =T, estimator = 'WLSMV')

template <- data.frame(Index = character(),
                       Factor = character(),
                       single_w1 = numeric(),
                       two_w1 = numeric(),                       
                       two_re_w1 = numeric(),
                       three.factor_w1= numeric(),
                       bifactor_w1 = numeric(),
                       
                       single_w2 = numeric(),
                       two_w2 = numeric(),
                       two_re_w2 = numeric(),                       
                       three.factor_w2 = numeric(),
                       bifactor_w2 = numeric())

template[1:10,'Index'] = c('H','', '', '', '','OmegaH', '', '', '', '')
template[1:10,'Factor'] = c('General','Harm', 'Dependence', 'Impaired control', 'Other issues','General','Harm', 'Dependence', 'Impaired control', 'Other issues')
template

template[is.na(template)]=''

# construct reliability (H-index, how well the latent variables are represented by the items assigned to them, similar to the R-squared in regression)
library(BifactorIndicesCalculator)

Lambda = inspect(cfa_1, what="std")$lambda
template[1,'single_w1'] = round(H(Lambda),2)

Lambda = inspect(cfa_2, what="std")$lambda
template[c(2,3),'two_w1'] = round(H(Lambda),2)[1:2]

Lambda = inspect(cfa_2_re, what="std")$lambda
template[c(2,3),'two_re_w1'] = round(H(Lambda),2)[1:2]

Lambda = inspect(cfa_3, what="std")$lambda
template[c(2,4,5),'three.factor_w1'] = round(H(Lambda),2)[1:3]

Lambda = inspect(cfa_bi, what="std")$lambda
template[c(2,3, 1),'bifactor_w1'] = round(H(Lambda),2)

# omega hirarchical
library(semTools)

template[6,'single_w1'] = round(reliability(cfa_1)['omega', 'g'],2)

df_relia = round(reliability(cfa_2),2)
template[6,'two_w1'] = round(reliabilityL2(cfa_2, "g")[['omegaL1']],2)
template[7,'two_w1'] = df_relia['omega', 'f1']
template[8,'two_w1'] = df_relia['omega', 'f2']

df_relia = round(reliability(cfa_2_re),2)
template[6,'two_re_w1'] = round(reliabilityL2(cfa_2_re, "g")[['omegaL1']],2)
template[7,'two_re_w1'] = df_relia['omega', 'f1']
template[8,'two_re_w1'] = df_relia['omega', 'f2']

template[6,'three.factor_w1'] = round(reliabilityL2(cfa_3, "g")[['omegaL1']],2)
df_relia = round(reliability(cfa_3),2)
template[7,'three.factor_w1'] = df_relia['omega', 'f1']
template[9,'three.factor_w1'] = df_relia['omega', 'f2']
template[10,'three.factor_w1'] = df_relia['omega', 'f3']


df_relia = round(reliability(cfa_bi),2)
template[6,'bifactor_w1'] = df_relia['omega', 'g']
template[7,'bifactor_w1'] = df_relia['omega', 'f1']
template[8,'bifactor_w1'] = df_relia['omega', 'f2']

template

# import data
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

# import data
library(dplyr)
ppgm_df_csv = read.csv( '../output/data_cleaned/ppgm_2017.csv', row = 1)
dat = ppgm_df_csv %>% select(-subjectkey)
head(dat,2)
dim(dat)


cfa_1 <- lavaan::cfa(model = mod_1, data=dat, ordered =T, estimator = 'WLSMV')
cfa_2 <- lavaan::cfa(model = mod_secondOrder_2, data=dat, ordered =T, estimator = 'WLSMV')
cfa_3 <- lavaan::cfa(model = mod_secondOrder_3, data=dat, ordered =T, estimator = 'WLSMV')
cfa_bi <- lavaan::cfa(model = mod_bifactor, data=dat, ordered =T, estimator = 'WLSMV', parameterization = 'theta')
cfa_2_re <- lavaan::cfa(model = mod_secondOrder_re, data=dat, ordered =T, estimator = 'WLSMV')

Lambda = inspect(cfa_1, what="std")$lambda
template[1,'single_w2'] = round(H(Lambda),2)

Lambda = inspect(cfa_2, what="std")$lambda
template[c(2,3),'two_w2'] = round(H(Lambda),2)[1:2]

Lambda = inspect(cfa_2_re, what="std")$lambda
template[c(2,3),'two_re_w2'] = round(H(Lambda),2)[1:2]

Lambda = inspect(cfa_3, what="std")$lambda
template[c(2,4,5),'three.factor_w2'] = round(H(Lambda),2)[1:3]

Lambda = inspect(cfa_bi, what="std")$lambda
template[c(2,3, 1),'bifactor_w2'] = round(H(Lambda),2)


template[6,'single_w2'] = round(reliability(cfa_1)['omega', 'g'],2)

df_relia = round(reliability(cfa_2),2)
template[6,'two_w2'] = round(reliabilityL2(cfa_2, "g")[['omegaL1']],2)
template[7,'two_w2'] = df_relia['omega', 'f1']
template[8,'two_w2'] = df_relia['omega', 'f2']

df_relia = round(reliability(cfa_2_re),2)
template[6,'two_re_w2'] = round(reliabilityL2(cfa_2_re, "g")[['omegaL1']],2)
template[7,'two_re_w2'] = df_relia['omega', 'f1']
template[8,'two_re_w2'] = df_relia['omega', 'f2']

template[6,'three.factor_w2'] = round(reliabilityL2(cfa_3, "g")[['omegaL1']],2)
df_relia = round(reliability(cfa_3),2)
template[7,'three.factor_w2'] = df_relia['omega', 'f1']
template[9,'three.factor_w2'] = df_relia['omega', 'f2']
template[10,'three.factor_w2'] = df_relia['omega', 'f3']


df_relia = round(reliability(cfa_bi),2)
template[6,'bifactor_w2'] = df_relia['omega', 'g']
template[7,'bifactor_w2'] = df_relia['omega', 'f1']
template[8,'bifactor_w2'] = df_relia['omega', 'f2']
template
write.csv(template, '../output/tables/reliability/reliability_H_omegaH.csv')


