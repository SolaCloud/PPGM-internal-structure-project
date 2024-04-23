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

# factor score for wave 1
single.factor = as.data.frame(lavPredict(cfa_1))$f1

two.factor.f1 = as.data.frame(lavPredict(cfa_2))$f1
two.factor.f2 = as.data.frame(lavPredict(cfa_2))$f2

three.factor.f1 = as.data.frame(lavPredict(cfa_3))$f1
three.factor.f2 = as.data.frame(lavPredict(cfa_3))$f2
three.factor.f3 = as.data.frame(lavPredict(cfa_3))$f3

bifactor.f1 = as.data.frame(lavPredict(cfa_bi))$f1
bifactor.f2 = as.data.frame(lavPredict(cfa_bi))$f2
bifactor.g = as.data.frame(lavPredict(cfa_bi))$g

two.factor.re.f1 = as.data.frame(lavPredict(cfa_2_re))$f1
two.factor.re.f2 = as.data.frame(lavPredict(cfa_2_re))$f2

score_w1 = data.frame(subjectkey_w1, 
                      single.factor, 
                      two.factor.f1, two.factor.f2,   
                      bifactor.f1, bifactor.f2, bifactor.g,
                      two.factor.re.f1, two.factor.re.f2,
                      three.factor.f1, three.factor.f2, three.factor.f3)

write.csv(score_w1, '../output/data_cleaned/model_scores_w1.csv')

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

# factor score for wave 2
single.factor = as.data.frame(lavPredict(cfa_1))$f1

two.factor.f1 = as.data.frame(lavPredict(cfa_2))$f1
two.factor.f2 = as.data.frame(lavPredict(cfa_2))$f2

three.factor.f1 = as.data.frame(lavPredict(cfa_3))$f1
three.factor.f2 = as.data.frame(lavPredict(cfa_3))$f2
three.factor.f3 = as.data.frame(lavPredict(cfa_3))$f3

bifactor.f1 = as.data.frame(lavPredict(cfa_bi))$f1
bifactor.f2 = as.data.frame(lavPredict(cfa_bi))$f2
bifactor.g = as.data.frame(lavPredict(cfa_bi))$g

two.factor.re.f1 = as.data.frame(lavPredict(cfa_2_re))$f1
two.factor.re.f2 = as.data.frame(lavPredict(cfa_2_re))$f2


score_w2 = data.frame(subjectkey_w2, 
                      single.factor, 
                      two.factor.f1, two.factor.f2,   
                      bifactor.f1, bifactor.f2, bifactor.g,
                      two.factor.re.f1, two.factor.re.f2,
                      three.factor.f1, three.factor.f2, three.factor.f3)

write.csv(score_w2, '../output/data_cleaned/model_scores_w2.csv')


