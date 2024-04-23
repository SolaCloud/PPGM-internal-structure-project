#!/usr/bin/env python
# coding: utf-8

# # wave 1 - prepare data

# In[1]:


import pandas as pd
two_factor = pd.read_csv('../output/tables/permutation/two_factor_mixing.csv')
bi_factor = pd.read_csv('../output/tables/permutation/Bifactor_mixing.csv').replace('Bifactor', 'bifactor')
three_factor_half = pd.read_csv('../output/tables/permutation/three_factor_mixing_halfShuffle.csv').replace('three.factor', 'three.factor.half')
three_factor_full = pd.read_csv('../output/tables/permutation/three_factor_mixing_fullShuffle.csv').replace('three.factor', 'three.factor.full')

# merge data
data_merged = pd.concat([two_factor, bi_factor, three_factor_half, three_factor_full])
data_merged = data_merged.rename(columns={"Unnamed: 0": "models","tli.scaled":"TLI", "rmsea.scaled":"RMSEA", "cfi.scaled":"CFI", "srmr":"SRMR"}).drop(columns=['df'])

data_long = pd.melt(data_merged, id_vars=['models'], value_vars=['chisq', 'SRMR',  'RMSEA', 'TLI',  'CFI'], var_name='Measure')

# theory-driven results
gold = pd.read_csv('../output/tables/model fit/model_fit_w1.csv').rename(columns={"Unnamed: 0": "models"})
gold_w1 =  gold [["models", "chisq", "SRMR", "RMSEA", "TLI", "CFI"]]

gold = pd.read_csv('../output/tables/model fit/model_fit_w2.csv').rename(columns={"Unnamed: 0": "models"})
gold_w2 =  gold [["models", "chisq", "SRMR", "RMSEA", "TLI", "CFI"]]


# In[2]:


import seaborn as sns
import matplotlib.pyplot as plt

df = data_long

#set font and theme

sns.set(font_scale=2)
sns.set_style("darkgrid", {'axes.grid' : False})

p = sns.displot(
    df, x="value", col="Measure", row="models", common_bins = False, facet_kws=dict(margin_titles=True, sharex=False, sharey=True), 
    hue="Measure" , palette = sns.color_palette("flare").as_hex()[0:5], legend = False)

# set common x axis range for each measure 
col = 0
# lim = (data_merged.chisq.min(), data_merged.chisq.max())
lim = (gold_w1.loc[3, 'chisq']-3, data_merged.chisq.max())
for i in range(0,4):
    p.axes[i,col].set_xlim(lim)

col = 1
# lim = (data_merged.SRMR.min(),data_merged.SRMR.max())
lim = (gold_w1.loc[3, 'SRMR']-0.01,data_merged.SRMR.max())
for i in range(0,4):
    p.axes[i,col].set_xlim(lim)

col = 2
lim = (data_merged.RMSEA.min(), data_merged.RMSEA.max())
for i in range(0,4):
    p.axes[i,col].set_xlim(lim)

col = 3
lim = (data_merged.TLI.min(), data_merged.TLI.max())
for i in range(0,4):
    p.axes[i,col].set_xlim(lim)
    
col = 4
lim = (data_merged.CFI.min(),data_merged.CFI.max())
for i in range(0,4):
    p.axes[i,col].set_xlim(lim)

        
# add vertical line    
line_position = list(gold_w1.loc[1,][1:6])+ list(gold_w1.loc[3,])[1:6]+ list(gold_w1.loc[4,])[1:6]+ list(gold_w1.loc[4,])[1:6]
for ax, pos in zip(p.axes.flat, line_position):
    ax.axvline(x=pos, color='darkred', linestyle='--')

plt.tight_layout()
plt.show()

p.savefig("../output/figures/permutation_w1.png")


# # wave 2 prepare data

# In[3]:


import pandas as pd
two_factor = pd.read_csv('../output/tables/permutation/two_factor_mixing_w2.csv')
bi_factor = pd.read_csv('../output/tables/permutation/Bifactor_mixing_w2.csv').replace('Bifactor', 'bifactor')
three_factor_half = pd.read_csv('../output/tables/permutation/three_factor_mixing_halfShuffle_w2.csv').replace('three.factor', 'three.factor.half')
three_factor_full = pd.read_csv('../output/tables/permutation/three_factor_mixing_fullShuffle_w2.csv').replace('three.factor', 'three.factor.full')

# merge data
data_merged = pd.concat([two_factor, bi_factor, three_factor_half, three_factor_full])
data_merged = data_merged.rename(columns={"Unnamed: 0": "models","tli.scaled":"TLI", "rmsea.scaled":"RMSEA", "cfi.scaled":"CFI", "srmr":"SRMR"}).drop(columns=['df'])

# drop outliers (this only occurs in wave 2)
data_merged = data_merged.query('chisq < 1000')
data_long = pd.melt(data_merged, id_vars=['models'], value_vars=['chisq', 'SRMR',  'RMSEA', 'TLI',  'CFI'], var_name='Measure')


# In[4]:


import seaborn as sns
import matplotlib.pyplot as plt

df = data_long

#set font and theme
sns.set(font_scale=2)
sns.set_style("darkgrid", {'axes.grid' : False})

p = sns.displot(
    df, x="value", col="Measure", row="models", common_bins = False, facet_kws=dict(margin_titles=True, sharex=False, sharey=True), 
    hue="Measure" , palette = sns.color_palette("crest").as_hex()[0:5], legend = False)

# set common x axis range for each measure 
col = 0
# lim = (data_merged.chisq.min(), data_merged.chisq.max())
lim = (gold_w2.loc[3, 'chisq']-3, data_merged.chisq.max())
for i in range(0,4):
    p.axes[i,col].set_xlim(lim)

col = 1
lim = (data_merged.SRMR.min(),data_merged.SRMR.max())
# lim = (gold_w2.loc[3, 'SRMR']-0.01,data_merged.SRMR.max())
for i in range(0,4):
    p.axes[i,col].set_xlim(lim)

col = 2
lim = (data_merged.RMSEA.min(), data_merged.RMSEA.max())
for i in range(0,4):
    p.axes[i,col].set_xlim(lim)

col = 3
lim = (data_merged.TLI.min(), data_merged.TLI.max())
for i in range(0,4):
    p.axes[i,col].set_xlim(lim)
    
col = 4
lim = (data_merged.CFI.min(),data_merged.CFI.max())
for i in range(0,4):
    p.axes[i,col].set_xlim(lim)

        
# add vertical line    
line_position = list(gold_w2.loc[1,])[1:6]+ list(gold_w2.loc[3,])[1:6]+ list(gold_w2.loc[4,])[1:6]+ list(gold_w2.loc[4,])[1:6]
for ax, pos in zip(p.axes.flat, line_position):
    ax.axvline(x=pos, color='darkred', linestyle='--')

plt.tight_layout()
plt.show()

p.savefig("../output/figures/permutation_w2.png")


# In[ ]:




