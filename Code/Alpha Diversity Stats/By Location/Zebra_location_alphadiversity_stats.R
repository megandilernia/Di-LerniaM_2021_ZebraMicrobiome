## Alpha diversity stats by zebra location ##

setwd("[insert working directory]")

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(purrr)
library(broom)
------------------------------------------------------------------------------------------------
alphadiversity <-read_excel(path="Zebra_alphadiversity.xlsx")
#filter(method=='ave') %>%
#select(group,sobs,shannon,invsimpson,coverage,ace,chao)
------------------------------------------------------------------------------------------------
##GG plots
  ## Individual (Animalcode) - switch out sample when you want to make these, this is just for OTUs
ggplot(alphadiversity, aes(sample=OTUs, group=Location,color=Location))+ geom_qq() +stat_qq_line()
theme_classic() #code to get rid of background 
ggsave("Zebrabylocation_OTUs_qqplot.pdf") 

#histogram  - switch out for which alpha diversity factor you want 
ggplot(alphadiversity, aes(x=OTUs))+ geom_histogram(binwidth = 10)
ggsave("Zebra_OTUs_histogram.pdf") 
-------------------------------------------------------------------------------------------------

#Shapiro_Wilk normality test
  #If normally distributed move on to AOV and Tukey's test, if not move to Kruskal-Wallis 
  # Results = Small p-value (less than 0.05) = not normally distributed 
alphadiversity %>% pull(OTUs)%>% shapiro.test()
alphadiversity %>% pull(coverage)%>% shapiro.test()
alphadiversity %>% pull(shannon)%>% shapiro.test()
alphadiversity %>% pull(chao)%>% shapiro.test()
alphadiversity %>% pull(ace)%>% shapiro.test()

----------------------------------------------------------------------------------
#AOV - One way ANOVA
  ## Only need to do an AOV if normally distributed = above 0.05
  #Only one from the above Shapiro test were normally distributed
  
#Shannon Diversity Index
Location_shannon_aov <-aov(shannon~Location, data=alphadiversity)  
summary(Location_shannon_aov)

----------------------------------------------------------------------------------
#Tukey's Test
 ##do a Tukey's ONLY if the AOV p<0.05 
  #AOVs was signficant, so need a Tukey's test
TukeyHSD(Location_shannon_aov)

---------------------------------------------------------------------------------
#Kruskal Test if the results of the Shapiro test are not normally distributed (p<0.05)
    #Individual
kruskal.test(OTUs~Location, data=alphadiversity) 
kruskal.test(coverage~Location, data=alphadiversity) 
kruskal.test(chao~Location, data=alphadiversity) 
kruskal.test(ace~Location, data=alphadiversity) 

----------------------------------------------------------------------------------
#Pairwise Wilcoxon Rank Test (proceed if p-value less than 0.05 from Kruskal test, BH=Benjamini & Hochberg)
pairwise.wilcox.test(g=alphadiversity$Location, x=alphadiversity$OTUs, p.adjust.method = "BH")
pairwise.wilcox.test(g=alphadiversity$Location, x=alphadiversity$coverage, p.adjust.method = "BH")
pairwise.wilcox.test(g=alphadiversity$Location, x=alphadiversity$chao, p.adjust.method = "BH")
pairwise.wilcox.test(g=alphadiversity$Location, x=alphadiversity$ace, p.adjust.method = "BH")



