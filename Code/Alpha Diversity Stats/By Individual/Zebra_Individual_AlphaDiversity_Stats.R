## Stats for Alpha Diversity Factors by Individual Zebra ##

setwd("[Insert working directory")

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(purrr)
library(broom)
------------------------------------------------------------------------------------------------
phyla <-read_excel(path="Zebra_phyla.xlsx")
#filter(method=='ave') %>%
#select(group,sobs,shannon,invsimpson,coverage,ace,chao)
------------------------------------------------------------------------------------------------
##GG plots 
  ## Individual (Animalcode) - switch out sample when you want to make these, this is just for OTUs
ggplot(alphadiversity, aes(sample=OTUs, group=AnimalCode,color=AnimalCode))+ geom_qq() +stat_qq_line()
theme_classic() #code to get rid of background 
ggsave("Zebrabyindividual_OTUs_qqplot.pdf") 

#histogram  - switch out for which alpha diversity factor you want 
ggplot(alphadiversity, aes(x=OTUs))+ geom_histogram(binwidth = 10)
ggsave("Zebraby_OTUs_histogram.pdf") 
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
#AOV 
  ## Only need to do an AOV if normally distributed = above 0.05
  #Only one from the above Shapiro test were normally distributed
  
#Shannon Diversity Index
AnimalCode_shannon_aov <-aov(shannon~AnimalCode, data=alphadiversity)  
summary(AnimalCode_shannon_aov)

----------------------------------------------------------------------------------
#Tukey's Test
 ##do a Tukey's ONLY if the AOV p<0.05 
  #AOVs was signficant, so need a Tukey's test
TukeyHSD(AnimalCode_shannon_aov)

---------------------------------------------------------------------------------
#Kruskal Test if the results of the Shapiro test are not normally distributed (p<0.05)
    #Individual
kruskal.test(OTUs~AnimalCode, data=alphadiversity) 
kruskal.test(coverage~AnimalCode, data=alphadiversity) 
kruskal.test(chao~AnimalCode, data=alphadiversity) 
kruskal.test(ace~AnimalCode, data=alphadiversity) 

----------------------------------------------------------------------------------
#Pairwise Wilcoxon Rank Test (proceed if p-value less than 0.05 from Kruskal test, BH=Benjamini & Hochberg)
pairwise.wilcox.test(g=alphadiversity$AnimalCode, x=alphadiversity$OTUs, p.adjust.method = "BH")
pairwise.wilcox.test(g=alphadiversity$AnimalCode, x=alphadiversity$coverage, p.adjust.method = "BH")
pairwise.wilcox.test(g=alphadiversity$AnimalCode, x=alphadiversity$chao, p.adjust.method = "BH")
pairwise.wilcox.test(g=alphadiversity$AnimalCode, x=alphadiversity$ace, p.adjust.method = "BH")



