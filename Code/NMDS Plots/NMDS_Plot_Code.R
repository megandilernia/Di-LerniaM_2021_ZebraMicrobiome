## NMDS Plots ###

setwd("[insert working directory]")

library(tidyverse)
library(readxl)
library(RColorBrewer)

font-family: 'Times New Roman'

# NMDS Plot Based on Individual Zebras

nmds <- read_tsv(file="DAKZebra.trim.contigs.good.unique.good.filter.unique.precluster.pick.pick.fn.unique_list.0.03.pick.0.03.norm.thetayc.0.03.lt.nmds.axes")
metadata <- read_excel(path="Zebra_phyla.xlsx")
metadata_nmds <- inner_join(metadata, nmds, by=c('Sample'='group'))

#ggplot is variable, then variable, aes, aesthetics function
ggplot(metadata_nmds, aes(x=axis1, y=axis2, fill=AnimalCode)) +
  geom_point(shape=21, size=4) +
  stat_ellipse(type="norm",linetype=2)+
  #defines color scheme
  scale_fill_manual(name=NULL,
                    values=c("mediumpurple", "navy", "dodgerblue", "olivedrab2", "blue", "red", "yellow", "orange", "mediumvioletred"),
                    breaks=c("Z1","Z2","Z3","Z4", "Z5", "Z6", "Z7", "Z8", "Z9"),
                    labels=c("Z1","Z2","Z3","Z4", "Z5", "Z6", "Z7", "Z8", "Z9")) +
  coord_fixed() +
  labs(x="NMDS1",
       y="NMDS2") +
  theme_bw() + theme(text=element_text(family="Times New Roman", size=14))

ggsave("nmdsby_zebra_individual.pdf")

# NMDS Plot Based on Zebra Location

nmds <- read_tsv(file="DAKZebra.trim.contigs.good.unique.good.filter.unique.precluster.pick.pick.fn.unique_list.0.03.pick.0.03.norm.thetayc.0.03.lt.nmds.axes")
metadata <- read_excel(path="Zebra_phyla.xlsx")
metadata_nmds <- inner_join(metadata, nmds, by=c('Sample'='group'))

#ggplot is variable, then variable, aes, aesthetics function
ggplot(metadata_nmds, aes(x=axis1, y=axis2, fill=Location)) +
  geom_point(shape=21, size=4) +
  stat_ellipse(type="norm",linetype=2)+
  #defines color scheme
  scale_fill_manual(name=NULL,
                    values=c("yellowgreen", "darkgreen","cadetblue", "cadetblue1","cornflowerblue"),
                    breaks=c("WS","NS","ARUSHA","SUNSET", "LPEMBE"),
                    labels=c("Animal Kingdom 1","Animal Kingdom 2","Lodge 1","Lodge 2", "Lodge 3")) +
  coord_fixed() +
  labs(x="NMDS1",
       y="NMDS2") +
  theme_classic() + theme(text=element_text(family="Times New Roman", size=14))


ggsave("nmdsbyzebralocation.pdf")




