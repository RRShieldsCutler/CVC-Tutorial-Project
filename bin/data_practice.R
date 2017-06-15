library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(learnr)
library(reshape2)

hmp = read.csv('data/hmp_tongue_stool.csv', header=1, row.names = 1, check.names = F, as.is = T)
hmp = data.frame(t(hmp))
hmp = hmp %>% setNames(sub('k__', '', names(.))) %>%
  setNames(sub('.p__', ';', names(.))) %>%
  setNames(sub('.c__', ';', names(.))) %>%
  setNames(sub('.o__', ';', names(.))) %>%
  setNames(sub('.f__', ';', names(.))) %>%
  setNames(sub('.g__', ';', names(.))) %>%
  setNames(sub('.s__', ';', names(.))) %>%
  setNames(sub('.t__', ';', names(.)))

hmp[is.na(hmp)] = 0

depths = rowSums(hmp)
hist(depths,breaks=30)
otu.counts = colSums(hmp > 0)
hist(otu.counts, breaks=30)

# Remove rare taxa
hmp = hmp[,colMeans(hmp > 0) >= 0.1]
dim(hmp)
otu.counts = colSums(hmp > 0)
hist(otu.counts, breaks=30)

# Remove samples with low depth
depths = rowSums(hmp)
sort(depths)[1:10]
hmp = hmp[depths >= 100000,]
dim(hmp)

hmp = tibble::rownames_to_column(hmp, var = 'SampleID')
hmp.tall = hmp %>% gather(key='organism', value='abundance', 2:as.numeric(ncol(hmp)))
hmp.split = hmp.tall %>% separate(organism, c('kingdom','phylum','class','order','family','genus','species','strain'), sep = ';')

hmp.phylum = hmp.split %>% group_by(SampleID, phylum) %>%
  summarise(p_abundance = sum(abundance)) %>%
  spread(phylum, p_abundance)

hmp.class = hmp.split %>% group_by(SampleID, class) %>%
  summarise(p_abundance = sum(abundance)) %>%
  spread(class, p_abundance)

hmp.order = hmp.split %>% group_by(SampleID, order) %>%
  summarise(p_abundance = sum(abundance)) %>%
  spread(order, p_abundance)

hmp.family = hmp.split %>% group_by(SampleID, family) %>%
  summarise(p_abundance = sum(abundance)) %>%
  spread(family, p_abundance)

hmp.genus = hmp.split %>% group_by(SampleID, genus) %>%
  summarise(p_abundance = sum(abundance)) %>%
  spread(genus, p_abundance)

hmp.species = hmp.split %>% group_by(SampleID, species) %>%
  summarise(p_abundance = sum(abundance)) %>%
  spread(species, p_abundance)

hmp.strain = hmp.split %>% group_by(SampleID, strain) %>%
  summarise(p_abundance = sum(abundance)) %>%
  spread(strain, p_abundance)



