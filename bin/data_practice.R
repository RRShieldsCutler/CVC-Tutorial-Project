library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(learnr)
library(reshape2)

hmp = read.csv('data/hmp_tongue_stool.csv', header=1, row.names=1, check.names = F, as.is = T)
map = read.delim('data/hmp_tonguestool_map.txt', sep = '\t', header=1)
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

# Function that takes the matrix, does beta div, and plots/colors by body site
plot_beta_div = function(x) {
  plot.df = x
  rownames(plot.df) = NULL
  plot_df = tibble::column_to_rownames(plot_df, var = 'SampleID')
  beta = as.matrix(vegdist(plot_df, method='bray', na.rm = F))
  pcoa = cmdscale(beta, k=2)
  pcoa = as_data_frame(pcoa)
  pcnames = c()
  for(i in 1:ncol(pcoa)){
    pcnames[i] <- paste("PC",i, sep="")
  }
  colnames(pcoa) = pcnames
  pcoa$SampleID = rownames(plot_df)
  pcoa = pcoa %>% left_join(map)
  
  ggplot(pcoa, aes(x=PC1, y=PC2, color=body_site)) +
    geom_point(size = 2) +
    theme_bw() + labs(x='PC1', y='PC2') +
    theme(panel.background = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10), 
          axis.ticks = element_line(),
          legend.title = element_blank(),
          legend.key.size = unit(0.2, "in"),
          legend.text = element_text(size=9),
          legend.position = 'right',
          axis.text = element_text(size=8),
          axis.title = element_text(size=9))
}

# row.names(hmp.genus) = NULL
# hmp.genus = tibble::column_to_rownames(hmp.genus, var = 'SampleID')
# beta = as.matrix(vegdist(hmp.genus, method='bray', na.rm = F))
# pcoa = cmdscale(beta, k=2)
# pcoa = as_data_frame(pcoa)
# pcnames = c()
# for(i in 1:ncol(pcoa)){
#   pcnames[i] <- paste("PC",i, sep="")
# }
# colnames(pcoa) = pcnames
# pcoa$SampleID = rownames(hmp.genus)
# pcoa = pcoa %>% left_join(map)

p = ggplot(pcoa, aes(x=PC1, y=PC2, color=body_site)) +
  geom_point(size = 2) +
  theme_bw() + labs(x='PC1', y='PC2') +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10),
        axis.ticks = element_line(),
        legend.title = element_blank(),
        legend.key.size = unit(0.2, "in"),
        legend.text = element_text(size=9),
        legend.position = 'right',
        axis.text = element_text(size=8),
        axis.title = element_text(size=9)) +
 coord_fixed()
p



