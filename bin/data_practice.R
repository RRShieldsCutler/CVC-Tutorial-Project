library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(learnr)

hmp = read.csv('~/Box Sync/knights_box/cp_wgs_data/hmp_analysis/taxoncounts_hmp_tonguestool_refcomplete.csv', header=1, row.names = 1, check.names = F, as.is = T)
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
hmp = tibble::rownames_to_column(hmp, var = 'SampleID')
hmp.tall = hmp %>% gather(key='organism', value='abundance', 2:as.numeric(ncol(hmp)))
hmp.split = hmp.tall %>% separate(organism, c('kingdom','phylum','class','order','family','genus','species','strain'), sep = ';')

