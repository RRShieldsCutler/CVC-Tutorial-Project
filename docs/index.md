## CVC Tutorial Project
Data processing and visualization project from CVC 2017

A `shiny` document that loads the HMP taxa table, processes and filters OTUs, then calculates beta diversity for each collapsed level of taxonomy.

The interactive document then allows the user to change the rank and see what happens to the Bray-Curtis beta diversity plot.

Requires several packages from the `tidyverse` and RStudio.

I used a neat trick from the `tidyverse` packages to create each table

```R
# Melt to long form
hmp.tall = hmp %>% gather(key='organism', value='abundance', 2:as.numeric(ncol(hmp)))

# Split up the ranks of each
hmp.split = hmp.tall %>% separate(organism, c('kingdom','phylum','class','order','family','genus','species','strain'), sep = ';')

# Remake the OTU table with the rank summarized at phylum
hmp.phylum = hmp.split %>% group_by(SampleID, phylum) %>%
  summarise(p_abundance = sum(abundance)) %>%
  spread(phylum, p_abundance)
```
