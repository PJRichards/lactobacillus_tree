## draw tree with metadata

# packages
library(ggtree)     # version 2.0.1
library(tidyverse)  # version 1.3.0
library(ape)        # version 5.3

tree <- read.tree("P_2020_02_25_193540780909/parsnp.tree") 

tree_names <- 
  as_tibble(tree) %>% 
   mutate(label = if_else(str_detect(label, "GCA"), 
                            str_sub(label, 17, str_length(label)-14), label),
          label = if_else(str_detect(label, "DC22"), 
                            str_sub(label, 1, 6), label))  %>% 
   as.phylo()

meta <- read_tsv("data/genomes/meta_all.txt", col_names = TRUE)
                   
meta_concise <- 
  meta %>%  
      mutate(label = if_else(str_detect(label, "GCA"),                                          str_sub(label, 17, str_length(label)-14), label),
             label = if_else(str_detect(label, "DC22"),                                          str_sub(label, 1, 6), label),
             host2 = 
                case_when(str_detect(host, "chicken") ~ "poultry",
                          str_detect(host, "wturkey") ~ "poultry",
                          str_detect(host, "human") ~ "human",
                          str_detect(host, "pig") ~ "pig",
                          str_detect(host, "rat") ~ "rat",
                          str_detect(host, "mouse") ~ "mouse",
                          str_detect(host, "whitefoot") ~ "mouse",
                          str_detect(host, "morus") ~ "other",
                          str_detect(host, "bee") ~ "other",
                          str_detect(host, "dairey") ~ "biotech",
                          str_detect(host, "probiotic") ~ "biotech")) %>% 
      select(-host) %>% column_to_rownames(var = "label")
    



tree.p <- ggtree(tree_names) + 
              geom_tiplab() + 
              geom_treescale(x=0.3, y=1)

hm.p <- gheatmap(tree.p, meta_concise, 
                 width = 0.5, hjust = 0, 
                 legend_title = "host", colnames = FALSE) 
  


png(filename = "tree_all.png", width = 200, height = 200, 
                                      units = "mm", res = 300)

hm.p

dev.off()