
library(here)
source(here::here("ReadSequences.R"))
source(here::here("NorwalkHelpers.R"))


NorwalkWrapper <- function() {
  ### Loads the metadata and filters based on a condition
  metadata = here::here("Phages") %>%
    .MostRecentOf(., "Genbank_Norwalk_linelist.+csv") %>%
      read.csv(.)
  completeGenomes = metadata %>% .[which(.$Nuc_Completeness == "complete"), ]
  
  here::here("Phages") %>%
    .MostRecentOf("Genbank_Norwalk.+fa") %>%
      PipelineNorwalk(., completeGenomes, "complete")
}
