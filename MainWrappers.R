
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

### Finds the most recently compiled genome from the pipeline folder
MostRecentMerge <- function() {
  here::here("Phages") %>%
    .MostRecentOf(., "^\\d{8}$") %>%
      .MostRecentOf("*upload.*.fa")
}

### Runs MAFFT to do MSA which will align the sequences for future analysis
### MAFFT is available at: https://mafft.cbrc.jp/alignment/software/
### MAFFT © 2013 Kazutaka Katoh
AlignSequences <- function(multifasta = MostRecentMerge(),
                           referenceFile = paste0(here::here("Phages"), "/Norwalk_refseq.fasta")) {
  ### Expands the path so that bash does not confuse the path
  path.expand(multifasta) %>%
    c(., gsub("\\.fa$", "_aligned.fa", .)) %>%
      { paste("mafft --anysymbol --keeplength --thread 4 --6merpair --addfragments",
              .[1], path.expand(referenceFile), ">", .[2]) }
}
