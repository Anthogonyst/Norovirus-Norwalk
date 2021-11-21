
library(here)
# TODO: Migrate appropriate functions from here
#source(here::here("WorkplaceGISAID/MergeSequences.R"))
source(here::here("NorwalkHelpers.R"))


### The ETL sequence pipeline to extract from the aggregate
PipelineNorwalk <- function(path, filesArray = NA, prefix = "generic",
                     ExtraFUN = .Empty, pipeOutFastas = FALSE) {
  tictoc::tic("Pipeline finished!", quiet = FALSE)
  .DateAndTime(TRUE)
  
  ### Parse anything to files
  files = ParseFlexibleFileFolder(path)
  
  ### Write destinations
  outputFolder = .PathsOut(prefix)
  mergeOutputFolder = .PathsMergeOut()
  
  ### The pipeline returns a list of fasta files
  theFastaFiles = LoadFiles(files) %>%
    FilterFor(., NewestAccessions(names(.))) %>%
      .[ReduceAccession(names(.)) %in% filesArray] %>%
        ExtraFUN(.) %>% 
          AddFastaAttributes(.)
  
  ### Writes the files onto the drive
  WriteSanitizedFasta(theFastaFiles, outputFolder)
  WriteOriginalFasta(theFastaFiles, sub("/$", "_unmodified/", outputFolder))
  WriteMergedFasta(theFastaFiles, mergeOutputFolder, prefix)
  tictoc::toc()
  
  ### Returns the filepaths of the resulting folders unless fastas are desired
  if (pipeOutFastas)
    return(theFastaFiles)
  else return(c(outputFolder, mergeOutputFolder))
}

### Removes the last digit in accession number to evaluate resequencing attempts
ReduceAccession <- function(x) {
  sub("\\.\\d+\\s*(\\|.*)?$", "", x)
}

### Filters out resequenced attempts and only returns the newest runs (chr array)
NewestAccessions <- function(x) {
  ### Base accession names and their duplicates
  nums = ReduceAccession(x)
  dupeVals = ReduceAccession(x) %>% duplicated(.) %>% x[.] %>% ReduceAccession(.)
  
  ### Maximizes the value to get the most recent of the dupes
  resolvedConflicts = sapply(dupeVals, function(y) {
    theDupes = which(nums == y)
    which(x == max(x[theDupes]))
  })
  
  ### Combines non-dupes and deduped and returns the array
  noConflicts = which(!nums %in% dupeVals)
  c(noConflicts, resolvedConflicts) %>%
    unlist(., FALSE) %>%
      x[.]
}

### Gets the Annot attribute in a vectorized method
GetAnnotations <- function(fa) {
  sapply(fa, function(x) { attributes(x)$Annot }, USE.NAMES = FALSE)
}

### Writes the fasta like SanitizedFasta() but without any modifications to its seqname
WriteOriginalFasta <- function(fastaFiles, outputPath, forceWritePath = TRUE) {
  .CheckOutputPath(outputPath, forcePath = forceWritePath)
  
  ### Intended for external files, it will simply name the file based on the fasta name
  len = mapply(fastaFiles, GetAnnotations(fastaFiles), FUN = function(x, y) {
    seqinr::write.fasta(
      sequences = x,
      names = y %>% sub(">", "", .),
      file.out = attributes(x)$name %>% .SafeFilename(.) %>% paste0(outputPath, "/", .),
      open = "w"
    )
  })
  
  ### The return code for a successful I/O operation is 0 on most computers
  paste("Done writing", length(len %>% .[. == 0]), "files!") %>%
    print(.)
  .LogEvent(attributes(fastaFiles)$filename, "finished_written_tagged_filenames")
  .LogEvent(attributes(fastaFiles)$sample, "finished_written_tagged_sampleIds")
  
  return(outputPath)
}
