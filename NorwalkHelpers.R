

##### Preloading #####

### Packages
.packages = c("here", "seqinr", "tictoc", "renv", "testthat", "stringr", "magrittr")

### Loads the package dependencies and installs them as necessary
.LoadPackages <- function(packages) {
  ### Install packages not yet installed
  installedPackages = packages %in% rownames(installed.packages())
  if (any(installedPackages == FALSE)) {
    install.packages(packages[!installedPackages])
  }
  
  ### Load packages
  invisible(lapply(packages, library, character.only = TRUE))
}
.LoadPackages(.packages)


##### Path Functions #####

### Path function for many fasta files
.PathsOut <- function(prefix) {
  paste0(here::here(), "/Phages/", prefix, "_fa_", .DateAndTime(), "/")
}

### Path function for aggregate fasta file
.PathsMergeOut <- function() {
  paste0(here::here(), "/Phages/", .DateAndTime() %>% substring(1, 8), "/")
}


### The opposite of the %in% operator
`%notin%` <- Negate(`%in%`)


##### Helper Functions #####

### Parses all files in the path that match a regex to return the most recent one
.MostRecentOf <- function(path, regex = "*") {
  filesInfo = path %>%
    list.files(., full.names = TRUE, pattern = regex, ignore.case = TRUE) %>%
    file.info()
  
  mostRecentFile = rownames(filesInfo)[which.max(filesInfo$mtime)]
  return(mostRecentFile)
}

