# Building directory structure

# This script, when run, will:
# take the vector of project names on the first line of code
# if the project does not exist, it will create it,
# build a standardized directory structure, and create an R Project


# to build a new standardized project within the UTCoAssessors GitHub account,
# simply add the new name as an element to the following vector, save the file, and run all lines
# This will update the repo with new empty RProjects for each model type

# List of project names; add to this vector and re-run this script
project_list <- c("Equity")



# build contents of each .Rproj file
Rproj_contents <- c("Version: 1.0","","RestoreWorkspace: Default","SaveWorkspace: Default",
                    "AlwaysSaveHistory: Default","","EnableCodeIndexing: Yes",
                    "UseSpacesForTab: Yes","NumSpacesForTab: 2","Encoding: UTF-8",
                    "","RnwWeave: Sweave","LaTeX: pdfLaTeX")


Rmd_header <- c("---","title: \"Untitled\"","output: ",
                "  html_document:","    toc: true","    toc_depth: 3","    toc_float: true",
                "    collapsed: false","    smooth_scroll: true","date: \"`r Sys.Date()`\"",
                "---","",
                "```{r setup, include=FALSE}",
                "knitr::opts_chunk$set(echo = FALSE,warning = FALSE,message = FALSE)",
                "options(scipen=999)",
                "library(tidyverse)","library(easystats)","library(Valuation)","library(modelr)",
                "theme_set(theme_minimal())",
                "```",
                "","___","")



# If project doesn't exist already, add from list and fill in standard directory structure
for(i in project_list){
  if(!dir.exists(i)){
    dir.create(i) # create main directory for project
    dir.create(file.path(i,"Data")) # add standardized sub-directories
    dir.create(file.path(i,"R"))
    dir.create(file.path(i,"Output"))
    dir.create(file.path(i,"Reports"))
    file.create(file.path(i,paste0(i,".Rproj"))) # Make an Rproj file
    writeLines(Rproj_contents,file.path(i,paste0(i,".Rproj"))) # Add standard content to Rproj file
    file.create(file.path(i,"R/01_Import_and_Clean.R")) # create blank R scripts
    file.create(file.path(i,"R/02_Modeling.R"))
    file.create(file.path(i,"R/03_Valuation.R"))
    file.create(file.path(i,"Reports/01_Model_Assessment.Rmd")) # create blank rmd report
    writeLines(Rmd_header,file.path(i,"Reports/01_Model_Assessment.Rmd")) # add custom header
  }
}

