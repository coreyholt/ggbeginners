# ggplots for ggbeginners

This workshop will show you how to make publication-ready plots with little to no prior experience in R. You will learn:
- How to recreate and optimise BlobTools plots
- How to easily visualise BUSCO data
- How to draw phylogenetic trees with accompanying variables
<br/><br/>
> The workshop is scheduled for **1.00pm on Friday 24th March in room 224 (BRC)**. The room is booked until 4.00pm but feel free to leave at any point. 
---
## Before we start ...
Please make sure R and Rstudio is downloaded on your computer.\
https://posit.co/download/rstudio-desktop/ 
<br/><br/>
Open Rstudio and install the packages we will be using will the following code
```
install.packages(c("tidyverse", "ape", "ggnewscale", "gginnards", "patchwork", "ggridges", "BiocManager"))
BiocManager::install("ggtree")
BiocManager::install("ggtreeExtra")
```
install.packages() is a base R function that can install any packages on the CRAN repository. CRAN = The Comprehensive R Archive Network and contains a huge number of stable packages.\
BiocManager is a package used to access the Bioconductor repository – similar to CRAN but more focussed on genomic analysis.
<br/><br/>
You can check a package has successfully installed by loading it with the library() function
```
library(tidyverse)
library(ggtree)
library(ape)
library(ggtreeExtra)
library(ggnewscale)
library(gginnards)
```
---
That's it for now. **But PLEASE ensure all packages are installed before the workshop.** [Email me](mailto:corey.holt@ubc.ca?subject=HELP%20ME,%20COREY) or come and see me in room 309 if you have any issues. 

