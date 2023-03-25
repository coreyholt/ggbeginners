# Author: Corey C. Holt
# Date: 2023-03-24
# Purpose: Plot a clustered heatmap with annotation

# If required, install the pheatmap and RColorBrewer packages
install.packages(c("pheatmap", "RColorBrewer"))

# Load libraries
library(pheatmap)
library(RColorBrewer)

# Example sample data frame
test.table <- data.frame(Gene1 =c(1,2,1,2,1,0),
                     Gene2 =c(2,2,0,0,0,0),
                     Gene3 =c(1,0,1,-2,1,2),
                     Gene4 =c(2,0,0,0,-1,-1),
                     Gene5 =c(1,1,1,1,-1,1),
                     Gene6=c(1,-2,-1,0,2,1),
                     row.names=c("Sample1", "Sample2", "Sample3",
                     "Sample4", "Sample5", "Sample6"))

#Use read_table/read_csv("path_to_csv_file.csv") to input your data

# Create or load dataframes indicating annotation information for samples and genes
sample.info <-data.frame(SampleGroup=c("A", "A", "A",
                                       "B", "B", "B"),
                         row.names=c("Sample1", "Sample2", "Sample3",
                                     "Sample4", "Sample5", "Sample6"))

gene.info <-data.frame(FunctionalGroup=c("Apoptosis", "Apoptosis",
                                     "Cytoskeleton", "IonChannels", 
                                     "IonChannels", "Cytoskeleton"),
                       row.names = c("Gene1", "Gene2", "Gene3", 
                                     "Gene4", "Gene5", "Gene6"))

# Assign colours for each grouping
ph_colours<-list(FunctionalGroup=c(Apoptosis="grey90", 
                                   Cytoskeleton="grey75", 
                                   IonChannels="grey50"),
                 SampleGroup=c(A="lightpink", B="darkolivegreen2"))

# Plot heatmap
pheatmap(mat=test.table,
         color = brewer.pal(9,"PuOr"), # This is a divergent colour scheme
         cluster_cols =TRUE, # Cluster columns based on hierarchical clustering
         cluster_rows = FALSE, # Don't cluster rows
         cellwidth = 30, cellheight=30, 
         annotation_col = gene.info, # Add annotation for columns
         annotation_row = sample.info, # Add annotation for rows
         annotation_colors = ph_colours, # Assign colours to annotation
         annotation_name_row=T, # Add row names to annotation
         gaps_row = 3, # Add gaps between rows
         cutree_cols = 3) # Cut tree into 3 clades

# gaps_row and cutree_cols will depend on the number of cells in your heatmap and the number of groupings based on sample information (in this case SampleGroup) and hierarchical clustering (3 clades here)

# See https://www.rdocumentation.org/packages/pheatmap/versions/1.0.12/topics/pheatmap for an explanation of the parameters

# The RColorBrewer package was used to generate 9 colours from the "PuOr" palette. The package has a number of divergent colour schemes good for log foldchange etc. Alternatively, you could supply a vector of colours or hex codes e.g. color = c("darkblue", "blue", "white", "yellow", "red"). But that's a terrible palette. Don't use that. 

# This can sometimes improve branch ordering with large datasets
install.packages("dendsort")
library(dendsort)
callback = function(hc, ...){dendsort(hc)}

# then add 
clustering_callback  = callback 
# to the pheatmap() function

pheatmap(mat=test.table,
         color = brewer.pal(9,"PuOr"), 
         cluster_cols =TRUE, 
         cluster_rows = FALSE,
         cellwidth = 30, cellheight=30, #
         annotation_col = gene.info,
         annotation_row = sample.info,
         annotation_colors = ph_colours,
         annotation_name_row=T,
         gaps_row = 3, #
         cutree_cols = 3,
         clustering_callback  = callback)
