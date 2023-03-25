# Author: Corey C. Holt
# Date: 2023-03-24
# Purpose: Plot blobDB output

# Load libraries
library(tidyverse)
library(patchwork)
library(ggridges)

# Read in blobDB output
blob <- read_table("~/Bioinformatics/scTranscriptomes/blobs/taxonomy.protoBlob1.blobDB.bestsum.table2.txt")

# Check column names
names(blob)

# Plot blobDB output by GC content and coverage (log10) with size of points proportional to length of contig and color by phylum (9% cutoff)
ggplot(blob, aes(GC, log10(cov0)))+ 
  geom_point(aes(size=length,
                 colour=`phylum.t.9%s`))


# Filter out contigs with no-hit
blob.f <- blob %>% filter(`phylum.t.9%s` != "no-hit")

# Edit plot further
ggplot(blob.f, aes(GC, log10(cov0)))+
  geom_point(aes(size=length,
                 fill=`phylum.t.9%s`),
             color="black",
             pch=21)+
  scale_x_continuous(limits=c(0,1))+ # Set x-axis limits
  scale_size(range = c(0, 4))+ # Set size of points
  theme(panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.grid.major = element_line(color="grey80", 
                                        size=0.1), # Set major grid lines to grey80 and size 0.1
        panel.background = element_rect(color="black"), # Set panel background to black
        strip.background = element_rect(color="black")) # Set strip background to black

# Count number of contigs per phylum
table(blob$`phylum.t.9%s`)

# Use sequential layers to plot contigs with "Eukaryota-undef" on top of contigs with other phyla
ggplot()+
  geom_point(data = blob.f %>% filter(`phylum.t.9%s` != "Eukaryota-undef"), # Filter out contigs with "Eukaryota-undef"
             aes(GC, log10(cov0),
                 size=length,
                 fill=`phylum.t.9%s`),
             color="black",
             pch=21)+ # Set point color to black and shape to 21
  geom_point(data = blob.f %>% filter(`phylum.t.9%s` == "Eukaryota-undef"), # Filter out contigs that are not "Eukaryota-undef"
             aes(GC, log10(cov0),
                 size=length,
                 fill=`phylum.t.9%s`),
             color="black",
             pch=21)+
  scale_x_continuous(limits=c(0,1))+ 
  #scale_size(range = c(0, 4))+ 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey80", 
                                        size=0.1),
        panel.background = element_rect(color="black"),
        strip.background = element_rect(color="black"))

# Use facet_wrap to plot contigs by phylum
ggplot(blob.f, aes(GC, log10(cov0)))+
  geom_point(aes(size=length,
                 fill=`phylum.t.9%s`),
             color="black",
             pch=21)+
  scale_x_continuous(limits=c(0,1),
                     labels =c("0", "0.25",
                               "0.50", "0.75", "1"))+
  facet_wrap("`phylum.t.9%s`")+
  #scale_size(range = c(0, 4))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey80", 
                                        size=0.1),
        panel.background = element_rect(color="black"),
        strip.background = element_rect(color="black"))


# Use facet_wrap to plot contigs by phylum and set number of rows to 1
ggplot(blob.f %>% filter(`phylum.t.9%s` %in% c("Actinobacteria", "Bacteria-undef","Firmicutes", "Proteobacteria")), 
       aes(GC, log10(cov0)))+
  geom_point(aes(size=length,
                 fill=`phylum.t.9%s`),
             color="black",
             pch=21)+
  facet_wrap("`phylum.t.9%s`", nrow=1)+
  scale_x_continuous(limits=c(0,1))+
  #scale_size(range = c(0, 4))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey80", 
                                        size=0.1),
        panel.background = element_rect(color="black"),
        strip.background = element_rect(color="black"),
        plot.margin = margin(t=4,r=1,b=4,l=1,unit='cm')) # Set margins

# Save plot as variable for use with patchwork
bl.p1 <- ggplot(blob.f, aes(GC, log10(cov0)))+
  geom_point(aes(size=length,
                 fill=`phylum.t.9%s`),
             color="black",
             pch=21)+
  scale_x_continuous(limits=c(0,1))+
  #scale_size(range = c(0, 4))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey80", 
                                        size=0.1),
        panel.background = element_rect(color="black"),
        strip.background = element_rect(color="black"))

# Plot histogram of GC content by phylum
bl.p2 <- ggplot(blob.f, aes(GC, weight=length))+
  geom_histogram(aes(color=`phylum.t.9%s`),
                 fill=NA,
                 bins=40)+
  scale_x_continuous(limits=c(0,1))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey80", 
                                        size=0.1),
        panel.background = element_rect(color="black"),
        strip.background = element_rect(color="black"),
        axis.text.x = element_blank(), # Remove x axis labels
        axis.ticks.x = element_blank(), # Remove x axis ticks
        axis.title.x = element_blank()) # Remove x axis title

# Use patchwork to combine plots into one figure with bl.p1 on top and bl.p2 on bottom
bl.p2/bl.p1 + plot_layout(heights=c(1,3))

# Plot density ridge plot of GC content by phylum for better visualization
bl.p3 <- ggplot(blob.f, aes(x=GC, y=`phylum.t.9%s`, weight=length))+
  geom_density_ridges(stat = "binline", # Use binline stat to plot density ridge plot
                      bins = 40, # Set number of bins
                      scale = 0.95, 
                      draw_baseline = FALSE,
                      aes(fill=`phylum.t.9%s`))+
  scale_x_continuous(limits=c(0,1))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey80", 
                                        size=0.1),
        panel.background = element_rect(color="black"),
        strip.background = element_rect(color="black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none") # Remove legend

# Use patchwork to combine plots into one figure with bl.p1 on top and bl.p3 on bottom
bl.p3/bl.p1 + plot_layout(heights=c(1,1),
                          guides = 'collect')
