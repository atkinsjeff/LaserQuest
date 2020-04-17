####################################
#Calculate phylogenetic diversity for the tree community at baseplots from 10 NEON sites
#for Chris Gough's study on diversity - complexity
#Written by Elizabeth LaRue elarue@purdue.edu
#9/22/2019
####################################


library(devtools)
devtools::install_github("jinyizju/V.PhyloMaker")
require(V.PhyloMaker) #this creates the megaphylogeny (and is faster than the old S.PhyloMaker)
library(picante) #measures phylogenetic diversity

###read in plot level data of individual tree species for 10 NEON sites
rawdat <- read.csv("./master.Chrissites.csv")

###Compile a species list to generate a phylogeny
species.list <- as.data.frame(unique(rawdat$taxonid)) #unique species
colnames(species.list) <- "scientificName"

####################################
#compile a list of presence and absence of species for each plot as picante input
#a community matrix
#use loop to open each file and to pull out plot, species name, and native status, then going to calculate SR
plot.names <- unique(rawdat$plotid)
n.plots <- length(plot.names)
OUT <- NULL
  for (i in 1:n.plots) { #pull out plot i
    plot.i.name <- plot.names[i]
    plot.i <- rawdat[rawdat$plotid == plot.i.name,]
    tot <- as.data.frame(unique(plot.i$taxonid), ncol = 1)
    colnames(tot) <- "scientificName"

    #MATCH species in the plot TO TOTAL SPECIES LIST
    # list is where the values you want to search are
    phylo <- as.data.frame(species.list, ncol = 1)
    phylo$plot <- NA
    for(p in 1:nrow(phylo)){
      x <-match(phylo[p,1], tot$scientificName);
      ifelse(is.na(x) == TRUE, phylo[p,2] <- 0, phylo[p,2] <- 1)
    }

    OUT <- rbind(OUT, phylo[,2])

  }

OUT <- cbind.data.frame(plot.names, OUT)
sp.names <- species.list
colnames(OUT) <- c("sample", sp.names[2])
dim(OUT)
head(OUT)
#write.csv(OUT, file="ChrisCommunityMatrix.csv", row.names = FALSE)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Create a phylogeny and then extract 3 common but different phylogenetic diversity metrics
treesp <- read.csv("./phylogeneticlist.csv")
treesp <- treesp[,-1] #remove USDA codes

#make the phylogeny
treephylo <- phylo.maker(treesp, scenarios = "S3", nodes = nodes.info.1)
plot.phylo(treephylo$scenario.3, cex = .3) #can see your phylogeny
treephylo$scenario.3

#download community data of native and exotic plants species for neon plots ----------------------------------------------------
community <- read.csv("./ChrisCommunityMatrix.csv", stringsAsFactors = FALSE, header = TRUE)
rownames(community) <- community[,1]
community <- community[,-1]
community <- data.matrix(community, rownames.force = TRUE)

#---------------------------------------------------------------------------------------------
#Use Picante package functions to get phylogenetic species richness, variability, and clustering

#phylogenetic species richness
psr.result <- psr(community, treephylo$scenario.3)

#phylogenetic species variability
psv.result <- psv(community, treephylo$scenario.3)
#phylogenetic species clustering
psc.result <- psc(community, treephylo$scenario.3)

out <- cbind(rownames(community), psc.result[,1], psr.result[,1], psv.result[,1])
colnames(out) <- c("plotID", "PSC", "PSR", "PSV")
write.csv(out, file="phylogeneticdiversityforChris.csv", row.names = FALSE)

