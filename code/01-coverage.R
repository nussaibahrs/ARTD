## ---------------------------
##
## Project: ART database
##
## Purpose of script: Calculate the proportion of taxa in ART compared to the PBDB
##
## Author: Danijela Dimitrijević & Nussaïbah B. Raja
## Copyright (c) N. Raja, 2021
## Email: nussaibah.raja.schoob@fau.de
##
## Date Created: 2021-12-21
## Last Modified:
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

### load libraries ---------------

library(divDyn)
library(chronosphere)
library(ggplot2)
library(ggthemes)
library(ggh4x)

# Theme for ggplot --------------------------------------------------------

theme_set(theme_hc())
theme_replace(axis.title.y=element_text(angle=90),
              axis.title = element_text(face="bold"))
pal <- c("#e44b49ff", "#74afb7ff")

### load data ----------------

pbdb <- chronosphere::fetch ("pbdb", ver="20220126", datadir="data/chronos") #Paleobiology database
dat <- subset(pbdb,order == "Scleractinia") # Scleractinian only

need <- c("occurrence_no", "collection_no", "identified_name", "accepted_name", 
          "accepted_rank", "early_interval", "late_interval", "max_ma", "min_ma", 
          "genus")
dat <- dat [, need] # make a new df with needed columns

art <- read.csv(file.path("data", "ART_v1.csv")) # ART database v1

ctd <- chronosphere::fetch("CoralTraitDB", datadir = "data/chronos") # Coral Traits Database
nrow(ctd)

tr_lst <- read.csv(file.path("data", "released_traits.csv"))

ctd <- ctd[ctd$trait_name %in% unique(tr_lst$trait_name),]


### cleaning ---------------

dat <- dat[dat$accepted_rank %in% c("genus", "species"), ]#only genus or species level
dat <- dat[dat$genus!="", ]

dat <- unique(dat) #omit duplicate data

### binning at stage level ---------------

data(stages, package="divDyn")
data(keys, package="divDyn")

# the 'stg' entries (lookup) 
stgMin <- divDyn::categorize(dat[ ,"early_interval"], keys$stgInt)
stgMax <- divDyn::categorize(dat[ ,"late_interval"], keys$stgInt)

# convert to numeric 

stgMin <- as.numeric(stgMin) 
stgMax <- as.numeric(stgMax)

# empty container 

dat$stg <- rep(NA, nrow(dat))

# select entries, where 

stgCondition <- c( 
  
  # the early and late interval fields indicate the same stg 
  
  which(stgMax==stgMin),
  
  # or the late_interval field is empty 
  
  which(stgMax==-1))

# in these entries, use the stg indicated by the early_interval 

dat$stg[stgCondition] <- stgMin[stgCondition]
dat <- dat[!is.na(dat$stg),]

#### Clean corals ----------------

# filter that to reef corals

traits <- chronosphere::fetch(dat="som", var="kiessling-coralgenera", datadir="data/chronos")

# assign the colonial/solitary status
dat$genus[dat$genus=="Montastraea"] <- "Montastrea"
dat$genus[dat$genus=="Turbinaria"] <- "Turbinaria Oken, 1815"

dat <- merge(dat, unique(traits[, c("genus_detail", "genus.proper", "GROWTH",
                                    "ECOLOGY", "MORPH")]), 
             by.x="genus", by.y="genus_detail", all.x=TRUE)

dat <- droplevels(dat)

save(dat, file="data/complete.Rdata")

### Genus ###

load("data/complete.Rdata")

# counts of all pbdb genera in each stage

ngen <- table(dat$genus, dat$stg)
ngen[ngen>0] <- 1
all_genera <- colSums(ngen)
all_genera <- data.frame(all_genera)


colnames(all_genera) <- c("all")

all_genera$stg <- rownames(all_genera)

### Proportion of all traits - genus

### load all traits from ART

all_trait <- art
all_trait <- merge(dat, all_trait, by.x = "genus", by.y = "genus_name")
all_trait <- merge(all_trait, stages)

# How many trait genera in each stage

ngen <- table(all_trait$genus, all_trait$stg)
ngen[ngen>0] <- 1
trait_genera <- colSums(ngen) #number of genera per stage

trait_genera <- data.frame(trait_genera) #turn into data frame so we can match the data
colnames(trait_genera) <- "trait_only"
trait_genera$stg <- rownames(trait_genera)

# Proportion of trait genera relative to pbdb genera

prop <- merge(trait_genera, all_genera)
prop$prop <- prop$trait_only/prop$all

plot(prop$stg, prop$prop)

prop0 <- merge(prop, stages)

# Number of trait values over time

no_art <- setNames(data.frame(table(all_trait$stg)),
         c("stg", "value"))
no_art$cat <- "ARTD"

# add CTD
tax <- strsplit(ctd$specie_name, " ")
ctd$genus_name <- unlist(lapply(tax, function (x) x[1]))
ctd$species_name <- unlist(lapply(tax, function (x) x[2]))

all_trait <- rbind(art[,c("genus_name", "species_name")], 
                   ctd[,c("genus_name", "species_name")])
all_trait <- merge(dat, all_trait, by.x = "genus", by.y = "genus_name")
all_trait <- merge(all_trait, stages)

# How many trait genera in each stage

ngen <- table(all_trait$genus, all_trait$stg)
ngen[ngen>0] <- 1
trait_genera <- colSums(ngen) #number of genera per stage

trait_genera <- data.frame(trait_genera) #turn into data frame so we can match the data
colnames(trait_genera) <- "trait_only"
trait_genera$stg <- rownames(trait_genera)

# Number of traits over time
no_ctd <- setNames(data.frame(table(all_trait$stg)),
                   c("stg", "value"))
no_ctd$cat <- "ARTD + CTD"

# Proportion of trait genera relative to pbdb genera ----

prop <- merge(trait_genera, all_genera)
prop$prop <- prop$trait_only/prop$all

prop1 <- merge(prop, stages)

prop0$cat <- "ARTD"
prop1$cat <- "ARTD + CTD"

prop <- rbind(prop0, prop1)

p0 <- ggplot(prop[prop$stg>50,], aes(x=mid, y=prop*100, col=cat)) +
  geom_vline(xintercept = 252, col="darkgrey", linetype="dashed") +
  geom_line(size=1) +
  geom_point(size=2, shape=21, fill="white", stroke=1) +
  scale_x_continuous(trans="reverse") +
  labs(x="Age (Ma)", y="% of genera in the PBDB", col="Data from") +
  scale_colour_manual(values=c(pal[2],pal[1]), 
                      breaks = c("ARTD + CTD", "ARTD"))

svg("figs/Fig_02.svg", w=7, h=5)
deeptime::gggeo_scale(p0, height = unit(1, "lines"), size = 3)
dev.off()

# Number of traits over time ----
no_trait <- rbind(no_art, no_ctd)
no_trait <- merge(no_trait, stages)

ggplot(no_trait, aes(x=mid, y=value, col=cat)) +
  geom_vline(xintercept = 252, col="darkgrey", linetype="dashed") +
  geom_line(size=1) +
  geom_point(size=2, shape=21, fill="white", stroke=1) +
  scale_x_continuous(trans="reverse") +
  labs(x="Age (Ma)", y="Number of traits", col="Data from") +
  scale_colour_manual(values=c(pal[2],pal[1]), 
                      breaks = c("ART + CTD", "ART"))

p1<- ggplot(no_trait[no_trait$cat=="ART",], aes(x=mid, y=value)) +
  geom_vline(xintercept = 252, col="darkgrey", linetype="dashed") +
  geom_line(size=1, col=pal[1]) +
  geom_point(size=2, shape=21, fill="white", stroke=1, col=pal[1]) +
  scale_x_continuous(trans="reverse") +
  labs(x="Age (Ma)", y="Number of traits")

svg("figs/Fig_02b.svg", w=7, h=5)
deeptime::gggeo_scale(p1, height = unit(1, "lines"), size = 3)
dev.off()

# Completeness of data ----------------------------------------------------
art <- read.csv(file.path("data", "ART_v1.csv")) # ART database v1

spec_tot <- length(unique(art$observation_id))

df <- setNames(
data.frame(
  sort(
    table(art$trait_name)/spec_tot
    )
  ), c("trait_name", "prop")
)

df <- merge(df, tr_lst)

ggplot(df, aes(y=trait_name, x=prop*100)) +
  geom_col(fill=pal[1], width=0.8) +
  labs(x="% completeness", y="") +
  facet_grid(vars(type), scales="free_y") +
  force_panelsizes(rows = c(1, 8, 6),
                   TRUE)

ggsave("figs/Fig_03.svg", w=8, h=5)
  