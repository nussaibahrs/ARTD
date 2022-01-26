library(divDyn)
library(chronosphere)
# Load data ---------------------------------------------------------------
data(stages)
load("data/complete.Rdata")
art <- read.csv(file.path("data", "ART_v1.csv")) # ART database v1
art <- art[art$trait_name=="Coloniality",]

ctd <- fetch("CoralTraitDB") # Coral Traits Database
tax <- strsplit(ctd$specie_name, " ")
ctd$genus_name <- unlist(lapply(tax, function (x) x[1]))
ctd$species_name <- unlist(lapply(tax, function (x) x[2]))
ctd <- ctd[ctd$trait_name=="Coloniality",]

cols <- c("genus_name", "value")

coloniality <- rbind(art[,cols], ctd[,cols])
coloniality  <- coloniality [coloniality $value %in% c("solitary", "colonial"),]

coloniality <- unique(coloniality)

df <- merge(dat, coloniality, by.x="genus.proper", by.y="genus_name", all.x=T)
df <- df[!is.na(df$value),]

# DivDyn ------------------------------------------------------------------
table(df$value)

cats <- c("solitary", "colonial")

df.raw <- lapply(cats, 
                 function(x) divDyn (df[df$value == x,], tax="genus.proper", bin="stg")
)

it=500
qu=0.5

df.sub <- lapply(cats, 
       function(x) subsample(df[df$value == x,], tax="genus.proper", bin="stg",
                             q=qu, it=it, type="sqs"))

# Plot --------------------------------------------------------------------
png("figs/coloniality.png", w=7, h=5, res=300, units = "in")

tsplot(stages, boxes="sys", shading="series", boxes.col="systemCol", 
       xlim=52:95, ylim=c(0,200), ylab="Range through genus richness") 

lines(stages$mid, df.raw[[1]]$divRT, lwd=2, col="darkred")
lines(stages$mid, df.raw[[2]]$divRT, lwd=2, col="darkorange")

legend("topleft", legend=cats, 
       lwd=2, col=c("darkred", "darkorange"),
       cex=0.8, 
       bg="white")

dev.off()

file.show("figs/coloniality.png")
