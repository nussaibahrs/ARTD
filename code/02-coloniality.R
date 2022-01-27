library(divDyn)
library(chronosphere)
library(ggplot2)
library(ggthemes)

# Theme for ggplot --------------------------------------------------------
theme_set(theme_hc())
theme_replace(axis.title.y=element_text(angle=90),
              axis.title = element_text(face="bold"))
pal <- c("#e44b49ff", "#74afb7ff")

# Load data ---------------------------------------------------------------
data(stages, package="divDyn")

load("data/complete.Rdata")

art <- read.csv(file.path("data", "ART_v1.csv")) # ART database v1
art <- art[art$trait_name=="Coloniality",]

ctd <- fetch("CoralTraitDB", datadir="data/chronos") # Coral Traits Database
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

# Plot --------------------------------------------------------------------
RT <- data.frame(stg=stages$stg,
                 mid=stages$mid,
                 solitary=df.raw[[1]]$divRT,
                 colonial=df.raw[[2]]$divRT)
RT <- RT[RT$stg > 50,]
RT_long <- reshape2::melt(RT, id.vars=c("stg","mid"), variable.name="coloniality", value.name = "diversity")


p2 <- ggplot(data=RT_long, 
             aes(x=mid, y=diversity, col=coloniality)
             ) +
  geom_vline(xintercept = 252, col="darkgrey", linetype="dashed") +
  geom_line(size=1) +
  #geom_point(size=1, fill="white", stroke=1) +
  scale_x_continuous(trans="reverse") +
  labs(x="Age (Ma)", y="Range-through genus richness", col="") +
  scale_colour_manual(values=c(pal[2],pal[1]), 
                      breaks = c("colonial", "solitary")) +
  theme(legend.position=c(0,1), 
        legend.justification=c(0,1))

svg("figs/Fig_05.svg", w=6, h=4)
deeptime::gggeo_scale(p2, height = unit(1, "lines"), size = 3)
dev.off()

RT <- na.omit(RT)

acf(RT$solitary)
acf(diff(RT$solitary))

acf(RT$colonial)
acf(diff(RT$colonial))

cor.test(RT$colonial, RT$solitary)
cor.test(diff(RT$colonial), diff(RT$solitary), method="spearman")

