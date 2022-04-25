library(sp)
library(ggplot2)
library(dplyr)
library(rgdal)
library(rgeos)

# Load data ---------------------------------------------------------------
art <- read.csv("data/ART_v1.csv")
refs <- read.csv("data/references.csv")
art <- merge(art, refs[,c("reference_no", "pbdb_reference_no")])

pbdb <- chronosphere::fetch("pbdb")

pbdb <- pbdb[pbdb$reference_no %in% unique(art$pbdb_reference_no),]
pbdb <- pbdb[pbdb$order=="Scleractinia",]

# Coordinates and time interval -------------------------------------------
pbdb$mid <- (pbdb$max_ma + pbdb$min_ma)/2

missing <- art[is.na(art$pbdb_reference_no),]

# fix, edit in database
cols <- c("early_interval", "late_interval", "min", "max")

missing$mid <- (missing$min + missing$max)/2

coords <- rbind(unique(pbdb[,c("lng", "lat", "mid")]),
                setNames(unique(missing[,c("longitude", "latitude", "mid")]),
                         c("lng", "lat", "mid"))
                )

data(stages, package="divDyn")

coords$system <- NA

for(i in 1:nrow(stages)){
  n <- which(coords$mid <= stages$bottom[i] & 
               coords$mid > stages$top[i])
  
  if(length(n) > 0) {
    coords$system[n] <- stages$system[i]
    cat("\r", i, "\n")
    }
  
}

table(coords$system)

sysdf <- stages %>% filter(stg > 51) %>% 
  group_by(system, systemCol) %>% 
  summarise(bottom=max(bottom),
            top=min(top)) %>% 
  arrange(desc(bottom))
sysdf$labs <- paste0(sysdf$system, " (", sysdf$bottom, "–", sysdf$top, " Ma)")

coords$system <- factor(coords$system, levels=sysdf$system)

# Plot --------------------------------------------------------------------
world <- readOGR("data/ne_50m_admin_0_countries.geojson")

outline <- bbox(world)
outline <- data.frame(xmin=outline["x","min"],
                      xmax=outline["x","max"],
                      ymin=outline["y","min"],
                      ymax=outline["y","max"])

world <- fortify(world)

points <- data.frame(lon=c(-98.35, 134.21), lat=c(39.5, -25.36))

gg <- ggplot()+ 
  geom_map(data=world, map=world,
                    aes(x=long, y=lat, map_id=id), 
                    fill="gray90", color="white", size=0.3) + 
  geom_rect(data=outline, 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color=1, fill=NA, size=0.3) +
  labs(x=NULL, y=NULL) +
  coord_map("mollweide")+ 
  theme_bw() + 
  theme(panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        legend.title=element_text(face="bold")
        )

gg +
  geom_point(data=coords, aes(x=lng, y=lat, col=system), size=1.8,
             alpha=0.8) +
  scale_color_manual(values=sysdf$systemCol, 
                     labels=sysdf$labs) +
  labs(colour="Geological Period")

ggsave("figs/Fig_02.svg", w=10, h=6)
  


