released <- read.csv(file.path("data", "released_traits.csv"))
ART <- read.csv(file.path("data", "ART_all.csv"))
nrow(ART)

ART$delete <- NULL

ART <- ART[ART$trait_name %in% released$trait_name,]
head(ART)
nrow(ART)

write.csv(ART, file.path("data", "ART_v1.csv"), row.names=F)
