# check duplicates

art <- read.csv("data/ART_all.csv")

traits <- unique(art$trait_name)
View(data.frame(traits))


art$trait_name[art$trait_name %in% c("Calicular centres distance", 
                                     "Distance between the calices")] <- "Distance between centres of calices"

write.csv(art, "data/ART_all.csv", row.names=F)
