library(refer) #https://github.com/adamkocsis/refer
library(bib2df)

meta <- refer::create_metadata("output")

refer::report(meta, draft=T, data_refs = "refs.bib")

refs <- read.csv("report/references.csv")
refs <- refs[,-c(1:3)]
df2bib(refs, "report/refs.bib")
