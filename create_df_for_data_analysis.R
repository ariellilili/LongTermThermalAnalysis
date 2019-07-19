library(dplyr)
library(ggplot2)


# set working directory
setwd(dir = "C:/Users/L/OneDrive/??????/PhD_long_term_compliance/matched bossa and samba database/calculated results")

# import datasets
b <-read.csv("bossa_score_by_location.csv", header = TRUE)
s <-read.csv("long-term indices.csv", header = TRUE)

df <- merge(x=b,y=s,all=TRUE)

write.csv(
    df,
    file = "bossa_samba_ready_for_analysis.csv",
    row.names = FALSE
)
