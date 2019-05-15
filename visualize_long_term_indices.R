library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)

# set working directory
setwd(dir = "C:/Users/L/OneDrive/??????/PhD_long_term_compliance/matched bossa and samba database")

# import
indices <- read.csv("2017 long-term indices.csv")

# for each index, compare across locations
a <- split(indices, f=indices$index)
i=1
for (df in a) {
    pic <- ggplot(df, aes(x=location,y=value)) + geom_col()+
        theme_bw() +
        ggtitle(as.character(unique(df$index)))
    assign(paste("p",i,sep = ""),pic)
    i=i+1
}

comb1 <- ggarrange(p1,p2,p3,p4,p5,p6,p7, ncol = 1, nrow = 7)
comb2 <- ggarrange(p8,p9,p10, ncol = 1, nrow = 3)
comb3 <- ggarrange(p12,p13,p14,p15,p16,p17, ncol=1,nrow = 6)
comb4 <- ggarrange(p11,p18,p19,p20,p21,p22,p23,p24, ncol=1,nrow = 8)

ggsave(plot=comb1, 
       filename = "long-term indices comparison across buildings 1.jpeg",
       width = 15, height = 21)
ggsave(plot=comb2, 
       filename = "long-term indices comparison across buildings 2.jpeg",
       width = 15, height = 10)
ggsave(plot=comb3, 
       filename = "long-term indices comparison across buildings 3.jpeg",
       width = 15, height = 20)
ggsave(plot=comb4, 
       filename = "long-term indices comparison across buildings 4.jpeg",
       width = 15, height = 25)