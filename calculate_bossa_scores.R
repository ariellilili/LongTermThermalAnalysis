library(dplyr)
library(lubridate)
library(ggplot2)
library(fmsb)
library(plotly)
library(ggpubr)
library(tidyr)

# # check available color palettes
# library(RColorBrewer)
# display.brewer.all()


# set working directory
setwd(dir = "C:/Users/L/OneDrive/??????/PhD_long_term_compliance/matched bossa and samba database")

#### prepare a dataframe from the original bossa survey ####

# get the list of file paths
filenames <- list.files(pattern = "BOSSA", full.names = TRUE)

# initial a dataframe to store results
results <- data.frame(floor = character(),
                      temp.year.mean.score = double(),
                      temp.year.sat.percent = double(),
                      num.survey = numeric(),
                      building = character(),
                      bossa.time = character())

# for each file, plot and calculate floor level mean score
for (f in filenames) {
    # read the file
    survey <- read.csv(f, header = TRUE)
    # keep columns of interest
    survey <-
        survey[, c(1, 2, 3, 7, 22, 23, 26, 27, 28, 29, 38, 39, 41, 48)]
    # rename columns
    col_headers <-
        c(
            "time",
            "age",
            "gender",
            "floor",
            "air.movement",
            "humidity",
            "temp.winter",
            "uncomf.reason.winter",
            "temp.summer",
            "uncomf.reason.summer",
            "temp.control",
            "air.movement.control",
            "adapt.freedom",
            "overall.comfort"
        )
    names(survey) <- col_headers
    
    # get yearly performance for each floor
    
    # take the average of summer and winter scores as the yearly score
    survey$temp.year.score <- (survey$temp.summer + survey$temp.winter)/2
    
    # score >= 4 means satisfaction, score < 4 means dissatisfaction
    survey$temp.summer <-
        with(survey, ifelse(temp.summer >= 4, 1, 0)) # 1=satisfaction
    survey$temp.winter <-
        with(survey, ifelse(temp.winter >= 4, 1,0))
    
    # only if the occupant is satisfied in both summer and winter
    # we consider he/she is satisfied over a year
    survey$temp.year.sat <-
        with(survey, ifelse(temp.summer == 1 & temp.winter == 1, 1, 0))
    
    # summarize by floor the yearly performance
    b <- survey[,c("floor","temp.year.score","temp.year.sat")] %>%
        group_by(floor) %>%
        summarise(temp.year.mean.score = mean(temp.year.score),
                  temp.year.sat.percent = sum(temp.year.sat)/n(),
                  num.survey = n())
    
    # add building info and bossa.time info
    a <- unlist(strsplit(f, "_"))
    b$floor = paste("Level",b$floor)
    b$building = a[2]
    b$bossa.time = substring(a[3],1,6)
    
    # store results
    results <- full_join(results, b)
}

# keep the records that had matched samba
key = read.csv("bossa_samba_match_key.csv")
results_clean <- merge(x = key, y = results, all.x = TRUE)

# save the results
write.csv(results_clean,file = "./calculated results/bossa_score_by_location.csv",row.names = FALSE)



# # other visualization

# # time plot
# min.time <- min(survey$time)
# max.time <- max(survey$time)
# p1 <- ggplot(survey) +
#     geom_histogram(aes(time)) +
#     scale_x_datetime(date_labels = "%F",
#                      breaks = c(min.time, max.time)) +
#     labs(
#         title = paste("Time of survey, ", build.name),
#         x = "",
#         y = "Count of Responses"
#     ) +
#     theme_minimal()
# ggsave(filename = paste("./calculated results/",build.name,"_survey_time.jpeg",sep=""),
#        plot = p1)


# # bar chart for mean scores
# # prepare dataframe for visualization
# a <- survey[, -c(1:3, 8, 10)] %>%
#     group_by(floor) %>%
#     summarise_all(mean) %>%
#     gather(key = "satisfaction", value = "mean.score",-floor)
# # factorize floor names
# a$floor <- as.factor(a$floor)
# # plot
# p2 <- ggplot(a) +
#     geom_col(aes(x = satisfaction, y = mean.score, fill = floor), position = "dodge") +
#     scale_fill_brewer(palette = "Set3") +
#     labs(
#         title = paste("Mean satisfaction scores\n ", build.name),
#         y = "Score\n1 = very dissatisfied, 4 = neutral, 7 = very satisfied",
#         x = "",
#         fill = "Floor"
#     ) +
#     theme_minimal() +
#     coord_flip()
# ggsave(filename = paste("./calculated results/",build.name,"_mean_scores.jpeg",sep = ""),
#        plot = p2)


# # radar chart of mean scores
# # plotly is interactive chart
# a <- survey[,-c(1:3, 8, 10)] %>%
#     group_by(floor) %>%
#     summarise_all(mean)
# 
# p2 <- plot_ly(
#     type = 'scatterpolar',
#     fill = 'toself',
#     width=700,
#     height=600
# ) %>%
#     add_trace(
#         r = as.numeric(as.vector(a[1,-1])),
#         theta = c('air movement','humidity','temp winter', 'temp summer', 'temp control', 'air control', 'adaptation freedom','overall comfort'),
#         name = a$floor[1]
#     ) %>%
#     add_trace(
#         r = as.numeric(as.vector(a[2,-1])),
#         theta = c('air movement','humidity','temp winter', 'temp summer', 'temp control', 'air control', 'adaptation freedom','overall comfort'),
#         name = a$floor[2]
#     ) %>%
#     layout(
#         title = paste("Mean Scores - ", build.name),
#         polar = list(
#             radialaxis = list(
#                 visible = T,
#                 range = c(1,7)
#             )
#         )
#     )
# 
# # export to static image locally
# # if (!require("processx")) install.packages("processx")
# orca(p2, paste(build.name, "_radar.png"))
