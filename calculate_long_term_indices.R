library(dplyr)
library(lubridate)

# set working directory
setwd(dir = "C:/Users/L/OneDrive/??????/PhD_long_term_compliance/matched bossa and samba database")

#### prepare the dataframes for calculation ####

# import dataset
m <- read.csv("thermal_measure_with_bossa_only.csv", header = TRUE)

# keep columns needed
m <-
    m[, c("alias", "floor_name", "Ta", "Tmrt", "PMV", "PPD", "created_at")]

# format timestamp
m$created_at <- ymd_hms(m$created_at)
m$date <- date(m$created_at)
m$hour <- hour(m$created_at)

# keep occupied hours 7am to 9pm (7 - 21)
m <- m %>% filter(hour >= 7 & hour <= 21)

# get operative temperature
m$To <- (m$Ta + m$Tmrt) / 2

# recode building name
m$alias <- recode(
    m$alias,
    "1 Martin Place" = "A",
    "Maritime Trade Towers" = "B",
    "33 Alfred" = "C",
    "200 George" = "D"
)

# keep columns needed for calculation
m <-
    m[, c("alias", "floor_name", "Ta", "To", "PMV", "PPD", "date", "hour")]

# get mean(measurement) per hour
m <- m %>%
    group_by(alias, floor_name, date, hour) %>%
    summarise(
        Ta = mean(Ta),
        To = mean(To),
        PMV = mean(PMV),
        PPD = mean(PPD)
    )

# set heating and cooling season
# Sydney seasons: 12-2 summer, 3-5 autumn, 6-8 winter, 9-11 spring
# consider month 11-4 as cooling season, 5-10 as heating season
m$season <-
    with(m, ifelse(month(date) >= 5 & month(date) <= 10,
                   "heating", "cooling"))

# for C level 21, keep measurements from 2017-2-1 to 2018-2-1
m <-
    m[!(m$alias == "C" &
            m$floor_name == "Level 21" & m$date > "2018-02-01"), ]


# get a list of dataframes for each floor in each building for building A,B,C
m1 = subset(m, alias != "D")
datasetAtoC <-
    split(m1, f = list(m1$alias, m1$floor_name), drop = TRUE)
m1 %>% group_by(alias, floor_name) %>%
    summarise(duration = paste(min(date), "to", max(date)))

# create dfs of differnt time periods for building D
m2 = subset(m, alias == "D")

# for bossa.time=201612
m3 <- m2[m2$date <= "2017-10-01",]
m3 %>% group_by(alias, floor_name) %>% summarise(days = length(unique(date)))
m3 <- split(m3, f = list(m3$alias, m3$floor_name), drop = TRUE)

# for bossa.time=201803
m4 <- m2[m2$date <= "2018-03-20" & m2$date >= "2017-03-01",]
m4 %>% group_by(alias, floor_name) %>% summarise(days = length(unique(date)))
m4 <- split(m4, f = list(m4$alias, m4$floor_name), drop = TRUE)

# for bossa.time=201808
m5 <- m2[m2$date <= "2018-09-10" & m2$date >= "2017-08-01",]
m5 %>% group_by(alias, floor_name) %>% summarise(days = length(unique(date)))
m5 <- split(m5, f = list(m5$alias, m5$floor_name), drop = TRUE)

# for bossa.time=201905
m6 <- m2[m2$date >= "2018-05-01",]
m6 %>% group_by(alias, floor_name) %>% summarise(days = length(unique(date)))
m6 <- split(m6, f = list(m6$alias, m6$floor_name), drop = TRUE)


#### create function to calculate indices for a single floor ####

indices_cal <- function(df) {
    # initial a dataframe to store results
    results <- data.frame(index.name = character(),
                          index.value = double())
    
    # percentage of time outside the PMV range
    for (limit in c(0.2, 0.5, 0.7)) {
        index.value <-
            100 * nrow(df %>% filter(PMV > limit |
                                         PMV < -limit)) / nrow(df)
        index.name <- paste("%|PMV|>", limit)
        row_to_add <- data.frame(index.value, index.name)
        results <- full_join(results, row_to_add)
    }
    
    
    # percentage of time outside an operative temperature range
    # and degree-hours indices
    
    # 1. To range from ISO 7730 class A
    # cooling season 23.5-25.5, heating season 21-23
    # tell if outside the above range, 0 = inside, 1 = outside
    df$out <-
        with(df, ifelse(
            season == "heating" & To >= 21 & To <= 23,
            0,
            ifelse(season == "cooling" &
                       To >= 23.5 & To <= 25.5, 0, 1)
        ))
    index.value <- sum(df$out) / nrow(df) * 100
    index.name <-
        paste("%_hours_outside_of_ISO_7730_class_A_temp_range")
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    # calculate degree-hours index.name
    # weighting factor
    df$wf <- with(df, ifelse(out == 1,
                             ifelse(
                                 season == "cooling",
                                 (pmax(To - 25.5, 23.5 - To) / 1 + 1),
                                 (pmax(To - 23, 21 - To) / 1 + 1)
                             ),
                             0))
    index.value <- sum(df$wf)
    index.name <- paste("degree-hours_ISO_7730_class_A")
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    
    # 2. To range from ISO 7730 class B
    # cooling season 23-26, heating season 20-24
    # tell if outside the above range, 0 = inside, 1 = outside
    df$out <-
        with(df, ifelse(
            season == "heating" & To >= 20 & To <= 24,
            0,
            ifelse(season == "cooling" &
                       To >= 23 & To <= 26, 0, 1)
        ))
    index.value <- sum(df$out) / nrow(df) * 100
    index.name <-
        paste("%_hours_outside_of_ISO_7730_class_B_temp_range")
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    # calculate degree-hours index.name
    # weighting factor
    df$wf <- with(df, ifelse(out == 1,
                             ifelse(
                                 season == "cooling",
                                 (pmax(To - 26, 23 - To) / 1.5 + 1),
                                 (pmax(To - 24, 20 - To) / 1.5 + 1)
                             ),
                             0))
    index.value <- sum(df$wf)
    index.name <- paste("degree-hours_ISO_7730_class_B")
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    
    # 3. To range from ISO 7730 class C
    # cooling season 22-27, heating season 19-25
    # tell if outside the above range, 0 = inside, 1 = outside
    df$out <-
        with(df, ifelse(
            season == "heating" & To >= 19 & To <= 25,
            0,
            ifelse(season == "cooling" &
                       To >= 22 & To <= 27, 0, 1)
        ))
    index.value <- sum(df$out) / nrow(df) * 100
    index.name <-
        paste("%_hours_outside_of_ISO_7730_class_C_temp_range")
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    # calculate degree-hours index.name
    # weighting factor
    df$wf <- with(df, ifelse(out == 1,
                             ifelse(
                                 season == "cooling",
                                 (pmax(To - 27, 22 - To) / 2 + 1),
                                 (pmax(To - 25, 19 - To) / 2 + 1)
                             ),
                             0))
    index.value <- sum(df$wf)
    index.name <- paste("degree-hours_ISO_7730_class_C")
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    
    # 4. To range from EN 15251 class I
    # heating season >= 21, cooling season <= 25.5
    # tell if outside the above range, 0 = inside, 1 = outside
    df$out <- with(df, ifelse(
        season == "heating" & To >= 21,
        0,
        ifelse(season == "cooling" &
                   To <= 25.5, 0, 1)
    ))
    index.value <- sum(df$out) / nrow(df) * 100
    index.name <-
        paste("%_hours_outside_of_EN_15251_class_I_temp_range")
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    # calculate degree-hours index.name
    # weighting factor
    df$wf <- with(df, ifelse(out == 1,
                             ifelse(season == "cooling",
                                    To - 25.5,
                                    21 - To),
                             0))
    index.value <- sum(df$wf)
    index.name <- paste("degree_hours_EN_15251_class_I")
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    
    # 5. To range from EN 15251 class II
    # heating season >= 20, cooling season <= 26
    # tell if outside the above range, 0 = inside, 1 = outside
    df$out <- with(df, ifelse(
        season == "heating" & To >= 20,
        0,
        ifelse(season == "cooling" &
                   To <= 26, 0, 1)
    ))
    index.value <- sum(df$out) / nrow(df) * 100
    index.name <-
        paste("%_hours_outside_of_EN_15251_class_II_temp_range")
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    # calculate degree-hours index.name
    # weighting factor
    df$wf <- with(df, ifelse(out == 1,
                             ifelse(season == "cooling",
                                    To - 26,
                                    20 - To),
                             0))
    index.value <- sum(df$wf)
    index.name <- paste("degree_hours_EN_15251_class_II")
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    
    # 6. To range from EN 15251 class III
    # heating season >= 19, cooling season <= 27
    # tell if outside the above range, 0 = inside, 1 = outside
    df$out <- with(df, ifelse(
        season == "heating" & To >= 19,
        0,
        ifelse(season == "cooling" &
                   To <= 27, 0, 1)
    ))
    index.value <- sum(df$out) / nrow(df) * 100
    index.name <-
        paste("%_hours_outside_of_EN_15251_class_III_temp_range")
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    # calculate degree-hours index.name
    # weighting factor
    df$wf <- with(df, ifelse(out == 1,
                             ifelse(season == "cooling",
                                    To - 27,
                                    19 - To),
                             0))
    index.value <- sum(df$wf)
    index.name <- paste("degree_hours_EN_15251_class_III")
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    
    # 7. percentage of time outside the range derived from ASHRAE DB2
    # Australia, Humid subtropical climate, acceptable Ta range is 19.9 - 27.2
    Ta.up <- 27.2
    Ta.low <- 19.9
    index.value <- 100 * nrow(df %>% filter(Ta > Ta.up |
                                                Ta < Ta.low)) / nrow(df)
    index.name <-
        paste("%_hours_outside_of_Ta_range_", Ta.low, "_", Ta.up)
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    
    # PPD weighted - ISO 7730
    PMV.class <- c(0.2, 0.5, 0.7)
    PPD.class <- c(6, 10, 15)
    class.name <- c("A", "B", "C")
    for (i in 1:3) {
        # weighting factor
        df$wf <-
            with(df,
                 ifelse(PMV >= PMV.class[i] |
                            PMV <= -PMV.class[i], PPD / PPD.class[i], 0))
        index.value <- sum(df$wf)
        index.name <-
            paste("PPD-weighted_ISO_7730_Class_", class.name[i])
        row_to_add <- data.frame(index.value, index.name)
        results <- full_join(results, row_to_add)
    }
    
    # PPD weighted - EN 15251
    class.name <- c("I", "II", "III")
    for (i in 1:3) {
        # weighting factor
        df$wf <-
            with(df,
                 ifelse(PMV > PMV.class[i] |
                            PMV < -PMV.class[i], PPD / PPD.class[i], 0))
        index.value <- sum(df$wf)
        index.name <-
            paste("PPD_weighted_EN_15251_Class_", class.name[i])
        row_to_add <- data.frame(index.value, index.name)
        results <- full_join(results, row_to_add)
    }
    
    
    # accumulated PPD
    index.value <- sum(df$PPD)
    index.name <- "sum_PPD"
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    
    # average PPD
    index.value <- mean(df$PPD)
    index.name <- "average_PPD"
    row_to_add <- data.frame(index.value, index.name)
    results <- full_join(results, row_to_add)
    
    # add necessary columns
    results$building <- as.character(unique(df$alias))
    results$floor <- as.character(unique(df$floor_name))
    results$samba_duration <- paste(min(df$date), "to", max(df$date))
    results$samba_days <- length(unique(df$date))
    
    # return a dataframe
    results
}

#### calculate the indices ####

# building A to C
resultsAtoC <- data.frame(
    index.name = character(),
    index.value = double(),
    building = as.character(),
    floor = as.character(),
    samba_duration = as.character(),
    samba_days = numeric()
)

for (df in datasetAtoC) {
    resultsAtoC <- full_join(resultsAtoC, indices_cal(df))
}

# add bossa timecode for later matching
# create a table for merge
buildings <- c('A', 'B', 'B', 'C', 'C', 'C', 'C', 'C', 'C')
floors <-
    c(
        'Level 19',
        'Level 9',
        'Level 10',
        'Level 6',
        'Level 21',
        'Level 6',
        'Level 21',
        'Level 12',
        'Level 2'
    )
timecodes <-
    c(
        '201703',
        '201805',
        '201805',
        '201611',
        '201611',
        '201706',
        '201706',
        '201803',
        '201810'
    )
key <-
    data.frame("building" = buildings,
               "floor" = floors,
               "bossa.time" = timecodes)

resultsAtoC <-
    merge(
        x = resultsAtoC,
        y = key,
        by = c("building", "floor"),
        all = TRUE
    )


# building D
resultsD <- data.frame(
    index.name = character(),
    index.value = double(),
    building = as.character(),
    floor = as.character(),
    samba_duration = as.character(),
    samba_days = numeric()
)

for (df in m3) {
    result <- indices_cal(df)
    result$bossa.time <- "201612"
    resultsD <- full_join(resultsD, result)
}

for (df in m4) {
    result <- indices_cal(df)
    result$bossa.time <- "201803"
    resultsD <- full_join(resultsD, result)
}

for (df in m5) {
    result <- indices_cal(df)
    result$bossa.time <- "201808"
    resultsD <- full_join(resultsD, result)
}

for (df in m6) {
    result <- indices_cal(df)
    result$bossa.time <- "201905"
    resultsD <- full_join(resultsD, result)
}

# combine all buildings' results
total_results <- full_join(resultsAtoC, resultsD)

# save the results
write.csv(total_results,
          file = "./calculated results/long-term indices.csv",
          row.names = FALSE)
