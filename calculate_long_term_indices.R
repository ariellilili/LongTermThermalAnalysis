library(dplyr)
library(lubridate)

# set working directory
setwd(dir = "C:/Users/L/OneDrive/??????/PhD_long_term_compliance/matched bossa and samba database")

#### prepare the dataframe for calculation ####

# import dataset
m2017 <-
    read.csv("2017_thermal_measure_with_bossa_only.csv", header = TRUE)

# keep columns needed
m2017 <-
    m2017[, c("alias", "floor_name", "Ta", "Tmrt", "PMV", "PPD", "created_at")]

# format timestamp
m2017$created_at <- ymd_hms(m2017$created_at)
m2017$date <- date(m2017$created_at)
m2017$hour <- hour(m2017$created_at)

# keep occupied hours 7am to 9pm (7 - 21)
m2017 <- m2017 %>% filter(hour >= 7 & hour <= 21)

# get operative temperature
m2017$To <- (m2017$Ta + m2017$Tmrt) / 2

# combine building name and floor name
m2017$location <-
    as.factor(paste(m2017$alias, m2017$floor_name, sep = " "))

# keep columns needed for calculation
m2017 <- m2017[, c("location", "Ta", "To", "PMV", "PPD", "date", "hour")]

# get mean(measurement) per hour
m2017 <- m2017 %>%
    group_by(location, date, hour) %>%
    summarise(
        Ta = mean(Ta),
        To = mean(To),
        PMV = mean(PMV),
        PPD = mean(PPD)
    )

# set heating and cooling season
# Sydney seasons: 12-2 summer, 3-5 autumn, 6-8 winter, 9-11 spring
# consider month 11-4 as cooling season, 5-10 as heating season
m2017$season <-
    with(m2017, ifelse(month(date) >= 5 & month(date) <= 10,
                       "heating", "cooling"))

# separate into several dataframes by location
dataset.list <- split(m2017, f = m2017$location)



#### create functions ####

indices_cal <- function(dataframe = df) {
    # initial a dataframe to store results
    results <- data.frame(index = character(),
                          value = double())
    
    # percentage of time outside the PMV range
    for (limit in c(0.2, 0.5, 0.7)) {
        value <-
            100 * nrow(df %>% filter(PMV > limit | PMV < -limit)) / nrow(df)
        index <- paste("%|PMV|>", limit)
        row_to_add <- data.frame(value, index)
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
    value <- sum(df$out) / nrow(df) * 100
    index <- paste("%_hours_outside_of_ISO_7730_class_A_temp_range")
    row_to_add <- data.frame(value, index)
    results <- full_join(results, row_to_add)
    # calculate degree-hours index
    # weighting factor
    df$wf <- with(df, ifelse(out == 1,
                             ifelse(
                                 season == "cooling",
                                 (pmax(To - 25.5, 23.5 - To) / 1 + 1),
                                 (pmax(To - 23, 21 - To) / 1 + 1)
                             ),
                             0))
    value <- sum(df$wf)
    index <- paste("degree-hours_ISO_7730_class_A")
    row_to_add <- data.frame(value, index)
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
    value <- sum(df$out) / nrow(df) * 100
    index <- paste("%_hours_outside_of_ISO_7730_class_B_temp_range")
    row_to_add <- data.frame(value, index)
    results <- full_join(results, row_to_add)
    # calculate degree-hours index
    # weighting factor
    df$wf <- with(df, ifelse(out == 1,
                             ifelse(
                                 season == "cooling",
                                 (pmax(To - 26, 23 - To) / 1.5 + 1),
                                 (pmax(To - 24, 20 - To) / 1.5 + 1)
                             ),
                             0))
    value <- sum(df$wf)
    index <- paste("degree-hours_ISO_7730_class_B")
    row_to_add <- data.frame(value, index)
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
    value <- sum(df$out) / nrow(df) * 100
    index <- paste("%_hours_outside_of_ISO_7730_class_C_temp_range")
    row_to_add <- data.frame(value, index)
    results <- full_join(results, row_to_add)
    # calculate degree-hours index
    # weighting factor
    df$wf <- with(df, ifelse(out == 1,
                             ifelse(
                                 season == "cooling",
                                 (pmax(To - 27, 22 - To) / 2 + 1),
                                 (pmax(To - 25, 19 - To) / 2 + 1)
                             ),
                             0))
    value <- sum(df$wf)
    index <- paste("degree-hours_ISO_7730_class_C")
    row_to_add <- data.frame(value, index)
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
    value <- sum(df$out) / nrow(df) * 100
    index <- paste("%_hours_outside_of_EN_15251_class_I_temp_range")
    row_to_add <- data.frame(value, index)
    results <- full_join(results, row_to_add)
    # calculate degree-hours index
    # weighting factor
    df$wf <- with(df, ifelse(out == 1,
                             ifelse(season == "cooling",
                                    To - 25.5,
                                    21 - To),
                             0))
    value <- sum(df$wf)
    index <- paste("degree_hours_EN_15251_class_I")
    row_to_add <- data.frame(value, index)
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
    value <- sum(df$out) / nrow(df) * 100
    index <- paste("%_hours_outside_of_EN_15251_class_II_temp_range")
    row_to_add <- data.frame(value, index)
    results <- full_join(results, row_to_add)
    # calculate degree-hours index
    # weighting factor
    df$wf <- with(df, ifelse(out == 1,
                             ifelse(season == "cooling",
                                    To - 26,
                                    20 - To),
                             0))
    value <- sum(df$wf)
    index <- paste("degree_hours_EN_15251_class_II")
    row_to_add <- data.frame(value, index)
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
    value <- sum(df$out) / nrow(df) * 100
    index <- paste("%_hours_outside_of_EN_15251_class_III_temp_range")
    row_to_add <- data.frame(value, index)
    results <- full_join(results, row_to_add)
    # calculate degree-hours index
    # weighting factor
    df$wf <- with(df, ifelse(out == 1,
                             ifelse(season == "cooling",
                                    To - 27,
                                    19 - To),
                             0))
    value <- sum(df$wf)
    index <- paste("degree_hours_EN_15251_class_III")
    row_to_add <- data.frame(value, index)
    results <- full_join(results, row_to_add)
    
    # 7. percentage of time outside the range derived from ASHRAE DB2
    # Australia, Humid subtropical climate, acceptable Ta range is 19.9 - 27.2
    Ta.up <- 27.2
    Ta.low <- 19.9
    value <- 100 * nrow(df %>% filter(Ta > Ta.up |
                                          Ta < Ta.low)) / nrow(df)
    index <- paste("%_hours_outside_of_Ta_range_",Ta.low, "_", Ta.up)
    row_to_add <- data.frame(value, index)
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
        value <- sum(df$wf)
        index <- paste("PPD-weighted_ISO_7730_Class_", class.name[i])
        row_to_add <- data.frame(value, index)
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
        value <- sum(df$wf)
        index <- paste("PPD_weighted_EN_15251_Class_", class.name[i])
        row_to_add <- data.frame(value, index)
        results <- full_join(results, row_to_add)
    }
    
    
    # accumulated PPD
    value <- sum(df$PPD)
    index <- "sum_PPD"
    row_to_add <- data.frame(value, index)
    results <- full_join(results, row_to_add)
    
    # average PPD
    value <- mean(df$PPD)
    index <- "average_PPD"
    row_to_add <- data.frame(value, index)
    results <- full_join(results, row_to_add)
    
    # add a column to mark the location name
    results$location <- as.character(unique(df$location))
    
    # return a dataframe
    results
}

#### calculatet the indices ####
total_results <- data.frame(index = character(),
                            value = double(),
                            location = as.character())
for (df in dataset.list) {
    total_results <- full_join(total_results, indices_cal(df))
}

# save the results
write.csv(
    total_results,
    file = paste("2017 long-term indices.csv"),
    row.names = FALSE
)

