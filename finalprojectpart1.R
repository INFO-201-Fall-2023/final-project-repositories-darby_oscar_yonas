library(dplyr)
# There are two data sets: Behavior Risk Factor Data of Tobacco and U.S. Life Expectancy at Birth
# We will do a inner join to merge the common columns in both data sets, 'State'.

tobacco_data <- read.csv("Tobacco_Risk_Data_2011_present.csv")
life_data <- read.csv("Life_Expectancy_at_Birth_State_2015.csv")

merged_data <- left_join(tobacco_data, life_data, by = "State")

# As our second data focuses on 2015, we will filter out unnecessary years in the tobacco_data (i.e. 2011 - 2014, 2016-2019)
filter_data <- filter(merged_data, YEAR == 2015)

# Add a continuous variable. We will calculate and add a new column "mean.life.expectancy" for future convenience.
mean_le_state <- group_by(filter_data, State)
mean_le_state <- mutate(mean_le_state, mean_life_expectancy = mean(na.omit(Life.Expectancy)))

# Add a categorical variable. We will organize states by their appropriate regions for future convenience as well.
assign_region <- function(state) {
  if (state %in% c("Washington", "Oregon", "Idaho", "Montana")) {
    return("Northwest")
  } else if (state %in% c("California", "Nevada", "Arizona", "New Mexico")) {
    return("West")
  } else if (state %in% c("Texas", "Oklahoma", "Louisiana", "Arkansas", "Mississippi", "Alabama", "Georgia", "Florida", "South Carolina", "North Carolina", "Virginia", "Tennessee", "Kentucky", "West Virginia", "Maryland", "Delaware", "District of Columbia")) {
    return("South")
  } else if (state %in% c("New York", "New Jersey", "Pennsylvania", "Connecticut", "Rhode Island", "Massachusetts", "Vermont", "New Hampshire", "Maine")) {
    return("Northeast")
  } else if (state %in% c("Illinois", "Indiana", "Ohio", "Michigan", "Wisconsin", "Minnesota", "Iowa", "Missouri", "North Dakota", "South Dakota", "Nebraska", "Kansas")) {
    return("Midwest")
  } else {
    return(NA)
  }
}

mean_le_state$Region <- sapply(mean_le_state$State, assign_region)
state_region <- mean_le_state[!is.na(mean_le_state$Region),]

# Add summarization data frame. As mean life expectancy was already created, we will calculate the mode as well.
summ_df <- group_by(state_region, State)
summ_df <- summarize(summ_df, mean_life_expectancy = mean(mean_life_expectancy), mode(Life.Expectancy))

# Final Cleanup. As joined dataset has continuously been updated.
final_df <- state_region
write.csv(final_df, "final_df.csv", row.names = FALSE)