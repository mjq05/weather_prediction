if (!requireNamespace("rlang", quietly = TRUE)) {
    install.packages("rlang")
}

if (!requireNamespace("tidymodels", quietly = TRUE)) {
    install.packages("tidymodels")
}

if (!requireNamespace("corrplot", quietly = TRUE)) {
    install.packages("corrplot")
}

# Load libraries
library(rlang)
library(tidymodels)
library(corrplot)

# Load data
data_url <- "C:/Users/sIUzy/Documents/weather_predictions/jfk_weather_sample.csv"
weather_data <- read.csv(data_url)

# Display structure of the dataset
str(weather_data)

colnames(weather_data)

head(weather_data)

missing_values <- colSums(is.na(weather_data))
print(missing_values)

weather_data <- na.omit(weather_data)

numeric_columns <- c("HOURLYDRYBULBTEMPF", "HOURLYWindSpeed", "HOURLYRelativeHumidity", "HOURLYDewPointTempF", "HOURLYSeaLevelPressure", "HOURLYPrecip")

suppressWarnings({
    for (col in numeric_columns) {
        weather_data[[col]] <- as.numeric(as.character(weather_data[[col]]))
        weather_data[[col]][is.na(weather_data[[col]])] <- 0 # Replace NAs with 0 or choose an appropriate strategy
    }
})

weather_data$DATE <- as.POSIXct(weather_data$DATE, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

correlation_matrix <- cor(weather_data[, numeric_columns], use = "complete.obs")
corrplot(correlation_matrix, method = "color")

colnames(weather_data)

summary(weather_data)

set.seed(123)
split_index <- initial_split(weather_data, prop = 0.8)
train_data <- training(split_index)
test_data <- testing(split_index)

linear_model <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")

weather_recipe <- recipe(HOURLYPrecip ~ HOURLYDRYBULBTEMPF + HOURLYWindSpeed + HOURLYRelativeHumidity + HOURLYDewPointTempF + HOURLYSeaLevelPressure, data = train_data) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())

weather_workflow <- workflow() %>%
    add_model(linear_model) %>%
    add_recipe(weather_recipe)

weather_fit <- weather_workflow %>%
    fit(data = train_data)

predictions <- weather_fit %>%
    predict(test_data)

if (!".pred" %in% colnames(predictions)) {
    stop("The '.pred' column is not present in the predictions.")
}

if (any(is.na(predictions$.pred))) {
    stop("There are missing values in the '.pred' column of predictions.")
}

plot_data <- data.frame(
    Predicted = predictions$.pred,
    Actual = test_data$HOURLYPrecip
)

plot_data <- plot_data[complete.cases(plot_data), ]

plot(plot_data$Predicted, plot_data$Actual,
    main = "Predictions vs Actual Values", xlab = "Predicted Precipitation", ylab = "Actual Precipitation"
)

rmse <- sqrt(mean((plot_data$Predicted - plot_data$Actual)^2, na.rm = TRUE))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

summary(weather_fit$fit)

plot(plot_data$Predicted, plot_data$Actual,
    main = "Predictions vs Actual Values", xlab = "Predicted Precipitation", ylab = "Actual Precipitation"
)
