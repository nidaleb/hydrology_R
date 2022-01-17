install.packages("MASS")
library(MASS)
# Reads completed file from csv
watersheds_df <- read.csv(here("output", "practical_2", "mersey_watersheds_ea.csv"))
colnames(watersheds_df) 


# Creates a vector of column names, including only those which contain "average" or "percent"
factors <- colnames(watersheds_df %>% dplyr::select(contains(c("average", "percent"))))

# Prints to console
factors
variables <- watersheds_df[factors]
# Column bind the NO2 column from watersheds_df with the data frame containing all the independent variables 
model_df <- cbind(NO2 = watersheds_df$NO2, variables)
# Fits a linear model
no2_model <- lm(formula = NO2 ~ ., data = model_df)
summary(no2_model)
# Stepwise regression model
step.model <- stepAIC(no2_model, # Input linear model
                      direction = "both",
                      trace = FALSE, # Print out intermediate results? 
                      k = 4) 
summary(step.model)

# Column bind the NO3 column from watersheds_df with the data frame containing all the independent variables 
model_df2 <- cbind(NO3 = watersheds_df$NO3, variables)
# Fits a linear model
no3_model <- lm(formula = NO3 ~ ., data = model_df2)
summary(no3_model)
# Stepwise regression model
step.model2 <- stepAIC(no3_model, # Input linear model
                      direction = "both",
                      trace = FALSE, # Print out intermediate results? 
                      k = 3) 
summary(step.model2)

# Column bind the NH4 column from watersheds_df with the data frame containing all the independent variables 
model_df3 <- cbind(NH4 = watersheds_df$NH4, variables)
# Fits a linear model
nh4_model <- lm(formula = NH4 ~ ., data = model_df3)
summary(nh4_model)
# Stepwise regression model
step.model3 <- stepAIC(nh4_model, # Input linear model
                       direction = "both",
                       trace = FALSE, # Print out intermediate results? 
                       k = 3) 
summary(step.model3)


# Column bind the Mg column from watersheds_df with the data frame containing all the independent variables 
model_df4 <- cbind(Mg = watersheds_df$Mg, variables)
# Fits a linear model
mg_model <- lm(formula = Mg ~ ., data = model_df4)
summary(mg_model)
# Stepwise regression model
step.model4 <- stepAIC(mg_model, # Input linear model
                       direction = "both",
                       trace = FALSE, # Print out intermediate results? 
                       k = 2) 
summary(step.model4)

# Column bind the Ca column from watersheds_df with the data frame containing all the independent variables 
model_df5 <- cbind(Ca = watersheds_df$Ca, variables)
# Fits a linear model
ca_model <- lm(formula = Ca ~ ., data = model_df5)
summary(ca_model)
# Stepwise regression model
step.model5 <- stepAIC(ca_model, # Input linear model
                       direction = "both",
                       trace = FALSE, # Print out intermediate results? 
                       k = 3) 
summary(step.model5)

# Column bind the PO4 column from watersheds_df with the data frame containing all the independent variables 
model_df6 <- cbind(PO4 = watersheds_df$PO4, variables)
# Fits a linear model
po4_model <- lm(formula = PO4 ~ ., data = model_df6)
summary(po4_model)
# Stepwise regression model
step.model6 <- stepAIC(po4_model, # Input linear model
                       direction = "both",
                       trace = FALSE, # Print out intermediate results? 
                       k = 4) 
summary(step.model6)

# Column bind the TON column from watersheds_df with the data frame containing all the independent variables 
model_df7 <- cbind(TON = watersheds_df$TON, variables)
# Fits a linear model
ton_model <- lm(formula = TON ~ ., data = model_df7)
summary(ton_model)
# Stepwise regression model
step.model7 <- stepAIC(ton_model, # Input linear model
                       direction = "both",
                       trace = FALSE, # Print out intermediate results? 
                       k = 3) 
summary(step.model7)

# Column bind the Zn column from watersheds_df with the data frame containing all the independent variables 
model_df8 <- cbind(Zn = watersheds_df$Zn, variables)
# Fits a linear model
zn_model <- lm(formula = Zn ~ ., data = model_df8)
summary(zn_model)
# Stepwise regression model
step.model8 <- stepAIC(zn_model, # Input linear model
                       direction = "both",
                       trace = FALSE, # Print out intermediate results? 
                       k = 3) 
summary(step.model8)

# Column bind the Ph column from watersheds_df with the data frame containing all the independent variables 
model_df9 <- cbind(Ph = watersheds_df$Ph, variables)
# Fits a linear model
ph_model <- lm(formula = Ph ~ ., data = model_df9)
summary(ph_model)
# Stepwise regression model
step.model9 <- stepAIC(ph_model, # Input linear model
                       direction = "both",
                       trace = FALSE, # Print out intermediate results? 
                       k = 2) 
summary(step.model9)

# Column bind the SSC column from watersheds_df with the data frame containing all the independent variables 
model_df10 <- cbind(SSC = watersheds_df$SSC, variables)
# Fits a linear model
ssc_model <- lm(formula = SSC ~ ., data = model_df10)
summary(ssc_model)
# Stepwise regression model
step.model10 <- stepAIC(ssc_model, # Input linear model
                       direction = "both",
                       trace = FALSE, # Print out intermediate results? 
                       k = 3) 
summary(step.model10)
#error analysis
# Predicts based on upon stepwise model, saving to watersheds dataframe
watersheds_df$predicted_no2 <- predict(step.model)
no2_plot <- ggplot(data = watersheds_df, aes(x = predicted_no2, y = NO2)) +
  # Adds the point data, modifying the shape, size, colour and fill
  geom_point(shape = 21, colour = "black", fill = "#5695FF", size = 1.5) +
  # Adding a linear regression ("lm"), removing standard error bars (se = FALSE)
  geom_smooth(method = "lm", se = FALSE, colour = "#FF7A71") +
  # Adding a 1:1 line for comparison
  geom_abline(intercept = 0, slope = 1, lty = "dashed") +
  # Setting the theme and aspect ratio
  theme_classic() +
  theme(aspect.ratio = 1) +
  # Add axis labels and a title
  labs(x = "Modelled NO2", y = "Measured NO2", 
       title = bquote('Plot of measured vs. modelled'~NO[2]~'values'))


no2_plot

head()
watersheds_df$predicted_no2
qqnorm(watersheds_df$NO2, pch = 1, frame = FALSE)
qqline(watersheds_df$NO2, col = "steelblue", lwd = 2)
#histo
model1<-lm(formula = NO2 ~ average_slope + Urban_percent, data = model_df)
#load ggplot2
library(ggplot2)

#create histogram of residuals
ggplot(data = model_df, aes(x = model1$residuals)) +
  geom_histogram(bins = 20,fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')
