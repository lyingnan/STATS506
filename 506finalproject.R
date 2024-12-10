library(tidyverse)  
library(readxl)     
library(data.table) 
csv_path <- "C:/Users/Lenovo/Desktop/506/Final project/MUP_PHY_R24_P05_V10_D22_Geo.csv"
xlsx_path <- "C:/Users/Lenovo/Desktop/506/Final project/population data.xlsx"
csv_data <- fread(csv_path, select = c("Rndrng_Prvdr_Geo_Desc", "Tot_Rndrng_Prvdrs", "Tot_Benes", "Avg_Sbmtd_Chrg", "Tot_Srvcs" ))
xlsx_data <- read_excel(xlsx_path)
head(csv_data)
head(xlsx_data)
colnames(csv_data) <- c("State", "providers", "beneficiaries","ChargeAmount","ServicesAmount")
csv_data <- csv_data %>%
  group_by(State) %>%
  summarise(
    providers = sum(providers, na.rm = TRUE),
    beneficiaries = sum(beneficiaries, na.rm = TRUE),
    ChargeAmount = sum(ChargeAmount, na.rm = TRUE),
    ServicesAmount = sum(ServicesAmount, na.rm = TRUE)
  )

data <- left_join(csv_data, xlsx_data, by = c("State" = "State"))

valid_states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
                  "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                  "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                  "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", 
                  "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", 
                  "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
                  "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                  "West Virginia", "Wisconsin", "Wyoming", "District of Columbia")

data <- data %>%
  filter(State %in% valid_states)
head(data)
summary(data)

print(colSums(is.na(csv_data)))
print(colSums(is.na(xlsx_data)))

prov_data <- read_csv("C:/Users/Lenovo/Desktop/506/Final project/MUP_PHY_R24_P07_V10_D22_Prov.csv")
state_abbreviation_map <- data.frame(
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", 
            "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
            "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", 
            "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
            "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  Abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", 
                   "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", 
                   "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", 
                   "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
                   "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                   "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
)
prov_data <- prov_data %>%
  left_join(state_abbreviation_map, by = c("Rndrng_Prvdr_State_Abrvtn" = "Abbreviation"))
prov_summary <- prov_data %>%
  group_by(State) %>%
  summarize(Bene_Avg_Age = mean(Bene_Avg_Age, na.rm = TRUE))
final_data <- data %>%
  left_join(prov_summary, by = "State")

print(colSums(is.na(final_data)))

data <- final_data %>%
  mutate(
    Bene_Avg_Age = ifelse(is.na(Bene_Avg_Age), median(Bene_Avg_Age, na.rm = TRUE), Bene_Avg_Age)
  )
head(data)
data <- data %>%
  mutate(
    log_MedianHouseholdIncome = log(medianhouseholdincome + 1),
    log_ServicesAmount = log(ServicesAmount + 1)
  )

library(tidyverse)  
library(car)        
data <- data %>%
  mutate(ratio =providers/ beneficiaries )
lm_model <- lm(ratio ~ UrbanPopulationRatio +  log_MedianHouseholdIncome + log_ServicesAmount + Bene_Avg_Age, data = data)
summary(lm_model)

vif(lm_model)
plot(lm_model, which = 1) 
plot(lm_model, which = 2)
hist(residuals(lm_model), main = "Residuals Histogram", xlab = "Residuals", breaks = 20)

library(randomForest)  
set.seed(506)
rf_model <- randomForest(
  ratio ~ UrbanPopulationRatio +  log_MedianHouseholdIncome + log_ServicesAmount + Bene_Avg_Age,
  data = data,
  ntree = 500,          
  mtry = 2,             
  importance = TRUE    
)
print(rf_model)
varImpPlot(rf_model)

library(ggplot2)
importance_df <- as.data.frame(importance(rf_model)) %>%
  rownames_to_column(var = "Variable") %>%
  mutate(IncNodePurity = IncNodePurity * 1000) 
ggplot(importance_df, aes(x = `%IncMSE`, y = reorder(Variable, `%IncMSE`))) +
  geom_point(aes(size = IncNodePurity), color = "blue", alpha = 0.7) +
  geom_segment(aes(xend = 0, yend = Variable), color = "lightblue", linetype = "dotted") +
  theme_minimal() +
  labs(
    title = "Variable Importance: %IncMSE and IncNodePurity",
    x = "%IncMSE",
    y = "Variable",
    size = "IncNodePurity"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

