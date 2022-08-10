#Asignacion 1 - Analisis Predictivo de Negocio
#Ivan De Frias - 108870
#Jorge Luis Torres - 1039293
#Emisiones de CO2

library(foreign) 
library(ggplot2)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(stargazer)
library(caTools)
library(caret)
library(stringr)

# ---- Datos ----
data <- read.csv("/Users/ivandefrias/Downloads/CO2 Emissions_Canada (1).csv")

# ---- Limpiando data ----
missing <- table(complete.cases(data)) # sin NAs

# ---- Analisis Exploratorio ----
ggplot(data=data, aes(x=CO2.Emissions.g.km., fill=Fuel.Type)) +
  geom_density(adjust=1.5, alpha=.4)

group_avg <- aggregate(data[,12], list(data$Make), mean)
group_avg <- group_avg[order(group_avg[,2], decreasing = TRUE),]
group_avg <- group_avg[1:10,1:2]

top10 <- c("BUGATTI","LAMBORGHINI","SRT","ROLLS-ROYCE","BENTLEY",
           "ASTON MARTIN MASERATI","GMC","RAM","LAND ROVER")

data_top10 <- subset(data, data$Make == "BUGATTI" | data$Make == "LAMBORGHINI" | data$Make == "SRT" | data$Make == "ROLLS-ROYCE" | data$Make == "BENTLEY" | data$Make == "ASTON MARTIN MASERATI" | data$Make == "GMC" | data$Make == "RAM" | data$Make == "LAND ROVER")

ggplot(data, aes(x = CO2.Emissions.g.km., y = Vehicle.Class, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Emissions by Vehicle Class') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(1, "lines"),
    strip.text.x = element_text(size = 2)
  )

ggplot(data_top10, aes(x = CO2.Emissions.g.km., y = `Make`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Top 10 Auto-Makers by CO2 Emissions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(1, "lines"),
    strip.text.x = element_text(size = 2)
  )

# ---- Transformcion de Transmicion ----
data$Transmission <- str_replace_all(data$Transmission, "[:digit:]", "")
# Barplot:
ggplot(data, aes(x=Transmission, y=CO2.Emissions.g.km., fill=Transmission)) + 
  geom_bar(stat='identity')

# ---- Data de entretamiento y de testeo de modelo ----
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
test_index <- setdiff(1:nrow(data), train_index)

train_data <- data[train_index,]
test_data <- data[test_index,]

# ---- Regresion ----
regressor = lm(formula = `CO2.Emissions.g.km.` ~ .,
               data = train_data)
summary(regressor)


