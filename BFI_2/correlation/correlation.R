# Pengda
library(psych)
library(lavaan)
library(Metrics)

data_normal_human <- read.table("Data/human_normal.csv",sep = ",",header = T)
data_normal_human <- na.omit(data_normal_human)
data_old_people <- read.table("Data/old_people.csv",sep = ",",header = T)
data_old_people <- na.omit(data_old_people)

data_persona_GPT3.5 <- read.table("Data/GPT_3.5/persona_bfi60_3.5.csv",sep = ",",header = T)
data_shape_GPT3.5 <- read.table("Data/GPT_3.5/shape_bfi60_300_3.5.csv",sep = ",",header = T)

data_persona_GPT4 <- read.table("Data/GPT_4/persona_bfi60_4.csv",sep = ",",header = T)
data_shape_GPT4 <- read.table("Data/GPT_4/shape_bfi60_300_4.csv",sep = ",",header = T)

data_persona_LLaMA3 <- read.table("Data/LLaMA3/persona_bfi60_instruction.csv",sep = ",",header = T)
data_shape_LLaMA3 <- read.table("Data/LLaMA3/shape_bfi60_instruction_300.csv",sep = ",",header = T)

data_normal_human <- data_normal_human[2: 61]
data_old_people <- data_old_people[21: 80]

data_persona_GPT3.5 <- data_persona_GPT3.5[2: 61]
data_shape_GPT3.5 <- data_shape_GPT3.5[2: 61]

data_persona_GPT4 <- data_persona_GPT4[2: 61]
data_shape_GPT4 <- data_shape_GPT4[2: 61]

data_persona_LLaMA3 <- data_persona_LLaMA3[2: 61]
data_shape_LLaMA3 <- data_shape_LLaMA3[2: 61]

reverse_code_columns <- c(11, 16, 26, 31, 36, 51, 12, 17, 22, 37, 42, 47, 3, 8, 23, 28, 48, 58, 4, 9, 
                          24, 29, 44, 49, 5, 25, 30, 45, 50, 55)

for (col in reverse_code_columns) {
  data_normal_human[, col] <- 6 - data_normal_human[, col]
  data_old_people[, col] <- 6 - data_old_people[, col]
  data_persona_GPT3.5[, col] <- 6 - data_persona_GPT3.5[, col]
  data_shape_GPT3.5[, col] <- 6 - data_shape_GPT3.5[, col]
  data_persona_GPT4[, col] <- 6 - data_persona_GPT4[, col]
  data_shape_GPT4[, col] <- 6 - data_shape_GPT4[, col]
  data_persona_LLaMA3[, col] <- 6 - data_persona_LLaMA3[, col]
  data_shape_LLaMA3[, col] <- 6 - data_shape_LLaMA3[, col]
}

filter_data <- function(df) {
  df[apply(df, 1, function(row) all(row >= 1 & row <= 5)), ]
}
data_normal_human <- filter_data(data_normal_human)
data_old_people <- filter_data(data_old_people)
data_persona_GPT3.5 <- filter_data(data_persona_GPT3.5)
data_shape_GPT3.5 <- filter_data(data_shape_GPT3.5)
data_persona_GPT4 <- filter_data(data_persona_GPT4)
data_shape_GPT4 <- filter_data(data_shape_GPT4)
data_persona_LLaMA3 <- filter_data(data_persona_LLaMA3)
data_shape_LLaMA3 <- filter_data(data_shape_LLaMA3)

# Facet
Sociability <- c(1, 16, 31, 46)
Assertiveness <- c(6, 21, 36, 51)
Energy_Level <- c(11, 26, 41, 56)
Compassion <- c(2, 17, 32, 47)
Respectfulness <- c(7, 22, 37, 52)
Trust <- c(12, 27, 42, 57)
Organization <- c(3, 18, 33, 48) 
Productiveness <- c(8, 23, 38, 53)
Responsibility <- c(13, 28, 43, 58)
Anxiety <- c(4, 19, 34, 49)
Depression <- c(9, 24, 39, 54)
Emotional_Volatility <- c(14, 29, 44, 59)
Intellectual_Curiosity <- c(10, 25, 40, 55)
Aesthetic_Sensitivity <- c(5, 20, 35, 50) 
Creative_Imagination <- c(15, 30, 45, 60)

# Domain
Extraversion <- c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56)
Agreeableness <- c(2, 7, 12, 17, 22, 27, 32, 37, 42, 47, 52, 57)
Conscientiousness <- c(3, 8, 13, 18, 23, 28, 33, 38, 43, 48, 53, 58)
Neuroticism <- c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59)
Openness <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)



#### scale correlation 
calculate_dimension_score <- function(data, indices) {
  rowSums(data[, indices], na.rm = TRUE)
}


# normal_human
data_normal_human$Extraversion <- calculate_dimension_score(data_normal_human, Extraversion) 
data_normal_human$Agreeableness <- calculate_dimension_score(data_normal_human, Agreeableness)
data_normal_human$Conscientiousness <- calculate_dimension_score(data_normal_human, Conscientiousness)
data_normal_human$Neuroticism <- calculate_dimension_score(data_normal_human, Neuroticism)
data_normal_human$Openness <- calculate_dimension_score(data_normal_human, Openness)

dimension_scores <- data_normal_human[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
write.csv(dimension_scores, "data_normal_human_correlation_domain.csv", row.names = FALSE)

correlation_matrix_normal_human <- cor(dimension_scores)
print(correlation_matrix_normal_human)



# persona_GPT3.5
data_persona_GPT3.5$Extraversion <- calculate_dimension_score(data_persona_GPT3.5, Extraversion) 
data_persona_GPT3.5$Agreeableness <- calculate_dimension_score(data_persona_GPT3.5, Agreeableness)
data_persona_GPT3.5$Conscientiousness <- calculate_dimension_score(data_persona_GPT3.5, Conscientiousness)
data_persona_GPT3.5$Neuroticism <- calculate_dimension_score(data_persona_GPT3.5, Neuroticism)
data_persona_GPT3.5$Openness <- calculate_dimension_score(data_persona_GPT3.5, Openness)

dimension_scores <- data_persona_GPT3.5[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
write.csv(dimension_scores, "data_persona_GPT3.5_correlation_domain.csv", row.names = FALSE)

correlation_matrix_persona_GPT3.5 <- cor(dimension_scores)
print(correlation_matrix_persona_GPT3.5)


# shape_GPT3.5
data_shape_GPT3.5$Extraversion <- calculate_dimension_score(data_shape_GPT3.5, Extraversion) 
data_shape_GPT3.5$Agreeableness <- calculate_dimension_score(data_shape_GPT3.5, Agreeableness)
data_shape_GPT3.5$Conscientiousness <- calculate_dimension_score(data_shape_GPT3.5, Conscientiousness)
data_shape_GPT3.5$Neuroticism <- calculate_dimension_score(data_shape_GPT3.5, Neuroticism)
data_shape_GPT3.5$Openness <- calculate_dimension_score(data_shape_GPT3.5, Openness)

dimension_scores <- data_shape_GPT3.5[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
write.csv(dimension_scores, "data_shape_GPT3.5_correlation_domain.csv", row.names = FALSE)

correlation_matrix_shape_GPT3.5 <- cor(dimension_scores)
print(correlation_matrix_shape_GPT3.5)



# persona_GPT4
data_persona_GPT4$Extraversion <- calculate_dimension_score(data_persona_GPT4, Extraversion) 
data_persona_GPT4$Agreeableness <- calculate_dimension_score(data_persona_GPT4, Agreeableness)
data_persona_GPT4$Conscientiousness <- calculate_dimension_score(data_persona_GPT4, Conscientiousness)
data_persona_GPT4$Neuroticism <- calculate_dimension_score(data_persona_GPT4, Neuroticism)
data_persona_GPT4$Openness <- calculate_dimension_score(data_persona_GPT4, Openness)

dimension_scores <- data_persona_GPT4[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
write.csv(dimension_scores, "data_persona_GPT4_correlation_domain.csv", row.names = FALSE)

correlation_matrix_persona_GPT4 <- cor(dimension_scores)
print(correlation_matrix_persona_GPT4)


# shape_GPT4
data_shape_GPT4$Extraversion <- calculate_dimension_score(data_shape_GPT4, Extraversion) 
data_shape_GPT4$Agreeableness <- calculate_dimension_score(data_shape_GPT4, Agreeableness)
data_shape_GPT4$Conscientiousness <- calculate_dimension_score(data_shape_GPT4, Conscientiousness)
data_shape_GPT4$Neuroticism <- calculate_dimension_score(data_shape_GPT4, Neuroticism)
data_shape_GPT4$Openness <- calculate_dimension_score(data_shape_GPT4, Openness)

dimension_scores <- data_shape_GPT4[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
write.csv(dimension_scores, "data_shape_GPT4_correlation_domain.csv", row.names = FALSE)

correlation_matrix_shape_GPT4 <- cor(dimension_scores)
print(correlation_matrix_shape_GPT4)


# persona_LLaMA3
data_persona_LLaMA3$Extraversion <- calculate_dimension_score(data_persona_LLaMA3, Extraversion) 
data_persona_LLaMA3$Agreeableness <- calculate_dimension_score(data_persona_LLaMA3, Agreeableness)
data_persona_LLaMA3$Conscientiousness <- calculate_dimension_score(data_persona_LLaMA3, Conscientiousness)
data_persona_LLaMA3$Neuroticism <- calculate_dimension_score(data_persona_LLaMA3, Neuroticism)
data_persona_LLaMA3$Openness <- calculate_dimension_score(data_persona_LLaMA3, Openness)

dimension_scores <- data_persona_LLaMA3[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
write.csv(dimension_scores, "data_persona_LLaMA3_correlation_domain.csv", row.names = FALSE)

correlation_matrix_persona_LLaMA3 <- cor(dimension_scores)
print(correlation_matrix_persona_LLaMA3)


# shape_LLaMA3
data_shape_LLaMA3$Extraversion <- calculate_dimension_score(data_shape_LLaMA3, Extraversion) 
data_shape_LLaMA3$Agreeableness <- calculate_dimension_score(data_shape_LLaMA3, Agreeableness)
data_shape_LLaMA3$Conscientiousness <- calculate_dimension_score(data_shape_LLaMA3, Conscientiousness)
data_shape_LLaMA3$Neuroticism <- calculate_dimension_score(data_shape_LLaMA3, Neuroticism)
data_shape_LLaMA3$Openness <- calculate_dimension_score(data_shape_LLaMA3, Openness)

dimension_scores <- data_shape_LLaMA3[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
write.csv(dimension_scores, "data_shape_LLaMA3_correlation_domain.csv", row.names = FALSE)

correlation_matrix_shape_LLaMA3 <- cor(dimension_scores)
print(correlation_matrix_shape_LLaMA3)


##### facet

# normal_human
data_normal_human$Sociability <- calculate_dimension_score(data_normal_human, Sociability)
data_normal_human$Assertiveness <- calculate_dimension_score(data_normal_human, Assertiveness)
data_normal_human$Energy_Level <- calculate_dimension_score(data_normal_human, Energy_Level)
data_normal_human$Compassion <- calculate_dimension_score(data_normal_human, Compassion)
data_normal_human$Respectfulness <- calculate_dimension_score(data_normal_human, Respectfulness)
data_normal_human$Trust <- calculate_dimension_score(data_normal_human, Trust)
data_normal_human$Organization <- calculate_dimension_score(data_normal_human, Organization)
data_normal_human$Productiveness <- calculate_dimension_score(data_normal_human, Productiveness)
data_normal_human$Responsibility <- calculate_dimension_score(data_normal_human, Responsibility)
data_normal_human$Anxiety <- calculate_dimension_score(data_normal_human, Anxiety)
data_normal_human$Depression <- calculate_dimension_score(data_normal_human, Depression)
data_normal_human$Emotional_Volatility <- calculate_dimension_score(data_normal_human, Emotional_Volatility)
data_normal_human$Intellectual_Curiosity <- calculate_dimension_score(data_normal_human, Intellectual_Curiosity)
data_normal_human$Aesthetic_Sensitivity <- calculate_dimension_score(data_normal_human, Aesthetic_Sensitivity)
data_normal_human$Creative_Imagination <- calculate_dimension_score(data_normal_human, Creative_Imagination)

dimension_scores <- data_normal_human[, c("Sociability", "Assertiveness", "Energy_Level", "Compassion", "Respectfulness", "Trust",
                                          "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional_Volatility",
                                          "Intellectual_Curiosity", "Aesthetic_Sensitivity", "Creative_Imagination")]

write.csv(dimension_scores, "data_normal_human_correlation_facet.csv", row.names = FALSE)

correlation_matrix_normal_human <- cor(dimension_scores)
print(correlation_matrix_normal_human)


# persona_GPT3.5
data_persona_GPT3.5$Sociability <- calculate_dimension_score(data_persona_GPT3.5, Sociability)
data_persona_GPT3.5$Assertiveness <- calculate_dimension_score(data_persona_GPT3.5, Assertiveness)
data_persona_GPT3.5$Energy_Level <- calculate_dimension_score(data_persona_GPT3.5, Energy_Level)
data_persona_GPT3.5$Compassion <- calculate_dimension_score(data_persona_GPT3.5, Compassion)
data_persona_GPT3.5$Respectfulness <- calculate_dimension_score(data_persona_GPT3.5, Respectfulness)
data_persona_GPT3.5$Trust <- calculate_dimension_score(data_persona_GPT3.5, Trust)
data_persona_GPT3.5$Organization <- calculate_dimension_score(data_persona_GPT3.5, Organization)
data_persona_GPT3.5$Productiveness <- calculate_dimension_score(data_persona_GPT3.5, Productiveness)
data_persona_GPT3.5$Responsibility <- calculate_dimension_score(data_persona_GPT3.5, Responsibility)
data_persona_GPT3.5$Anxiety <- calculate_dimension_score(data_persona_GPT3.5, Anxiety)
data_persona_GPT3.5$Depression <- calculate_dimension_score(data_persona_GPT3.5, Depression)
data_persona_GPT3.5$Emotional_Volatility <- calculate_dimension_score(data_persona_GPT3.5, Emotional_Volatility)
data_persona_GPT3.5$Intellectual_Curiosity <- calculate_dimension_score(data_persona_GPT3.5, Intellectual_Curiosity)
data_persona_GPT3.5$Aesthetic_Sensitivity <- calculate_dimension_score(data_persona_GPT3.5, Aesthetic_Sensitivity)
data_persona_GPT3.5$Creative_Imagination <- calculate_dimension_score(data_persona_GPT3.5, Creative_Imagination)

dimension_scores <- data_persona_GPT3.5[, c("Sociability", "Assertiveness", "Energy_Level", "Compassion", "Respectfulness", "Trust",
                                          "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional_Volatility",
                                          "Intellectual_Curiosity", "Aesthetic_Sensitivity", "Creative_Imagination")]

write.csv(dimension_scores, "data_persona_GPT3.5_correlation_facet.csv", row.names = FALSE)

correlation_matrix_persona_GPT3.5 <- cor(dimension_scores)
print(correlation_matrix_persona_GPT3.5)


# shape_GPT3.5
data_shape_GPT3.5$Sociability <- calculate_dimension_score(data_shape_GPT3.5, Sociability)
data_shape_GPT3.5$Assertiveness <- calculate_dimension_score(data_shape_GPT3.5, Assertiveness)
data_shape_GPT3.5$Energy_Level <- calculate_dimension_score(data_shape_GPT3.5, Energy_Level)
data_shape_GPT3.5$Compassion <- calculate_dimension_score(data_shape_GPT3.5, Compassion)
data_shape_GPT3.5$Respectfulness <- calculate_dimension_score(data_shape_GPT3.5, Respectfulness)
data_shape_GPT3.5$Trust <- calculate_dimension_score(data_shape_GPT3.5, Trust)
data_shape_GPT3.5$Organization <- calculate_dimension_score(data_shape_GPT3.5, Organization)
data_shape_GPT3.5$Productiveness <- calculate_dimension_score(data_shape_GPT3.5, Productiveness)
data_shape_GPT3.5$Responsibility <- calculate_dimension_score(data_shape_GPT3.5, Responsibility)
data_shape_GPT3.5$Anxiety <- calculate_dimension_score(data_shape_GPT3.5, Anxiety)
data_shape_GPT3.5$Depression <- calculate_dimension_score(data_shape_GPT3.5, Depression)
data_shape_GPT3.5$Emotional_Volatility <- calculate_dimension_score(data_shape_GPT3.5, Emotional_Volatility)
data_shape_GPT3.5$Intellectual_Curiosity <- calculate_dimension_score(data_shape_GPT3.5, Intellectual_Curiosity)
data_shape_GPT3.5$Aesthetic_Sensitivity <- calculate_dimension_score(data_shape_GPT3.5, Aesthetic_Sensitivity)
data_shape_GPT3.5$Creative_Imagination <- calculate_dimension_score(data_shape_GPT3.5, Creative_Imagination)

dimension_scores <- data_shape_GPT3.5[, c("Sociability", "Assertiveness", "Energy_Level", "Compassion", "Respectfulness", "Trust",
                                            "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional_Volatility",
                                            "Intellectual_Curiosity", "Aesthetic_Sensitivity", "Creative_Imagination")]

write.csv(dimension_scores, "data_shape_GPT3.5_correlation_facet.csv", row.names = FALSE)

correlation_matrix_shape_GPT3.5 <- cor(dimension_scores)
print(correlation_matrix_shape_GPT3.5)


# persona_GPT4
data_persona_GPT4$Sociability <- calculate_dimension_score(data_persona_GPT4, Sociability)
data_persona_GPT4$Assertiveness <- calculate_dimension_score(data_persona_GPT4, Assertiveness)
data_persona_GPT4$Energy_Level <- calculate_dimension_score(data_persona_GPT4, Energy_Level)
data_persona_GPT4$Compassion <- calculate_dimension_score(data_persona_GPT4, Compassion)
data_persona_GPT4$Respectfulness <- calculate_dimension_score(data_persona_GPT4, Respectfulness)
data_persona_GPT4$Trust <- calculate_dimension_score(data_persona_GPT4, Trust)
data_persona_GPT4$Organization <- calculate_dimension_score(data_persona_GPT4, Organization)
data_persona_GPT4$Productiveness <- calculate_dimension_score(data_persona_GPT4, Productiveness)
data_persona_GPT4$Responsibility <- calculate_dimension_score(data_persona_GPT4, Responsibility)
data_persona_GPT4$Anxiety <- calculate_dimension_score(data_persona_GPT4, Anxiety)
data_persona_GPT4$Depression <- calculate_dimension_score(data_persona_GPT4, Depression)
data_persona_GPT4$Emotional_Volatility <- calculate_dimension_score(data_persona_GPT4, Emotional_Volatility)
data_persona_GPT4$Intellectual_Curiosity <- calculate_dimension_score(data_persona_GPT4, Intellectual_Curiosity)
data_persona_GPT4$Aesthetic_Sensitivity <- calculate_dimension_score(data_persona_GPT4, Aesthetic_Sensitivity)
data_persona_GPT4$Creative_Imagination <- calculate_dimension_score(data_persona_GPT4, Creative_Imagination)

dimension_scores <- data_persona_GPT4[, c("Sociability", "Assertiveness", "Energy_Level", "Compassion", "Respectfulness", "Trust",
                                            "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional_Volatility",
                                            "Intellectual_Curiosity", "Aesthetic_Sensitivity", "Creative_Imagination")]

write.csv(dimension_scores, "data_persona_GPT4_correlation_facet.csv", row.names = FALSE)

correlation_matrix_persona_GPT4 <- cor(dimension_scores)
print(correlation_matrix_persona_GPT4)


# shape_GPT4
data_shape_GPT4$Sociability <- calculate_dimension_score(data_shape_GPT4, Sociability)
data_shape_GPT4$Assertiveness <- calculate_dimension_score(data_shape_GPT4, Assertiveness)
data_shape_GPT4$Energy_Level <- calculate_dimension_score(data_shape_GPT4, Energy_Level)
data_shape_GPT4$Compassion <- calculate_dimension_score(data_shape_GPT4, Compassion)
data_shape_GPT4$Respectfulness <- calculate_dimension_score(data_shape_GPT4, Respectfulness)
data_shape_GPT4$Trust <- calculate_dimension_score(data_shape_GPT4, Trust)
data_shape_GPT4$Organization <- calculate_dimension_score(data_shape_GPT4, Organization)
data_shape_GPT4$Productiveness <- calculate_dimension_score(data_shape_GPT4, Productiveness)
data_shape_GPT4$Responsibility <- calculate_dimension_score(data_shape_GPT4, Responsibility)
data_shape_GPT4$Anxiety <- calculate_dimension_score(data_shape_GPT4, Anxiety)
data_shape_GPT4$Depression <- calculate_dimension_score(data_shape_GPT4, Depression)
data_shape_GPT4$Emotional_Volatility <- calculate_dimension_score(data_shape_GPT4, Emotional_Volatility)
data_shape_GPT4$Intellectual_Curiosity <- calculate_dimension_score(data_shape_GPT4, Intellectual_Curiosity)
data_shape_GPT4$Aesthetic_Sensitivity <- calculate_dimension_score(data_shape_GPT4, Aesthetic_Sensitivity)
data_shape_GPT4$Creative_Imagination <- calculate_dimension_score(data_shape_GPT4, Creative_Imagination)

dimension_scores <- data_shape_GPT4[, c("Sociability", "Assertiveness", "Energy_Level", "Compassion", "Respectfulness", "Trust",
                                            "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional_Volatility",
                                            "Intellectual_Curiosity", "Aesthetic_Sensitivity", "Creative_Imagination")]

write.csv(dimension_scores, "data_shape_GPT4_correlation_facet.csv", row.names = FALSE)

correlation_matrix_shape_GPT4 <- cor(dimension_scores)
print(correlation_matrix_shape_GPT4)


# persona_LLaMA3
data_persona_LLaMA3$Sociability <- calculate_dimension_score(data_persona_LLaMA3, Sociability)
data_persona_LLaMA3$Assertiveness <- calculate_dimension_score(data_persona_LLaMA3, Assertiveness)
data_persona_LLaMA3$Energy_Level <- calculate_dimension_score(data_persona_LLaMA3, Energy_Level)
data_persona_LLaMA3$Compassion <- calculate_dimension_score(data_persona_LLaMA3, Compassion)
data_persona_LLaMA3$Respectfulness <- calculate_dimension_score(data_persona_LLaMA3, Respectfulness)
data_persona_LLaMA3$Trust <- calculate_dimension_score(data_persona_LLaMA3, Trust)
data_persona_LLaMA3$Organization <- calculate_dimension_score(data_persona_LLaMA3, Organization)
data_persona_LLaMA3$Productiveness <- calculate_dimension_score(data_persona_LLaMA3, Productiveness)
data_persona_LLaMA3$Responsibility <- calculate_dimension_score(data_persona_LLaMA3, Responsibility)
data_persona_LLaMA3$Anxiety <- calculate_dimension_score(data_persona_LLaMA3, Anxiety)
data_persona_LLaMA3$Depression <- calculate_dimension_score(data_persona_LLaMA3, Depression)
data_persona_LLaMA3$Emotional_Volatility <- calculate_dimension_score(data_persona_LLaMA3, Emotional_Volatility)
data_persona_LLaMA3$Intellectual_Curiosity <- calculate_dimension_score(data_persona_LLaMA3, Intellectual_Curiosity)
data_persona_LLaMA3$Aesthetic_Sensitivity <- calculate_dimension_score(data_persona_LLaMA3, Aesthetic_Sensitivity)
data_persona_LLaMA3$Creative_Imagination <- calculate_dimension_score(data_persona_LLaMA3, Creative_Imagination)

dimension_scores <- data_persona_LLaMA3[, c("Sociability", "Assertiveness", "Energy_Level", "Compassion", "Respectfulness", "Trust",
                                        "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional_Volatility",
                                        "Intellectual_Curiosity", "Aesthetic_Sensitivity", "Creative_Imagination")]

write.csv(dimension_scores, "data_persona_LLaMA3_correlation_facet.csv", row.names = FALSE)

correlation_matrix_persona_LLaMA3 <- cor(dimension_scores)
print(correlation_matrix_persona_LLaMA3)


# shape_LLaMA3
data_shape_LLaMA3$Sociability <- calculate_dimension_score(data_shape_LLaMA3, Sociability)
data_shape_LLaMA3$Assertiveness <- calculate_dimension_score(data_shape_LLaMA3, Assertiveness)
data_shape_LLaMA3$Energy_Level <- calculate_dimension_score(data_shape_LLaMA3, Energy_Level)
data_shape_LLaMA3$Compassion <- calculate_dimension_score(data_shape_LLaMA3, Compassion)
data_shape_LLaMA3$Respectfulness <- calculate_dimension_score(data_shape_LLaMA3, Respectfulness)
data_shape_LLaMA3$Trust <- calculate_dimension_score(data_shape_LLaMA3, Trust)
data_shape_LLaMA3$Organization <- calculate_dimension_score(data_shape_LLaMA3, Organization)
data_shape_LLaMA3$Productiveness <- calculate_dimension_score(data_shape_LLaMA3, Productiveness)
data_shape_LLaMA3$Responsibility <- calculate_dimension_score(data_shape_LLaMA3, Responsibility)
data_shape_LLaMA3$Anxiety <- calculate_dimension_score(data_shape_LLaMA3, Anxiety)
data_shape_LLaMA3$Depression <- calculate_dimension_score(data_shape_LLaMA3, Depression)
data_shape_LLaMA3$Emotional_Volatility <- calculate_dimension_score(data_shape_LLaMA3, Emotional_Volatility)
data_shape_LLaMA3$Intellectual_Curiosity <- calculate_dimension_score(data_shape_LLaMA3, Intellectual_Curiosity)
data_shape_LLaMA3$Aesthetic_Sensitivity <- calculate_dimension_score(data_shape_LLaMA3, Aesthetic_Sensitivity)
data_shape_LLaMA3$Creative_Imagination <- calculate_dimension_score(data_shape_LLaMA3, Creative_Imagination)

dimension_scores <- data_shape_LLaMA3[, c("Sociability", "Assertiveness", "Energy_Level", "Compassion", "Respectfulness", "Trust",
                                            "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional_Volatility",
                                            "Intellectual_Curiosity", "Aesthetic_Sensitivity", "Creative_Imagination")]

write.csv(dimension_scores, "data_shape_LLaMA3_correlation_facet.csv", row.names = FALSE)

correlation_matrix_shape_LLaMA3 <- cor(dimension_scores)
print(correlation_matrix_shape_LLaMA3)

