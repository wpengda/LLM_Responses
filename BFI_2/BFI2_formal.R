# Pengda Wang

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





#### Item level result
item.results.normal_human <- describeBy(data_normal_human)
item.results.old_people <- describeBy(data_old_people)

item.results.persona_GPT3.5 <- describeBy(data_persona_GPT3.5)
item.results.shape_GPT3.5 <- describeBy(data_shape_GPT3.5)

item.results.persona_GPT4 <- describeBy(data_persona_GPT4)
item.results.shape_GPT4 <- describeBy(data_shape_GPT4)

item.results.persona_LLaMA3 <- describeBy(data_persona_LLaMA3)
item.results.shape_LLaMA3 <- describeBy(data_shape_LLaMA3)

table.item <- data.frame(normal_human.mean = item.results.normal_human[,c("mean")],
                         old_people.mean = item.results.old_people[,c("mean")],
                         
                         persona_GPT3.5.mean = item.results.persona_GPT3.5[,c("mean")],
                         shape_GPT3.5.mean = item.results.shape_GPT3.5[,c("mean")],
                         
                         persona_GPT4.mean = item.results.persona_GPT4[,c("mean")],
                         shape_GPT4.mean = item.results.shape_GPT4[,c("mean")],
                         
                         persona_LLaMA3.mean = item.results.persona_LLaMA3[,c("mean")],
                         shape_LLaMA3.mean = item.results.shape_LLaMA3[,c("mean")],
                         
                         normal_human.sd = item.results.normal_human[,c("sd")],
                         old_people.sd = item.results.old_people[,c("sd")],
                         
                         persona_GPT3.5.sd = item.results.persona_GPT3.5[,c("sd")],
                         shape_GPT3.5.sd = item.results.shape_GPT3.5[,c("sd")],
                         
                         persona_GPT4.sd = item.results.persona_GPT4[,c("sd")],
                         shape_GPT4.sd = item.results.shape_GPT4[,c("sd")],
                         persona_LLaMA3.sd = item.results.persona_LLaMA3[,c("sd")],
                         shape_LLaMA3.sd = item.results.shape_LLaMA3[,c("sd")]
)

# write in csv file
# write.table(table.item,"table.item.csv",sep = ",")




#### Cronbach's alpha
calculate_alpha <- function(data) {
  # check variance
  if (any(apply(data, 2, var, na.rm = TRUE) == 0)) {
    return("NA - no variance")
  }
  
  # calculate Cronbach's alpha without checking for warnings
  alpha_result <- tryCatch({
    a_result <- psych::alpha(data)
    return(as.character(a_result$total$raw_alpha))
  }, error = function(e) {
    # deal with error
    return("NA - calculation error")
  })
  
  return(alpha_result)
}

# domain
alpha_extraversion_normal_human <- calculate_alpha(data_normal_human[, Extraversion])
alpha_agreeableness_normal_human <- calculate_alpha(data_normal_human[, Agreeableness])
alpha_conscientiousness_normal_human <- calculate_alpha(data_normal_human[, Conscientiousness])
alpha_neuroticism_normal_human <- calculate_alpha(data_normal_human[, Neuroticism])
alpha_openness_normal_human <- calculate_alpha(data_normal_human[, Openness])

alpha_extraversion_old_people <- calculate_alpha(data_old_people[, Extraversion])
alpha_agreeableness_old_people <- calculate_alpha(data_old_people[, Agreeableness])
alpha_conscientiousness_old_people <- calculate_alpha(data_old_people[, Conscientiousness])
alpha_neuroticism_old_people <- calculate_alpha(data_old_people[, Neuroticism])
alpha_openness_old_people <- calculate_alpha(data_old_people[, Openness])

alpha_extraversion_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Extraversion])
alpha_agreeableness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Agreeableness])
alpha_conscientiousness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Conscientiousness])
alpha_neuroticism_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Neuroticism])
alpha_openness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Openness])

alpha_extraversion_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Extraversion])
alpha_agreeableness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Agreeableness])
alpha_conscientiousness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Conscientiousness])
alpha_neuroticism_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Neuroticism])
alpha_openness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Openness])

alpha_extraversion_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Extraversion])
alpha_agreeableness_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Agreeableness])
alpha_conscientiousness_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Conscientiousness])
alpha_neuroticism_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Neuroticism])
alpha_openness_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Openness])

alpha_extraversion_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Extraversion])
alpha_agreeableness_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Agreeableness])
alpha_conscientiousness_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Conscientiousness])
alpha_neuroticism_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Neuroticism])
alpha_openness_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Openness])

alpha_extraversion_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Extraversion])
alpha_agreeableness_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Agreeableness])
alpha_conscientiousness_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Conscientiousness])
alpha_neuroticism_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Neuroticism])
alpha_openness_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Openness])

alpha_extraversion_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Extraversion])
alpha_agreeableness_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Agreeableness])
alpha_conscientiousness_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Conscientiousness])
alpha_neuroticism_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Neuroticism])
alpha_openness_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Openness])


# facet
alpha_sociability_normal_human <- calculate_alpha(data_normal_human[, Sociability])
alpha_assertiveness_normal_human <- calculate_alpha(data_normal_human[, Assertiveness])
alpha_energy_level_normal_human <- calculate_alpha(data_normal_human[, Energy_Level])
alpha_compassion_normal_human <- calculate_alpha(data_normal_human[, Compassion])
alpha_respectfulness_normal_human <- calculate_alpha(data_normal_human[, Respectfulness])
alpha_trust_normal_human <- calculate_alpha(data_normal_human[, Trust])
alpha_organization_normal_human <- calculate_alpha(data_normal_human[, Organization])
alpha_productiveness_normal_human <- calculate_alpha(data_normal_human[, Productiveness])
alpha_responsibility_normal_human <- calculate_alpha(data_normal_human[, Responsibility])
alpha_anxiety_normal_human <- calculate_alpha(data_normal_human[, Anxiety])
alpha_depression_normal_human <- calculate_alpha(data_normal_human[, Depression])
alpha_emotional_volatility_normal_human<- calculate_alpha(data_normal_human[, Emotional_Volatility])
alpha_intellectual_curiosity_normal_human <- calculate_alpha(data_normal_human[, Intellectual_Curiosity])
alpha_aesthetic_sensitivity_normal_human <- calculate_alpha(data_normal_human[, Aesthetic_Sensitivity])
alpha_creative_imagination_normal_human <- calculate_alpha(data_normal_human[, Creative_Imagination])

alpha_sociability_old_people <- calculate_alpha(data_old_people[, Sociability])
alpha_assertiveness_old_people <- calculate_alpha(data_old_people[, Assertiveness])
alpha_energy_level_old_people <- calculate_alpha(data_old_people[, Energy_Level])
alpha_compassion_old_people <- calculate_alpha(data_old_people[, Compassion])
alpha_respectfulness_old_people <- calculate_alpha(data_old_people[, Respectfulness])
alpha_trust_old_people <- calculate_alpha(data_old_people[, Trust])
alpha_organization_old_people <- calculate_alpha(data_old_people[, Organization])
alpha_productiveness_old_people <- calculate_alpha(data_old_people[, Productiveness])
alpha_responsibility_old_people <- calculate_alpha(data_old_people[, Responsibility])
alpha_anxiety_old_people <- calculate_alpha(data_old_people[, Anxiety])
alpha_depression_old_people <- calculate_alpha(data_old_people[, Depression])
alpha_emotional_volatility_old_people<- calculate_alpha(data_old_people[, Emotional_Volatility])
alpha_intellectual_curiosity_old_people <- calculate_alpha(data_old_people[, Intellectual_Curiosity])
alpha_aesthetic_sensitivity_old_people <- calculate_alpha(data_old_people[, Aesthetic_Sensitivity])
alpha_creative_imagination_old_people <- calculate_alpha(data_old_people[, Creative_Imagination])

alpha_sociability_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Sociability])
alpha_assertiveness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Assertiveness])
alpha_energy_level_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Energy_Level])
alpha_compassion_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Compassion])
alpha_respectfulness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Respectfulness])
alpha_trust_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Trust])
alpha_organization_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Organization])
alpha_productiveness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Productiveness])
alpha_responsibility_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Responsibility])
alpha_anxiety_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Anxiety])
alpha_depression_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Depression])
alpha_emotional_volatility_persona_GPT3.5<- calculate_alpha(data_persona_GPT3.5[, Emotional_Volatility])
alpha_intellectual_curiosity_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Intellectual_Curiosity])
alpha_aesthetic_sensitivity_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Aesthetic_Sensitivity])
alpha_creative_imagination_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Creative_Imagination])

alpha_sociability_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Sociability])
alpha_assertiveness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Assertiveness])
alpha_energy_level_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Energy_Level])
alpha_compassion_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Compassion])
alpha_respectfulness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Respectfulness])
alpha_trust_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Trust])
alpha_organization_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Organization])
alpha_productiveness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Productiveness])
alpha_responsibility_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Responsibility])
alpha_anxiety_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Anxiety])
alpha_depression_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Depression])
alpha_emotional_volatility_shape_GPT3.5<- calculate_alpha(data_shape_GPT3.5[, Emotional_Volatility])
alpha_intellectual_curiosity_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Intellectual_Curiosity])
alpha_aesthetic_sensitivity_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Aesthetic_Sensitivity])
alpha_creative_imagination_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Creative_Imagination])

alpha_sociability_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Sociability])
alpha_assertiveness_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Assertiveness])
alpha_energy_level_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Energy_Level])
alpha_compassion_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Compassion])
alpha_respectfulness_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Respectfulness])
alpha_trust_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Trust])
alpha_organization_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Organization])
alpha_productiveness_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Productiveness])
alpha_responsibility_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Responsibility])
alpha_anxiety_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Anxiety])
alpha_depression_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Depression])
alpha_emotional_volatility_persona_GPT4<- calculate_alpha(data_persona_GPT4[, Emotional_Volatility])
alpha_intellectual_curiosity_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Intellectual_Curiosity])
alpha_aesthetic_sensitivity_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Aesthetic_Sensitivity])
alpha_creative_imagination_persona_GPT4 <- calculate_alpha(data_persona_GPT4[, Creative_Imagination])

alpha_sociability_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Sociability])
alpha_assertiveness_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Assertiveness])
alpha_energy_level_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Energy_Level])
alpha_compassion_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Compassion])
alpha_respectfulness_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Respectfulness])
alpha_trust_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Trust])
alpha_organization_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Organization])
alpha_productiveness_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Productiveness])
alpha_responsibility_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Responsibility])
alpha_anxiety_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Anxiety])
alpha_depression_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Depression])
alpha_emotional_volatility_shape_GPT4<- calculate_alpha(data_shape_GPT4[, Emotional_Volatility])
alpha_intellectual_curiosity_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Intellectual_Curiosity])
alpha_aesthetic_sensitivity_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Aesthetic_Sensitivity])
alpha_creative_imagination_shape_GPT4 <- calculate_alpha(data_shape_GPT4[, Creative_Imagination])

alpha_sociability_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Sociability])
alpha_assertiveness_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Assertiveness])
alpha_energy_level_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Energy_Level])
alpha_compassion_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Compassion])
alpha_respectfulness_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Respectfulness])
alpha_trust_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Trust])
alpha_organization_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Organization])
alpha_productiveness_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Productiveness])
alpha_responsibility_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Responsibility])
alpha_anxiety_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Anxiety])
alpha_depression_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Depression])
alpha_emotional_volatility_persona_LLaMA3<- calculate_alpha(data_persona_LLaMA3[, Emotional_Volatility])
alpha_intellectual_curiosity_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Intellectual_Curiosity])
alpha_aesthetic_sensitivity_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Aesthetic_Sensitivity])
alpha_creative_imagination_persona_LLaMA3 <- calculate_alpha(data_persona_LLaMA3[, Creative_Imagination])

alpha_sociability_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Sociability])
alpha_assertiveness_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Assertiveness])
alpha_energy_level_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Energy_Level])
alpha_compassion_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Compassion])
alpha_respectfulness_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Respectfulness])
alpha_trust_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Trust])
alpha_organization_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Organization])
alpha_productiveness_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Productiveness])
alpha_responsibility_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Responsibility])
alpha_anxiety_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Anxiety])
alpha_depression_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Depression])
alpha_emotional_volatility_shape_LLaMA3<- calculate_alpha(data_shape_LLaMA3[, Emotional_Volatility])
alpha_intellectual_curiosity_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Intellectual_Curiosity])
alpha_aesthetic_sensitivity_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Aesthetic_Sensitivity])
alpha_creative_imagination_shape_LLaMA3 <- calculate_alpha(data_shape_LLaMA3[, Creative_Imagination])

result_df <- data.frame(
  Trait = c("Sociability", "Assertiveness", "Energy Level", "Compassion", "Respectfulness",
            "Trust", "Organization", "Productiveness", "Responsibility", "Anxiety",
            "Depression", "Emotional Volatility", "Intellectual Curiosity", "Aesthetic Sensitivity",
            "Creative Imagination"),
  normal_human = c(alpha_sociability_normal_human, alpha_assertiveness_normal_human, alpha_energy_level_normal_human,
                 alpha_compassion_normal_human, alpha_respectfulness_normal_human, alpha_trust_normal_human,
                 alpha_organization_normal_human, alpha_productiveness_normal_human, alpha_responsibility_normal_human,
                 alpha_anxiety_normal_human, alpha_depression_normal_human, alpha_emotional_volatility_normal_human,
                 alpha_intellectual_curiosity_normal_human, alpha_aesthetic_sensitivity_normal_human,
                 alpha_creative_imagination_normal_human),
  old_people = c(alpha_sociability_old_people, alpha_assertiveness_old_people, alpha_energy_level_old_people,
            alpha_compassion_old_people, alpha_respectfulness_old_people, alpha_trust_old_people,
            alpha_organization_old_people, alpha_productiveness_old_people, alpha_responsibility_old_people,
            alpha_anxiety_old_people, alpha_depression_old_people, alpha_emotional_volatility_old_people,
            alpha_intellectual_curiosity_old_people, alpha_aesthetic_sensitivity_old_people,
            alpha_creative_imagination_old_people),
  persona_GPT3.5 = c(alpha_sociability_persona_GPT3.5, alpha_assertiveness_persona_GPT3.5, alpha_energy_level_persona_GPT3.5,
              alpha_compassion_persona_GPT3.5, alpha_respectfulness_persona_GPT3.5, alpha_trust_persona_GPT3.5,
              alpha_organization_persona_GPT3.5, alpha_productiveness_persona_GPT3.5, alpha_responsibility_persona_GPT3.5,
              alpha_anxiety_persona_GPT3.5, alpha_depression_persona_GPT3.5, alpha_emotional_volatility_persona_GPT3.5,
              alpha_intellectual_curiosity_persona_GPT3.5, alpha_aesthetic_sensitivity_persona_GPT3.5,
              alpha_creative_imagination_persona_GPT3.5),
  shape_GPT3.5 = c(alpha_sociability_shape_GPT3.5, alpha_assertiveness_shape_GPT3.5, alpha_energy_level_shape_GPT3.5,
                     alpha_compassion_shape_GPT3.5, alpha_respectfulness_shape_GPT3.5, alpha_trust_shape_GPT3.5,
                     alpha_organization_shape_GPT3.5, alpha_productiveness_shape_GPT3.5, alpha_responsibility_shape_GPT3.5,
                     alpha_anxiety_shape_GPT3.5, alpha_depression_shape_GPT3.5, alpha_emotional_volatility_shape_GPT3.5,
                     alpha_intellectual_curiosity_shape_GPT3.5, alpha_aesthetic_sensitivity_shape_GPT3.5,
                     alpha_creative_imagination_shape_GPT3.5),
  persona_GPT4 = c(alpha_sociability_persona_GPT4, alpha_assertiveness_persona_GPT4, alpha_energy_level_persona_GPT4,
                     alpha_compassion_persona_GPT4, alpha_respectfulness_persona_GPT4, alpha_trust_persona_GPT4,
                     alpha_organization_persona_GPT4, alpha_productiveness_persona_GPT4, alpha_responsibility_persona_GPT4,
                     alpha_anxiety_persona_GPT4, alpha_depression_persona_GPT4, alpha_emotional_volatility_persona_GPT4,
                     alpha_intellectual_curiosity_persona_GPT4, alpha_aesthetic_sensitivity_persona_GPT4,
                     alpha_creative_imagination_persona_GPT4),
  shape_GPT4 = c(alpha_sociability_shape_GPT4, alpha_assertiveness_shape_GPT4, alpha_energy_level_shape_GPT4,
                   alpha_compassion_shape_GPT4, alpha_respectfulness_shape_GPT4, alpha_trust_shape_GPT4,
                   alpha_organization_shape_GPT4, alpha_productiveness_shape_GPT4, alpha_responsibility_shape_GPT4,
                   alpha_anxiety_shape_GPT4, alpha_depression_shape_GPT4, alpha_emotional_volatility_shape_GPT4,
                   alpha_intellectual_curiosity_shape_GPT4, alpha_aesthetic_sensitivity_shape_GPT4,
                   alpha_creative_imagination_shape_GPT4),
  persona_LLaMA3 = c(alpha_sociability_persona_LLaMA3, alpha_assertiveness_persona_LLaMA3, alpha_energy_level_persona_LLaMA3,
                   alpha_compassion_persona_LLaMA3, alpha_respectfulness_persona_LLaMA3, alpha_trust_persona_LLaMA3,
                   alpha_organization_persona_LLaMA3, alpha_productiveness_persona_LLaMA3, alpha_responsibility_persona_LLaMA3,
                   alpha_anxiety_persona_LLaMA3, alpha_depression_persona_LLaMA3, alpha_emotional_volatility_persona_LLaMA3,
                   alpha_intellectual_curiosity_persona_LLaMA3, alpha_aesthetic_sensitivity_persona_LLaMA3,
                   alpha_creative_imagination_persona_LLaMA3),
  shape_LLaMA3 = c(alpha_sociability_shape_LLaMA3, alpha_assertiveness_shape_LLaMA3, alpha_energy_level_shape_LLaMA3,
                 alpha_compassion_shape_LLaMA3, alpha_respectfulness_shape_LLaMA3, alpha_trust_shape_LLaMA3,
                 alpha_organization_shape_LLaMA3, alpha_productiveness_shape_LLaMA3, alpha_responsibility_shape_LLaMA3,
                 alpha_anxiety_shape_LLaMA3, alpha_depression_shape_LLaMA3, alpha_emotional_volatility_shape_LLaMA3,
                 alpha_intellectual_curiosity_shape_LLaMA3, alpha_aesthetic_sensitivity_shape_LLaMA3,
                 alpha_creative_imagination_shape_LLaMA3)
  
)

# Write the data frame to a CSV file
# write.table(result_df, "alpha.csv", sep = ",", row.names = FALSE)



#### Facet mean and sd
# normal_human
normal_human_facet_results <- data.frame(
  Trait = c("Sociability", "Assertiveness", "Energy Level", "Compassion", "Respectfulness", "Trust",
            "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional Volatility",
            "Intellectual Curiosity", "Aesthetic Sensitivity", "Creative Imagination"),
  Mean = c(
    mean(rowMeans(data_normal_human[, Sociability], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Assertiveness], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Energy_Level], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Compassion], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Respectfulness], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Trust], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Organization], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Productiveness], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Responsibility], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Anxiety], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Depression], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Emotional_Volatility], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Intellectual_Curiosity], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Aesthetic_Sensitivity], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Creative_Imagination], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_normal_human[, Sociability], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Assertiveness], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Energy_Level], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Compassion], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Respectfulness], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Trust], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Organization], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Productiveness], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Responsibility], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Anxiety], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Depression], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Emotional_Volatility], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Intellectual_Curiosity], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Aesthetic_Sensitivity], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Creative_Imagination], na.rm = TRUE))
  )
)

# write in csv file
# write.table(normal_human_facet_results,"normal_human_facet_results.csv",sep = ",")


# old_people
old_people_facet_results <- data.frame(
  Trait = c("Sociability", "Assertiveness", "Energy Level", "Compassion", "Respectfulness", "Trust",
            "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional Volatility",
            "Intellectual Curiosity", "Aesthetic Sensitivity", "Creative Imagination"),
  Mean = c(
    mean(rowMeans(data_old_people[, Sociability], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Assertiveness], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Energy_Level], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Compassion], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Respectfulness], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Trust], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Organization], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Productiveness], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Responsibility], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Anxiety], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Depression], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Emotional_Volatility], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Intellectual_Curiosity], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Aesthetic_Sensitivity], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Creative_Imagination], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_old_people[, Sociability], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Assertiveness], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Energy_Level], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Compassion], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Respectfulness], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Trust], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Organization], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Productiveness], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Responsibility], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Anxiety], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Depression], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Emotional_Volatility], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Intellectual_Curiosity], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Aesthetic_Sensitivity], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Creative_Imagination], na.rm = TRUE))
  )
)

# write in csv file
#write.table(old_people_facet_results,"old_people_facet_results.csv",sep = ",")


# persona_GPT3.5
persona_GPT3.5_facet_results <- data.frame(
  Trait = c("Sociability", "Assertiveness", "Energy Level", "Compassion", "Respectfulness", "Trust",
            "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional Volatility",
            "Intellectual Curiosity", "Aesthetic Sensitivity", "Creative Imagination"),
  Mean = c(
    mean(rowMeans(data_persona_GPT3.5[, Sociability], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Assertiveness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Energy_Level], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Compassion], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Respectfulness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Trust], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Organization], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Productiveness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Responsibility], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Anxiety], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Depression], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Emotional_Volatility], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Intellectual_Curiosity], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Aesthetic_Sensitivity], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Creative_Imagination], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_persona_GPT3.5[, Sociability], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Assertiveness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Energy_Level], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Compassion], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Respectfulness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Trust], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Organization], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Productiveness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Responsibility], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Anxiety], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Depression], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Emotional_Volatility], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Intellectual_Curiosity], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Aesthetic_Sensitivity], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Creative_Imagination], na.rm = TRUE))
  )
)

# write in csv file
#write.table(persona_GPT3.5_facet_results,"persona_GPT3.5_facet_results.csv",sep = ",")


# shape_GPT3.5
shape_GPT3.5_facet_results <- data.frame(
  Trait = c("Sociability", "Assertiveness", "Energy Level", "Compassion", "Respectfulness", "Trust",
            "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional Volatility",
            "Intellectual Curiosity", "Aesthetic Sensitivity", "Creative Imagination"),
  Mean = c(
    mean(rowMeans(data_shape_GPT3.5[, Sociability], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Assertiveness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Energy_Level], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Compassion], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Respectfulness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Trust], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Organization], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Productiveness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Responsibility], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Anxiety], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Depression], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Emotional_Volatility], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Intellectual_Curiosity], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Aesthetic_Sensitivity], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Creative_Imagination], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_shape_GPT3.5[, Sociability], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Assertiveness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Energy_Level], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Compassion], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Respectfulness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Trust], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Organization], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Productiveness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Responsibility], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Anxiety], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Depression], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Emotional_Volatility], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Intellectual_Curiosity], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Aesthetic_Sensitivity], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Creative_Imagination], na.rm = TRUE))
  )
)

# write in csv file
#write.table(shape_GPT3.5_facet_results,"shape_GPT3.5_facet_results.csv",sep = ",")




# persona_GPT4
persona_GPT4_facet_results <- data.frame(
  Trait = c("Sociability", "Assertiveness", "Energy Level", "Compassion", "Respectfulness", "Trust",
            "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional Volatility",
            "Intellectual Curiosity", "Aesthetic Sensitivity", "Creative Imagination"),
  Mean = c(
    mean(rowMeans(data_persona_GPT4[, Sociability], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Assertiveness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Energy_Level], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Compassion], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Respectfulness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Trust], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Organization], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Productiveness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Responsibility], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Anxiety], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Depression], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Emotional_Volatility], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Intellectual_Curiosity], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Aesthetic_Sensitivity], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Creative_Imagination], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_persona_GPT4[, Sociability], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Assertiveness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Energy_Level], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Compassion], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Respectfulness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Trust], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Organization], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Productiveness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Responsibility], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Anxiety], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Depression], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Emotional_Volatility], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Intellectual_Curiosity], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Aesthetic_Sensitivity], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Creative_Imagination], na.rm = TRUE))
  )
)

# write in csv file
#write.table(persona_GPT4_facet_results,"persona_GPT4_facet_results.csv",sep = ",")


# shape_GPT4
shape_GPT4_facet_results <- data.frame(
  Trait = c("Sociability", "Assertiveness", "Energy Level", "Compassion", "Respectfulness", "Trust",
            "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional Volatility",
            "Intellectual Curiosity", "Aesthetic Sensitivity", "Creative Imagination"),
  Mean = c(
    mean(rowMeans(data_shape_GPT4[, Sociability], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Assertiveness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Energy_Level], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Compassion], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Respectfulness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Trust], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Organization], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Productiveness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Responsibility], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Anxiety], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Depression], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Emotional_Volatility], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Intellectual_Curiosity], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Aesthetic_Sensitivity], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Creative_Imagination], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_shape_GPT4[, Sociability], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Assertiveness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Energy_Level], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Compassion], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Respectfulness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Trust], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Organization], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Productiveness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Responsibility], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Anxiety], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Depression], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Emotional_Volatility], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Intellectual_Curiosity], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Aesthetic_Sensitivity], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Creative_Imagination], na.rm = TRUE))
  )
)

# write in csv file
#write.table(shape_GPT4_facet_results,"shape_GPT4_facet_results.csv",sep = ",")


# persona_LLaMA3
persona_LLaMA3_facet_results <- data.frame(
  Trait = c("Sociability", "Assertiveness", "Energy Level", "Compassion", "Respectfulness", "Trust",
            "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional Volatility",
            "Intellectual Curiosity", "Aesthetic Sensitivity", "Creative Imagination"),
  Mean = c(
    mean(rowMeans(data_persona_LLaMA3[, Sociability], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Assertiveness], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Energy_Level], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Compassion], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Respectfulness], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Trust], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Organization], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Productiveness], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Responsibility], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Anxiety], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Depression], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Emotional_Volatility], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Intellectual_Curiosity], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Aesthetic_Sensitivity], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Creative_Imagination], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_persona_LLaMA3[, Sociability], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Assertiveness], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Energy_Level], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Compassion], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Respectfulness], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Trust], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Organization], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Productiveness], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Responsibility], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Anxiety], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Depression], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Emotional_Volatility], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Intellectual_Curiosity], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Aesthetic_Sensitivity], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Creative_Imagination], na.rm = TRUE))
  )
)

# write in csv file
#write.table(persona_LLaMA3_facet_results,"persona_LLaMA3_facet_results.csv",sep = ",")


# shape_LLaMA3
shape_LLaMA3_facet_results <- data.frame(
  Trait = c("Sociability", "Assertiveness", "Energy Level", "Compassion", "Respectfulness", "Trust",
            "Organization", "Productiveness", "Responsibility", "Anxiety", "Depression", "Emotional Volatility",
            "Intellectual Curiosity", "Aesthetic Sensitivity", "Creative Imagination"),
  Mean = c(
    mean(rowMeans(data_shape_LLaMA3[, Sociability], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Assertiveness], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Energy_Level], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Compassion], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Respectfulness], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Trust], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Organization], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Productiveness], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Responsibility], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Anxiety], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Depression], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Emotional_Volatility], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Intellectual_Curiosity], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Aesthetic_Sensitivity], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Creative_Imagination], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_shape_LLaMA3[, Sociability], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Assertiveness], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Energy_Level], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Compassion], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Respectfulness], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Trust], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Organization], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Productiveness], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Responsibility], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Anxiety], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Depression], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Emotional_Volatility], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Intellectual_Curiosity], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Aesthetic_Sensitivity], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Creative_Imagination], na.rm = TRUE))
  )
)

# write in csv file
#write.table(shape_LLaMA3_facet_results,"shape_LLaMA3_facet_results.csv",sep = ",")






#### Domain level result
# normal_human
normal_human_scale_results <- data.frame(
  Trait = c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness"),
  Mean = c(
    mean(rowMeans(data_normal_human[, Extraversion], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Agreeableness], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Conscientiousness], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Neuroticism], na.rm = TRUE)),
    mean(rowMeans(data_normal_human[, Openness], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_normal_human[, Extraversion], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Agreeableness], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Conscientiousness], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Neuroticism], na.rm = TRUE)),
    sd(rowMeans(data_normal_human[, Openness], na.rm = TRUE))
  )
)

# write in csv file
#write.table(normal_human_scale_results,"normal_human_scale_results.csv",sep = ",")

# old_people
old_people_scale_results <- data.frame(
  Trait = c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness"),
  Mean = c(
    mean(rowMeans(data_old_people[, Extraversion], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Agreeableness], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Conscientiousness], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Neuroticism], na.rm = TRUE)),
    mean(rowMeans(data_old_people[, Openness], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_old_people[, Extraversion], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Agreeableness], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Conscientiousness], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Neuroticism], na.rm = TRUE)),
    sd(rowMeans(data_old_people[, Openness], na.rm = TRUE))
  )
)

# write in csv file
#write.table(old_people_scale_results,"old_people_scale_results.csv",sep = ",")

# persona_GPT3.5
persona_GPT3.5_scale_results <- data.frame(
  Trait = c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness"),
  Mean = c(
    mean(rowMeans(data_persona_GPT3.5[, Extraversion], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Agreeableness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Conscientiousness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Neuroticism], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Openness], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_persona_GPT3.5[, Extraversion], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Agreeableness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Conscientiousness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Neuroticism], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Openness], na.rm = TRUE))
  )
)

# write in csv file
#write.table(persona_GPT3.5_scale_results,"persona_GPT3.5_scale_results.csv",sep = ",")

# shape_GPT3.5
shape_GPT3.5_scale_results <- data.frame(
  Trait = c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness"),
  Mean = c(
    mean(rowMeans(data_shape_GPT3.5[, Extraversion], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Agreeableness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Conscientiousness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Neuroticism], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Openness], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_shape_GPT3.5[, Extraversion], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Agreeableness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Conscientiousness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Neuroticism], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Openness], na.rm = TRUE))
  )
)

# write in csv file
#write.table(shape_GPT3.5_scale_results,"shape_GPT3.5_scale_results.csv",sep = ",")



# persona_GPT4
persona_GPT4_scale_results <- data.frame(
  Trait = c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness"),
  Mean = c(
    mean(rowMeans(data_persona_GPT4[, Extraversion], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Agreeableness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Conscientiousness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Neuroticism], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT4[, Openness], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_persona_GPT4[, Extraversion], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Agreeableness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Conscientiousness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Neuroticism], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT4[, Openness], na.rm = TRUE))
  )
)

# write in csv file
#write.table(persona_GPT4_scale_results,"persona_GPT4_scale_results.csv",sep = ",")

# shape_GPT4
shape_GPT4_scale_results <- data.frame(
  Trait = c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness"),
  Mean = c(
    mean(rowMeans(data_shape_GPT4[, Extraversion], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Agreeableness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Conscientiousness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Neuroticism], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT4[, Openness], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_shape_GPT4[, Extraversion], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Agreeableness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Conscientiousness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Neuroticism], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT4[, Openness], na.rm = TRUE))
  )
)

# write in csv file
#write.table(shape_GPT4_scale_results,"shape_GPT4_scale_results.csv",sep = ",")




# persona_LLaMA3
persona_LLaMA3_scale_results <- data.frame(
  Trait = c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness"),
  Mean = c(
    mean(rowMeans(data_persona_LLaMA3[, Extraversion], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Agreeableness], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Conscientiousness], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Neuroticism], na.rm = TRUE)),
    mean(rowMeans(data_persona_LLaMA3[, Openness], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_persona_LLaMA3[, Extraversion], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Agreeableness], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Conscientiousness], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Neuroticism], na.rm = TRUE)),
    sd(rowMeans(data_persona_LLaMA3[, Openness], na.rm = TRUE))
  )
)

# write in csv file
#write.table(persona_LLaMA3_scale_results,"persona_LLaMA3_scale_results.csv",sep = ",")

# shape_LLaMA3
shape_LLaMA3_scale_results <- data.frame(
  Trait = c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness"),
  Mean = c(
    mean(rowMeans(data_shape_LLaMA3[, Extraversion], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Agreeableness], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Conscientiousness], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Neuroticism], na.rm = TRUE)),
    mean(rowMeans(data_shape_LLaMA3[, Openness], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_shape_LLaMA3[, Extraversion], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Agreeableness], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Conscientiousness], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Neuroticism], na.rm = TRUE)),
    sd(rowMeans(data_shape_LLaMA3[, Openness], na.rm = TRUE))
  )
)

# write in csv file
#write.table(shape_LLaMA3_scale_results,"shape_LLaMA3_scale_results.csv",sep = ",")





#### scale correlation 
calculate_dimension_score <- function(data, indices) {
  rowMeans(data[, indices], na.rm = TRUE)
}


# normal_human
data_normal_human$Extraversion <- calculate_dimension_score(data_normal_human, Extraversion)
data_normal_human$Agreeableness <- calculate_dimension_score(data_normal_human, Agreeableness)
data_normal_human$Conscientiousness <- calculate_dimension_score(data_normal_human, Conscientiousness)
data_normal_human$Neuroticism <- calculate_dimension_score(data_normal_human, Neuroticism)
data_normal_human$Openness <- calculate_dimension_score(data_normal_human, Openness)

dimension_scores <- data_normal_human[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
correlation_matrix_normal_human <- cor(dimension_scores)

print(correlation_matrix_normal_human)


# old_people
data_old_people$Extraversion <- calculate_dimension_score(data_old_people, Extraversion) 
data_old_people$Agreeableness <- calculate_dimension_score(data_old_people, Agreeableness)
data_old_people$Conscientiousness <- calculate_dimension_score(data_old_people, Conscientiousness)
data_old_people$Neuroticism <- calculate_dimension_score(data_old_people, Neuroticism)
data_old_people$Openness <- calculate_dimension_score(data_old_people, Openness)

dimension_scores <- data_old_people[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
correlation_matrix_old_people <- cor(dimension_scores)

print(correlation_matrix_old_people)


# persona_GPT3.5
data_persona_GPT3.5$Extraversion <- calculate_dimension_score(data_persona_GPT3.5, Extraversion) 
data_persona_GPT3.5$Agreeableness <- calculate_dimension_score(data_persona_GPT3.5, Agreeableness)
data_persona_GPT3.5$Conscientiousness <- calculate_dimension_score(data_persona_GPT3.5, Conscientiousness)
data_persona_GPT3.5$Neuroticism <- calculate_dimension_score(data_persona_GPT3.5, Neuroticism)
data_persona_GPT3.5$Openness <- calculate_dimension_score(data_persona_GPT3.5, Openness)

dimension_scores <- data_persona_GPT3.5[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
correlation_matrix_persona_GPT3.5 <- cor(dimension_scores)

print(correlation_matrix_persona_GPT3.5)


# shape_GPT3.5
data_shape_GPT3.5$Extraversion <- calculate_dimension_score(data_shape_GPT3.5, Extraversion) 
data_shape_GPT3.5$Agreeableness <- calculate_dimension_score(data_shape_GPT3.5, Agreeableness)
data_shape_GPT3.5$Conscientiousness <- calculate_dimension_score(data_shape_GPT3.5, Conscientiousness)
data_shape_GPT3.5$Neuroticism <- calculate_dimension_score(data_shape_GPT3.5, Neuroticism)
data_shape_GPT3.5$Openness <- calculate_dimension_score(data_shape_GPT3.5, Openness)

dimension_scores <- data_shape_GPT3.5[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
correlation_matrix_shape_GPT3.5 <- cor(dimension_scores)

print(correlation_matrix_shape_GPT3.5)



# persona_GPT4
data_persona_GPT4$Extraversion <- calculate_dimension_score(data_persona_GPT4, Extraversion) 
data_persona_GPT4$Agreeableness <- calculate_dimension_score(data_persona_GPT4, Agreeableness)
data_persona_GPT4$Conscientiousness <- calculate_dimension_score(data_persona_GPT4, Conscientiousness)
data_persona_GPT4$Neuroticism <- calculate_dimension_score(data_persona_GPT4, Neuroticism)
data_persona_GPT4$Openness <- calculate_dimension_score(data_persona_GPT4, Openness)

dimension_scores <- data_persona_GPT4[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
correlation_matrix_persona_GPT4 <- cor(dimension_scores)

print(correlation_matrix_persona_GPT4)


# shape_GPT4
data_shape_GPT4$Extraversion <- calculate_dimension_score(data_shape_GPT4, Extraversion) 
data_shape_GPT4$Agreeableness <- calculate_dimension_score(data_shape_GPT4, Agreeableness)
data_shape_GPT4$Conscientiousness <- calculate_dimension_score(data_shape_GPT4, Conscientiousness)
data_shape_GPT4$Neuroticism <- calculate_dimension_score(data_shape_GPT4, Neuroticism)
data_shape_GPT4$Openness <- calculate_dimension_score(data_shape_GPT4, Openness)

dimension_scores <- data_shape_GPT4[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
correlation_matrix_shape_GPT4 <- cor(dimension_scores)

print(correlation_matrix_shape_GPT4)


# persona_LLaMA3
data_persona_LLaMA3$Extraversion <- calculate_dimension_score(data_persona_LLaMA3, Extraversion) 
data_persona_LLaMA3$Agreeableness <- calculate_dimension_score(data_persona_LLaMA3, Agreeableness)
data_persona_LLaMA3$Conscientiousness <- calculate_dimension_score(data_persona_LLaMA3, Conscientiousness)
data_persona_LLaMA3$Neuroticism <- calculate_dimension_score(data_persona_LLaMA3, Neuroticism)
data_persona_LLaMA3$Openness <- calculate_dimension_score(data_persona_LLaMA3, Openness)

dimension_scores <- data_persona_LLaMA3[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
correlation_matrix_persona_LLaMA3 <- cor(dimension_scores)

print(correlation_matrix_persona_LLaMA3)


# shape_LLaMA3
data_shape_LLaMA3$Extraversion <- calculate_dimension_score(data_shape_LLaMA3, Extraversion) 
data_shape_LLaMA3$Agreeableness <- calculate_dimension_score(data_shape_LLaMA3, Agreeableness)
data_shape_LLaMA3$Conscientiousness <- calculate_dimension_score(data_shape_LLaMA3, Conscientiousness)
data_shape_LLaMA3$Neuroticism <- calculate_dimension_score(data_shape_LLaMA3, Neuroticism)
data_shape_LLaMA3$Openness <- calculate_dimension_score(data_shape_LLaMA3, Openness)

dimension_scores <- data_shape_LLaMA3[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")]
correlation_matrix_shape_LLaMA3 <- cor(dimension_scores)

print(correlation_matrix_shape_LLaMA3)






############################# summary #################################

# item mean
item_BFI2_mean <- read.table("Summary_data/item_BFI2_mean.csv",sep = ",", header = T)

# old_people ~ normal_human
#mae_old_people.mean_normal_human <- mean(abs(item_BFI2_mean$normal_human.mean - item_BFI2_mean$old_people.mean))
#profile_correlation_persona_GPT3.5.mean_normal_human <- cor(item_BFI2_mean$normal_human.mean, item_BFI2_mean$old_people.mean)
#mae_old_people.mean_normal_human
#profile_correlation_persona_GPT3.5.mean_normal_human

# persona_GPT3.5 ~ normal_human
mae_persona_GPT3.5.mean_normal_human <- mean(abs(item_BFI2_mean$normal_human.mean - item_BFI2_mean$persona_GPT3.5.mean))
profile_correlation_persona_GPT3.5.mean_normal_human <- cor(item_BFI2_mean$normal_human.mean, item_BFI2_mean$persona_GPT3.5.mean)
mae_persona_GPT3.5.mean_normal_human
profile_correlation_persona_GPT3.5.mean_normal_human

# persona_GPT3.5 ~ old_people
mae_persona_GPT3.5.mean_old_people <- mean(abs(item_BFI2_mean$old_people.mean - item_BFI2_mean$persona_GPT3.5.mean))
profile_correlation_persona_GPT3.5.mean_old_people <- cor(item_BFI2_mean$old_people.mean, item_BFI2_mean$persona_GPT3.5.mean)
mae_persona_GPT3.5.mean_old_people
profile_correlation_persona_GPT3.5.mean_old_people

# shape_GPT3.5 ~ normal_human
mae_shape_GPT3.5.mean_normal_human <- mean(abs(item_BFI2_mean$normal_human.mean - item_BFI2_mean$shape_GPT3.5.mean))
profile_correlation_shape_GPT3.5.mean_normal_human <- cor(item_BFI2_mean$normal_human.mean, item_BFI2_mean$shape_GPT3.5.mean)
mae_shape_GPT3.5.mean_normal_human
profile_correlation_shape_GPT3.5.mean_normal_human

# shape_GPT3.5 ~ old_people
mae_shape_GPT3.5.mean_old_people <- mean(abs(item_BFI2_mean$old_people.mean - item_BFI2_mean$shape_GPT3.5.mean))
profile_correlation_shape_GPT3.5.mean_old_people <- cor(item_BFI2_mean$old_people.mean, item_BFI2_mean$shape_GPT3.5.mean)
mae_shape_GPT3.5.mean_old_people
profile_correlation_shape_GPT3.5.mean_old_people



# persona_GPT4 ~ normal_human
mae_persona_GPT4.mean_normal_human <- mean(abs(item_BFI2_mean$normal_human.mean - item_BFI2_mean$persona_GPT4.mean))
profile_correlation_persona_GPT4.mean_normal_human <- cor(item_BFI2_mean$normal_human.mean, item_BFI2_mean$persona_GPT4.mean)
mae_persona_GPT4.mean_normal_human
profile_correlation_persona_GPT4.mean_normal_human

# persona_GPT4 ~ old_people
mae_persona_GPT4.mean_old_people <- mean(abs(item_BFI2_mean$old_people.mean - item_BFI2_mean$persona_GPT4.mean))
profile_correlation_persona_GPT4.mean_old_people <- cor(item_BFI2_mean$old_people.mean, item_BFI2_mean$persona_GPT4.mean)
mae_persona_GPT4.mean_old_people
profile_correlation_persona_GPT4.mean_old_people

# shape_GPT4 ~ normal_human
mae_shape_GPT4.mean_normal_human <- mean(abs(item_BFI2_mean$normal_human.mean - item_BFI2_mean$shape_GPT4.mean))
profile_correlation_shape_GPT4.mean_normal_human <- cor(item_BFI2_mean$normal_human.mean, item_BFI2_mean$shape_GPT4.mean)
mae_shape_GPT4.mean_normal_human
profile_correlation_shape_GPT4.mean_normal_human

# shape_GPT4 ~ old_people
mae_shape_GPT4.mean_old_people <- mean(abs(item_BFI2_mean$old_people.mean - item_BFI2_mean$shape_GPT4.mean))
profile_correlation_shape_GPT4.mean_old_people <- cor(item_BFI2_mean$old_people.mean, item_BFI2_mean$shape_GPT4.mean)
mae_shape_GPT4.mean_old_people
profile_correlation_shape_GPT4.mean_old_people



# persona_LLaMA3 ~ normal_human
mae_persona_LLaMA3.mean_normal_human <- mean(abs(item_BFI2_mean$normal_human.mean - item_BFI2_mean$persona_LLaMA3.mean))
profile_correlation_persona_LLaMA3.mean_normal_human <- cor(item_BFI2_mean$normal_human.mean, item_BFI2_mean$persona_LLaMA3.mean)
mae_persona_LLaMA3.mean_normal_human
profile_correlation_persona_LLaMA3.mean_normal_human

# persona_LLaMA3 ~ old_people
mae_persona_LLaMA3.mean_old_people <- mean(abs(item_BFI2_mean$old_people.mean - item_BFI2_mean$persona_LLaMA3.mean))
profile_correlation_persona_LLaMA3.mean_old_people <- cor(item_BFI2_mean$old_people.mean, item_BFI2_mean$persona_LLaMA3.mean)
mae_persona_LLaMA3.mean_old_people
profile_correlation_persona_LLaMA3.mean_old_people

# shape_LLaMA3 ~ normal_human
mae_shape_LLaMA3.mean_normal_human <- mean(abs(item_BFI2_mean$normal_human.mean - item_BFI2_mean$shape_LLaMA3.mean))
profile_correlation_shape_LLaMA3.mean_normal_human <- cor(item_BFI2_mean$normal_human.mean, item_BFI2_mean$shape_LLaMA3.mean)
mae_shape_LLaMA3.mean_normal_human
profile_correlation_shape_LLaMA3.mean_normal_human

# shape_LLaMA3 ~ old_people
mae_shape_LLaMA3.mean_old_people <- mean(abs(item_BFI2_mean$old_people.mean - item_BFI2_mean$shape_LLaMA3.mean))
profile_correlation_shape_LLaMA3.mean_old_people <- cor(item_BFI2_mean$old_people.mean, item_BFI2_mean$shape_LLaMA3.mean)
mae_shape_LLaMA3.mean_old_people
profile_correlation_shape_LLaMA3.mean_old_people




# item sd
item_BFI2_sd <- read.table("Summary_data/item_BFI2_sd.csv",sep = ",", header = T)

# old_people ~ normal_human
#mae_old_people.sd_normal_human <- mean(abs(item_BFI2_sd$normal_human.sd - item_BFI2_sd$old_people.sd))
#profile_correlation_old_people.sd_normal_human <- cor(item_BFI2_sd$normal_human.sd, item_BFI2_sd$old_people.sd)
#mae_old_people.sd_normal_human
#profile_correlation_old_people.sd_normal_human

# persona_GPT3.5 ~ normal_human
mae_persona_GPT3.5.sd_normal_human <- mean(abs(item_BFI2_sd$normal_human.sd - item_BFI2_sd$persona_GPT3.5.sd))
profile_correlation_persona_GPT3.5.sd_normal_human <- cor(item_BFI2_sd$normal_human.sd, item_BFI2_sd$persona_GPT3.5.sd)
mae_persona_GPT3.5.sd_normal_human
profile_correlation_persona_GPT3.5.sd_normal_human

# persona_GPT3.5 ~ old_people
mae_persona_GPT3.5.sd_old_people <- mean(abs(item_BFI2_sd$old_people.sd - item_BFI2_sd$persona_GPT3.5.sd))
profile_correlation_persona_GPT3.5.sd_old_people <- cor(item_BFI2_sd$old_people.sd, item_BFI2_sd$persona_GPT3.5.sd)
mae_persona_GPT3.5.sd_old_people
profile_correlation_persona_GPT3.5.sd_old_people

# shape_GPT3.5 ~ normal_human
mae_shape_GPT3.5.sd_normal_human <- mean(abs(item_BFI2_sd$normal_human.sd - item_BFI2_sd$shape_GPT3.5.sd))
profile_correlation_shape_GPT3.5.sd_normal_human <- cor(item_BFI2_sd$normal_human.sd, item_BFI2_sd$shape_GPT3.5.sd)
mae_shape_GPT3.5.sd_normal_human
profile_correlation_shape_GPT3.5.sd_normal_human

# shape_GPT3.5 ~ old_people
mae_shape_GPT3.5.sd_old_people <- mean(abs(item_BFI2_sd$old_people.sd - item_BFI2_sd$shape_GPT3.5.sd))
profile_correlation_shape_GPT3.5.sd_old_people <- cor(item_BFI2_sd$old_people.sd, item_BFI2_sd$shape_GPT3.5.sd)
mae_shape_GPT3.5.sd_old_people
profile_correlation_shape_GPT3.5.sd_old_people



# persona_GPT4 ~ normal_human
mae_persona_GPT4.sd_normal_human <- mean(abs(item_BFI2_sd$normal_human.sd - item_BFI2_sd$persona_GPT4.sd))
profile_correlation_persona_GPT4.sd_normal_human <- cor(item_BFI2_sd$normal_human.sd, item_BFI2_sd$persona_GPT4.sd)
mae_persona_GPT4.sd_normal_human
profile_correlation_persona_GPT4.sd_normal_human

# persona_GPT4 ~ old_people
mae_persona_GPT4.sd_old_people <- mean(abs(item_BFI2_sd$old_people.sd - item_BFI2_sd$persona_GPT4.sd))
profile_correlation_persona_GPT4.sd_old_people <- cor(item_BFI2_sd$old_people.sd, item_BFI2_sd$persona_GPT4.sd)
mae_persona_GPT4.sd_old_people
profile_correlation_persona_GPT4.sd_old_people

# shape_GPT4 ~ normal_human
mae_shape_GPT4.sd_normal_human <- mean(abs(item_BFI2_sd$normal_human.sd - item_BFI2_sd$shape_GPT4.sd))
profile_correlation_shape_GPT4.sd_normal_human <- cor(item_BFI2_sd$normal_human.sd, item_BFI2_sd$shape_GPT4.sd)
mae_shape_GPT4.sd_normal_human
profile_correlation_shape_GPT4.sd_normal_human

# shape_GPT4 ~ old_people
mae_shape_GPT4.sd_old_people <- mean(abs(item_BFI2_sd$old_people.sd - item_BFI2_sd$shape_GPT4.sd))
profile_correlation_shape_GPT4.sd_old_people <- cor(item_BFI2_sd$old_people.sd, item_BFI2_sd$shape_GPT4.sd)
mae_shape_GPT4.sd_old_people
profile_correlation_shape_GPT4.sd_old_people


# persona_LLaMA3 ~ normal_human
mae_persona_LLaMA3.sd_normal_human <- mean(abs(item_BFI2_sd$normal_human.sd - item_BFI2_sd$persona_LLaMA3.sd))
profile_correlation_persona_LLaMA3.sd_normal_human <- cor(item_BFI2_sd$normal_human.sd, item_BFI2_sd$persona_LLaMA3.sd)
mae_persona_LLaMA3.sd_normal_human
profile_correlation_persona_LLaMA3.sd_normal_human

# persona_LLaMA3 ~ old_people
mae_persona_LLaMA3.sd_old_people <- mean(abs(item_BFI2_sd$old_people.sd - item_BFI2_sd$persona_LLaMA3.sd))
profile_correlation_persona_LLaMA3.sd_old_people <- cor(item_BFI2_sd$old_people.sd, item_BFI2_sd$persona_LLaMA3.sd)
mae_persona_LLaMA3.sd_old_people
profile_correlation_persona_LLaMA3.sd_old_people

# shape_LLaMA3 ~ normal_human
mae_shape_LLaMA3.sd_normal_human <- mean(abs(item_BFI2_sd$normal_human.sd - item_BFI2_sd$shape_LLaMA3.sd))
profile_correlation_shape_LLaMA3.sd_normal_human <- cor(item_BFI2_sd$normal_human.sd, item_BFI2_sd$shape_LLaMA3.sd)
mae_shape_LLaMA3.sd_normal_human
profile_correlation_shape_LLaMA3.sd_normal_human

# shape_LLaMA3 ~ old_people
mae_shape_LLaMA3.sd_old_people <- mean(abs(item_BFI2_sd$old_people.sd - item_BFI2_sd$shape_LLaMA3.sd))
profile_correlation_shape_LLaMA3.sd_old_people <- cor(item_BFI2_sd$old_people.sd, item_BFI2_sd$shape_LLaMA3.sd)
mae_shape_LLaMA3.sd_old_people
profile_correlation_shape_LLaMA3.sd_old_people





# facet mean
facet_BFI2_mean <- read.table("Summary_data/facet_BFI2_mean.csv",sep = ",", header = T)

# old_people ~ normal_human
#mae_old_people.mean_normal_human <- mean(abs(facet_BFI2_mean$normal_human.mean - facet_BFI2_mean$old_people.mean))
#profile_correlation_old_people.mean_normal_human <- cor(facet_BFI2_mean$normal_human.mean, facet_BFI2_mean$old_people.mean)
#mae_old_people.mean_normal_human
#profile_correlation_old_people.mean_normal_human

# persona_GPT3.5 ~ normal_human
mae_persona_GPT3.5.mean_normal_human <- mean(abs(facet_BFI2_mean$normal_human.mean - facet_BFI2_mean$persona_GPT3.5.mean))
profile_correlation_persona_GPT3.5.mean_normal_human <- cor(facet_BFI2_mean$normal_human.mean, facet_BFI2_mean$persona_GPT3.5.mean)
mae_persona_GPT3.5.mean_normal_human
profile_correlation_persona_GPT3.5.mean_normal_human

# persona_GPT3.5 ~ old_people
mae_persona_GPT3.5.mean_old_people <- mean(abs(facet_BFI2_mean$old_people.mean - facet_BFI2_mean$persona_GPT3.5.mean))
profile_correlation_persona_GPT3.5.mean_old_people <- cor(facet_BFI2_mean$old_people.mean, facet_BFI2_mean$persona_GPT3.5.mean)
mae_persona_GPT3.5.mean_old_people
profile_correlation_persona_GPT3.5.mean_old_people

# shape_GPT3.5 ~ normal_human
mae_shape_GPT3.5.mean_normal_human <- mean(abs(facet_BFI2_mean$normal_human.mean - facet_BFI2_mean$shape_GPT3.5.mean))
profile_correlation_shape_GPT3.5.mean_normal_human <- cor(facet_BFI2_mean$normal_human.mean, facet_BFI2_mean$shape_GPT3.5.mean)
mae_shape_GPT3.5.mean_normal_human
profile_correlation_shape_GPT3.5.mean_normal_human

# shape_GPT3.5 ~ old_people
mae_shape_GPT3.5.mean_old_people <- mean(abs(facet_BFI2_mean$old_people.mean - facet_BFI2_mean$shape_GPT3.5.mean))
profile_correlation_shape_GPT3.5.mean_old_people <- cor(facet_BFI2_mean$old_people.mean, facet_BFI2_mean$shape_GPT3.5.mean)
mae_shape_GPT3.5.mean_old_people
profile_correlation_shape_GPT3.5.mean_old_people



# persona_GPT4 ~ normal_human
mae_persona_GPT4.mean_normal_human <- mean(abs(facet_BFI2_mean$normal_human.mean - facet_BFI2_mean$persona_GPT4.mean))
profile_correlation_persona_GPT4.mean_normal_human <- cor(facet_BFI2_mean$normal_human.mean, facet_BFI2_mean$persona_GPT4.mean)
mae_persona_GPT4.mean_normal_human
profile_correlation_persona_GPT4.mean_normal_human

# persona_GPT4 ~ old_people
mae_persona_GPT4.mean_old_people <- mean(abs(facet_BFI2_mean$old_people.mean - facet_BFI2_mean$persona_GPT4.mean))
profile_correlation_persona_GPT4.mean_old_people <- cor(facet_BFI2_mean$old_people.mean, facet_BFI2_mean$persona_GPT4.mean)
mae_persona_GPT4.mean_old_people
profile_correlation_persona_GPT4.mean_old_people

# shape_GPT4 ~ normal_human
mae_shape_GPT4.mean_normal_human <- mean(abs(facet_BFI2_mean$normal_human.mean - facet_BFI2_mean$shape_GPT4.mean))
profile_correlation_shape_GPT4.mean_normal_human <- cor(facet_BFI2_mean$normal_human.mean, facet_BFI2_mean$shape_GPT4.mean)
mae_shape_GPT4.mean_normal_human
profile_correlation_shape_GPT4.mean_normal_human

# shape_GPT4 ~ old_people
mae_shape_GPT4.mean_old_people <- mean(abs(facet_BFI2_mean$old_people.mean - facet_BFI2_mean$shape_GPT4.mean))
profile_correlation_shape_GPT4.mean_old_people <- cor(facet_BFI2_mean$old_people.mean, facet_BFI2_mean$shape_GPT4.mean)
mae_shape_GPT4.mean_old_people
profile_correlation_shape_GPT4.mean_old_people


# persona_LLaMA3 ~ normal_human
mae_persona_LLaMA3.mean_normal_human <- mean(abs(facet_BFI2_mean$normal_human.mean - facet_BFI2_mean$persona_LLaMA3.mean))
profile_correlation_persona_LLaMA3.mean_normal_human <- cor(facet_BFI2_mean$normal_human.mean, facet_BFI2_mean$persona_LLaMA3.mean)
mae_persona_LLaMA3.mean_normal_human
profile_correlation_persona_LLaMA3.mean_normal_human

# persona_LLaMA3 ~ old_people
mae_persona_LLaMA3.mean_old_people <- mean(abs(facet_BFI2_mean$old_people.mean - facet_BFI2_mean$persona_LLaMA3.mean))
profile_correlation_persona_LLaMA3.mean_old_people <- cor(facet_BFI2_mean$old_people.mean, facet_BFI2_mean$persona_LLaMA3.mean)
mae_persona_LLaMA3.mean_old_people
profile_correlation_persona_LLaMA3.mean_old_people

# shape_LLaMA3 ~ normal_human
mae_shape_LLaMA3.mean_normal_human <- mean(abs(facet_BFI2_mean$normal_human.mean - facet_BFI2_mean$shape_LLaMA3.mean))
profile_correlation_shape_LLaMA3.mean_normal_human <- cor(facet_BFI2_mean$normal_human.mean, facet_BFI2_mean$shape_LLaMA3.mean)
mae_shape_LLaMA3.mean_normal_human
profile_correlation_shape_LLaMA3.mean_normal_human

# shape_LLaMA3 ~ old_people
mae_shape_LLaMA3.mean_old_people <- mean(abs(facet_BFI2_mean$old_people.mean - facet_BFI2_mean$shape_LLaMA3.mean))
profile_correlation_shape_LLaMA3.mean_old_people <- cor(facet_BFI2_mean$old_people.mean, facet_BFI2_mean$shape_LLaMA3.mean)
mae_shape_LLaMA3.mean_old_people
profile_correlation_shape_LLaMA3.mean_old_people




# facet sd
facet_BFI2_sd <- read.table("Summary_data/facet_BFI2_sd.csv",sep = ",", header = T)

# old_people ~ normal_human
mae_old_people.sd_normal_human <- mean(abs(facet_BFI2_sd$normal_human.sd - facet_BFI2_sd$old_people.sd))
profile_correlation_old_people.sd_normal_human <- cor(facet_BFI2_sd$normal_human.sd, facet_BFI2_sd$old_people.sd)
mae_old_people.sd_normal_human
profile_correlation_old_people.sd_normal_human


# persona_GPT3.5 ~ normal_human
mae_persona_GPT3.5.sd_normal_human <- mean(abs(facet_BFI2_sd$normal_human.sd - facet_BFI2_sd$persona_GPT3.5.sd))
profile_correlation_persona_GPT3.5.sd_normal_human <- cor(facet_BFI2_sd$normal_human.sd, facet_BFI2_sd$persona_GPT3.5.sd)
mae_persona_GPT3.5.sd_normal_human
profile_correlation_persona_GPT3.5.sd_normal_human

# persona_GPT3.5 ~ old_people
mae_persona_GPT3.5.sd_old_people <- mean(abs(facet_BFI2_sd$old_people.sd - facet_BFI2_sd$persona_GPT3.5.sd))
profile_correlation_persona_GPT3.5.sd_old_people <- cor(facet_BFI2_sd$old_people.sd, facet_BFI2_sd$persona_GPT3.5.sd)
mae_persona_GPT3.5.sd_old_people
profile_correlation_persona_GPT3.5.sd_old_people

# shape_GPT3.5 ~ normal_human
mae_shape_GPT3.5.sd_normal_human <- mean(abs(facet_BFI2_sd$normal_human.sd - facet_BFI2_sd$shape_GPT3.5.sd))
profile_correlation_shape_GPT3.5.sd_normal_human <- cor(facet_BFI2_sd$normal_human.sd, facet_BFI2_sd$shape_GPT3.5.sd)
mae_shape_GPT3.5.sd_normal_human
profile_correlation_shape_GPT3.5.sd_normal_human

# shape_GPT3.5 ~ old_people
mae_shape_GPT3.5.sd_old_people <- mean(abs(facet_BFI2_sd$old_people.sd - facet_BFI2_sd$shape_GPT3.5.sd))
profile_correlation_shape_GPT3.5.sd_old_people <- cor(facet_BFI2_sd$old_people.sd, facet_BFI2_sd$shape_GPT3.5.sd)
mae_shape_GPT3.5.sd_old_people
profile_correlation_shape_GPT3.5.sd_old_people




# persona_GPT4 ~ normal_human
mae_persona_GPT4.sd_normal_human <- mean(abs(facet_BFI2_sd$normal_human.sd - facet_BFI2_sd$persona_GPT4.sd))
profile_correlation_persona_GPT4.sd_normal_human <- cor(facet_BFI2_sd$normal_human.sd, facet_BFI2_sd$persona_GPT4.sd)
mae_persona_GPT4.sd_normal_human
profile_correlation_persona_GPT4.sd_normal_human

# persona_GPT4 ~ old_people
mae_persona_GPT4.sd_old_people <- mean(abs(facet_BFI2_sd$old_people.sd - facet_BFI2_sd$persona_GPT4.sd))
profile_correlation_persona_GPT4.sd_old_people <- cor(facet_BFI2_sd$old_people.sd, facet_BFI2_sd$persona_GPT4.sd)
mae_persona_GPT4.sd_old_people
profile_correlation_persona_GPT4.sd_old_people

# shape_GPT4 ~ normal_human
mae_shape_GPT4.sd_normal_human <- mean(abs(facet_BFI2_sd$normal_human.sd - facet_BFI2_sd$shape_GPT4.sd))
profile_correlation_shape_GPT4.sd_normal_human <- cor(facet_BFI2_sd$normal_human.sd, facet_BFI2_sd$shape_GPT4.sd)
mae_shape_GPT4.sd_normal_human
profile_correlation_shape_GPT4.sd_normal_human

# shape_GPT4 ~ old_people
mae_shape_GPT4.sd_old_people <- mean(abs(facet_BFI2_sd$old_people.sd - facet_BFI2_sd$shape_GPT4.sd))
profile_correlation_shape_GPT4.sd_old_people <- cor(facet_BFI2_sd$old_people.sd, facet_BFI2_sd$shape_GPT4.sd)
mae_shape_GPT4.sd_old_people
profile_correlation_shape_GPT4.sd_old_people


# persona_LLaMA3 ~ normal_human
mae_persona_LLaMA3.sd_normal_human <- mean(abs(facet_BFI2_sd$normal_human.sd - facet_BFI2_sd$persona_LLaMA3.sd))
profile_correlation_persona_LLaMA3.sd_normal_human <- cor(facet_BFI2_sd$normal_human.sd, facet_BFI2_sd$persona_LLaMA3.sd)
mae_persona_LLaMA3.sd_normal_human
profile_correlation_persona_LLaMA3.sd_normal_human

# persona_LLaMA3 ~ old_people
mae_persona_LLaMA3.sd_old_people <- mean(abs(facet_BFI2_sd$old_people.sd - facet_BFI2_sd$persona_LLaMA3.sd))
profile_correlation_persona_LLaMA3.sd_old_people <- cor(facet_BFI2_sd$old_people.sd, facet_BFI2_sd$persona_LLaMA3.sd)
mae_persona_LLaMA3.sd_old_people
profile_correlation_persona_LLaMA3.sd_old_people

# shape_LLaMA3 ~ normal_human
mae_shape_LLaMA3.sd_normal_human <- mean(abs(facet_BFI2_sd$normal_human.sd - facet_BFI2_sd$shape_LLaMA3.sd))
profile_correlation_shape_LLaMA3.sd_normal_human <- cor(facet_BFI2_sd$normal_human.sd, facet_BFI2_sd$shape_LLaMA3.sd)
mae_shape_LLaMA3.sd_normal_human
profile_correlation_shape_LLaMA3.sd_normal_human

# shape_LLaMA3 ~ old_people
mae_shape_LLaMA3.sd_old_people <- mean(abs(facet_BFI2_sd$old_people.sd - facet_BFI2_sd$shape_LLaMA3.sd))
profile_correlation_shape_LLaMA3.sd_old_people <- cor(facet_BFI2_sd$old_people.sd, facet_BFI2_sd$shape_LLaMA3.sd)
mae_shape_LLaMA3.sd_old_people
profile_correlation_shape_LLaMA3.sd_old_people





# domain mean
domain_BFI2_mean <- read.table("Summary_data/domain_BFI2_mean.csv",sep = ",", header = T)

# old_people ~ normal_humanz
mae_old_people.mean_normal_human <- mean(abs(domain_BFI2_mean$normal_human.mean - domain_BFI2_mean$old_people.mean))
profile_correlation_old_people.mean_normal_human <- cor(domain_BFI2_mean$normal_human.mean, domain_BFI2_mean$old_people.mean)
mae_old_people.mean_normal_human
profile_correlation_old_people.mean_normal_human

# persona_GPT3.5 ~ normal_human
mae_persona_GPT3.5.mean_normal_human <- mean(abs(domain_BFI2_mean$normal_human.mean - domain_BFI2_mean$persona_GPT3.5.mean))
profile_correlation_persona_GPT3.5.mean_normal_human <- cor(domain_BFI2_mean$normal_human.mean, domain_BFI2_mean$persona_GPT3.5.mean)
mae_persona_GPT3.5.mean_normal_human
profile_correlation_persona_GPT3.5.mean_normal_human

# persona_GPT3.5 ~ old_people
mae_persona_GPT3.5.mean_old_people <- mean(abs(domain_BFI2_mean$old_people.mean - domain_BFI2_mean$persona_GPT3.5.mean))
profile_correlation_persona_GPT3.5.mean_old_people <- cor(domain_BFI2_mean$old_people.mean, domain_BFI2_mean$persona_GPT3.5.mean)
mae_persona_GPT3.5.mean_old_people
profile_correlation_persona_GPT3.5.mean_old_people

# shape_GPT3.5 ~ normal_human
mae_shape_GPT3.5.mean_normal_human <- mean(abs(domain_BFI2_mean$normal_human.mean - domain_BFI2_mean$shape_GPT3.5.mean))
profile_correlation_shape_GPT3.5.mean_normal_human <- cor(domain_BFI2_mean$normal_human.mean, domain_BFI2_mean$shape_GPT3.5.mean)
mae_shape_GPT3.5.mean_normal_human
profile_correlation_shape_GPT3.5.mean_normal_human

# shape_GPT3.5 ~ old_people
mae_shape_GPT3.5.mean_old_people <- mean(abs(domain_BFI2_mean$old_people.mean - domain_BFI2_mean$shape_GPT3.5.mean))
profile_correlation_shape_GPT3.5.mean_old_people <- cor(domain_BFI2_mean$old_people.mean, domain_BFI2_mean$shape_GPT3.5.mean)
mae_shape_GPT3.5.mean_old_people
profile_correlation_shape_GPT3.5.mean_old_people



# persona_GPT4 ~ normal_human
mae_persona_GPT4.mean_normal_human <- mean(abs(domain_BFI2_mean$normal_human.mean - domain_BFI2_mean$persona_GPT4.mean))
profile_correlation_persona_GPT4.mean_normal_human <- cor(domain_BFI2_mean$normal_human.mean, domain_BFI2_mean$persona_GPT4.mean)
mae_persona_GPT4.mean_normal_human
profile_correlation_persona_GPT4.mean_normal_human

# persona_GPT4 ~ old_people
mae_persona_GPT4.mean_old_people <- mean(abs(domain_BFI2_mean$old_people.mean - domain_BFI2_mean$persona_GPT4.mean))
profile_correlation_persona_GPT4.mean_old_people <- cor(domain_BFI2_mean$old_people.mean, domain_BFI2_mean$persona_GPT4.mean)
mae_persona_GPT4.mean_old_people
profile_correlation_persona_GPT4.mean_old_people

# shape_GPT4 ~ normal_human
mae_shape_GPT4.mean_normal_human <- mean(abs(domain_BFI2_mean$normal_human.mean - domain_BFI2_mean$shape_GPT4.mean))
profile_correlation_shape_GPT4.mean_normal_human <- cor(domain_BFI2_mean$normal_human.mean, domain_BFI2_mean$shape_GPT4.mean)
mae_shape_GPT4.mean_normal_human
profile_correlation_shape_GPT4.mean_normal_human

# shape_GPT4 ~ old_people
mae_shape_GPT4.mean_old_people <- mean(abs(domain_BFI2_mean$old_people.mean - domain_BFI2_mean$shape_GPT4.mean))
profile_correlation_shape_GPT4.mean_old_people <- cor(domain_BFI2_mean$old_people.mean, domain_BFI2_mean$shape_GPT4.mean)
mae_shape_GPT4.mean_old_people
profile_correlation_shape_GPT4.mean_old_people


# persona_LLaMA3 ~ normal_human
mae_persona_LLaMA3.mean_normal_human <- mean(abs(domain_BFI2_mean$normal_human.mean - domain_BFI2_mean$persona_LLaMA3.mean))
profile_correlation_persona_LLaMA3.mean_normal_human <- cor(domain_BFI2_mean$normal_human.mean, domain_BFI2_mean$persona_LLaMA3.mean)
mae_persona_LLaMA3.mean_normal_human
profile_correlation_persona_LLaMA3.mean_normal_human

# persona_LLaMA3 ~ old_people
mae_persona_LLaMA3.mean_old_people <- mean(abs(domain_BFI2_mean$old_people.mean - domain_BFI2_mean$persona_LLaMA3.mean))
profile_correlation_persona_LLaMA3.mean_old_people <- cor(domain_BFI2_mean$old_people.mean, domain_BFI2_mean$persona_LLaMA3.mean)
mae_persona_LLaMA3.mean_old_people
profile_correlation_persona_LLaMA3.mean_old_people

# shape_LLaMA3 ~ normal_human
mae_shape_LLaMA3.mean_normal_human <- mean(abs(domain_BFI2_mean$normal_human.mean - domain_BFI2_mean$shape_LLaMA3.mean))
profile_correlation_shape_LLaMA3.mean_normal_human <- cor(domain_BFI2_mean$normal_human.mean, domain_BFI2_mean$shape_LLaMA3.mean)
mae_shape_LLaMA3.mean_normal_human
profile_correlation_shape_LLaMA3.mean_normal_human

# shape_LLaMA3 ~ old_people
mae_shape_LLaMA3.mean_old_people <- mean(abs(domain_BFI2_mean$old_people.mean - domain_BFI2_mean$shape_LLaMA3.mean))
profile_correlation_shape_LLaMA3.mean_old_people <- cor(domain_BFI2_mean$old_people.mean, domain_BFI2_mean$shape_LLaMA3.mean)
mae_shape_LLaMA3.mean_old_people
profile_correlation_shape_LLaMA3.mean_old_people




# domain sd
domain_BFI2_sd <- read.table("Summary_data/domain_BFI2_sd.csv",sep = ",", header = T)

# persona_GPT3.5 ~ normal_human
mae_persona_GPT3.5.sd_normal_human <- mean(abs(domain_BFI2_sd$normal_human.sd - domain_BFI2_sd$persona_GPT3.5.sd))
profile_correlation_persona_GPT3.5.sd_normal_human <- cor(domain_BFI2_sd$normal_human.sd, domain_BFI2_sd$persona_GPT3.5.sd)
mae_persona_GPT3.5.sd_normal_human
profile_correlation_persona_GPT3.5.sd_normal_human

# persona_GPT3.5 ~ old_people
mae_persona_GPT3.5.sd_old_people <- mean(abs(domain_BFI2_sd$old_people.sd - domain_BFI2_sd$persona_GPT3.5.sd))
profile_correlation_persona_GPT3.5.sd_old_people <- cor(domain_BFI2_sd$old_people.sd, domain_BFI2_sd$persona_GPT3.5.sd)
mae_persona_GPT3.5.sd_old_people
profile_correlation_persona_GPT3.5.sd_old_people

# shape_GPT3.5 ~ normal_human
mae_shape_GPT3.5.sd_normal_human <- mean(abs(domain_BFI2_sd$normal_human.sd - domain_BFI2_sd$shape_GPT3.5.sd))
profile_correlation_shape_GPT3.5.sd_normal_human <- cor(domain_BFI2_sd$normal_human.sd, domain_BFI2_sd$shape_GPT3.5.sd)
mae_shape_GPT3.5.sd_normal_human
profile_correlation_shape_GPT3.5.sd_normal_human

# shape_GPT3.5 ~ old_people
mae_shape_GPT3.5.sd_old_people <- mean(abs(domain_BFI2_sd$old_people.sd - domain_BFI2_sd$shape_GPT3.5.sd))
profile_correlation_shape_GPT3.5.sd_old_people <- cor(domain_BFI2_sd$old_people.sd, domain_BFI2_sd$shape_GPT3.5.sd)
mae_shape_GPT3.5.sd_old_people
profile_correlation_shape_GPT3.5.sd_old_people



# persona_GPT4 ~ normal_human
mae_persona_GPT4.sd_normal_human <- mean(abs(domain_BFI2_sd$normal_human.sd - domain_BFI2_sd$persona_GPT4.sd))
profile_correlation_persona_GPT4.sd_normal_human <- cor(domain_BFI2_sd$normal_human.sd, domain_BFI2_sd$persona_GPT4.sd)
mae_persona_GPT4.sd_normal_human
profile_correlation_persona_GPT4.sd_normal_human

# persona_GPT4 ~ old_people
mae_persona_GPT4.sd_old_people <- mean(abs(domain_BFI2_sd$old_people.sd - domain_BFI2_sd$persona_GPT4.sd))
profile_correlation_persona_GPT4.sd_old_people <- cor(domain_BFI2_sd$old_people.sd, domain_BFI2_sd$persona_GPT4.sd)
mae_persona_GPT4.sd_old_people
profile_correlation_persona_GPT4.sd_old_people

# shape_GPT4 ~ normal_human
mae_shape_GPT4.sd_normal_human <- mean(abs(domain_BFI2_sd$normal_human.sd - domain_BFI2_sd$shape_GPT4.sd))
profile_correlation_shape_GPT4.sd_normal_human <- cor(domain_BFI2_sd$normal_human.sd, domain_BFI2_sd$shape_GPT4.sd)
mae_shape_GPT4.sd_normal_human
profile_correlation_shape_GPT4.sd_normal_human

# shape_GPT4 ~ old_people
mae_shape_GPT4.sd_old_people <- mean(abs(domain_BFI2_sd$old_people.sd - domain_BFI2_sd$shape_GPT4.sd))
profile_correlation_shape_GPT4.sd_old_people <- cor(domain_BFI2_sd$old_people.sd, domain_BFI2_sd$shape_GPT4.sd)
mae_shape_GPT4.sd_old_people
profile_correlation_shape_GPT4.sd_old_people



# persona_LLaMA3 ~ normal_human
mae_persona_LLaMA3.sd_normal_human <- mean(abs(domain_BFI2_sd$normal_human.sd - domain_BFI2_sd$persona_LLaMA3.sd))
profile_correlation_persona_LLaMA3.sd_normal_human <- cor(domain_BFI2_sd$normal_human.sd, domain_BFI2_sd$persona_LLaMA3.sd)
mae_persona_LLaMA3.sd_normal_human
profile_correlation_persona_LLaMA3.sd_normal_human

# persona_LLaMA3 ~ old_people
mae_persona_LLaMA3.sd_old_people <- mean(abs(domain_BFI2_sd$old_people.sd - domain_BFI2_sd$persona_LLaMA3.sd))
profile_correlation_persona_LLaMA3.sd_old_people <- cor(domain_BFI2_sd$old_people.sd, domain_BFI2_sd$persona_LLaMA3.sd)
mae_persona_LLaMA3.sd_old_people
profile_correlation_persona_LLaMA3.sd_old_people

# shape_LLaMA3 ~ normal_human
mae_shape_LLaMA3.sd_normal_human <- mean(abs(domain_BFI2_sd$normal_human.sd - domain_BFI2_sd$shape_LLaMA3.sd))
profile_correlation_shape_LLaMA3.sd_normal_human <- cor(domain_BFI2_sd$normal_human.sd, domain_BFI2_sd$shape_LLaMA3.sd)
mae_shape_LLaMA3.sd_normal_human
profile_correlation_shape_LLaMA3.sd_normal_human

# shape_LLaMA3 ~ old_people
mae_shape_LLaMA3.sd_old_people <- mean(abs(domain_BFI2_sd$old_people.sd - domain_BFI2_sd$shape_LLaMA3.sd))
profile_correlation_shape_LLaMA3.sd_old_people <- cor(domain_BFI2_sd$old_people.sd, domain_BFI2_sd$shape_LLaMA3.sd)
mae_shape_LLaMA3.sd_old_people
profile_correlation_shape_LLaMA3.sd_old_people



#######################################SEM########################################

colnames(data_normal_human) <- paste0("item", 1:60)
colnames(data_old_people) <- paste0("item", 1:60)

colnames(data_persona_GPT3.5) <- paste0("item", 1:60)
colnames(data_shape_GPT3.5) <- paste0("item", 1:60)

colnames(data_persona_GPT4) <- paste0("item", 1:60)
colnames(data_shape_GPT4) <- paste0("item", 1:60)

colnames(data_persona_LLaMA3) <- paste0("item", 1:60)
colnames(data_shape_LLaMA3) <- paste0("item", 1:60)

#write.csv(data_normal_human, "data_normal_human.csv", row.names = FALSE)
#write.csv(data_persona_GPT3.5, "data_persona_GPT3.5.csv", row.names = FALSE)
#write.csv(data_shape_GPT3.5, "data_shape_GPT3.5.csv", row.names = FALSE)
#write.csv(data_persona_GPT4, "data_persona_GPT4.csv", row.names = FALSE)
#write.csv(data_shape_GPT4, "data_shape_GPT4.csv", row.names = FALSE)
#write.csv(data_persona_LLaMA3, "data_persona_LLaMA3.csv", row.names = FALSE)
#write.csv(data_shape_LLaMA3, "data_shape_LLaMA3.csv", row.names = FALSE)



#### Factor analysis to examine structural validity 

mod.ext <- "Sociability =~ item1+item16+item31+item46
            Assertiveness =~ item6+item21+item36+item51
            Energy_Level =~ item11+item26+item41+item56"

mod.agr <- "Compassion =~ item2+item17+item32+item47
            Respectfulness =~ item7+item22+item37+item52
            Trust =~ item12+item27+item42+item57"

mod.con <- "Organization =~ item3+item18+item33+item48
            Productiveness =~ item8+item23+item38+item53
            Responsibility =~ item13+item28+item43+item58"

mod.neu <- "Anxiety =~ item4+item19+item34+item49
            Depression =~ item9+item24+item39+item54
            Emotional_Volatility =~ item14+item29+item44+item59"

mod.ope <- "Intellectual_Curiosity =~ item10+item25+item40+item55
            Aesthetic_Sensitivity =~ item5+item20+item35+item50
            Creative_Imagination =~ item15+item30+item45+item60"



# normal_human
fit.normal_human.ext <- lavaan::sem(mod.ext,data = data_normal_human,std.lv=T)
fit.normal_human.agr <- lavaan::sem(mod.agr,data = data_normal_human,std.lv=T)
fit.normal_human.con <- lavaan::sem(mod.con,data = data_normal_human,std.lv=T)
fit.normal_human.neu <- lavaan::sem(mod.neu,data = data_normal_human,std.lv=T)
fit.normal_human.ope <- lavaan::sem(mod.ope,data = data_normal_human,std.lv=T)

# old_people
fit.old_people.ext <- lavaan::sem(mod.ext,data = data_old_people,std.lv=T)
fit.old_people.agr <- lavaan::sem(mod.agr,data = data_old_people,std.lv=T)
fit.old_people.con <- lavaan::sem(mod.con,data = data_old_people,std.lv=T)
fit.old_people.neu <- lavaan::sem(mod.neu,data = data_old_people,std.lv=T)
fit.old_people.ope <- lavaan::sem(mod.ope,data = data_old_people,std.lv=T)

# persona_GPT3.5
fit.persona_GPT3.5.ext <- lavaan::sem(mod.ext,data = data_persona_GPT3.5,std.lv=T)
fit.persona_GPT3.5.agr <- lavaan::sem(mod.agr,data = data_persona_GPT3.5,std.lv=T)
fit.persona_GPT3.5.con <- lavaan::sem(mod.con,data = data_persona_GPT3.5,std.lv=T)
fit.persona_GPT3.5.neu <- lavaan::sem(mod.neu,data = data_persona_GPT3.5,std.lv=T)
fit.persona_GPT3.5.ope <- lavaan::sem(mod.ope,data = data_persona_GPT3.5,std.lv=T)

# shape_GPT3.5
fit.shape_GPT3.5.ext <- lavaan::sem(mod.ext,data = data_shape_GPT3.5,std.lv=T)
fit.shape_GPT3.5.agr <- lavaan::sem(mod.agr,data = data_shape_GPT3.5,std.lv=T)
fit.shape_GPT3.5.con <- lavaan::sem(mod.con,data = data_shape_GPT3.5,std.lv=T)
fit.shape_GPT3.5.neu <- lavaan::sem(mod.neu,data = data_shape_GPT3.5,std.lv=T)
fit.shape_GPT3.5.ope <- lavaan::sem(mod.ope,data = data_shape_GPT3.5,std.lv=T)

# persona_GPT4
fit.persona_GPT4.ext <- lavaan::sem(mod.ext,data = data_persona_GPT4,std.lv=T)
fit.persona_GPT4.agr <- lavaan::sem(mod.agr,data = data_persona_GPT4,std.lv=T)
fit.persona_GPT4.con <- lavaan::sem(mod.con,data = data_persona_GPT4,std.lv=T)
fit.persona_GPT4.neu <- lavaan::sem(mod.neu,data = data_persona_GPT4,std.lv=T)
fit.persona_GPT4.ope <- lavaan::sem(mod.ope,data = data_persona_GPT4,std.lv=T)

# shape_GPT4
fit.shape_GPT4.ext <- lavaan::sem(mod.ext,data = data_shape_GPT4,std.lv=T)
fit.shape_GPT4.agr <- lavaan::sem(mod.agr,data = data_shape_GPT4,std.lv=T)
fit.shape_GPT4.con <- lavaan::sem(mod.con,data = data_shape_GPT4,std.lv=T)
fit.shape_GPT4.neu <- lavaan::sem(mod.neu,data = data_shape_GPT4,std.lv=T)
fit.shape_GPT4.ope <- lavaan::sem(mod.ope,data = data_shape_GPT4,std.lv=T)

# persona_LLaMA3
fit.persona_LLaMA3.ext <- lavaan::sem(mod.ext,data = data_persona_LLaMA3,std.lv=T)
fit.persona_LLaMA3.agr <- lavaan::sem(mod.agr,data = data_persona_LLaMA3,std.lv=T)
fit.persona_LLaMA3.con <- lavaan::sem(mod.con,data = data_persona_LLaMA3,std.lv=T)
fit.persona_LLaMA3.neu <- lavaan::sem(mod.neu,data = data_persona_LLaMA3,std.lv=T)
fit.persona_LLaMA3.ope <- lavaan::sem(mod.ope,data = data_persona_LLaMA3,std.lv=T)

# shape_LLaMA3
fit.shape_LLaMA3.ext <- lavaan::sem(mod.ext,data = data_shape_LLaMA3,std.lv=T)
fit.shape_LLaMA3.agr <- lavaan::sem(mod.agr,data = data_shape_LLaMA3,std.lv=T)
fit.shape_LLaMA3.con <- lavaan::sem(mod.con,data = data_shape_LLaMA3,std.lv=T)
fit.shape_LLaMA3.neu <- lavaan::sem(mod.neu,data = data_shape_LLaMA3,std.lv=T)
fit.shape_LLaMA3.ope <- lavaan::sem(mod.ope,data = data_shape_LLaMA3,std.lv=T)

# check results 
lavaan::summary(fit.normal_human.ext,standardized=T,fit=T)
lavaan::summary(fit.old_people.ext,standardized=T,fit=T)
lavaan::summary(fit.persona_GPT3.5.ext,standardized=T,fit=T)
lavaan::summary(fit.shape_GPT3.5.ext,standardized=T,fit=T)
lavaan::summary(fit.persona_GPT4.ext,standardized=T,fit=T)
lavaan::summary(fit.shape_GPT4.ext,standardized=T,fit=T)

lavaan::summary(fit.normal_human.agr,standardized=T,fit=T)
lavaan::summary(fit.old_people.agr,standardized=T,fit=T)
lavaan::summary(fit.persona_GPT3.5.agr,standardized=T,fit=T)
lavaan::summary(fit.shape_GPT3.5.agr,standardized=T,fit=T)
lavaan::summary(fit.persona_GPT4.agr,standardized=T,fit=T)
lavaan::summary(fit.shape_GPT4.agr,standardized=T,fit=T)

lavaan::summary(fit.normal_human.con,standardized=T,fit=T)
lavaan::summary(fit.old_people.con,standardized=T,fit=T)
lavaan::summary(fit.persona_GPT3.5.con,standardized=T,fit=T)
lavaan::summary(fit.shape_GPT3.5.con,standardized=T,fit=T)
lavaan::summary(fit.persona_GPT4.con,standardized=T,fit=T)
lavaan::summary(fit.shape_GPT4.con,standardized=T,fit=T)

lavaan::summary(fit.normal_human.neu,standardized=T,fit=T)
lavaan::summary(fit.old_people.neu,standardized=T,fit=T)
lavaan::summary(fit.persona_GPT3.5.neu,standardized=T,fit=T)
lavaan::summary(fit.shape_GPT3.5.neu,standardized=T,fit=T)
lavaan::summary(fit.persona_GPT4.neu,standardized=T,fit=T)
lavaan::summary(fit.shape_GPT4.neu,standardized=T,fit=T)

lavaan::summary(fit.normal_human.ope,standardized=T,fit=T)
lavaan::summary(fit.old_people.ope,standardized=T,fit=T)
lavaan::summary(fit.persona_GPT3.5.ope,standardized=T,fit=T)
lavaan::summary(fit.shape_GPT3.5.ope,standardized=T,fit=T)
lavaan::summary(fit.persona_GPT4.ope,standardized=T,fit=T)
lavaan::summary(fit.shape_GPT4.ope,standardized=T,fit=T)




# Summarize results
# Model fit 
# CFI>.90, TLI>.90, RMSEA < .06 and SRMR < .06 indicate good fit.
# If the model fit of GPT-based responses is as good as human responses, then it can serve as one piece of evidence supporting the notion that

mod.fit <- t(data.frame(
  ext.normal_human = fitMeasures(fit.normal_human.ext, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ext.old_people = fitMeasures(fit.old_people.ext, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ext.persona_GPT3.5 = fitMeasures(fit.persona_GPT3.5.ext, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ext.shape_GPT3.5 = fitMeasures(fit.shape_GPT3.5.ext, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ext.persona_GPT4 = fitMeasures(fit.persona_GPT4.ext, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ext.shape_GPT4 = fitMeasures(fit.shape_GPT4.ext, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ext.persona_LLaMA3 = fitMeasures(fit.persona_LLaMA3.ext, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ext.shape_LLaMA3 = fitMeasures(fit.shape_LLaMA3.ext, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  
  agr.normal_human = fitMeasures(fit.normal_human.agr, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  agr.old_people = fitMeasures(fit.old_people.agr, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  agr.persona_GPT3.5 = fitMeasures(fit.persona_GPT3.5.agr, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  agr.shape_GPT3.5 = fitMeasures(fit.shape_GPT3.5.agr, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  agr.persona_GPT4 = fitMeasures(fit.persona_GPT4.agr, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  agr.shape_GPT4 = fitMeasures(fit.shape_GPT4.agr, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  agr.persona_LLaMA3 = fitMeasures(fit.persona_LLaMA3.agr, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  agr.shape_LLaMA3 = fitMeasures(fit.shape_LLaMA3.agr, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  
  con.normal_human = fitMeasures(fit.normal_human.con, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  con.old_people = fitMeasures(fit.old_people.con, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  con.persona_GPT3.5 = fitMeasures(fit.persona_GPT3.5.con, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  con.shape_GPT3.5 = fitMeasures(fit.shape_GPT3.5.con, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  con.persona_GPT4 = fitMeasures(fit.persona_GPT4.con, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  con.shape_GPT4 = fitMeasures(fit.shape_GPT4.con, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  con.persona_LLaMA3 = fitMeasures(fit.persona_LLaMA3.con, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  con.shape_LLaMA3 = fitMeasures(fit.shape_LLaMA3.con, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  
  neu.normal_human = fitMeasures(fit.normal_human.neu, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  neu.old_people = fitMeasures(fit.old_people.neu, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  neu.persona_GPT3.5 = fitMeasures(fit.persona_GPT3.5.neu, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  neu.shape_GPT3.5 = fitMeasures(fit.shape_GPT3.5.neu, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  neu.persona_GPT4 = fitMeasures(fit.persona_GPT4.neu, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  neu.shape_GPT4 = fitMeasures(fit.shape_GPT4.neu, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  neu.persona_LLaMA3 = fitMeasures(fit.persona_LLaMA3.neu, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  neu.shape_LLaMA3 = fitMeasures(fit.shape_LLaMA3.neu, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  
  ope.normal_human = fitMeasures(fit.normal_human.ope, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ope.old_people = fitMeasures(fit.old_people.ope, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ope.persona_GPT3.5 = fitMeasures(fit.persona_GPT3.5.ope, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ope.shape_GPT3.5 = fitMeasures(fit.shape_GPT3.5.ope, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ope.persona_GPT4 = fitMeasures(fit.persona_GPT4.ope, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ope.shape_GPT4 = fitMeasures(fit.shape_GPT4.ope, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ope.persona_LLaMA3 = fitMeasures(fit.persona_LLaMA3.ope, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  ope.shape_LLaMA3 = fitMeasures(fit.shape_LLaMA3.ope, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean"))
  
)
)

write.table(mod.fit,"mod.fit.csv",sep = ",")


# Factor loadings 
# If loadings of GPT-based responses are close to those obtained from human responses, then it can serve as one piece of evidence supporting the notion that
loadings <- data.frame(
  normal_human.ext = lavaan::standardizedsolution(fit.normal_human.ext)[1:24,"est.std"],
  old_people.ext = lavaan::standardizedsolution(fit.old_people.ext)[1:24,"est.std"],
  persona_GPT3.5.ext = lavaan::standardizedsolution(fit.persona_GPT3.5.ext)[1:24,"est.std"],
  shape_GPT3.5.ext = lavaan::standardizedsolution(fit.shape_GPT3.5.ext)[1:24,"est.std"],
  persona_GPT4.ext = lavaan::standardizedsolution(fit.persona_GPT4.ext)[1:24,"est.std"],
  shape_GPT4.ext = lavaan::standardizedsolution(fit.shape_GPT4.ext)[1:24,"est.std"],
  persona_LLaMA3.ext = lavaan::standardizedsolution(fit.persona_LLaMA3.ext)[1:24,"est.std"],
  shape_LLaMA3.ext = lavaan::standardizedsolution(fit.shape_LLaMA3.ext)[1:24,"est.std"],
  
  normal_human.agr = lavaan::standardizedsolution(fit.normal_human.agr)[1:24,"est.std"],
  old_people.agr = lavaan::standardizedsolution(fit.old_people.agr)[1:24,"est.std"],
  persona_GPT3.5.agr = lavaan::standardizedsolution(fit.persona_GPT3.5.agr)[1:24,"est.std"],
  shape_GPT3.5.agr = lavaan::standardizedsolution(fit.shape_GPT3.5.agr)[1:24,"est.std"],
  persona_GPT4.agr = lavaan::standardizedsolution(fit.persona_GPT4.agr)[1:24,"est.std"],
  shape_GPT4.agr = lavaan::standardizedsolution(fit.shape_GPT4.agr)[1:24,"est.std"],
  persona_LLaMA3.agr = lavaan::standardizedsolution(fit.persona_LLaMA3.agr)[1:24,"est.std"],
  shape_LLaMA3.agr = lavaan::standardizedsolution(fit.shape_LLaMA3.agr)[1:24,"est.std"],
  
  normal_human.con = lavaan::standardizedsolution(fit.normal_human.con)[1:24,"est.std"],
  old_people.con = lavaan::standardizedsolution(fit.old_people.con)[1:24,"est.std"],
  persona_GPT3.5.con = lavaan::standardizedsolution(fit.persona_GPT3.5.con)[1:24,"est.std"],
  shape_GPT3.5.con = lavaan::standardizedsolution(fit.shape_GPT3.5.con)[1:24,"est.std"],
  persona_GPT4.con = lavaan::standardizedsolution(fit.persona_GPT4.con)[1:24,"est.std"],
  shape_GPT4.con = lavaan::standardizedsolution(fit.shape_GPT4.con)[1:24,"est.std"],
  persona_LLaMA3.con = lavaan::standardizedsolution(fit.persona_LLaMA3.con)[1:24,"est.std"],
  shape_LLaMA3.con = lavaan::standardizedsolution(fit.shape_LLaMA3.con)[1:24,"est.std"],
  
  normal_human.neu = lavaan::standardizedsolution(fit.normal_human.neu)[1:24,"est.std"],
  old_people.neu = lavaan::standardizedsolution(fit.old_people.neu)[1:24,"est.std"],
  persona_GPT3.5.neu = lavaan::standardizedsolution(fit.persona_GPT3.5.neu)[1:24,"est.std"],
  shape_GPT3.5.neu = lavaan::standardizedsolution(fit.shape_GPT3.5.neu)[1:24,"est.std"],
  persona_GPT4.neu = lavaan::standardizedsolution(fit.persona_GPT4.neu)[1:24,"est.std"],
  shape_GPT4.neu = lavaan::standardizedsolution(fit.shape_GPT4.neu)[1:24,"est.std"],
  persona_LLaMA3.neu = lavaan::standardizedsolution(fit.persona_LLaMA3.neu)[1:24,"est.std"],
  shape_LLaMA3.neu = lavaan::standardizedsolution(fit.shape_LLaMA3.neu)[1:24,"est.std"],
  
  normal_human.ope = lavaan::standardizedsolution(fit.normal_human.ope)[1:24,"est.std"],
  old_people.ope = lavaan::standardizedsolution(fit.old_people.ope)[1:24,"est.std"],
  persona_GPT3.5.ope = lavaan::standardizedsolution(fit.persona_GPT3.5.ope)[1:24,"est.std"],
  shape_GPT3.5.ope = lavaan::standardizedsolution(fit.shape_GPT3.5.ope)[1:24,"est.std"],
  persona_GPT4.ope = lavaan::standardizedsolution(fit.persona_GPT4.ope)[1:24,"est.std"],
  shape_GPT4.ope = lavaan::standardizedsolution(fit.shape_GPT4.ope)[1:24,"est.std"],
  persona_LLaMA3.ope = lavaan::standardizedsolution(fit.persona_LLaMA3.ope)[1:24,"est.std"],
  shape_LLaMA3.ope = lavaan::standardizedsolution(fit.shape_LLaMA3.ope)[1:24,"est.std"]
  
)

write.table(loadings,"loadings.csv",sep = ",")



# Inter-factor correlations
# If factor correlations obtained from GPT-based responses are close to those obtained from human responses, then it can serve as one piece of evidence supporting the notion that

correlations <- data.frame(
  normal_human.ext = lavaan::standardizedsolution(fit.normal_human.ext)[25:30,"est.std"],
  old_people.ext = lavaan::standardizedsolution(fit.old_people.ext)[25:30,"est.std"],
  persona_GPT3.5.ext = lavaan::standardizedsolution(fit.persona_GPT3.5.ext)[25:30,"est.std"],
  shape_GPT3.5.ext = lavaan::standardizedsolution(fit.shape_GPT3.5.ext)[25:30,"est.std"],
  persona_GPT4.ext = lavaan::standardizedsolution(fit.persona_GPT4.ext)[25:30,"est.std"],
  shape_GPT4.ext = lavaan::standardizedsolution(fit.shape_GPT4.ext)[25:30,"est.std"],
  persona_LLaMA3.ext = lavaan::standardizedsolution(fit.persona_LLaMA3.ext)[25:30,"est.std"],
  shape_LLaMA3.ext = lavaan::standardizedsolution(fit.shape_LLaMA3.ext)[25:30,"est.std"],
  
  normal_human.agr = lavaan::standardizedsolution(fit.normal_human.agr)[25:30,"est.std"],
  old_people.agr = lavaan::standardizedsolution(fit.old_people.agr)[25:30,"est.std"],
  persona_GPT3.5.agr = lavaan::standardizedsolution(fit.persona_GPT3.5.agr)[25:30,"est.std"],
  shape_GPT3.5.agr = lavaan::standardizedsolution(fit.shape_GPT3.5.agr)[25:30,"est.std"],
  persona_GPT4.agr = lavaan::standardizedsolution(fit.persona_GPT4.agr)[25:30,"est.std"],
  shape_GPT4.agr = lavaan::standardizedsolution(fit.shape_GPT4.agr)[25:30,"est.std"],
  persona_LLaMA3.agr = lavaan::standardizedsolution(fit.persona_LLaMA3.agr)[25:30,"est.std"],
  shape_LLaMA3.agr = lavaan::standardizedsolution(fit.shape_LLaMA3.agr)[25:30,"est.std"],
  
  normal_human.con = lavaan::standardizedsolution(fit.normal_human.con)[25:30,"est.std"],
  old_people.con = lavaan::standardizedsolution(fit.old_people.con)[25:30,"est.std"],
  persona_GPT3.5.con = lavaan::standardizedsolution(fit.persona_GPT3.5.con)[25:30,"est.std"],
  shape_GPT3.5.con = lavaan::standardizedsolution(fit.shape_GPT3.5.con)[25:30,"est.std"],
  persona_GPT4.con = lavaan::standardizedsolution(fit.persona_GPT4.con)[25:30,"est.std"],
  shape_GPT4.con = lavaan::standardizedsolution(fit.shape_GPT4.con)[25:30,"est.std"],
  persona_LLaMA3.con = lavaan::standardizedsolution(fit.persona_LLaMA3.con)[25:30,"est.std"],
  shape_LLaMA3.con = lavaan::standardizedsolution(fit.shape_LLaMA3.con)[25:30,"est.std"],
  
  normal_human.neu = lavaan::standardizedsolution(fit.normal_human.neu)[25:30,"est.std"],
  old_people.neu = lavaan::standardizedsolution(fit.old_people.neu)[25:30,"est.std"],
  persona_GPT3.5.neu = lavaan::standardizedsolution(fit.persona_GPT3.5.neu)[25:30,"est.std"],
  shape_GPT3.5.neu = lavaan::standardizedsolution(fit.shape_GPT3.5.neu)[25:30,"est.std"],
  persona_GPT4.neu = lavaan::standardizedsolution(fit.persona_GPT4.neu)[25:30,"est.std"],
  shape_GPT4.neu = lavaan::standardizedsolution(fit.shape_GPT4.neu)[25:30,"est.std"],
  persona_LLaMA3.neu = lavaan::standardizedsolution(fit.persona_LLaMA3.neu)[25:30,"est.std"],
  shape_LLaMA3.neu = lavaan::standardizedsolution(fit.shape_LLaMA3.neu)[25:30,"est.std"],
  
  normal_human.ope = lavaan::standardizedsolution(fit.normal_human.ope)[25:30,"est.std"],
  old_people.ope = lavaan::standardizedsolution(fit.old_people.ope)[25:30,"est.std"],
  persona_GPT3.5.ope = lavaan::standardizedsolution(fit.persona_GPT3.5.ope)[25:30,"est.std"],
  shape_GPT3.5.ope = lavaan::standardizedsolution(fit.shape_GPT3.5.ope)[25:30,"est.std"],
  persona_GPT4.ope = lavaan::standardizedsolution(fit.persona_GPT4.ope)[25:30,"est.std"],
  shape_GPT4.ope = lavaan::standardizedsolution(fit.shape_GPT4.ope)[25:30,"est.std"],
  persona_LLaMA3.ope = lavaan::standardizedsolution(fit.persona_LLaMA3.ope)[25:30,"est.std"],
  shape_LLaMA3.ope = lavaan::standardizedsolution(fit.shape_LLaMA3.ope)[25:30,"est.std"]

)

write.table(correlations,"correlations.csv",sep = ",")



### tcc
# ext
loadings_normal_human_ext <- lavInspect(fit.normal_human.ext, "std")$lambda
loadings_persona_GPT3.5_ext <- lavInspect(fit.persona_GPT3.5.ext, "std")$lambda
loadings_shape_GPT3.5_ext <- lavInspect(fit.shape_GPT3.5.ext, "std")$lambda
loadings_persona_GPT4_ext <- lavInspect(fit.persona_GPT4.ext, "std")$lambda
loadings_shape_GPT4_ext <- lavInspect(fit.shape_GPT4.ext, "std")$lambda
loadings_persona_LLaMA3_ext <- lavInspect(fit.persona_LLaMA3.ext, "std")$lambda
loadings_shape_LLaMA3_ext <- lavInspect(fit.shape_LLaMA3.ext, "std")$lambda


tcc_normal_persona_GPT3.5 <- factor.congruence(loadings_normal_human_ext, loadings_persona_GPT3.5_ext)
tcc_normal_shape_GPT3.5 <- factor.congruence(loadings_normal_human_ext, loadings_shape_GPT3.5_ext)
tcc_normal_persona_GPT4 <- factor.congruence(loadings_normal_human_ext, loadings_persona_GPT4_ext)
tcc_normal_shape_GPT4 <- factor.congruence(loadings_normal_human_ext, loadings_shape_GPT4_ext)
tcc_normal_persona_LLaMA3 <- factor.congruence(loadings_normal_human_ext, loadings_persona_LLaMA3_ext)
tcc_normal_shape_LLaMA3 <- factor.congruence(loadings_normal_human_ext, loadings_shape_LLaMA3_ext)

tcc_normal_persona_GPT3.5
tcc_normal_shape_GPT3.5
tcc_normal_persona_GPT4
tcc_normal_shape_GPT4
tcc_normal_persona_LLaMA3
tcc_normal_shape_LLaMA3

loadings_normal_human_ext[loadings_normal_human_ext == 0] <- NA
loadings_persona_GPT3.5_ext[loadings_persona_GPT3.5_ext == 0] <- NA
loadings_shape_GPT3.5_ext[loadings_shape_GPT3.5_ext == 0] <- NA
loadings_persona_GPT4_ext[loadings_persona_GPT4_ext == 0] <- NA
loadings_shape_GPT4_ext[loadings_shape_GPT4_ext == 0] <- NA
loadings_persona_LLaMA3_ext[loadings_persona_LLaMA3_ext == 0] <- NA
loadings_shape_LLaMA3_ext[loadings_shape_LLaMA3_ext == 0] <- NA

loadings_normal_human_ext <- as.data.frame(loadings_normal_human_ext)
loadings_persona_GPT3.5_ext <- as.data.frame(loadings_persona_GPT3.5_ext)
loadings_shape_GPT3.5_ext <- as.data.frame(loadings_shape_GPT3.5_ext)
loadings_persona_GPT4_ext <- as.data.frame(loadings_persona_GPT4_ext)
loadings_shape_GPT4_ext <- as.data.frame(loadings_shape_GPT4_ext)
loadings_persona_LLaMA3_ext <- as.data.frame(loadings_persona_LLaMA3_ext)
loadings_shape_LLaMA3_ext <- as.data.frame(loadings_shape_LLaMA3_ext)

mae <- function(x, y) {
  mean(abs(x - y), na.rm = TRUE)
}

dimensions <- c("Sociability", "Assertiveness", "Energy_Level")

calculate_mae <- function(human_loadings, model_loadings, dimensions) {
  mae_values <- sapply(1:length(dimensions), function(i) {
    mae(human_loadings[[i]], model_loadings[[i]])
  })
  names(mae_values) <- dimensions
  return(mae_values)
}

# Calculate MAE for each model compared to the human baseline
mae_persona_GPT3.5 <- calculate_mae(loadings_normal_human_ext, loadings_persona_GPT3.5_ext, dimensions)
mae_shape_GPT3.5 <- calculate_mae(loadings_normal_human_ext, loadings_shape_GPT3.5_ext, dimensions)
mae_persona_GPT4 <- calculate_mae(loadings_normal_human_ext, loadings_persona_GPT4_ext, dimensions)
mae_shape_GPT4 <- calculate_mae(loadings_normal_human_ext, loadings_shape_GPT4_ext, dimensions)
mae_persona_LLaMA3 <- calculate_mae(loadings_normal_human_ext, loadings_persona_LLaMA3_ext, dimensions)
mae_shape_LLaMA3 <- calculate_mae(loadings_normal_human_ext, loadings_shape_LLaMA3_ext, dimensions)

# Display MAE results
mae_persona_GPT3.5
mae_shape_GPT3.5
mae_persona_GPT4
mae_shape_GPT4
mae_persona_LLaMA3
mae_shape_LLaMA3




# agr
loadings_normal_human_agr <- lavInspect(fit.normal_human.agr, "std")$lambda
loadings_persona_GPT3.5_agr <- lavInspect(fit.persona_GPT3.5.agr, "std")$lambda
loadings_shape_GPT3.5_agr <- lavInspect(fit.shape_GPT3.5.agr, "std")$lambda
loadings_persona_GPT4_agr <- lavInspect(fit.persona_GPT4.agr, "std")$lambda
loadings_shape_GPT4_agr <- lavInspect(fit.shape_GPT4.agr, "std")$lambda
loadings_persona_LLaMA3_agr <- lavInspect(fit.persona_LLaMA3.agr, "std")$lambda
loadings_shape_LLaMA3_agr <- lavInspect(fit.shape_LLaMA3.agr, "std")$lambda

tcc_normal_persona_GPT3.5 <- factor.congruence(loadings_normal_human_agr, loadings_persona_GPT3.5_agr)
tcc_normal_shape_GPT3.5 <- factor.congruence(loadings_normal_human_agr, loadings_shape_GPT3.5_agr)
tcc_normal_persona_GPT4 <- factor.congruence(loadings_normal_human_agr, loadings_persona_GPT4_agr)
tcc_normal_shape_GPT4 <- factor.congruence(loadings_normal_human_agr, loadings_shape_GPT4_agr)
tcc_normal_persona_LLaMA3 <- factor.congruence(loadings_normal_human_agr, loadings_persona_LLaMA3_agr)
tcc_normal_shape_LLaMA3 <- factor.congruence(loadings_normal_human_agr, loadings_shape_LLaMA3_agr)

tcc_normal_persona_GPT3.5
tcc_normal_shape_GPT3.5
tcc_normal_persona_GPT4
tcc_normal_shape_GPT4
tcc_normal_persona_LLaMA3
tcc_normal_shape_LLaMA3



loadings_normal_human_agr[loadings_normal_human_agr == 0] <- NA
loadings_persona_GPT3.5_agr[loadings_persona_GPT3.5_agr == 0] <- NA
loadings_shape_GPT3.5_agr[loadings_shape_GPT3.5_agr == 0] <- NA
loadings_persona_GPT4_agr[loadings_persona_GPT4_agr == 0] <- NA
loadings_shape_GPT4_agr[loadings_shape_GPT4_agr == 0] <- NA
loadings_persona_LLaMA3_agr[loadings_persona_LLaMA3_agr == 0] <- NA
loadings_shape_LLaMA3_agr[loadings_shape_LLaMA3_agr == 0] <- NA

loadings_normal_human_agr <- as.data.frame(loadings_normal_human_agr)
loadings_persona_GPT3.5_agr <- as.data.frame(loadings_persona_GPT3.5_agr)
loadings_shape_GPT3.5_agr <- as.data.frame(loadings_shape_GPT3.5_agr)
loadings_persona_GPT4_agr <- as.data.frame(loadings_persona_GPT4_agr)
loadings_shape_GPT4_agr <- as.data.frame(loadings_shape_GPT4_agr)
loadings_persona_LLaMA3_agr <- as.data.frame(loadings_persona_LLaMA3_agr)
loadings_shape_LLaMA3_agr <- as.data.frame(loadings_shape_LLaMA3_agr)

mae <- function(x, y) {
  mean(abs(x - y), na.rm = TRUE)
}

dimensions <- c("Compassion", "Respectfulness", "Trust")

calculate_mae <- function(human_loadings, model_loadings, dimensions) {
  mae_values <- sapply(1:length(dimensions), function(i) {
    mae(human_loadings[[i]], model_loadings[[i]])
  })
  names(mae_values) <- dimensions
  return(mae_values)
}

# Calculate MAE for each model compared to the human baseline
mae_persona_GPT3.5 <- calculate_mae(loadings_normal_human_agr, loadings_persona_GPT3.5_agr, dimensions)
mae_shape_GPT3.5 <- calculate_mae(loadings_normal_human_agr, loadings_shape_GPT3.5_agr, dimensions)
mae_persona_GPT4 <- calculate_mae(loadings_normal_human_agr, loadings_persona_GPT4_agr, dimensions)
mae_shape_GPT4 <- calculate_mae(loadings_normal_human_agr, loadings_shape_GPT4_agr, dimensions)
mae_persona_LLaMA3 <- calculate_mae(loadings_normal_human_agr, loadings_persona_LLaMA3_agr, dimensions)
mae_shape_LLaMA3 <- calculate_mae(loadings_normal_human_agr, loadings_shape_LLaMA3_agr, dimensions)

# Display MAE results
mae_persona_GPT3.5
mae_shape_GPT3.5
mae_persona_GPT4
mae_shape_GPT4
mae_persona_LLaMA3
mae_shape_LLaMA3











# con
loadings_normal_human_con <- lavInspect(fit.normal_human.con, "std")$lambda
loadings_persona_GPT3.5_con <- lavInspect(fit.persona_GPT3.5.con, "std")$lambda
loadings_shape_GPT3.5_con <- lavInspect(fit.shape_GPT3.5.con, "std")$lambda
loadings_persona_GPT4_con <- lavInspect(fit.persona_GPT4.con, "std")$lambda
loadings_shape_GPT4_con <- lavInspect(fit.shape_GPT4.con, "std")$lambda
loadings_persona_LLaMA3_con <- lavInspect(fit.persona_LLaMA3.con, "std")$lambda
loadings_shape_LLaMA3_con <- lavInspect(fit.shape_LLaMA3.con, "std")$lambda

tcc_normal_persona_GPT3.5 <- factor.congruence(loadings_normal_human_con, loadings_persona_GPT3.5_con)
tcc_normal_shape_GPT3.5 <- factor.congruence(loadings_normal_human_con, loadings_shape_GPT3.5_con)
tcc_normal_persona_GPT4 <- factor.congruence(loadings_normal_human_con, loadings_persona_GPT4_con)
tcc_normal_shape_GPT4 <- factor.congruence(loadings_normal_human_con, loadings_shape_GPT4_con)
tcc_normal_persona_LLaMA3 <- factor.congruence(loadings_normal_human_con, loadings_persona_LLaMA3_con)
tcc_normal_shape_LLaMA3 <- factor.congruence(loadings_normal_human_con, loadings_shape_LLaMA3_con)

tcc_normal_persona_GPT3.5
tcc_normal_shape_GPT3.5
tcc_normal_persona_GPT4
tcc_normal_shape_GPT4
tcc_normal_persona_LLaMA3
tcc_normal_shape_LLaMA3




loadings_normal_human_con[loadings_normal_human_con == 0] <- NA
loadings_persona_GPT3.5_con[loadings_persona_GPT3.5_con == 0] <- NA
loadings_shape_GPT3.5_con[loadings_shape_GPT3.5_con == 0] <- NA
loadings_persona_GPT4_con[loadings_persona_GPT4_con == 0] <- NA
loadings_shape_GPT4_con[loadings_shape_GPT4_con == 0] <- NA
loadings_persona_LLaMA3_con[loadings_persona_LLaMA3_con == 0] <- NA
loadings_shape_LLaMA3_con[loadings_shape_LLaMA3_con == 0] <- NA

loadings_normal_human_con <- as.data.frame(loadings_normal_human_con)
loadings_persona_GPT3.5_con <- as.data.frame(loadings_persona_GPT3.5_con)
loadings_shape_GPT3.5_con <- as.data.frame(loadings_shape_GPT3.5_con)
loadings_persona_GPT4_con <- as.data.frame(loadings_persona_GPT4_con)
loadings_shape_GPT4_con <- as.data.frame(loadings_shape_GPT4_con)
loadings_persona_LLaMA3_con <- as.data.frame(loadings_persona_LLaMA3_con)
loadings_shape_LLaMA3_con <- as.data.frame(loadings_shape_LLaMA3_con)

mae <- function(x, y) {
  mean(abs(x - y), na.rm = TRUE)
}

dimensions <- c("Organization", "Productiveness", "Responsibility")

calculate_mae <- function(human_loadings, model_loadings, dimensions) {
  mae_values <- sapply(1:length(dimensions), function(i) {
    mae(human_loadings[[i]], model_loadings[[i]])
  })
  names(mae_values) <- dimensions
  return(mae_values)
}

# Calculate MAE for each model compared to the human baseline
mae_persona_GPT3.5 <- calculate_mae(loadings_normal_human_con, loadings_persona_GPT3.5_con, dimensions)
mae_shape_GPT3.5 <- calculate_mae(loadings_normal_human_con, loadings_shape_GPT3.5_con, dimensions)
mae_persona_GPT4 <- calculate_mae(loadings_normal_human_con, loadings_persona_GPT4_con, dimensions)
mae_shape_GPT4 <- calculate_mae(loadings_normal_human_con, loadings_shape_GPT4_con, dimensions)
mae_persona_LLaMA3 <- calculate_mae(loadings_normal_human_con, loadings_persona_LLaMA3_con, dimensions)
mae_shape_LLaMA3 <- calculate_mae(loadings_normal_human_con, loadings_shape_LLaMA3_con, dimensions)

# Display MAE results
mae_persona_GPT3.5
mae_shape_GPT3.5
mae_persona_GPT4
mae_shape_GPT4
mae_persona_LLaMA3
mae_shape_LLaMA3







# neu
loadings_normal_human_neu <- lavInspect(fit.normal_human.neu, "std")$lambda
loadings_persona_GPT3.5_neu <- lavInspect(fit.persona_GPT3.5.neu, "std")$lambda
loadings_shape_GPT3.5_neu <- lavInspect(fit.shape_GPT3.5.neu, "std")$lambda
loadings_persona_GPT4_neu <- lavInspect(fit.persona_GPT4.neu, "std")$lambda
loadings_shape_GPT4_neu <- lavInspect(fit.shape_GPT4.neu, "std")$lambda
loadings_persona_LLaMA3_neu <- lavInspect(fit.persona_LLaMA3.neu, "std")$lambda
loadings_shape_LLaMA3_neu <- lavInspect(fit.shape_LLaMA3.neu, "std")$lambda

tcc_normal_persona_GPT3.5 <- factor.congruence(loadings_normal_human_neu, loadings_persona_GPT3.5_neu)
tcc_normal_shape_GPT3.5 <- factor.congruence(loadings_normal_human_neu, loadings_shape_GPT3.5_neu)
tcc_normal_persona_GPT4 <- factor.congruence(loadings_normal_human_neu, loadings_persona_GPT4_neu)
tcc_normal_shape_GPT4 <- factor.congruence(loadings_normal_human_neu, loadings_shape_GPT4_neu)
tcc_normal_persona_LLaMA3 <- factor.congruence(loadings_normal_human_neu, loadings_persona_LLaMA3_neu)
tcc_normal_shape_LLaMA3 <- factor.congruence(loadings_normal_human_neu, loadings_shape_LLaMA3_neu)

tcc_normal_persona_GPT3.5
tcc_normal_shape_GPT3.5
tcc_normal_persona_GPT4
tcc_normal_shape_GPT4
tcc_normal_persona_LLaMA3
tcc_normal_shape_LLaMA3



loadings_normal_human_neu[loadings_normal_human_neu == 0] <- NA
loadings_persona_GPT3.5_neu[loadings_persona_GPT3.5_neu == 0] <- NA
loadings_shape_GPT3.5_neu[loadings_shape_GPT3.5_neu == 0] <- NA
loadings_persona_GPT4_neu[loadings_persona_GPT4_neu == 0] <- NA
loadings_shape_GPT4_neu[loadings_shape_GPT4_neu == 0] <- NA
loadings_persona_LLaMA3_neu[loadings_persona_LLaMA3_neu == 0] <- NA
loadings_shape_LLaMA3_neu[loadings_shape_LLaMA3_neu == 0] <- NA

loadings_normal_human_neu <- as.data.frame(loadings_normal_human_neu)
loadings_persona_GPT3.5_neu <- as.data.frame(loadings_persona_GPT3.5_neu)
loadings_shape_GPT3.5_neu <- as.data.frame(loadings_shape_GPT3.5_neu)
loadings_persona_GPT4_neu <- as.data.frame(loadings_persona_GPT4_neu)
loadings_shape_GPT4_neu <- as.data.frame(loadings_shape_GPT4_neu)
loadings_persona_LLaMA3_neu <- as.data.frame(loadings_persona_LLaMA3_neu)
loadings_shape_LLaMA3_neu <- as.data.frame(loadings_shape_LLaMA3_neu)

mae <- function(x, y) {
  mean(abs(x - y), na.rm = TRUE)
}

dimensions <- c("Anxiety", "Depression", "Emotional_Volatility")

calculate_mae <- function(human_loadings, model_loadings, dimensions) {
  mae_values <- sapply(1:length(dimensions), function(i) {
    mae(human_loadings[[i]], model_loadings[[i]])
  })
  names(mae_values) <- dimensions
  return(mae_values)
}

# Calculate MAE for each model compared to the human baseline
mae_persona_GPT3.5 <- calculate_mae(loadings_normal_human_neu, loadings_persona_GPT3.5_neu, dimensions)
mae_shape_GPT3.5 <- calculate_mae(loadings_normal_human_neu, loadings_shape_GPT3.5_neu, dimensions)
mae_persona_GPT4 <- calculate_mae(loadings_normal_human_neu, loadings_persona_GPT4_neu, dimensions)
mae_shape_GPT4 <- calculate_mae(loadings_normal_human_neu, loadings_shape_GPT4_neu, dimensions)
mae_persona_LLaMA3 <- calculate_mae(loadings_normal_human_neu, loadings_persona_LLaMA3_neu, dimensions)
mae_shape_LLaMA3 <- calculate_mae(loadings_normal_human_neu, loadings_shape_LLaMA3_neu, dimensions)

# Display MAE results
mae_persona_GPT3.5
mae_shape_GPT3.5
mae_persona_GPT4
mae_shape_GPT4
mae_persona_LLaMA3
mae_shape_LLaMA3






# ope
loadings_normal_human_ope <- lavInspect(fit.normal_human.ope, "std")$lambda
loadings_persona_GPT3.5_ope <- lavInspect(fit.persona_GPT3.5.ope, "std")$lambda
loadings_shape_GPT3.5_ope <- lavInspect(fit.shape_GPT3.5.ope, "std")$lambda
loadings_persona_GPT4_ope <- lavInspect(fit.persona_GPT4.ope, "std")$lambda
loadings_shape_GPT4_ope <- lavInspect(fit.shape_GPT4.ope, "std")$lambda
loadings_persona_LLaMA3_ope <- lavInspect(fit.persona_LLaMA3.ope, "std")$lambda
loadings_shape_LLaMA3_ope <- lavInspect(fit.shape_LLaMA3.ope, "std")$lambda

tcc_normal_persona_GPT3.5 <- factor.congruence(loadings_normal_human_ope, loadings_persona_GPT3.5_ope)
tcc_normal_shape_GPT3.5 <- factor.congruence(loadings_normal_human_ope, loadings_shape_GPT3.5_ope)
tcc_normal_persona_GPT4 <- factor.congruence(loadings_normal_human_ope, loadings_persona_GPT4_ope)
tcc_normal_shape_GPT4 <- factor.congruence(loadings_normal_human_ope, loadings_shape_GPT4_ope)
tcc_normal_persona_LLaMA3 <- factor.congruence(loadings_normal_human_ope, loadings_persona_LLaMA3_ope)
tcc_normal_shape_LLaMA3 <- factor.congruence(loadings_normal_human_ope, loadings_shape_LLaMA3_ope)

tcc_normal_persona_GPT3.5
tcc_normal_shape_GPT3.5
tcc_normal_persona_GPT4
tcc_normal_shape_GPT4
tcc_normal_persona_LLaMA3
tcc_normal_shape_LLaMA3



loadings_normal_human_ope[loadings_normal_human_ope == 0] <- NA
loadings_persona_GPT3.5_ope[loadings_persona_GPT3.5_ope == 0] <- NA
loadings_shape_GPT3.5_ope[loadings_shape_GPT3.5_ope == 0] <- NA
loadings_persona_GPT4_ope[loadings_persona_GPT4_ope == 0] <- NA
loadings_shape_GPT4_ope[loadings_shape_GPT4_ope == 0] <- NA
loadings_persona_LLaMA3_ope[loadings_persona_LLaMA3_ope == 0] <- NA
loadings_shape_LLaMA3_ope[loadings_shape_LLaMA3_ope == 0] <- NA

loadings_normal_human_ope <- as.data.frame(loadings_normal_human_ope)
loadings_persona_GPT3.5_ope <- as.data.frame(loadings_persona_GPT3.5_ope)
loadings_shape_GPT3.5_ope <- as.data.frame(loadings_shape_GPT3.5_ope)
loadings_persona_GPT4_ope <- as.data.frame(loadings_persona_GPT4_ope)
loadings_shape_GPT4_ope <- as.data.frame(loadings_shape_GPT4_ope)
loadings_persona_LLaMA3_ope <- as.data.frame(loadings_persona_LLaMA3_ope)
loadings_shape_LLaMA3_ope <- as.data.frame(loadings_shape_LLaMA3_ope)

mae <- function(x, y) {
  mean(abs(x - y), na.rm = TRUE)
}

dimensions <- c("Intellectual_Curiosity", "Aesthetic_Sensitivity", "Creative_Imagination")

calculate_mae <- function(human_loadings, model_loadings, dimensions) {
  mae_values <- sapply(1:length(dimensions), function(i) {
    mae(human_loadings[[i]], model_loadings[[i]])
  })
  names(mae_values) <- dimensions
  return(mae_values)
}

# Calculate MAE for each model compared to the human baseline
mae_persona_GPT3.5 <- calculate_mae(loadings_normal_human_ope, loadings_persona_GPT3.5_ope, dimensions)
mae_shape_GPT3.5 <- calculate_mae(loadings_normal_human_ope, loadings_shape_GPT3.5_ope, dimensions)
mae_persona_GPT4 <- calculate_mae(loadings_normal_human_ope, loadings_persona_GPT4_ope, dimensions)
mae_shape_GPT4 <- calculate_mae(loadings_normal_human_ope, loadings_shape_GPT4_ope, dimensions)
mae_persona_LLaMA3 <- calculate_mae(loadings_normal_human_ope, loadings_persona_LLaMA3_ope, dimensions)
mae_shape_LLaMA3 <- calculate_mae(loadings_normal_human_ope, loadings_shape_LLaMA3_ope, dimensions)

# Display MAE results
mae_persona_GPT3.5
mae_shape_GPT3.5
mae_persona_GPT4
mae_shape_GPT4
mae_persona_LLaMA3
mae_shape_LLaMA3







### five factor model

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

data_old_people$Sociability <- calculate_dimension_score(data_old_people, Sociability) 
data_old_people$Assertiveness <- calculate_dimension_score(data_old_people, Assertiveness)
data_old_people$Energy_Level <- calculate_dimension_score(data_old_people, Energy_Level)
data_old_people$Compassion <- calculate_dimension_score(data_old_people, Compassion)
data_old_people$Respectfulness <- calculate_dimension_score(data_old_people, Respectfulness)
data_old_people$Trust <- calculate_dimension_score(data_old_people, Trust)
data_old_people$Organization <- calculate_dimension_score(data_old_people, Organization)
data_old_people$Productiveness <- calculate_dimension_score(data_old_people, Productiveness)
data_old_people$Responsibility <- calculate_dimension_score(data_old_people, Responsibility)
data_old_people$Anxiety <- calculate_dimension_score(data_old_people, Anxiety)
data_old_people$Depression <- calculate_dimension_score(data_old_people, Depression)
data_old_people$Emotional_Volatility <- calculate_dimension_score(data_old_people, Emotional_Volatility)
data_old_people$Intellectual_Curiosity <- calculate_dimension_score(data_old_people, Intellectual_Curiosity)
data_old_people$Aesthetic_Sensitivity <- calculate_dimension_score(data_old_people, Aesthetic_Sensitivity)
data_old_people$Creative_Imagination <- calculate_dimension_score(data_old_people, Creative_Imagination)

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



mod.all <- "Extraversion =~ Sociability+Assertiveness+Energy_Level
            Agreeableness =~ Compassion+Respectfulness+Trust
            Conscientiousness =~ Organization+Productiveness+Responsibility
            Neuroticism  =~ Anxiety+Depression+Emotional_Volatility
            Openness =~ Intellectual_Curiosity+Aesthetic_Sensitivity+Creative_Imagination"


fit.normal_human <- lavaan::sem(mod.all,data = data_normal_human,std.lv=T)
fit.old_people <- lavaan::sem(mod.all,data = data_old_people,std.lv=T)
fit.persona_GPT3.5 <- lavaan::sem(mod.all,data = data_persona_GPT3.5,std.lv=T)
fit.shape_GPT3.5 <- lavaan::sem(mod.all,data = data_shape_GPT3.5,std.lv=T)
fit.persona_GPT4 <- lavaan::sem(mod.all,data = data_persona_GPT4,std.lv=T)
fit.shape_GPT4 <- lavaan::sem(mod.all,data = data_shape_GPT4,std.lv=T)
fit.persona_LLaMA3 <- lavaan::sem(mod.all,data = data_persona_LLaMA3,std.lv=T)
fit.shape_LLaMA3 <- lavaan::sem(mod.all,data = data_shape_LLaMA3,std.lv=T)

mod.fit <- t(data.frame(
  normal_human = fitMeasures(fit.normal_human, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  old_people = fitMeasures(fit.old_people, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  persona_GPT3.5 = fitMeasures(fit.persona_GPT3.5, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  shape_GPT3.5 = fitMeasures(fit.shape_GPT3.5, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  persona_GPT4 = fitMeasures(fit.persona_GPT4, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  shape_GPT4 = fitMeasures(fit.shape_GPT4, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  persona_LLaMA3 = fitMeasures(fit.persona_LLaMA3, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean")),
  shape_LLaMA3 = fitMeasures(fit.shape_LLaMA3, fit.measures = c("chisq","df", "cfi", "tli", "rmsea","srmr_bentler_nomean"))
))
#write.table(mod.fit,"mod.fit_all.csv",sep = ",")


# Factor loadings 
loadings <- data.frame(
  normal_human = lavaan::standardizedsolution(fit.normal_human)[1:15,"est.std"],
  old_people = lavaan::standardizedsolution(fit.old_people)[1:15,"est.std"],
  persona_GPT3.5 = lavaan::standardizedsolution(fit.persona_GPT3.5)[1:15,"est.std"],
  shape_GPT3.5 = lavaan::standardizedsolution(fit.shape_GPT3.5)[1:15,"est.std"],
  persona_GPT4 = lavaan::standardizedsolution(fit.persona_GPT4)[1:15,"est.std"],
  shape_GPT4 = lavaan::standardizedsolution(fit.shape_GPT4)[1:15,"est.std"],
  persona_LLaMA3 = lavaan::standardizedsolution(fit.persona_LLaMA3)[1:15,"est.std"],
  shape_LLaMA3 = lavaan::standardizedsolution(fit.shape_LLaMA3)[1:15,"est.std"]
)
#write.table(loadings,"loadings_all.csv",sep = ",")



# Inter-factor correlations
correlations <- data.frame(
  normal_human = lavaan::standardizedsolution(fit.normal_human)[31:45,"est.std"],
  old_people = lavaan::standardizedsolution(fit.old_people)[31:45,"est.std"],
  persona_GPT3.5 = lavaan::standardizedsolution(fit.persona_GPT3.5)[31:45,"est.std"],
  shape_GPT3.5 = lavaan::standardizedsolution(fit.shape_GPT3.5)[31:45,"est.std"],
  persona_GPT4 = lavaan::standardizedsolution(fit.persona_GPT4)[31:45,"est.std"],
  shape_GPT4 = lavaan::standardizedsolution(fit.shape_GPT4)[31:45,"est.std"],
  persona_LLaMA3 = lavaan::standardizedsolution(fit.persona_LLaMA3)[31:45,"est.std"],
  shape_LLaMA3 = lavaan::standardizedsolution(fit.shape_LLaMA3)[31:45,"est.std"]
)

#write.table(correlations,"correlations_all.csv",sep = ",")


### tcc
loadings_normal_human <- lavInspect(fit.normal_human, "std")$lambda
loadings_persona_GPT3.5 <- lavInspect(fit.persona_GPT3.5, "std")$lambda
loadings_shape_GPT3.5 <- lavInspect(fit.shape_GPT3.5, "std")$lambda
loadings_persona_GPT4 <- lavInspect(fit.persona_GPT4, "std")$lambda
loadings_shape_GPT4 <- lavInspect(fit.shape_GPT4, "std")$lambda
loadings_persona_LLaMA3 <- lavInspect(fit.persona_LLaMA3, "std")$lambda
loadings_shape_LLaMA3 <- lavInspect(fit.shape_LLaMA3, "std")$lambda

tcc_normal_persona_GPT3.5 <- factor.congruence(loadings_normal_human, loadings_persona_GPT3.5)
tcc_normal_shape_GPT3.5 <- factor.congruence(loadings_normal_human, loadings_shape_GPT3.5)
tcc_normal_persona_GPT4 <- factor.congruence(loadings_normal_human, loadings_persona_GPT4)
tcc_normal_shape_GPT4 <- factor.congruence(loadings_normal_human, loadings_shape_GPT4)
tcc_normal_persona_LLaMA3 <- factor.congruence(loadings_normal_human, loadings_persona_LLaMA3)
tcc_normal_shape_LLaMA3 <- factor.congruence(loadings_normal_human, loadings_shape_LLaMA3)

tcc_normal_persona_GPT3.5
tcc_normal_shape_GPT3.5
tcc_normal_persona_GPT4
tcc_normal_shape_GPT4
tcc_normal_persona_LLaMA3
tcc_normal_shape_LLaMA3




loadings_normal_human[loadings_normal_human == 0] <- NA
loadings_persona_GPT3.5[loadings_persona_GPT3.5 == 0] <- NA
loadings_shape_GPT3.5[loadings_shape_GPT3.5 == 0] <- NA
loadings_persona_GPT4[loadings_persona_GPT4 == 0] <- NA
loadings_shape_GPT4[loadings_shape_GPT4 == 0] <- NA
loadings_persona_LLaMA3[loadings_persona_LLaMA3 == 0] <- NA
loadings_shape_LLaMA3[loadings_shape_LLaMA3 == 0] <- NA

loadings_normal_human <- as.data.frame(loadings_normal_human)
loadings_persona_GPT3.5 <- as.data.frame(loadings_persona_GPT3.5)
loadings_shape_GPT3.5 <- as.data.frame(loadings_shape_GPT3.5)
loadings_persona_GPT4 <- as.data.frame(loadings_persona_GPT4)
loadings_shape_GPT4 <- as.data.frame(loadings_shape_GPT4)
loadings_persona_LLaMA3 <- as.data.frame(loadings_persona_LLaMA3)
loadings_shape_LLaMA3 <- as.data.frame(loadings_shape_LLaMA3)

mae <- function(x, y) {
  mean(abs(x - y), na.rm = TRUE)
}

dimensions <- c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")

calculate_mae <- function(human_loadings, model_loadings, dimensions) {
  mae_values <- sapply(1:length(dimensions), function(i) {
    mae(human_loadings[[i]], model_loadings[[i]])
  })
  names(mae_values) <- dimensions
  return(mae_values)
}

# Calculate MAE for each model compared to the human baseline
mae_persona_GPT3.5 <- calculate_mae(loadings_normal_human, loadings_persona_GPT3.5, dimensions)
mae_shape_GPT3.5 <- calculate_mae(loadings_normal_human, loadings_shape_GPT3.5, dimensions)
mae_persona_GPT4 <- calculate_mae(loadings_normal_human, loadings_persona_GPT4, dimensions)
mae_shape_GPT4 <- calculate_mae(loadings_normal_human, loadings_shape_GPT4, dimensions)
mae_persona_LLaMA3 <- calculate_mae(loadings_normal_human, loadings_persona_LLaMA3, dimensions)
mae_shape_LLaMA3 <- calculate_mae(loadings_normal_human, loadings_shape_LLaMA3, dimensions)

# Display MAE results
mae_persona_GPT3.5
mae_shape_GPT3.5
mae_persona_GPT4
mae_shape_GPT4
mae_persona_LLaMA3
mae_shape_LLaMA3









#######################social_desirability###################
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

# item.results.normal_human <- describeBy(data_normal_human)
# item.results.old_people <- describeBy(data_old_people)
# item.results.persona_GPT3.5 <- describeBy(data_persona_GPT3.5)
# item.results.shape_GPT3.5 <- describeBy(data_shape_GPT3.5)
# item.results.persona_GPT4 <- describeBy(data_persona_GPT4)
# item.results.shape_GPT4 <- describeBy(data_shape_GPT4)
# item.results.persona_LLaMA3 <- describeBy(data_persona_LLaMA3)
# item.results.shape_LLaMA3 <- describeBy(data_shape_LLaMA3)
# 
# table.item <- data.frame(normal_human.mean = item.results.normal_human[,c("mean")],
#                          old_people.mean = item.results.old_people[,c("mean")],
# 
#                          persona_GPT3.5.mean = item.results.persona_GPT3.5[,c("mean")],
#                          shape_GPT3.5.mean = item.results.shape_GPT3.5[,c("mean")],
# 
#                          persona_GPT4.mean = item.results.persona_GPT4[,c("mean")],
#                          shape_GPT4.mean = item.results.shape_GPT4[,c("mean")],
# 
#                          persona_LLaMA3.mean = item.results.persona_LLaMA3[,c("mean")],
#                          shape_LLaMA3.mean = item.results.shape_LLaMA3[,c("mean")],
# 
#                          normal_human.sd = item.results.normal_human[,c("sd")],
#                          old_people.sd = item.results.old_people[,c("sd")],
# 
#                          persona_GPT3.5.sd = item.results.persona_GPT3.5[,c("sd")],
#                          shape_GPT3.5.sd = item.results.shape_GPT3.5[,c("sd")],
# 
#                          persona_GPT4.sd = item.results.persona_GPT4[,c("sd")],
#                          shape_GPT4.sd = item.results.shape_GPT4[,c("sd")],
#                          persona_LLaMA3.sd = item.results.persona_LLaMA3[,c("sd")],
#                          shape_LLaMA3.sd = item.results.shape_LLaMA3[,c("sd")]
# )
# 
# # write in csv file
# write.table(table.item,"table.item.csv",sep = ",")


data_social_desirability <- read.table("Data/social_desirability.csv",sep = ",",header = T)


correlation_matrix <- cor(data_social_desirability[c("Mean_SocialD", 
                                                     "MSDpersona_GPT3.5.mean", "MSDshape_GPT3.5.mean", 
                                                     "MSDpersona_GPT4.mean", "MSDshape_GPT4.mean", 
                                                     "MSDpersona_LLaMA3.mean", "MSDshape_LLaMA3.mean")])

print(correlation_matrix)


correlation_matrix <- cor(data_social_desirability[c("Mean_SocialD", "normal_human.mean",
                                                     "persona_GPT3.5.mean", "shape_GPT3.5.mean", 
                                                     "persona_GPT4.mean", "shape_GPT4.mean", 
                                                     "persona_LLaMA3.mean", "shape_LLaMA3.mean")])

print(correlation_matrix)




dependent_vars <- c("normal_human.mean", "persona_GPT3.5.mean", "shape_GPT3.5.mean", 
                    "persona_GPT4.mean", "shape_GPT4.mean", 
                    "persona_LLaMA3.mean", "shape_LLaMA3.mean")

for (var in dependent_vars) {
  formula <- as.formula(paste(var, "~ Mean_SocialD"))
  model <- lm(formula, data = data_social_desirability)
  cat("Regression for", var, ":\n")
  print(summary(model))
  cat("\n\n")
}



dependent_vars <- c("MSDpersona_GPT3.5.mean", "MSDshape_GPT3.5.mean", 
                    "MSDpersona_GPT4.mean", "MSDshape_GPT4.mean", 
                    "MSDpersona_LLaMA3.mean", "MSDshape_LLaMA3.mean")

for (var in dependent_vars) {
  formula <- as.formula(paste(var, "~ Mean_SocialD"))
  model <- lm(formula, data = data_social_desirability)
  cat("Regression for", var, ":\n")
  print(summary(model))
  cat("\n\n")
}

















