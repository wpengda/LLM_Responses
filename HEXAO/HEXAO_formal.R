# Pengda Wang

library(psych)
library(lavaan)
library(Metrics)


# data_human <- read.table("Data/reordered_hexaco_human.csv",sep = ",",header = T)
data_persona_GPT3.5 <- read.table("Data/GPT_3.5/persona_hexaco.csv", sep = ",", header = TRUE, quote = "\"", dec = ".", fill = TRUE)
data_shape_GPT3.5 <- read.table("Data/GPT_3.5/shape_hexaco_100.csv", sep = ",", header = TRUE, quote = "\"", dec = ".", fill = TRUE)

# four data to compare
data_honest <- subset(read.csv("Data/reordered_hexaco_human.csv",sep = ","),sample!="industry")[,5:104]
data_faking <- subset(read.csv("Data/reordered_hexaco_human.csv",sep = ","),sample=="industry")[,5:104]
data_persona_GPT3.5 <- data_persona_GPT3.5[2: 101]
data_shape_GPT3.5 <- data_shape_GPT3.5[2: 101]

reverse_code_columns <- c(6, 54, 12, 36, 84, 42, 66, 90, 72, 96, 29, 77, 35, 59, 41, 89, 95, 52, 76, 10,
                          82, 16, 70, 94, 51, 75, 9, 15, 63, 87, 21, 93, 50, 74, 56, 80, 38, 20, 44, 92,
                          1, 25, 55, 79, 13, 85, 19, 91, 99, 100)

for (col in reverse_code_columns) {
  data_honest[, col] <- 6 - data_honest[, col]
  data_faking[, col] <- 6 - data_faking[, col]
  data_persona_GPT3.5[, col] <- 6 - data_persona_GPT3.5[, col]
  data_shape_GPT3.5[, col] <- 6 - data_shape_GPT3.5[, col]
}

# Facet
Sincerity <- c(6, 30, 54, 78)
Fairness <- c(12, 36, 60, 84)
Greed_avoidance <- c(18, 42, 66, 90)
Modesty <- c(24, 48, 72, 96)

Fearfulness <- c(5, 29, 53, 77)
Anxiety <- c(11, 35, 59, 83)
Dependence <- c(17, 41, 65, 89)
Sentimentality <- c(23, 47, 71, 95)

Social_self_esteem <- c(4, 28, 52, 76)
Social_boldness <- c(10, 34, 58, 82)
Sociability <- c(16, 40, 64, 88)
Liveliness <- c(22, 46, 70, 94)

Forgiveness <- c(3, 27, 51, 75)
Gentleness <- c(9, 33, 57, 81)
Flexibility <- c(15, 39, 63, 87)
Patience <- c(21, 45, 69, 93)

Organization <- c(2, 26, 50, 74)
Diligence <- c(8, 32, 56, 80)
Perfectionism <- c(14, 38, 62, 86)
Prudence <- c(20, 44, 68, 92)

Aesthetic_appreciation <- c(1, 25, 49, 73)
Inquisitiveness <- c(7, 31, 55, 79)
Creativity <- c(13, 37, 61, 85)
Unconventionality <- c(19, 43, 67, 91)

Altruism <- c(97, 98, 99, 100)

# Domain
Honesty_humility <- c(6, 30, 54, 78, 12, 36, 60, 84, 18, 42, 66, 90, 24, 48, 72, 96)
Emotionality <- c(5, 29, 53, 77, 11, 35, 59, 83, 17, 41, 65, 89, 23, 47, 71, 95)
Extraversion <- c(4, 28, 52, 76, 10, 34, 58, 82, 16, 40, 64, 88, 22, 46, 70, 94)
Agreeableness <- c(3, 27, 51, 75, 9, 33, 57, 81, 15, 39, 63, 87, 21, 45, 69, 93)
Conscientiousness <- c(2, 26, 50, 74, 8, 32, 56, 80, 14, 38, 62, 86, 20, 44, 68, 92)
Openness_to_experience <- c(1, 25, 49, 73, 7, 31, 55, 79, 13, 37, 61, 85, 19, 43, 67, 91)




#### Item level result
item.results.honest <- describeBy(data_honest)
item.results.faking <- describeBy(data_faking)
item.results.persona_GPT3.5 <- describeBy(data_persona_GPT3.5)
item.results.shape_GPT3.5 <- describeBy(data_shape_GPT3.5)

table.item <- data.frame(
                         honest.mean = item.results.honest[,c("mean")],
                         faking.mean = item.results.faking[,c("mean")],
                         persona_GPT3.5.mean = item.results.persona_GPT3.5[,c("mean")],
                         shape_GPT3.5.mean = item.results.shape_GPT3.5[,c("mean")],
                         
                         honest.mean = item.results.honest[,c("sd")],
                         faking.mean = item.results.faking[,c("sd")],
                         persona_GPT3.5.sd = item.results.persona_GPT3.5[,c("sd")],
                         shape_GPT3.5.sd = item.results.shape_GPT3.5[,c("sd")]
)

# write in csv file
write.table(table.item,"table.item.csv",sep = ",")




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


# Domain
alpha_honesty_humility_honest <- calculate_alpha(data_honest[, Honesty_humility])
alpha_emotionality_honest <- calculate_alpha(data_honest[, Emotionality])
alpha_extraversion_honest <- calculate_alpha(data_honest[, Extraversion])
alpha_agreeableness_honest <- calculate_alpha(data_honest[, Agreeableness])
alpha_conscientiousness_honest <- calculate_alpha(data_honest[, Conscientiousness])
alpha_openness_honest <- calculate_alpha(data_honest[, Openness_to_experience])

alpha_honesty_humility_faking <- calculate_alpha(data_faking[, Honesty_humility])
alpha_emotionality_faking <- calculate_alpha(data_faking[, Emotionality])
alpha_extraversion_faking <- calculate_alpha(data_faking[, Extraversion])
alpha_agreeableness_faking <- calculate_alpha(data_faking[, Agreeableness])
alpha_conscientiousness_faking <- calculate_alpha(data_faking[, Conscientiousness])
alpha_openness_faking <- calculate_alpha(data_faking[, Openness_to_experience])


alpha_honesty_humility_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Honesty_humility])
alpha_emotionality_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Emotionality])
alpha_extraversion_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Extraversion])
alpha_agreeableness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Agreeableness])
alpha_conscientiousness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Conscientiousness])
alpha_openness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Openness_to_experience])

alpha_honesty_humility_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Honesty_humility])
alpha_emotionality_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Emotionality])
alpha_extraversion_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Extraversion])
alpha_agreeableness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Agreeableness])
alpha_conscientiousness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Conscientiousness])
alpha_openness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Openness_to_experience])


# Facet
alpha_sincerity_honest <- calculate_alpha(data_honest[, Sincerity])
alpha_fairness_honest <- calculate_alpha(data_honest[, Fairness])
alpha_greed_avoidance_honest <- calculate_alpha(data_honest[, Greed_avoidance])
alpha_modesty_honest <- calculate_alpha(data_honest[, Modesty])
alpha_fearfulness_honest <- calculate_alpha(data_honest[, Fearfulness])
alpha_anxiety_honest <- calculate_alpha(data_honest[, Anxiety])
alpha_dependence_honest <- calculate_alpha(data_honest[, Dependence])
alpha_sentimentality_honest <- calculate_alpha(data_honest[, Sentimentality])
alpha_social_self_esteem_honest <- calculate_alpha(data_honest[, Social_self_esteem])
alpha_social_boldness_honest <- calculate_alpha(data_honest[, Social_boldness])
alpha_sociability_honest <- calculate_alpha(data_honest[, Sociability])
alpha_liveliness_honest <- calculate_alpha(data_honest[, Liveliness])
alpha_forgiveness_honest <- calculate_alpha(data_honest[, Forgiveness])
alpha_gentleness_honest <- calculate_alpha(data_honest[, Gentleness])
alpha_flexibility_honest <- calculate_alpha(data_honest[, Flexibility])
alpha_patience_honest <- calculate_alpha(data_honest[, Patience])
alpha_organization_honest <- calculate_alpha(data_honest[, Organization])
alpha_diligence_honest <- calculate_alpha(data_honest[, Diligence])
alpha_perfectionism_honest <- calculate_alpha(data_honest[, Perfectionism])
alpha_prudence_honest <- calculate_alpha(data_honest[, Prudence])
alpha_aesthetic_appreciation_honest <- calculate_alpha(data_honest[, Aesthetic_appreciation])
alpha_inquisitiveness_honest <- calculate_alpha(data_honest[, Inquisitiveness])
alpha_creativity_honest <- calculate_alpha(data_honest[, Creativity])
alpha_unconventionality_honest <- calculate_alpha(data_honest[, Unconventionality])
alpha_altruism_honest <- calculate_alpha(data_honest[, Altruism])

alpha_sincerity_faking <- calculate_alpha(data_faking[, Sincerity])
alpha_fairness_faking <- calculate_alpha(data_faking[, Fairness])
alpha_greed_avoidance_faking <- calculate_alpha(data_faking[, Greed_avoidance])
alpha_modesty_faking <- calculate_alpha(data_faking[, Modesty])
alpha_fearfulness_faking <- calculate_alpha(data_faking[, Fearfulness])
alpha_anxiety_faking <- calculate_alpha(data_faking[, Anxiety])
alpha_dependence_faking <- calculate_alpha(data_faking[, Dependence])
alpha_sentimentality_faking <- calculate_alpha(data_faking[, Sentimentality])
alpha_social_self_esteem_faking <- calculate_alpha(data_faking[, Social_self_esteem])
alpha_social_boldness_faking <- calculate_alpha(data_faking[, Social_boldness])
alpha_sociability_faking <- calculate_alpha(data_faking[, Sociability])
alpha_liveliness_faking <- calculate_alpha(data_faking[, Liveliness])
alpha_forgiveness_faking <- calculate_alpha(data_faking[, Forgiveness])
alpha_gentleness_faking <- calculate_alpha(data_faking[, Gentleness])
alpha_flexibility_faking <- calculate_alpha(data_faking[, Flexibility])
alpha_patience_faking <- calculate_alpha(data_faking[, Patience])
alpha_organization_faking <- calculate_alpha(data_faking[, Organization])
alpha_diligence_faking <- calculate_alpha(data_faking[, Diligence])
alpha_perfectionism_faking <- calculate_alpha(data_faking[, Perfectionism])
alpha_prudence_faking <- calculate_alpha(data_faking[, Prudence])
alpha_aesthetic_appreciation_faking <- calculate_alpha(data_faking[, Aesthetic_appreciation])
alpha_inquisitiveness_faking <- calculate_alpha(data_faking[, Inquisitiveness])
alpha_creativity_faking <- calculate_alpha(data_faking[, Creativity])
alpha_unconventionality_faking <- calculate_alpha(data_faking[, Unconventionality])
alpha_altruism_faking <- calculate_alpha(data_faking[, Altruism])

alpha_sincerity_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Sincerity])
alpha_fairness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Fairness])
alpha_greed_avoidance_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Greed_avoidance])
alpha_modesty_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Modesty])
alpha_fearfulness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Fearfulness])
alpha_anxiety_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Anxiety])
alpha_dependence_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Dependence])
alpha_sentimentality_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Sentimentality])
alpha_social_self_esteem_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Social_self_esteem])
alpha_social_boldness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Social_boldness])
alpha_sociability_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Sociability])
alpha_liveliness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Liveliness])
alpha_forgiveness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Forgiveness])
alpha_gentleness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Gentleness])
alpha_flexibility_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Flexibility])
alpha_patience_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Patience])
alpha_organization_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Organization])
alpha_diligence_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Diligence])
alpha_perfectionism_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Perfectionism])
alpha_prudence_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Prudence])
alpha_aesthetic_appreciation_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Aesthetic_appreciation])
alpha_inquisitiveness_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Inquisitiveness])
alpha_creativity_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Creativity])
alpha_unconventionality_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Unconventionality])
alpha_altruism_persona_GPT3.5 <- calculate_alpha(data_persona_GPT3.5[, Altruism])

alpha_sincerity_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Sincerity])
alpha_fairness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Fairness])
alpha_greed_avoidance_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Greed_avoidance])
alpha_modesty_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Modesty])
alpha_fearfulness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Fearfulness])
alpha_anxiety_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Anxiety])
alpha_dependence_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Dependence])
alpha_sentimentality_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Sentimentality])
alpha_social_self_esteem_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Social_self_esteem])
alpha_social_boldness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Social_boldness])
alpha_sociability_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Sociability])
alpha_liveliness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Liveliness])
alpha_forgiveness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Forgiveness])
alpha_gentleness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Gentleness])
alpha_flexibility_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Flexibility])
alpha_patience_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Patience])
alpha_organization_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Organization])
alpha_diligence_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Diligence])
alpha_perfectionism_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Perfectionism])
alpha_prudence_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Prudence])
alpha_aesthetic_appreciation_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Aesthetic_appreciation])
alpha_inquisitiveness_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Inquisitiveness])
alpha_creativity_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Creativity])
alpha_unconventionality_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Unconventionality])
alpha_altruism_shape_GPT3.5 <- calculate_alpha(data_shape_GPT3.5[, Altruism])


result_df <- data.frame(
  Trait = c("Sincerity", "Fairness", "Greed Avoidance", "Modesty",
            "Fearfulness", "Anxiety", "Dependence", "Sentimentality",
            "Social Self-Esteem", "Social Boldness", "Sociability", "Liveliness",
            "Forgiveness", "Gentleness", "Flexibility", "Patience",
            "Organization", "Diligence", "Perfectionism", "Prudence",
            "Aesthetic Appreciation", "Inquisitiveness", "Creativity", "Unconventionality",
            "Altruism"),
  honest = c(alpha_sincerity_honest, alpha_fairness_honest, alpha_greed_avoidance_honest, alpha_modesty_honest,
                   alpha_fearfulness_honest, alpha_anxiety_honest, alpha_dependence_honest, alpha_sentimentality_honest,
                   alpha_social_self_esteem_honest, alpha_social_boldness_honest, alpha_sociability_honest, alpha_liveliness_honest,
                   alpha_forgiveness_honest, alpha_gentleness_honest, alpha_flexibility_honest, alpha_patience_honest,
                   alpha_organization_honest, alpha_diligence_honest, alpha_perfectionism_honest, alpha_prudence_honest,
                   alpha_aesthetic_appreciation_honest, alpha_inquisitiveness_honest, alpha_creativity_honest, alpha_unconventionality_honest,
                   alpha_altruism_honest),
  faking = c(alpha_sincerity_faking, alpha_fairness_faking, alpha_greed_avoidance_faking, alpha_modesty_faking,
                 alpha_fearfulness_faking, alpha_anxiety_faking, alpha_dependence_faking, alpha_sentimentality_faking,
                 alpha_social_self_esteem_faking, alpha_social_boldness_faking, alpha_sociability_faking, alpha_liveliness_faking,
                 alpha_forgiveness_faking, alpha_gentleness_faking, alpha_flexibility_faking, alpha_patience_faking,
                 alpha_organization_faking, alpha_diligence_faking, alpha_perfectionism_faking, alpha_prudence_faking,
                 alpha_aesthetic_appreciation_faking, alpha_inquisitiveness_faking, alpha_creativity_faking, alpha_unconventionality_faking,
                 alpha_altruism_faking),
  persona_GPT3.5 = c(alpha_sincerity_persona_GPT3.5, alpha_fairness_persona_GPT3.5, alpha_greed_avoidance_persona_GPT3.5, alpha_modesty_persona_GPT3.5,
                     alpha_fearfulness_persona_GPT3.5, alpha_anxiety_persona_GPT3.5, alpha_dependence_persona_GPT3.5, alpha_sentimentality_persona_GPT3.5,
                     alpha_social_self_esteem_persona_GPT3.5, alpha_social_boldness_persona_GPT3.5, alpha_sociability_persona_GPT3.5, alpha_liveliness_persona_GPT3.5,
                     alpha_forgiveness_persona_GPT3.5, alpha_gentleness_persona_GPT3.5, alpha_flexibility_persona_GPT3.5, alpha_patience_persona_GPT3.5,
                     alpha_organization_persona_GPT3.5, alpha_diligence_persona_GPT3.5, alpha_perfectionism_persona_GPT3.5, alpha_prudence_persona_GPT3.5,
                     alpha_aesthetic_appreciation_persona_GPT3.5, alpha_inquisitiveness_persona_GPT3.5, alpha_creativity_persona_GPT3.5, alpha_unconventionality_persona_GPT3.5,
                     alpha_altruism_persona_GPT3.5),
  shape_GPT3.5 = c(alpha_sincerity_shape_GPT3.5, alpha_fairness_shape_GPT3.5, alpha_greed_avoidance_shape_GPT3.5, alpha_modesty_shape_GPT3.5,
                   alpha_fearfulness_shape_GPT3.5, alpha_anxiety_shape_GPT3.5, alpha_dependence_shape_GPT3.5, alpha_sentimentality_shape_GPT3.5,
                   alpha_social_self_esteem_shape_GPT3.5, alpha_social_boldness_shape_GPT3.5, alpha_sociability_shape_GPT3.5, alpha_liveliness_shape_GPT3.5,
                   alpha_forgiveness_shape_GPT3.5, alpha_gentleness_shape_GPT3.5, alpha_flexibility_shape_GPT3.5, alpha_patience_shape_GPT3.5,
                   alpha_organization_shape_GPT3.5, alpha_diligence_shape_GPT3.5, alpha_perfectionism_shape_GPT3.5, alpha_prudence_shape_GPT3.5,
                   alpha_aesthetic_appreciation_shape_GPT3.5, alpha_inquisitiveness_shape_GPT3.5, alpha_creativity_shape_GPT3.5, alpha_unconventionality_shape_GPT3.5,
                   alpha_altruism_shape_GPT3.5)
)

# Write the data frame to a CSV file
write.table(result_df, "alpha_HEXACO.csv", sep = ",", row.names = FALSE)




#### Facet mean and sd
# honest
honest_facet_results <- data.frame(
  Trait = c("Sincerity", "Fairness", "Greed Avoidance", "Modesty",
            "Fearfulness", "Anxiety", "Dependence", "Sentimentality",
            "Social Self-Esteem", "Social Boldness", "Sociability", "Liveliness",
            "Forgiveness", "Gentleness", "Flexibility", "Patience",
            "Organization", "Diligence", "Perfectionism", "Prudence",
            "Aesthetic Appreciation", "Inquisitiveness", "Creativity", "Unconventionality",
            "Altruism"),
  Mean = c(
    mean(rowMeans(data_honest[, Sincerity], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Fairness], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Greed_avoidance], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Modesty], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Fearfulness], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Anxiety], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Dependence], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Sentimentality], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Social_self_esteem], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Social_boldness], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Sociability], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Liveliness], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Forgiveness], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Gentleness], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Flexibility], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Patience], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Organization], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Diligence], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Perfectionism], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Prudence], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Aesthetic_appreciation], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Inquisitiveness], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Creativity], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Unconventionality], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Altruism], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_honest[, Sincerity], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Fairness], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Greed_avoidance], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Modesty], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Fearfulness], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Anxiety], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Dependence], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Sentimentality], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Social_self_esteem], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Social_boldness], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Sociability], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Liveliness], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Forgiveness], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Gentleness], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Flexibility], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Patience], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Organization], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Diligence], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Perfectionism], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Prudence], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Aesthetic_appreciation], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Inquisitiveness], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Creativity], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Unconventionality], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Altruism], na.rm = TRUE))
  )
)

# write in csv file
write.table(honest_facet_results, "honest_HEXACO_facet_results.csv", sep = ",", row.names = FALSE)




# faking
faking_facet_results <- data.frame(
  Trait = c("Sincerity", "Fairness", "Greed Avoidance", "Modesty",
            "Fearfulness", "Anxiety", "Dependence", "Sentimentality",
            "Social Self-Esteem", "Social Boldness", "Sociability", "Liveliness",
            "Forgiveness", "Gentleness", "Flexibility", "Patience",
            "Organization", "Diligence", "Perfectionism", "Prudence",
            "Aesthetic Appreciation", "Inquisitiveness", "Creativity", "Unconventionality",
            "Altruism"),
  Mean = c(
    mean(rowMeans(data_faking[, Sincerity], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Fairness], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Greed_avoidance], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Modesty], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Fearfulness], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Anxiety], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Dependence], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Sentimentality], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Social_self_esteem], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Social_boldness], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Sociability], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Liveliness], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Forgiveness], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Gentleness], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Flexibility], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Patience], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Organization], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Diligence], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Perfectionism], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Prudence], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Aesthetic_appreciation], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Inquisitiveness], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Creativity], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Unconventionality], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Altruism], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_faking[, Sincerity], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Fairness], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Greed_avoidance], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Modesty], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Fearfulness], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Anxiety], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Dependence], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Sentimentality], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Social_self_esteem], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Social_boldness], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Sociability], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Liveliness], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Forgiveness], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Gentleness], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Flexibility], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Patience], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Organization], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Diligence], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Perfectionism], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Prudence], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Aesthetic_appreciation], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Inquisitiveness], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Creativity], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Unconventionality], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Altruism], na.rm = TRUE))
  )
)

# write in csv file
write.table(faking_facet_results, "faking_HEXACO_facet_results.csv", sep = ",", row.names = FALSE)



# persona
persona_GPT3.5_facet_results <- data.frame(
  Trait = c("Sincerity", "Fairness", "Greed Avoidance", "Modesty",
            "Fearfulness", "Anxiety", "Dependence", "Sentimentality",
            "Social Self-Esteem", "Social Boldness", "Sociability", "Liveliness",
            "Forgiveness", "Gentleness", "Flexibility", "Patience",
            "Organization", "Diligence", "Perfectionism", "Prudence",
            "Aesthetic Appreciation", "Inquisitiveness", "Creativity", "Unconventionality",
            "Altruism"),
  Mean = c(
    mean(rowMeans(data_persona_GPT3.5[, Sincerity], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Fairness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Greed_avoidance], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Modesty], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Fearfulness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Anxiety], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Dependence], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Sentimentality], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Social_self_esteem], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Social_boldness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Sociability], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Liveliness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Forgiveness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Gentleness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Flexibility], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Patience], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Organization], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Diligence], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Perfectionism], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Prudence], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Aesthetic_appreciation], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Inquisitiveness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Creativity], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Unconventionality], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Altruism], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_persona_GPT3.5[, Sincerity], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Fairness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Greed_avoidance], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Modesty], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Fearfulness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Anxiety], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Dependence], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Sentimentality], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Social_self_esteem], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Social_boldness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Sociability], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Liveliness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Forgiveness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Gentleness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Flexibility], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Patience], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Organization], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Diligence], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Perfectionism], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Prudence], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Aesthetic_appreciation], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Inquisitiveness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Creativity], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Unconventionality], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Altruism], na.rm = TRUE))
  )
)

# write in csv file
write.table(persona_GPT3.5_facet_results, "persona_GPT3.5_HEXACO_facet_results.csv", sep = ",", row.names = FALSE)


# shape
shape_GPT3.5_facet_results <- data.frame(
  Trait = c("Sincerity", "Fairness", "Greed Avoidance", "Modesty",
            "Fearfulness", "Anxiety", "Dependence", "Sentimentality",
            "Social Self-Esteem", "Social Boldness", "Sociability", "Liveliness",
            "Forgiveness", "Gentleness", "Flexibility", "Patience",
            "Organization", "Diligence", "Perfectionism", "Prudence",
            "Aesthetic Appreciation", "Inquisitiveness", "Creativity", "Unconventionality",
            "Altruism"),
  Mean = c(
    mean(rowMeans(data_shape_GPT3.5[, Sincerity], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Fairness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Greed_avoidance], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Modesty], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Fearfulness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Anxiety], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Dependence], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Sentimentality], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Social_self_esteem], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Social_boldness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Sociability], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Liveliness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Forgiveness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Gentleness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Flexibility], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Patience], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Organization], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Diligence], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Perfectionism], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Prudence], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Aesthetic_appreciation], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Inquisitiveness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Creativity], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Unconventionality], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Altruism], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_shape_GPT3.5[, Sincerity], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Fairness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Greed_avoidance], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Modesty], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Fearfulness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Anxiety], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Dependence], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Sentimentality], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Social_self_esteem], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Social_boldness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Sociability], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Liveliness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Forgiveness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Gentleness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Flexibility], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Patience], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Organization], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Diligence], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Perfectionism], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Prudence], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Aesthetic_appreciation], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Inquisitiveness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Creativity], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Unconventionality], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Altruism], na.rm = TRUE))
  )
)

# write in csv file
write.table(shape_GPT3.5_facet_results, "shape_GPT3.5_HEXACO_facet_results.csv", sep = ",", row.names = FALSE)




#### Domain level result
# honest for HEXACO model
honest_domain_results <- data.frame(
  Trait = c("Honesty-Humility", "Emotionality", "Extraversion", "Agreeableness", "Conscientiousness", "Openness to Experience"),
  Mean = c(
    mean(rowMeans(data_honest[, Honesty_humility], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Emotionality], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Extraversion], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Agreeableness], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Conscientiousness], na.rm = TRUE)),
    mean(rowMeans(data_honest[, Openness_to_experience], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_honest[, Honesty_humility], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Emotionality], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Extraversion], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Agreeableness], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Conscientiousness], na.rm = TRUE)),
    sd(rowMeans(data_honest[, Openness_to_experience], na.rm = TRUE))
  )
)

# write in csv file
write.table(honest_domain_results, "honest_HEXACO_domain_results.csv", sep = ",", row.names = FALSE)



# faking for HEXACO model
faking_domain_results <- data.frame(
  Trait = c("Honesty-Humility", "Emotionality", "Extraversion", "Agreeableness", "Conscientiousness", "Openness to Experience"),
  Mean = c(
    mean(rowMeans(data_faking[, Honesty_humility], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Emotionality], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Extraversion], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Agreeableness], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Conscientiousness], na.rm = TRUE)),
    mean(rowMeans(data_faking[, Openness_to_experience], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_faking[, Honesty_humility], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Emotionality], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Extraversion], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Agreeableness], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Conscientiousness], na.rm = TRUE)),
    sd(rowMeans(data_faking[, Openness_to_experience], na.rm = TRUE))
  )
)

# write in csv file
write.table(faking_domain_results, "faking_HEXACO_domain_results.csv", sep = ",", row.names = FALSE)



# persona_GPT3.5 for HEXACO model
persona_GPT3.5_domain_results <- data.frame(
  Trait = c("Honesty-Humility", "Emotionality", "Extraversion", "Agreeableness", "Conscientiousness", "Openness to Experience"),
  Mean = c(
    mean(rowMeans(data_persona_GPT3.5[, Honesty_humility], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Emotionality], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Extraversion], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Agreeableness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Conscientiousness], na.rm = TRUE)),
    mean(rowMeans(data_persona_GPT3.5[, Openness_to_experience], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_persona_GPT3.5[, Honesty_humility], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Emotionality], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Extraversion], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Agreeableness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Conscientiousness], na.rm = TRUE)),
    sd(rowMeans(data_persona_GPT3.5[, Openness_to_experience], na.rm = TRUE))
  )
)

# write in csv file
write.table(persona_GPT3.5_domain_results, "persona_GPT3.5_HEXACO_domain_results.csv", sep = ",", row.names = FALSE)



# shape_GPT3.5 for HEXACO model
shape_GPT3.5_domain_results <- data.frame(
  Trait = c("Honesty-Humility", "Emotionality", "Extraversion", "Agreeableness", "Conscientiousness", "Openness to Experience"),
  Mean = c(
    mean(rowMeans(data_shape_GPT3.5[, Honesty_humility], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Emotionality], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Extraversion], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Agreeableness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Conscientiousness], na.rm = TRUE)),
    mean(rowMeans(data_shape_GPT3.5[, Openness_to_experience], na.rm = TRUE))
  ),
  SD = c(
    sd(rowMeans(data_shape_GPT3.5[, Honesty_humility], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Emotionality], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Extraversion], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Agreeableness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Conscientiousness], na.rm = TRUE)),
    sd(rowMeans(data_shape_GPT3.5[, Openness_to_experience], na.rm = TRUE))
  )
)

# write in csv file
write.table(shape_GPT3.5_domain_results, "shape_GPT3.5_HEXACO_domain_results.csv", sep = ",", row.names = FALSE)




#### scale correlation 
# Helper function to calculate domain scores
calculate_dimension_score <- function(data, indices) {
  rowSums(data[, indices], na.rm = TRUE)
}

# honest with HEXACO model
data_honest$Honesty_Humility <- calculate_dimension_score(data_honest, Honesty_humility)
data_honest$Emotionality <- calculate_dimension_score(data_honest, Emotionality)
data_honest$Extraversion <- calculate_dimension_score(data_honest, Extraversion)
data_honest$Agreeableness <- calculate_dimension_score(data_honest, Agreeableness)
data_honest$Conscientiousness <- calculate_dimension_score(data_honest, Conscientiousness)
data_honest$Openness_to_Experience <- calculate_dimension_score(data_honest, Openness_to_experience)

# Select the new domain scores
dimension_scores_hexaco <- data_honest[, c("Honesty_Humility", "Emotionality", "Extraversion", "Agreeableness", "Conscientiousness", "Openness_to_Experience")]

# Calculate the correlation matrix for the HEXACO domains
correlation_matrix_honest_hexaco <- cor(dimension_scores_hexaco)

# Print the correlation matrix
print(correlation_matrix_honest_hexaco)




# faking with HEXACO model
data_faking$Honesty_Humility <- calculate_dimension_score(data_faking, Honesty_humility)
data_faking$Emotionality <- calculate_dimension_score(data_faking, Emotionality)
data_faking$Extraversion <- calculate_dimension_score(data_faking, Extraversion)
data_faking$Agreeableness <- calculate_dimension_score(data_faking, Agreeableness)
data_faking$Conscientiousness <- calculate_dimension_score(data_faking, Conscientiousness)
data_faking$Openness_to_Experience <- calculate_dimension_score(data_faking, Openness_to_experience)

# Select the new domain scores
dimension_scores_hexaco <- data_faking[, c("Honesty_Humility", "Emotionality", "Extraversion", "Agreeableness", "Conscientiousness", "Openness_to_Experience")]

# Calculate the correlation matrix for the HEXACO domains
correlation_matrix_faking_hexaco <- cor(dimension_scores_hexaco)

# Print the correlation matrix
print(correlation_matrix_faking_hexaco)



# persona_GPT3.5 with HEXACO model
data_persona_GPT3.5$Honesty_Humility <- calculate_dimension_score(data_persona_GPT3.5, Honesty_humility)
data_persona_GPT3.5$Emotionality <- calculate_dimension_score(data_persona_GPT3.5, Emotionality)
data_persona_GPT3.5$Extraversion <- calculate_dimension_score(data_persona_GPT3.5, Extraversion)
data_persona_GPT3.5$Agreeableness <- calculate_dimension_score(data_persona_GPT3.5, Agreeableness)
data_persona_GPT3.5$Conscientiousness <- calculate_dimension_score(data_persona_GPT3.5, Conscientiousness)
data_persona_GPT3.5$Openness_to_Experience <- calculate_dimension_score(data_persona_GPT3.5, Openness_to_experience)

# Select the new domain scores
dimension_scores_persona_GPT3_5 <- data_persona_GPT3.5[, c("Honesty_Humility", "Emotionality", "Extraversion", "Agreeableness", "Conscientiousness", "Openness_to_Experience")]

# Calculate the correlation matrix for the HEXACO domains
correlation_matrix_persona_GPT3_5 <- cor(dimension_scores_persona_GPT3_5)

# Print the correlation matrix
print(correlation_matrix_persona_GPT3_5)



# shape_GPT3.5 with HEXACO model
data_shape_GPT3.5$Honesty_Humility <- calculate_dimension_score(data_shape_GPT3.5, Honesty_humility)
data_shape_GPT3.5$Emotionality <- calculate_dimension_score(data_shape_GPT3.5, Emotionality)
data_shape_GPT3.5$Extraversion <- calculate_dimension_score(data_shape_GPT3.5, Extraversion)
data_shape_GPT3.5$Agreeableness <- calculate_dimension_score(data_shape_GPT3.5, Agreeableness)
data_shape_GPT3.5$Conscientiousness <- calculate_dimension_score(data_shape_GPT3.5, Conscientiousness)
data_shape_GPT3.5$Openness_to_Experience <- calculate_dimension_score(data_shape_GPT3.5, Openness_to_experience)

# Select the new domain scores
dimension_scores_shape_GPT3_5 <- data_shape_GPT3.5[, c("Honesty_Humility", "Emotionality", "Extraversion", "Agreeableness", "Conscientiousness", "Openness_to_Experience")]

# Calculate the correlation matrix for the HEXACO domains
correlation_matrix_shape_GPT3_5 <- cor(dimension_scores_shape_GPT3_5)

# Print the correlation matrix
print(correlation_matrix_shape_GPT3_5)




############################# summary #################################

# item mean
item_HEXAO_mean <- read.table("Summary_data/item_HEXAO_mean.csv",sep = ",", header = T)

# persona_GPT3.5 ~ honest
mae_persona_GPT3.5.mean_honest <- mean(abs(item_HEXAO_mean$honest.mean - item_HEXAO_mean$persona_GPT3.5.mean))
profile_correlation_persona_GPT3.5.mean_honest <- cor(item_HEXAO_mean$honest.mean, item_HEXAO_mean$persona_GPT3.5.mean)
mae_persona_GPT3.5.mean_honest
profile_correlation_persona_GPT3.5.mean_honest

# persona_GPT3.5 ~ faking
mae_persona_GPT3.5.mean_faking <- mean(abs(item_HEXAO_mean$faking.mean - item_HEXAO_mean$persona_GPT3.5.mean))
profile_correlation_persona_GPT3.5.mean_faking <- cor(item_HEXAO_mean$faking.mean, item_HEXAO_mean$persona_GPT3.5.mean)
mae_persona_GPT3.5.mean_faking
profile_correlation_persona_GPT3.5.mean_faking

# shape_GPT3.5 ~ honest
mae_shape_GPT3.5.mean_honest <- mean(abs(item_HEXAO_mean$honest.mean - item_HEXAO_mean$shape_GPT3.5.mean))
profile_correlation_shape_GPT3.5.mean_honest <- cor(item_HEXAO_mean$honest.mean, item_HEXAO_mean$shape_GPT3.5.mean)
mae_shape_GPT3.5.mean_honest
profile_correlation_shape_GPT3.5.mean_honest

# shape_GPT3.5 ~ faking
mae_shape_GPT3.5.mean_faking <- mean(abs(item_HEXAO_mean$faking.mean - item_HEXAO_mean$shape_GPT3.5.mean))
profile_correlation_shape_GPT3.5.mean_faking <- cor(item_HEXAO_mean$faking.mean, item_HEXAO_mean$shape_GPT3.5.mean)
mae_shape_GPT3.5.mean_faking
profile_correlation_shape_GPT3.5.mean_faking

# item sd
item_HEXAO_sd <- read.table("Summary_data/item_HEXAO_sd.csv",sep = ",", header = T)

# persona_GPT3.5 ~ honest
mae_persona_GPT3.5.sd_honest <- mean(abs(item_HEXAO_sd$honest.sd - item_HEXAO_sd$persona_GPT3.5.sd))
profile_correlation_persona_GPT3.5.sd_honest <- cor(item_HEXAO_sd$honest.sd, item_HEXAO_sd$persona_GPT3.5.sd)
mae_persona_GPT3.5.sd_honest
profile_correlation_persona_GPT3.5.sd_honest

# persona_GPT3.5 ~ faking
mae_persona_GPT3.5.sd_faking <- mean(abs(item_HEXAO_sd$faking.sd - item_HEXAO_sd$persona_GPT3.5.sd))
profile_correlation_persona_GPT3.5.sd_faking <- cor(item_HEXAO_sd$faking.sd, item_HEXAO_sd$persona_GPT3.5.sd)
mae_persona_GPT3.5.sd_faking
profile_correlation_persona_GPT3.5.sd_faking

# shape_GPT3.5 ~ honest
mae_shape_GPT3.5.sd_honest <- mean(abs(item_HEXAO_sd$honest.sd - item_HEXAO_sd$shape_GPT3.5.sd))
profile_correlation_shape_GPT3.5.sd_honest <- cor(item_HEXAO_sd$honest.sd, item_HEXAO_sd$shape_GPT3.5.sd)
mae_shape_GPT3.5.sd_honest
profile_correlation_shape_GPT3.5.sd_honest

# shape_GPT3.5 ~ faking
mae_shape_GPT3.5.sd_faking <- mean(abs(item_HEXAO_sd$faking.sd - item_HEXAO_sd$shape_GPT3.5.sd))
profile_correlation_shape_GPT3.5.sd_faking <- cor(item_HEXAO_sd$faking.sd, item_HEXAO_sd$shape_GPT3.5.sd)
mae_shape_GPT3.5.sd_faking
profile_correlation_shape_GPT3.5.sd_faking




# facet mean
facet_HEXAO_mean <- read.table("Summary_data/facet_HEXAO_mean.csv",sep = ",", header = T)

# persona_GPT3.5 ~ honest
mae_persona_GPT3.5.mean_honest <- mean(abs(facet_HEXAO_mean$honest.mean - facet_HEXAO_mean$persona_GPT3.5.mean))
profile_correlation_persona_GPT3.5.mean_honest <- cor(facet_HEXAO_mean$honest.mean, facet_HEXAO_mean$persona_GPT3.5.mean)
mae_persona_GPT3.5.mean_honest
profile_correlation_persona_GPT3.5.mean_honest

# persona_GPT3.5 ~ faking
mae_persona_GPT3.5.mean_faking <- mean(abs(facet_HEXAO_mean$faking.mean - facet_HEXAO_mean$persona_GPT3.5.mean))
profile_correlation_persona_GPT3.5.mean_faking <- cor(facet_HEXAO_mean$faking.mean, facet_HEXAO_mean$persona_GPT3.5.mean)
mae_persona_GPT3.5.mean_faking
profile_correlation_persona_GPT3.5.mean_faking

# shape_GPT3.5 ~ honest
mae_shape_GPT3.5.mean_honest <- mean(abs(facet_HEXAO_mean$honest.mean - facet_HEXAO_mean$shape_GPT3.5.mean))
profile_correlation_shape_GPT3.5.mean_honest <- cor(facet_HEXAO_mean$honest.mean, facet_HEXAO_mean$shape_GPT3.5.mean)
mae_shape_GPT3.5.mean_honest
profile_correlation_shape_GPT3.5.mean_honest

# shape_GPT3.5 ~ faking
mae_shape_GPT3.5.mean_faking <- mean(abs(facet_HEXAO_mean$faking.mean - facet_HEXAO_mean$shape_GPT3.5.mean))
profile_correlation_shape_GPT3.5.mean_faking <- cor(facet_HEXAO_mean$faking.mean, facet_HEXAO_mean$shape_GPT3.5.mean)
mae_shape_GPT3.5.mean_faking
profile_correlation_shape_GPT3.5.mean_faking



# facet sd
facet_HEXAO_sd <- read.table("Summary_data/facet_HEXAO_sd.csv",sep = ",", header = T)

# persona_GPT3.5 ~ honest
mae_persona_GPT3.5.sd_honest <- mean(abs(facet_HEXAO_sd$honest.sd - facet_HEXAO_sd$persona_GPT3.5.sd))
profile_correlation_persona_GPT3.5.sd_honest <- cor(facet_HEXAO_sd$honest.sd, facet_HEXAO_sd$persona_GPT3.5.sd)
mae_persona_GPT3.5.sd_honest
profile_correlation_persona_GPT3.5.sd_honest

# persona_GPT3.5 ~ faking
mae_persona_GPT3.5.sd_faking <- mean(abs(facet_HEXAO_sd$faking.sd - facet_HEXAO_sd$persona_GPT3.5.sd))
profile_correlation_persona_GPT3.5.sd_faking <- cor(facet_HEXAO_sd$faking.sd, facet_HEXAO_sd$persona_GPT3.5.sd)
mae_persona_GPT3.5.sd_faking
profile_correlation_persona_GPT3.5.sd_faking

# shape_GPT3.5 ~ honest
mae_shape_GPT3.5.sd_honest <- mean(abs(facet_HEXAO_sd$honest.sd - facet_HEXAO_sd$shape_GPT3.5.sd))
profile_correlation_shape_GPT3.5.sd_honest <- cor(facet_HEXAO_sd$honest.sd, facet_HEXAO_sd$shape_GPT3.5.sd)
mae_shape_GPT3.5.sd_honest
profile_correlation_shape_GPT3.5.sd_honest

# shape_GPT3.5 ~ faking
mae_shape_GPT3.5.sd_faking <- mean(abs(facet_HEXAO_sd$faking.sd - facet_HEXAO_sd$shape_GPT3.5.sd))
profile_correlation_shape_GPT3.5.sd_faking <- cor(facet_HEXAO_sd$faking.sd, facet_HEXAO_sd$shape_GPT3.5.sd)
mae_shape_GPT3.5.sd_faking
profile_correlation_shape_GPT3.5.sd_faking




# domain mean
domain_HEXAO_mean <- read.table("Summary_data/domain_HEXAO_mean.csv",sep = ",", header = T)

# persona_GPT3.5 ~ honest
mae_persona_GPT3.5.mean_honest <- mean(abs(domain_HEXAO_mean$honest.mean - domain_HEXAO_mean$persona_GPT3.5.mean))
profile_correlation_persona_GPT3.5.mean_honest <- cor(domain_HEXAO_mean$honest.mean, domain_HEXAO_mean$persona_GPT3.5.mean)
mae_persona_GPT3.5.mean_honest
profile_correlation_persona_GPT3.5.mean_honest

# persona_GPT3.5 ~ faking
mae_persona_GPT3.5.mean_faking <- mean(abs(domain_HEXAO_mean$faking.mean - domain_HEXAO_mean$persona_GPT3.5.mean))
profile_correlation_persona_GPT3.5.mean_faking <- cor(domain_HEXAO_mean$faking.mean, domain_HEXAO_mean$persona_GPT3.5.mean)
mae_persona_GPT3.5.mean_faking
profile_correlation_persona_GPT3.5.mean_faking

# shape_GPT3.5 ~ honest
mae_shape_GPT3.5.mean_honest <- mean(abs(domain_HEXAO_mean$honest.mean - domain_HEXAO_mean$shape_GPT3.5.mean))
profile_correlation_shape_GPT3.5.mean_honest <- cor(domain_HEXAO_mean$honest.mean, domain_HEXAO_mean$shape_GPT3.5.mean)
mae_shape_GPT3.5.mean_honest
profile_correlation_shape_GPT3.5.mean_honest

# shape_GPT3.5 ~ faking
mae_shape_GPT3.5.mean_faking <- mean(abs(domain_HEXAO_mean$faking.mean - domain_HEXAO_mean$shape_GPT3.5.mean))
profile_correlation_shape_GPT3.5.mean_faking <- cor(domain_HEXAO_mean$faking.mean, domain_HEXAO_mean$shape_GPT3.5.mean)
mae_shape_GPT3.5.mean_faking
profile_correlation_shape_GPT3.5.mean_faking



# domain sd
domain_HEXAO_sd <- read.table("Summary_data/domain_HEXAO_sd.csv",sep = ",", header = T)

# persona_GPT3.5 ~ honest
mae_persona_GPT3.5.sd_honest <- mean(abs(domain_HEXAO_sd$honest.sd - domain_HEXAO_sd$persona_GPT3.5.sd))
profile_correlation_persona_GPT3.5.sd_honest <- cor(domain_HEXAO_sd$honest.sd, domain_HEXAO_sd$persona_GPT3.5.sd)
mae_persona_GPT3.5.sd_honest
profile_correlation_persona_GPT3.5.sd_honest

# persona_GPT3.5 ~ faking
mae_persona_GPT3.5.sd_faking <- mean(abs(domain_HEXAO_sd$faking.sd - domain_HEXAO_sd$persona_GPT3.5.sd))
profile_correlation_persona_GPT3.5.sd_faking <- cor(domain_HEXAO_sd$faking.sd, domain_HEXAO_sd$persona_GPT3.5.sd)
mae_persona_GPT3.5.sd_faking
profile_correlation_persona_GPT3.5.sd_faking

# shape_GPT3.5 ~ honest
mae_shape_GPT3.5.sd_honest <- mean(abs(domain_HEXAO_sd$honest.sd - domain_HEXAO_sd$shape_GPT3.5.sd))
profile_correlation_shape_GPT3.5.sd_honest <- cor(domain_HEXAO_sd$honest.sd, domain_HEXAO_sd$shape_GPT3.5.sd)
mae_shape_GPT3.5.sd_honest
profile_correlation_shape_GPT3.5.sd_honest

# shape_GPT3.5 ~ faking
mae_shape_GPT3.5.sd_faking <- mean(abs(domain_HEXAO_sd$faking.sd - domain_HEXAO_sd$shape_GPT3.5.sd))
profile_correlation_shape_GPT3.5.sd_faking <- cor(domain_HEXAO_sd$faking.sd, domain_HEXAO_sd$shape_GPT3.5.sd)
mae_shape_GPT3.5.sd_faking
profile_correlation_shape_GPT3.5.sd_faking



#######################################SEM########################################


colnames(data_honest) <- paste0("item", 1:100)
colnames(data_faking) <- paste0("item", 1:100)
colnames(data_persona_GPT3.5) <- paste0("item", 1:100)
colnames(data_shape_GPT3.5) <- paste0("item", 1:100)

#### Factor analysis to examine structural validity 

mod.honesty <- "Sincerity =~ item6+item30+item54+item78
                Fairness =~ item12+item36+item60+item84
                Greed_avoidance =~ item18+item42+item66+item90
                Modesty =~ item24+item48+item72+item96"

mod.emotionality <- "Fearfulness =~ item5+item29+item53+item77
                    Anxiety =~ item11+item35+item59+item83
                    Dependence =~ item17+item41+item65+item89
                    Sentimentality =~ item23+item47+item71+item95"

mod.extraversion <- "Social_Self_Esteem =~ item4+item28+item52+item76
                    Social_Boldness =~ item10+item34+item58+item82
                    Sociability =~ item16+item40+item64+item88
                    Liveliness =~ item22+item46+item70+item94"

mod.agreeableness <- "Forgiveness =~ item3+item27+item51+item75
                     Gentleness =~ item9+item33+item57+item81
                     Flexibility =~ item15+item39+item63+item87
                     Patience =~ item21+item45+item69+item93"

mod.conscientiousness <- "Organization =~ item2+item26+item50+item74
                         Diligence =~ item8+item32+item56+item80
                         Perfectionism =~ item14+item38+item62+item86
                         Prudence =~ item20+item44+item68+item92"

mod.openness <- "Aesthetic_Appreciation =~ item1+item25+item49+item73
                 Inquisitiveness =~ item7+item31+item55+item79
                 Creativity =~ item13+item37+item61+item85
                 Unconventionality =~ item19+item43+item67+item91"


# honesty
fit.honest.hon <- lavaan::sem(mod.honesty, data = data_honest, std.lv = TRUE)
fit.faking.hon <- lavaan::sem(mod.honesty, data = data_faking, std.lv = TRUE)
fit.persona_GPT3.5.hon <- lavaan::sem(mod.honesty, data = data_persona_GPT3.5, std.lv = TRUE)
fit.shape_GPT3.5.hon <- lavaan::sem(mod.honesty, data = data_shape_GPT3.5, std.lv = TRUE)

# emotionality
fit.honest.emo <- lavaan::sem(mod.emotionality, data = data_honest, std.lv = TRUE)
fit.faking.emo <- lavaan::sem(mod.emotionality, data = data_faking, std.lv = TRUE)
fit.persona_GPT3.5.emo <- lavaan::sem(mod.emotionality, data = data_persona_GPT3.5, std.lv = TRUE)
fit.shape_GPT3.5.emo <- lavaan::sem(mod.emotionality, data = data_shape_GPT3.5, std.lv = TRUE)

# extraversion
fit.honest.ext <- lavaan::sem(mod.extraversion, data = data_honest, std.lv = TRUE)
fit.faking.ext <- lavaan::sem(mod.extraversion, data = data_faking, std.lv = TRUE)
fit.persona_GPT3.5.ext <- lavaan::sem(mod.extraversion, data = data_persona_GPT3.5, std.lv = TRUE)
fit.shape_GPT3.5.ext <- lavaan::sem(mod.extraversion, data = data_shape_GPT3.5, std.lv = TRUE)

# agreeableness
fit.honest.agr <- lavaan::sem(mod.agreeableness, data = data_honest, std.lv = TRUE)
fit.faking.agr <- lavaan::sem(mod.agreeableness, data = data_faking, std.lv = TRUE)
fit.persona_GPT3.5.agr <- lavaan::sem(mod.agreeableness, data = data_persona_GPT3.5, std.lv = TRUE)
fit.shape_GPT3.5.agr <- lavaan::sem(mod.agreeableness, data = data_shape_GPT3.5, std.lv = TRUE)

# conscientiousness
fit.honest.con <- lavaan::sem(mod.conscientiousness, data = data_honest, std.lv = TRUE)
fit.faking.con <- lavaan::sem(mod.conscientiousness, data = data_faking, std.lv = TRUE)
fit.persona_GPT3.5.con <- lavaan::sem(mod.conscientiousness, data = data_persona_GPT3.5, std.lv = TRUE)
fit.shape_GPT3.5.con <- lavaan::sem(mod.conscientiousness, data = data_shape_GPT3.5, std.lv = TRUE)

# openness
fit.honest.ope <- lavaan::sem(mod.openness, data = data_honest, std.lv = TRUE)
fit.faking.ope <- lavaan::sem(mod.openness, data = data_faking, std.lv = TRUE)
fit.persona_GPT3.5.ope <- lavaan::sem(mod.openness, data = data_persona_GPT3.5, std.lv = TRUE)
fit.shape_GPT3.5.ope <- lavaan::sem(mod.openness, data = data_shape_GPT3.5, std.lv = TRUE)

# Check results for one trait as an example
# Honesty
lavaan::summary(fit.honest.hon, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.faking.hon, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.persona_GPT3.5.hon, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.shape_GPT3.5.hon, standardized = TRUE, fit = TRUE)

# Emotionality
lavaan::summary(fit.honest.emo, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.faking.emo, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.persona_GPT3.5.emo, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.shape_GPT3.5.emo, standardized = TRUE, fit = TRUE)

# Extraversion
lavaan::summary(fit.honest.ext, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.faking.ext, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.persona_GPT3.5.ext, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.shape_GPT3.5.ext, standardized = TRUE, fit = TRUE)

# Agreeableness
lavaan::summary(fit.honest.agr, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.faking.agr, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.persona_GPT3.5.agr, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.shape_GPT3.5.agr, standardized = TRUE, fit = TRUE)

# Conscientiousness
lavaan::summary(fit.honest.con, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.faking.con, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.persona_GPT3.5.con, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.shape_GPT3.5.con, standardized = TRUE, fit = TRUE)

# Openness
lavaan::summary(fit.honest.ope, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.faking.ope, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.persona_GPT3.5.ope, standardized = TRUE, fit = TRUE)
lavaan::summary(fit.shape_GPT3.5.ope, standardized = TRUE, fit = TRUE)


 

# Model fit 
# CFI>.90, TLI>.90, RMSEA < .06 and SRMR < .06 indicate good fit.
# If the model fit of GPT-based responses is as good as human responses, then it can serve as one piece of evidence supporting the notion that

mod.fit <- t(data.frame(
  hon.honest = fitMeasures(fit.honest.hon, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  hon.faking = fitMeasures(fit.faking.hon, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  hon.persona_GPT3.5 = fitMeasures(fit.persona_GPT3.5.hon, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  hon.shape_GPT3.5 = fitMeasures(fit.shape_GPT3.5.hon, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  
  emo.honest = fitMeasures(fit.honest.emo, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  emo.faking = fitMeasures(fit.faking.emo, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  emo.persona_GPT3.5 = fitMeasures(fit.persona_GPT3.5.emo, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  emo.shape_GPT3.5 = fitMeasures(fit.shape_GPT3.5.emo, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  
  ext.honest = fitMeasures(fit.honest.ext, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  ext.faking = fitMeasures(fit.faking.ext, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  ext.persona_GPT3.5 = fitMeasures(fit.persona_GPT3.5.ext, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  ext.shape_GPT3.5 = fitMeasures(fit.shape_GPT3.5.ext, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  
  agr.honest = fitMeasures(fit.honest.agr, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  agr.faking = fitMeasures(fit.faking.agr, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  agr.persona_GPT3.5 = fitMeasures(fit.persona_GPT3.5.agr, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  agr.shape_GPT3.5 = fitMeasures(fit.shape_GPT3.5.agr, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  
  con.honest = fitMeasures(fit.honest.con, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  con.faking = fitMeasures(fit.faking.con, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  con.persona_GPT3.5 = fitMeasures(fit.persona_GPT3.5.con, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  con.shape_GPT3.5 = fitMeasures(fit.shape_GPT3.5.con, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  
  ope.honest = fitMeasures(fit.honest.ope, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  ope.faking = fitMeasures(fit.faking.ope, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  ope.persona_GPT3.5 = fitMeasures(fit.persona_GPT3.5.ope, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean")),
  ope.shape_GPT3.5 = fitMeasures(fit.shape_GPT3.5.ope, fit.measures = c("chisq.scaled", "df", "cfi", "tli", "rmsea", "srmr_bentler_nomean"))
))

write.table(mod.fit, "mod.fit.csv", sep = ",")



# Factor loadings 
# If loadings of GPT-based responses are close to those obtained from human responses, it can serve as one piece of evidence supporting the notion that
loadings <- data.frame(
  honest.hon = lavaan::standardizedsolution(fit.honest.hon)[1:16, "est.std"],
  faking.hon = lavaan::standardizedsolution(fit.faking.hon)[1:16, "est.std"],
  persona_GPT3.5.hon = lavaan::standardizedsolution(fit.persona_GPT3.5.hon)[1:16, "est.std"],
  shape_GPT3.5.hon = lavaan::standardizedsolution(fit.shape_GPT3.5.hon)[1:16, "est.std"],
  
  honest.emo = lavaan::standardizedsolution(fit.honest.emo)[1:16, "est.std"],
  faking.emo = lavaan::standardizedsolution(fit.faking.emo)[1:16, "est.std"],
  persona_GPT3.5.emo = lavaan::standardizedsolution(fit.persona_GPT3.5.emo)[1:16, "est.std"],
  shape_GPT3.5.emo = lavaan::standardizedsolution(fit.shape_GPT3.5.emo)[1:16, "est.std"],
  
  honest.ext = lavaan::standardizedsolution(fit.honest.ext)[1:16, "est.std"],
  faking.ext = lavaan::standardizedsolution(fit.faking.ext)[1:16, "est.std"],
  persona_GPT3.5.ext = lavaan::standardizedsolution(fit.persona_GPT3.5.ext)[1:16, "est.std"],
  shape_GPT3.5.ext = lavaan::standardizedsolution(fit.shape_GPT3.5.ext)[1:16, "est.std"],
  
  honest.agr = lavaan::standardizedsolution(fit.honest.agr)[1:16, "est.std"],
  faking.agr = lavaan::standardizedsolution(fit.faking.agr)[1:16, "est.std"],
  persona_GPT3.5.agr = lavaan::standardizedsolution(fit.persona_GPT3.5.agr)[1:16, "est.std"],
  shape_GPT3.5.agr = lavaan::standardizedsolution(fit.shape_GPT3.5.agr)[1:16, "est.std"],
  
  honest.con = lavaan::standardizedsolution(fit.honest.con)[1:16, "est.std"],
  faking.con = lavaan::standardizedsolution(fit.faking.con)[1:16, "est.std"],
  persona_GPT3.5.con = lavaan::standardizedsolution(fit.persona_GPT3.5.con)[1:16, "est.std"],
  shape_GPT3.5.con = lavaan::standardizedsolution(fit.shape_GPT3.5.con)[1:16, "est.std"],
  
  honest.ope = lavaan::standardizedsolution(fit.honest.ope)[1:16, "est.std"],
  faking.ope = lavaan::standardizedsolution(fit.faking.ope)[1:16, "est.std"],
  persona_GPT3.5.ope = lavaan::standardizedsolution(fit.persona_GPT3.5.ope)[1:16, "est.std"],
  shape_GPT3.5.ope = lavaan::standardizedsolution(fit.shape_GPT3.5.ope)[1:16, "est.std"]
)

write.table(loadings, "loadings.csv", sep = ",")



# Inter-factor correlations
# If factor correlations obtained from GPT-based responses are close to those obtained from human responses, then it can serve as one piece of evidence supporting the notion that

correlations <- data.frame(
  honest.hon = lavaan::standardizedsolution(fit.honest.hon)[37:42, "est.std"],
  faking.hon = lavaan::standardizedsolution(fit.faking.hon)[37:42, "est.std"],
  persona_GPT3.5.hon = lavaan::standardizedsolution(fit.persona_GPT3.5.hon)[37:42, "est.std"],
  shape_GPT3.5.hon = lavaan::standardizedsolution(fit.shape_GPT3.5.hon)[37:42, "est.std"],
  
  honest.emo = lavaan::standardizedsolution(fit.honest.emo)[37:42, "est.std"],
  faking.emo = lavaan::standardizedsolution(fit.faking.emo)[37:42, "est.std"],
  persona_GPT3.5.emo = lavaan::standardizedsolution(fit.persona_GPT3.5.emo)[37:42, "est.std"],
  shape_GPT3.5.emo = lavaan::standardizedsolution(fit.shape_GPT3.5.emo)[37:42, "est.std"],
  
  honest.ext = lavaan::standardizedsolution(fit.honest.ext)[37:42, "est.std"],
  faking.ext = lavaan::standardizedsolution(fit.faking.ext)[37:42, "est.std"],
  persona_GPT3.5.ext = lavaan::standardizedsolution(fit.persona_GPT3.5.ext)[37:42, "est.std"],
  shape_GPT3.5.ext = lavaan::standardizedsolution(fit.shape_GPT3.5.ext)[37:42, "est.std"],
  
  honest.agr = lavaan::standardizedsolution(fit.honest.agr)[37:42, "est.std"],
  faking.agr = lavaan::standardizedsolution(fit.faking.agr)[37:42, "est.std"],
  persona_GPT3.5.agr = lavaan::standardizedsolution(fit.persona_GPT3.5.agr)[37:42, "est.std"],
  shape_GPT3.5.agr = lavaan::standardizedsolution(fit.shape_GPT3.5.agr)[37:42, "est.std"],
  
  honest.con = lavaan::standardizedsolution(fit.honest.con)[37:42, "est.std"],
  faking.con = lavaan::standardizedsolution(fit.faking.con)[37:42, "est.std"],
  persona_GPT3.5.con = lavaan::standardizedsolution(fit.persona_GPT3.5.con)[37:42, "est.std"],
  shape_GPT3.5.con = lavaan::standardizedsolution(fit.shape_GPT3.5.con)[37:42, "est.std"],
  
  honest.ope = lavaan::standardizedsolution(fit.honest.ope)[37:42, "est.std"],
  faking.ope = lavaan::standardizedsolution(fit.faking.ope)[37:42, "est.std"],
  persona_GPT3.5.ope = lavaan::standardizedsolution(fit.persona_GPT3.5.ope)[37:42, "est.std"],
  shape_GPT3.5.ope = lavaan::standardizedsolution(fit.shape_GPT3.5.ope)[37:42, "est.std"]
)

write.table(correlations, "correlations.csv", sep = ",")
















