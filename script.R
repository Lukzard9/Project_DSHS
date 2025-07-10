# ====================================================================
# GLOBAL HEALTH AND DEVELOPMENT (2012-2021)
# ====================================================================


library(tidyverse)
library(corrplot)
library(gridExtra)
library(scales)
library(countrycode)
library(fmsb)
library(dunn.test)
library(forecast)
library(reshape2)
library(ggpubr)

df <- read.csv("dataset/global_health.csv")


#PANORAMICA GENERALE DEL DATASET
cat("=== PANORAMICA GENERALE ===\n")
cat("Dimensioni del dataset:", nrow(df), "righe x", ncol(df), "colonne\n")
cat("Periodo coperto:", min(df$Year, na.rm = TRUE), "-", max(df$Year, na.rm = TRUE), "\n")
cat("Numero di paesi unici:", length(unique(df$Country)), "\n")

str(df)
summary(df)

#Trend globali
global_trends <- df %>%
  group_by(Year) %>%
  summarise(
    Avg_Life_Expectancy = mean(Life_Expectancy, na.rm = TRUE),
    Avg_Urban_Pop = mean(Urban_Population_Percent, na.rm = TRUE),
    Avg_Water_Access = mean(Safe_Water_Access_Percent, na.rm = TRUE),
    Avg_Immunization = mean(Immunization_Rate, na.rm = TRUE),
    .groups = 'drop'
  )

trends_long <- global_trends %>%
  select(Year, Avg_Life_Expectancy, Avg_Urban_Pop, Avg_Water_Access, Avg_Immunization) %>%
  gather(key = "Indicator", value = "Value", -Year) %>%
  mutate(
    Indicator = case_when(
      Indicator == "Avg_Life_Expectancy" ~ "Aspettativa di Vita",
      Indicator == "Avg_Urban_Pop" ~ "% Pop. Urbana",
      Indicator == "Avg_Water_Access" ~ "% Accesso Acqua",
      Indicator == "Avg_Immunization" ~ "% Immunizzazione"
    )
  )

p1 <- ggplot(trends_long, aes(x = Year, y = Value, color = Indicator)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Trend Globali degli Indicatori di Sviluppo (2012-2021)",
       x = "Anno", y = "Valore", color = "Indicatore") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)


#GENDER GAP NELL'ASPETTATIVA DI VITA
df_clean <- df %>% 
  filter(!is.na(Life_Expectancy) & 
           !is.na(Life_Expectancy_Female) & 
           !is.na(Life_Expectancy_Male))

p2 <- ggplot(df_clean, aes(x = Life_Expectancy_Male, y = Life_Expectancy_Female)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Aspettativa di vita maschile (anni)",
       y = "Aspettativa di vita femminile (anni)") +
  theme_minimal()

print(p2)


#CORRELAZIONE PIL-ASPETTATIVA DI VITA
gdp_health <- df %>%
  filter(!is.na(GDP_Per_Capita) & !is.na(Life_Expectancy) & GDP_Per_Capita > 0)

correlation_gdp_life <- cor(gdp_health$GDP_Per_Capita, gdp_health$Life_Expectancy)
cat("Correlazione PIL pro capite - Aspettativa di vita:", round(correlation_gdp_life, 3), "\n")

p3 <- ggplot(gdp_health, aes(x = GDP_Per_Capita, y = Life_Expectancy)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "loess", color = "red") +
  scale_x_log10(labels = dollar_format()) +
  labs(x = "PIL pro capite (scala log)", y = "Aspettativa di Vita (anni)") +
  theme_minimal()

print(p3)


#MALNUTRIZIONE E OBESITÀ
nutrition_life_data <- df %>%
  filter(!is.na(Obesity_Rate_Percent) & !is.na(Underweight_Rate_Percent) & 
           !is.na(Life_Expectancy)) %>%
  group_by(Country) %>%
  summarise(
    Avg_Obesity = mean(Obesity_Rate_Percent, na.rm = TRUE),
    Avg_Underweight = mean(Underweight_Rate_Percent, na.rm = TRUE),
    Avg_Life_Expectancy = mean(Life_Expectancy, na.rm = TRUE),
    .groups = 'drop'
  )

p4_obesity <- ggplot(nutrition_life_data, aes(x = Avg_Obesity, y = Avg_Life_Expectancy)) +
  geom_point(alpha = 0.7, size = 2, color = "#E74C3C") +
  geom_smooth(method = "loess", color = "orange") +
  labs(x = "Tasso Obesità (%)", y = "Aspettativa di Vita (anni)") +
  theme_minimal()

p4_underweight <- ggplot(nutrition_life_data, aes(x = Avg_Underweight, y = Avg_Life_Expectancy)) +
  geom_point(alpha = 0.7, size = 2, color = "#3498DB") +
  geom_smooth(method = "loess", color = "orange") +
  labs(x = "Tasso Sottopeso (%)", y = "Aspettativa di Vita (anni)") +
  theme_minimal()

grid.arrange(p4_obesity, p4_underweight, ncol = 2)


#AGGIUNTA CONTINENTI
df <- df %>%
  mutate(Continent = countrycode(sourcevar = Country, 
                                 origin = "country.name", 
                                 destination = "continent"))


#ACCESSO ALL'ACQUA PER CONTINENTE
continent_water_stats <- df %>%
  filter(!is.na(Safe_Water_Access_Percent), !is.na(Life_Expectancy), !is.na(Continent)) %>%
  group_by(Continent) %>%
  summarise(
    Avg_Water_Access = mean(Safe_Water_Access_Percent, na.rm = TRUE),
    Avg_Life_Expectancy = mean(Life_Expectancy, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Avg_Life_Expectancy) %>%
  mutate(Continent_Ordered = factor(Continent, levels = Continent))

water_access_plot <- ggplot(continent_water_stats, 
                            aes(x = Continent_Ordered, y = Avg_Water_Access)) +
  geom_col(fill = "steelblue", alpha = 0.7, color = "black", width = 0.7) +
  geom_text(aes(label = paste0(round(Avg_Water_Access, 1), "%")), 
            vjust = -0.5, size = 5.5, fontface = "bold") +
  labs(x = "Continente (in ordine per aspettativa di vita)",
       y = "Accesso medio all'acqua potabile (%)") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))

print(water_access_plot)


#SPIDER GRAPHS PER CONTINENTE
prepare_spider_data <- function(df) {
  continent_stats <- df %>%
    filter(!is.na(Continent)) %>%
    group_by(Continent) %>%
    summarise(
      Total_Alcohol = mean(Alcohol_Consumption_Per_Capita, na.rm = TRUE),
      Water_Access_Percent = mean(Water_Access_Percent, na.rm = TRUE),
      Sanitary_Expense_Per_Capita = mean(Sanitary_Expense_Per_Capita, na.rm = TRUE),
      Air_Pollution = mean(Air_Pollution, na.rm = TRUE),
      Immunization_Rate = mean(Immunization_Rate, na.rm = TRUE),
      Life_Expectancy = mean(Life_Expectancy, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(!is.na(Life_Expectancy))
  
  continent_normalized <- continent_stats %>%
    mutate(
      Alcohol_Score = rescale(log10(Total_Alcohol + 1), to = c(0, 100)),
      Water_Score = pmax(0, pmin(100, Water_Access_Percent)),
      Sanitary_Score = rescale(Sanitary_Expense_Per_Capita, to = c(0, 100)),
      Air_Quality_Score = 100 - rescale(Air_Pollution, to = c(0, 100)),
      Immunization_Score = pmax(0, pmin(100, Immunization_Rate))
    ) %>%
    arrange(desc(Life_Expectancy)) %>%
    select(Continent, Life_Expectancy, Alcohol_Score, Water_Score, 
           Sanitary_Score, Air_Quality_Score, Immunization_Score)
  
  return(continent_normalized)
}

create_spider_plot <- function(continent_name, scores, life_exp) {
  spider_df <- data.frame(
    Alcohol = scores[1],
    Accesso_Acqua = scores[2],
    Spese_Sanitarie = scores[3],
    Qualita_Aria = scores[4],
    Immunizzazione = scores[5]
  )
  
  colnames(spider_df) <- c("Consumo Alcol", "Accesso Acqua", "Spese Sanitarie", 
                           "Qualità Aria", "Immunizzazione")
  
  spider_df <- rbind(rep(100, 5), rep(0, 5), spider_df)
  
  continent_colors <- list(
    "Africa" = "#e74c3c", "Asia" = "#3498db", "Europe" = "#2ecc71",
    "North America" = "#f39c12", "South America" = "#9b59b6", "Oceania" = "#1abc9c"
  )
  
  color_border <- continent_colors[[continent_name]]
  if(is.null(color_border)) color_border <- "#34495e"
  color_fill <- alpha(color_border, 0.3)
  
  radarchart(spider_df,
             axistype = 1,
             pcol = color_border,
             pfcol = color_fill,
             plwd = 4,
             plty = 1,
             cglcol = "grey60",
             cglty = 1,
             axislabcol = "grey40",
             caxislabels = seq(0, 100, 25),
             cglwd = 1.5,
             vlcex = 1.3,
             title = paste0("SPIDER GRAPH - ", continent_name, 
                            "\nAspettativa di vita: ", round(life_exp, 1), " anni"))
}

spider_data <- prepare_spider_data(df)

for(i in 1:nrow(spider_data)) {
  continent <- spider_data$Continent[i]
  life_exp <- spider_data$Life_Expectancy[i]
  scores <- c(
    spider_data$Alcohol_Score[i],
    spider_data$Water_Score[i],
    spider_data$Sanitary_Score[i],
    spider_data$Air_Quality_Score[i],
    spider_data$Immunization_Score[i]
  )
  
  create_spider_plot(continent, scores, life_exp)
}


#ANALISI STATISTICA
life_data <- df %>%
  filter(!is.na(Life_Expectancy), !is.na(Continent)) %>%
  select(Country, Continent, Year, Life_Expectancy, GDP_Per_Capita, 
         Water_Access_Percent, Air_Pollution, Immunization_Rate,
         Sanitary_Expense_Per_Capita, Urban_Population_Percent,
         Fertility_Rate, Hospital_Beds_Per_1000)

# Test di normalità
shapiro_test <- shapiro.test(life_data$Life_Expectancy)
cat("Test di Shapiro-Wilk:\n")
cat("  W =", round(shapiro_test$statistic, 4), ", p-value =", 
    format(shapiro_test$p.value, scientific = TRUE), "\n")

# Q-Q plot per valutare normalità
qqnorm(life_data$Life_Expectancy, main = "Q-Q Plot - Aspettativa di Vita")
qqline(life_data$Life_Expectancy, col = "red")

# Trasformazione Box-Cox
lambda <- BoxCox.lambda(life_data$Life_Expectancy)
Life_Expectancy_BoxCox <- BoxCox(life_data$Life_Expectancy, lambda)
shapiro_test2 <- zzTest(Life_Expectancy_BoxCox)
cat("Test di Shapiro-Wilk dopo Box-Cox:\n")
cat("  W =", round(shapiro_test2$statistic, 4), ", p-value =", 
    format(shapiro_test2$p.value, scientific = TRUE), "\n")

# Q-Q plot per dati trasformati
qqnorm(Life_Expectancy_BoxCox, main = "Q-Q Plot - Aspettativa di Vita (Box-Cox)")
qqline(Life_Expectancy_BoxCox, col = "red")

# Test di Kruskal-Wallis per confronto tra continenti
kruskal_result <- kruskal.test(Life_Expectancy ~ Continent, data = life_data)
cat("\nTest di Kruskal-Wallis:\n")
cat("  Chi-quadrato =", round(kruskal_result$statistic, 4), 
    ", p-value =", format(kruskal_result$p.value, scientific = TRUE), "\n")

# Analisi di correlazione
numeric_vars <- life_data %>%
  select(Life_Expectancy, GDP_Per_Capita, Water_Access_Percent, 
         Air_Pollution, Immunization_Rate, Sanitary_Expense_Per_Capita,
         Urban_Population_Percent, Fertility_Rate, Hospital_Beds_Per_1000) %>%
  na.omit()

spearman_corr <- cor(numeric_vars, method = "spearman")
corrplot(spearman_corr, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.7, tl.cex = 0.8, tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#D73027", "#FC8D59", "#FEE090", 
                                  "#FFFFFF", "#E0F3F8", "#91BFDB", 
                                  "#4575B4"))(200), cl.pos = "n",
         mar = c(0,0,0.1,0))

# Boxplot per continenti
boxplot_kruskal <- ggplot(life_data, aes(x = Continent, y = Life_Expectancy, fill = Continent)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  labs(x = "Continente", y = "Aspettativa di Vita (anni)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set3")

print(boxplot_kruskal)

# Violin plot
violin_plot <- ggplot(life_data, aes(x = Continent, y = Life_Expectancy, fill = Continent)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, alpha = 0.8, outlier.shape = NA) +
  stat_summary(fun = median, geom = "point", size = 2, color = "red") +
  labs(x = "Continente", y = "Aspettativa di Vita (anni)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set2")

print(violin_plot)

# Test post-hoc di Dunn
if(kruskal_result$p.value < 0.05) {
  dunn_result <- dunn.test(life_data$Life_Expectancy, life_data$Continent, 
                           method = "bonferroni")
  
  # Heatmap dei p-values
  continents <- unique(life_data$Continent)
  n_cont <- length(continents)
  p_matrix <- matrix(1, nrow = n_cont, ncol = n_cont)
  rownames(p_matrix) <- continents
  colnames(p_matrix) <- continents
  
  comparisons <- dunn_result$comparisons
  p_values <- dunn_result$P.adjusted
  
  for(i in 1:length(comparisons)) {
    comp_split <- strsplit(comparisons[i], " - ")[[1]]
    if(length(comp_split) == 2) {
      cont1 <- comp_split[1]
      cont2 <- comp_split[2]
      if(cont1 %in% continents && cont2 %in% continents) {
        p_matrix[cont1, cont2] <- p_values[i]
        p_matrix[cont2, cont1] <- p_values[i]
      }
    }
  }
  
  p_matrix_long <- melt(p_matrix)
  colnames(p_matrix_long) <- c("Continent1", "Continent2", "P_Value")
  
  p_matrix_long$Significance <- ifelse(p_matrix_long$P_Value < 0.001, "p < 0.001",
                                       ifelse(p_matrix_long$P_Value < 0.01, "p < 0.01",
                                              ifelse(p_matrix_long$P_Value < 0.05, "p < 0.05", "p ≥ 0.05")))
  
  heatmap_dunn <- ggplot(p_matrix_long, aes(x = Continent1, y = Continent2, fill = Significance)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = ifelse(P_Value == 1, "1.000",
                                 ifelse(P_Value < 0.001, sprintf("%.3e", P_Value),
                                        sprintf("%.3f", P_Value)))), 
              color = "black", size = 4, fontface = "bold") +
    scale_fill_manual(values = c("p < 0.001" = "#e74c3c", "p < 0.01" = "#fc8d59", 
                                 "p < 0.05" = "#fee08b", "p ≥ 0.05" = "#e0f3f8")) +
    labs(fill = "Significatività") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  print(heatmap_dunn)
}
