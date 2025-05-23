# Load libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)

# Download COVID-19 data (Johns Hopkins University)
cases_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
deaths_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
vax_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"

cases <- read_csv(cases_url)
deaths <- read_csv(deaths_url)
vax <- read_csv(vax_url)

# Transform the cases and deaths data into long format
cases_long <- cases %>%
  pivot_longer(
    cols = matches("^\\d+/\\d+/\\d+$"),
    names_to = "Date",
    values_to = "Cases"
  )

deaths_long <- deaths %>%
  pivot_longer(
    cols = matches("^\\d+/\\d+/\\d+$"),
    names_to = "Date",
    values_to = "Deaths"
  )

# Merge cases and deaths long data
covid_long <- cases_long %>%
  select(`Country/Region`, Date, Cases) %>%
  left_join(deaths_long %>% select(`Country/Region`, Date, Deaths), by = c("Country/Region", "Date"))

# Convert Date to proper date format
covid_long <- covid_long %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))

# Get the latest available date
latest_available_date <- max(covid_long$Date, na.rm = TRUE)

# Filter for the latest date
covid_latest <- covid_long %>%
  filter(Date == latest_available_date) %>%
  group_by(`Country/Region`) %>%
  summarise(
    Total_Cases = sum(Cases, na.rm = TRUE),
    Total_Deaths = sum(Deaths, na.rm = TRUE)
  )

# Use latest vaccination records directly without aggregation
vax_latest <- vax %>%
  group_by(location) %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  select(location, people_fully_vaccinated_per_hundred) %>%
  rename(Vaccination_Rate = people_fully_vaccinated_per_hundred) %>%
  filter(!is.na(Vaccination_Rate))

# Merge summaries
covid_summary <- covid_latest %>%
  left_join(vax_latest, by = c("Country/Region" = "location")) %>%
  mutate(Case_Fatality_Rate = (Total_Deaths / Total_Cases) * 100)

# Remove countries with very low case numbers and invalid vaccination rates
covid_filtered <- covid_summary %>%
  filter(Total_Cases > 10000, !is.na(Vaccination_Rate), !is.na(Case_Fatality_Rate))

# Scale the data for clustering (internal factors only)
covid_scaled <- covid_filtered %>%
  select(Total_Cases, Total_Deaths, Case_Fatality_Rate) %>%
  scale() %>%
  as.data.frame()
row.names(covid_scaled) <- covid_filtered$`Country/Region`

# Determine optimal number of clusters using Elbow and Silhouette methods
fviz_nbclust(covid_scaled, kmeans, method = "wss") + 
  labs(title = "Elbow Method", x = "Number of Clusters (k)", y = "Total Within-Cluster Sum of Squares")

fviz_nbclust(covid_scaled, kmeans, method = "silhouette") + 
  labs(title = "Silhouette Method", x = "Number of Clusters (k)", y = "Average Silhouette Width")

# Apply K-means clustering (assuming k=4 based on previous observations)
kmeans_result <- kmeans(covid_scaled, centers = 4, nstart = 25)

# Add cluster assignment to original data
covid_filtered$Cluster <- as.factor(kmeans_result$cluster)

# Visualize clusters with small labels and highlight outliers
cluster_plot <- fviz_cluster(kmeans_result, data = covid_scaled, labelsize = 2, repel = TRUE) + 
  geom_text(aes(label = ifelse(row.names(covid_scaled) %in% 
                                 row.names(covid_scaled)[which(rowSums(abs(scale(covid_scaled))) > quantile(rowSums(abs(scale(covid_scaled))), 0.95))], 
                               row.names(covid_scaled), "")), size = 3, color = "red") +
  labs(title = "Cluster Plot",
       x = "Combination of Cases, Deaths, Fatality Rate (PC1)",
       y = "Secondary Combination (PC2)")

print(cluster_plot)

# Dendrogram for hierarchical clustering
hc <- hclust(dist(covid_scaled))
fviz_dend(hc, k = 4, show_labels = TRUE, label_cols = covid_filtered$Cluster, rect = TRUE, cex = 0.4) + 
  labs(title = "Dendrogram of Countries", x = "Countries", y = "Height (Dissimilarity)")