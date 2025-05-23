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

# Load libraries
library(readxl)

# Read in grant data (skip first 5 lines of metadata)
grant_data <- read_csv("COVID19_Grant_Report.csv", skip = 5)

# Clean grant funding and filter for 2020 only
grant_data_clean <- grant_data %>%
  mutate(`Award Funding` = parse_number(`Award Funding`)) %>%
  filter(`Award Fiscal Year` == "2020") %>%
  group_by(State) %>%
  summarise(Total_Funding_2020 = sum(`Award Funding`, na.rm = TRUE), .groups = "drop")

# Read population data and clean
population_data <- read_excel("NST-EST2024-POP.xlsx", skip = 3) %>%
  select(State = 1, Population_2020 = 3) %>%
  slice(6:56) %>%
  mutate(
    State = str_remove(State, "^\\."),  # remove leading dot
    Population_2020 = as.numeric(Population_2020)
  )

# State name to abbreviation mapping
state_abbrev_map <- c(
  'Alabama' = 'AL', 'Alaska' = 'AK', 'Arizona' = 'AZ', 'Arkansas' = 'AR',
  'California' = 'CA', 'Colorado' = 'CO', 'Connecticut' = 'CT', 'Delaware' = 'DE',
  'Florida' = 'FL', 'Georgia' = 'GA', 'Hawaii' = 'HI', 'Idaho' = 'ID',
  'Illinois' = 'IL', 'Indiana' = 'IN', 'Iowa' = 'IA', 'Kansas' = 'KS',
  'Kentucky' = 'KY', 'Louisiana' = 'LA', 'Maine' = 'ME', 'Maryland' = 'MD',
  'Massachusetts' = 'MA', 'Michigan' = 'MI', 'Minnesota' = 'MN', 'Mississippi' = 'MS',
  'Missouri' = 'MO', 'Montana' = 'MT', 'Nebraska' = 'NE', 'Nevada' = 'NV',
  'New Hampshire' = 'NH', 'New Jersey' = 'NJ', 'New Mexico' = 'NM', 'New York' = 'NY',
  'North Carolina' = 'NC', 'North Dakota' = 'ND', 'Ohio' = 'OH', 'Oklahoma' = 'OK',
  'Oregon' = 'OR', 'Pennsylvania' = 'PA', 'Rhode Island' = 'RI', 'South Carolina' = 'SC',
  'South Dakota' = 'SD', 'Tennessee' = 'TN', 'Texas' = 'TX', 'Utah' = 'UT',
  'Vermont' = 'VT', 'Virginia' = 'VA', 'Washington' = 'WA', 'West Virginia' = 'WV',
  'Wisconsin' = 'WI', 'Wyoming' = 'WY', 'District of Columbia' = 'DC'
)

# Add abbreviations for merging
population_data <- population_data %>%
  mutate(State_Abbrev = state_abbrev_map[State])

# Merge on state abbreviation
merged_data <- grant_data_clean %>%
  inner_join(population_data, by = c("State" = "State_Abbrev")) %>%
  mutate(Funding_Per_Capita = Total_Funding_2020 / Population_2020)

# Print results
print(merged_data %>% arrange(desc(Funding_Per_Capita)))

# Plot funding per capita by state
ggplot(merged_data, aes(x = Funding_Per_Capita, y = reorder(State, Funding_Per_Capita))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "COVID-19 Funding per Capita by State (2020)",
    x = "Funding per Capita (USD)",
    y = "State"
  ) +
  theme_minimal()

# Load U.S. COVID case data from NYT
us_cases_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
us_cases <- read_csv(us_cases_url)

# Map NYT state names to abbreviations for joining
nyt_state_map <- setNames(names(state_abbrev_map), state_abbrev_map)  # reverse map

us_cases_2020 <- us_cases %>%
  filter(date <= "2020-12-31") %>%
  group_by(state) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  mutate(State = state_abbrev_map[state]) %>%
  filter(!is.na(State)) %>%
  select(State, Total_Cases_2020 = cases)

# Merge with your existing merged_data
state_analysis <- merged_data %>%
  inner_join(us_cases_2020, by = "State") %>%
  mutate(
    Cases_Per_Capita = Total_Cases_2020 / Population_2020,
    Funding_Per_Case = Total_Funding_2020 / Total_Cases_2020
  )

# Preview the results
print(state_analysis %>% 
        select(State, Population_2020, Total_Cases_2020, 
               Cases_Per_Capita, Funding_Per_Capita, Funding_Per_Case))

# Cases per capita vs funding per capita
ggplot(state_analysis, aes(x = Cases_Per_Capita, y = Funding_Per_Capita, label = State)) +
  geom_point(color = "darkred", size = 3) +
  geom_text(size = 2.5, vjust = -0.8) +
  labs(
    title = "Cases per Capita vs. Funding per Capita (2020)",
    x = "COVID-19 Cases per Capita",
    y = "Funding per Capita (USD)"
  ) +
  theme_minimal()

# Funding per case by state
ggplot(state_analysis, aes(x = Funding_Per_Case, y = reorder(State, Funding_Per_Case))) +
  geom_col(fill = "forestgreen") +
  labs(
    title = "COVID-19 Funding per Case by State (2020)",
    x = "Funding per Reported Case (USD)",
    y = "State"
  ) +
  theme_minimal()
