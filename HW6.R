tree_data <- read.csv("~/Documents/plan372-sp23/HW6/TS3_Raw_tree_data.csv")

#
#question 1
#First, I extracted the two-letter state code from the "City" column using the str_extract() function from the stringr package and saved the results in a new column called "State". 
#Then, I created a frequency table of the "State" column using the table() function and saved it as "state_counts". 
#I converted "state_counts" to a data frame using the as.data.frame() function, so that it can be used for plotting. 
#Finally, I created a bar plot using the ggplot() function from the ggplot2 package, setting the data argument to "state_counts_df". I used geom_bar() with stat = "identity" to create a bar plot of the frequency counts, and sets the x and y labels and the plot title using xlab(), ylab(), and ggtitle(). The bars are filled with the color "steelblue".
library(stringr)
tree_data$State <- str_extract(tree_data$City, "\\b[A-Z]{2}\\b")
state_counts <- table(tree_data$State)

library(ggplot2)
state_counts_df <- as.data.frame(state_counts)
ggplot(data = state_counts_df, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  xlab("State") + 
  ylab("Number of Records") +
  ggtitle("Number of Records by State")



#question 2
#I used the dplyr package in R to filter the tree_data dataset to only include records from North and South Carolina. I assigned the resulting filtered dataset to a new object called tree_data_ncsc.
#Then, I used the unique() function to extract a vector of unique city names from the filtered dataset. I assigned this new vector to the variable cities_ncsc.
library(dplyr)

# Filter dataset to only include records from North and South Carolina
tree_data_ncsc <- filter(tree_data, State %in% c("NC", "SC"))

# Get a vector of unique city names in North and South Carolina
cities_ncsc <- unique(tree_data_ncsc$City)




#question 3
#First I extracted a subset of the original tree_data dataset containing only the rows for which the State column contains either "NC" or "SC". This was done using the str_detect function from the stringr package to match the pattern "NC" or "SC" in the State column. Then I assigned this subset to a new object called nc_sc_data.
nc_sc_data <- tree_data[str_detect(tree_data$State, "NC|SC"), ]
#Created a new column in nc_sc_data called Genus and assigned it values by extracting the genus name from the ScientificName column. This was done using the str_extract function from the stringr package to match the pattern "^[A-Za-z]+". The extracted genus names were assigned to the new Genus column.
nc_sc_data$Genus <- str_extract(nc_sc_data$ScientificName, "^[A-Za-z]+")
#This line below calculates the mean crown diameter (AvgCdia) for each unique genus name in the nc_sc_data dataset. The aggregate function is used to group the data by the Genus column and calculate the mean of the AvgCdia column for each group. The results were stored in a new data frame called genus_avg_crown, which has two columns: Group.1 (containing the unique genus names) and x (containing the mean crown diameter for each genus).
genus_avg_crown <- aggregate(nc_sc_data$AvgCdia, by = list(nc_sc_data$Genus), mean)




#extra credit 1
# Filter data for NC/SC only
nc_sc_data <- filter(tree_data, State %in% c("NC", "SC"))

# Extract genus from the scientific name
nc_sc_data$genus <- str_extract(nc_sc_data$ScientificName, "^[A-Za-z]+")

# Calculate average age for each genus
genus_age <- nc_sc_data %>%
  group_by(genus) %>%
  summarise(avg_age = mean(Age))

# Compare average ages of different genera
genus_age %>% arrange(desc(avg_age))



#extra credit 2

#First I filtered the tree_data dataset to only include records from North and South Carolina by creating a new dataset called nc_sc_data. Then I extracted the genus of each tree from the ScientificName column using the str_extract() function and saved it as a new column called genus.
#Then I calculated the average crown diameter and growth rate for each genus in the nc_sc_data dataset using the group_by() and summarise() functions from the dplyr package. The results were saved as a new dataset called genus_avg_cdia.
nc_sc_data <- filter(tree_data, State %in% c("NC", "SC"))
nc_sc_data$genus <- str_extract(nc_sc_data$ScientificName, "^[A-Za-z]+")
genus_avg_cdia <- nc_sc_data %>%
  group_by(genus) %>%
  summarise(avg_cdia = mean(AvgCdia..m.),
            avg_growth_rate = mean(TreeHt..m.)/mean(Age))

#Finally, I created a scatter plot using ggplot() from the ggplot2 package, with the x-axis representing the average crown diameter and the y-axis representing the average growth rate for each genus. 
ggplot(data = genus_avg_cdia, aes(x = avg_cdia, y = avg_growth_rate, label = genus)) + 
  geom_point(size = 3) + 
  geom_text(hjust = 0, vjust = 0) + 
  xlab("Average Crown Diameter (m)") + 
  ylab("Average Growth Rate (m/year)") + 
  ggtitle("Genus by Average Crown Diameter and Growth Rate")




#extra credit 3
# Filter data for NC/SC only
nc_sc_data <- filter(tree_data, State %in% c("NC", "SC"))

# Extract genus and species from the scientific name
nc_sc_data$genus <- str_extract(nc_sc_data$ScientificName, "^[A-Za-z]+")
nc_sc_data$species <- str_extract(nc_sc_data$ScientificName, "(?<=[A-Za-z])(\\s\\w+|\\s\\w+\\W\\w+)?(?=$|[xX])")

# Remove x in hybrid species and additional information after species
nc_sc_data$species <- gsub("x", "", nc_sc_data$species)
nc_sc_data$species <- gsub("\\s\\W\\w+$", "", nc_sc_data$species)

# Count number of species for each genus
species_count <- nc_sc_data %>%
  group_by(genus) %>%
  summarize(num_species = n_distinct(species))
