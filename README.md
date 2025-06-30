# Abundance-from-biom
This file tell you how to get the genera and Species from the BIOM file
Load & clean your data: Reads in the full microbiome table (microbiome.csv), pulls out only the species-level names (everything starting with s__…), drops any columns that didn’t match, and saves the “species-only” table.

Handle missing values: Re-reads that species-only file, replaces any remaining NAs with zero, and makes sure every column is numeric so we can do math on it.

Compute relative abundances: Converts each sample’s raw counts into percentages of the total (so every row sums to 100%).

Prune rare species: Finds and removes any species that never occur (all zeros) or that appear above 0.001% in fewer than 20% of your samples—keeping only the species that are reasonably abundant across your dataset.
separating genus and species
  

  # Load the dataset

  microdata<- read.csv("microbiome.csv")
  
  # Extract all column names
  all_columns <- colnames(microdata)
  
  # Initialize vector for species names
  species_names <- c()
  
  # Process each column to extract species names
  for (col in all_columns) {
    # Extract species name (e.g., "s__UBA3207.sp900314585")
    species_match <- regmatches(col, regexpr("s__[^.]+(\\.[^.]*)*", col))
    if (length(species_match) > 0) {
      species_names <- c(species_names, species_match)
    } else {
      species_names <- c(species_names, NA)
    }
  }
  
  # Create a new dataset with the same data
  microbiome_species <- microdata
  
  # Rename the columns of the new dataset with the extracted species names
  colnames(microbiome_species) <- species_names
  
  # Remove columns that have NA as a column name
  microbiome_species <- microbiome_species[, !is.na(colnames(microbiome_species))]
  
  # Count the number of NA columns
  na_count <- sum(is.na(species_names))
  
  # Count the total number of correct columns
  correct_columns_count <- ncol(microbiome_species)
  
  # Check for duplicate column names
  duplicate_columns <- species_names[duplicated(species_names) & !is.na(species_names)]
  
  # Save the modified dataframe to a new CSV file
  write.csv(microbiome_species, "microbiome_species_only.csv", row.names = FALSE)
  
  # Output the results
  cat("Number of NA columns:", na_count, "\n")
  cat("Total number of correct columns:", correct_columns_count, "\n")
  cat("Duplicate column names:", duplicate_columns, "\n")
  
  # Load necessary library
  library(dplyr)
  
#this is the microbiome species only dataset
  microbiome_species<- read.csv("microbiome_species_only.csv")
  
  # Replace NA values with 0
  microbiome_species[is.na(microbiome_species)] <- 0
  
  # Confirm NAs are handled
  cat("Remaining NA values after replacement:", sum(is.na(microbiome_species)), "\n")
  
  View(microbiome_species)
  
  # Calculate the relative abundance
  
  #make all the colums numeric
  microbiome_species <- microbiome_species %>% mutate(across(everything(), ~ as.numeric(.)))

  # Inspect data structure
  str(microbiome_species)
  summary(microbiome_species)
  
  # Check for unexpected values
  any(is.na(microbiome_species))  # Should return FALSE if no NAs
  any(microbiome_species== 0)    # Should return FALSE if no zeros
  
#calculate the relative abundance
  relative_abundance <- (microbiome_species / rowSums(microbiome_species)) *100
  
head(relative_abundance)

# Check the structure of the relative abundance data
str(relative_abundance)

# View first few rows and columns
print(head(relative_abundance[, 1:10]))  # Print first 10 species to check

# Identify columns (species) where all values are zero
zero_species_cols <- colSums(relative_abundance == 0) == nrow(relative_abundance)

# Count the number of columns with all zero values
num_zero_species <- sum(zero_species_cols)
cat("Number of species with all zero values:", num_zero_species, "\n")

# Remove species (columns) with all zeros
relative_abundance_cleaned <- relative_abundance[, !zero_species_cols]

# Confirm the changes
cat("Remaining species count after removal:", ncol(relative_abundance_cleaned), "\n")

# Define threshold values
abundance_threshold <- 0.001
sample_threshold <- 0.20 * nrow(relative_abundance_cleaned)  # 20% of the samples

# Count the number of samples where species have relative abundance > 0.001
species_above_threshold <- colSums(relative_abundance_cleaned > abundance_threshold)

# Identify species that meet the criteria
selected_species <- species_above_threshold > sample_threshold

# Count the number of species that meet the criteria
cat("Number of species retained:", sum(selected_species), "\n")

# Subset the data to keep only those species
filtered_relative_abundance <- relative_abundance_cleaned[, selected_species]








