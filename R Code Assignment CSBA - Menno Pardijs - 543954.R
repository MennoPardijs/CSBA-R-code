library(jsonlite)
library(dplyr)
library(cluster)

set.seed(123)

###########################################################LOADING DATA############################################################
# Load and prepare the data
data_path <- "~/Downloads/TVs-all-merged.json"
data <- fromJSON(data_path)

# Extract product numbers (names of the list elements)
product <- names(data)

# Create a data frame with product numbers
products_df <- data.frame(Product_Number = product)

result_list <- list()

for (product_id in names(data)) {
  product_list <- data[[product_id]] 
  product_data <- bind_rows(product_list)
  product_data <- product_data %>% mutate(ProductID = product_id)
  result_list <- append(result_list, list(product_data))
}

# Combine all products into a single DataFrame
result_df <- bind_rows(result_list)

#Expanding the nested dataframe into one dataframe
expanded_features <- bind_rows(result_df$featuresMap)
final_df <- cbind(result_df %>% select(-featuresMap), expanded_features)


###########################################################BOOTSTRAPPING############################################################
# Function for bootstrapping
perform_bootstrapping <- function(data, num_bootstraps = 5) {
  set.seed(42)  # For reproducibility
  results <- list()
  
  # Split data into 5 bootstraps, with 63% training, 37% testing
  for (i in 1:num_bootstraps) {
    train_indices <- sample(seq_len(nrow(data)), size = 0.63 * nrow(data), replace = TRUE)
    test_indices <- setdiff(seq_len(nrow(data)), train_indices)
    
    train_df <- data[train_indices, ]
    test_df <- data[test_indices, ]
    
    # Store the split dataframes
    results[[i]] <- list(
      train_df = train_df,
      test_df = test_df
    )
  }
  
  return(results)
}

# Perform bootstrapping for 5 bootstraps
bootstrap_results <- perform_bootstrapping(final_df, num_bootstraps = 5)

###########################################################CLEAINING DATA W.R.T. INCH & HZ############################################################
# Define a function for cleaning the data
clean_data <- function(df) {
  df <- as.data.frame(lapply(df, function(x) {
    if (is.character(x)) {
      # Replace patterns like 65", 65'' or 65' with 65inch to create normalized definition of inch
      x <- gsub('(\\d+)"', '\\1inch', x)    
      x <- gsub("(\\d+)''", '\\1inch', x)   
      x <- gsub("(\\d+)'", '\\1inch', x)    
      x <- gsub("(\\d+)[\u201C\u201D\u275D\u275E]", "\\1inch", x, perl = TRUE)  
      
      # Replace other patterns regarding inch with the normalized "inch"
      x <- gsub('"|\\b(Inch(es)?| inch|-inch)\\b', 'inch', x, ignore.case = TRUE)
      
      # Normalize any remaining odd quotation marks (e.g., ” to inch)
      x <- gsub("(\\d+)[”]", "\\1inch", x) # Handles additional typographic quotes
      
      # Replace all variations of Hertz, hertz, Hz, etc., with "hz"
      x <- gsub("\\b(Hertz|Hz|HZ|hertz|'?\\s?-?hz)\\b", "Hz", x, ignore.case = TRUE)
    }
    x
  }))
  return(df)
}

# Apply the cleaning function to all the bootstraps
for (i in seq_along(bootstrap_results)) {
  bootstrap_results[[i]]$train_df <- clean_data(bootstrap_results[[i]]$train_df)
  bootstrap_results[[i]]$test_df <- clean_data(bootstrap_results[[i]]$test_df)
}


###########################################################ADDING NEW COLUMNS############################################################
# Updated process_dataframe function to ensure that only the necessary columns are retained

process_dataframe <- function(df, unique_brands, columns_needed) {
  
  # Extract brand names from the title or from the columns named brand
  extract_brand_from_title <- function(title, brands) {
    matched_brands <- brands[sapply(brands, function(brand) grepl(brand, title, ignore.case = TRUE))]
    if (length(matched_brands) > 0) {
      return(matched_brands[1])
    } else {
      first_word <- strsplit(title, "\\s+")[[1]][1]
      return(first_word)
    }
  }
  df$Extracted_Brand <- sapply(df$title, extract_brand_from_title, brands = unique_brands)
  
  # Extract inch information from the relevant column or title
  extract_inch_from_title <- function(title) {
    match <- regmatches(title, regexpr("\\b\\d+inch\\b", title, ignore.case = TRUE))
    if (length(match) > 0 && match != "") {
      return(match)
    } else {
      return(NA)
    }
  }
  df$Extracted_Inch <- sapply(df$title, extract_inch_from_title)
  df$Extracted_Inch <- ifelse(!is.na(df$Screen.Size.Class) & df$Screen.Size.Class != "",
                              df$Screen.Size.Class,
                              df$Extracted_Inch)
  df$Extracted_Inch <- ifelse(is.na(df$Extracted_Inch) | df$Extracted_Inch == "",
                              df$Screen.Size..Measured.Diagonally.,
                              df$Extracted_Inch)
  
  # Extract resolution (1080p, 720p, etc) that ends on "p" from title
  extract_resolution_from_title <- function(title) {
    match <- regmatches(title, regexpr("\\b\\d+p\\b", title, ignore.case = TRUE))
    if (length(match) > 0 && match != "") {
      return(match)
    } else {
      return(NA)
    }
  }
  df$Extracted_Resolution <- sapply(df$title, extract_resolution_from_title)
  
  # Extract maximum resolution (1920 x 1080, etc.) from the relevant column
  extract_max_resolution <- function(value) {
    match <- regmatches(value, regexpr("\\b\\d+\\s?[xX]\\s?\\d+\\b", value, ignore.case = TRUE))
    if (length(match) > 0 && match != "") {
      return(match)
    } else {
      return(NA)
    }
  }
  df$Extracted_Maximum_Resolution <- sapply(df$Maximum.Resolution, extract_max_resolution)
  df$Extracted_Maximum_Resolution <- ifelse(
    is.na(df$Extracted_Maximum_Resolution) | df$Extracted_Maximum_Resolution == "",
    sapply(df$title, extract_max_resolution),
    df$Extracted_Maximum_Resolution
  )
  
  # Extract refresh rate (120Hz, 60Hz, etc.)
  extract_refresh_rate <- function(value) {
    match <- regmatches(value, regexpr("\\b\\d+hz\\b", value, ignore.case = TRUE))
    if (length(match) > 0 && match != "") {
      return(match)
    } else {
      return(NA)
    }
  }
  df$Extracted_Refresh_Rate <- sapply(df$Screen.Refresh.Rate, extract_refresh_rate)
  df$Extracted_Refresh_Rate <- ifelse(
    is.na(df$Extracted_Refresh_Rate) | df$Extracted_Refresh_Rate == "",
    sapply(df$title, extract_refresh_rate),
    df$Extracted_Refresh_Rate
  )
  
  # Extract all model words words (that consist of two of the three token types: alphanumerical, numerical and special characters)
  extract_model_words <- function(title) {
    matches <- regmatches(title, gregexpr("[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*", title))
    if (length(matches) > 0 && matches[[1]] != "") {
      return(paste(unlist(matches), collapse = ", "))
    } else {
      return(NA)
    }
  }
  df$Extracted_Model_Words <- sapply(df$title, extract_model_words)
  
  # Filter the extracted model words based on only alphanumerical or numerical tokens and that it should not end on inch, p or Hz. This gives the productID (if it exists in the title)
  filter_model_words <- function(words) {
    if (!is.na(words)) {
      word_list <- unlist(strsplit(words, ", "))
      filtered_words <- word_list[
        grepl("^[a-zA-Z0-9]+$", word_list) & 
          nchar(word_list) >= 4 &             
          !grepl("(inch|p|Hz)$", word_list, ignore.case = TRUE)
      ]
      if (length(filtered_words) > 0) {
        return(paste(filtered_words, collapse = ", "))
      } else {
        return(NA)
      }
    } else {
      return(NA)
    }
  }
  df$Filtered_Model_Words <- sapply(df$Extracted_Model_Words, filter_model_words)
  
  # Only select the specified columns
  df <- df[, columns_needed, drop = FALSE]
  
  return(df)
}

# Extract unique brands from the dataset
unique_brands <- unique(na.omit(final_df$Brand))

# Define the columns/features that are used for further duplicate detection
columns_needed <- c(
  "Extracted_Brand",
  "Extracted_Inch",
  "Extracted_Resolution",
  "Extracted_Maximum_Resolution",
  "Extracted_Refresh_Rate",
  "Filtered_Model_Words",
  "modelID"
)

# Apply the updated cleaning/extraction of the features to all the different bootstraps (both train and test samples)
for (i in seq_along(bootstrap_results)) {
  bootstrap_results[[i]]$train_df <- process_dataframe(bootstrap_results[[i]]$train_df, unique_brands, columns_needed)
  bootstrap_results[[i]]$test_df <- process_dataframe(bootstrap_results[[i]]$test_df, unique_brands, columns_needed)
}

# Create minimal bootstrap dataframes
create_minimal_bootstrap <- function(bootstrap_sample) {
  list(
    train_df = bootstrap_sample$train_df[, columns_needed, drop = FALSE],
    test_df = bootstrap_sample$test_df[, columns_needed, drop = FALSE]
  )
}

# Apply the minimal bootstrap function to all bootstrap samples
minimal_bootstrap_results <- lapply(bootstrap_results, create_minimal_bootstrap)



###########################################################Obtaining Binary Vectors############################################################
# The code below is based on Algorithm 1 of the paper on Duplicate Detection in Web Shops using LSH to Reduce the Number of Computations (Van Dam et al, 2016)
# It uses a three step approach

create_binary_vectors <- function(data) {
  
  # Step 1: Extract unique values across all features
  unique_features <- list(
    Extracted_Brand = unique(na.omit(data$Extracted_Brand)),
    Extracted_Inch = unique(na.omit(data$Extracted_Inch)),
    Extracted_Resolution = unique(na.omit(data$Extracted_Resolution)),
    Extracted_Maximum_Resolution = unique(na.omit(data$Extracted_Maximum_Resolution)),
    Extracted_Refresh_Rate = unique(na.omit(data$Extracted_Refresh_Rate)),
    Filtered_Model_Words = unique(unlist(strsplit(data$Filtered_Model_Words, ", ")))
  )
  
  # Flatten the list of unique values and remove NAs
  unique_values <- unique(unlist(unique_features))
  unique_values <- unique_values[!is.na(unique_values)]
  
  # Step 2: Initialize the binary matrix
  binary_matrix <- matrix(0, nrow = nrow(data), ncol = length(unique_values))
  colnames(binary_matrix) <- unique_values 
  rownames(binary_matrix) <- data$modelID 
  
  # Step 3: Fill in the binary matrix for all the features
  for (i in 1:nrow(data)) {
    features_combined <- c(
      data$Extracted_Brand[i],
      data$Extracted_Inch[i],
      data$Extracted_Resolution[i],
      data$Extracted_Maximum_Resolution[i],
      data$Extracted_Refresh_Rate[i],
      unlist(strsplit(data$Filtered_Model_Words[i], ", "))
    )
    features_combined <- features_combined[!is.na(features_combined)] # Remove any NA values
    binary_matrix[i, features_combined] <- 1 # Set binary vector to 1 for matching features
  }
  
  return(binary_matrix)
}




#####################################################################MIN-HASHING##############################################################
#The code below is based on Algorithm 2 of the paper on Duplicate Detection in Web Shops using LSH to Reduce the Number of Computations (Van Dam et al, 2016)

min_hashing <- function(binary_matrix, n) {
  #Initialize the signature matrix
  num_products <- nrow(binary_matrix)  
  num_model_words <- ncol(binary_matrix) 
  signature_matrix <- matrix(Inf, nrow = n, ncol = num_products)
  
  #Assign product IDs (modelID) to columns of the signature matrix for later evaluation of the true positives
  colnames(signature_matrix) <- rownames(binary_matrix)
  
  #Generate random permutations for the min hashing
  row_permutations <- replicate(n, sample(1:num_model_words, num_model_words)) 
  
  #Filling in in the signature matrix
  for (i in 1:n) { 
    perm <- row_permutations[, i] 
    
    for (j in 1:num_products) {  
      product_vector <- binary_matrix[j, ]  
      permuted_vector <- product_vector[perm]  
      first_one_index <- which(permuted_vector == 1)[1]
      
      if (!is.na(first_one_index)) {
        signature_matrix[i, j] <- perm[first_one_index]  
      }
    }
  }
  
  return(signature_matrix)
}

###############################################################Defining the Jaccard Similarity####################################################################
# Defining the function of the Jaccard similarity based on the known union and intersection of two sets
jaccard_similarity <- function(set1, set2) {
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  return(ifelse(union > 0, intersection / union, 0))
}



# Function to filter candidate pairs based on a threshold set for the Jaccard similarity
filter_candidate_pairs_by_jaccard <- function(candidate_pairs, data, threshold = 0.3) {
  filtered_pairs <- list()
  
  for (pair in candidate_pairs) {
    product1 <- pair[1]
    product2 <- pair[2]
    
    model_words1 <- as.character(data[data$modelID == product1, "Filtered_Model_Words"])
    model_words2 <- as.character(data[data$modelID == product2, "Filtered_Model_Words"])
    
    # If model words are NA, treat them as empty strings
    model_words1 <- ifelse(length(model_words1) == 0 || is.na(model_words1), "", model_words1)
    model_words2 <- ifelse(length(model_words2) == 0 || is.na(model_words2), "", model_words2)
    
    # If the extracted model words (modelID) extracted from the title is identical, the products are duplicates
    if (model_words1 != "" && model_words2 != "" && model_words1 == model_words2) {
      filtered_pairs <- append(filtered_pairs, list(pair))
      next # Skip further checks for this pair
    }
    
    # Create sets for each product based on the relevant features
    set1 <- c(
      as.character(data[data$modelID == product1, "Extracted_Brand"]),
      as.character(data[data$modelID == product1, "Extracted_Inch"]),
      as.character(data[data$modelID == product1, "Extracted_Resolution"]),
      as.character(data[data$modelID == product1, "Extracted_Maximum_Resolution"]),
      as.character(data[data$modelID == product1, "Extracted_Refresh_Rate"]),
      unlist(strsplit(data[data$modelID == product1, "Filtered_Model_Words"], ", "))
    )
    set2 <- c(
      as.character(data[data$modelID == product2, "Extracted_Brand"]),
      as.character(data[data$modelID == product2, "Extracted_Inch"]),
      as.character(data[data$modelID == product2, "Extracted_Resolution"]),
      as.character(data[data$modelID == product2, "Extracted_Maximum_Resolution"]),
      as.character(data[data$modelID == product2, "Extracted_Refresh_Rate"]),
      unlist(strsplit(data[data$modelID == product2, "Filtered_Model_Words"], ", "))
    )
    
    # Compute simple Jaccard similarity between the sets
    similarity <- jaccard_similarity(set1, set2)
    
    # Filter based on the set threshold
    if (!is.na(similarity) && similarity >= threshold) {
      filtered_pairs <- append(filtered_pairs, list(pair))
    }
  }
  
  return(filtered_pairs)
}



###############################################################LSH-Algorithm####################################################################
# Updated locality-sensitive hashing function using the Jaccard similarity
locality_sensitive_hashing_with_jaccard <- function(signature_matrix, num_bands, data, jaccard_threshold = 0.6) {
  num_rows <- nrow(signature_matrix)
  num_cols <- ncol(signature_matrix)
  rows_per_band <- floor(num_rows / num_bands)
  
  # Debugging step to check if the number of bands is feasible
  if (rows_per_band == 0) stop("Number of bands is too high or signature matrix has too few rows.")
  
  global_buckets <- list()
  candidate_pairs <- list()
  
  # Generate buckets based on bands
  for (band in 1:num_bands) {
    start_row <- (band - 1) * rows_per_band + 1
    end_row <- min(band * rows_per_band, num_rows)
    band_signature <- signature_matrix[start_row:end_row, , drop = FALSE]
    
    for (product_idx in 1:num_cols) {
      band_signature_key <- paste(band_signature[, product_idx], collapse = "-")
      unique_bucket_key <- paste0("Band", band, "_", band_signature_key)
      
      # Add product to the appropriate bucket (remove duplicates such that the same product cannot be added to a bucket twice)
      if (!is.null(global_buckets[[unique_bucket_key]])) {
        global_buckets[[unique_bucket_key]] <- unique(c(global_buckets[[unique_bucket_key]], colnames(signature_matrix)[product_idx]))
      } else {
        global_buckets[[unique_bucket_key]] <- c(colnames(signature_matrix)[product_idx])
      }
    }
  }
  
  
  # Generate candidate pairs from buckets (only if a bucket has more than one product)
  for (bucket_name in names(global_buckets)) {
    products_in_bucket <- global_buckets[[bucket_name]]
    if (length(products_in_bucket) > 1) {
      
      # Create pairs from products in the same bucket
      pairs <- combn(products_in_bucket, 2, simplify = FALSE)
      candidate_pairs <- append(candidate_pairs, pairs)
    }
  }
  
  # Filter candidate pairs using Jaccard similarity (if lower than the threshold, they are removed as candidate pair)
  filtered_pairs <- filter_candidate_pairs_by_jaccard(candidate_pairs, data, jaccard_threshold)
  
  return(list(
    buckets = global_buckets,
    candidate_pairs = unique(filtered_pairs)
  ))
}



###########################################################Similarity Matrix ###############################################################
compute_similarity_matrix <- function(data, candidate_pairs) {
  # Extract unique product IDs from candidate pairs
  product_ids <- unique(unlist(candidate_pairs))
  num_products <- length(product_ids)
  
  # Initialize similarity matrix
  similarity_matrix <- matrix(0, nrow = num_products, ncol = num_products)
  rownames(similarity_matrix) <- product_ids
  colnames(similarity_matrix) <- product_ids
  
  
  for (pair in candidate_pairs) {
    product1 <- pair[1]
    product2 <- pair[2]
    
    # Create sets for each product
    set1 <- c(
      as.character(data[data$modelID == product1, "Extracted_Brand"]),
      as.character(data[data$modelID == product1, "Extracted_Inch"]),
      as.character(data[data$modelID == product1, "Extracted_Resolution"]),
      as.character(data[data$modelID == product1, "Extracted_Maximum_Resolution"]),
      as.character(data[data$modelID == product1, "Extracted_Refresh_Rate"]),
      unlist(strsplit(data[data$modelID == product1, "Filtered_Model_Words"], ", "))
    )
    
    set2 <- c(
      as.character(data[data$modelID == product2, "Extracted_Brand"]),
      as.character(data[data$modelID == product2, "Extracted_Inch"]),
      as.character(data[data$modelID == product2, "Extracted_Resolution"]),
      as.character(data[data$modelID == product2, "Extracted_Maximum_Resolution"]),
      as.character(data[data$modelID == product2, "Extracted_Refresh_Rate"]),
      unlist(strsplit(data[data$modelID == product2, "Filtered_Model_Words"], ", "))
    )
    
    # Compute simple Jaccard similarity
    similarity <- jaccard_similarity(set1, set2)
    
    # Update the initialized similarity matrix
    similarity_matrix[product1, product2] <- similarity
    similarity_matrix[product2, product1] <- similarity
  }
  
  # Return similarity matrix as distance (1 - similarity) such that it can be used for clustering
  return(as.dist(1 - similarity_matrix))
}






#################################################################Clustering###############################################################

perform_clustering <- function(similarity_matrix, threshold = 0.3, method = "average", clustering_type = "agglomerative") {
  if (clustering_type == "agglomerative") {                       # Perform agglomerative clustering with specified linkage method
    hc <- hclust(as.dist(similarity_matrix), method = method)
    clusters <- cutree(hc, h = threshold)
  } else if (clustering_type == "divisive") {                     # Perform divisive clustering
    diana_result <- diana(as.dist(similarity_matrix))
    clusters <- cutree(as.hclust(diana_result), h = threshold)
  } else if (clustering_type == "single_linkage") {               # Explicit case for single linkage agglomerative clustering
    hc <- hclust(as.dist(similarity_matrix), method = "single")
    clusters <- cutree(hc, h = threshold)
  } else {
    stop("Invalid clustering_type. Choose 'agglomerative', 'divisive', or 'single_linkage'.")
  }
  
  # Assign product IDs as cluster names
  names(clusters) <- rownames(as.matrix(similarity_matrix))
  return(clusters)
}


#################################################################Evaluation###############################################################
# Function to evaluate the different clustering methods
evaluate_with_clustering <- function(train_data, test_data, jaccard_threshold = 0.6, clustering_threshold = 0.3, clustering_type = "agglomerative", linkage_method = "average") {
  # Generate binary vectors from training data
  binary_vectors <- create_binary_vectors(train_data)
  
  # Perform Min-ashing on binary vectors
  signature_matrix <- min_hashing(binary_vectors, n = 100)
  
  # Apply LSH with simple Jaccard filtering to generate candidate pairs
  lsh_results <- locality_sensitive_hashing_with_jaccard(
    signature_matrix,
    num_bands = 5,
    data = test_data,
    jaccard_threshold = jaccard_threshold
  )
  
  # Compute similarity matrix for the candidate pairs computed before
  similarity_matrix <- compute_similarity_matrix(test_data, lsh_results$candidate_pairs)
  
  # Perform clustering on the similarity matrix
  if (clustering_type == "single_linkage") {
    clusters <- perform_clustering(similarity_matrix, threshold = clustering_threshold, method = "single", clustering_type = "agglomerative")
  } else {
    clusters <- perform_clustering(similarity_matrix, threshold = clustering_threshold, method = linkage_method, clustering_type = clustering_type)
  }
  
  # Extract predicted clusters
  predicted_clusters <- split(names(clusters), clusters)
  
  # Generate true clusters based on the assigned modelIDs
  true_clusters <- split(test_data$modelID, test_data$modelID)
  
  # Calculate true positives, false positives, and false negatives
  tp <- 0
  fp <- 0
  fn <- 0
  
  for (predicted_cluster in predicted_clusters) {
    products <- unique(predicted_cluster)
    true_labels <- unique(unlist(lapply(products, function(p) true_clusters[[p]])))
    
    if (length(true_labels) > 1) {
      tp <- tp + length(true_labels) - 1
      fp <- fp + length(products) - length(true_labels)
    } else {
      fp <- fp + length(products)
    }
  }
  
  # Calculate false negatives
  fn <- sum(sapply(true_clusters, length)) - tp
  
  # Calculate Precision, Recall, and the F1-Measure
  precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
  recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
  f1 <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)
  
  # Calculate Pair Quality, Pair Completeness, and the F1*-Measure
  total_possible_comparisons <- choose(nrow(test_data), 2)
  pair_quality <- ifelse(length(lsh_results$candidate_pairs) > 0, tp / length(lsh_results$candidate_pairs), 0)
  pair_completeness <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
  f1_star <- ifelse((pair_quality + pair_completeness) > 0, 
                    2 * (pair_quality * pair_completeness) / (pair_quality + pair_completeness), 
                    0)
  
  # Calculate Fraction of Comparisons Made
  fraction_comparisons_made <- ifelse(total_possible_comparisons > 0, 
                                      length(lsh_results$candidate_pairs) / total_possible_comparisons, 
                                      0)
  
  # Return a list which consists of all the computed metrics in the previous steps
  return(list(
    precision = precision,
    recall = recall,
    f1 = f1,
    pair_quality = pair_quality,
    pair_completeness = pair_completeness,
    f1_star = f1_star,
    fraction_comparisons_made = fraction_comparisons_made
  ))
}


# Perform clustering metrics for agglomerative, divisive, and single linkage clustering for every bootstrap
bootstrap_clustering_metrics <- list(
  agglomerative = lapply(minimal_bootstrap_results, function(split) {
    evaluate_with_clustering(
      train_data = split$train_df, 
      test_data = split$test_df,
      jaccard_threshold = 0.6,          
      clustering_threshold = 0.3,       
      clustering_type = "agglomerative", 
      linkage_method = "average"        
    )
  }),
  divisive = lapply(minimal_bootstrap_results, function(split) {
    evaluate_with_clustering(
      train_data = split$train_df, 
      test_data = split$test_df,
      jaccard_threshold = 0.6,          
      clustering_threshold = 0.3,       
      clustering_type = "divisive"      
    )
  }),
  single_linkage = lapply(minimal_bootstrap_results, function(split) {
    evaluate_with_clustering(
      train_data = split$train_df, 
      test_data = split$test_df,
      jaccard_threshold = 0.6,          
      clustering_threshold = 0.3,       
      clustering_type = "single_linkage" 
    )
  })
)


# Aggregate clustering metrics across all bootstraps for each clustering type and compute the averages of all the metrics
average_clustering_metrics <- lapply(bootstrap_clustering_metrics, function(metrics_list) {
  setNames(
    lapply(names(metrics_list[[1]]), function(metric_name) {
      mean(sapply(metrics_list, function(metrics) metrics[[metric_name]]), na.rm = TRUE)
    }),
    names(metrics_list[[1]])
  )
})


# Print all the average clustering performance metrics for the different clustering types
# Agglomerative clustering with average linkage
cat("Average Clustering Performance Metrics Across Bootstraps (Agglomerative):\n")
cat("Precision:", format(average_clustering_metrics$agglomerative[["precision"]], digits = 4), "\n")
cat("Recall:", format(average_clustering_metrics$agglomerative[["recall"]], digits = 4), "\n")
cat("F1 Measure:", format(average_clustering_metrics$agglomerative[["f1"]], digits = 4), "\n")
cat("Pair Quality:", format(average_clustering_metrics$agglomerative[["pair_quality"]], digits = 4), "\n")
cat("Pair Completeness:", format(average_clustering_metrics$agglomerative[["pair_completeness"]], digits = 4), "\n")
cat("F1* Measure:", format(average_clustering_metrics$agglomerative[["f1_star"]], digits = 4), "\n")
cat("Fraction of Comparisons Made:", format(average_clustering_metrics$agglomerative[["fraction_comparisons_made"]], digits = 4), "\n")

# Divisive clustering
cat("\nAverage Clustering Performance Metrics Across Bootstraps (Divisive):\n")
cat("Precision:", format(average_clustering_metrics$divisive[["precision"]], digits = 4), "\n")
cat("Recall:", format(average_clustering_metrics$divisive[["recall"]], digits = 4), "\n")
cat("F1 Measure:", format(average_clustering_metrics$divisive[["f1"]], digits = 4), "\n")
cat("Pair Quality:", format(average_clustering_metrics$divisive[["pair_quality"]], digits = 4), "\n")
cat("Pair Completeness:", format(average_clustering_metrics$divisive[["pair_completeness"]], digits = 4), "\n")
cat("F1* Measure:", format(average_clustering_metrics$divisive[["f1_star"]], digits = 4), "\n")
cat("Fraction of Comparisons Made:", format(average_clustering_metrics$divisive[["fraction_comparisons_made"]], digits = 4), "\n")

# Single linkage
cat("\nAverage Clustering Performance Metrics Across Bootstraps (Single Linkage):\n")
cat("Precision:", format(average_clustering_metrics$single_linkage[["precision"]], digits = 4), "\n")
cat("Recall:", format(average_clustering_metrics$single_linkage[["recall"]], digits = 4), "\n")
cat("F1 Measure:", format(average_clustering_metrics$single_linkage[["f1"]], digits = 4), "\n")
cat("Pair Quality:", format(average_clustering_metrics$single_linkage[["pair_quality"]], digits = 4), "\n")
cat("Pair Completeness:", format(average_clustering_metrics$single_linkage[["pair_completeness"]], digits = 4), "\n")
cat("F1* Measure:", format(average_clustering_metrics$single_linkage[["f1_star"]], digits = 4), "\n")
cat("Fraction of Comparisons Made:", format(average_clustering_metrics$single_linkage[["fraction_comparisons_made"]], digits = 4), "\n")

