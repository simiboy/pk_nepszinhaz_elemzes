library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

#1. Loading data

#1.1 Load interview-level metadata (contains IDs and attributes for each interview)
load("interview_data.RData")

# -------------------------------------------------------------------
#1.2 Load quotations dataset and reshape it into one-code-per-row format
# -------------------------------------------------------------------
quotations <- read_excel("Rév8-quotations.xlsx") %>%
  
  # Strip file extension from document name (e.g., "TBSA02.docx" → "TBSA02")
  mutate(document = sub("\\..*", "", document)) %>%
  
  # If a quotation has multiple codes (comma-separated), create separate rows
  separate_rows(codes, sep = ",\\s*") %>%
  
  # Rename 'codes' → 'code' (single code per row now)
  rename(code = codes) %>%
  
  # Remove empty or unused column
  select(-comment)

#1.3 Load codebook and attach codegroup classifications to each quotation

codes <- read_excel("Rév8-codes.xlsx")

quotations <- quotations %>%
  
  # Match each quotation's code to its codegroup via the codebook
  left_join(
    codes %>% select(name, `codegroup 1`),
    by = c("code" = "name")
  ) %>%
  
  # Clean column name for easier use
  rename(codegroup = `codegroup 1`)

#1.4 Bring interview-level metadata (e.g., category) into quotations
# Matches quotation$document to interview_data$id

quotations <- quotations %>%
  left_join(
    interview_data %>%
      select(id, category, elhelyezkedés, gender),   # Add more metadata columns here if needed
    by = c("document" = "id")
  )

#1.5 Diagnostics: check whether all document ↔ id links matched correctly


# Quotations whose document code does NOT exist in interview_data$id
# Ideally should return zero rows
missing_in_interview <- quotations %>%
  anti_join(interview_data, by = c("document" = "id")) %>%
  distinct(document)

missing_in_interview  # should be empty

# Interview IDs that never appear in the quotations dataset
# Ideally should also return zero rows
missing_in_quotations <- interview_data %>%
  anti_join(quotations, by = c("id" = "document")) %>%
  distinct(id)

missing_in_quotations  # should be empty


# =======  ANALYSIS ==============

#2 Analysing based on citizen cateogries

compute_code_sentiment <- function(data, my_code, my_category) {
  
  # Step 1: Filter initial df
  df <- data %>%
    filter(code == my_code,
           category == my_category)
  
  # Step 2: Find matching rows with different code
  df_pos_neg <- data %>%
    filter(codegroup == "Pozitív / Negatív") %>%
    select(-codegroup)  # Remove codegroup to avoid duplication if needed
  
  # Step 3: Join with original df via quotation and other columns except code
  # Assuming 'quotation' is the unique connecting key
  df <- df %>%
    left_join(df_pos_neg %>% select(quotation, code), 
              by = "quotation", 
              suffix = c("", "_posneg"))
  
  df_summary <- df %>%
    # Keep only rows that matched with Pos/Neg
    filter(!is.na(code_posneg)) %>%
    group_by(document) %>%
    summarise(
      total_posneg = n(),  # total rows in Pos/Neg for this document
      nr_pos = sum(code_posneg == "Pozitív"),  # count of Pos codes
      share_pos = nr_pos / total_posneg   # share of Pos
    )
  print(df_summary)
  share_positive <- mean(df_summary$share_pos, na.rm = T)
  return (share_positive)
}


plot_code_sentiment_across_categories <- function(data, code_name, main_title) {
  # Get all unique categories
  categories <- unique(data$category)
  
  # Compute sentiment and counts for each category
  sentiment_summary <- lapply(categories, function(cat) {
    df <- data %>% filter(code == code_name, category == cat)
    
    # Compute positive sentiment
    share_pos <- compute_code_sentiment(data, code_name, cat)
    
    # Count of quotations in this category for this code
    n_rows <- nrow(df)
    
    return(data.frame(category = cat, share_pos = share_pos, n = n_rows))
  }) %>%
    bind_rows()
  
  # Basic barplot
  barplot(
    sentiment_summary$share_pos,
    names.arg = sentiment_summary$category,
    col = "#00AC57",
    border = "white",
    ylim = c(0, 1),  # extra space for counts above bars
    main = paste("Share of Positive Sentiment for\n", main_title),
    ylab = "Share Positive",
    xlab = "Category"
  )
  

  # Add n values 
  text(
    x = seq_along(sentiment_summary$share_pos),
    y = sentiment_summary$share_pos,
    labels = paste0("n=", sentiment_summary$n),
    pos = 1,
    cex = 1,
    col = "black"
  )
}
# Example usage
compute_code_sentiment(quotations, "Biztonságérzet", "fiatal_bérlő")
plot_code_sentiment_across_categories(quotations, "Blaha Lujza tér", "Blaha Lujza tér")

unique_codes <- quotations %>%
  filter(codegroup %in% c(
    "Utcával kapcsolatos attitűdök", 
    "Utca változásai", 
    "Utcai fejlesztések értékelése", 
    "Jövőkép az utcáról", 
    "Társasház működése (jelenleg)", 
    "Jövőkép a társasházról", 
    "Önkormányzati szerep", 
    "Lakhatási helyzet"
  )) %>%
  distinct(code, codegroup) %>%
  arrange(codegroup)


# Loop through each code and inspect the plot
for (i in 1:nrow(unique_codes)) {
  code_name <- unique_codes$code[i]
  code_group <- unique_codes$codegroup[i]
  
  if (!is.na(code_name)) {
    
    # Plot with code group in parentheses
    plot_code_sentiment_across_categories(quotations, code_name, 
                                          main_title = paste0(code_name, " (", code_group, ")"))
    
    # Wait for user input with option to quit
    user_input <- readline(prompt = paste("Press [Enter] to continue to the next code, or type 'q' to quit:", 
                                          code_name, "(", code_group, ")"))
    if (tolower(user_input) == "q") {
      cat("Quitting the loop.\n")
      break
    }
  }
}

#=========================================
#3 Analysing based on gender
#=========================================


compute_code_sentiment_gender <- function(data, my_code, my_gender) {
  
  # Step 1: Filter initial df
  df <- data %>%
    filter(code == my_code,
           gender == my_gender)
  
  # Step 2: Find matching rows with different code
  df_pos_neg <- data %>%
    filter(codegroup == "Pozitív / Negatív") %>%
    select(-codegroup)
  
  # Step 3: Join with original df via quotation
  df <- df %>%
    left_join(df_pos_neg %>% select(quotation, code),
              by = "quotation",
              suffix = c("", "_posneg"))
  
  df_summary <- df %>%
    filter(!is.na(code_posneg)) %>%
    group_by(document) %>%
    summarise(
      total_posneg = n(),
      nr_pos = sum(code_posneg == "Pozitív"),
      share_pos = nr_pos / total_posneg
    )
  
  print(df_summary)
  share_positive <- mean(df_summary$share_pos, na.rm = TRUE)
  return(share_positive)
}

plot_code_sentiment_across_genders <- function(data, code_name, main_title) {
  
  # All gender groups in the data
  genders <- unique(data$gender)
  
  # Compute sentiment per gender
  sentiment_summary <- lapply(genders, function(g) {
    df <- data %>% filter(code == code_name, gender == g)
    
    # Compute positive sentiment for this gender
    share_pos <- compute_code_sentiment_gender(data, code_name, g)
    
    # Count rows for this code & gender
    n_rows <- nrow(df)
    
    return(data.frame(gender = g, share_pos = share_pos, n = n_rows))
  }) %>% bind_rows()
  
  # Barplot
  barplot(
    sentiment_summary$share_pos,
    names.arg = sentiment_summary$gender,
    col = "#C38BA4",
    border = "white",
    ylim = c(0, 1),
    main = paste("Share of Positive Sentiment for\n", main_title),
    ylab = "Share Positive",
    xlab = "Gender"
  )
  
  # Add counts under bars
  text(
    x = seq_along(sentiment_summary$share_pos),
    y = sentiment_summary$share_pos,
    labels = paste0("n=", sentiment_summary$n),
    pos = 1,
    cex = 1,
    col = "black"
  )
}


# Example usage
compute_code_sentiment_gender(quotations, "Biztonságérzet", "Férfi")
plot_code_sentiment_across_genders(quotations, "Blaha Lujza tér", "Blaha Lujza tér")

# Loop through each code and inspect the plot
for (i in 1:nrow(unique_codes)) {
  code_name <- unique_codes$code[i]
  code_group <- unique_codes$codegroup[i]
  
  if (!is.na(code_name)) {
    
    # Plot with code group in parentheses
    plot_code_sentiment_across_genders(quotations, code_name, 
                                          main_title = paste0(code_name, " (", code_group, ")"))
    
    # Wait for user input with option to quit
    user_input <- readline(prompt = paste("Press [Enter] to continue to the next code, or type 'q' to quit:", 
                                          code_name, "(", code_group, ")"))
    if (tolower(user_input) == "q") {
      cat("Quitting the loop.\n")
      break
    }
  }
}




#=========================================
# 4 Analysing based on elhelyezkedés
#=========================================

compute_code_sentiment_location <- function(data, my_code, my_location) {
  
  # Step 1: Filter initial df
  df <- data %>%
    filter(code == my_code,
           elhelyezkedés == my_location)
  
  # Step 2: Find matching rows with different code
  df_pos_neg <- data %>%
    filter(codegroup == "Pozitív / Negatív") %>%
    select(-codegroup)
  
  # Step 3: Join with original df via quotation
  df <- df %>%
    left_join(df_pos_neg %>% select(quotation, code),
              by = "quotation",
              suffix = c("", "_posneg"))
  
  df_summary <- df %>%
    filter(!is.na(code_posneg)) %>%
    group_by(document) %>%
    summarise(
      total_posneg = n(),
      nr_pos = sum(code_posneg == "Pozitív"),
      share_pos = nr_pos / total_posneg
    )
  
  print(df_summary)
  share_positive <- mean(df_summary$share_pos, na.rm = TRUE)
  return(share_positive)
}

plot_code_sentiment_across_locations <- function(data, code_name, main_title) {
  
  # All elhelyezkedés groups
  locations <- levels(data$elhelyezkedés)
  
  # Compute sentiment per location
  sentiment_summary <- lapply(locations, function(loc) {
    df <- data %>% filter(code == code_name, elhelyezkedés == loc)
    
    share_pos <- compute_code_sentiment_location(data, code_name, loc)
    n_rows <- nrow(df)
    
    return(data.frame(elhelyezkedés = loc, share_pos = share_pos, n = n_rows))
  }) %>% bind_rows()
  
  old_par <- par(mar = c(10, 4, 4, 2))  # bottom = 10 lines
  
  # barplot without names, save bar centers
  bar_centers <- barplot(
    sentiment_summary$share_pos,
    names.arg = FALSE,        # we add rotated labels manually
    col = "#3A439A",
    border = "white",
    ylim = c(0, 1),
    main = paste("Share of Positive Sentiment for\n", main_title),
    ylab = "Share Positive",
    xlab = ""
  )
  
  text(
    x = bar_centers, 
    y = par("usr")[3] - 0.02,  # slightly below plot region
    labels = sentiment_summary$elhelyezkedés,
    srt = 45,                  # rotation angle
    xpd = TRUE,                # allow drawing in margin area
    adj = 1,
    cex = 0.9
  )
  
  # Add counts under bars (slightly above the labels)
  text(
    x = bar_centers,
    y = sentiment_summary$share_pos,
    labels = paste0("n=", sentiment_summary$n),
    pos = 1,
    cex = 1,
    col = "black"
  )
  
  # Reset to previous plot settings
  par(old_par)
}


# Example usage:
plot_code_sentiment_across_locations(quotations, "Teleki tér", "Teleki tér")

# Loop through each code and inspect the plot
for (i in 1:nrow(unique_codes)) {
  code_name <- unique_codes$code[i]
  code_group <- unique_codes$codegroup[i]
  
  if (!is.na(code_name)) {
    
    # Plot with code group in parentheses
    plot_code_sentiment_across_locations(quotations, code_name, 
                                          main_title = paste0(code_name, " (", code_group, ")"))
    
    # Wait for user input with option to quit
    user_input <- readline(prompt = paste("Press [Enter] to continue to the next code, or type 'q' to quit:", 
                                          code_name, "(", code_group, ")"))
    if (tolower(user_input) == "q") {
      cat("Quitting the loop.\n")
      break
    }
  }
}


#====================================
# 5. Codegroup distribution per category
plot_codegroup_occurrence <- function(df, codegroup_name, as_percentage = TRUE) {
  # Filter for the specified codegroup
  df_filtered <- df[df$codegroup == codegroup_name, ]
  
  # Count each code only once per document
  df_unique <- unique(df_filtered[, c("document", "code", "category")])
  
  # Create table of occurrences
  df_counts <- table(df_unique$code, df_unique$category)
  
  # If percentages requested, convert counts
  if (as_percentage) {
    category_totals <- colSums(df_counts)  # total docs per category
    df_counts <- sweep(df_counts, 2, category_totals, FUN = "/") * 100
    y_label <- "Percentage of documents"
  } else {
    y_label <- "Count"
  }
  
  # Define colors
  categories <- colnames(df_counts)
  custom_colors <- c("#00AC57", "#254335", "#DF7201","#3A439A", "#80D5AB", "#C896AC")
  colors <- setNames(custom_colors[1:length(categories)], categories)
  
  # Plot side-by-side barplot with space for rotated labels
  par(mar = c(8, 5, 4, 2))  # increase bottom margin for x-axis labels
  bp <- barplot(t(df_counts),
                beside = TRUE,
                col = colors,
                xlab = "",
                ylab = y_label,
                names.arg = rep("", nrow(df_counts)),  # <- suppress default labels
                main = paste("Occurrences of categories per code in codegroup:\n", codegroup_name),
                ylim = c(0, max(df_counts) * 1.2))
  
  # Rotate x-axis labels 45 degrees
  text(x = colMeans(bp), 
       y = par("usr")[3] - 0.05 * max(df_counts), 
       labels = rownames(df_counts), 
       srt = 45, 
       adj = 1, 
       xpd = TRUE)
  
  # Add legend
  legend("topright", legend = categories, fill = colors, xpd = TRUE)
}

# Example usage:
plot_codegroup_occurrence(quotations, "Ideköltözés okai és körülményei")
plot_codegroup_occurrence(quotations, "Utcán mit változtatna")
plot_codegroup_occurrence(quotations, "Utcahasználat (jelenleg)")

#====================================
# 6. Codegroup distribution per GENDER
plot_codegroup_gender <- function(df, codegroup_name, as_percentage = TRUE) {
  # Filter for the specified codegroup
  df_filtered <- df[df$codegroup == codegroup_name, ]
  
  # Count each code only once per document
  df_unique <- unique(df_filtered[, c("document", "code", "gender")])
  
  # Create table of occurrences by gender
  df_counts <- table(df_unique$code, df_unique$gender)
  
  # If percentages requested, convert counts
  if (as_percentage) {
    gender_totals <- colSums(df_counts)  # total docs per gender
    df_counts <- sweep(df_counts, 2, gender_totals, FUN = "/") * 100
    y_label <- "Percentage of documents"
  } else {
    y_label <- "Count"
  }
  
  # Define colors
  genders <- colnames(df_counts)
  custom_colors <- c("#00AC57", "#3A439A")
  colors <- setNames(custom_colors[1:length(genders)], genders)
  
  # Plot side-by-side barplot with space for rotated labels
  par(mar = c(8, 5, 4, 2))  # increase bottom margin for x-axis labels
  bp <- barplot(t(df_counts),
                beside = TRUE,
                col = colors,
                xlab = "",              
                names.arg = rep("", nrow(df_counts)),  # <- suppress default labels
                ylab = y_label,
                main = paste("Occurrences of codes by gender in codegroup:\n", codegroup_name),
                ylim = c(0, max(df_counts) * 1.2))
  
  # Rotate x-axis labels 45 degrees
  text(x = colMeans(bp), 
       y = par("usr")[3] - 0.05 * max(df_counts), 
       labels = rownames(df_counts), 
       srt = 45, 
       adj = 1, 
       xpd = TRUE)
  
  # Add legend
  legend("topright", legend = genders, fill = colors, xpd = TRUE)
}

# Example usage:
plot_codegroup_gender(quotations, "Ideköltözés okai és körülményei")
plot_codegroup_gender(quotations, "Utcán mit változtatna")
plot_codegroup_gender(quotations, "Utcahasználat (jelenleg)")


