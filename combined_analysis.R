library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load interview-level metadata (contains IDs and attributes for each interview)
load("interview_data.RData")

# -------------------------------------------------------------------
# Load quotations dataset and reshape it into one-code-per-row format
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

# -------------------------------------------------------------------
# Load codebook and attach codegroup classifications to each quotation
# -------------------------------------------------------------------
codes <- read_excel("Rév8-codes.xlsx")

quotations <- quotations %>%
  
  # Match each quotation's code to its codegroup via the codebook
  left_join(
    codes %>% select(name, `codegroup 1`),
    by = c("code" = "name")
  ) %>%
  
  # Clean column name for easier use
  rename(codegroup = `codegroup 1`)

# -------------------------------------------------------------------
# Bring interview-level metadata (e.g., category) into quotations
# Matches quotation$document to interview_data$id
# -------------------------------------------------------------------
quotations <- quotations %>%
  left_join(
    interview_data %>%
      select(id, category),   # Add more metadata columns here if needed
    by = c("document" = "id")
  )

# -------------------------------------------------------------------
# Diagnostics: check whether all document ↔ id links matched correctly
# -------------------------------------------------------------------

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


plot_code_sentiment_across_categories <- function(data, code_name) {
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
    col = "skyblue",
    border = "white",
    ylim = c(0, 1),  # extra space for counts above bars
    main = paste("Share of Positive Sentiment for Code:", code_name),
    ylab = "Share Positive",
    xlab = "Category"
  )
  

  # Add n values 
  text(
    x = seq_along(sentiment_summary$share_pos),
    y = sentiment_summary$share_pos,
    labels = paste0("n=", sentiment_summary$n),
    pos = 1,
    cex = 0.8,
    col = "black"
  )
}
# Example usage
#compute_code_sentiment(quotations, "Biztonságérzet", "fiatal_bérlő")
plot_code_sentiment_across_categories(quotations, "Blaha Lujza tér")
