
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(stringr)

# Set as default font for base R plots
font_add("InterItalic", "Inter-Italic.otf")  
showtext_auto()
par(family = "InterItalic")


# ==============================
#1. Loading data

#1.1 Load interview-level metadata (contains IDs and attributes for each interview)
load("interview_data.RData")

categories <- levels(interview_data$category)

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
      select(id, category, elhelyezkedés, gender, f_years_in_apartment_grp, age_group, housing_type_aggr),   # Add more metadata columns here if needed
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

#2 Comparative analysis

#2.1. Function to compute share_pos for ANY grouping variable

quotations <- quotations %>%
  mutate(
    category = as.character(category),
    gender   = as.character(gender),
    code     = as.character(code)
  )
compute_sentiment <- function(data, code_name, group_var, group_value) {
  
  df <- data %>%
    filter(code == code_name,
           !!sym(group_var) == group_value)
  
  df_pos_neg <- data %>%
    filter(codegroup == "Pozitív / Negatív") %>%
    select(quotation, code)
  
  df <- df %>%
    left_join(df_pos_neg, by = "quotation", suffix = c("", "_posneg"))
  
  df_summary <- df %>%
    filter(!is.na(code_posneg)) %>%
    group_by(document) %>%
    summarise(
      total_posneg = n(),
      nr_pos = sum(code_posneg == "Pozitív"),
      share_pos = nr_pos / total_posneg,
      .groups = "drop"
    )
  
  # Return both share_pos and N (total rows across documents)
  tibble(
    share_pos = mean(df_summary$share_pos, na.rm = TRUE),
    N = sum(df_summary$total_posneg, na.rm = TRUE)
  )
}

# ---------------------------------------------
# 2.2. Build a complete table: code × (categories + genders)
# ---------------------------------------------
categories <- sort(unique(quotations$category))     # 3 levels
genders    <- sort(unique(quotations$gender))       # 2 levels

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
  arrange(codegroup) %>%
  pull(code)

# Build all combinations
grid <- expand.grid(
  code = unique_codes,
  group = c(categories, genders),
  stringsAsFactors = FALSE
)

# Add type field (category or gender)
grid <- grid %>%
  mutate(group_type = ifelse(group %in% categories, "category", "gender"))

# Compute sentiment share for each cell
results <- grid %>%
  rowwise() %>%
  mutate(temp = list(compute_sentiment(
    quotations, 
    code_name = code,
    group_var = ifelse(group_type == "category", "category", "gender"),
    group_value = group
  ))) %>%
  unnest_wider(temp) %>%   # creates share_pos and N columns
  ungroup()


# ---------------------------------------------
# 2.3. Create separate color scales
#    - Categories: white → #00AC57
#    - Genders   : white → #C38BA4
# ---------------------------------------------

# ggplot requires separate numeric scale → convert color to palette scale
results <- results %>%
  mutate(
    shade = share_pos   # 0–1 scale for shading
  )

# ---------------------------------------------
# 2.4. Plot: heatmap with custom shading per group type
# ---------------------------------------------
results <- results %>%
  left_join(
    quotations %>% distinct(code, codegroup),  # get one codegroup per code
    by = "code"
  )

results <- results %>%
  arrange(codegroup, code) %>%
  mutate(
    code = factor(code, levels = unique(code)),
    codegroup = factor(codegroup)
  )

ggplot(results, aes(x = group, y = code, fill = shade)) +
  geom_tile(color = "white") +
  
  # Add N as text
  geom_text(aes(label = N), size = 3, color = "#444") +
  
  # Group codes on y-axis by codegroup
  facet_grid(
    rows = vars(codegroup),
    cols = vars(group_type), 
    scales = "free", 
    space = "free"
  ) +
  
  scale_fill_gradient2(
    name = "Share positive",
    limits = c(0, 1),
    midpoint = 0.5,
    low = "#C38BA4",
    mid = "white",
    high = "#00AC57",     # gradient for categories
    na.value = "grey90"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    strip.text   = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.spacing = unit(0.4, "lines")
  )


#==========================
#2.5 Same with elhelyezkedés

# Get levels in the original order
locations <- levels(quotations$elhelyezkedés)

# Ensure 'elhelyezkedés' is a character/factor
quotations <- quotations %>%
  mutate(
    elhelyezkedés = as.character(elhelyezkedés)
  )

# Build a grid: code × elhelyezkedés
grid_locations <- expand.grid(
  code = unique_codes,
  group = locations,
  stringsAsFactors = FALSE
) %>%
  mutate(group_type = "elhelyezkedés")

# Compute sentiment for each code × location combination
results_locations <- grid_locations %>%
  rowwise() %>%
  mutate(temp = list(compute_sentiment(
    quotations,
    code_name = code,
    group_var = "elhelyezkedés",
    group_value = group
  ))) %>%
  unnest_wider(temp) %>%
  ungroup()

# Add shade for heatmap
results_locations <- results_locations %>%
  mutate(shade = share_pos) %>%
  left_join(
    quotations %>% distinct(code, codegroup),
    by = "code"
  ) %>%
  arrange(codegroup, code) %>%
  mutate(
    code = factor(code, levels = unique(code)),
    codegroup = factor(codegroup)
  )

# Plot heatmap
ggplot(results_locations, aes(x = group, y = code, fill = shade)) +
  geom_tile(color = "white") +
  geom_text(aes(label = N), size = 3, color = "#444") +
  facet_grid(
    rows = vars(codegroup),
    cols = vars(group_type), 
    scales = "free", 
    space = "free"
  ) +
  scale_fill_gradient2(
    name = "Share positive",
    limits = c(0, 1),
    midpoint = 0.5,
    low = "#C38BA4",   # can adjust if desired
    mid = "white",
    high = "#00AC57",
    na.value = "grey90"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    strip.text   = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.spacing = unit(0.4, "lines")
  )


# ---------------------------------------------
# 2.4b. Heatmap: using age_group, gender and housing_type_aggr on x-axis
# ---------------------------------------------

# Ensure we have character group labels
quotations <- quotations %>%
  mutate(
    age_group = as.character(age_group),
    gender = as.character(gender),
    housing_type_aggr = as.character(housing_type_aggr)
  )

 # Prefer factor level ordering from `interview_data` (if present), fallback to available values in `quotations`
age_levels <- if(!is.null(levels(interview_data$age_group))) levels(interview_data$age_group) else sort(unique(quotations$age_group))
gender_levels <- if(!is.null(levels(interview_data$gender))) levels(interview_data$gender) else sort(unique(quotations$gender))
housing_levels <- if(!is.null(levels(interview_data$housing_type_aggr))) levels(interview_data$housing_type_aggr) else sort(unique(quotations$housing_type_aggr))

# Build grids and set group as a factor with the desired ordering per variable
grid_age <- expand.grid(code = unique_codes, group = age_levels, stringsAsFactors = FALSE) %>%
  mutate(group_type = "age_group", group = factor(group, levels = age_levels))
grid_gender <- expand.grid(code = unique_codes, group = gender_levels, stringsAsFactors = FALSE) %>%
  mutate(group_type = "gender", group = factor(group, levels = gender_levels))
grid_housing <- expand.grid(code = unique_codes, group = housing_levels, stringsAsFactors = FALSE) %>%
  mutate(group_type = "housing_type_aggr", group = factor(group, levels = housing_levels))

grid_vars <- bind_rows(grid_age, grid_gender, grid_housing)

results_vars <- grid_vars %>%
  rowwise() %>%
  mutate(temp = list(compute_sentiment(
    quotations,
    code_name = code,
    group_var = group_type,
    group_value = group
  ))) %>%
  unnest_wider(temp) %>%
  ungroup() %>%
  mutate(shade = share_pos) %>%
  left_join(
    quotations %>% distinct(code, codegroup),
    by = "code"
  ) %>%
  arrange(codegroup, code) %>%
  mutate(
    code = factor(code, levels = unique(code)),
    codegroup = factor(codegroup)
  )


# Plot heatmap for the three variables
ggplot(results_vars, aes(x = group, y = code, fill = shade)) +
  geom_tile(color = "white") +
  geom_text(aes(label = N), size = 3, color = "#444") +
  facet_grid(
    rows = vars(codegroup),
    cols = vars(group_type),
    scales = "free",
    space = "free"
  ) +
  scale_fill_gradient2(
    name = "Share positive",
    limits = c(0, 1),
    midpoint = 0.5,
    low = "#C38BA4",
    mid = "white",
    high = "#00AC57",
    na.value = "grey90"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    strip.text   = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.spacing = unit(0.4, "lines")
  )

#============================================
#3 Analysing based on citizen cateogries

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
  n_documents <- nrow(df_summary)
  
  return(list(
    share_positive = share_positive,
    n_documents = n_documents
  ))
}


plot_code_sentiment_across_categories <- function(data, code_name, main_title) {
  
  sentiment_summary <- lapply(categories, function(cat) {
    df <- data %>% filter(code == code_name, category == cat)
    results <- compute_code_sentiment(data, code_name, cat)
    return(data.frame(category = cat, share_pos = results$share_positive, n = results$n_documents))
  }) %>%
    bind_rows()
  
  bp <- barplot(
    sentiment_summary$share_pos,
    names.arg = toupper(sentiment_summary$category),
    col = "#00AC57",
    border = "white",
    ylim = c(0, 1),
    main = toupper(paste("POZITÍV ÉRZELEM ARÁNYA\n", main_title)),
    ylab = toupper("Pozitív arány"),
    xlab = toupper("")
  )
  
  # Allow drawing outside plot area
  par(xpd = TRUE)
  
  # Add N labels below x-axis
  text(
    x = bp,
    y = -0.18,
    labels = toupper(paste0("N=", sentiment_summary$n)),
    cex = 1,
    col = "black"
  )
  
  # Reset clipping
  par(xpd = FALSE)
}

# Example usage
compute_code_sentiment(quotations, "Biztonságérzet", "fiatal_bérlő")
plot_code_sentiment_across_categories(quotations, "Közbiztonság és közterületi viselkedés", "Közbiztonság és közterületi viselkedés\n(Utca változásai)")

plot_code_sentiment_across_categories(quotations, "Általános jövőkép", "Általános jövőkép (Jövőkép az utcáról)")
plot_code_sentiment_across_categories(quotations, "Társadalmi sokszínűség (etnikai / osztálybeli / más)", "Társadalmi sokszínűség \n(Utcával kapcsolatos attitűdök)"
)
plot_code_sentiment_across_categories(quotations, "Biztonságérzet", "Biztonságérzet \n(Utcával kapcsolatos attitűdök)"
)
plot_code_sentiment_across_categories(quotations, "Tisztaság / rendezettség", "Tisztaság / rendezettség \n(Utcával kapcsolatos attitűdök)"
)


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
  print(nrow(df_summary))
  share_positive <- mean(df_summary$share_pos, na.rm = TRUE)
  n_documents <- nrow(df_summary)
  
  return(list(
    share_positive = share_positive,
    n_documents = n_documents
  ))
}

plot_code_sentiment_across_genders <- function(data, code_name, main_title) {
  
  genders <- unique(data$gender)
  
  sentiment_summary <- lapply(genders, function(g) {
    df <- data %>% filter(code == code_name, gender == g, codegroup == "Pozitív / Negatív")
    results <- compute_code_sentiment_gender(data, code_name, g)
    n_rows <- df %>% nrow()
    return(data.frame(gender = g, share_pos = results$share_positive, n = results$n_documents))
  }) %>% bind_rows()
  
  bp <- barplot(
    sentiment_summary$share_pos,
    names.arg = toupper(sentiment_summary$gender),
    col = "#C38BA4",
    border = "white",
    ylim = c(0, 1),
    main = toupper(paste("POZITÍV ÉRZELEM ARÁNYA\n", main_title)),
    ylab = toupper("Pozitív arány"),
    xlab = toupper("")
  )
  
  # Allow drawing outside plot area
  par(xpd = TRUE)
  
  # Add N labels below x-axis
  text(
    x = bp,
    y = -0.18,
    labels = toupper(paste0("N=", sentiment_summary$n)),
    cex = 1,
    col = "black"
  )
  
  # Reset clipping
  par(xpd = FALSE)
}


# Example usage
compute_code_sentiment_gender(quotations, "Általános jövőkép", "Férfi")
plot_code_sentiment_across_genders(quotations, "Általános jövőkép", "Általános jövőkép (Jövőkép az utcáról)")

plot_code_sentiment_across_genders(quotations, "Közbiztonság és közterületi viselkedés", "Közbiztonság és közterületi viselkedés\n(Utca változásai)")
plot_code_sentiment_across_genders(quotations, "Társadalmi sokszínűség (etnikai / osztálybeli / más)", "Társadalmi sokszínűség \n(Utcával kapcsolatos attitűdök)"
)
plot_code_sentiment_across_genders(quotations, "Biztonságérzet", "Biztonságérzet \n(Utcával kapcsolatos attitűdök)"
)
plot_code_sentiment_across_genders(quotations, "Tisztaság / rendezettség", "Tisztaság / rendezettség \n(Utcával kapcsolatos attitűdök)"
)


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
  
  locations <- levels(data$elhelyezkedés)
  
  sentiment_summary <- lapply(locations, function(loc) {
    df <- data %>% filter(code == code_name, elhelyezkedés == loc)
    share_pos <- compute_code_sentiment_location(data, code_name, loc)
    n_rows <- nrow(df)
    return(data.frame(elhelyezkedés = loc, share_pos = share_pos, n = n_rows))
  }) %>% bind_rows()
  
  old_par <- par(mar = c(14, 4, 4, 2))
  
  bar_centers <- barplot(
    sentiment_summary$share_pos,
    names.arg = FALSE,
    col = "#3A439A",
    border = "white",
    ylim = c(0, 1),
    main = toupper(paste("POZITÍV ÉRZELEM ARÁNYA\n", main_title)),
    ylab = toupper("Pozitív arány"),
    xlab = ""
  )
  
  text(
    x = bar_centers,
    y = par("usr")[3] - 0.02,
    labels = toupper(sentiment_summary$elhelyezkedés),
    srt = 45,
    xpd = TRUE,
    adj = 1,
    cex = 0.9
  )
  
  text(
    x = bar_centers,
    y = sentiment_summary$share_pos,
    labels = toupper(paste0("N=", sentiment_summary$n)),
    pos = 1,
    cex = 1,
    col = "black"
  )
  
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




#================================
#5. Codegroup positive distribution by CATEGORY

compute_codegroup_sentiment <- function(data, my_codegroup, my_category) {
  
  # data <- quotations
  # my_codegroup <- "Utca változásai"
  # my_category <- "fiatal_bérlő"
  
  # Step 1: Filter data for the codegroup and category
  df <- data %>%
    filter(codegroup == my_codegroup,
           category == my_category)
  
  if(nrow(df) == 0) return(list(share_positive = NA, n_documents = 0))
  
  # Step 2: Filter Positív / Negatív codes
  df_pos_neg <- data %>%
    filter(codegroup == "Pozitív / Negatív") %>%
    select(quotation, code)  # Keep only relevant columns
  
  # Step 3: Join with Pos/Neg via quotation
  df <- df %>%
    left_join(df_pos_neg, by = "quotation", suffix = c("", "_posneg"), relationship = "many-to-many")
  
  # Step 4: Summarize per document
  df_summary <- df %>%
    filter(!is.na(code)) %>%  # keep only rows that have Pos/Neg codes
    group_by(document) %>%
    summarise(
      total_posneg = sum(code_posneg == "Pozitív" | code_posneg == "Negatív", na.rm = T),
      nr_pos = sum(code_posneg == "Pozitív", na.rm = T),
      share_pos = nr_pos / total_posneg
    )
  
  share_positive <- mean(df_summary$share_pos, na.rm = TRUE)
  n_documents <- nrow(df_summary)
  
  return(list(
    share_positive = share_positive,
    n_documents = n_documents
  ))
}


plot_codegroup_sentiment_across_categories <- function(data, codegroup_name, main_title) {
  
  sentiment_summary <- lapply(categories, function(cat) {
    results <- compute_codegroup_sentiment(data, codegroup_name, cat)
    data.frame(category = cat, share_pos = results$share_positive, n = results$n_documents)
  }) %>%
    bind_rows()
  
  bp <- barplot(
    sentiment_summary$share_pos,
    names.arg = toupper(sentiment_summary$category),
    col = "#00AC57",
    border = "white",
    ylim = c(0, 1),
    main = toupper(paste("POZITÍV ÉRZELEM ARÁNYA\n", main_title)),
    ylab = toupper("Pozitív arány"),
    xlab = toupper("")
  )
  
  # Allow drawing outside plot area
  par(xpd = TRUE)
  
  # Add N labels below x-axis
  text(
    x = bp,
    y = -0.18,
    labels = toupper(paste0("N=", sentiment_summary$n)),
    cex = 1,
    col = "black"
  )
  
  # Reset clipping
  par(xpd = FALSE)
}

par(mar = c(4, 4, 4, 2))
# Example usage
compute_codegroup_sentiment(quotations, "Utca változásai", "fiatal_bérlő")
plot_codegroup_sentiment_across_categories(quotations, "Utca változásai", "Utca változásai")
plot_codegroup_sentiment_across_categories(quotations, "Utcai fejlesztések értékelése", "Utcai fejlesztések értékelése")


#================================================
# Codegroup positive distribution by GENDER

compute_codegroup_sentiment_gender <- function(data, my_codegroup, my_gender) {
  
  # Step 1: Filter data for the codegroup and gender
  df <- data %>%
    filter(codegroup == my_codegroup,
           gender == my_gender)
  
  if(nrow(df) == 0) return(list(share_positive = NA, n_documents = 0))
  
  # Step 2: Filter Positív / Negatív codes
  df_pos_neg <- data %>%
    filter(codegroup == "Pozitív / Negatív") %>%
    select(quotation, code)  # Keep only relevant columns
  
  # Step 3: Join with Pos/Neg via quotation, allow many-to-many
  df <- df %>%
    left_join(df_pos_neg, by = "quotation", suffix = c("", "_posneg"), relationship = "many-to-many")
  
  # Step 4: Summarize per document
  df_summary <- df %>%
    filter(!is.na(code_posneg)) %>%  # keep only rows that have Pos/Neg codes
    group_by(document) %>%
    summarise(
      total_posneg = sum(code_posneg %in% c("Pozitív", "Negatív"), na.rm = TRUE),
      nr_pos = sum(code_posneg == "Pozitív", na.rm = TRUE),
      share_pos = nr_pos / total_posneg,
      .groups = "drop"
    )
  
  share_positive <- mean(df_summary$share_pos, na.rm = TRUE)
  n_documents <- nrow(df_summary)
  
  return(list(
    share_positive = share_positive,
    n_documents = n_documents
  ))
}


plot_codegroup_sentiment_across_genders <- function(data, codegroup_name, main_title) {
  
  genders <- unique(data$gender)
  
  sentiment_summary <- lapply(genders, function(g) {
    results <- compute_codegroup_sentiment_gender(data, codegroup_name, g)
    data.frame(gender = g, share_pos = results$share_positive, n = results$n_documents)
  }) %>%
    bind_rows()
  
  bp <- barplot(
    sentiment_summary$share_pos,
    names.arg = toupper(sentiment_summary$gender),
    col = "#C38BA4",
    border = "white",
    ylim = c(0, 1),
    main = toupper(paste("POZITÍV ÉRZELEM ARÁNYA\n", main_title)),
    ylab = toupper("Pozitív arány"),
    xlab = toupper("")
  )
  
  # Allow drawing outside plot area
  par(xpd = TRUE)
  
  # Add N labels below x-axis
  text(
    x = bp,
    y = -0.18,
    labels = toupper(paste0("N=", sentiment_summary$n)),
    cex = 1,
    col = "black"
  )
  
  # Reset clipping
  par(xpd = FALSE)
}

par(mar = c(4, 4, 4, 2))

# Example usage
compute_codegroup_sentiment_gender(quotations, "Utca változásai", "Nő")
plot_codegroup_sentiment_across_genders(quotations, "Utca változásai", "Utca változásai")
plot_codegroup_sentiment_across_genders(quotations, "Utcai fejlesztések értékelése", "Utcai fejlesztések értékelése")




#====================================
# 7. Codegroup distribution per category
plot_codegroup <- function(df, codegroup_name, group_var = "category") {
  
  # for debug
  #df <- quotations
  #codegroup_name <- "Utcahasználat (jelenleg)"
  #group_var <- "category"
  
  unique_docs <- df[!duplicated(df$document), ]
  
  # Filter for the specified codegroup
  df_filtered <- df[df$codegroup == codegroup_name, ]
  
  # Determine grouping
  if (!is.na(group_var) && group_var %in% colnames(df)) {
    # Count each code once per document per group
    df_unique <- unique(df_filtered[, c("document", "code", group_var)])
    
    # Ensure group_var is a factor (if not already)
    df_unique[[group_var]] <- factor(df_unique[[group_var]], levels = levels(df[[group_var]]))
    
    # Create table with group levels preserved
    df_counts <- table(df_unique$code, df_unique[[group_var]])
    df_counts <- df_counts[, levels(df_unique[[group_var]]), drop = FALSE]  # enforce column order
    
    group_totals <- table(unique_docs[[group_var]])
    df_counts <- sweep(df_counts, 2, group_totals, FUN = "/") * 100

    # Define colors
    groups <- colnames(df_counts)
    custom_colors <- c("#00AC57",  "#3A439A", "#DF7201", "#C896AC",  "#80D5AB", "#C896AC")
    colors <- setNames(custom_colors[1:length(groups)], groups)
    
    # Plot
    par(mar = c(14, 5, 4, 2))
    bp <- barplot(t(df_counts),
                  beside = TRUE,
                  col = colors,
                  xlab = "",
                  ylab = toupper("előfordulás aránya (%)"),
                  names.arg = rep("", nrow(df_counts)),
                  main = toupper(paste("Kódok előfordulása", 
                                       ifelse(group_var == "gender", "nemekre bontva", "kategóriánként"),
                                       "\n", codegroup_name)),
                  ylim = c(0, 100))
    
    # Rotate x-axis labels
    text(x = colMeans(bp), 
         y = par("usr")[3] - 0.05 * max(df_counts), 
         labels = toupper(sub("-.*", "", rownames(df_counts))),
         srt = 45, adj = 1, xpd = TRUE)
    
    # Add legend
    legend("topleft", legend = toupper(groups), fill = colors, xpd = TRUE)
    
  } else {
    # No grouping variable, just counts per code
    df_unique <- unique(df_filtered[, c("document", "code")])
    df_counts <- table(df_unique$code)
    
    df_counts <- df_counts / nrow(unique_docs) * 100
    
    # Plot
    par(mar = c(14, 5, 4, 2))
    bp <- barplot(df_counts,
                  col = "#00AC57",
                  xlab = "",
                  ylab = toupper("előfordulás aránya (%)"),
                  names.arg = rep("", length(df_counts)),  # suppress default labels
                  main = toupper(paste("Kódok előfordulása\n", codegroup_name)),
                  ylim = c(0, 100))
    
    # Add x-axis labels rotated 45 degrees
    text(x = bp, 
         y = par("usr")[3] - 0.05 * max(df_counts), 
         labels = toupper(sub("-.*", "", names(df_counts))),
         srt = 45, adj = 1, xpd = TRUE)
  }
}

plot_codegroup_stacked <- function(df, codegroup_name, group_var = "category") {
  
  # Filter for the specified codegroup
  df_filtered <- df[df$codegroup == codegroup_name, ]
  
  # Keep one occurrence per document–code–group
  df_unique <- unique(df_filtered[, c("document", "code", group_var)])
  
  # Ensure grouping variable is factor with original levels
  df_unique[[group_var]] <- factor(
    df_unique[[group_var]],
    levels = levels(df[[group_var]])
  )
  
  # Count codes per group
  df_counts <- table(df_unique[[group_var]], df_unique$code)
  
  # Convert to percentages within each group (row-wise)
  df_perc <- prop.table(df_counts, margin = 1) * 100
  
  # Group sizes (n per category)
  group_n <- table(unique(df[, c("document", group_var)])[[group_var]])
  print(group_n)
  # Define colors for codes
  codes <- colnames(df_perc)
  custom_colors <- c(
    "#353535","#00AC57", "#DF7201", "#3A439A",
    "#80D5AB", "#9C3C3C", "#6A5ACD", "#B8860B"
  )
  colors <- setNames(custom_colors[1:length(codes)], codes)
  
  # Plot
  par(mar = c(16, 5, 4, 2), xpd = TRUE)
  
  bp <- barplot(
    t(df_perc),
    col = colors,
    border = NA,
    xlab = ,
    ylab = toupper("előfordulás aránya (%)"),
    names.arg = rep("", nrow(df_perc)),
    main = toupper(codegroup_name),
    ylim = c(0, 100)
  )
  
  # Add x-axis labels rotated 45 degrees
  text(x = bp, 
       y = -10, 
       labels = toupper(levels(df[[group_var]])),
       srt = 45, adj = 1, xpd = TRUE)
  
  # Add N below each column
  text(
    x = bp,
    y = -5,
    labels = paste0("N = ", group_n),
    xpd = TRUE,
    cex = 0.9
  )
  
  # Legend for codes
  legend(
    x = 0.8,
    y = -75,
    legend = rev(toupper(codes)),
    fill = rev(colors),
    cex = 0.9
  )
  mtext(
    toupper("Mikor költözött a lakásba?"),
    side = 1,
    line = 9
  )
}



plot_codegroup(quotations, "Ideköltözés okai és körülményei", group_var = NA)
plot_codegroup(quotations, "Ideköltözés okai és körülményei", group_var = "category")
plot_codegroup(quotations, "Ideköltözés okai és körülményei", group_var = "gender")
plot_codegroup_stacked(quotations, "Ideköltözés okai és körülményei", group_var = "f_years_in_apartment_grp")
plot_codegroup(quotations, "Utcahasználat (jelenleg)", group_var = NA)
plot_codegroup(quotations, "Utcahasználat (jelenleg)", group_var = "category")
plot_codegroup(quotations, "Utcahasználat (jelenleg)", group_var = "gender")
plot_codegroup(quotations, "Utcán mit változtatna", group_var = NA)
plot_codegroup(quotations, "Utcán mit változtatna", group_var = "category")
plot_codegroup(quotations, "Utcán mit változtatna", group_var = "gender")




#============================================
#8 Analysing based on citizen cateogries


compute_code_sentiment_developments <- function(data, my_code, my_category) {
  
  # Step 1: Filter initial df
  df <- data %>%
    filter(code == my_code)
  
  if(!is.na(my_category)){
    df <- df %>%
      filter(category == my_category)
  }
  
  # Step 2: Find matching rows with different code
  df_pos_neg <- data %>%
    filter(code == "Pozitív" | code == "Negatív" | code == "Nincs hatása") %>%
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
      nr_pos = sum(code_posneg == "Pozitív"),
      nr_neg = sum(code_posneg == "Negatív"),
      nr_nh = sum(code_posneg == "Nincs hatása")
    )
  print(df_summary)
  n_documents <- nrow(df_summary)
  
  return(list(
    summary = df_summary,
    n_documents = n_documents
  ))
}
plot_utcai_fejlesztesek_sentiment <- function(data) {
  
  # 1️⃣ Get all relevant codes (exclude "Nincs hatása" as before)
  codes <- data %>%
    filter(codegroup == "Utcai fejlesztések értékelése" &
             code != "Nincs hatása") %>%
    distinct(code) %>%
    pull(code)
  
  # 2️⃣ Compute document-level sentiment shares for each code
  sentiment_list <- lapply(codes, function(cd) {
    
    res <- compute_code_sentiment_developments(
      data = data,
      my_code = cd,
      my_category = NA
    )
    
    df <- res$summary
    
    # Skip codes with no matched documents
    if(nrow(df) == 0) return(NULL)
    
    # Document-level shares
    doc_shares <- df %>%
      mutate(
        doc_total = nr_pos + nr_neg + nr_nh,
        doc_share_pos = nr_pos / doc_total,
        doc_share_neg = nr_neg / doc_total,
        doc_share_nh  = nr_nh  / doc_total
      )
    
    tibble(
      code = cd,
      share_pos = mean(doc_shares$doc_share_pos, na.rm = TRUE),
      share_neg = mean(doc_shares$doc_share_neg, na.rm = TRUE),
      share_nh  = mean(doc_shares$doc_share_nh,  na.rm = TRUE),
      n_docs = nrow(doc_shares)
    )
  })
  
  sentiment_df <- bind_rows(sentiment_list)
  
  # 3️⃣ Prepare bar matrix
  bar_matrix <- t(as.matrix(
    sentiment_df %>%
      select(share_pos, share_neg, share_nh)
  ))
  
  colnames(bar_matrix) <- toupper(sentiment_df$code)
  rownames(bar_matrix) <- c("POZITÍV", "NEGATÍV", "NINCS HATÁSA")
  
  # 4️⃣ Set plotting parameters (extra space below)
  op <- par(mar = c(10, 5, 4, 2))
  
  # 5️⃣ Plot stacked bars
  bp <- barplot(
    bar_matrix,
    col = c("#00AC57", "#DF7201", "#9DA7A1"),
    border = "white",
    ylim = c(0, 1),
    main = toupper("UTCai FEJLESZTÉSEK ÉRTÉKELÉSE"),
    ylab = toupper("Arány"),
    names.arg = rep("", length(codes)),
    xlab = "",
    las = 2
  )
  
  # 6️⃣ Rotate code names 45°
  text(
    x = bp,
    y = -0.02,
    labels = colnames(bar_matrix),
    srt = 45,
    adj = 1,
    xpd = TRUE,
    cex = 0.9
  )
  
  # 7️⃣ Add N labels below
  text(
    x = bp,
    y = 1.05,
    labels = paste0("N=", sentiment_df$n_docs),
    cex = 0.9,
    xpd = TRUE
  )
  
  # 8️⃣ Add legend
  legend(
    x = 4, 
    y = -0.4,
    legend = rownames(bar_matrix),
    fill = c("#00AC57", "#DF7201", "#9DA7A1"),
    bty = "n",
    xpd = TRUE
  )
  
  # 9️⃣ Reset plotting parameters
  par(op)
  
  # 10️⃣ Return document-weighted sentiment shares invisibly
  invisible(sentiment_df)
}



plot_utcai_fejlesztesek_sentiment(quotations)
