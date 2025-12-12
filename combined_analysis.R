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
  return (share_positive)
}


plot_code_sentiment_across_categories <- function(data, code_name, main_title) {
  
  categories <- unique(data$category)
  
  sentiment_summary <- lapply(categories, function(cat) {
    df <- data %>% filter(code == code_name, category == cat)
    share_pos <- compute_code_sentiment(data, code_name, cat)
    n_rows <- nrow(df)
    return(data.frame(category = cat, share_pos = share_pos, n = n_rows))
  }) %>%
    bind_rows()
  
  barplot(
    sentiment_summary$share_pos,
    names.arg = toupper(sentiment_summary$category),
    col = "#00AC57",
    border = "white",
    ylim = c(0, 1),
    main = toupper(paste("POZITÍV ÉRZELEM ARÁNYA\n", main_title)),
    ylab = toupper("Pozitív arány"),
    xlab = toupper("Kategória")
  )
  
  text(
    x = seq_along(sentiment_summary$share_pos),
    y = sentiment_summary$share_pos,
    labels = toupper(paste0("N=", sentiment_summary$n)),
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
  
  genders <- unique(data$gender)
  
  sentiment_summary <- lapply(genders, function(g) {
    df <- data %>% filter(code == code_name, gender == g)
    share_pos <- compute_code_sentiment_gender(data, code_name, g)
    n_rows <- nrow(df)
    return(data.frame(gender = g, share_pos = share_pos, n = n_rows))
  }) %>% bind_rows()
  
  barplot(
    sentiment_summary$share_pos,
    names.arg = toupper(sentiment_summary$gender),
    col = "#C38BA4",
    border = "white",
    ylim = c(0, 1),
    main = toupper(paste("POZITÍV ÉRZELEM ARÁNYA\n", main_title)),
    ylab = toupper("Pozitív arány"),
    xlab = toupper("Nem")
  )
  
  text(
    x = seq_along(sentiment_summary$share_pos),
    y = sentiment_summary$share_pos,
    labels = toupper(paste0("N=", sentiment_summary$n)),
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


#====================================
# 5. Codegroup distribution per category
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
    custom_colors <- c("#00AC57",  "#3A439A", "#DF7201", "#254335",  "#80D5AB", "#C896AC")
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
                  ylim = c(0, max(df_counts) * 1.2))
    
    # Rotate x-axis labels
    text(x = colMeans(bp), 
         y = par("usr")[3] - 0.05 * max(df_counts), 
         labels = toupper(sub("-.*", "", rownames(df_counts))),
         srt = 45, adj = 1, xpd = TRUE)
    
    # Add legend
    legend(16, 60, legend = toupper(groups), fill = colors, xpd = TRUE)
    
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
                  ylim = c(0, max(df_counts) * 1.2))
    
    # Add x-axis labels rotated 45 degrees
    text(x = bp, 
         y = par("usr")[3] - 0.05 * max(df_counts), 
         labels = toupper(sub("-.*", "", names(df_counts))),
         srt = 45, adj = 1, xpd = TRUE)
  }
}


plot_codegroup(quotations, "Ideköltözés okai és körülményei", group_var = NA)
plot_codegroup(quotations, "Ideköltözés okai és körülményei", group_var = "category")
plot_codegroup(quotations, "Ideköltözés okai és körülményei", group_var = "gender")
plot_codegroup(quotations, "Utcahasználat (jelenleg)", group_var = NA)
plot_codegroup(quotations, "Utcahasználat (jelenleg)", group_var = "category")
plot_codegroup(quotations, "Utcahasználat (jelenleg)", group_var = "gender")
plot_codegroup(quotations, "Utcán mit változtatna", group_var = NA)
plot_codegroup(quotations, "Utcán mit változtatna", group_var = "category")
plot_codegroup(quotations, "Utcán mit változtatna", group_var = "gender")



