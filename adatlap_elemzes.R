# --- Libraries ---
library(dplyr)
library(ggplot2)
library(corrplot)
library(DescTools)
library(polycor)
library(scales)

# --- 1. Load Data ---
read_data <- function(filepath, column_names) {
  df <- read.csv(filepath, header = TRUE, stringsAsFactors = FALSE)
  colnames(df) <- column_names
  return(df)
}

interview_data <- read_data(
  "Adatlapok - kvótakövetés - Adatlap.csv",
  c(
    "id", "interviewer1", "interviewer2", "birth_year", "gender", "education", "housing_type",
    "household_size", "num_children", "apartment_size", "apartment_location", "occupation",
    "years_in_district", "years_in_apartment", "prev_residence", "prev_district", "prev_aggr",
    "housing_expenses_total", "utilities_cost", "rent_cost", "mortgage_cost", "total_income",
    "expense_coverage", "address", "floor", "contact", "apartment_quality", "extra_notes",
    "interview_length", "other_comments"
  )
)

# --- 2. Clean Data ---
clean_currency <- function(x) {
  x_clean <- gsub("\\s|Ft|\\,", "", x)
  as.integer(ifelse(x_clean == "" | is.na(x_clean), NA, x_clean))
}

currency_cols <- c("housing_expenses_total", "utilities_cost", "rent_cost", "mortgage_cost", "total_income")
interview_data[currency_cols] <- lapply(interview_data[currency_cols], clean_currency)
interview_data[currency_cols] <- lapply(interview_data[currency_cols], clean_currency)


# Load tarsashazak separately if needed
tarsashazak <- read.csv("tarsashaztipologia.csv", header = TRUE, stringsAsFactors = FALSE)
interview_data <- interview_data %>%
  left_join(
    tarsashazak %>% select(cím, elhelyezkedés),
    by = c("address" = "cím")
  )

# Clean `housing_type`: remove any parenthetical explanations (e.g., "(...)")
# and trim surrounding whitespace so factor levels are concise.
interview_data <- interview_data %>%
  mutate(housing_type = trimws(gsub("[[:space:]]*\\([^)]*\\)", "", housing_type)))

# Convert factors
interview_data <- interview_data %>%
  mutate(
    gender = factor(gender),
    education = factor(education, levels = c(
      "kevesebb, mint 8 osztály",   
      "befejezett 8 osztály",         
      "szakmunkásképző",
      "szakközépiskolai érettségi",
      "gimnáziumi érettségi",
      "főiskolai v. egyetemi diploma")),
    housing_type = factor(housing_type),
    apartment_location = factor(apartment_location),
    expense_coverage = factor(expense_coverage, levels = c(
      "Nagyon nehezen", "Nehezen", "Viszonylag nehezen",
      "Viszonylag könnyen", "Könnyen", "Nagyon könnyen", "Nem tudja")),
    elhelyezkedés = factor(elhelyezkedés, levels = c(
      "József krt. - Kiss József utca", 
      "Kiss József utca - Nagy Fuvaros utca", 
      "Nagy Fuvaros utca - Alföldi utca",
      "Alföldi utca - Teleki tér")),
    address = factor(address)
  ) %>%
  mutate(
    housing_overburden = housing_expenses_total / total_income,
    lakhatasi_szegeny = housing_overburden > 0.3,
    income_per_person = total_income / household_size,    
    education_aggr = case_when(
      education %in% c(
        "kevesebb, mint 8 osztály",
        "befejezett 8 osztály",
        "szakmunkásképző"
      ) ~ "érettségi alatt",
      
      education %in% c(
        "szakközépiskolai érettségi",
        "gimnáziumi érettségi"
      ) ~ "érettségi",
      
      education == "főiskolai v. egyetemi diploma" ~ "diploma",
      
      TRUE ~ NA_character_
    ),
    
    education_aggr = factor(education_aggr, levels = c(
      "érettségi alatt", "érettségi", "diploma"
    )),
    age_group = dplyr::case_when(
          !is.na(birth_year) & birth_year <= 1960 ~ "65 év feletti",
          !is.na(birth_year) & birth_year <= 1990 ~ "35 és 65 év közötti",
          !is.na(birth_year) & birth_year > 1990  ~ "35 év alatti",
          TRUE ~ NA_character_
      ),
      age_group = factor(age_group, levels = c("65 év feletti", "35 és 65 év közötti", "35 év alatti")),
    housing_type_aggr = case_when(
      housing_type == "saját tulajdon" ~ "tulajdonos",
      housing_type != "saját tulajdon" ~ "nem tulajdonos"
    )
  )

interview_data$housing_expenses_PP <- interview_data$housing_expenses_total / interview_data$household_size

interview_data <- interview_data %>%
  mutate(
    category = case_when(
      birth_year < 1965 ~ "idős",
      housing_type != "piaci bérlet" ~ "fiatal_tulaj",
      TRUE ~ "fiatal_bérlő"
    ),
    category = factor(category, levels = c("idős", "fiatal_tulaj", "fiatal_bérlő"))
  )

# Quick check
table(interview_data$category)

interview_data <- interview_data %>%
  mutate(
    category_A = case_when(
      birth_year < 1965 ~ "idős",
      housing_expenses_total > 100000 ~ "fiatal_prekár",
      TRUE ~ "fiatal_stabil"
    ),
    category_A = factor(category_A, levels = c("idős", "fiatal_stabil", "fiatal_prekár"))
  )

table(interview_data$category_A)
interview_data <- interview_data %>%
  mutate(
    category_B = case_when(
      birth_year < 1965 ~ "idős",
      housing_expenses_PP > 100000 ~ "fiatal_prekár",
      TRUE ~ "fiatal_stabil"
    ),
    category_B = factor(category_B, levels = c("idős", "fiatal_stabil", "fiatal_prekár"))
  )

table(interview_data$category_B)
interview_data <- interview_data %>%
  mutate(
    category_C = case_when(
      birth_year < 1965 ~ "idős",
      housing_overburden > 0.3 ~ "fiatal_prekár",
      TRUE ~ "fiatal_stabil"
    ),
    category_C = factor(category_C, levels = c("idős", "fiatal_stabil", "fiatal_prekár"))
  )

table(interview_data$category_C)



interview_data$f_years_in_apartment_grp <- factor(
  dplyr::case_when(
    interview_data$years_in_apartment <= 1990 ~ "1990 előtt",
    interview_data$years_in_apartment > 1990 &
      interview_data$years_in_apartment <= 2016 ~ "1990 és 2016 között",
    interview_data$years_in_apartment > 2016 &
      interview_data$years_in_apartment <= 2023 ~ "2016 és 2023 között",
    interview_data$years_in_apartment > 2023 ~ "utóbbi két évben",
    TRUE ~ NA_character_
  ),
  levels = c(
    "1990 előtt",
    "1990 és 2016 között",
    "2016 és 2023 között",
    "utóbbi két évben"
  )
)

# --- Save the cleaned and processed interview_data ---
save(interview_data, file = "interview_data.RData")


# --- 3. Numeric Analysis ---
numeric_vars <- interview_data %>%
  select(birth_year, household_size, num_children, apartment_size,
         housing_expenses_total, mortgage_cost, total_income, floor)

# Summary and correlation
summary(numeric_vars)
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", addCoef.col = "black", number.cex = 0.7)

# --- 4. Scatterplot Matrix ---
plot_scatter_matrix <- function(df, numeric_cols, color_var) {
  cols <- ifelse(df[[color_var]], "red", "blue")
  panel_func <- function(x, y, ...) points(x, y, col = cols, pch = 16)
  op <- par(xpd = TRUE, mar = c(4, 4, 6, 2))
  
  pairs(df[numeric_cols],
        main = "Scatterplot Matrix of Numeric Demographics",
        pch = 16,
        col = cols,
        lower.panel = panel_func,
        upper.panel = panel_func)
  
  legend(
    "bottom",
    inset = -0.1,
    horiz = TRUE,
    legend = c("Lakhatási szegény", "Nem lakhatási szegény"),
    col = c("red", "blue"),
    pch = 16,
    bty = "n",
    cex = 1.1
  )
  par(op)
}

plot_scatter_matrix(interview_data, names(numeric_vars), "lakhatasi_szegeny")

# --- 5. ggplot Visualizations ---
plot_numeric_vs <- function(x, y, color, title, palette = NULL) {
  p <- ggplot(interview_data, aes_string(x = x, y = y, color = color)) +
    geom_point(alpha = 0.7, size = 4) +
    theme_minimal() +
    labs(title = title, x = x, y = y)
  
  if (!is.null(palette)) {
    p <- p + scale_color_manual(values = palette)
  }
  print(p)
}

plot_numeric_vs("birth_year", "housing_expenses_total", "housing_type",
                "Total Housing Expenses of Household vs Birth Year by Housing Type",
                c("red", "blue", "green", "orange", "purple", "brown"))

plot_numeric_vs("birth_year", "housing_overburden", "housing_type",
                "Housing Overburden Rate vs Birth Year by Housing Type",
                c("red", "blue", "green", "orange", "purple", "brown"))


plot_numeric_vs("birth_year", "housing_expenses_PP", "housing_type",
                "Housing Expenses per person vs Birth Year by Housing Type",
                c("red", "blue", "green", "orange", "purple", "brown"))


plot_numeric_vs("birth_year", "housing_expenses_total", "lakhatasi_szegeny",
                "Housing Expenses vs Birth Year by Housing Overburden",
                c("blue", "red"))

plot_numeric_vs("birth_year", "total_income", "lakhatasi_szegeny",
                "Total Income vs Birth Year by Housing Overburden",
                c("blue", "red"))

# --- 6. Stacked Bar Charts ---
plot_stacked_bar <- function(df, x, fill, title, y_label = "Proportion (%)", palette = NULL) {
  plot_data <- df %>%
    group_by(across(all_of(c(x, fill)))) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(across(all_of(x))) %>%
    mutate(prop = count / sum(count))
  
  ggplot(plot_data, aes_string(x = x, y = "prop", fill = fill)) +
    geom_bar(stat = "identity", position = "fill", color = "white") +
    scale_y_continuous(labels = percent_format()) +
    labs(title = title, x = x, y = y_label) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          panel.grid.minor = element_blank()) +
    {if (!is.null(palette)) scale_fill_manual(values = palette)} 
}

# Expense coverage by housing overburden
plot_stacked_bar(interview_data, "expense_coverage", "lakhatasi_szegeny",
                 "Lakhatási szegénység megoszlása a megélhetés nehézsége szerint",
                 palette = c("blue", "red"))

plot_stacked_bar(interview_data, "education_aggr", "housing_type", "Housing Type Distribution by Education Level")


# Education by expense coverage with color ramp
expense_levels <- levels(interview_data$expense_coverage)
colors <- ifelse(expense_levels == "Nem tudja", "grey",
                 colorRampPalette(c("red", "green"))(length(expense_levels) - 1))
names(colors) <- expense_levels

plot_stacked_bar(interview_data, "education_aggr", "expense_coverage",
                 "Expense Coverage by Education Level",
                 palette = colors)


# elhelyezkedés elemzés

# --- 2. Stacked barplots by elhelyezkedés ---
# Education by elhelyezkedés
plot_stacked_bar(interview_data, "elhelyezkedés", "education_aggr", "Education Level by elhelyezkedés")

# Expense coverage by elhelyezkedés
expense_levels <- levels(interview_data$expense_coverage)
colors_exp <- ifelse(expense_levels == "Nem tudja", "grey",
                     colorRampPalette(c("red", "green"))(length(expense_levels) - 1))
names(colors_exp) <- expense_levels

plot_stacked_bar(interview_data, "elhelyezkedés", "expense_coverage",
                 "Expense Coverage by elhelyezkedés",
                 palette = colors_exp)



# Increase bottom margin
par(mar = c(10, 5, 4, 1))

plot(
  (interview_data$income_per_person / 1000) ~ interview_data$expense_coverage,
  xlab = "",
  ylab = "ezer forint per fő",
  xaxt = "n",
  main = "Egy főre jutó bevétel megélhetés nehézsége szerint"
)
axis(1, at = 1:length(levels(interview_data$expense_coverage)), labels = levels(interview_data$expense_coverage), las = 2)

counts <- table(interview_data$expense_coverage)
y_values <- tapply(interview_data$income_per_person / 1000, interview_data$expense_coverage, mean, na.rm = T)
text(
  x = 1:length(levels(interview_data$expense_coverage)) + 0.2,
  y = 700,
  labels = counts,
  col = "red",
  pos = 3  # above the points
)
text(x = 0.8, y = 700, "n =", col = "red", pos = 3)




# Housing type by elhelyezkedés
plot_stacked_bar(interview_data, "elhelyezkedés", "housing_type", "Housing Type by elhelyezkedés")

# Age group by elhelyezkedés
age_colors <- c("young" = "#F8766D", "middleaged" = "#7CAE00", "old" = "#00BFC4")

plot_stacked_bar(interview_data, "elhelyezkedés", "age_group", "Age Groups by elhelyezkedés", palette = age_colors)

# Years in VIII district and in aparmtment
plot(
  NA, NA,
  xlim = range(interview_data$years_in_district, na.rm = TRUE),
  ylim = range(interview_data$birth_year, na.rm = TRUE),
  xlab = "Év",
  ylab = "Születési év",
  main = "Lakhatási történelem"
)
abline(a = 0, b = 1, lty = 2, col = "darkgrey")
text(1980, 1980, col="darkgrey", "Születése óta ott él →", pos = 2)
points(
  interview_data$years_in_district,
  interview_data$birth_year,
  pch = 16,
  col = "orange"
)
points(
  interview_data$years_in_apartment,
  interview_data$birth_year,
  pch = 16,
  col = "darkgreen"
)
legend(
  "topleft",
  legend = c("Mióta él a VIII. kerületben?", "Mióta él ebben a lakásban?"),
  col = c("orange", "darkgreen"),
  pch = 16,
  bty = "n"
)


# Years in VIII district and in aparmtment UPDATED
# Base plot
plot(
  NA, NA,
  xlim = range(interview_data$years_in_district, interview_data$years_in_apartment, na.rm = TRUE),
  ylim = range(interview_data$birth_year, na.rm = TRUE),
  xlab = "Év",
  ylab = "Születési év",
  main = "Lakhatási történelem"
)

abline(a = 0, b = 1, lty = 2, col = "darkgrey")
text(1980, 1980, col = "darkgrey", "Születése óta ott él →", pos = 2)

# Loop through interview_data to plot points/arrows
for(i in seq_len(nrow(interview_data))) {
  x1 <- interview_data$years_in_district[i]
  x2 <- interview_data$years_in_apartment[i]
  y <- interview_data$birth_year[i]
  prev <- interview_data$prev_district[i]
  
  if(is.na(x1) | is.na(x2) | is.na(y)) next  # skip missing values
  
  # Special case: birth year = years_in_apartment
  if(y == x2) {
    points(x2, y, pch = 16, col = "darkred", cex = 1.2)
  } else if(x1 == x2) {
    col_val <- if(!is.na(prev)) "blue" else "orange"
    points(x1, y, pch = 16, col = col_val, cex = 1.2)
  } else {
    # Arrow case
    col_val <- if(!is.na(prev)) "blue" else "orange"

    segments(x0 = x1, y0 = y, x1 = x2, y1 = y, col = alpha(col_val, 0.4), lwd = 1.5)
    
    points(x1, y, pch = 18, col = alpha(col_val, 0.2), cex = 1.2)
    points(x2, y, pch = 16, col = "#33aa33", cex = 1.2)
    
  }
}

# Legend
legend(
  "topleft",
  legend = c(
    "A kerületbe költözött vidékről",
    "A kerületbe költözött másik kerületből",
    "",
    "Az utcába költözött vidékről",
    "Az utcába költözött másik kerületből",
    "Az utcába költözött a kerületből",
    "Az utcába született"
  ),
  col = c("orange", "blue", "",  "orange", "blue", "#33aa33", "darkred"),
  pch = c(18, 18, NA, 16, 16, 16, 16),
  bty = "n",
  pt.cex = 1.2
)



