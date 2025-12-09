library(readxl)
library(dplyr)
library(tidyr)

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
