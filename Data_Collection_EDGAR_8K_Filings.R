# Load the necessary library
install.packages("edgar")
library(edgar)

# Define the function to fetch filings
fetch_8K_filings <- function(cik_no, filing_year) {
  # Fetch the 8-K filings for the given CIK and filing year
  cat("Fetching data for CIK:", cik_no, " for year:", filing_year, "\n")
  filings <- get8KItems(cik.no = cik_no, filing.year = filing_year)
  # Check if data exists and return it
  if (!is.null(filings) && nrow(filings) > 0) {
    return(filings)
  } else {
    cat("No data found for CIK:", cik_no, " for year:", filing_year, "\n")
    return(NULL)
  }
}

# List of CIKs to process
cik_list <- c(
  "78003", "200406", "310158", "1551152", "1327811", "1652044", "1321655", "1543151", "1467623", "1650372",
  "1512673", "1794515", "1713683", "1640147", "1660134", "51143", "804328", "796343", "1633917", "1585521",
  "1441816", "320193", "789019", "1018724", "1326801", "1318605", "1045810", "1341439", "50863", "1373715",
  "1288776", "1439404", "1730168", "1309108", "1580808", "1065280", "1463172", "1601712", "1489393", "1567892",
  "1356104", "1418091", "1334814", "1383312", "1490281", "19617", "831001", "72971", "886982", "895421", "4962",
  "92380", "1364742", "1130310", "1555280")


# Filing years range from 2010 to 2024
filing_years <- 2010:2024

# Initialize an empty dataframe to store the combined data
combined_filings <- data.frame()

# Loop through each CIK and each filing year
for (cik in cik_list) {
  for (year in filing_years) {
    filings_data <- fetch_8K_filings(cik, year)
    # Append data if not NULL
    if (!is.null(filings_data)) {
      combined_filings <- rbind(combined_filings, filings_data)
    }
  }
}

# Check if combined_filings has data before writing to CSV
if (nrow(combined_filings) > 0) {
  # Write the combined dataframe to a CSV file
  write.csv(combined_filings, "final_combined_8K_filings_all.csv", row.names = FALSE)
  cat("Data successfully written to final_combined_8K_filings_all.csv\n")
} else {
  cat("No data available to write to CSV.\n")
}


# View the combined dataframe (optional)
if (nrow(combined_filings) > 0) {
  View(combined_filings)
} else {
  cat("No data to display.\n")
}