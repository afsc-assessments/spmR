# setup.R - Fisheries Projection Shiny App Setup
# Run this script first to install all required packages

cat("Setting up Fisheries Projection Shiny App...\n")
cat("Checking and installing required packages...\n\n")

# List of required packages
required_packages <- c(
  "shiny",           # Core Shiny framework
  "shinydashboard",  # Dashboard layout
  "DT",              # Interactive data tables
  "plotly",          # Interactive plots
  "dplyr",           # Data manipulation
  "readr",           # File reading utilities
  "stringr"          # String manipulation
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages) > 0) {
    cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, dependencies = TRUE)
    cat("Installation complete!\n\n")
  } else {
    cat("All required packages are already installed.\n\n")
  }
}

# Install missing packages
install_if_missing(required_packages)

# Verify all packages can be loaded
cat("Verifying package installation...\n")
success <- TRUE

for(pkg in required_packages) {
  if(requireNamespace(pkg, quietly = TRUE)) {
    cat("✓", pkg, "- OK\n")
  } else {
    cat("✗", pkg, "- FAILED\n")
    success <- FALSE
  }
}

cat("\n")

if(success) {
  cat("🎉 Setup complete! All packages are ready.\n")
  cat("You can now run the app with: shiny::runApp('app.R')\n")
} else {
  cat("❌ Setup encountered issues. Please try installing failed packages manually.\n")
  cat("Example: install.packages(c('package1', 'package2'))\n")
}

cat("\n--- Next Steps ---\n")
cat("1. Make sure you have your fisheries projection data file ready\n")
cat("2. Run: shiny::runApp('app.R')\n")
cat("3. Upload your data file through the app interface\n")
cat("4. Explore your projections with interactive plots!\n")
