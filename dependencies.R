# ==============================================================================
# FILE: dependencies.R
# PROJECT: AquaFever - Rainfall-Driven Malaria Monitoring System
# STUDENT: Sandra Adhiambo (24/05568)
# PURPOSE: Install all required packages as per SRS Section 2.3
# ==============================================================================

# 1. Check R Version (SRS Section 2.3: R ≥ 4.2.0)
if (getRversion() < "4.2.0") {
  stop("❌ ERROR: Your R version is outdated. Please install R version 4.2.0 or higher.")
} else {
  cat(paste("✅ R Version Check Passed:", getRversion(), "\n\n"))
}

# 2. List Required Packages (SRS Section 2.3 & Proposal Section 6.2)
packages <- c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "ggplot2",
  "plotly",
  "caret",
  "randomForest",
  "forecast",
  "sf",
  "leaflet",
  "leaflet.extras",
  "rmarkdown",
  "knitr",
  "zoo",
  "shinyjs",
  "DT",
  "readxl"
)

# 3. Install Packages
cat("📦 Installing AquaFever dependencies (this may take a few minutes)...\n\n")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste("   Installing", pkg, "...\n"))
    tryCatch({
      install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org")
    }, error = function(e) {
      cat(paste("   ⚠️ Warning: Could not install", pkg, "\n"))
    })
  } else {
    cat(paste("   ✅", pkg, "already installed\n"))
  }
}

# 4. Verify Installation
cat("\n🔍 Verifying installation...\n\n")

# Create installed vector
installed <- rep(FALSE, length(packages))
names(installed) <- packages

for (pkg in packages) {
  installed[pkg] <- requireNamespace(pkg, quietly = TRUE)
}

# Check results
if (all(installed)) {
  cat("✅ SUCCESS: All dependencies installed successfully!\n")
  cat("🚀 You are ready for Step 2: Data Preparation.\n")
} else {
  failed <- names(installed)[!installed]
  cat("❌ ERROR: Failed to install the following packages:\n")
  print(failed)
  cat("\n💡 TIP: Please check your internet connection and try again.\n")
  cat("   If problems persist, install packages manually using:\n")
  cat('   install.packages("package_name")\n')
}
