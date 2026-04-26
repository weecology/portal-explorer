# List of required packages
required_packages <- c(
  "dplyr",
  "forecast",
  "ggplot2",
  "gridExtra",
  "htmltools",
  "leaflet",
  "lubridate",
  "rgl",
  "sf",
  "shiny",
  "shinythemes",
  "shinyWidgets",
  "stats",
  "stringr",
  "yaml",
  "remotes"
)

# Function to install missing packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    message(paste("Installing package:", package))
    install.packages(package, dependencies = TRUE)
  } else {
    message(paste("Package already installed:", package))
  }
}

# Check for system dependencies on macOS
if(Sys.info()["sysname"] == "Darwin") {
  message("\n--- IMPORTANT NOTE FOR MACOS USERS ---")
  message("The 'rgl' package requires XQuartz (X11) to be installed.")
  message("If you encounter errors with rgl, please install XQuartz:")
  message("- Option 1: Visit https://www.xquartz.org/ and download the installer")
  message("- Option 2: Using Homebrew: brew install --cask xquartz")
  message("After installing XQuartz, you may need to restart your computer.")
  message("---------------------------------------\n")
} else if(Sys.info()["sysname"] == "Linux") {
  message("\n--- IMPORTANT NOTE FOR LINUX USERS ---")
  message("The 'rgl' package requires OpenGL and X11 development libraries.")
  message("If you encounter errors with rgl, please install the required dependencies:")
  message("- For Ubuntu/Debian: sudo apt-get install libgl1-mesa-dev libglu1-mesa-dev xorg-dev")
  message("- For Fedora/RHEL/CentOS: sudo dnf install mesa-libGL-devel mesa-libGLU-devel libX11-devel libXt-devel")
  message("- For Arch Linux: sudo pacman -S mesa glu libxcb")
  message("After installing, you may need to reinstall the rgl package:")
  message("install.packages('rgl', repos='http://cran.rstudio.com/')")
  message("---------------------------------------\n")
}

# Install CRAN packages
message("Checking and installing required CRAN packages...")
for (package in required_packages) {
  install_if_missing(package)
}

# Install GitHub packages
message("\nChecking and installing GitHub packages...")
if (!require("leaflet.mapboxgl")) {
  message("Installing leaflet.mapboxgl from GitHub...")
  remotes::install_github("rstudio/leaflet.mapboxgl")
} else {
  message("leaflet.mapboxgl already installed")
}
remotes::install_github("weecology/portalcasting")
remotes::install_github("weecology/portalr")


message("\nAll required packages have been installed!")
# Print package versions for reference
message("\nInstalled package versions:")
for (package in c(required_packages, "leaflet.mapboxgl")) {
  if (require(package, character.only = TRUE)) {
    version <- packageVersion(package)
    message(sprintf("%s: %s", package, version))
  }
}
