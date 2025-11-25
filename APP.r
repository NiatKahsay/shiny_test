

# Install and load rsconnect if not already done
if (!require(rsconnect)) install.packages("rsconnect")
library(rsconnect)

# Generate manifest.json
writeManifest()
