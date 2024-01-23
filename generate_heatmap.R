
library(ggplot2)
library(reshape2)

# Define the range of beta and handicap severity
beta_seq <- seq(0, 0.1, length.out = 100)
handicap_seq <- seq(0, 200, length.out = 100)

# Create an empty matrix to store the results
results_matrix <- matrix(nrow = length(handicap_seq), ncol = length(beta_seq))

# Populate the matrix with the handicap effect (H) values
for(i in 1:length(handicap_seq)) {
  for(j in 1:length(beta_seq)) {
    beta <- beta_seq[j]
    k <- handicap_seq[i]
    V <- 1000 # fixed variance
    H <- beta * k / (1 + beta * V)
    results_matrix[i, j] <- H
  }
}

# Convert the matrix to a data frame for plotting
results_df <- melt(results_matrix)
names(results_df) <- c("HandicapSeverity", "BetaStrength", "HandicapEffect")

# Set up PDF output
pdf("handicap_effect_heatmap_2A.pdf", width = 8, height = 6)

# Plot the heatmap
heatmap_plot <- ggplot(results_df, aes(x = BetaStrength, y = HandicapSeverity, fill = HandicapEffect)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = median(results_df$HandicapEffect), limit = c(min(results_df$HandicapEffect), max(results_df$HandicapEffect)),
                       space = "Lab", name="Handicap Effect") +
  theme_minimal() +
  labs(x = "Strength of Epistasis (β)", y = "Handicap Severity", title = "Heatmap of Handicap Effect") +
  coord_fixed(ratio = 1)

# Print the plot to the PDF
print(heatmap_plot)

# Close the PDF device
dev.off()
