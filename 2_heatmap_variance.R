library(ggplot2)
library(reshape2)

# Define the fixed beta and the range of variance and handicap severity
beta <- 0.0008
variance_seq <- seq(0, 10000, length.out = 100)
handicap_seq <- seq(0, 200, length.out = 100)

# Create an empty matrix to store the results
results_matrix <- matrix(nrow = length(handicap_seq), ncol = length(variance_seq))

# Populate the matrix with the handicap effect (H) values
for(i in 1:length(handicap_seq)) {
  for(j in 1:length(variance_seq)) {
    V <- variance_seq[j]
    k <- handicap_seq[i]
    H <- beta * k / (1 + beta * V)
    results_matrix[i, j] <- H
  }
}

# Convert the matrix to a data frame for plotting
results_df <- melt(results_matrix)
names(results_df) <- c("HandicapSeverity", "Variance", "HandicapEffect")

# Set up PDF output
pdf("handicap_effect_heatmap_2B.pdf", width = 8, height = 6)

# Plot the heatmap
heatmap_plot <- ggplot(results_df, aes(x = Variance, y = HandicapSeverity, fill = HandicapEffect)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "red", mid = "yellow", 
                       midpoint = median(results_df$HandicapEffect), limit = c(min(results_df$HandicapEffect), max(results_df$HandicapEffect)),
                       space = "Lab", name="Handicap Effect") +
  theme_minimal() +
  labs(x = "Variance", y = "Handicap Severity", title = "Heatmap of Handicap Effect (Fig 2B)") +
  coord_fixed(ratio = 1) + scale_x_continuous(labels=function(x)x*100) + scale_y_continuous(labels=function(y)y*2) 

# Print the plot to the PDF
print(heatmap_plot)

# Close the PDF device
dev.off()
