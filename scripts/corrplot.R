my_ggcorrplor <- function(data) {
  ggcorrplot(
    data,
    method = "square",
    type = "lower",
    legend.title = "Correlation\ncoefficient",
    outline.color = "gray",
    hc.order = TRUE,
    lab = TRUE,
    lab_col = "black",
    lab_size = 4,
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 11,
    digits = 1
  ) +
    theme(text = element_text(color = "black", size = 11, family = "Syne"), 
          axis.text = element_text(color = "black")) +
    labs(x = element_blank(), y = element_blank())
}

my_ggcorrplor_grey <- function(data) {
  ggcorrplot(
    data,
    method = "square",
    type = "lower",
    legend.title = "Correlation\ncoefficient",
    colors = c("#F9F9F9", "#D2D2D2", "#676767", "#1B1B1B"),
    outline.color = "gray",
    hc.order = TRUE,
    lab = TRUE,
    lab_col = "black",
    lab_size = 4,
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 11,
    digits = 1
  ) +
    theme(text = element_text(color = "black", size = 11, family = "Syne"),
          axis.text = element_text(color = "black")) +
    labs(x = element_blank(), y = element_blank())
}
