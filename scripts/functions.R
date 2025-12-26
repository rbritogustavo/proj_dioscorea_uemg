# Frequency count
freq_count <- function(df, col, sep = ";", col_name) {
  freq_df <- df %>% 
    separate_rows({{col}}, sep = sep) %>% 
    mutate({{col}} := trimws({{col}})) %>% 
    count({{col}}, sort = TRUE, name = col_name)
  
  return(freq_df)
}

# Basic statistics
calc_stats <- function(data, col) {
  # Function
  stats <- data |>
    summarise(
      sum = sum({{ col }}),
      max = max({{ col }}),
      min = min({{ col }}),
      mean = mean({{ col }}),
      std_dev = sd({{ col }})
    )

  return(stats)
}

# Base donut plot -----
donut_plot <- function(data, values, label = values, color = "white", fill, palette = NULL) {
  ggdonutchart(
    data = data,
    x = {{ values }},
    label = {{ label }},
    color = color,
    fill = {{ fill }},
    palette = palette,
    lab.font = c("bold")
  )
}

# Base choropleth map -----
choropleth_map <- function(data, fill, color = "grey70", na_value = "grey90") {
  ggplot(data = {{ data }}) +
    geom_sf(aes(fill = {{ fill }}), color = "grey70") +
    scale_fill_manual(
      na.value = na_value,
      values = c("#009E73", "#E69F00", "#0072B2", "grey90")
    ) +
    labs(fill = NULL) +
    theme_void() +
    theme(legend.position = "bottom")
}

# Base bar plot (vertical or horizontal) -----
bar_plot <- function(data,
                     x,
                     y,
                     slice = 5,
                     fill = "#0072B2",
                     lab_x = NULL,
                     lab_title = NULL,
                     orientation = c("vertical", "horizontal")) {
  orientation <- match.arg(orientation)

  df <- data %>%
    slice_max(n = slice, order_by = {{ y }}) %>%
    mutate({{ x }} := fct_reorder({{ x }}, {{ y }}, .desc = TRUE))

  p <- ggplot(df, aes({{ x }}, {{ y }})) +
    geom_col(fill = fill)

  if (orientation == "vertical") {
    p <- p +
      geom_text(aes(label = {{ y }}), vjust = -0.5, fontface = "bold")
  } else {
    p <- p +
      geom_text(aes(label = {{ y }}), hjust = -0.2, fontface = "bold") +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(df %>% pull({{ x }}))))
  }

  p <- p +
    scale_x_discrete(expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = lab_x, title = lab_title) +
    theme_minimal() +
    theme(
      axis.line.x = element_line(color = "gray30"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )

  return(p)
}
