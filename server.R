
library(shiny)
library(magrittr)
library(ggplot2)

grid_size <- 5

initial_spread <- (grid_size ^ 2) %>% 
  rnorm %>% 
  is_greater_than(0.25) %>% 
  as.numeric


count_neighbors <- function(.data, row) {
  x <- row[[1]]
  y <- row[[2]]
  is_alive <- row[[3]]
  
  neighbors_x <- c(x - 1, x, x + 1) %>% 
    rep(each = 3) %>% 
    add(grid_size) %>% 
    mod(grid_size)
  neighbors_y <- c(y - 1, y, y + 1) %>% 
    rep(times = 3) %>% 
    add(grid_size) %>% 
    mod(grid_size)
  live_neighbors <- lapply(1:8, function(i) {
      .data %>% 
        filter(
          x == neighbors_x[i],
          y == neighbors_y[i]
        ) %>% 
        use_series(
          cell_value
        )
    })
  names(live_neighbors) <- paste0(neighbors_x,',',neighbors_y)
}

create_new_grid <- function(old_grid) {
  data.frame(
    x = old_grid$x,
    y = old_grid$y,
    live_neighbors = old_grid %>% 
      apply(1, function(row) count_neighbors(old_grid, row))
  )
}

cycle_grid <- function(old_grid) {
  new_grid <- create_new_grid(old_grid)
  invisible(new_grid)
}

plot_grid <- function(grid) {
  ggplot(grid) +
    geom_tile(aes(x = x, y = y, fill = cell_value)) +
    theme_bw()
}

shinyServer(function(input, output, session) {
  current_grid <- data.frame(
    'x' = rep(1:grid_size, each = grid_size) - 1,
    'y' = rep(1:grid_size, times = grid_size) - 1,
    'cell_value' = initial_spread
  )
  
  output$grid <- renderPlot({
    ggplot(current_grid) +
      geom_tile(aes(x = x, y = y, fill = cell_value)) +
      theme_bw()
  })
})

