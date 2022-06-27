

create_grid_in_convex_hull_from_points =
  function(
    input_points, # expects df of columns "x" and "y"
    distance_between_grid_points = 500,
    epsilon = 0.0, # perturbation distance
    run_if = FALSE
  ){
    '
    Given set of well points, perturb up/down/left/right by epsilon.
    Create convex hull around those points.
    Create a regular grid of points within that convex hull.
    Returns a data.frame with columns "x" and "y"
    '
    if (run_if) {
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("tests/check_column_names_as_expected.R", local = TRUE)
      
      if (!("dismo" %in% rownames(installed.packages()))) install.packages("dismo")
      
      libraries_to_load = c("ggplot2",
                            "dplyr",
                            "tidyr",
                            "readr",
                            "purrr",
                            "tibble",
                            "stringr",
                            "forcats", "data.table", "dplyr")
      install_load_libraries_strings(libraries_to_load)
      
      input_points = 
        input_points %>% 
        na.omit()
      
      check_column_names_as_expected(input_points, c("x", "y"), "Input reference locations")
      
      input_points =
        input_points %>% 
        bind_rows(input_points %>% 
                    mutate(x = x + epsilon)) %>% 
        bind_rows(input_points %>% 
                    mutate(x = x - epsilon)) %>% 
        bind_rows(input_points %>% 
                    mutate(y = y + epsilon)) %>% 
        bind_rows(input_points %>% 
                    mutate(y = y - epsilon))
      
      ### get input points which are vertices of convex hull, in clockwise order
      chull_index = chull(x = input_points$x, y = input_points$y)
      
      convex_hull = dismo::convHull( as.matrix(input_points)[chull_index, ], n = 1)
      
      positions_grid = 
        expand.grid(x = seq(from = min(convex_hull@presence$x), 
                            to = max(convex_hull@presence$x),
                            by = distance_between_grid_points),
                    y = seq(from = min(convex_hull@presence$y), 
                            to = max(convex_hull@presence$y),
                            by = distance_between_grid_points))
      
      convHull_predictions =
        dismo::predict(convex_hull, 
                       as.matrix(positions_grid)
                       #, progress="."
        )
      
      positions_grid = 
        positions_grid %>% 
        mutate(in_convex_hull =  convHull_predictions) %>% 
        filter(in_convex_hull == 1) %>% 
        dplyr::select(-in_convex_hull)
      
      return(positions_grid)
      
    }
  }