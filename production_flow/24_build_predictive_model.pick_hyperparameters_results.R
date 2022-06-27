

build_predictive_model.pick_hyperparameters_results =
  function(
    model_folder_locn,
    train_or_test_rank = c("test", "train"),
    manually_picked_rank = NULL,
    manually_pick_graph_extents = NULL, # list(xlim = c(0, 100), ylim = c(0,0.15))
    # manual_HP_parms_override = NULL, #list(mtry_try, min.node.size_try, num_trees_try, sample.fraction_try)
    run_if = FALSE
  ){
    if (run_if) {
      "
      Read back in 'HP_grid.csv'
      Create the hp plot, NRMSE vs rank, save to disk
      capture the hyperparameters of best/selected iteration, save as csv
      "
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("tests/check_column_names_as_expected.R", local = TRUE)
      
      install_load_libraries_strings(c("data.table",
                                       "ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats"))
      model_folder_locn =
        add_path_slash_if_needed(model_folder_locn)
      
      HP_grid = fread(paste0(model_folder_locn,
                             "HP_grid.csv"))
      check_column_names_as_expected(
        data_frame_to_check = HP_grid,
        expected_column_names = c("RUN_ID",
                                  "mtry_try",
                                  "min.node.size_try",
                                  "num_trees_try",
                                  "sample.fraction_try",
                                  "VAL_NRMSE",
                                  "TRAIN_NRMSE"),
        data_frame_description = "HP_grid Results")
      
      ### extract first element of "train or test rank to use"
      if (length(train_or_test_rank) < 1) stop("train_or_test_rank missing. Enter string or vector of strings")
      if (length(train_or_test_rank) > 1) train_or_test_rank = train_or_test_rank[1]
      if (!(train_or_test_rank %in% c("test", "train"))) stop(paste("Invalid value for train_or_test_rank:", train_or_test_rank))
      
      ### error check manually_pick_graph_extents, order of checks matters
      if (!is.null(manually_pick_graph_extents)) {
        if (length(manually_pick_graph_extents) != 2) stop("manually_pick_graph_extents should be length 2")
        if (!("xlim" %in% names(manually_pick_graph_extents))) stop("no xlim in manually_pick_graph_extents")
        if (!("ylim" %in% names(manually_pick_graph_extents))) stop("no ylim in manually_pick_graph_extents")
        if (length(manually_pick_graph_extents$xlim) != 2) stop("manually_pick_graph_extents$xlim not length 2")
        if (length(manually_pick_graph_extents$ylim) != 2) stop("manually_pick_graph_extents$ylim not length 2")
        if (manually_pick_graph_extents$xlim[1] > manually_pick_graph_extents$xlim[2]) stop("manually_pick_graph_extents$xlim 1st > 2nd")
        if (manually_pick_graph_extents$ylim[1] > manually_pick_graph_extents$ylim[2]) stop("manually_pick_graph_extents$ylim 1st > 2nd")
      }
      list(xlim = c(0, 100), ylim = c(0,0.15))
      
      ### get ranked order of VAL and TRAIN NRMSE's
      HP_grid =
        HP_grid %>% 
        arrange(VAL_NRMSE, TRAIN_NRMSE) %>%
        mutate(VAL_RANK = 1:n()) %>%
        arrange(TRAIN_NRMSE, VAL_NRMSE) %>%
        mutate(TRAIN_RANK = 1:n()) %>%
        arrange(RUN_ID)
      
      ### HP grid results df
      HP_grid_results =
        HP_grid %>% 
        filter(VAL_NRMSE == min(VAL_NRMSE)) %>% 
        arrange(VAL_RANK) %>% 
        mutate(RESULT = "best VAL_NRMSE") %>% 
        bind_rows(HP_grid %>% 
                    filter(TRAIN_NRMSE == min(TRAIN_NRMSE)) %>%
                    arrange(TRAIN_RANK) %>%
                    mutate(RESULT = "best TRAIN_NRMSE"))
      
      if (!is.null(manually_picked_rank)) {
        if (train_or_test_rank == "train") {
          HP_grid_results =
            HP_grid_results %>% 
            bind_rows(HP_grid %>% 
                        filter(TRAIN_RANK == manually_picked_rank) %>% 
                        mutate(RESULT = "manually picked on train rank"))
          
        } else if (train_or_test_rank == "test") {
          HP_grid_results =
            HP_grid_results %>% 
            bind_rows(HP_grid %>% 
                        filter(VAL_RANK == manually_picked_rank) %>% 
                        mutate(RESULT = "manually picked on VAL rank"))
        }
      }
      
      HP_grid_results %>% 
        fwrite(paste0(model_folder_locn,
                      "HP_grid_results.csv"))
      
      ### graph data
      HP_grid_for_graph =
        HP_grid %>% 
        pivot_longer(cols = contains("NRMSE"),
                     values_to =  "NRMSE_COMBINED",
                     names_to = "WHICH_NRMSE")
      
      HP_grid_for_graph %>% 
        fwrite(paste0(model_folder_locn,
                      "HP_grid_for_graph.csv"))
      
      
      ### build initial plots
      HP_plot_VAL_1 =
        HP_grid_for_graph %>% 
        ggplot(aes(VAL_RANK, NRMSE_COMBINED, color = WHICH_NRMSE)) +
        geom_line() +
        theme_bw() +
        scale_color_brewer(palette = "Set1")
      
      HP_plot_TRAIN_1 =
        HP_grid_for_graph %>% 
        ggplot(aes(TRAIN_RANK, NRMSE_COMBINED, color = WHICH_NRMSE)) +
        geom_line() +
        theme_bw() +
        scale_color_brewer(palette = "Set1")
      
      ### update plot if rank manually picked
      if (!is.null(manually_picked_rank)) {
        manually_picked_rank = as.integer(manually_picked_rank)
        
        if (train_or_test_rank == "test") { # then apply to train graph
          HP_plot_VAL_1 =
            HP_plot_VAL_1 +
            geom_vline(xintercept = manually_picked_rank,
                       color = "darkblue") +
            ggtitle(paste0("Picked result at rank ",
                           manually_picked_rank))
        }
        if (train_or_test_rank == "train") { # then apply to train graph
          HP_plot_TRAIN_1 =
            HP_plot_TRAIN_1 +
            geom_vline(xintercept = manually_picked_rank,
                       color = "firebrick") +
            ggtitle(paste0("Picked result at rank ",
                           manually_picked_rank))
        }
      }
      
      ggsave(filename = paste0(model_folder_locn, "HP_plot_VAL_1.png"),
             plot = HP_plot_VAL_1)
      ggsave(filename = paste0(model_folder_locn, "HP_plot_TRAIN_1.png"),
             plot = HP_plot_TRAIN_1)
      
      
      ### update plot if graph extents specified
      if (!is.null(manually_pick_graph_extents)) { # if graph extents specified
        if (train_or_test_rank == "train") { # then apply to train graph
          HP_plot_2 =
            HP_plot_TRAIN_1 +
            coord_cartesian(xlim = manually_pick_graph_extents$xlim,
                            ylim = manually_pick_graph_extents$ylim)
        }
        if (train_or_test_rank == "test") { # then apply to test graph
          HP_plot_2 =
            HP_plot_VAL_1 +
            coord_cartesian(xlim = manually_pick_graph_extents$xlim,
                            ylim = manually_pick_graph_extents$ylim)
          
        }
        ggsave(filename = paste0(model_folder_locn, "HP_plot_2.png"),
               plot = HP_plot_2)
      }
      
      # return(HP_grid_for_graph)
      
    }
  }