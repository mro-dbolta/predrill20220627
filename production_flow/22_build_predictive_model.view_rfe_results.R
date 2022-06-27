
build_predictive_model.view_rfe_results =
  function(
    rfe_results_location,
    train_data_locn,
    test_data_locn,
    manually_picked_size = NULL, # if this is null, pick best val error result
    manually_pick_graph_extents = NULL, # list(xlim = c(0, 100), ylim = c(0,0.15))
    target_variables = c("LIQUID_360", "LIQUID_720"),
    run_if = FALSE
  ){
    if (run_if) {
      "
      This function reads back in 'RFE_statistics.csv'
      then creates the rfe plot, saved to disk
      then gets the best features, saves as a csv
      
      For first run, leave manually_picked_size as NULL, check output files, 
      then specify size, and rerun. 
      "
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/calc_NRMSE_TSS.R", local = TRUE)
      source("tests/check_column_names_as_expected.R", local = TRUE)
      if (!("caret" %in% rownames(installed.packages()))) install.packages("caret")
      
      install_load_libraries_strings(c("data.table",
                                       "ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats"))
      rfe_results_location =
        add_path_slash_if_needed(rfe_results_location)
      
      rfe_statistics = fread(paste0(rfe_results_location,
                                    "RFE_statistics.csv"))
      
      check_column_names_as_expected(data_frame_to_check = rfe_statistics,
                                     expected_column_names = c("VARIABLE_REMOVED",
                                                               "NRMSE",
                                                               "NRMSE_TRAIN",
                                                               "NUMBER_OF_INPUT_VARIABLES"),
                                     data_frame_description = "RFE Statistics Results")
      
      rfe_statistics =
        rfe_statistics %>% 
        mutate(REMAINING_NUMBER_VARIABLES_IN_MODEL = 
                 if_else(VARIABLE_REMOVED == "None", NUMBER_OF_INPUT_VARIABLES,
                         NUMBER_OF_INPUT_VARIABLES - 1L))
      
      # return(rfe_statistics)
      
      ######################################################################
      ### get final errors
      # total sum of squares error
      test_data =
        load_data_that_has_api(test_data_locn)
      
      NRMSE_TSS = calc_NRMSE_TSS(
        target_variables = target_variables,
        data_set_df = test_data
      )
      
      train_data =
        load_data_that_has_api(train_data_locn)
      
      NRMSE_TRAIN_TSS = calc_NRMSE_TSS(
        target_variables = target_variables,
        data_set_df = train_data
      )
      
      ######################################################################
      
      last_variable_removed =
        rfe_statistics  %>% 
        ### have to do this sequentially because last variable may not be globally worst model
        filter(NUMBER_OF_INPUT_VARIABLES == min(NUMBER_OF_INPUT_VARIABLES)) %>% 
        filter(NRMSE == max(NRMSE)) %>% 
        .$VARIABLE_REMOVED
      
      rfe_statistics_best_at_each =
        rfe_statistics %>% 
        group_by(NUMBER_OF_INPUT_VARIABLES) %>% 
        filter(NRMSE == min(NRMSE)) %>% 
        ungroup()  %>% 
        arrange(desc(NUMBER_OF_INPUT_VARIABLES)) %>% 
        bind_rows(
          ### last row is total sum of squares error
          data.frame(
            VARIABLE_REMOVED = last_variable_removed,
            NRMSE = NRMSE_TSS,
            NRMSE_TRAIN = NRMSE_TRAIN_TSS,
            NUMBER_OF_INPUT_VARIABLES = 1,
            REMAINING_NUMBER_VARIABLES_IN_MODEL = 0
          )
        )
      
      
      
      # return(rfe_statistics_best_at_each)
      
      results_summary_df =
        rfe_statistics %>%
        filter(NRMSE == min(NRMSE)) %>% 
        mutate(RESULT = "LOWEST VALIDATION ERROR") %>% 
        select(RESULT,
               REMAINING_NUMBER_VARIABLES_IN_MODEL,
               NRMSE_TRAIN,
               NRMSE)
      
      best_result_size =
        rfe_statistics %>%
        filter(NRMSE == min(NRMSE)) %>%
        .$REMAINING_NUMBER_VARIABLES_IN_MODEL
      
      
      ### train & test error graph
      rfe_results_plot =
        rfe_statistics_best_at_each %>% 
        select(-VARIABLE_REMOVED, -NUMBER_OF_INPUT_VARIABLES) %>% 
        pivot_longer(cols = starts_with("NRMSE"),
                     values_to =  "NRMSE_COMBINED",
                     names_to = "WHICH_NRMSE") %>% 
        ggplot(aes(REMAINING_NUMBER_VARIABLES_IN_MODEL, NRMSE_COMBINED, color = WHICH_NRMSE)) +
        geom_line() +
        geom_point() +
        scale_color_brewer(palette = "Set1") +
        geom_vline(xintercept = best_result_size, color = "firebrick") +
        theme_bw()
      
      
      if (is.null(manually_picked_size)) {
        rfe_results_plot =
          rfe_results_plot +
          ggtitle(paste0("Best Val Error: red at ",
                         best_result_size))
        
        ### get the best features
        best_features =
          rfe_statistics %>% 
          filter(NUMBER_OF_INPUT_VARIABLES == best_result_size) %>% 
          .$VARIABLE_REMOVED
        
      } else {
        # https://stackoverflow.com/questions/8217901/breaking-loop-when-warnings-appear-in-r/8217929
        options(warn = 2)  # treat warnings as errors
        manually_picked_size = as.integer(manually_picked_size)
        options(warn = 1)  # print warnings as they occur
        
        ### update plot for manual picked size
        rfe_results_plot =
          rfe_results_plot +
          geom_vline(xintercept = manually_picked_size, 
                     color = "darkblue") +
          ggtitle(paste0("Best Val Error: red at ",
                         best_result_size,
                         ", Picked: blue at ",
                         manually_picked_size))
        
        ### update results_summary_df for manual picked size
        results_summary_df_manual_pick =
          rfe_statistics_best_at_each %>% 
          filter(REMAINING_NUMBER_VARIABLES_IN_MODEL == manually_picked_size) %>% 
          mutate(RESULT = "MANUALLY PICKED RESULT") %>% 
          select(RESULT, REMAINING_NUMBER_VARIABLES_IN_MODEL, NRMSE_TRAIN, NRMSE)
        
        results_summary_df =
          results_summary_df %>% 
          bind_rows(results_summary_df_manual_pick)
        
        ### get the best features
        best_features =
          rfe_statistics %>% 
          filter(NUMBER_OF_INPUT_VARIABLES == manually_picked_size) %>% 
          .$VARIABLE_REMOVED
        
      }
      
      ggsave(
        filename = paste0(rfe_results_location,
                          "rfe_results_plot.png"),
        plot = rfe_results_plot
      )
      
      if (!is.null(manually_pick_graph_extents)) {
        rfe_results_plot_zoomed =
          rfe_results_plot +
          coord_cartesian(xlim = manually_pick_graph_extents$xlim,
                          ylim = manually_pick_graph_extents$ylim)
        
        ggsave(
          filename = paste0(rfe_results_location,
                            "rfe_results_plot_zoomed.png"),
          plot = rfe_results_plot_zoomed
        )
        
      }
      
      
      best_features =
        data.frame(BEST_FEATURES = best_features)
      best_features %>% 
        fwrite(paste0(rfe_results_location,
                      "best_features.csv"))
      
      results_summary_df %>% 
        fwrite(paste0(rfe_results_location,
                      "results_summary.csv"))
      
      ###########################################################
      
      ### "importances"
      if (is.null(manually_picked_size)) {
        
        variable_importances_df =
          rfe_statistics_best_at_each %>% 
          filter(NUMBER_OF_INPUT_VARIABLES <= best_result_size + 1L) %>% 
          mutate(VARIABLE_REMOVED = if_else(NUMBER_OF_INPUT_VARIABLES == max(NUMBER_OF_INPUT_VARIABLES),
                                            "ALL_VARIABLES_IN",
                                            VARIABLE_REMOVED),
                 VARIABLE_REMOVED = if_else(NRMSE == min(NRMSE),
                                            paste0(VARIABLE_REMOVED,"_best_overall"),
                                            VARIABLE_REMOVED))
        
        importance_plot =
          variable_importances_df %>% 
          mutate(VARIABLE_TYPE = str_locate(VARIABLE_REMOVED, "_")[,"start"],
                 VARIABLE_TYPE = str_sub(VARIABLE_REMOVED, end = VARIABLE_TYPE - 1L)) %>% 
          ggplot(aes(y = reorder(VARIABLE_REMOVED, NUMBER_OF_INPUT_VARIABLES), # don't use "desc()" because graph actually starts at bottom
                     x = NRMSE,
                     fill = VARIABLE_TYPE)) +
          geom_col(color = "black") +
          theme_bw() +
          xlab("Holdout NRMSE") +
          ylab("Variable Removed in Order (Last Removed @ Bottom)") +
          scale_fill_brewer(palette = "Set1")
        
      } else {
        
        variable_importances_df =
          rfe_statistics_best_at_each %>% 
          filter(NUMBER_OF_INPUT_VARIABLES <= manually_picked_size + 1L) %>% 
          mutate(VARIABLE_REMOVED = if_else(NUMBER_OF_INPUT_VARIABLES == max(NUMBER_OF_INPUT_VARIABLES),
                                            "ALL_VARIABLES_IN",
                                            VARIABLE_REMOVED),
                 VARIABLE_REMOVED = if_else(NRMSE == min(NRMSE),
                                            paste0(VARIABLE_REMOVED,"_best_after_removed"),
                                            VARIABLE_REMOVED))
        
        importance_plot =
          variable_importances_df %>% 
          mutate(VARIABLE_TYPE = str_locate(VARIABLE_REMOVED, "_")[,"start"],
                 VARIABLE_TYPE = str_sub(VARIABLE_REMOVED, end = VARIABLE_TYPE - 1L)) %>% 
          ggplot(aes(y = reorder(VARIABLE_REMOVED, NUMBER_OF_INPUT_VARIABLES), # don't use "desc()" because graph actually starts at bottom
                     x = NRMSE,
                     fill = VARIABLE_TYPE)) +
          geom_col(color = "black") +
          theme_bw() +
          xlab("Holdout NRMSE") +
          ylab("Variable Removed in Order (Last Removed @ Bottom)") +
          scale_fill_brewer(palette = "Set1")
      }
      
      variable_importances_df %>% 
        fwrite(paste0(rfe_results_location,
                      "variable_importances.csv"))
      
      ggsave(
        filename = paste0(rfe_results_location,
                          "importance_plot.png"),
        plot = importance_plot
      )
      
      # return(variable_importances_df)
      
      # return(list(rfe_statistics_best_at_each,
      #             best_features))
      
    }
  }