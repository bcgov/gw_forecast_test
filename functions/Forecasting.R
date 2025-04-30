forecast_model <- function(Time_series_data, forecast_days, num_cores, figure_location,output_path, model_path,pgown_well_info,ensemble_forecast_data,deterministic_forecast_data){
  
  
  # Creates historical groundwater levels for figures
  
  temp_WL_states<- Time_series_data %>%
    mutate(days_in_year = yday(Date)) %>%
    #filter(Date <= as.Date("2023-12-31"))%>%
    group_by(days_in_year, Well)%>%
    drop_na(groundwater) %>%
    summarise(Min = min(groundwater), 
              per5th = quantile(groundwater, 0.05),
              per10th = quantile(groundwater, 0.1),
              per25th = quantile(groundwater, 0.25),
              per50th = quantile(groundwater, 0.5),
              per75th = quantile(groundwater, 0.75),
              per90th = quantile(groundwater, 0.9),
              per95th = quantile(groundwater, 0.95),
              Max = max(groundwater)) %>%
    mutate(fake_date = as.Date("2020-01-01")+days_in_year)
  
  
  Waterlevel_adjustments <- Time_series_data %>%
    filter(year(Date) != year(Sys.Date()))%>%
    group_by(Well) %>%
    summarise(groundwater_period_average = mean(groundwater_period_average, na.rm = TRUE)) %>%
    ungroup() 
  
  # register number of Parallel cores to utilize
  
  registerDoParallel(cores = num_cores) 
  
  # Creates a list of Wells from the available data
  pgown_well_info_temp <- pgown_well_info 
  
    
  #  filter(Snow_influenced == 0)
  
  
  pgown_well_info_temp2 <- pgown_well_info %>%
    dplyr::select(Well,Regional_group)
  
  last_measurements <- Time_series_data %>%
    group_by(Well) %>%
    drop_na(groundwater)%>%
    summarise(last_measurements= max(Date, na.rm = TRUE)) %>%
    ungroup()
  
  last_measurements <- left_join(pgown_well_info_temp2,last_measurements)
  
  
  
  Well_list <- as.list(unique(pgown_well_info_temp$Well))
  
  # Pre-allocate a list to store the results
  simulated_data <- vector("list", length(Well_list))
  
  
  #run calculations for each well in parellel
  
  simulated_data <- foreach(y = Well_list, .combine = rbind, 
                            .packages = c("ggpubr", "dplyr",
                                          'ggplot2',
                                          'tidyr',
                                          'lubridate',
                                          'readr',
                                          'stringr',
                                          'tibble',
                                          'forcats',
                                          'purrr',
                                          'gt',
                                          "mgcv", "randomForest","zoo","ggnewscale", "cowplot","nnet")) %dopar% {
                               # filter data by well     
                              #y=Well_list[1]
  
   last_measurements_well <- last_measurements %>%
     filter(Well == y) %>%
     pull(last_measurements)
   if(between(last_measurements_well, Sys.Date() - 3, Sys.Date())) {
     

                              #y = "OW204"
    temp <- Time_series_data %>%
      filter(Well == y) 
    
    
    temp_WL_states_temp <- temp_WL_states %>%
      filter(Well == y)
    
    pgown_well_info_Well_info <- pgown_well_info_temp%>%
      filter(Well == y)
    
    #groundwater level adjustment to actual DTW
    
    Waterlevel_adjustments_temp <- Waterlevel_adjustments %>%
      filter(Well == y)
    
    Waterlevel_adjustments_temp<- Waterlevel_adjustments_temp$groundwater_period_average
    
    # extract well leg time 
    lag_period <- pgown_well_info_Well_info %>%
      pull(Lag_time)
    
    a_coeff <- pgown_well_info_Well_info %>%
      pull(DWC_Precip)
    
    
    
    a_coeff_snow <- pgown_well_info_Well_info %>%
      pull(DWC_Snow)
    
    ann_size <- pgown_well_info_Well_info %>%
      pull(ann_size)  
    
    ann_decay<- pgown_well_info_Well_info %>%
      pull(ann_decay)
    
    ann_maxit <- pgown_well_info_Well_info %>%
      pull(ann_maxit)
    
    # Function to classify performance based on R² and NRMSE
    classify_performance <- function(r2, nrmse) {
      if (r2 >= 0.8 & nrmse <= 0.15) {
        return("Good")
      } else if ((r2 >= 0.6 & nrmse <= 0.25) | (r2 >= 0.8 & nrmse <= 0.25)) {
        return("Fair")
      } else {
        return("Poor")
      }
    }
    
    # Function to classify forecast results based on RSR
    classify_forecast_results <- function(rsr) {
      if (rsr < 1) {
        return("Results of Value")
      } else {
        return("Results of Limited Value")
      }
    }
    
    # Assign categories for each forecast period
    well_lag_time_2 <- pgown_well_info_Well_info %>%
      rowwise() %>%
      mutate(
        Performance_14 = classify_performance(R2_14, NRMSE_14),
        Forecast_14 = classify_forecast_results(RSR_14),
        Performance_30 = classify_performance(R2_30, NRMSE_30),
        Forecast_30 = classify_forecast_results(RSR_30),
        Performance_60 = classify_performance(R2_60, NRMSE_60),
        Forecast_60 = classify_forecast_results(RSR_60),
        Performance_90 = classify_performance(R2_90, NRMSE_90),
        Forecast_90 = classify_forecast_results(RSR_90)
      ) %>%
      mutate(
        comb_perf_value_14 = paste0(Performance_14, ", ", Forecast_14),
        comb_perf_value_30 = paste0(Performance_30, ", ", Forecast_30),
        comb_perf_value_60 = paste0(Performance_60, ", ", Forecast_60),
        comb_perf_value_90 = paste0(Performance_90, ", ", Forecast_90)
      ) %>%
      dplyr::select(Well, comb_perf_value_14, comb_perf_value_30, comb_perf_value_60, comb_perf_value_90) %>%
      gather(key = "per_name", value = "performance", comb_perf_value_14, comb_perf_value_30, comb_perf_value_60, comb_perf_value_90) %>%
      mutate(lag_day = ifelse(per_name == "comb_perf_value_14", 14, 
                              ifelse(per_name == "comb_perf_value_30", 30,
                                     ifelse(per_name == "comb_perf_value_60", 60,
                                            ifelse(per_name == "comb_perf_value_90", 90,NA))))) %>%
      dplyr::select(Well, lag_day,performance)
    
    
    
    
    
    
    
    deterministic_forecast_data_y <- deterministic_forecast_data %>%
      filter(Well == y) %>%
      dplyr::rename(total_precip = PP, mean_temp = TA) %>%
      mutate(FC_type = 1)%>%
      mutate(Date = as.Date(Date))%>%
      dplyr::select(Well, Date, total_precip, mean_temp, FC_type )
    
    ensemble_forecast_data_y <- ensemble_forecast_data %>%
      filter(Well == y) %>%
      dplyr::rename(total_precip = PP, mean_temp = TA, en_sim = simulation) %>%
      mutate(FC_type = 2) %>%
      dplyr::select(Well, Date, total_precip, mean_temp, en_sim,FC_type )
    
    max_ensemble <- max(ensemble_forecast_data_y$en_sim)
    
    


    #create empty data frame to put data    
    Time_series_data_2 <- data.frame()

    # RAIN INFLUENCED ----------------------------------------------------------
    if (unique(pgown_well_info_Well_info$Snow_influenced == 0)) {
      plot_type <- "rain"
      #run for predicted forcast intervals (i.e 14,30,60 and 90 days)
        for(x in forecast_days){
          #x=90
        #summarise leading and lagging variables

      
      # Preprocess temp
      temp2 <- temp %>%
          mutate(lag_day = x, #forcast interval
               Date_predicted = lead(Date,x),
               precipitation_lag = rollsum(total_precip, lag_period, fill = NA, align='right', na.rm = TRUE), #recharge lagging precipitation
               mean_temp_lag = rollmean(mean_temp, lag_period, fill = NA,align='right', na.rm = TRUE), # recharge lagging temperature
               groundwater_predict = lead(groundwater, x), # actual groundwater level we are predicting
               mean_temp_lead = rollmean(mean_temp, x, fill = NA,align='left', na.rm = TRUE), #forecasted temperature
               precip_lead = rollsum(total_precip, x, fill = NA,align='left', na.rm = TRUE) #forecasted precipitation
        ) %>%
        mutate( SWE = NA,
                SWE_lag_diff = NA,
                SWE_lead_diff = NA,
                groundwater_diff = groundwater_predict - groundwater,
                actual_predicted_groundwater = groundwater_predict,
                year = year(Date_predicted),
                days_in_year = yday(Date_predicted)
        ) %>%
        dplyr::select( # only select variables of interest remove others
          Date,
          Date_predicted,
          Well,
          lag_day,
          days_in_year,
          year,
          groundwater, 
          groundwater_predict, 
          actual_predicted_groundwater,
          mean_temp_lag, 
          mean_temp_lead,  
          groundwater_diff,
          precipitation_lag, 
          precip_lead, 
          SWE,
          SWE_lag_diff,
          SWE_lead_diff
        )
      
      
      
      
      calculate_weighted_lead <- function(i, x, lag_period, a_coeff) {
        
        temp %>%
          
          mutate(lag_day = x, #forcast interval
                 Date_predicted = lead(Date,x)
                ) %>%
          mutate(lag_day_adjusted = (x-lag_period))%>%
          mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff))%>%
          mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff ))%>%
          mutate(normal_weight = (normal_weight - 0)/(weight_max-0))%>%
          mutate(weighted_lead = lead(total_precip,i)*normal_weight)%>%
          dplyr::select( # only select variables of interest remove others
            Date,
            Date_predicted,
            Well,
            lag_day,
            lag_day_adjusted,
            weighted_lead
          )
        

      }
      
      lead_data <- map_dfr(1:x, calculate_weighted_lead, x = x, lag_period = lag_period, a_coeff = a_coeff)
      
      
      lead_data <-   lead_data %>%
        group_by(# only select variables of interest remove others
          Date,
          Date_predicted,
          Well,
          lag_day
        ) %>%
        summarise(weighted_lead = mean(weighted_lead, na.rm = TRUE))%>%
        ungroup() 
      
      
      
      lag_data <- data.frame()
      
      lag_period_length = lag_period*3
      
      calculate_weighted_lag <- function(i, lag_period, a_coeff) {

        temp %>%
         
          mutate(lag_day = x, #forcast interval
                 Date_predicted = lead(Date,x)
                      ) %>%
          mutate(lag_day_adjusted = (lag_period))%>%
          mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff))%>%
          mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff ))%>%
          mutate(normal_weight = (normal_weight - 0)/(weight_max-0))%>%
          mutate(weighted_lag = lag(total_precip,i)*normal_weight)%>%
          dplyr::select( # only select variables of interest remove others
            Date,
            Date_predicted,
            Well,
            lag_day,
            lag_day_adjusted,
            weighted_lag
          )
        

      }
      
      
      lag_data <- map_dfr(1:(lag_period * 3), calculate_weighted_lag, lag_period = lag_period, a_coeff = a_coeff)
      
      
      
      lag_data <-   lag_data %>%
        group_by(# only select variables of interest remove others
          Date,
          Date_predicted,
          Well,
          lag_day
        ) %>%
        summarise(weighted_lag = mean(weighted_lag, na.rm = TRUE))%>%
        ungroup() 
      
      lead_data_lag <- full_join(lead_data, lag_data)
      
      temp2 <- full_join(temp2,lead_data_lag)
      
      temp2 <- temp2 %>%
        mutate(precip_lead_org = precip_lead,
               precipitation_lag_org = precipitation_lag,
               precip_lead = weighted_lead,
               precipitation_lag = weighted_lag)%>%
        mutate(max_precip_lead = max(precip_lead, na.rm = TRUE),
               min_precip_lead = min(precip_lead, na.rm = TRUE),
               max_precipitation_lag = max(precipitation_lag, na.rm = TRUE),
               min_precipitation_lag = min(precipitation_lag, na.rm = TRUE),
               max_mean_temp_lead = max(mean_temp_lead, na.rm = TRUE),
               min_mean_temp_lead = min(mean_temp_lead, na.rm = TRUE),
               max_mean_temp_lag = max(mean_temp_lag, na.rm = TRUE),
               min_mean_temp_lag = min(mean_temp_lag, na.rm = TRUE),
               max_groundwater = max(groundwater, na.rm = TRUE),
               min_groundwater = min(groundwater, na.rm = TRUE),
               max_groundwater_diff = max(groundwater_diff, na.rm = TRUE),
               min_groundwater_diff = min(groundwater_diff, na.rm = TRUE)
        ) %>%
        mutate(precip_lead = (precip_lead - min_precip_lead)/(max_precip_lead-min_precip_lead),
               precipitation_lag = (precipitation_lag - min_precipitation_lag)/(max_precipitation_lag-min_precipitation_lag),
               mean_temp_lag = (mean_temp_lag - min_mean_temp_lag)/(max_mean_temp_lag-min_mean_temp_lag),
               mean_temp_lead = (mean_temp_lead - min_mean_temp_lead)/(max_mean_temp_lead-min_mean_temp_lead),
               groundwater = (groundwater - min_groundwater)/(max_groundwater-min_groundwater),
               actual_predicted_groundwater = (actual_predicted_groundwater - min_groundwater)/(max_groundwater-min_groundwater), 
               groundwater_predict =  (groundwater_predict - min_groundwater)/(max_groundwater-min_groundwater),
               groundwater_diff =  (groundwater_diff - min_groundwater_diff)/(max_groundwater_diff-min_groundwater_diff)
        ) 
     
      
      temp2_original <- temp2
      
          
      
        
        
      # model_file <- paste0(model_path,"ANN_Model_",y,"_",x,".Rdata")
      model_file <- paste0("https://nrs.objectstore.gov.bc.ca/rfc-conditions/gw_forecasting/models/","ANN_Model_",y[[1]],"_",x,".Rdata") #AWS
      # load(url(model_file))
      
      # model_file <- paste0("https://nrs.objectstore.gov.bc.ca/rfc-conditions/gw_forecasting/models/","ANN_Model_",y[[1]],"_",x,".Rdata") #AWS
      # con <- url(model_file)
      # on.exit(close(con), add = TRUE)  # ensures connection closes even on error
      # load(con)
      # close(con) 
      
      # Create unique temp file per task
      tmp_rdata <- tempfile(fileext = ".RData")
      
      # Safely download the file
      download.file(model_file, destfile = tmp_rdata, mode = "wb")
      
      # Load it
      load(tmp_rdata)
      
        
           # Calculate the mean and standard deviation of the variables by day
        # need to use raw precip data 
        
        
        
        daily_stats<- temp2_original %>%
          mutate(
            mean_temp_lead = mean_temp_lead*(max_mean_temp_lead - min_mean_temp_lead) + min_mean_temp_lead#,
          )%>%
          group_by(days_in_year) %>%
          summarise(
            precip_lead_mean = mean(precip_lead_org, na.rm = TRUE),
            precip_lead_sd = sd(precip_lead_org, na.rm = TRUE),
            mean_temp_lead_mean = mean(mean_temp_lead, na.rm = TRUE),
            mean_temp_lead_sd = sd(mean_temp_lead, na.rm = TRUE)
                )
        
        
        # Generate Monte Carlo values for each day
        monte_carlo_df <- daily_stats %>%
          tidyr::uncount(500) %>%
          mutate(
            precip_lead = rnorm(n(), mean = precip_lead_mean, sd = precip_lead_sd),
            mean_temp_lead = rnorm(n(), mean = mean_temp_lead_mean, sd = mean_temp_lead_sd)
          ) 
        
        monte_carlo_df <- monte_carlo_df %>%
          dplyr::select(days_in_year, precip_lead, mean_temp_lead) %>%
          mutate(precip_lead = precip_lead/x, mean_temp_lead = mean_temp_lead) 
        
        
        last_date <- temp2_original%>%
          drop_na(groundwater)%>%
          summarise(last_measurements= max(Date, na.rm = TRUE)) %>%
          pull(last_measurements)
        
        
        last_date_days_in_year  <- yday(last_date+x)
        
        Prediction_date <- last_date+x
        monte_carlo_df <- monte_carlo_df %>%
          filter(days_in_year == last_date_days_in_year)%>%
          mutate(sim_num = row_number()) %>%
          mutate(joiner = 1) %>%
          dplyr::select(sim_num, joiner, precip_lead, mean_temp_lead)
        # Join the new data frame with the original one
        
        
        #forcasted_data
        
        temp_forcast <- data.frame()
        
        for(p in 1:x){
          
          
          temp_fc <- data.frame(
            Well = unlist(y),
            Date = as.Date(last_date+p),
            Snow_influenced =  unique(temp$Snow_influenced),
            groundwater_period_average = unique(temp$groundwater_period_average)
          ) 
          
          
          
          temp_fc <- temp_fc %>%
            mutate(days_in_year = yday(Date))
          
          temp_forcast <- rbind(temp_forcast,temp_fc)
          
        }
        
        
        
        
        temp_forcast4 <- data.frame()

        for(g in 1:max_ensemble){

          ensemble_forecast_data_z <- ensemble_forecast_data_y %>%
            filter(en_sim == g)
          
          
          temp_forcast2 <- full_join(deterministic_forecast_data_y,ensemble_forecast_data_z)
          
          temp_forcast2_2 <- temp_forcast2 %>%
          dplyr::select(-en_sim) %>%
            mutate(days_in_year = yday(Date+x))
          
          temp_forcast3 <- full_join(temp_forcast,temp_forcast2_2 )
          
    

          temp_forcast3_2  <- temp_forcast3 %>%
            mutate(joiner = 1) %>%
            full_join(monte_carlo_df) %>%
              mutate(total_precip = ifelse(is.na(FC_type), precip_lead, total_precip),
                     mean_temp = ifelse(is.na(FC_type), mean_temp_lead, mean_temp)) %>%
            mutate(FC_type = ifelse(is.na(FC_type), 3, FC_type)) %>%
            group_by(Well, Date) %>%
            mutate(min_FC = min(FC_type))%>%
            filter(min_FC == FC_type)%>%
            ungroup()%>%
            mutate(lag_day_adjusted = (lag_period)) %>%
            group_by(sim_num) %>%
            arrange(Date) %>%
            mutate(lead_num = row_number()) %>%
            ungroup()%>%
            mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff))%>%
            mutate(normal_weight = dnorm(lead_num, mean = lag_day_adjusted, sd = lag_period*a_coeff ))%>%
            mutate(normal_weight = (normal_weight - 0)/(weight_max-0)) %>%
            mutate(total_precip = total_precip*normal_weight) %>%
            filter(Date >= last_date & Date <= Prediction_date) %>%
            group_by(sim_num, Well)%>%
            summarise(precip_lead = mean(total_precip, na.rm = TRUE),
                      mean_temp_lead = mean(mean_temp, na.rm = TRUE)) %>%
            mutate(en_sim = g) %>%
            ungroup() 
          
          
          
          temp_forcast4 <- rbind(temp_forcast4,temp_forcast3_2)
        }
        
        
        temp_forcast4 <- temp_forcast4 %>%
          mutate(joiner = 1)
        
        
        
        temp2_pred <- temp2 %>%
          filter(Date ==last_date)%>%
          dplyr::select(Date, Well,groundwater,groundwater_diff, precipitation_lag, mean_temp_lag,
                        max_groundwater, min_groundwater, max_groundwater_diff, min_groundwater_diff, 
                        min_mean_temp_lead, max_mean_temp_lead, max_precip_lead, min_precip_lead) %>%
          mutate(joiner = 1) %>%
          mutate(Date_predicted = Date+x) %>%
          full_join(temp_forcast4) %>%
          mutate(precip_lead = (precip_lead - min_precip_lead)/(max_precip_lead-min_precip_lead),
                 mean_temp_lead = (mean_temp_lead - min_mean_temp_lead)/(max_mean_temp_lead-min_mean_temp_lead))
        
      
        # Filter the DataFrame by the last date -- and combine with percentiles
        temp2_pred2 <- temp2_pred 
          
         
           predicted_response_ANN <- predict(fit_ann, newdata = temp2_pred2)
           temp2_pred2 <- cbind(temp2_pred2, predicted_response_ANN)
          
          
          
          #Combine data
          temp2_pred2 <- temp2_pred2 %>%
            dplyr::rename(
                          ANN = predicted_response_ANN
            ) %>%
            gather(c(
                     ANN), key = "Model", value = predicted_value)
          
          temp2_pred2 <- temp2_pred2 %>%
            mutate(predicted_value = predicted_value*(max_groundwater_diff - min_groundwater_diff) + min_groundwater_diff,
                   groundwater = groundwater*(max_groundwater - min_groundwater) + min_groundwater)%>%
            mutate(predicted_value = groundwater + predicted_value) %>%
            mutate(lag_day = x)
          
          
          Time_series_data_2 <- rbind(Time_series_data_2, temp2_pred2)
          
          
        }
        

    }else if (unique(pgown_well_info_Well_info$Snow_influenced == 1)) {      # SNOW INFLUENCED ----------------------------------------------------------
      plot_type <- "snow"
      
      #run for predicted forcast intervals (i.e 14,30,60 and 90 days)
        for(x in forecast_days){



          
          temp2 <- temp %>%
            mutate(lag_day = x, #forcast interval
                   Date_predicted = lead(Date,x),
                   precipitation_lag = rollsum(total_precip, lag_period, fill = NA,align='right', na.rm = T), #recharge lagging precipitation
                   mean_temp_lag = rollmean(mean_temp, lag_period, fill = NA,align='right', na.rm = T), #recharge lagging temperature
                   groundwater_predict = lead(groundwater, x), #actual groundwater level we are predicting
                   mean_temp_lead = rollmean(mean_temp, x, fill = NA,align='left', na.rm = T), #forecasted precipitation
                   precip_lead = rollsum(total_precip, x, fill = NA,align='left', na.rm = T), #forecasted temperature
                   SWE_lag = lag(SWE, lag_period), #Snow level at rechrage lag period
                   SWE_lead = lead(SWE,x) #snow level at prediction date
            ) %>%
            mutate(
              SWE_lag_diff =  SWE - SWE_lag, #snowmelt that occured during lag period
              SWE_lead_diff =  SWE_lead - SWE,
              groundwater_diff = groundwater_predict - groundwater,
              actual_predicted_groundwater = groundwater_predict,
              year = year(Date_predicted),
              days_in_year = yday(Date_predicted)) %>%#snowmelt that occured during forecasted period
            dplyr::select( # only select variables of interest remove others
              Date,
              Date_predicted,
              Well,
              lag_day,
              days_in_year,
              year,
              groundwater, 
              groundwater_predict, 
              groundwater_diff,
              actual_predicted_groundwater,
              mean_temp_lag, 
              mean_temp_lead,  
              precipitation_lag, 
              precip_lead, 
              SWE,
              SWE_lag_diff,
              SWE_lead_diff#,
              #max_groundwater,
              #min_groundwater
            )
          
          
          
          
          calculate_weighted_lead <- function(i, x, lag_period, a_coeff) {

            temp %>%
              mutate(lag_day = x, #forcast interval
                     Date_predicted = lead(Date,x),
                     SWE_lag = lag(SWE, 1), #Snow level at rechrage lag period
                     SWE_lead = lead(SWE,1)
              ) %>%
              mutate(
                SWE_lag_diff =  SWE - SWE_lag, #snowmelt that occured during lag period
                SWE_lead_diff =  SWE_lead - SWE)%>%
              mutate(lag_day_adjusted = (x- lag_period))%>%
              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff))%>%
              mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff ))%>%
              mutate(normal_weight = (normal_weight - 0)/(weight_max-0))%>%
              mutate(weight_max2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow))%>%
              mutate(normal_weight2 = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow ))%>%
              mutate(normal_weight2 = (normal_weight2 - 0)/(weight_max2-0))%>%
              mutate(weighted_lead = lead(total_precip,i)*normal_weight,
                     weighted_lead_temp = lead(mean_temp,i)*normal_weight2,
                     weighted_lead_SWE = lead(SWE_lead_diff,i)*normal_weight2)%>%
              dplyr::select( # only select variables of interest remove others
                Date,
                Date_predicted,
                Well,
                lag_day,
                lag_day_adjusted,
                weighted_lead,
                weighted_lead_SWE,
                weighted_lead_temp)
            
            
          }
          
          lead_data <- map_dfr(1:x, calculate_weighted_lead, x = x, lag_period = lag_period, a_coeff = a_coeff)
          
          lead_data <-   lead_data %>%
            group_by(# only select variables of interest remove others
              Date,
              Date_predicted,
              Well,
              lag_day
            ) %>%
            summarise(weighted_lead = mean(weighted_lead, na.rm = TRUE),
                      weighted_lead_SWE = mean(weighted_lead_SWE, na.rm = TRUE),
                      weighted_lead_temp = mean(weighted_lead_temp, na.rm = TRUE))%>%
            ungroup() 
          
          
          
          
          lag_period_length = lag_period*3
          
          calculate_weighted_lag <- function(i, lag_period, a_coeff) {
            #i = 5
            
            temp %>%
              mutate(lag_day = x, #forcast interval
                     Date_predicted = lead(Date,x),
                     SWE_lag = lag(SWE, 1), #Snow level at rechrage lag period
                     SWE_lead = lead(SWE,1)
                                 ) %>%
              mutate(
                SWE_lag_diff =  SWE - SWE_lag, #snowmelt that occured during lag period
                SWE_lead_diff =  SWE_lead - SWE)%>%
              mutate(lag_day_adjusted = lag_period)%>%
              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff))%>%
              mutate(weight_max2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow))%>%
              mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff ))%>%
              mutate(normal_weight2 = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow ))%>%
              mutate(normal_weight = (normal_weight - 0)/(weight_max-0))%>%
              mutate(normal_weight2 = (normal_weight2 - 0)/(weight_max2-0))%>%
              mutate(weighted_lag = lag(total_precip,i)*normal_weight,
                     weighted_lag_temp = lag(mean_temp,i)*normal_weight2,
                     weighted_lag_SWE = lag(SWE_lead_diff,i)*normal_weight2)%>%
              dplyr::select( # only select variables of interest remove others
                Date,
                Date_predicted,
                Well,
                lag_day,
                lag_day_adjusted,
                weighted_lag,
                weighted_lag_SWE,
                weighted_lag_temp
              )
            
            
          }
          
          
          lag_data <- map_dfr(1:(lag_period * 3), calculate_weighted_lag, lag_period = lag_period, a_coeff = a_coeff)
          
          lag_data <-   lag_data %>%
            group_by(# only select variables of interest remove others
              Date,
              Date_predicted,
              Well,
              lag_day
            ) %>%
            summarise(weighted_lag = mean(weighted_lag, na.rm = TRUE),
                      weighted_lag_SWE = mean(weighted_lag_SWE, na.rm = TRUE),
                      weighted_lag_temp = mean(weighted_lag_temp, na.rm = TRUE)
            )%>%
            ungroup() 
          
          lead_data_lag <- full_join(lead_data, lag_data)
          
          temp2 <- full_join(temp2,lead_data_lag)
          
          temp2 <- temp2 %>%
            mutate(precip_lead_org = precip_lead,
                   precipitation_lag_org = precipitation_lag,
                   precip_lead = weighted_lead,
                   precipitation_lag = weighted_lag,
                   SWE_lead_diff_org = SWE_lead_diff,
                   SWE_lag_diff_org = SWE_lag_diff,
                   SWE_lead_diff = weighted_lead_SWE,
                   SWE_lag_diff = weighted_lag_SWE,
                   mean_temp_lead = weighted_lead_temp,
                   mean_temp_lag = weighted_lag_temp
            ) %>%
            mutate(max_precip_lead = max(precip_lead, na.rm = TRUE),
                   min_precip_lead = min(precip_lead, na.rm = TRUE),
                   max_precipitation_lag = max(precipitation_lag, na.rm = TRUE),
                   min_precipitation_lag = min(precipitation_lag, na.rm = TRUE),
                   max_SWE_lead_diff = max(SWE_lead_diff, na.rm = TRUE),
                   min_SWE_lead_diff = min(SWE_lead_diff, na.rm = TRUE),
                   max_SWE_lag_diff = max(SWE_lag_diff, na.rm = TRUE),
                   min_SWE_lag_diff = min(SWE_lag_diff, na.rm = TRUE),
                   max_mean_temp_lead = max(mean_temp_lead, na.rm = TRUE),
                   min_mean_temp_lead = min(mean_temp_lead, na.rm = TRUE),
                   max_mean_temp_lag = max(mean_temp_lag, na.rm = TRUE),
                   min_mean_temp_lag = min(mean_temp_lag, na.rm = TRUE),
                   max_groundwater = max(groundwater, na.rm = TRUE),
                   min_groundwater = min(groundwater, na.rm = TRUE),
                   max_groundwater_diff = max(groundwater_diff, na.rm = TRUE),
                   min_groundwater_diff = min(groundwater_diff, na.rm = TRUE)
            ) %>%
            mutate(precip_lead = (precip_lead - min_precip_lead)/(max_precip_lead-min_precip_lead),
                   precipitation_lag = (precipitation_lag - min_precipitation_lag)/(max_precipitation_lag-min_precipitation_lag),
                   SWE_lead_diff = (SWE_lead_diff - min_SWE_lead_diff)/(max_SWE_lead_diff-min_SWE_lead_diff),
                   SWE_lag_diff = (SWE_lag_diff - min_SWE_lag_diff)/(max_SWE_lag_diff-min_SWE_lag_diff),
                   mean_temp_lag = (mean_temp_lag - min_mean_temp_lag)/(max_mean_temp_lag-min_mean_temp_lag),
                   mean_temp_lead = (mean_temp_lead - min_mean_temp_lead)/(max_mean_temp_lead-min_mean_temp_lead),
                   groundwater = (groundwater - min_groundwater)/(max_groundwater-min_groundwater),
                   actual_predicted_groundwater = (actual_predicted_groundwater - min_groundwater)/(max_groundwater-min_groundwater), 
                   groundwater_predict =  (groundwater_predict - min_groundwater)/(max_groundwater-min_groundwater),
                   groundwater_diff =  (groundwater_diff - min_groundwater_diff)/(max_groundwater_diff-min_groundwater_diff))
          
          
          
          
          temp2_original <- temp2
          
        
        
        
        model_file <- paste0(model_path,"ANN_Model_",y,"_",x,".Rdata")
        load(model_file)
        

        
        # Calculate the mean and standard deviation of the variables by day
      
          
          
          daily_stats<- temp2_original %>%
            mutate(
              mean_temp_lead = mean_temp_lead*(max_mean_temp_lead - min_mean_temp_lead) + min_mean_temp_lead#,
            )%>%
            group_by(days_in_year) %>%
            summarise(
              precip_lead_mean = mean(precip_lead_org, na.rm = TRUE),
              precip_lead_sd = sd(precip_lead_org, na.rm = TRUE),
              mean_temp_lead_mean = mean(mean_temp_lead, na.rm = TRUE),
              mean_temp_lead_sd = sd(mean_temp_lead, na.rm = TRUE),
              SWE_lead_diff_mean = mean(SWE_lead_diff_org, na.rm = TRUE),
              SWE_lead_diff_sd = sd(SWE_lead_diff_org, na.rm = TRUE)
            )
          
          
          # Generate Monte Carlo values for each day
          monte_carlo_df <- daily_stats %>%
            tidyr::uncount(500) %>%
            mutate(
              precip_lead = rnorm(n(), mean = precip_lead_mean, sd = precip_lead_sd),
              mean_temp_lead = rnorm(n(), mean = mean_temp_lead_mean, sd = mean_temp_lead_sd),
              SWE_lead_diff = rnorm(n(), mean = SWE_lead_diff_mean, sd = SWE_lead_diff_sd)
              ) 
          
          monte_carlo_df <- monte_carlo_df %>%
            dplyr::select(days_in_year, precip_lead, mean_temp_lead,SWE_lead_diff) %>%
            mutate(precip_lead = precip_lead/x, 
                   mean_temp_lead = mean_temp_lead,
                   SWE_lead_diff = SWE_lead_diff/x) 
          
          
          last_date <- temp2_original%>%
          drop_na(groundwater)%>%
            summarise(last_measurements= max(Date, na.rm = TRUE)) %>%
            pull(last_measurements)
          

          last_date_days_in_year  <- yday(last_date+x)
          
          Prediction_date <- last_date+x
          
          monte_carlo_df <- monte_carlo_df %>%
            filter(days_in_year == last_date_days_in_year)%>%
            mutate(sim_num = row_number()) %>%
            mutate(joiner = 1) %>%
            dplyr::select(sim_num, joiner, precip_lead, mean_temp_lead,SWE_lead_diff)
          
          
          temp_forcast <- data.frame()
          
          for(p in 1:x){
            
            
            temp_fc <- data.frame(
              Well = unlist(y),
              Date = as.Date(last_date+p),
              Snow_influenced =  unique(temp$Snow_influenced),
              groundwater_period_average = unique(temp$groundwater_period_average)
            ) 
            
            
            
            temp_fc <- temp_fc %>%
              mutate(days_in_year = yday(Date+x))
            
            temp_forcast <- rbind(temp_forcast,temp_fc)
            
          }
          
          
          
          
          temp_forcast4 <- data.frame()

          for(g in 1:max_ensemble){

            
            ensemble_forecast_data_z <- ensemble_forecast_data_y %>%
              filter(en_sim == g)
            
            
            temp_forcast2 <- full_join(deterministic_forecast_data_y,ensemble_forecast_data_z)
            
            temp_forcast2_2 <- temp_forcast2 %>%
              dplyr::select(-en_sim) %>%
              mutate(days_in_year = yday(Date+x))
            
            temp_forcast3 <- full_join(temp_forcast,temp_forcast2_2 )
            
            
            
            temp_forcast3_2  <- temp_forcast3 %>%
              mutate(joiner = 1) %>%
              full_join(monte_carlo_df) %>%
              mutate(total_precip = ifelse(is.na(FC_type), precip_lead, total_precip),
                     mean_temp = ifelse(is.na(FC_type), mean_temp_lead, mean_temp)) %>%
              mutate(FC_type = ifelse(is.na(FC_type), 3, FC_type)) %>%
              group_by(Well, Date) %>%
              mutate(min_FC = min(FC_type))%>%
              filter(min_FC == FC_type)%>%
              ungroup()%>%
              mutate(lag_day_adjusted = (lag_period)) %>%
              group_by(sim_num) %>%
              arrange(Date) %>%
              mutate(lead_num = row_number()) %>%
              ungroup()%>%
              mutate(lag_day_adjusted = (x- lag_period))%>%
              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff))%>%
              mutate(normal_weight = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff ))%>%
              mutate(normal_weight = (normal_weight - 0)/(weight_max-0))%>%
              mutate(weight_max2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow))%>%
              mutate(normal_weight2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow ))%>%
              mutate(normal_weight2 = (normal_weight2 - 0)/(weight_max2-0))%>%
              mutate(total_precip = total_precip*normal_weight) %>%
                mutate(mean_temp = mean_temp*normal_weight2) %>%
                mutate(SWE_lead_diff = SWE_lead_diff*normal_weight2)%>%
              filter(Date >= last_date & Date <= Prediction_date) %>%
              group_by(sim_num, Well)%>%
              summarise(precip_lead = mean(total_precip, na.rm = TRUE),
                        mean_temp_lead = mean(mean_temp, na.rm = TRUE),
                        SWE_lead_diff = mean(SWE_lead_diff, na.rm = TRUE)) %>%
              mutate(en_sim = g) %>%
              ungroup() 
            
            
            
            temp_forcast4 <- rbind(temp_forcast4,temp_forcast3_2)
          }
          
          
          
          temp_forcast4 <- temp_forcast4 %>%
            mutate(joiner = 1)
          
          
          
          temp2_pred <- temp2 %>%
            filter(Date ==last_date)%>%
            dplyr::select(Date, Well,groundwater,groundwater_diff, precipitation_lag, mean_temp_lag,
                          max_groundwater, min_groundwater, max_groundwater_diff, min_groundwater_diff, 
                          min_mean_temp_lead, max_mean_temp_lead, max_precip_lead, min_precip_lead, 
                          SWE,SWE_lag_diff,max_SWE_lead_diff,min_SWE_lead_diff) %>%
            mutate(joiner = 1) %>%
            mutate(Date_predicted = Date+x) %>%
            full_join(temp_forcast4) %>%
            mutate(SWE_lead_diff = ifelse(SWE_lead_diff < 0,ifelse(SWE_lead_diff <= -SWE, -SWE, SWE_lead_diff),SWE_lead_diff)) %>%
            mutate(precip_lead = (precip_lead - min_precip_lead)/(max_precip_lead-min_precip_lead),
                   mean_temp_lead = (mean_temp_lead - min_mean_temp_lead)/(max_mean_temp_lead-min_mean_temp_lead),
                   SWE_lead_diff = (SWE_lead_diff - min_SWE_lead_diff)/(max_SWE_lead_diff-min_SWE_lead_diff)
            )
          
          temp2_test <- temp2 %>%
            filter(days_in_year ==last_date_days_in_year )

          
      
          
          predicted_response_ANN <- predict(fit_ann, newdata = temp2_pred)
          temp2_pred <- cbind(temp2_pred, predicted_response_ANN)
          
          
          
          
          #Combine data
          temp2_pred <- temp2_pred %>%
            dplyr::rename(ANN = predicted_response_ANN
            ) %>%
            gather(c(
                     ANN), key = "Model", value = predicted_value)
          
          temp2_pred <- temp2_pred %>%
            mutate(predicted_value = predicted_value*(max_groundwater_diff - min_groundwater_diff) + min_groundwater_diff,
                   groundwater = groundwater*(max_groundwater - min_groundwater) + min_groundwater)%>%
            mutate(predicted_value = groundwater + predicted_value) %>%
            mutate(lag_day = x)
          
          
          Time_series_data_2 <- rbind(Time_series_data_2, temp2_pred)
          
        
      }
      
    }
    
    # PLOTTING -----------------------------------------------------------------
    
     
  
    #gather model predictions for plot

  Time_series_data_2_plot <- Time_series_data_2 %>%
    group_by(Well, Date,Date_predicted,lag_day,Model, groundwater) %>%
    summarise(predicted_value_mean = mean(predicted_value, na.rm = TRUE),
              predicted_value_min = min(predicted_value, na.rm = TRUE),
              predicted_value_max = max(predicted_value, na.rm = TRUE),
              predicted_value_25th =  quantile(predicted_value, 0.25, na.rm = TRUE),
              predicted_value_75th = quantile(predicted_value, 0.75, na.rm = TRUE),
              predicted_value_50th = quantile(predicted_value, 0.50, na.rm = TRUE),
              predicted_value_90th = quantile(predicted_value, 0.90, na.rm = TRUE),
              predicted_value_10th = quantile(predicted_value, 0.10, na.rm = TRUE),
              predicted_value_5th = quantile(predicted_value, 0.05, na.rm = TRUE),
              predicted_value_95th = quantile(predicted_value, 0.95, na.rm = TRUE)
    ) %>%
    ungroup()


    #Entire data set to plot actual levels 
      temp <- temp %>%
        mutate(days_in_year = yday(Date))
      
      start_day = Sys.Date() - 365 + max(forecast_days) + 7
      end_day = Sys.Date() + max(forecast_days) + 7
      
      start_year = year(start_day)
      end_year = year(end_day)
      
     # filter historical data percentiles to just past forecast date
      
      if(start_year == end_year){
        
        temp_WL_states_temp_plot <- temp_WL_states_temp %>%
          mutate(month = month(fake_date), day = day(fake_date)) %>%
          mutate(Date = make_date(year = start_year, month =month, day = day))
        
      }else{
        
        temp_WL_states_temp1 <- temp_WL_states_temp %>%
          mutate(month = month(fake_date), day = day(fake_date))%>%
          mutate(Date = make_date(year = start_year, month =month, day = day))
        
        temp_WL_states_temp2 <- temp_WL_states_temp %>%
          mutate(month = month(fake_date), day = day(fake_date))%>%
          mutate(Date = make_date(year = end_year, month =month, day = day))
        
        temp_WL_states_temp_plot <- rbind(temp_WL_states_temp1,temp_WL_states_temp2)
        
      }
      
      temp_WL_states_temp_plot <- temp_WL_states_temp_plot %>%
        drop_na(Date)

  
      snow_influenced  <- if (unique(pgown_well_info_Well_info$Snow_influenced == 0)) {
       
        
        #climate graphs. 
        temp_historical_conditions <- temp %>%
          mutate(precipitation_lag = rollsum(total_precip, 30, fill = NA,align='right', na.rm = T), #recharge lagging precipitation
                 mean_temp_lag = rollmean(mean_temp, 30, fill = NA,align='right', na.rm = T)
          ) %>%
          mutate(days_in_year = yday(Date)) %>%
          dplyr::select(Date,days_in_year, precipitation_lag,mean_temp_lag )%>%
          gather(c(precipitation_lag, mean_temp_lag), key = "variable",value = "Value" )
        
        #histoircal stats
        
        
        
        deterministic_forecast_data_temp<- full_join(temp, deterministic_forecast_data_y)
        
        
        deterministic_forecast_data_temp <- deterministic_forecast_data_temp %>%
          mutate(precipitation_lag = rollsum(total_precip, 30, fill = NA,align='right', na.rm = T), #recharge lagging precipitation
                 mean_temp_lag = rollmean(mean_temp, 30, fill = NA,align='right', na.rm = T)
          ) %>%
          mutate(days_in_year = yday(Date)) %>%
          dplyr::select(Date,days_in_year, precipitation_lag,mean_temp_lag , FC_type )%>%
          gather(c(precipitation_lag, mean_temp_lag), key = "variable",value = "Value" ) %>%
          drop_na(FC_type)%>%
          mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)", 
                                   ifelse(variable == "precipitation_lag", "Precip (30 day)",NA)))
        
        deterministic_forecast_data_temp_max <- max(deterministic_forecast_data_temp$Date)
        
        ensemble_forecast_data_y_temp <- data.frame()
        
        
        
        for(g in 1:max_ensemble){

          
          ensemble_forecast_data_z <- ensemble_forecast_data_y %>%
            filter(en_sim == g)
          
          ensemble_forecast_data_z<- full_join(temp, ensemble_forecast_data_z)
          
          
          ensemble_forecast_data_z <- ensemble_forecast_data_z %>%
            mutate(precipitation_lag = rollsum(total_precip, 30, fill = NA,align='right', na.rm = T), #recharge lagging precipitation
                   mean_temp_lag = rollmean(mean_temp, 30, fill = NA,align='right', na.rm = T)
            ) %>%
            mutate(days_in_year = yday(Date)) %>%
            dplyr::select(Date,days_in_year, precipitation_lag,mean_temp_lag, FC_type, en_sim )%>%
            gather(c(precipitation_lag, mean_temp_lag), key = "variable",value = "Value" ) %>%
            drop_na(FC_type)%>%
            mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)", 
                                     ifelse(variable == "precipitation_lag", "Precip (30 day)", NA)))
          
          
          
          ensemble_forecast_data_y_temp <- rbind(ensemble_forecast_data_y_temp,ensemble_forecast_data_z)
        }
        
        
        ensemble_forecast_data_y_temp <- ensemble_forecast_data_y_temp %>%
          filter(Date >=deterministic_forecast_data_temp_max )
        
        
        
        temp_historical_conditions_stats <- temp_historical_conditions %>%
          mutate(month= month(Date), year = year(Date), day = day(Date))%>%
          group_by(month, day, variable) %>%
          summarise(predicted_value_mean = mean(Value, na.rm = TRUE),
                    predicted_value_min = min(Value, na.rm = TRUE),
                    predicted_value_max = max(Value, na.rm = TRUE),
                    predicted_value_25th =  quantile(Value, 0.25, na.rm = TRUE),
                    predicted_value_75th = quantile(Value, 0.75, na.rm = TRUE),
                    predicted_value_50th = quantile(Value, 0.50, na.rm = TRUE),
                    predicted_value_90th = quantile(Value, 0.90, na.rm = TRUE),
                    predicted_value_10th = quantile(Value, 0.10, na.rm = TRUE),
                    predicted_value_5th = quantile(Value, 0.05, na.rm = TRUE),
                    predicted_value_95th = quantile(Value, 0.95, na.rm = TRUE)
          ) %>%
          ungroup() 
        
        
        if(start_year == end_year){
          
          temp_historical_conditions_stats_plot <- temp_historical_conditions_stats %>%
            mutate(Date = make_date(year = start_year, month =month, day = day))
          
        }else{
          
          temp_historical_conditions_stats_temp1 <- temp_historical_conditions_stats %>%
            mutate(Date = make_date(year = start_year, month =month, day = day))
          
          temp_historical_conditions_stats_temp2 <- temp_historical_conditions_stats %>%
            mutate(Date = make_date(year = end_year, month =month, day = day))
          
          temp_historical_conditions_stats_plot <- rbind(temp_historical_conditions_stats_temp1,temp_historical_conditions_stats_temp2)
          
        }
        
        temp_historical_conditions_stats_plot <- temp_historical_conditions_stats_plot %>%
          drop_na(Date) %>%
          mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)", 
                                   ifelse(variable == "precipitation_lag", "Precip (30 day)", NA)))
        
        
        
        temp_historical_conditions <- temp_historical_conditions %>%
          mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)", 
                                   ifelse(variable == "precipitation_lag", "Precip (30 day)", NA)))
        
        
        gglayers <- list(
          #scale_y_continuous(sec.axis = sec_axis( trans=~.-temp2_toc, name="Depth below ground surface (m)")),
          theme_bw(),
          ylab("Degrees/mm"),
          xlab(""),
          scale_x_date(date_labels = ("%b"), date_breaks = "1 month", limits = c(start_day, end_day)),
          theme(#legend.title = element_blank(),
            legend.position = "right",
            legend.background = element_blank(),
            axis.text.x = element_text(angle = 30, hjust =1),
            legend.spacing.y = unit(0.1, 'mm'),
            legend.margin = ggplot2::margin(t = 0.25, b = 0.25)))
        
        
        
        
        temp_graph2 <- ggplot() +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_5th, ymin = predicted_value_min, fill = "1 Min - Q5"), size = 1) +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_10th, ymin = predicted_value_5th, fill = "2 Q5 - Q10"), size = 1) +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_25th, ymin = predicted_value_10th, fill = "3 Q10 - Q25"), size = 1) +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_75th, ymin = predicted_value_25th, fill = "4 Q25 - Q75"), size = 1) +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_90th, ymin = predicted_value_75th, fill = "5 Q75 - Q90"), size = 1) +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_95th, ymin = predicted_value_90th, fill = "6 Q90 - Q95"), size = 1) +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_max, ymin = predicted_value_95th, fill = "7 Q95 - Max"), size = 1) +
          scale_fill_brewer(name = "Historical Data (2004-2023)", palette = "Spectral", direction = 1) +
          scale_alpha_manual(name = "Historical Data (2004-2023)", values = c(1, 1, 1, 1, 1, 1, 1, 1)) +
          gglayers +
          new_scale_colour() +
          new_scale_fill() +
          geom_line(data = temp_historical_conditions, aes(x = Date, y = Value, colour = "Recorded GW Level"), linewidth = 1) +
          geom_line(data = deterministic_forecast_data_temp, aes(x = Date, y = Value, colour = "2"), linetype = 1,linewidth = 1) +
          geom_line(data = ensemble_forecast_data_y_temp, aes(x = Date, y = Value, colour = "3", group = en_sim), linetype = 1, linewidth = 1) +
          scale_colour_manual(name = "", values = c("black","red","orange")) +
          theme(legend.position = "none") +  # Remove legend
          facet_grid(variable ~ ., scale = "free_y")
        temp_graph2
        
        
        
        
        
      }else if (unique(pgown_well_info_Well_info$Snow_influenced == 1)) {      # SNOW INFLUENCED ----------------------------------------------------------
        
        #climate graphs. 
        temp_historical_conditions <- temp %>%
          mutate(precipitation_lag = rollsum(total_precip, 30, fill = NA,align='right', na.rm = T), #recharge lagging precipitation
                 mean_temp_lag = rollmean(mean_temp, 30, fill = NA,align='right', na.rm = T)
          ) %>%
          mutate(days_in_year = yday(Date)) %>%
          dplyr::select(Date,days_in_year, precipitation_lag,mean_temp_lag ,SWE )%>%
          gather(c(precipitation_lag, mean_temp_lag, SWE), key = "variable",value = "Value" )
        
        #histoircal stats
        
        
        deterministic_forecast_data_temp<- full_join(temp, deterministic_forecast_data_y)
        
        
        deterministic_forecast_data_temp <- deterministic_forecast_data_temp %>%
          mutate(precipitation_lag = rollsum(total_precip, 30, fill = NA,align='right', na.rm = T), #recharge lagging precipitation
                 mean_temp_lag = rollmean(mean_temp, 30, fill = NA,align='right', na.rm = T)
          ) %>%
          mutate(days_in_year = yday(Date)) %>%
          dplyr::select(Date,days_in_year, precipitation_lag,mean_temp_lag ,SWE, FC_type )%>%
          gather(c(precipitation_lag, mean_temp_lag, SWE), key = "variable",value = "Value" ) %>%
          drop_na(FC_type)%>%
          mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)", 
                                                   ifelse(variable == "precipitation_lag", "Precip (30 day)",
                                                          ifelse(variable == "SWE", "SWE", NA))))
        deterministic_forecast_data_temp_max <- max(deterministic_forecast_data_temp$Date)
        
        ensemble_forecast_data_y_temp <- data.frame()
          
        
        
        for(g in 1:max_ensemble){
          # g = 2
          
          
          ensemble_forecast_data_z <- ensemble_forecast_data_y %>%
            filter(en_sim == g)
          
          ensemble_forecast_data_z<- full_join(temp, ensemble_forecast_data_z)
          
          
          ensemble_forecast_data_z <- ensemble_forecast_data_z %>%
            mutate(precipitation_lag = rollsum(total_precip, 30, fill = NA,align='right', na.rm = T), #recharge lagging precipitation
                   mean_temp_lag = rollmean(mean_temp, 30, fill = NA,align='right', na.rm = T)
            ) %>%
            mutate(days_in_year = yday(Date)) %>%
            dplyr::select(Date,days_in_year, precipitation_lag,mean_temp_lag ,SWE, FC_type, en_sim )%>%
            gather(c(precipitation_lag, mean_temp_lag, SWE), key = "variable",value = "Value" ) %>%
            drop_na(FC_type)%>%
            mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)", 
                                     ifelse(variable == "precipitation_lag", "Precip (30 day)",
                                            ifelse(variable == "SWE", "SWE", NA))))
          
          
          
          ensemble_forecast_data_y_temp <- rbind(ensemble_forecast_data_y_temp,ensemble_forecast_data_z)
        }
        
        
        ensemble_forecast_data_y_temp <- ensemble_forecast_data_y_temp %>%
          filter(Date >=deterministic_forecast_data_temp_max )
        
        
        
        temp_historical_conditions_stats <- temp_historical_conditions %>%
          mutate(month= month(Date), year = year(Date), day = day(Date))%>%
          group_by(month, day, variable) %>%
          summarise(predicted_value_mean = mean(Value, na.rm = TRUE),
                    predicted_value_min = min(Value, na.rm = TRUE),
                    predicted_value_max = max(Value, na.rm = TRUE),
                    predicted_value_25th =  quantile(Value, 0.25, na.rm = TRUE),
                    predicted_value_75th = quantile(Value, 0.75, na.rm = TRUE),
                    predicted_value_50th = quantile(Value, 0.50, na.rm = TRUE),
                    predicted_value_90th = quantile(Value, 0.90, na.rm = TRUE),
                    predicted_value_10th = quantile(Value, 0.10, na.rm = TRUE),
                    predicted_value_5th = quantile(Value, 0.05, na.rm = TRUE),
                    predicted_value_95th = quantile(Value, 0.95, na.rm = TRUE)
          ) %>%
          ungroup() 
        
        
        if(start_year == end_year){
          
          temp_historical_conditions_stats_plot <- temp_historical_conditions_stats %>%
            mutate(Date = make_date(year = start_year, month =month, day = day))
          
        }else{
          
          temp_historical_conditions_stats_temp1 <- temp_historical_conditions_stats %>%
            mutate(Date = make_date(year = start_year, month =month, day = day))
          
          temp_historical_conditions_stats_temp2 <- temp_historical_conditions_stats %>%
            mutate(Date = make_date(year = end_year, month =month, day = day))
          
          temp_historical_conditions_stats_plot <- rbind(temp_historical_conditions_stats_temp1,temp_historical_conditions_stats_temp2)
          
        }
        
        temp_historical_conditions_stats_plot <- temp_historical_conditions_stats_plot %>%
          drop_na(Date) %>%
          mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)", 
                                            ifelse(variable == "precipitation_lag", "Precip (30 day)",
                                                   ifelse(variable == "SWE", "SWE", NA))))
        
        
        
        temp_historical_conditions <- temp_historical_conditions %>%
          mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)", 
                                   ifelse(variable == "precipitation_lag", "Precip (30 day)",
                                          ifelse(variable == "SWE", "SWE", NA))))
        
        
        
        gglayers <- list(
          #scale_y_continuous(sec.axis = sec_axis( trans=~.-temp2_toc, name="Depth below ground surface (m)")),
          theme_bw(),
          ylab("Degrees C/mm/mm"),
          xlab(""),
          scale_x_date(date_labels = ("%b"), date_breaks = "1 month", limits = c(start_day, end_day)),
          theme(#legend.title = element_blank(),
            legend.position = "right",
            legend.background = element_blank(),
            axis.text.x = element_text(angle = 30, hjust =1),
            legend.spacing.y = unit(0.1, 'mm'),
            legend.margin = ggplot2::margin(t = 0.25, b = 0.25)))
        
        
        #deterministic_forecast_data_temp
        #ensemble_forecast_data_y_temp
        
        temp_graph2 <- ggplot() +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_5th, ymin = predicted_value_min, fill = "1 Min - Q5"), size = 1) +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_10th, ymin = predicted_value_5th, fill = "2 Q5 - Q10"), size = 1) +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_25th, ymin = predicted_value_10th, fill = "3 Q10 - Q25"), size = 1) +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_75th, ymin = predicted_value_25th, fill = "4 Q25 - Q75"), size = 1) +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_90th, ymin = predicted_value_75th, fill = "5 Q75 - Q90"), size = 1) +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_95th, ymin = predicted_value_90th, fill = "6 Q90 - Q95"), size = 1) +
          geom_ribbon(data = temp_historical_conditions_stats_plot,alpha = 0.5,  aes(x = Date, ymax = predicted_value_max, ymin = predicted_value_95th, fill = "7 Q95 - Max"), size = 1) +
          scale_fill_brewer(name = "Historical Data (2004-2023)", palette = "Spectral", direction = 1) +
          scale_alpha_manual(name = "Historical Data (2004-2023)", values = c(1, 1, 1, 1, 1, 1, 1, 1)) +
          gglayers +
          new_scale_colour() +
          new_scale_fill() +
          geom_line(data = temp_historical_conditions, aes(x = Date, y = Value, colour = "1"), linewidth = 1) +
          geom_line(data = deterministic_forecast_data_temp, aes(x = Date, y = Value, colour = "2"), linetype = 1,linewidth = 1) +
          geom_line(data = ensemble_forecast_data_y_temp, aes(x = Date, y = Value, colour = "3", group = en_sim), linetype = 1, linewidth = 1) +
          scale_colour_manual(name = "", values = c("black","red","orange")) +
          theme(legend.position = "none") +  # Remove legend
          facet_grid(variable ~ ., scale = "free_y")
        temp_graph2
        
        
        
      }
      
      
      
      
      deterministic_forecast_data_y_last_date<- max(deterministic_forecast_data_y$Date)
      
      ensemble_forecast_data_y_last_date <- max(ensemble_forecast_data_y$Date)
      
      Time_series_data_2_plot <- left_join(Time_series_data_2_plot, well_lag_time_2)
      
    
      Time_series_data_2_plot$Model <- factor(Time_series_data_2_plot$Model, levels = c("Good, Results of Value", "Fair, Results of Value", "Fair, Results of Limited Value"))
      
      
      
      
      # Define custom color scale for performance categories
      performance_colors <- c('Good, Results of Value' = 'blue',
                              'Good, Results of Limited Value' = "green",
                              'Fair, Results of Value' = 'orange', 
                              'Fair, Results of Limited Value' = 'red')
      
      gglayers <- list(
        theme_bw(),
        ylab("Water Level Below Ground (m)"),
        xlab(""),
        scale_x_date(date_labels = ("%b"), date_breaks = "1 month", limits = c(start_day, end_day)),
        theme(
          legend.position = "right",
          legend.background = element_blank(),
          axis.text.x = element_text(angle = 30, hjust = 1),
          legend.spacing.y = unit(0.1, 'mm'),
          legend.margin = ggplot2::margin(t = 0.25, b = 0.25)
        )
      )
      
      # Create the first plot (temp_graph) without the legend
      temp_graph <- ggplot() +
        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per5th, ymin = Waterlevel_adjustments_temp - Min, fill = "1 Min - Q5", alpha = "1 Min - Q5"), size = 1) +
        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per10th, ymin = Waterlevel_adjustments_temp - per5th, fill = "2 Q5 - Q10", alpha = "2 Q5 - Q10"), size = 1) +
        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per25th, ymin = Waterlevel_adjustments_temp - per10th, fill = "3 Q10 - Q25", alpha = "3 Q10 - Q25"), size = 1) +
        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per75th, ymin = Waterlevel_adjustments_temp - per25th, fill = "4 Q25 - Q75", alpha = "4 Q25 - Q75"), size = 1) +
        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per90th, ymin = Waterlevel_adjustments_temp - per75th, fill = "5 Q75 - Q90", alpha = "5 Q75 - Q90"), size = 1) +
        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per95th, ymin = Waterlevel_adjustments_temp - per90th, fill = "6 Q90 - Q95", alpha = "6 Q90 - Q95"), size = 1) +
        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - Max, ymin = Waterlevel_adjustments_temp - per95th, fill = "7 Q95 - Max", alpha = "7 Q95 - Max"), size = 1) +
        scale_fill_brewer(name = "Historical Data (2004-2023)", palette = "Spectral", direction = 1) +
        scale_alpha_manual(name = "Historical Data (2004-2023)", values = c(1, 1, 1, 1, 1, 1, 1, 1)) +
        gglayers +
        new_scale_colour() +
        geom_line(data = temp, aes(x = Date, y = Waterlevel_adjustments_temp - groundwater, colour = "Recorded GW Level"), linewidth = 1) +
        scale_colour_manual(name = "", values = c("black")) +
        new_scale_colour() +
        geom_crossbar(data = Time_series_data_2_plot, aes(x = Date_predicted, y = Waterlevel_adjustments_temp - predicted_value_50th, ymin = Waterlevel_adjustments_temp - predicted_value_25th, ymax = Waterlevel_adjustments_temp - predicted_value_75th, colour = performance), alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
        geom_errorbar(data = Time_series_data_2_plot, aes(x = Date_predicted, ymin = Waterlevel_adjustments_temp - predicted_value_5th, ymax = Waterlevel_adjustments_temp - predicted_value_95th, colour = performance), alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
        scale_colour_manual(name = "Performance", values = performance_colors) +
        new_scale_colour() +
        geom_vline(aes(xintercept = deterministic_forecast_data_y_last_date, colour = "Deterministic Forecast"), linetype = 2) +
        geom_vline(aes(xintercept = ensemble_forecast_data_y_last_date, colour = "Ensemble Forecast"), linetype = 3) +
        scale_colour_manual(name = "Climate Forecast Range", values = c("dark green", "dark blue", "steelblue2", "yellow")) +
        guides(
          fill = guide_legend(order = 1, title = "Historical Data"),
          alpha = guide_legend(order = 1, title = "Historical Data"),
          colour = guide_legend(order = 2, title = "Recorded & Predicted Levels")
        ) +
        theme(legend.position = "none") +  # Remove legend
        facet_grid(Well ~ ., scale = "free_y")+
        scale_y_reverse()
      
      temp_graph
      # Create the second plot (temp_graph2) without the legend
      
      legend_plot <- cowplot::get_legend(
        ggplot() +
          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per5th, ymin = Waterlevel_adjustments_temp - Min, fill = "1 Min - Q5"), size = 1) +
          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per10th, ymin = Waterlevel_adjustments_temp - per5th, fill = "2 Q5 - Q10"), size = 1) +
          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per25th, ymin = Waterlevel_adjustments_temp - per10th, fill = "3 Q10 - Q25"), size = 1) +
          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per75th, ymin = Waterlevel_adjustments_temp - per25th, fill = "4 Q25 - Q75"), size = 1) +
          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per90th, ymin = Waterlevel_adjustments_temp - per75th, fill = "5 Q75 - Q90"), size = 1) +
          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per95th, ymin = Waterlevel_adjustments_temp - per90th, fill = "6 Q90 - Q95"), size = 1) +
          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - Max, ymin = Waterlevel_adjustments_temp - per95th, fill = "7 Q95 - Max"), size = 1) +
          scale_fill_brewer(name = "Historical Data", palette = "Spectral", direction = 1) +
          geom_line(data = temp, aes(x = Date, y = Waterlevel_adjustments_temp - groundwater, colour = "1) Recorded GW Level/ \n Recorded Climate"), linewidth = 1) +
          geom_line(data = deterministic_forecast_data_temp, aes(x = Date, y = Value, colour = "2) Deterministic Forecast"), linetype = 1, linewidth = 1) +
          geom_line(data = ensemble_forecast_data_y_temp, aes(x = Date, y = Value, colour = "3) Ensemble Forecast", group = en_sim), linetype = 1, linewidth = 1) +
          scale_colour_manual(name = "", values = c("black", "red", "orange")) +
          new_scale_color() +
          geom_crossbar(data = Time_series_data_2_plot, aes(x = Date_predicted, y = Waterlevel_adjustments_temp - predicted_value_50th, ymin = Waterlevel_adjustments_temp - predicted_value_25th, ymax = Waterlevel_adjustments_temp - predicted_value_75th, colour = performance), alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
          geom_errorbar(data = Time_series_data_2_plot, aes(x = Date_predicted, ymin = Waterlevel_adjustments_temp - predicted_value_5th, ymax = Waterlevel_adjustments_temp - predicted_value_95th, colour = performance), alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
          scale_colour_manual(name = "Performance", values = performance_colors) +
          new_scale_color() +
          geom_vline(aes(xintercept = deterministic_forecast_data_y_last_date, colour = "Deterministic Forecast"), linetype = 2) +
          geom_vline(aes(xintercept = ensemble_forecast_data_y_last_date, colour = "Ensemble Forecast"), linetype = 3) +
          scale_colour_manual(name = "Climate Forecast Range", values = c("dark green", "dark blue", "steelblue2", "yellow")) +
          theme(
            legend.position = "right",
            legend.key.size = unit(0.5, "lines"),
            legend.box = "vertical",
            legend.title = element_text(size = 9)  # Adjust the size as needed
          )
      )
      
      plot(legend_plot)
      
      #
      
      # aquifer info, aquifer number, aquifer subtype.
      
      #disclaimer.
      boxplot_description <- paste("Recorded & \n Predicted Levels \n 95th, 75th, 50th,\n 25th, 5th")
      
      Aquifer_id <- paste("Aquifer ID =",pgown_well_info_Well_info$aquifer_id)
      subtype <- paste("Aquifer subtype = \n",  pgown_well_info_Well_info$subtype)
      Climate_station_id <- paste("Climate Station =", pgown_well_info_Well_info$Climate_station_Id)
      snow_station <- paste("Snow Station =",pgown_well_info_Well_info$snow_stn)
      
      above_normal_definition <- "1) Above Normal - above 75th percentile"
      normal_definition <- "2) Normal - 25th to 75th percentile"
      below_normal_definition <- "3) Below Normal - below 25th percentile"

      disclaimer <-"Groundwater level forecasts use statistical models and"
      disclaimer1 <-"third-party data. These models have two types of "
      disclaimer2 <-"errors: systematic (model limitations) and random " 
      disclaimer3 <-" (input data) Forecasts may differ from actual observations,"
      disclaimer4 <-"and water levels could exceed forecast bounds. Users must"
      disclaimer5 <-"accept responsibility for their use and interpretation" 
    
      
      
     snow_influenced  <- if (unique(pgown_well_info_Well_info$Snow_influenced == 0)) {
       text_annotation <- ggpubr::text_grob(
         paste(
           boxplot_description,
           "",
           "Information",
           Aquifer_id,
           subtype,
           Climate_station_id,
           "",
           "Disclaimer",
           disclaimer,
           disclaimer1,
           disclaimer2,
           disclaimer3,
           disclaimer4,
           disclaimer5,
           sep = "\n"
         ),
         x = 0, hjust = 0, face = "italic", size = 8
       )
                            }else if (unique(pgown_well_info_Well_info$Snow_influenced == 1)) {      # SNOW INFLUENCED ----------------------------------------------------------
                            
                              text_annotation <- ggpubr::text_grob(
                                paste(
                                  boxplot_description,
                                  "",
                                  "Information",
                                  Aquifer_id,
                                  subtype,
                                  Climate_station_id,
                                  snow_station,
                                  "",
                                  "Disclaimer",
                                  disclaimer,
                                  disclaimer1,
                                  disclaimer2,
                                  disclaimer3,
                                  disclaimer4,
                                  disclaimer5,
                                  sep = "\n"
                                ),
                                x = 0, hjust = 0, face = "italic", size = 8
                              ) 
                              }
  
  
  
      # Add the specified text below the table
     
      
      text_annotation2 <- ggpubr::text_grob(
        paste(
          "",
          "",
          "Groundwater Conditions",
          above_normal_definition,
          normal_definition,
          below_normal_definition,
          "",
          "",
          sep = "\n"
        ),
        x = 0, hjust = 0, face = "italic", size = 8
      )
      
      Probabilities <- Time_series_data_2 %>%
        mutate(days_in_year = yday(Date_predicted))
      
      
      
      
      Probabilities <- left_join(Probabilities, temp_WL_states_temp)
      
      calculate_likelihood <- function(data, condition) {
        data %>%
          mutate(count = 1) %>%
          group_by(Well,Date_predicted, lag_day ,Model) %>%
          mutate(total = sum(count)) %>%
          summarise(count = sum(if_else({{condition}}, count, 0)), total = mean(total)) %>%
          ungroup() %>%
          mutate(likelihood = if_else(total == 0, NA_real_, count/total))
      }
      
      # Use the function to calculate likelihoods
      Probabilities_above_normal <- calculate_likelihood(Probabilities, predicted_value >= per75th)
      Probabilities_normal <- calculate_likelihood(Probabilities, predicted_value <= per75th & predicted_value >= per25th)
      Probabilities_below_normal <- calculate_likelihood(Probabilities, predicted_value <= per25th)

      # Combine the data frames
      Probabilities_combined <- bind_rows(
        mutate(Probabilities_above_normal, category = "1) Above Normal"),
        mutate(Probabilities_normal, category = "2) Normal"),
        mutate(Probabilities_below_normal, category = "3) Below Normal")
      )
      
      
      # Calculate ensemble likelihoods
  
      
      
      Probabilities_combined <- Probabilities_combined %>%
        mutate(likelihood = likelihood*100)

      
      # Probabilities_combined <- Probabilities_combined %>%
      #   mutate(likelihood2 = pmin(round(likelihood / 5) * 5, 95))
      Probabilities_combined <- Probabilities_combined %>%
        filter(Model == "ANN")
      
      Probabilities_combined_liklihood <- Probabilities_combined %>%
        arrange(Date_predicted)%>%
        mutate(table_name = paste0(Date_predicted," (",lag_day," days; ",Model,")"))%>%
        dplyr::select(category, table_name, likelihood)%>%
        mutate(likelihood = case_when(likelihood < 5 ~ "<5",
                                      likelihood > 95 ~ ">95",
                                      TRUE ~ as.character(round(likelihood, 0)))) %>%
        spread(table_name,likelihood) 
      
      Probabilities_combined_lag_day <- Probabilities_combined %>%
        arrange(Date_predicted)%>%
        mutate(table_name = paste0(Date_predicted," (",lag_day," days; ",Model,")"))%>%
        mutate(lag_day = paste0(lag_day," days"))%>%
        dplyr::select(category,table_name, lag_day)%>%
        mutate(category = "")%>%
        distinct(category,table_name, lag_day)%>%
        spread(table_name,lag_day) 
      
      Probabilities_combined_Date_predicted <- Probabilities_combined %>%
        arrange(Date_predicted)%>%
        mutate(table_name = paste0(Date_predicted," (",lag_day," days; ",Model,")"))%>%
        dplyr::select(category,table_name, Date_predicted)%>%
        mutate(category = "Groundwater",
               Date_predicted = as.character(Date_predicted))%>%
        distinct(category,table_name, Date_predicted)%>%
        spread(table_name,Date_predicted) 
      
      Probabilities_combined_model <- Probabilities_combined %>%
        arrange(Date_predicted)%>%
        mutate(table_name = paste0(Date_predicted," (",lag_day," days; ",Model,")"))%>%
        dplyr::select(category,table_name, Model)%>%
        mutate(category = "Conditions")%>%
        distinct(category,table_name, Model)%>%
        spread(table_name,Model) 
      
      
      Probabilities_combined2 <- rbind(Probabilities_combined_lag_day,
                                      Probabilities_combined_Date_predicted, #,Probabilities_combined_model,
                                      Probabilities_combined_liklihood)
      
      # Set the row names

      # Remove the category column
     
      table_grid <- gridExtra::grid.table(Probabilities_combined2)
      
      #Customize the table theme
      
      mytheme <- gridExtra::ttheme_default(
        core = list(fg_params=list(cex = 0.7)),
        colhead = list(fg_params=list(cex = 0.7)),
        rowhead = list(fg_params=list(cex = 0.7)),
        colhead=list(fg_params = list(parse=TRUE)))
      temp_chart <- gridExtra::tableGrob(Probabilities_combined2, theme = mytheme, rows = NULL, cols = NULL)
      

      
      temp_chart$widths <- unit(c(2, rep(1, ncol(temp_chart)-1)), "null")
      temp_chart$heights <- unit(c(1.25, rep(1, nrow(temp_chart)-1)), "null")
      
      temp_chart_gg <- ggplot() +
        annotation_custom(temp_chart) 
      
      

  
      
      #Annotate the ggplot table
      
      temp_chart_gg <- ggpubr::annotate_figure(temp_chart_gg,
                                       top = ggpubr::text_grob(" "),
                                       fig.lab = "Table 1: Likelihood of Groundwater Conditions (%)", 
                                       fig.lab.pos = "top.left")
   
      
      # Arrange the plots and the table in a grid with cowplot
      temp_figure <- cowplot::plot_grid(
        cowplot::plot_grid(temp_graph, temp_graph2, ncol = 1, align = "v", axis = "lr", rel_heights = c(1, 0.75)),
        cowplot::plot_grid(
          cowplot::plot_grid(legend_plot, text_annotation, ncol = 2, rel_widths = c(0.5, 1)),
          temp_chart_gg,text_annotation2,
          ncol = 1,
          rel_heights = c(1, 0.5, 0.3)
        ),
        ncol = 2,
        rel_widths = c(1.5, 1)
      )
      
      
      
      
      #Arrange the plot and the table in a grid
      #temp_figure <- ggpubr::ggarrange(temp_graph, temp_chart_gg, ncol = 2, widths = c(3, 1))
      
      
      
      location <- pgown_well_info %>%
        filter(Well == y)%>%
        pull(Location)
      
      recharge_type <- ifelse(plot_type == "snow", "snowmelt dominated environment", "rainfall dominated environment")
      
      
      
      #Annotate the final figure
      final_figure <- ggpubr::annotate_figure(temp_figure,
                                      top = ggpubr::text_grob(" "),
                                      fig.lab = paste0(y," - ",location,", ",recharge_type," (Prediction Date: ", last_date, ")"), 
                                      fig.lab.pos = "top.left"
                                      )
            
      #Save the final figure
      
       ggsave(final_figure, filename = paste0(output_path,"/","Well_",y,"_Model_Predictions_", plot_type,".jpeg"), height = 8, width = 13.5, units = "in")
     #  ggsave(final_figure, filename = paste0(figure_location,"Well_",y,"_Model_Predictions_", plot_type,".jpeg"), height = 8, width = 11.5, units = "in")
       
       
       #######  new plotting -----
       
       # - remove day 366, remove climate forecast lines
       # - x expand = c(0,0)\
       # remove rounded likelihoods to 5 in spreadsheet, <5 and >95, both in table and in output csv below
       # plot just precipitation?
       # remove past year from percentiles plot (OW409)
       
       
       
       # new table
       Probabilities_combined_Date_predicted2 <- Probabilities_combined %>%
         arrange(Date_predicted)%>%
         mutate(table_name = paste0(Date_predicted," (",lag_day," days; ",Model,")"))%>%
         dplyr::select(category,table_name, Date_predicted)%>%
         mutate(category = "Groundwater Level",
                Date_predicted = as.character(Date_predicted))%>%
         distinct(category,table_name, Date_predicted)%>%
         spread(table_name,Date_predicted) 
       
       
       test <- left_join(Probabilities_combined_lag_day %>%  pivot_longer(cols = 2:5, values_to = "ndays"),
                         Probabilities_combined_Date_predicted2 %>%  pivot_longer(cols = 2:5, values_to = "date"),
                          by = "name") %>% 
         mutate(header = paste0(ndays, "\n", format(as.Date(date), "%b-%d"))) %>% 
         select(name, header) %>%
         pivot_wider(names_from = name, values_from = header) %>% 
         mutate(category = "Groundwater Level\nConditions")
         
       
       table_knitr <- Probabilities_combined_liklihood
       names(table_knitr) <- as.vector(data.frame(test %>% 
                                                    select(category, everything()))[1,])
       
       table_for_gt <- Probabilities_combined %>%
         arrange(Date_predicted)%>%
         # mutate(table_name = paste0(lag_day, "-Days\\\\",format(Date_predicted, "%b-%d")))%>%
         # dplyr::select("Groundwater Level\\\\Conditions" = category, table_name, likelihood)%>%
         mutate(table_name = paste0(lag_day, "-Days ",format(Date_predicted, "%b-%d")))%>%
         dplyr::select("Groundwater Level Conditions" = category, table_name, likelihood)%>%
         mutate(likelihood = case_when(likelihood < 5 ~ "<5",
                                       likelihood > 95 ~ ">95",
                                       TRUE ~ as.character(round(likelihood, 0)))) %>%
         spread(table_name,likelihood) 
       
       flag_tbl_above <- table_for_gt %>%
         slice(1) %>% # only first row
         select(-1) %>%
         mutate(across(everything(), ~ {
           num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", .x)))
           .x == ">95" | num > 50
         }))
       
       flag_tbl_normal <- table_for_gt %>%
         slice(2) %>% # only first row
         select(-1) %>%
         mutate(across(everything(), ~ {
           num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", .x)))
           .x == ">95" | num > 50
         }))
       
       flag_tbl_below <- table_for_gt %>%
         slice(3) %>% # only first row
         select(-1) %>%
         mutate(across(everything(), ~ {
           num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", .x)))
           .x == ">95" | num > 50
         }))
       
       

       table_gt <- table_for_gt %>%
         gt() %>%
         tab_style(
           style = cell_fill(color = "lightblue"),
           locations = cells_body(
             rows = 1,
             columns = all_of(names(flag_tbl_above)[unlist(flag_tbl_above)])
           )
         ) %>%
         tab_style(
           style = cell_fill(color = "lightgreen"),
           locations = cells_body(
             rows = 2,
             columns = all_of(names(flag_tbl_normal)[unlist(flag_tbl_normal)])
           )
         ) %>%
         tab_style(
           style = cell_fill(color = "lightpink"),
           locations = cells_body(
             rows = 3,
             columns = all_of(names(flag_tbl_below)[unlist(flag_tbl_below)])
           )
         ) # %>% cols_label(
           # !!colnames(table_for_gt)[1] := html(colnames(table_for_gt)[1]),
           # !!colnames(table_for_gt)[2] := html(colnames(table_for_gt)[2]),
           # !!colnames(table_for_gt)[3] := html(colnames(table_for_gt)[3]),
           # !!colnames(table_for_gt)[4] := html(colnames(table_for_gt)[4]),
           # !!colnames(table_for_gt)[5] := html(colnames(table_for_gt)[5])
        # )
       
       

       Probabilities_combined22 <- bind_rows(test, #,Probabilities_combined_model,
                                        Probabilities_combined_liklihood) %>% 
         select(category, everything())
       
       # Remove the category column
       
       table_grid <- gridExtra::grid.table(Probabilities_combined22)
       
       table <- gt::gt(Probabilities_combined22)
       #Customize the table theme
       
       mytheme <- gridExtra::ttheme_default(
         core = list(fg_params=list(cex = 0.7)),
         colhead = list(fg_params=list(cex = 0.7)),
         rowhead = list(fg_params=list(cex = 0.7)),
         colhead=list(fg_params = list(parse=TRUE)))
       temp_chart <- gridExtra::tableGrob(Probabilities_combined22, theme = mytheme, rows = NULL, cols = NULL)
       
       # for (i in seq_len(nrow(Probabilities_combined22))) {
       #   for (j in seq_len(ncol(Probabilities_combined22))) {
       #     loc <- which(temp_chart$layout$t == i + 1 & temp_chart$layout$l == j)
       #     temp_chart$grobs[[loc]]$gp <- gpar(fill = "white", col = NA)
       #   }
       # }
       # 
       # header_locs <- which(temp_chart$layout$t == 1)
       # for (loc in header_locs) {
       #   temp_chart$grobs[[loc]]$gp <- gpar(fill = "#e0e0e0", fontface = "bold")  # light gray
       # }
       
       
       # # for (i in seq_len(nrow(temp_chart))) {
       # #   for (j in seq_len(ncol(temp_chart))) {
       # #     val <- temp_chart[i, j]
       # #     
       # #     if (val > 6) {
       # #       # Calculate which grob is the background cell
       # #       loc <- which(tg$layout$t == i + 1 & tg$layout$l == j)
       # #       tg$grobs[[loc]]$gp <- gpar(fill = "#ffe066", col = NA)
       # #     }
       # #   }
       # # }
       # 
       # for (i in 2:4) {
       #   for (j in 2:5) {
       #     val <- Probabilities_combined22[i, j]
       #     
       #     if (val == "<5") {
       #       # Locate the cell in the table layout
       #       loc <- which(temp_chart$layout$t == i + 1 & temp_chart$layout$l == j)
       #       
       #       # Set background fill color for the matching cell
       #       temp_chart$grobs[[loc]]$gp <- gpar(fill = "#ffcccc")  # light red
       #     }
       #   }
       # }
       
       
       
       temp_chart$widths <- unit(c(2, rep(1, ncol(temp_chart)-1)), "null")
       temp_chart$heights <- unit(c(1.25, rep(1, nrow(temp_chart)-1)), "null")
       
       temp_chart_gg <- ggplot() +
         annotation_custom(temp_chart) 
       
       
       
       
       
       #Annotate the ggplot table
       
       temp_chart_gg <- ggpubr::annotate_figure(temp_chart_gg,
                                                top = ggpubr::text_grob(" "),
                                                fig.lab = "Likelihood of Groundwater Conditions (%)", 
                                                fig.lab.pos = "top.left")
       
       
       
       
       
       recharge_type_2 <- ifelse(plot_type == "snow", "Snow", "Rain")
       
       
       
       
       
       # Temporary directory for rendering
       tmp_dir <- file.path(tempdir(), paste0("tmp_", y))
       dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
       
       # font_dir <- paste0(tmp_dir, "\\fonts\\")
       # dir.create(font_dir)
       temp_font_dir <- file.path(tmp_dir, "fonts")
       dir.create(temp_font_dir, recursive = TRUE, showWarnings = FALSE)
       
       
       # Copy files into temp dir
       rmd_copy <- file.copy("docs/gw_forecast_report.Rmd", tmp_dir)
       tex_copy <- file.copy("docs/gw_forecast_report.tex", tmp_dir)
       png_copy <- file.copy("docs/BCID_V_RGB_rev.png", tmp_dir)
       fonts_copy <- file.copy(from = list.files("docs/fonts/", full.names = TRUE),
                               to = temp_font_dir, recursive = FALSE)
       
       
       
       
       
       
       library(ggplot2)
       library(cowplot)
       library(png)      # to read PNG images
       library(grid)     # to convert to a rasterGrob
       library(gridExtra)
       
       logo <- readPNG(file.path(tmp_dir,"BCID_V_RGB_rev.png"))  # replace with your file path
      # logo <- readPNG("docs/BCID_V_RGB_rev.png")  # replace with your file path
       logo_grob <- rasterGrob(logo, interpolate = TRUE)
       
       table_data <- temp_chart_gg
       # table_grob <- tableGrob(table_data, rows = NULL)
       
       table_title <- ggdraw() +
         draw_label(paste0("Observation Well ", y, " - ", location), 
                    x = 0.5, y = 0.85, hjust = 0.5, fontface = 'bold', size = 16, color = "black") +    
         # draw_label(paste0(y, " - ", location), 
         #            x = 0.5, y = 0.87, hjust = 0.5, fontface = 'bold', size = 16, color = "black") +
         draw_label(paste0(recharge_type_2, "-Dominated Model","\nPrediction Date: ", last_date), 
                    x = 0.5, y = 0.4, hjust = 0.5,  size = 12, color = "black")
       
       main_plot <- temp_graph
       
       disclaimer <- ggdraw() +
         draw_label(paste0("Disclaimer: Groundwater level forecasts use statistical models and third-\n",
                           "party data. These models have two types of errors: systematic (model\n",
                           "limitations) and random (input data). Forecasts may differ from actual\n",
                           "observations, and water levels could exceed forecast bounds. Users must\n",
                           "accept responsibility for their use and interpretation."), 
                    x = 0.5, y = 0.1, hjust = 0.5, vjust = 0,
                    fontface = 'italic', size = 9, color = "black")
       
       plot_table_combo <- plot_grid(
         plot_grid(main_plot, temp_graph2, ncol = 1, rel_heights = c(0.7, 0.3)),
         plot_grid(table_title, table_data,disclaimer, ncol = 1, rel_heights = c(0.45, .45, .1)),  # Table + Title on the right  ncol = 2,
         rel_widths = c(2, 1)  # Adjust for table width
       )
       
       banner <- ggdraw() +
         draw_grob(logo_grob, x = 0.9, y = 0.09, width = 0.1, height = 0.8) +  # Logo
         draw_label(paste0("Groundwater Level Forecast"), x = 0.03, y = 0.7, hjust = 0,
                    fontface = "bold", size = 20, color = "white") +
         draw_label(paste0("Model forecast simulations"), x = 0.03, y = 0.4, hjust = 0,
                    fontface = "bold", size = 11, color = "white") +
         draw_label(paste0("Used to estimate possible range of groundwater levels against historical levels"), x = 0.03, y = 0.2, hjust = 0,
                    fontface = "bold", size = 11, color = "white") +
         theme(plot.background = element_rect(fill = "#234075b3"))+
         draw_line(
           x = c(0, 1), y = c(0, 0),  # from left to right at y = 0
           size = 1, color = "#e3a82b"
         )
       
       
       footer_text <- ggdraw() +
         draw_label(paste0("BC River Forecast Centre\nProduced on: ",format(Sys.time(), "%d %b %Y %H:%M")), 
                    x = 0.995, y = 0.1, hjust = 1, vjust = 0,
                    fontface = 'italic', size = 10, color = "black")
       
       
       # final_output <- plot_grid(
       #   banner,
       #   ggplot() + theme_void(),#blank space hac
       #   plot_table_combo,
       #   footer_text,  # Footer text at the bottom
       #   ncol = 1,
       #   rel_heights = c(0.10, 0.01,1,0.05)  # Adjust banner height
       # )
       
       blank_space <- ggplot() + theme_void()#blank space hac

       
       final_output <- plot_grid(
         banner,
         blank_space,
         table_title,
         plot_grid(plot_grid(main_plot, temp_graph2, ncol = 1, rel_heights = c(0.6,0.4)),
                   legend_plot, rel_widths = c(0.8,0.2)),
         plot_grid(blank_space, table_data, blank_space, ncol = 3, rel_widths = c(0.2,0.6,0.2)),
         footer_text,  # Footer text at the bottom
         ncol = 1,
         rel_heights = c(0.075, 0.02, 0.075, 0.4, 0.18, 0.25)  # Adjust banner height
       )
       
       
       final_output
       
       # ggsave(final_figure, filename = paste0(output_path,"/","Well_",y,"_Model_Predictions_", plot_type,".jpeg"), height = 8, width = 13.5, units = "in")
       save_plot(plot = final_output, filename = paste0(output_path,"/","Well_",y,"_Model_Predictions.png"), 
                 base_width = 8.5, base_height = 11, bg = "white")
       
       
       
       # directory_path <- paste0("G:/R/projects/Groundwater Forecasting/", output_path)
       
       
       
   
       # 
       # # Sanity checks
       # stopifnot(file.exists(file.path(tmp_dir, "gw_forecast_report.Rmd")))
       # stopifnot(file.exists(file.path(tmp_dir, "gw_forecast_report.tex")))
       # stopifnot(file.exists(file.path(tmp_dir, "BCID_V_RGB_rev.png")))
       # stopifnot(length(list.files(temp_font_dir)) > 0)
       
       # Path to copied Rmd
       tmp_rmd <- file.path(tmp_dir, "gw_forecast_report.Rmd")
       
       
       # # Logging (useful for debugging parallel jobs)
       # message("Rendering report for: ", y)
       # message("Temporary directory: ", tmp_dir)
       # message("Intermediate directory: ", int_dir)
       
       rmarkdown::render(input = tmp_rmd,
                         params = list("well_id" = y[[1]],
                                       "plot" = plot_grid(plot_grid(main_plot, temp_graph2, ncol = 1, rel_heights = c(0.6,0.4)),
                                                          legend_plot, rel_widths = c(0.8,0.2)),
                                       "table" = table_gt),
                         output_dir = output_path,
                         output_file = paste0("Well_", y, "_Model_Predictions.pdf"),
                         intermediates_dir = tmp_dir,  # Isolate temp files
                         envir = new.env())
       
       
       
       ######
       
       
       
       
       Probabilities_combined_output <- Probabilities_combined %>%
         # mutate(likelihood = ifelse(likelihood < 5, "<5", round(likelihood, 0))) %>%
         mutate(conditions = category)%>%
         dplyr::select(Well, Date_predicted, lag_day, Model,likelihood,conditions)
       Probabilities_combined_output
       
       Waterlevel_adjustments
       
       Time_series_data_2_plot <- Time_series_data_2_plot %>%
       dplyr::select(-Model) %>%
         mutate(across(where(is.numeric), ~ na_if(.x, -Inf))) %>%
         mutate(across(where(is.numeric), ~ na_if(.x, Inf))) %>%
         mutate(across(where(is.numeric), ~ na_if(.x, NaN))) %>%
         mutate(across(where(is.numeric), ~ replace_na(.x, NA))) %>%
         mutate(
           groundwater = Waterlevel_adjustments_temp - as.numeric(groundwater),
           predicted_value_mean = Waterlevel_adjustments_temp - as.numeric(predicted_value_mean),
           predicted_value_min = Waterlevel_adjustments_temp - as.numeric(predicted_value_min),
           predicted_value_max = Waterlevel_adjustments_temp - as.numeric(predicted_value_max),
           predicted_value_25th = Waterlevel_adjustments_temp - as.numeric(predicted_value_25th),
           predicted_value_75th = Waterlevel_adjustments_temp - as.numeric(predicted_value_75th),
           predicted_value_50th = Waterlevel_adjustments_temp - as.numeric(predicted_value_50th),
           predicted_value_90th = Waterlevel_adjustments_temp -as.numeric(predicted_value_90th),
           predicted_value_10th = Waterlevel_adjustments_temp - as.numeric(predicted_value_10th),
           predicted_value_5th = Waterlevel_adjustments_temp - as.numeric(predicted_value_5th),
           predicted_value_95th = Waterlevel_adjustments_temp - as.numeric(predicted_value_95th)
         ) %>%
         mutate(missing_data = ifelse(is.na(groundwater), "Missing", NA))
        
      Probabilities_combined_output<- full_join(Probabilities_combined_output,Time_series_data_2_plot)

  }else{
    
    Probabilities_combined_output <- data.frame(Well = y,
                                                Date_predicted = NA,
                                                lag_day = NA, 
                                                Model = "ANN",
                                                likelihood = NA,
                                                conditions = NA,
                                                Date = Sys.Date(),
                                                groundwater = NA,
                                                predicted_value_mean = NA,
                                                predicted_value_min = NA,
                                                predicted_value_max = NA,
                                                predicted_value_25th = NA,
                                                predicted_value_75th = NA,
                                                predicted_value_50th = NA,
                                                predicted_value_90th = NA,
                                                predicted_value_10th = NA,
                                                predicted_value_5th = NA,
                                                predicted_value_95th = NA,
                                                performance = NA,
                                                missing_data = NA)
    
    
  }
      

      return(Probabilities_combined_output)
      
  }
  
}

