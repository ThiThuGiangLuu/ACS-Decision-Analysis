  #install.packages("decisionSupport")
  library(decisionSupport)
  #no comma in number in dataset
  
  make_variables<-function(est,n=1)
  {
  x<-random(rho=est,n=n)
  for(i in colnames(x))
    assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)
  }
  
  make_variables(estimate_read_csv("acis_inputs_EN.csv"))
  
  acis_costbenefit <- function(x, varnames){
    
    # Intervention 1: Weather station-SMS-gender
    # Intervention 2: SMS-gender
    # Intervention 3: SMS-loudspeaker
    # Intervention 4: Paper-loudspeaker
    # i1: risk/cost/benefit variable incurred for intervention1
    # i2: risk/cost/benefit variable incurred for intervention2
    # i3: risk/cost/benefit variable incurred for intervention3
    # i4: risk/cost/benefit variable incurred for intervention4
    # i12: risk/cost/benefit variable incurred for intervention1 and intervention 2
    # i234: risk/cost/benefit for intervention2, intervention3 and intervention 4
    # i34: risk/cost/benefit for intervention3 and intervention 4
    # i1234: risk/cost/benefit for all intervention1, intervention2, intervention 3 and intervention 4
  
  # RISKS THAT IMPACTS BENEFITS
    # inaccurate forecast:  risk which is on-going every season/year
    # weather risks: risk which is on-going every season/year
    
    #CALCULATING UNCERTAINTY and RISKS####
    # Drought risk for each year
    drought_risk_i1234<-chance_event(chance_drought_i1234,value_if = 1, n=n_years)
    # Risk of having events can cause re-fertilize
    risk_refertilize_i1234<-chance_event(chance_refertilize_i1234,value_if = 1, n=n_years)
    # Risk of re_sow due to extreme events (rain, cold) 
    risk_resow_i1234<-chance_event(chance_resow_i1234,value_if = 1, n=n_years)
    # Chance of having inaccurate weather forecast, intervention 1
    inaccurate_forecast_i1<-chance_event(chance_inaccurate_forecast_i1,value_if = 1,n=n_years)
    # Chance of having inaccurate weather forecast, intervention 2, 3, 4
    inaccurate_forecast_i234<-chance_event(chance_inaccurate_forecast_i234,value_if = 1, n=n_years)
    # Chance of having inaccurate drought forecast at the beginning of season intervention 1, 2, 3, 4
    inaccurate_forecast_extreme_drought_i1234<-chance_event(chance_inaccurate_forecast_extreme_drought_i1234,value_if = 1, n=n_years)
    # Extreme cold risk for each year
    risk_extreme_cold<-chance_event(chance_extreme_cold,value_if = 1, n=n_years)
    # Chance of having inaccurate extreme cold forecast 
    inaccurate_forecast_extreme_cold_i1234<-chance_event(chance_inaccurate_forecast_extreme_cold_i1234,value_if = 1, n=n_years)
    
    # CALCULATING COSTS####
    
  # I. Forecast generation####
    # 1. Set up new local met station for the whole project period (Intervention 1)
    cost_new_met_station<-rep(0,n_years)
    cost_new_met_station[1]<-(met_station_esta_i1+vv(met_station_main_i1, var_CV,1)+
      cost_forecasts_access_i1)/exchange_rate
    cost_new_met_station[2:5]<-vv(met_station_main_i1,var_CV,4)/exchange_rate
    
    # 2. Buy forecast from provincial met station for the whole project period 
    # (Intervention1234)
    cost_forecast_province<-rep(0,n_years)
    cost_forecast_province[1:5]<-(cost_weekly_forecasts_i1234+
                                    cost_seasonal_forecasts_i1234)/exchange_rate
  #II. Translation####
    
    # 3. Translation from forecasts (training on translation) to advisory costs 
    #for the whole project period (Intervention1234)
   
    cost_translation<-rep(0,n_years)
    cost_translation[1:2]<-vv(cost_cb_translation_staff_i1234, var_CV,2)*
      cb_translation_n12/exchange_rate
    cost_translation[3:5]<-vv(cost_cb_translation_staff_i1234, var_CV,3)*
      cb_translation_n345/exchange_rate
  #III. Transfer####
    # Calculating total households and farm households in 5 years
    # https://socratic.org/questions/how-do-you-calculate-population-growth
    time<-1:n_years
    total_households_i1234<-vv(baseline_households_i1234, var_CV, n_years)*
      exp(household_increase_rate*time)
    
    total_farm_households_i1234<-vv(baseline_farm_households_i1234, var_CV, n_years)*
      exp(household_increase_rate*time)
    
    # 4. Transfer and communication costs: district and commune/village for 
    #the whole project period (Intervention1234)
    cost_capacity_communication<-rep(0,n_years)
    cost_capacity_communication[1]<-(vv(cost_cb_commune_i1234,var_CV,1)*n_communes+
    vv(leftlet_year1_i1234,var_CV,1)*vv(total_farm_households_i1234,var_CV,1)+
    vv(village_meeting_launch_i1234,var_CV,1)*n_village+
      vv(video_i1234_peryear,var_CV,1)+
      vv(cost_print_seasonal_bulletinA0_i1234,var_CV,1)*
      (n_village*n_per_village+n_communes)*n_print_per_year)/exchange_rate
    
    cost_capacity_communication[2:5]<-(
    vv(video_i1234_peryear,var_CV,4)+
    vv(cost_print_seasonal_bulletinA0_i1234,var_CV,4)*
    (n_village*n_per_village+n_communes)*n_print_per_year)/exchange_rate
    
    # 5. Cost for sending SMS to rice farmers for the whole project period (Intervention12)
    cost_rice_SMS<- rep(0,n_years)
    cost_rice_SMS[1:5]<-vv(cost_per_SMS_i123,var_CV,5)*
      vv(messages_per_time_rice_i123, var_CV,5)*
      vv(number_times_per_year_rice_i123,var_CV,5)*total_farm_households_i1234/
      exchange_rate
      
     cost_animal_SMS<-rep(0,n_years)
     cost_animal_SMS[1:5]<-vv(cost_per_SMS_i123,var_CV,5)*
       vv(messages_per_time_animal_i123,var_CV,5)*
      vv(number_times_per_year_animal_i123, var_CV,5)*
       vv(percent_animal_households_i1234,var_CV,5)*
        vv(total_farm_households_i1234, var_CV,5)/exchange_rate
       
    #6. Cost for sending SMS to village leader for the whole project period(Intervention3)
     cost_SMS_villageleader<- rep(0,n_years)
     cost_SMS_villageleader[1:5]<-vv(cost_per_SMS_i123,var_CV,5)*
       vv(messages_per_time_rice_i123, var_CV,5)*
       vv(number_times_per_year_rice_i123,var_CV,5)*commune_village_SMS_i3/
       exchange_rate+
       vv(cost_per_SMS_i123,var_CV,5)*
       vv(messages_per_time_animal_i123,var_CV,5)*
       vv(number_times_per_year_animal_i123, var_CV,5)*
       commune_village_SMS_i3/exchange_rate 
       
     # 7. Allowance for village leaders for the whole project period (Intervention34)
     cost_allowance_village_leader<-rep(0,n_years)
     cost_allowance_village_leader[1:5]<-n_village*
       vv(allowance_village_leader_permonth_loud_i34,var_CV,5)*
       months_per_year/exchange_rate
    
     # 8. Cost_collect_bulletin (Intervention4)
     cost_collect_bulletin<-rep(0,n_years)
     cost_collect_bulletin[1:5]<-(allowance_bulletin_collect_time_short_i4*
            vv(percent_short_distance_i4,var_CV,5)*
              times_per_month*months_per_year*n_village)/exchange_rate+
      (allowance_bulletin_collect_time_long_i4*
         vv(1-percent_short_distance_i4,var_CV,5)*
         times_per_month*months_per_year*n_village)/exchange_rate 
     
   # IV. Support the use and learning####
     
     # 9. Establish demonstration models (5000m2/season, 2models of 1ha/year) 
     # Total fertilizer cost per ha (Intervention1234)
     fa_adv_cost_perha <-(NPK5105_advice_i1234*NPK5105_price_i1234+N_advice_i1234*N_price_i1234+
                            K_advice_i1234*K_price_i1234)/exchange_rate
     # Consider the partial support from the Government to develop demonstration model (5000m2~1/2ha)
     cost_per_modeli1234<-((seed_advice_i1234*price_seed_i1234+
      plant_protection_support_i1234)/exchange_rate+fa_adv_cost_perha*percent_fertilizer_model_supporti1234)/2
      
     cost_model<-rep(0,n_years)
     cost_model[1:5]<-(model_training_i1234*n_training+no_model_compare_i1234*
     cost_per_modeli1234+model_monitor_i1234+field_visit_i1234)/
       exchange_rate
     
     # 10. cost for gender intervention social action and analysis SAA (training, dialogue, commnity events)
     # for the whole project period (Intervention1234)
     cost_gender<-rep(0,n_years)
     cost_gender[1]<-(SAA_TOT+dialogue_village*n_dialogues*n_village+
                          cost_community_event*n_village)/exchange_rate 
     cost_gender[2:5]<-0
  # V. Monitoring ####
     # 11. Monitoring and Evaluation for the whole project period (Intervention1234)
     ME_cost<-rep(0,n_years)
     ME_cost[0:2]<-0
     ME_cost[3]<-(ME_district_i1234+ME_commune_i1234*n_communes)/exchange_rate
     ME_cost[5]<-(ME_district_i1234+ME_commune_i1234*n_communes)/exchange_rate
    
    # Total cost for intervention 1####
     total_cost_i1<-cost_new_met_station+cost_forecast_province+
       cost_translation+cost_capacity_communication+cost_rice_SMS+
       cost_animal_SMS+cost_model+cost_gender+ME_cost
    
    # Total cost for intervention 2####
     total_cost_i2<-cost_forecast_province+cost_translation+
       cost_capacity_communication+cost_rice_SMS+cost_animal_SMS+
       cost_model+cost_gender+ME_cost
    # Total cost for intervention 3####
     total_cost_i3<-cost_forecast_province+cost_translation+
       cost_capacity_communication+cost_SMS_villageleader+
       cost_allowance_village_leader+cost_model+ME_cost
                                  
     # Total cost for intervention 4####
     total_cost_i4<-cost_forecast_province+cost_translation+
       cost_capacity_communication+cost_collect_bulletin+
       cost_allowance_village_leader+cost_model+ME_cost
                                              
  # CALCULATING BENEFITS####
   
    #I. ECONOMIC BENEFITS####
    #1. Rice benefits####
    #for all interventions in the condition of having good weather forecast
    #1.1. Advisories to cope with extreme drought####
    #Refer to drought risk for each year
     drought_risk_i1234<-chance_event(chance_drought_i1234,value_if = 1, n=n_years)
    
    # drought area for each year 
     rice_drought_i1234<-rep(0,n_years)
     rice_drought_i1234[1:5]<-rice_area_drought_i1234*drought_risk_i1234
    # rice area (total area of two seasons a year) that are not affected by drought 
     rice_area_effect<-rep(0,n_years)
     rice_area_effect[1:5]<-vv(total_rice_area_i1234,var_CV, 5)-
       vv(rice_drought_i1234,var_CV, 5)
    
    # Avoid losses due to drought for the whole project period
    
     reduced_drought_losses<-rep(0, n_years)
     reduced_drought_losses[1:5]<-vv(reduce_loss_drought_i1234,var_CV, 5)*
       vv(rice_drought_i1234,var_CV_high, 5)/exchange_rate
  
    # 1.2. Seeds benefit####
    # Seed dose reduction
    
       seed_dose_reduction_perha<-rep(0,n_years)
       seed_dose_reduction_perha[1:5]<-((seed_baseline_i1234-seed_applied_i1234)*
         vv(price_seed_i1234, var_CV, 5)+vv(labor_pick_i1234,var_CV, 5))/exchange_rate
    
    # Total seed dose reduction benefit intervention 1 
    # sowing adoption rate intervention 1
    # Write the function for adoption, if adoption reaches to the peak,#use saturated rate 
       adoptionrate<-function(p,q,n_years,dis_adoption,r_saturated)
       {
         r<-rep(0, n_years)
         for (t in c(2:n_years))
         {r[1]<-p
         r[t]<-r[t-1]+q*r[t-1]-r[t-1]*dis_adoption}
         ifelse(r>r_saturated,r_saturated,r)
       }
       
       seed_rate_i1<-adoptionrate(rate_seed_inno_i1,
                rate_seed_imitation_i1,n_years,
                dis_adoption_i123,rate_saturated_i12)
    # seed benefit=dose benefit((farmer dose-advisory does)*advised times) + 
    # times benefit(farmer dose*reduced times)
    # 1.2.1. Benefit from dose reduction####
       benefit_dose_seed_i1<-rep(0,n_years)
       benefit_dose_seed_i1[1:5]<-seed_dose_reduction_perha*rice_area_effect*
         seed_rate_i1*effective_rate 
    # Total seed dose reduction benefit intervention 2
    # sowing adoption rate intervention 2
       
       seed_rate_i2<-adoptionrate(rate_seed_inno_i2,
                                  rate_seed_imitation_i2,n_years,
                                  dis_adoption_i123,rate_saturated_i12)
       
    # benefit from dose reduction   
       benefit_dose_seed_i2<-rep(0,n_years)
       benefit_dose_seed_i2[1:5]<-seed_dose_reduction_perha*rice_area_effect*
         seed_rate_i2*effective_rate
    # Total seed dose reduction benefit intervention 3 
       # sowing adoption rate intervention 3
       seed_rate_i3<-adoptionrate(rate_seed_inno_i3,
                                  rate_seed_imitation_i3,n_years,
                                  dis_adoption_i123,rate_saturated_i34)
       # benefit from dose reduction   
       benefit_dose_seed_i3<-rep(0,n_years)
       benefit_dose_seed_i3[1:5]<-seed_dose_reduction_perha*rice_area_effect*
         seed_rate_i3*effective_rate
    # Total seed dose reduction benefit intervention 4 
       # sowing adoption rate intervention 4
       seed_rate_i4<-adoptionrate(rate_seed_inno_i4,
                                  rate_seed_imitation_i4,n_years,
                                  dis_adoption_i4,rate_saturated_i34)
       # benefit from dose reduction   
       benefit_dose_seed_i4<-rep(0,n_years)
       benefit_dose_seed_i4[1:5]<-seed_dose_reduction_perha*rice_area_effect*
         seed_rate_i4*effective_rate
      # 1.2.2. Benefits from reduced times of re-sowing####
      
      # Refer to risk of re_sow due to extreme events (rain, cold) if business as usual 
      # risk_resow_i1234<-chance_event(chance_resow_i1234,value_if = 1, n=n_years)
      # taking into consideration of the chance to reduce the risk
       resow_reduced_i1234<-risk_resow_i1234*chance_resow_advice_i1234
       
      # Benefit for time reduction in sowing for intervention 1 
      # Costs for sowing for 1 ha per year (equal to costs can be reduced if it can avoid)
      # No need to *2 for dose as the area is already*for two seasons
       reduce_resow_costsperha_i1234<-(seed_baseline_i1234)*
                                        vv(price_seed_i1234, var_CV, 5)
       
       inaccurate_forecast_i1<-chance_event(chance_inaccurate_forecast_i1,value_if = 1,n=n_years)
       benefit_time_seed_i1<-rep(0,n_years)
       benefit_time_seed_i1[1:5]<-resow_reduced_i1234*rice_area_effect*
                (reduce_resow_costsperha_i1234+labor_seed_fertilize_i1234)*
                vv(resow_fer_area_percentage_i1234,var_CV_high,5)*
         seed_rate_i1*effective_rate*(1-inaccurate_forecast_i1)/exchange_rate
           
      # Benefit for time reduction in sowing for intervention 2 
       inaccurate_forecast_i234<-chance_event(chance_inaccurate_forecast_i234,value_if = 1, n=n_years)
       benefit_time_seed_i2<-rep(0,n_years)
       benefit_time_seed_i2[1:5]<-resow_reduced_i1234*rice_area_effect*
         (reduce_resow_costsperha_i1234+labor_seed_fertilize_i1234)*
         vv(resow_fer_area_percentage_i1234,var_CV_high,5)*
         seed_rate_i2*effective_rate*(1-inaccurate_forecast_i234)/exchange_rate
       
    # Benefit for time reduction in sowing for intervention 3 
       benefit_time_seed_i3<-rep(0,n_years)
       benefit_time_seed_i3[1:5]<-resow_reduced_i1234*rice_area_effect*
         (reduce_resow_costsperha_i1234+labor_seed_fertilize_i1234)*
         vv(resow_fer_area_percentage_i1234,var_CV_high,5)*
         seed_rate_i3*effective_rate*(1-inaccurate_forecast_i234)/exchange_rate
    # Benefit for time reduction in sowing for intervention 4 
       benefit_time_seed_i4<-rep(0,n_years)
       benefit_time_seed_i4[1:5]<-resow_reduced_i1234*rice_area_effect*
         (reduce_resow_costsperha_i1234+labor_seed_fertilize_i1234)*
         vv(resow_fer_area_percentage_i1234,var_CV_high,5)*
         seed_rate_i4*effective_rate*(1-inaccurate_forecast_i234)/exchange_rate
      
    # 1.3 Fertilizer benefits####
    # 1.3.1. Dose reduction####
    # for fertilizer per season per ha
       
       benefit_dose_fer_perha<-((NPK5105_baseline_i1234 -NPK5105_applied_i1234)*NPK5105_price_i1234+
            (N_baseline_i1234-N_applied_i1234)*N_price_i1234+
            (K_baseline_i1234-K_applied_i1234)*K_price_i1234)/exchange_rate
       
    # Benefit for dose reduction for intervention 1 
       # ferlizer and plant protection adoption rate 
       rate_fer_pla_i1<- adoptionrate(rate_fer_pla_inno_i1,
                        rate_fer_pla_immi_i1,n_years,
                        dis_adoption_i123,rate_saturated_i12)
    # Benefit - For area, it needs to be discounted with potential area destroyed by 
    # annual severe risks such as hailstone/flashflood
       benefit_dose_fer_i1<-rep(0,n_years)
       benefit_dose_fer_i1[1:5]<-benefit_dose_fer_perha*rice_area_effect*
         (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))* rate_fer_pla_i1*effective_rate
        
    # Benefit for dose reduction for intervention 2
       # ferlizer and plant protection adoption rate 
       rate_fer_pla_i2<-adoptionrate(rate_fer_pla_inno_i2,
                          rate_fer_pla_immi_i2,n_years,
                          dis_adoption_i123,rate_saturated_i12)
       # benefit
       benefit_dose_fer_i2<-rep(0,n_years)
       benefit_dose_fer_i2[1:5]<-benefit_dose_fer_perha*rice_area_effect*
         (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*rate_fer_pla_i2*effective_rate
        
    # Benefit for dose reduction for intervention 3 
       # ferlizer and plant protection adoption rate 
       rate_fer_pla_i3<-adoptionrate(rate_fer_pla_inno_i3,
                          rate_fer_pla_immi_i3,n_years,
                          dis_adoption_i123,rate_saturated_i34)
       # benefit
       benefit_dose_fer_i3<-rep(0,n_years)
       benefit_dose_fer_i3[1:5]<-benefit_dose_fer_perha*rice_area_effect*
         (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
         rate_fer_pla_i3*effective_rate
        
    # Benefit for dose reduction for intervention 4    
       # ferlizer and plant protection adoption rate 
       rate_fer_pla_i4<-adoptionrate(rate_fer_pla_inno_i4,
                          rate_fer_pla_immi_i4,n_years,
                          dis_adoption_i4,rate_saturated_i34)
       # benefit
       benefit_dose_fer_i4<-rep(0,n_years)
       benefit_dose_fer_i4[1:5]<-benefit_dose_fer_perha*rice_area_effect*
         (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*rate_fer_pla_i4*effective_rate
       
             
    # 1.3.2. Benefit for fertilizing times reduction#### 
    # Refer to risk of having events can cause re-fertilize
    # risk_refertilize_i1234<-chance_event(chance_refertilize_i1234,value_if = 1, n=n_years)
    # taking into consideration of the chance to reduce the risk 
       refertilize_reduced_i1234<-risk_refertilize_i1234*chance_refertilize_advice_i1234
       
    # cost for fertlizing for one ha using farmers'dose
       
       fa_fer_cost_perha<-(NPK5105_baseline_i1234*NPK5105_price_i1234+
                        N_baseline_i1234*N_price_i1234+
                        K_baseline_i1234*K_price_i1234)/exchange_rate
    
    # Benefit for time reduction for intervention 1 
       benefit_time_fer_i1<-rep(0,n_years)
       benefit_time_fer_i1[1:5]<-refertilize_reduced_i1234*rice_area_effect*
         (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
         (fa_fer_cost_perha+labor_seed_fertilize_i1234/exchange_rate)*
         vv(resow_fer_area_percentage_i1234,var_CV_high,5)*
         rate_fer_pla_i1*effective_rate*(1-inaccurate_forecast_i1)
       
    # Benefit for time reduction for intervention 2 
       benefit_time_fer_i2<-rep(0,n_years)
       benefit_time_fer_i2[1:5]<-refertilize_reduced_i1234*rice_area_effect*
         (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
         (fa_fer_cost_perha+labor_seed_fertilize_i1234/exchange_rate)*
         vv(resow_fer_area_percentage_i1234, var_CV_high,5)*
         rate_fer_pla_i2*effective_rate*(1-inaccurate_forecast_i234)
      
    # Benefit for time reduction for intervention 3
       benefit_time_fer_i3<-rep(0,n_years)
       benefit_time_fer_i3[1:5]<-refertilize_reduced_i1234*rice_area_effect*
         (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
         (fa_fer_cost_perha+labor_seed_fertilize_i1234/exchange_rate)*
         vv(resow_fer_area_percentage_i1234, var_CV_high,5)*
         rate_fer_pla_i3*effective_rate*(1-inaccurate_forecast_i234)
           
    # Benefit for time reduction for intervention 4
       benefit_time_fer_i4<-rep(0,n_years)
       benefit_time_fer_i4[1:5]<-refertilize_reduced_i1234*rice_area_effect*
         (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
         (fa_fer_cost_perha+labor_seed_fertilize_i1234/exchange_rate)*
         vv(resow_fer_area_percentage_i1234, var_CV_high,5)*
         rate_fer_pla_i4*effective_rate*(1-inaccurate_forecast_i234)
            
    # 1.4. Plant protection benefits####
     
    
    #Farmers use many different types of pesticides with different names so the calculation will base
    # on the potential time reduced in 5 years/100m2 and the average cost for each time 
    # Benefit effective combined dose and timing in applying plant protection substances 
    # Reduced time sprayed per year per ha. This is not only due to weather but also due to
    # knowledge, behaviour and access to information to local sale agent. Therefore, chance to reduce
    # is calculated for all and already considered affected area and inaccurate forecasts
      
      reduce_cost_spray_peryear_i1234<-vv(reduced_times_spray_i1234, var_CV_high,5)*
                                        vv(reduced_cost_per_time_spray_i1234,var_CV_high,5)
      increase_cost_monitor_peryear_i1234<-vv(times_monitor_increased, var_CV_high,5)*
                                  vv(cost_monitor_pest_increased,var_CV_high,5)
      change_cost_spray_perha<- (reduce_cost_spray_peryear_i1234-increase_cost_monitor_peryear_i1234)*10
      
     # Benefit for time reduction for intervention 1 
      benefit_time_spray_i1<-rep(0,n_years)
        benefit_time_spray_i1[1:5]<-change_cost_spray_perha*rice_area_effect*
          (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
          rate_fer_pla_i1*effective_rate/exchange_rate
           
    # Benefit for time reduction for intervention 2
        benefit_time_spray_i2<-rep(0,n_years)
        benefit_time_spray_i2[1:5]<-change_cost_spray_perha*rice_area_effect*
          (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
          rate_fer_pla_i2*effective_rate/exchange_rate
           
    # Benefit for time reduction for intervention 3
        benefit_time_spray_i3<-rep(0,n_years)
        benefit_time_spray_i3[1:5]<-change_cost_spray_perha*rice_area_effect*
          (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
          rate_fer_pla_i3*effective_rate/exchange_rate
           
    # Benefit for time reduction for intervention 4
        benefit_time_spray_i4<-rep(0,n_years)
        benefit_time_spray_i4[1:5]<-change_cost_spray_perha*rice_area_effect*
          (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
          rate_fer_pla_i4*effective_rate/exchange_rate
    #1.5. Rice yield benefit#### 
        # Rice yield benefit for Intervention 1
        rice_benefit_change_i1<-rep(0,n_years)
        rice_benefit_change_i1[1:5]<-rice_area_effect*
          (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
          vv(yield_change_i1234, var_CV, 5)*rice_price*
          ((seed_rate_i1+2*rate_fer_pla_i1)/3)*effective_rate/exchange_rate
        # Rice yield benefit for Intervention 2
        rice_benefit_change_i2<-rep(0,n_years)
        rice_benefit_change_i2[1:5]<-rice_area_effect*
          (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
          vv(yield_change_i1234, var_CV, 5)*rice_price*
          ((seed_rate_i2+2*rate_fer_pla_i2)/3)*effective_rate/exchange_rate
        # Rice yield benefit for Intervention 3
        rice_benefit_change_i3<-rep(0,n_years)
        rice_benefit_change_i3[1:5]<-rice_area_effect*
          (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
          vv(yield_change_i1234, var_CV, 5)*rice_price*
          ((seed_rate_i3+2*rate_fer_pla_i3)/3)*effective_rate/exchange_rate
        # Rice yield benefit for Intervention 4
        rice_benefit_change_i4<-rep(0,n_years)
        rice_benefit_change_i4[1:5]<-rice_area_effect*
          (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
          vv(yield_change_i1234, var_CV, 5)*rice_price*
          ((seed_rate_i4+2*rate_fer_pla_i4)/3)*effective_rate/exchange_rate
    #1.6. Discounting uncertainty of forecasts-accurate good weather forecast####    
    # Total benefit for rice: This benefit equal to total rice benefit in the 
    # good weather forecasts conditions discounting the inaccurate weather forecasts. 
    # When having negative effect. It does not normally affect all but just a part of that. For example, farmers do not need
    # to put all fertilizer back
    # Total benefit for rice intervention 1
    #Refer to risks####
    #inaccurate_forecast_i1<-chance_event(chance_inaccurate_forecast_i1,value_if = 1,n=n_years)
    #inaccurate_forecast_i234<-chance_event(chance_inaccurate_forecast_i234,value_if = 1, n=n_years)
    #inaccurate_forecast_extreme_drought_i1234<-chance_event(chance_inaccurate_forecast_extreme_drought_i1234,value_if = 1, n=n_years)
    
    total_rice_i1<-(1-inaccurate_forecast_extreme_drought_i1234)*reduced_drought_losses-
          (inaccurate_forecast_extreme_drought_i1234*rice_profit_no_drought*rice_drought_i1234/exchange_rate)+
      benefit_dose_seed_i1+benefit_dose_fer_i1+
      (benefit_time_seed_i1+benefit_time_fer_i1)*(1-inaccurate_forecast_i1)-
      (benefit_time_seed_i1+benefit_time_fer_i1)*inaccurate_forecast_i1
       benefit_time_spray_i1+
       rice_benefit_change_i1*(1-inaccurate_forecast_i1)-
       rice_benefit_change_i1*inaccurate_forecast_i1
    
    # Total benefit for rice intervention 2
       total_rice_i2<-(1-inaccurate_forecast_extreme_drought_i1234)*reduced_drought_losses-
         (inaccurate_forecast_extreme_drought_i1234*rice_profit_no_drought*rice_drought_i1234/exchange_rate)+
         benefit_dose_seed_i2+benefit_dose_fer_i2+
         (benefit_time_seed_i2+benefit_time_fer_i2)*(1-inaccurate_forecast_i234)-
         (benefit_time_seed_i2+benefit_time_fer_i2)*inaccurate_forecast_i234
       benefit_time_spray_i2+
         rice_benefit_change_i2*(1-inaccurate_forecast_i234)-
         rice_benefit_change_i2*inaccurate_forecast_i234
    # Total benefit for rice intervention 3
       total_rice_i3<-(1-inaccurate_forecast_extreme_drought_i1234)*reduced_drought_losses-
         (inaccurate_forecast_extreme_drought_i1234*rice_profit_no_drought*rice_drought_i1234/exchange_rate)+
         benefit_dose_seed_i3+benefit_dose_fer_i3+
         (benefit_time_seed_i3+benefit_time_fer_i3)*(1-inaccurate_forecast_i234)-
         (benefit_time_seed_i3+benefit_time_fer_i3)*inaccurate_forecast_i234
       benefit_time_spray_i3+
         rice_benefit_change_i3*(1-inaccurate_forecast_i234)-
         rice_benefit_change_i3*inaccurate_forecast_i234
       
    # Total benefit for rice intervention 4
       total_rice_i4<-(1-inaccurate_forecast_extreme_drought_i1234)*reduced_drought_losses-
         (inaccurate_forecast_extreme_drought_i1234*rice_profit_no_drought*rice_drought_i1234/exchange_rate)+
         benefit_dose_seed_i4+benefit_dose_fer_i4+
         (benefit_time_seed_i4+benefit_time_fer_i4)*(1-inaccurate_forecast_i234)-
         (benefit_time_seed_i4+benefit_time_fer_i4)*inaccurate_forecast_i234
       benefit_time_spray_i4+
         rice_benefit_change_i4*(1-inaccurate_forecast_i234)-
         rice_benefit_change_i4*inaccurate_forecast_i234
    
    # 2. Buffalo benefits#### 
    # risk of extreme cold that can affect buffalos and cows. Advisories can be provied
    # in any extreme cold events
    risk_extreme_cold<-chance_event(chance_extreme_cold,value_if = 1, n=n_years)
    
    # Benefit for buffalo intervention 1
    
    animal_rate_i1<-adoptionrate(rate_ani_inno_i1,
                                 rate_ani_immi_i1,n_years,
                                 dis_adoption_i123,
                                 rate_saturated_i12)
      
    #benefit for buffalo
    # inaccurate forecast are same for all interventions
    inaccurate_forecast_extreme_cold_i1234<-chance_event(chance_inaccurate_forecast_extreme_cold_i1234,value_if = 1, n=n_years)
    buffalo_benefiti1<-rep(0, n_years)
    buffalo_benefiti1[1:5]<-(risk_extreme_cold*
      vv(total_buffalo_i1234,var_CV_high,5)*
      vv(price_buffalo_i1234,var_CV_high,5)*
      vv(reduced_death_animal_i1,var_CV_high,5)*
        animal_rate_i1*effective_rate/exchange_rate)*
      (1-2*inaccurate_forecast_extreme_cold_i1234)
     
    
    # Benefit for buffalo intervention 2
    animal_rate_i2<-adoptionrate(rate_ani_inno_i2,
                                 rate_ani_immi_i2,n_years,
                                 dis_adoption_i123,
                                 rate_saturated_i12)
    #benefit for buffalo
    buffalo_benefiti2<-rep(0, n_years)
    buffalo_benefiti2[1:5]<-(risk_extreme_cold*
                               vv(total_buffalo_i1234,var_CV_high,5)*
                               vv(price_buffalo_i1234,var_CV_high,5)*
                               vv(reduced_death_animal_i2,var_CV_high,5)*
                               animal_rate_i2*effective_rate/exchange_rate)*
                              (1-2*inaccurate_forecast_extreme_cold_i1234)
    
    # Benefit for buffalo intervention 3
    animal_rate_i3<-adoptionrate(rate_ani_inno_i3,
                                 rate_ani_immi_i3,n_years,
                                 dis_adoption_i123,
                                 rate_saturated_i34)
    #benefit for buffalo
    buffalo_benefiti3<-rep(0, n_years)
    buffalo_benefiti3[1:5]<-(risk_extreme_cold*
                               vv(total_buffalo_i1234,var_CV_high,5)*
                               vv(price_buffalo_i1234,var_CV_high,5)*
                               vv(reduced_death_animal_i3,var_CV_high,5)*
                               animal_rate_i3*effective_rate/exchange_rate)*
                              (1-2*inaccurate_forecast_extreme_cold_i1234)
    # Benefit for buffalo intervention 4
    animal_rate_i4<-adoptionrate(rate_ani_inno_i4,
                                 rate_ani_immi_i4,n_years,
                                 dis_adoption_i4,
                                 rate_saturated_i34)
    
    #benefit for buffalo
    buffalo_benefiti4<-rep(0, n_years)
    buffalo_benefiti4[1:5]<-(risk_extreme_cold*
                               vv(total_buffalo_i1234,var_CV_high,5)*
                               vv(price_buffalo_i1234,var_CV_high,5)*
                               vv(reduced_death_animal_i4,var_CV_high,5)*
                               animal_rate_i4*effective_rate/exchange_rate)*
                              (1-2*inaccurate_forecast_extreme_cold_i1234)
    # 3. Cow benefits####
    # Benefit for cow intervention 1
    cow_benefiti1<-rep(0, n_years)
    cow_benefiti1[1:5]<-(risk_extreme_cold*
                               vv(total_cow_i1234,var_CV_high,5)*
                               vv(price_cow_i1234,var_CV_high,5)*
                               vv(reduced_death_animal_i1,var_CV_high,5)*
                               animal_rate_i1*effective_rate/exchange_rate)*
                              (1-2*inaccurate_forecast_extreme_cold_i1234)
    
    # Benefit for cow intervention 2
    cow_benefiti2<-rep(0, n_years)
    cow_benefiti2[1:5]<-(risk_extreme_cold*
                           vv(total_cow_i1234,var_CV_high,5)*
                           vv(price_cow_i1234,var_CV_high,5)*
                           vv(reduced_death_animal_i2,var_CV_high,5)*
                           animal_rate_i2*effective_rate/exchange_rate)*
                          (1-2*inaccurate_forecast_extreme_cold_i1234)
    # Benefit for cow intervention 3
    cow_benefiti3<-rep(0, n_years)
    cow_benefiti3[1:5]<-(risk_extreme_cold*
                           vv(total_cow_i1234,var_CV_high,5)*
                           vv(price_cow_i1234,var_CV_high,5)*
                           vv(reduced_death_animal_i3,var_CV_high,5)*
                           animal_rate_i3*effective_rate/exchange_rate)*
                          (1-2*inaccurate_forecast_extreme_cold_i1234)
    # Benefit for cow intervention 4
    cow_benefiti4<-rep(0, n_years)
    cow_benefiti4[1:5]<-(risk_extreme_cold*
                           vv(total_cow_i1234,var_CV_high,5)*
                           vv(price_cow_i1234,var_CV_high,5)*
                           vv(reduced_death_animal_i4,var_CV_high,5)*
                           animal_rate_i4*effective_rate/exchange_rate)*
                           (1-2*inaccurate_forecast_extreme_cold_i1234)
        
  # II. ENVIRONMENT####
    # Benefit for environment intervention 1 with good forecast assumption
    # 1. Fish benefits####
    #Assuming that % farmers who apply plant protection advices will not harm the fishes
    fish_benefiti1<-rep(0,n_years)
    fish_benefiti1[1:5]<-vv(area_pond_impacted_i1234,var_CV_high,5)*
    vv(lost_fish_i1234,var_CV_high,5)*vv(chance_reduced_risk_fish_death, var_CV_high,5)*
      rate_fer_pla_i1*effective_rate/exchange_rate
    #2. Water benefit####
    water_benefiti1<-rep(0,n_years)
    water_benefiti1[1:5]<-vv(reduced_water_expenditure_i1234,var_CV_high,5)*
      vv(total_households_i1234,var_CV,5)*
      vv(percent_pollution_reduction_i1234,var_CV_high,5)*
      rate_fer_pla_i1*effective_rate/exchange_rate
    # Total environmental benefit intervention 1 
    env_benefiti1<-fish_benefiti1+water_benefiti1
    
    # Benefit for environment intervention 2
    # Fish benefits: Assuming that % farmers who apply plant protection advices will not harm the fishes
    fish_benefiti2<-rep(0,n_years)
    fish_benefiti2[1:5]<-vv(area_pond_impacted_i1234,var_CV_high,5)*
      vv(lost_fish_i1234,var_CV_high,5)*vv(chance_reduced_risk_fish_death, var_CV_high,5)*
      rate_fer_pla_i2*effective_rate/exchange_rate
    #water benefit
    water_benefiti2<-rep(0,n_years)
    water_benefiti2[1:5]<-vv(reduced_water_expenditure_i1234,var_CV_high,5)*
      vv(total_households_i1234,var_CV,5)*
      vv(percent_pollution_reduction_i1234,var_CV_high,5)*
      rate_fer_pla_i2*effective_rate/exchange_rate
    # Total environmental benefit intervention 2
    env_benefiti2<-fish_benefiti2+water_benefiti2
     
    
    # Benefit for environment intervention 3
    # Fish benefits: Assuming that % farmers who apply plant protection advices will not harm the fishes
    fish_benefiti3<-rep(0,n_years)
    fish_benefiti3[1:5]<-vv(area_pond_impacted_i1234,var_CV_high,5)*
      vv(chance_reduced_risk_fish_death, var_CV_high,5)*
      vv(lost_fish_i1234,var_CV_high,5)*
      rate_fer_pla_i3*effective_rate/exchange_rate
    #water benefit
    water_benefiti3<-rep(0,n_years)
    water_benefiti3[1:5]<-vv(reduced_water_expenditure_i1234,var_CV_high,5)*
      vv(total_households_i1234,var_CV,5)*
      vv(percent_pollution_reduction_i1234,var_CV_high,5)*
      rate_fer_pla_i3*effective_rate/exchange_rate
    # Total environmental benefit 3 
    env_benefiti3<-fish_benefiti3+water_benefiti3
    
    # Benefit for environment intervention 4
    # Fish benefits: Assuming that % farmers who apply plant protection advices will not 
    #harm the fishes
    fish_benefiti4<-rep(0,n_years)
    fish_benefiti4[1:5]<-vv(area_pond_impacted_i1234,var_CV_high,5)*
      vv(chance_reduced_risk_fish_death, var_CV_high,5)*
      vv(lost_fish_i1234,var_CV_high,5)*
      rate_fer_pla_i4*effective_rate/exchange_rate
    #water benefit
    water_benefiti4<-rep(0,n_years)
    water_benefiti4[1:5]<-vv(reduced_water_expenditure_i1234,var_CV_high,5)*
      vv(total_households_i1234,var_CV,5)*
      vv(percent_pollution_reduction_i1234,var_CV_high,5)*
      rate_fer_pla_i4*effective_rate/exchange_rate
    # Total environmental benefit 4
    env_benefiti4<-fish_benefiti4+water_benefiti4
    
  # III. SOCIAL IMPACTS####
    # Benefit for social impacts intervention 1
    gender_benefiti1<-rep(0,n_years)
    gender_benefiti1[1]<-0
    gender_benefiti1[2:5]<-(vv(new_income_farm_peryear_i12,var_CV_high,4)*
    vv(rate_farm,var_CV_high,4)*total_farm_households_i1234[2:5]+
    vv(new_income_nonfarm_peryear_i12, var_CV_high,4)*
    vv(rate_nonfarm,var_CV_high,4)*total_farm_households_i1234[2:5])*gender_coverage/exchange_rate
      
    # Benefit for social impacts intervention 2
    gender_benefiti2<-gender_benefiti1
    
    # Benefit for social impacts intervention 3
    gender_benefiti3<-rep(0,n_years)
    gender_benefiti3[1:5]<-0
    
    # Benefit for social impacts intervention 4
    gender_benefiti4<-rep(0,n_years)
    gender_benefiti4[1:5]<-0
    
  # IV.HEALTH IMPACTS####
    
    # Reduced expenditure on health
    health_impacti1<-rep(0,n_years)
    health_impacti1[1:5]<-(vv(reduced_expenditure_health_i1234/5,var_CV_high,5)*
                             vv(total_farm_households_i1234,var_CV,5)*
                             vv(percent_pollution_reduction_i1234,var_CV_high,5)*
                             rate_fer_pla_i1*effective_rate/exchange_rate)
    
    # total human impact intervention 1
    health_impacti1
    
    # Benefit for health intervention 2
    health_impacti2<-rep(0,n_years)
    health_impacti2[1:5]<-(vv(reduced_expenditure_health_i1234/5,var_CV_high,5)*
                             vv(total_farm_households_i1234,var_CV,5)*
                             vv(percent_pollution_reduction_i1234,var_CV_high,5)*
                             rate_fer_pla_i2*effective_rate/exchange_rate)
    
    # total human impact intervention 2
    health_impacti2
    
    
    # Benefit for human intervention 3
    # Reduced expenditure on health
    health_impacti3<-rep(0,n_years)
    health_impacti3[1:5]<-(vv(reduced_expenditure_health_i1234/5,var_CV_high,5)*
                             vv(total_farm_households_i1234,var_CV,5)*
                             vv(percent_pollution_reduction_i1234,var_CV_high,5)*
                             rate_fer_pla_i3*effective_rate/exchange_rate)
    
    # total human impact intervention 3
    health_impacti3
    
    # Benefit for human intervention 4
    # Reduced expenditure on health
    health_impacti4<-rep(0,n_years)
    health_impacti4[1:5]<-(vv(reduced_expenditure_health_i1234/5,var_CV_high,5)*
                             vv(total_farm_households_i1234,var_CV,5)*
                             vv(percent_pollution_reduction_i1234,var_CV_high,5)*
                             rate_fer_pla_i4*effective_rate/exchange_rate)
    
    # total human impact intervention 4
    health_impacti4
    
  # V.GHG emission reduction####
    #GHG reduction intervention 1
    GHG_impactsi1<-rep(0,n_years)
    GHG_impactsi1[1:5]<-(vv(methan_reduction_co2eq,var_CV_high,5)+
                      vv(nito_oxide_reduction_co2eq,var_CV_high,5))*
                      vv(total_rice_area_i1234,var_CV, 5)*
                      vv(carbon_price,var_CV_high,5)*
                      rate_fer_pla_i1*effective_rate/exchange_rate
    #GHG reduction intervention 2
    GHG_impactsi2<-rep(0,n_years)
    GHG_impactsi2[1:5]<-(vv(methan_reduction_co2eq,var_CV_high,5)+
                           vv(nito_oxide_reduction_co2eq,var_CV_high,5))*
                            vv(total_rice_area_i1234,var_CV, 5)*
                            vv(carbon_price,var_CV_high,5)*
                      rate_fer_pla_i2*effective_rate/exchange_rate
    #GHG reduction intervention 3
    GHG_impactsi3<-rep(0,n_years)
    GHG_impactsi3[1:5]<-(vv(methan_reduction_co2eq,var_CV_high,5)+
                           vv(nito_oxide_reduction_co2eq,var_CV_high,5))*
                            vv(total_rice_area_i1234,var_CV, 5)*
                            vv(carbon_price,var_CV_high,5)*
                            rate_fer_pla_i3*effective_rate/exchange_rate
    #GHG reduction intervention 4
    GHG_impactsi4<-rep(0,n_years)
    GHG_impactsi4[1:5]<-(vv(methan_reduction_co2eq,var_CV_high,5)+
                           vv(nito_oxide_reduction_co2eq,var_CV_high,5))*
                            vv(total_rice_area_i1234,var_CV, 5)*
                            vv(carbon_price,var_CV_high,5)*
                            rate_fer_pla_i4*effective_rate/exchange_rate
    
    # Total benefit intervention 1####
    total_benefiti1<-total_rice_i1+buffalo_benefiti1+cow_benefiti1+env_benefiti1+
      gender_benefiti1+health_impacti1+GHG_impactsi1
    
    # Annual bottom-line benefit intervention 1
    bottomline_benefiti1=total_benefiti1-total_cost_i1
    
    # Annual cash flow intervention 1
    cash_flowi1<-discount(bottomline_benefiti1, discount_rate, calculate_NPV = FALSE)
    cum_cash_flowi1<-cumsum(cash_flowi1)
    
    # NPV Intervention 1####
    NPV1<-discount(bottomline_benefiti1, discount_rate, calculate_NPV = TRUE)
    
    # Benefit-cost ratio intervention 1####
    # Benefit-cost ratio is the discounted value of the project's benefits divided 
    # by the discounted value of the project's costs
    #discount total cost https://bizfluent.com/how-6200049-calculate-benefit-cost-ratio.html
    discount_total_benefit_i1<-discount(total_benefiti1,discount_rate, calculate_NPV = TRUE)
    discount_total_cost_i1<-discount(total_cost_i1, discount_rate, calculate_NPV = TRUE)
    bcri1<-discount_total_benefit_i1/discount_total_cost_i1
   
     #NPV economic Intervention 1####
    total_cost_i1e<-cost_new_met_station+cost_forecast_province+
      cost_translation+cost_capacity_communication+cost_rice_SMS+
      cost_animal_SMS+cost_model+ME_cost
    
    total_benefiti1e<-total_rice_i1+buffalo_benefiti1+cow_benefiti1
    bottomline_benefiti1e=total_benefiti1e-total_cost_i1e
    NPV1e<-discount(bottomline_benefiti1e, discount_rate, calculate_NPV = TRUE)
    
    #BCR Economic Intervention 1####
    discount_total_benefit_i1e<-discount(total_benefiti1e,discount_rate, calculate_NPV = TRUE)
    discount_total_cost_i1e<-discount(total_cost_i1e, discount_rate, calculate_NPV = TRUE)
    bcri1e<-discount_total_benefit_i1e/discount_total_cost_i1e
    
    
    # Direct farm benefit for each rice household intervention 1####
    #ricebenefit_household1<-total_rice_i1/total_farm_households_i1234
    
    # Direct farm benefit for each animal household intervention 1####
    #animalbenefit_household1<-(buffalo_benefiti1+cow_benefiti1)/
      #(percent_animal_households_i1234*total_farm_households_i1234)
    
    # Total benefit intervention 2####
    total_benefiti2<-total_rice_i2+buffalo_benefiti2+cow_benefiti2+env_benefiti2+
      gender_benefiti2+health_impacti2+GHG_impactsi2
    # Annual bottom-line benefit intervention 2
    bottomline_benefiti2=total_benefiti2-total_cost_i2
    # Annual cash flow intervention 2
    cash_flowi2<-discount(bottomline_benefiti2, discount_rate, calculate_NPV = FALSE)
    cum_cash_flowi2<-cumsum(cash_flowi2)
    # NPV Intervention 2####
    NPV2<-discount(bottomline_benefiti2, discount_rate, calculate_NPV = TRUE)
    
    # Benefit-cost ratio intervention 2####
    discount_total_benefit_i2<-discount(total_benefiti2,discount_rate, calculate_NPV = TRUE)
    discount_total_cost_i2<-discount(total_cost_i2, discount_rate, calculate_NPV = TRUE)
    bcri2<-discount_total_benefit_i2/discount_total_cost_i2
    
    #NPV economic Intervention 2####
    total_cost_i2e<-cost_forecast_province+cost_translation+
      cost_capacity_communication+cost_rice_SMS+cost_animal_SMS+
      cost_model+ME_cost
   
    total_benefiti2e<-total_rice_i2+buffalo_benefiti2+cow_benefiti2
    bottomline_benefiti2e=total_benefiti2e-total_cost_i2e
    NPV2e<-discount(bottomline_benefiti2e, discount_rate, calculate_NPV = TRUE)
    #BCR Economic Intervention 2####
    discount_total_benefit_i2e<-discount(total_benefiti2e,discount_rate, calculate_NPV = TRUE)
    discount_total_cost_i2e<-discount(total_cost_i2e, discount_rate, calculate_NPV = TRUE)
    bcri2e<-discount_total_benefit_i2e/discount_total_cost_i2e
    
    # Direct farm benefit for each rice household####
    #ricebenefit_household2<-total_rice_i2/total_farm_households_i1234
    
    # Direct farm benefit for each animal household####
    #animalbenefit_household2<-(buffalo_benefiti2+cow_benefiti2)/
      #percent_animal_households_i1234*total_farm_households_i1234
    
    # Total benefits intervention 3
    total_benefiti3<-total_rice_i3+buffalo_benefiti3+cow_benefiti3+env_benefiti3+
      +gender_benefiti3+health_impacti3+GHG_impactsi3
    # Annual bottom-line benefit intervention 3
    bottomline_benefiti3=total_benefiti3-total_cost_i3
    # Annual cash flow intervention 3
    cash_flowi3<-discount(bottomline_benefiti3, discount_rate, calculate_NPV = FALSE)
    cum_cash_flowi3<-cumsum(cash_flowi3)
    # NPV Intervention 3####
    NPV3<-discount(bottomline_benefiti3, discount_rate, calculate_NPV = TRUE)
    
    # Benefit-cost ratio intervention 3####
    discount_total_benefit_i3<-discount(total_benefiti3,discount_rate, calculate_NPV = TRUE)
    discount_total_cost_i3<-discount(total_cost_i3, discount_rate, calculate_NPV = TRUE)
    bcri3<-discount_total_benefit_i3/discount_total_cost_i3
    
    
    #NPV economic Intervention 3####
    #Total cost economic is equal total cost intervention 3
    
    total_benefiti3e<-total_rice_i3+buffalo_benefiti3+cow_benefiti3
    bottomline_benefiti3e=total_benefiti3e-total_cost_i3
    NPV3e<-discount(bottomline_benefiti3e, discount_rate, calculate_NPV = TRUE)
    
    #BCR Economic Intervention 3####
    discount_total_benefit_i3e<-discount(total_benefiti3e,discount_rate, calculate_NPV = TRUE)
    bcri3e<-discount_total_benefit_i3e/discount_total_cost_i3
    
    # Direct farm benefit for each rice household#### 
    #Need to consider the adopted only####
    #ricebenefit_household3<-total_rice_i3/total_farm_households_i1234
    
    # Direct farm benefit for each animal household####
    #animalbenefit_household3<-(buffalo_benefiti3+cow_benefiti3)/
     # (percent_animal_households_i1234*total_farm_households_i1234)
    
    # Total benefits intervention 4####
    total_benefiti4<-total_rice_i4+buffalo_benefiti4+cow_benefiti4+env_benefiti4+
      gender_benefiti4+health_impacti4+GHG_impactsi4
    # Annual bottom-line benefit intervention 4
    bottomline_benefiti4=total_benefiti4-total_cost_i4
    # Annual cash flow intervention 4
    cash_flowi4<-discount(bottomline_benefiti4, discount_rate, calculate_NPV = FALSE)
    cum_cash_flowi4<-cumsum(cash_flowi4)
    # NPV Intervention 4####
    NPV4<-discount(bottomline_benefiti4, discount_rate, calculate_NPV = TRUE)
    
    # Benefit-cost ratio intervention 4####
    discount_total_benefit_i4<-discount(total_benefiti4,discount_rate, calculate_NPV = TRUE)
    discount_total_cost_i4<-discount(total_cost_i4, discount_rate, calculate_NPV = TRUE)
    bcri4<-discount_total_benefit_i4/discount_total_cost_i4
    
    #NPV economic Intervention 4####
    #Total cost economic is equal to total cost of intervention4
    total_benefiti4e<-total_rice_i4+buffalo_benefiti4+cow_benefiti4
    bottomline_benefiti4e=total_benefiti4e-total_cost_i4
    NPV4e<-discount(bottomline_benefiti4e, discount_rate, calculate_NPV = TRUE)
    #BCR Economic Intervention 4####
    discount_total_benefit_i4e<-discount(total_benefiti4e,discount_rate, calculate_NPV = TRUE)
    bcri4e<-discount_total_benefit_i4e/discount_total_cost_i4
    
    # Direct farm benefit for each rice household####
    #ricebenefit_household4<-total_rice_i4/total_farm_households_i1234
    # Direct farm benefit for each animal household####
    #animalbenefit_household4<-(buffalo_benefiti4+cow_benefiti4)/
      #(percent_animal_households_i1234*total_farm_households_i1234)
    
    #Compare option 3 and option 1 and 2
    option3_option1<-NPV3-NPV1
    option3_option2<-NPV3-NPV2
    
    return(list(
          NPV_Intervention1<-NPV1,
          NPV_Intervention2<-NPV2,
          NPV_Intervention3<-NPV3,
          NPV_Intervention4<-NPV4,
          option3_option1<-NPV3-NPV1,
          option3_option2<-NPV3-NPV2,
          Benefit_Cost_Ratio_Intervention1<-bcri1, 
          Benefit_Cost_Ratio_Intervention2<-bcri2,
          Benefit_Cost_Ratio_Intervention3<-bcri3, 
          Benefit_Cost_Ratio_Intervention4<-bcri4,
          cum_cash_Intervention1<-cum_cash_flowi1,
          cum_cash_Intervention2<-cum_cash_flowi2, 
          cum_cash_Intervention3<-cum_cash_flowi3, 
          cum_cash_Intervention4<-cum_cash_flowi4,
          total_costs_intervention1<-total_cost_i1,
          total_costs_intervention2<-total_cost_i2,
          total_costs_intervention3<-total_cost_i3,
          total_costs_intervention4<-total_cost_i4,
          NPV_economic_Intervention1<-NPV1e,
          NPV_economic_Intervention2<-NPV2e,
          NPV_economic_Intervention3<-NPV3e,
          NPV_economic_Intervention4<-NPV4e,
          Benefit_Cost_Ratio_Intervention1e<-bcri1e,
          Benefit_Cost_Ratio_Intervention2e<-bcri2e,
          Benefit_Cost_Ratio_Intervention3e<-bcri3e,
          Benefit_Cost_Ratio_Intervention4e<-bcri4e))
    }
  
  # Running the model ####
  decisionSupport::decisionSupport(
    "acis_inputs_EN.csv",
    outputPath = paste("MCResults",sep=""),
    welfareFunction = acis_costbenefit,
    numberOfModelRuns = 1e4, #run 10,000 times
    functionSyntax = "plainNames")
  

  
  
 
 
