
#install.packages("decisionSupport")
library(decisionSupport)
#no comma in number in dataset

make_variables<-function(est,n=1)
{
x<-random(rho=est,n=n)
for(i in colnames(x))
  assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(estimate_read_csv("acis_inputs_EN_check.csv"))

acis_costbenefit <- function(x, varnames){
  
  # i1: risk/cost/benefit for intervention1
  # i2: risk/cost/benefit for intervention1
  # i3: risk/cost/benefit for intervention1
  # i4: risk/cost/benefit for intervention4
  # i12: risk/cost/benefit for intervention12
  # i234: risk/cost/benefit for intervention234
  # i34: risk/cost/benefit for intervention34
  # i1234: risk/cost/benefit for intervention1234

# RISKS THAT IMPACTS BENEFITS
  # Incorporating into the benefit functions: Risks include: Chance for 
  # drought, cold spell, heavy rain
  # inaccurate forecast is one risk but it is on-going every season/year
  
  
# CALCULATING COSTS####
  
  # I. Forecast generation####
  # 1. Set up new local met station for the whole project period
  cost_new_met_station<-rep(0,n_years)
  cost_new_met_station[1]<-(met_station_esta_i1+vv(met_station_main_i1, var_CV,1)+
    cost_forecasts_access_i1)/exchange_rate
  cost_new_met_station[2:5]<-vv(met_station_main_i1,var_CV,4)/exchange_rate
  
  # 2. Buy forecast from provincial met station for the whole project period
  cost_forecast_province<-rep(0,n_years)
  cost_forecast_province[1:5]<-(cost_weekly_forecasts_i1234+
                                  cost_seasonal_forecasts_i1234)/exchange_rate
  #II. Translation####
  
  # 3. Translation from forecasts (training on translation) to advisory costs for the whole project period
 
  cost_translation<-rep(0,n_years)
  cost_translation[1:2]<-vv(cost_cb_translation_staff_i1234, var_CV,2)*
    cb_translation_n12/exchange_rate
  cost_translation[3:5]<-vv(cost_cb_translation_staff_i1234, var_CV,3)*
    cb_translation_n234/exchange_rate
  #III. Transfer####
  
  # 4. Transfer and communication costs: district and commune/village for 
  #the whole project period
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
  
  # 5. Cost for sending SMS to rice farmers for the whole project period
  cost_rice_SMS<- rep(0,n_years)
  cost_rice_SMS[1:5]<-vv(cost_per_SMS_i123,var_CV,5)*
    vv(messages_per_time_rice_i123, var_CV,5)*
    vv(number_times_per_year_rice_i123,var_CV,5)*total_farm_households_i1234/
    exchange_rate
    
   cost_animal_SMS<-rep(0,n_years)
   cost_animal_SMS[1:5]<-vv(cost_per_SMS_i123,var_CV,5)*
     vv(messages_per_time_animal_i123,var_CV,5)*
    vv(number_times_per_year_animal_i12, var_CV,5)*
     vv(percent_animal_households_i1234,var_CV,5)*
      vv(total_farm_households_i1234, var_CV,5)/exchange_rate
     
  #6. Cost for sending SMS to village leader for the whole project period
   cost_SMS_villageleader<- rep(0,n_years)
   cost_SMS_villageleader[1:5]<-vv(cost_per_SMS_i123,var_CV,5)*
     vv(messages_per_time_rice_i123, var_CV,5)*
     vv(number_times_per_year_rice_i123,var_CV,5)*commune_village_SMS_i3/
     exchange_rate+
     vv(cost_per_SMS_i123,var_CV,5)*
     vv(messages_per_time_animal_i123,var_CV,5)*
     vv(number_times_per_year_animal_i123, var_CV,5)*
     commune_village_SMS_i3/exchange_rate 
     
   # 7. Allowance for village leaders for the whole project period
   cost_allowance_village_leader<-rep(0,n_years)
   cost_allowance_village_leader[1:5]<-n_village*
     vv(allowance_village_leader_permonth_loud_i34,var_CV,5)*
     months_per_year/exchange_rate
  
   # 8. Cost_collect_bulletin
   cost_collect_bulletin<-rep(0,n_years)
   cost_collect_bulletin[1:5]<-(allowance_bulletin_collect_time_short_i4*
          vv(percent_short_distance_i4,var_CV,5)*
            times_per_month*months_per_year*n_village)/exchange_rate+
    (allowance_bulletin_collect_time_long_i4*
       vv(percent_long_distance_i4,var_CV,5)*
       times_per_month*months_per_year*n_village)/exchange_rate 
   
   # IV. Support the use and learning####
   
   # 9. Establish demonstration models (5000m2/season, 2models of 1ha/year) 
   # for the whole project period
   cost_per_modeli1234<-((seed_guideline_i1234*price_seed_i1234+
              plant_protection_support_i1234)/exchange_rate+
     fa_adv_cost_perha*percent_fertilizer_model_supporti1234)/2
     
   
   cost_model<-rep(0,n_years)
   cost_model[1:5]<-(model_training_i1234*n_training+no_model_compare_i1234*
   cost_per_modeli1234+model_monitor_i1234+field_visit_i1234)/
     exchange_rate
   
   # 10. cost for gender intervention social action and analysis SAA (training, dialogue, commnity events)
   # for the whole project period
   cost_gender<-rep(0,n_years)
   cost_gender[1]<-(SAA_TOT+dialogue_village*n_dialogues*n_village+
                        cost_community_event*n_village)/exchange_rate 
   cost_gender[2:5]<-0
   #V. Monitoring####
   # 11. Monitoring and Evaluation for the whole project period
   ME_cost<-rep(0,n_years)
   ME_cost[0:2]<-0
   ME_cost[3]<-(ME_district_i1234+ME_commune_i1234*n_communes)/exchange_rate
   ME_cost[5]<-(ME_district_i1234+ME_commune_i1234*n_communes)/exchange_rate
  
  # Total cost for invetervention 1####
   total_cost_i1<-cost_new_met_station+cost_forecast_province+
     cost_translation+cost_capacity_communication+cost_rice_SMS+
     cost_animal_SMS+cost_model+cost_gender+ME_cost
  
  # Total cost for invetervention 2####
   total_cost_i2<-cost_forecast_province+cost_translation+
     cost_capacity_communication+cost_rice_SMS+cost_animal_SMS+
     cost_model+cost_gender+ME_cost
  # Total cost for invetervention 3####
   total_cost_i3<-cost_forecast_province+cost_translation+
     cost_capacity_communication+cost_SMS_villageleader+
     cost_allowance_village_leader+cost_model+ME_cost
                                
   # Total cost for invetervention 4####
   total_cost_i4<-cost_forecast_province+cost_translation+
     cost_capacity_communication+cost_collect_bulletin+
     cost_allowance_village_leader+cost_model+ME_cost
                                            
# CALCULATING BENEFITS####
 
  #I. ECONOMIC BENEFITS####
  #1. Rice benefits####
  #for all interventions in the good weather forecast conditions
  #1.1. Advisories to cope with extreme drought####
  #calculating drought risk for each year
   drought_risk_i1234<-chance_event(chance_drought_i1234,value_if = 1, n=n_years)
  # drought area for each year 
   rice_drought_i1234<-rep(0,n_years)
   rice_drought_i1234[1:5]<-rice_area_drought_i1234*drought_risk_i1234
  # rice area that are not effected by drought
   rice_area_effect<-rep(0,n_year)
   rice_area_effect[1:5]<-vv(total_rice_area_i1234,var_CV, 5)-
     vv(rice_drought_i1234,var_CV, 5)
  
  # Avoid losses due to drought for the whole project period
  
   reduced_drought_losses<-rep(0, n_years)
   reduced_drought_losses[1]<-0
   reduced_drought_losses[1:4]<-vv(reduce_loss_drought_i1234,var_CV, 4)*
     vv(rice_drought_i1234,var_CV_high, 4)/exchange_rate

  # 1.2. Seeds benefit####
  # Seed dose reduction
  
     seed_dose_reduction_perha<-rep(0,n_years)
     seed_dose_reduction_perha[1:5]<-((seed_farmers_i1234-seed_guideline_i1234)*
       vv(price_seed_i1234, var_CV, 5)+vv(labor_pick_i1234,var_CV, 5))/exchange_rate
  
  # Total seed dose reduction benefit intervention 1 
  # seeding adotion rate intervention 1
  # Write the function for adoption, if adoption reachs to the peak,#use saturated rate 
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
     
  # 1.2.1. Benefit from dose reduction####
     benefit_dose_seed_i1<-rep(0,n_years)
     benefit_dose_seed_i1[1:5]<-seed_dose_reduction_perha*rice_area_effect*
       seed_rate_i1*effective_rate 
  # Total seed dose reduction benefit intervention 2
  # seeding adotion rate intervention 2
     
     seed_rate_i2<-adoptionrate(rate_seed_inno_i2,
                                rate_seed_imitation_i2,n_years,
                                dis_adoption_i123,rate_saturated_i12)
     
  # benefit from dose reduction   
     benefit_dose_seed_i2<-rep(0,n_years)
     benefit_dose_seed_i2[1:5]<-seed_dose_reduction_perha*rice_area_effect*
       seed_rate_i2*effective_rate
  # Total seed dose reduction benefit intervention 3 
     # seeding adoption rate intervention 3
     seed_rate_i3<-adoptionrate(rate_seed_inno_i3,
                                rate_seed_imitation_i3,n_years,
                                dis_adoption_i123,rate_saturated_i34)
     # benefit from dose reduction   
     benefit_dose_seed_i3<-rep(0,n_years)
     benefit_dose_seed_i3[1:5]<-seed_dose_reduction_perha*rice_area_effect*
       seed_rate_i3*effective_rate
  # Total seed dose reduction benefit intervention 4 
     # seeding adotion rate intervention 4
     seed_rate_i4<-adoptionrate(rate_seed_inno_i4,
                                rate_seed_imitation_i4,n_years,
                                dis_adoption_i4,rate_saturated_i34)
     # benefit from dose reduction   
     benefit_dose_seed_i4<-rep(0,n_years)
     benefit_dose_seed_i4[1:5]<-seed_dose_reduction_perha*rice_area_effect*
       seed_rate_i4*effective_rate
    # 1.2.2. Benefits from reduced times of re-seeding####
    
    # risk of having extreme weather affecting re-seeding
     
     risk_rain_cold_i1234<-chance_event(chance_rain_cold_i1234,value_if = 1, n=n_years)
    # taking into consideration of the chance to reduce the risk
     times_reseed_reduced<-risk_rain_cold_i1234*percent_reduce_reseed_times_i1234
     
    # Benefit for time reduction in seeding for intervention 1 
     benefit_time_seed_i1<-rep(0,n_years)
     benefit_time_seed_i1[1:5]<-times_reseed_reduced* 
              reduce_reseed_costsperha_i1234*rice_area_effect*
              vv(reseed_area_percentage_i1234,var_CV_high,5)*
       seed_rate_i1/exchange_rate
          
          
  # Benefit for time reduction in seeding for intervention 2 
     benefit_time_seed_i2<-rep(0,n_years)
     benefit_time_seed_i2[1:5]<-times_reseed_reduced* 
       reduce_reseed_costsperha_i1234*rice_area_effect*
       vv(reseed_area_percentage_i1234,var_CV_high,5)*
       seed_rate_i2/exchange_rate
     
  # Benefit for time reduction in seeding for intervention 3 
     benefit_time_seed_i3<-rep(0,n_years)
     benefit_time_seed_i3[1:5]<-times_reseed_reduced* 
       reduce_reseed_costsperha_i1234*rice_area_effect*
       vv(reseed_area_percentage_i1234,var_CV_high,5)*
       seed_rate_i3/exchange_rate
  # Benefit for time reduction in seeding for intervention 4 
     benefit_time_seed_i4<-rep(0,n_years)
     benefit_time_seed_i4[1:5]<-times_reseed_reduced* 
       reduce_reseed_costsperha_i1234*rice_area_effect*
       vv(reseed_area_percentage_i1234,var_CV_high,5)*
       seed_rate_i4/exchange_rate
    
  # 1.3 Fertilizer benefits####
  # 1.3.1. Dose reduction####
  # for fertilizer per season (4 times fertilizing) per ha
     
     benefit_dose_fer_perha<-((NPK5105f_dose_i1234 -NPK5105a_dose_i1234)*
          NPK5105_price_i1234+
          (NPK12510f_dose_i1234-NPK12510a_dose_i1234)*NPK12510_price_i1234+
          (Nf_dose_i1234-Na_dose_i1234)*N_price_i1234+
          (Kf_dose_i1234-Ka_dose_i1234)*K_price_i1234)/exchange_rate
     
  # Benefit for dose reduction for intervention 1 
     # ferlizer and plant protection adotion rate 
     rate_fer_pla_i1<- adoptionrate(rate_fer_pla_inno_i1,
                      rate_fer_pla_immi_i1,n_years,
                      dis_adoption_i123,rate_saturated_i12)
    # benefit
     benefit_dose_fer_i1<-rep(0,n_years)
     benefit_dose_fer_i1[1:5]<-benefit_dose_fer_perha*rice_area_effect*
        rate_fer_pla_i1
      
  # Benefit for dose reduction for intervention 2
     # ferlizer and plant protection adotion rate 
     rate_fer_pla_i2<-adoptionrate(rate_fer_pla_inno_i2,
                        rate_fer_pla_immi_i2,n_years,
                        dis_adoption_i123,rate_saturated_i12)
     # benefit
     benefit_dose_fer_i2<-rep(0,n_years)
     benefit_dose_fer_i2[1:5]<-benefit_dose_fer_perha*rice_area_effect*
       rate_fer_pla_i2
      
  # Benefit for dose reduction for intervention 3 
     # ferlizer and plant protection adotion rate 
     rate_fer_pla_i3<-adoptionrate(rate_fer_pla_inno_i3,
                        rate_fer_pla_immi_i3,n_years,
                        dis_adoption_i123,rate_saturated_i34)
     # benefit
     benefit_dose_fer_i3<-rep(0,n_years)
     benefit_dose_fer_i3[1:5]<-benefit_dose_fer_perha*rice_area_effect*
       rate_fer_pla_i3
      
  # Benefit for dose reduction for intervention 4    
     # ferlizer and plant protection adotion rate 
     rate_fer_pla_i4<-adoptionrate(rate_fer_pla_inno_i4,
                        rate_fer_pla_immi_i4,n_years,
                        dis_adoption_i4,rate_saturated_i34)
     # benefit
     benefit_dose_fer_i4<-rep(0,n_years)
     benefit_dose_fer_i4[1:5]<-benefit_dose_fer_perha*rice_area_effect*
       rate_fer_pla_i4
     
           
  # 1.3.2. Benefit for fertilizing times reduction#### 
  # chance reduces re-fertilize
     
     refertilize_reduced_i1234<-chance_event(chance_refertilize_reduced_i1234,
                                             value_if = 1,n=n_years)
     
  # cost for fertlizing for one year for one ha using farmers'dose
     
     fa_fer_cost_perha<-(NPK5105f_dose_i1234*NPK5105_price_i1234+
                      NPK12510f_dose_i1234*NPK12510_price_i1234+
                      Nf_dose_i1234*N_price_i1234+
                      Kf_dose_i1234*K_price_i1234)/exchange_rate
  # cost for fertlizing for one year for one ha using adivosies'dose
     fa_adv_cost_perha<-(NPK5105a_dose_i1234*NPK5105_price_i1234+
                      NPK12510a_dose_i1234*NPK12510_price_i1234+
                      Na_dose_i1234*N_price_i1234+
                      Ka_dose_i1234*K_price_i1234)/exchange_rate
     
  # Benefit for time reduction for intervention 1 
     benefit_time_fer_i1<-rep(0,n_years)
     benefit_time_fer_i1[1:5]<-refertilize_reduced_i1234*fa_fer_cost_perha*
     rice_area_effect*vv(refertilize_area_percentage_i1234, var_CV_high,5)*
       rate_fer_pla_i1
  # Benefit for time reduction for intervention 2 
     benefit_time_fer_i2<-rep(0,n_years)
     benefit_time_fer_i2[1:5]<-refertilize_reduced_i1234*fa_fer_cost_perha*
       rice_area_effect*vv(refertilize_area_percentage_i1234, var_CV_high,5)*
       rate_fer_pla_i2
    
  # Benefit for time reduction for intervention 3
     benefit_time_fer_i3<-rep(0,n_years)
     benefit_time_fer_i3[1:5]<-refertilize_reduced_i1234*fa_fer_cost_perha*
       rice_area_effect*vv(refertilize_area_percentage_i1234, var_CV_high,5)*
       rate_fer_pla_i3
         
  # Benefit for time reduction for intervention 4
     benefit_time_fer_i4<-rep(0,n_years)
     benefit_time_fer_i4[1:5]<-refertilize_reduced_i1234*fa_fer_cost_perha*
       rice_area_effect*vv(refertilize_area_percentage_i1234, var_CV_high,5)*
       rate_fer_pla_i4
          
  # 1.4. Plant protection benefits####
   
  # Benefit effective combined dose and timing in applying plant protection substances 
     
  # Reduced time sprayed per year
     
    reduce_times_spray_peryear_i1234<-reduced_times_spray_i1234/n_years
     
   # Benefit for time reduction for intervention 1 
    benefit_time_spray_i1<-rep(0,n_years)
      benefit_time_spray_i1[1:5]<-reduce_times_spray_peryear_i1234*
      reduced_cost_per_time_spray_i1234*
      rice_area_effect*rate_fer_pla_i1/exchange_rate
         
  # Benefit for time reduction for intervention 2
      benefit_time_spray_i2<-rep(0,n_years)
      benefit_time_spray_i2[1:5]<-reduce_times_spray_peryear_i1234*
        reduced_cost_per_time_spray_i1234*
        rice_area_effect*rate_fer_pla_i2/exchange_rate
         
  # Benefit for time reduction for intervention 3
      benefit_time_spray_i3<-rep(0,n_years)
      benefit_time_spray_i3[1:5]<-reduce_times_spray_peryear_i1234*
        reduced_cost_per_time_spray_i1234*
        rice_area_effect*rate_fer_pla_i3/exchange_rate
         
  # Benefit for time reduction for intervention 4
      benefit_time_spray_i4<-rep(0,n_years)
      benefit_time_spray_i4[1:5]<-reduce_times_spray_peryear_i1234*
        reduced_cost_per_time_spray_i1234*
        rice_area_effect*rate_fer_pla_i4/exchange_rate
  #1.5. Rice yield benefit#### 
      # Rice yield benefit for Intervention 1
      rice_benefit_change_i1<-rep(0,n_year)
      rice_benefit_change_i1[1:5]<-rice_area_effect*
        vv(changed_yield_i1234, var_CV, 5)*rice_price*
        ((rate_farmers_seed_i1+2*rate_fer_pla_i1)/3)*effective_rate/exchange_rate
      # Rice yield benefit for Intervention 2
      rice_benefit_change_i2<-rep(0,n_year)
      rice_benefit_change_i2[1:5]<-rice_area_effect*
        vv(changed_yield_i1234, var_CV, 5)*rice_price*
        ((rate_farmers_seed_i2+2*rate_fer_pla_i2)/3)*effective_rate/exchange_rate
      # Rice yield benefit for Intervention 3
      rice_benefit_change_i3<-rep(0,n_year)
      rice_benefit_change_i3[1:5]<-rice_area_effect*
        vv(changed_yield_i1234, var_CV, 5)*rice_price*
        ((rate_farmers_seed_i3+2*rate_fer_pla_i3)/3)*effective_rate/exchange_rate
      # Rice yield benefit for Intervention 4
      rice_benefit_change_i4<-rep(0,n_year)
      rice_benefit_change_i4[1:5]<-rice_area_effect*
        vv(changed_yield_i1234, var_CV, 5)*rice_price*
        ((rate_farmers_seed_i4+2*rate_fer_pla_i4)/3)*effective_rate/exchange_rate
  #1.6. Discounting uncertainty of forecasts####    
  # Total benefit for rice: This benefit equal to total rice benefit in the 
  # good weather forecasts conditions discounting the inaccurate weather forecasts. 
  # When having negative effect. It does not normally affect all but just a part of that
  # Total benefit for rice intervention 1
  total_rice_i1<-(reduced_drought_losses+
    benefit_dose_seed_i1+benefit_time_seed_i1+
    benefit_dose_fer_i1 +benefit_time_fer_i1+
    benefit_time_spray_i1+rice_benefit_change_i1)*
    (1-inaccurate_forecast_i1*affected_coeff_inaccurate_forecast_i1234)
  # Total benefit for rice intervention 2
  total_rice_i2<-(reduced_drought_losses+
    benefit_dose_seed_i2+benefit_time_seed_i2+
    benefit_dose_fer_i2 +benefit_time_fer_i2+
    benefit_time_spray_i2+rice_benefit_change_i2)*
    (1-inaccurate_forecast_i234*affected_coeff_inaccurate_forecast_i1234)
  # Total benefit for rice intervention 3
  total_rice_i3<-(reduced_drought_losses+
    benefit_dose_seed_i3+benefit_time_seed_i3+
    benefit_dose_fer_i3 +benefit_time_fer_i3+
    benefit_time_spray_i3+rice_benefit_change_i3)*
    (1-inaccurate_forecast_i234*affected_coeff_inaccurate_forecast_i1234)
  # Total benefit for rice intervention 4
  total_rice_i4<-(reduced_drought_losses+
    benefit_dose_seed_i4+benefit_time_seed_i4+
    benefit_dose_fer_i4 +benefit_time_fer_i4+
    benefit_time_spray_i4+rice_benefit_change_i4)*
    (1-inaccurate_forecast_i234*affected_coeff_inaccurate_forecast_i1234)
  
  # 2. Buffalo benefits#### 
  # risk of extreme cold that can affect buffalos and cows
  risk_extreme_cold<-chance_event(chance_extreme_cold,value_if = 1, n=n_years)
  
  # Benefit for buffalo intervention 1
  
  animal_rate_i1<-adoptionrate(rate_ani_inno_i1,
                               rate_ani_immi_i1,n_years,
                               dis_adoption_i123,
                               rate_saturated_i12)
    
  #benefit for buffalo 
  buffalo_benefiti1<-rep(0, n_years)
  buffalo_benefiti1[1:5]<-(risk_extreme_cold*
    vv(total_buffalo_i1234,var_CV_high,5)*
    vv(price_buffalo_i1234,var_CV_high,5)*
    vv(reduced_death_animal_i1,var_CV_high,5)*
      animal_rate_i1/exchange_rate)*
    (1-inaccurate_forecast_i1*affected_coeff_inaccurate_forecast_i1234)
  
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
                             animal_rate_i2/exchange_rate)*
    (1-inaccurate_forecast_i1*affected_coeff_inaccurate_forecast_i1234)
  
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
                             animal_rate_i3/exchange_rate)*
    (1-inaccurate_forecast_i1*affected_coeff_inaccurate_forecast_i1234)
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
                             animal_rate_i4/exchange_rate)*
    (1-inaccurate_forecast_i1*affected_coeff_inaccurate_forecast_i1234)
  # 3. Cow benefits####
  # Benefit for cow intervention 1
  cow_benefiti1<-rep(0, n_years)
  cow_benefiti1[1:5]<-(risk_extreme_cold*
                             vv(total_cow_i1234,var_CV_high,5)*
                             vv(price_cow_i1234,var_CV_high,5)*
                             vv(reduced_death_animal_i1,var_CV_high,5)*
                             animal_rate_i1/exchange_rate)*
    (1-inaccurate_forecast_i1*affected_coeff_inaccurate_forecast_i1234)
  
  # Benefit for cow intervention 2
  cow_benefiti2<-rep(0, n_years)
  cow_benefiti2[1:5]<-(risk_extreme_cold*
                         vv(total_cow_i1234,var_CV_high,5)*
                         vv(price_cow_i1234,var_CV_high,5)*
                         vv(reduced_death_animal_i2,var_CV_high,5)*
                         animal_rate_i2/exchange_rate)*
    (1-inaccurate_forecast_i234*affected_coeff_inaccurate_forecast_i1234)
  # Benefit for cow intervention 3
  cow_benefiti3<-rep(0, n_years)
  cow_benefiti3[1:5]<-(risk_extreme_cold*
                         vv(total_cow_i1234,var_CV_high,5)*
                         vv(price_cow_i1234,var_CV_high,5)*
                         vv(reduced_death_animal_i3,var_CV_high,5)*
                         animal_rate_i3/exchange_rate)*
    (1-inaccurate_forecast_i234*affected_coeff_inaccurate_forecast_i1234)
  # Benefit for cow intervention 4
  cow_benefiti4<-rep(0, n_years)
  cow_benefiti4[1:5]<-(risk_extreme_cold*
                         vv(total_cow_i1234,var_CV_high,5)*
                         vv(price_cow_i1234,var_CV_high,5)*
                         vv(reduced_death_animal_i4,var_CV_high,5)*
                         animal_rate_i4/exchange_rate)*
    (1-inaccurate_forecast_i234*affected_coeff_inaccurate_forecast_i1234)
      
  # II. ENVIRONMENT####
  # Benefit for environment intervention 1 with good forecast assumption
  # 1. Fish benefits####
  #Assuming that % farmers who apply plant protection advices will not harm the fishes
  fish_benefiti1<-rep(0,n_years)
  fish_benefiti1[1:5]<-vv(area_pond_impacted_i1234,var_CV_high,5)*
               vv(lost_fish_i1234,var_CV_high,5)*
    vv(rate_fer_pla_i1,var_CV_high,5)/exchange_rate
  #2. Water benefit####
  water_benefiti1<-rep(0,n_years)
  water_benefiti1[1:5]<-vv(reduced_water_expenditure_i1234,var_CV_high,5)*
    vv(total_households_i1234,var_CV,5)*
    vv(percent_pollution_reduction_i1234,var_CV_high,5)*
    rate_fer_pla_i1/exchange_rate
  # Total environmental benefit intervention 1, discounting the inaccurate forecast 
  env_benefiti1<-(fish_benefiti1+water_benefiti1)*
    (1-inaccurate_forecast_i1*affected_coeff_inaccurate_forecast_i1234)
  
  # Benefit for environment intervention 2
  # Fish benefits: Assuming that % farmers who apply plant protection advices will not harm the fishes
  fish_benefiti2<-rep(0,n_years)
  fish_benefiti2[1:5]<-vv(area_pond_impacted_i1234,var_CV_high,5)*
    vv(lost_fish_i1234,var_CV_high,5)*
    rate_fer_pla_i2/exchange_rate
  #water benefit
  water_benefiti2<-rep(0,n_years)
  water_benefiti2[1:5]<-vv(reduced_water_expenditure_i1234,var_CV_high,5)*
    vv(total_households_i1234,var_CV,5)*
    vv(percent_pollution_reduction_i1234,var_CV_high,5)*
    rate_fer_pla_i2/exchange_rate
  # Total environmental benefit intervention 2
  env_benefiti2<-(fish_benefiti2+water_benefiti2)*
    (1-inaccurate_forecast_i234*affected_coeff_inaccurate_forecast_i1234)
  
  # Benefit for environment intervention 3
  # Fish benefits: Assuming that % farmers who apply plant protection advices will not harm the fishes
  fish_benefiti3<-rep(0,n_years)
  fish_benefiti3[1:5]<-vv(area_pond_impacted_i1234,var_CV_high,5)*
    vv(lost_fish_i1234,var_CV_high,5)*
    rate_fer_pla_i3/exchange_rate
  #water benefit
  water_benefiti3<-rep(0,n_years)
  water_benefiti3[1:5]<-vv(reduced_water_expenditure_i1234,var_CV_high,5)*
    vv(total_households_i1234,var_CV,5)*
    vv(percent_pollution_reduction_i1234,var_CV_high,5)*
    rate_fer_pla_i3/exchange_rate
  # Total environmental benefit, discounting the inaccurate forecast 
  env_benefiti3<-(fish_benefiti3+water_benefiti3)*
    (1-inaccurate_forecast_i234*affected_coeff_inaccurate_forecast_i1234)
  
  # Benefit for environment intervention 4
  # Fish benefits: Assuming that % farmers who apply plant protection advices will not 
  #harm the fishes
  fish_benefiti4<-rep(0,n_years)
  fish_benefiti4[1:5]<-vv(area_pond_impacted_i1234,var_CV_high,5)*
    vv(lost_fish_i1234,var_CV_high,5)*
    rate_fer_pla_i4/exchange_rate
  #water benefit
  water_benefiti4<-rep(0,n_years)
  water_benefiti4[1:5]<-vv(reduced_water_expenditure_i1234,var_CV_high,5)*
    vv(total_households_i1234,var_CV,5)*
    vv(percent_pollution_reduction_i1234,var_CV_high,5)*
    rate_fer_pla_i4/exchange_rate
  # Total environmental benefit, discounting the inaccurate forecast 
  env_benefiti4<-(fish_benefiti4+water_benefiti4)*
    (1-inaccurate_forecast_i234*affected_coeff_inaccurate_forecast_i1234)
  
  # III. SOCIAL IMPACTS####
  # Benefit for social impacts intervention 1
  gender_benefiti1<-rep(0,n_years)
  gender_benefiti1[1]<-0
  gender_benefiti1[2:5]<-(vv(new_income_farm_peryear_i12,var_CV_high,4)*
  vv(rate_farm,var_CV_high,4)*total_farm_households_i1234+
  vv(new_income_nonfarm_peryear_i12, var_CV_high,4)*
  vv(rate_nonfarm,var_CV_high,4)*total_farm_households_i1234*gender_coverage)/exchange_rate
    
  # Benefit for social impacts intervention 2
  gender_benefiti2<-gender_benefiti1
  
  # Benefit for social impacts intervention 3
  gender_benefiti3<-rep(0,n_years)
  gender_benefiti3[1:5]<-0
  
  # Benefit for social impacts intervention 4
  gender_benefiti4<-rep(0,n_years)
  gender_benefiti3[1:5]<-0
  
  # IV.HUMAN IMPACTS (health and capacity to advise)####
  
  # Benefit for human intervention 1
  # Reduced expenditure on health
  health_impacti1<-rep(0,n_years)
  health_impacti1[1:5]<-(vv(reduced_expenditure_health_i1234,var_CV_high,5)*
    vv(total_farm_households_i1234,var_CV,5)*
    vv(percent_pollution_reduction_i1234,var_CV_high,5)*
    rate_fer_pla_i1/exchange_rate)*
    (1-inaccurate_forecast_i1*affected_coeff_inaccurate_forecast_i1234)
  
  # Impact on generating more income for staff and their relatives and neighbors
  staff_capa_benefiti1234<-number_staff_i1234*income_increased_staff_i1234*
    year_staff_i1234/exchange_rate
  # total human impact intervention 1
  human_impacti1<-health_impacti1+staff_capa_benefiti1234
  
  # Benefit for human intervention 2
  # Reduced expenditure on health
  health_impacti2<-rep(0,n_years)
  health_impacti2[1:5]<-(vv(reduced_expenditure_health_i1234,var_CV_high,5)*
                           vv(total_farm_households_i1234,var_CV,5)*
                           vv(percent_pollution_reduction_i1234,var_CV_high,5)*
                           rate_fer_pla_i2/exchange_rate)*
    (1-inaccurate_forecast_i234*affected_coeff_inaccurate_forecast_i1234)
  
  # Impact on generating more income for staff and their relatives and neighbors
  staff_capa_benefiti1234<-number_staff_i1234*income_increased_staff_i1234*
    year_staff_i1234/exchange_rate
  # total human impact intervention 2
  human_impacti2<-health_impacti2+staff_capa_benefiti1234
  
  
  # Benefit for human intervention 3
  # Reduced expenditure on health
  health_impacti3<-rep(0,n_years)
  health_impacti3[1:5]<-(vv(reduced_expenditure_health_i1234,var_CV_high,5)*
                           vv(total_farm_households_i1234,var_CV,5)*
                           vv(percent_pollution_reduction_i1234,var_CV_high,5)*
                           rate_fer_pla_i3/exchange_rate)*
    (1-inaccurate_forecast_i234*affected_coeff_inaccurate_forecast_i1234)
  
  # Impact on generating more income for staff and their relatives and neighbors
  staff_capa_benefiti1234<-number_staff_i1234*income_increased_staff_i1234*
    year_staff_i1234/exchange_rate
  # total human impact intervention 3
  human_impacti3<-health_impacti3+staff_capa_benefiti1234
  
  # Benefit for human intervention 4
  # Reduced expenditure on health
  health_impacti4<-rep(0,n_years)
  health_impacti4[1:5]<-(vv(reduced_expenditure_health_i1234,var_CV_high,5)*
                           vv(total_farm_households_i1234,var_CV,5)*
                           vv(percent_pollution_reduction_i1234,var_CV_high,5)*
                           rate_fer_pla_i4/exchange_rate)*
    (1-inaccurate_forecast_i234*affected_coeff_inaccurate_forecast_i1234)
  
  # Impact on generating more income for staff and their relatives and neighbors
  staff_capa_benefiti1234<-number_staff_i1234*income_increased_staff_i1234*
    year_staff_i1234/exchange_rate
  # total human impact intervention 4
  human_impacti4<-health_impacti4+staff_capa_benefiti1234
   
  
  # Total benefit intervention 1####
  total_benefiti1<-total_rice_i1+buffalo_benefiti1+cow_benefiti1+env_benefiti1+
    gender_benefiti1+human_impacti1
  
  # Annual bottomline benefit intervention 1
  bottomline_benefiti1=total_benefiti1-total_cost_i1
  
  # Annual cash flow intervention 1
  cash_flowi1<-discount(bottomline_benefiti1, discount_rate, calculate_NPV = FALSE)
  cum_cash_flowi1<-cumsum(cash_flowi1)
  
  # NPV Intervention 1####
  NPV1<-discount(bottomline_benefiti1, discount_rate, calculate_NPV = TRUE)
  
  # Benefit-cost ratio intervention 1####
  # Benefit-cost ratio is the discounted value of the project's benefits divided 
  # by the discounted value of the project's costs
  #discount total cost https://bizfluent.com/info-8352967-measurable-organizational-values.html
  discount_total_cost_i1<-discount(total_cost_i1, discount_rate, calculate_NPV = TRUE)
  bcri1<-NPV1/discount_total_cost_i1
  
  
  # Total benefit intervention 2####
  total_benefiti2<-total_rice_i2+buffalo_benefiti2+cow_benefiti2+env_benefiti2+
    gender_benefiti2+human_impacti2
  # Annual bottomline benefit intervention 2
  bottomline_benefiti2=total_benefiti2-total_cost_i2
  # Annual cash flow intervention 2
  cash_flowi2<-discount(bottomline_benefiti2, discount_rate, calculate_NPV = FALSE)
  cum_cash_flowi2<-cumsum(cash_flowi2)
  # NPV Intervention 2####
  NPV2<-discount(bottomline_benefiti2, discount_rate, calculate_NPV = TRUE)
  # Benefit-cost ratio intervention 2####
  discount_total_cost_i2<-discount(total_cost_i2, discount_rate, calculate_NPV = TRUE)
  bcri2<-NPV2/discount_total_cost_i2
  
  
  # Total benefits intervention 3
  total_benefiti3<-total_rice_i3+buffalo_benefiti3+cow_benefiti3+env_benefiti3+
    +gender_benefiti3+human_impacti3
  # Annual bottomline benefit intervention 3
  bottomline_benefiti3=total_benefiti3-total_cost_i3
  # Annual cash flow intervention 3
  cash_flowi3<-discount(bottomline_benefiti3, discount_rate, calculate_NPV = FALSE)
  cum_cash_flowi3<-cumsum(cash_flowi3)
  # NPV Intervention 3####
  NPV3<-discount(bottomline_benefiti3, discount_rate, calculate_NPV = TRUE)
  # Benefit-cost ratio intervention 3####
  discount_total_cost_i3<-discount(total_cost_i3, discount_rate, calculate_NPV = TRUE)
  bcri3<-NPV3/discount_total_cost_i3
  
  
  # Total benefits intervention 4####
  total_benefiti4<-total_rice_i4+buffalo_benefiti4+cow_benefiti4+env_benefiti4+
    gender_benefiti4+human_impacti4
  # Annual bottomline benefit intervention 4
  bottomline_benefiti4=total_benefiti4-total_cost_i4
  # Annual cash flow intervention 4
  cash_flowi4<-discount(bottomline_benefiti4, discount_rate, calculate_NPV = FALSE)
  cum_cash_flowi4<-cumsum(cash_flowi4)
  # NPV Intervention 4####
  NPV4<-discount(bottomline_benefiti4, discount_rate, calculate_NPV = TRUE)
  # Benefit-cost ratio intervention 4####
  discount_total_cost_i4<-discount(total_cost_i4, discount_rate, calculate_NPV = TRUE)
  bcri4<-NPV4/discount_total_cost_i4
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
        cum_cash_Intervention4<-cum_cash_flowi4))
}

# Running the model ####
decisionSupport::decisionSupport(
  "acis_inputs_EN_check.csv",
  outputPath = paste(filepath,"MCResults",sep=""),
  welfareFunction = acis_costbenefit,
  numberOfModelRuns = 1e4, #run 10,000 times
  functionSyntax = "plainNames")




