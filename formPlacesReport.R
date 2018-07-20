source("formReport.R")

#add sum statistic for each placement
total_placement_stat<-function(report){
  report.all<-
    group_by(report[,-c(2)], Placement)%>%
    dplyr::summarize_if(.predicate = function(x) is.numeric(x),
                        .funs = funs(sum="sum"))
  report.all<-as.data.frame(report.all)
  
  colnames(report.all)<-gsub("_sum$", "", colnames(report.all))
  report.all<-cbind(report.all, CampaignName = rep(" все", nrow(report.all)))
  
  report.all<-report.all[, colnames(report)]
  
  
  return(report.all)

}

make_report_ya_places<-function(date_start=Sys.Date()-7, date_end=Sys.Date()){
  
    ###yandex, in ga placement is keyword
    #direct
    report.direct<-yadirGetReport(ReportType = "CAMPAIGN_PERFORMANCE_REPORT", 
                                  DateRangeType="CUSTOM_DATE", DateFrom = date_start, DateTo = date_end, 
                                  FieldNames = c("Cost","Clicks","Impressions","CampaignName", "CampaignId","Placement"), 
                                  FilterList=NULL, IncludeVAT = "NO", IncludeDiscount = "NO", Login = yalogin, Token = my_token)
    if(nrow(report.direct)==0){

      return(data.frame(Placement = character(0), 
                        CampaignName = character(0),
                        Cost = numeric(0),
                        Clicks= integer(0),
                        Impressions = integer(0),  
                        sessions = integer(0), 
                        transactionRevenue  = numeric(0),
                        allGoalsCompletions = integer(0), 
                        CPC = numeric(0),
                        CTR = numeric(0), 
                        CPA  = numeric(0), 
                        ROI = numeric(0)))
    }
    
    report.direct$Cost<-as.numeric(as.vector(report.direct$Cost))
    report.direct$CampaignName<-as.character(report.direct$CampaignName)

    report.direct<-inner_join(report.direct, utm_camp_names)
    report.direct$CampaignName<-report.direct$UtmCampaignName
    report.direct$UtmCampaignName<-NULL
        
    #filter rsya
    report.direct.rsya<-dplyr::filter(report.direct, startsWith(CampaignName, "rsya"))
    
    
    report.direct.rsya$Placement<-as.vector(report.direct.rsya$Placement) 
    
    

    
    
    #ga
    source_medium_filter <- dim_filter(dimension="sourceMedium",operator="REGEXP",expressions="yandex / (cpc|cpm)")
    
    campaign_filter<-dim_filter(dimension="campaign",operator="REGEXP",expressions="^rsya")
    
    
    my_filter_clause <- filter_clause_ga4(list(source_medium_filter,campaign_filter), operator=c("AND"))
    
    if (length(goals_and_transactions) <= 8){
      report.ga.placement<-google_analytics_4(ga_view_id,
                                             date_range = c(date_start, date_end), 
                                             metrics = c("sessions", "transactionRevenue", goals_and_transactions), 
                                             dimensions = c("campaign","keyword"), 
                                             dim_filters = my_filter_clause, anti_sample = TRUE)  
      if (is.null(report.ga.placement)){
        report.ga.placement<- data.frame(campaign=character(0), 
                                         keyword=character(0),
                                         sessions=integer(0), 
                                         transactionRevenue=numeric(0))
        goals_and_transactions_df <- data.frame(matrix(0,0,length(goals_and_transactions)))
        colnames(goals_and_transactions_df) <- goals_and_transactions
        report.ga.placement <- cbind(report.ga.placement, goals_and_transactions_df)
        
      }
    }else if(length(goals_and_transactions) <= 16){
      report.ga.placement1<-google_analytics_4(ga_view_id,
                                              date_range = c(date_start, date_end), 
                                              metrics = c("sessions", "transactionRevenue", goals_and_transactions[1:8]), 
                                              dimensions = c("campaign","keyword"), 
                                              dim_filters = my_filter_clause, anti_sample = TRUE)  
      if (is.null(report.ga.placement1)){
        report.ga.placement<- data.frame(campaign=character(0), 
                                         keyword=character(0),
                                         sessions=integer(0), 
                                         transactionRevenue=numeric(0))
        goals_and_transactions_df <- data.frame(matrix(0,0,length(goals_and_transactions)))
        colnames(goals_and_transactions_df) <- goals_and_transactions
        report.ga.placement <- cbind(report.ga.placement, goals_and_transactions_df)
        
      }else{
        report.ga.placement2<-google_analytics_4(ga_view_id,
                                                 date_range = c(date_start, date_end), 
                                                 metrics = c("sessions", "transactionRevenue", goals_and_transactions[9:length(goals_and_transactions)]), 
                                                 dimensions = c("campaign","keyword"), 
                                                 dim_filters = my_filter_clause, anti_sample = TRUE) 
        report.ga.placement<-inner_join(report.ga.placement1, report.ga.placement2)
        report.ga.placement<-report.ga.placement[,c( "campaign", "keyword", "sessions", "transactionRevenue", goals_and_transactions)]
      }
    }else{
      
      report.ga.placement1<-google_analytics_4(ga_view_id,
                                               date_range = c(date_start, date_end), 
                                               metrics = c("sessions", "transactionRevenue", goals_and_transactions[1:8]), 
                                               dimensions = c("campaign","keyword"), 
                                               dim_filters = my_filter_clause, anti_sample = TRUE)  
      if (is.null(report.ga.placement1)){
        report.ga.placement<- data.frame(campaign=character(0), 
                                         keyword=character(0),
                                         sessions=integer(0), 
                                         transactionRevenue=numeric(0))
        goals_and_transactions_df <- data.frame(matrix(0,0,length(goals_and_transactions)))
        colnames(goals_and_transactions_df) <- goals_and_transactions
        report.ga.placement <- cbind(report.ga.placement, goals_and_transactions_df)
        
      }else{
        report.ga.placement2<-google_analytics_4(ga_view_id,
                                                 date_range = c(date_start, date_end), 
                                                 metrics = c("sessions", "transactionRevenue", goals_and_transactions[9:16]), 
                                                 dimensions = c("campaign","keyword"), 
                                                 dim_filters = my_filter_clause, anti_sample = TRUE) 
        report.ga.placement3<-google_analytics_4(ga_view_id,
                                                 date_range = c(date_start, date_end), 
                                                 metrics = c("sessions", "transactionRevenue", goals_and_transactions[17:length(goals_and_transactions)]), 
                                                 dimensions = c("campaign","keyword"), 
                                                 dim_filters = my_filter_clause, anti_sample = TRUE) 
        
        report.ga.placement<-inner_join(report.ga.placement1, report.ga.placement2)
        report.ga.placement<-inner_join(report.ga.placement, report.ga.placement3)
        report.ga.placement<-report.ga.placement[,c( "campaign", "keyword", "sessions", "transactionRevenue", goals_and_transactions)]
      }
    }
    
    report.placement.rsya<-filter(report.ga.placement, startsWith(campaign, "rsya") & grepl(".*\\|.+", keyword, perl=TRUE))
    
    report.placement.rsya<-extract(report.placement.rsya, keyword, into = c('keyword', 'placement'), '(.+)\\|([^|]+)\\|?$')
    
    
    
 
    report.rsya<-dplyr::left_join(report.direct.rsya, 
                                   report.placement.rsya, by=c("CampaignName"="campaign", "Placement"="placement"))
    
    report.rsya<-report.rsya[,c("Placement", "CampaignName", "Cost", 
                               "Clicks", "Impressions", "sessions", "transactionRevenue", goals_and_transactions)]
    
    # NA to 0
    report.rsya[is.na(report.rsya)] <- 0
    
    report.rsya.all<-total_placement_stat(report.rsya)
    
    report.rsya.total<-rbind(report.rsya, report.rsya.all)
    #sum up goal completion
    if (length(goals_and_transactions)==1){
      report.rsya.total$allGoalsCompletions<-report.rsya.total[,goals_and_transactions]
    }else{
      report.rsya.total$allGoalsCompletions<-apply(report.rsya.total[,goals_and_transactions], 1, sum)
    }
    
    #get columns
    cols<-setdiff(colnames(report.rsya.total), goals_and_transactions)
    report.rsya.total<-report.rsya.total[cols]
    report.rsya.total$CPC<-report.rsya.total$Cost/report.rsya.total$Clicks
    report.rsya.total$CTR<-report.rsya.total$Clicks/report.rsya.total$Impressions
    report.rsya.total$CPA<-report.rsya.total$Cost/report.rsya.total$allGoalsCompletions
    report.rsya.total$ROI<-
      round(100*(report.rsya.total$transactionRevenue - report.rsya.total$Cost)/report.rsya.total$Cost)
    return(report.rsya.total)
}

#adwords

make_report_google_places<-function(date_start=Sys.Date()-7, date_end=Sys.Date()){
    source_medium_filter <- dim_filter(dimension="sourceMedium",operator="REGEXP",expressions="google / (cpc|cpm)")
    
    
    
    my_filter_clause <- filter_clause_ga4(list(source_medium_filter))
    
    
    if (length(goals_and_transactions) <= 5){
      report.google.ga.placement<-google_analytics_4(ga_view_id,
                                                date_range = c(date_start, date_end), 
                                                metrics = c("adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions), 
                                                dimensions = c("adPlacementDomain","campaign"), 
                                                dim_filters = my_filter_clause, anti_sample = TRUE)  
      if(is.null(report.google.ga.placement)){
        
        return(data.frame(Placement = character(0), 
                          CampaignName = character(0),
                          Cost = numeric(0),
                          Clicks= integer(0),
                          Impressions = integer(0),  
                          sessions = integer(0), 
                          transactionRevenue  = numeric(0),
                          allGoalsCompletions = integer(0), 
                          CPC = numeric(0),
                          CTR = numeric(0), 
                          CPA  = numeric(0), 
                          ROI = numeric(0)))
      }
    }else if(length(goals_and_transactions) <= 10){
      report.google.ga.placement1<-google_analytics_4(ga_view_id,
                                                     date_range = c(date_start, date_end), 
                                                     metrics = c("adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions[1:5]), 
                                                     dimensions = c("adPlacementDomain","campaign"), 
                                                     dim_filters = my_filter_clause, anti_sample = TRUE)  
      if(is.null(report.google.ga.placement1)){
        
        return(data.frame(Placement = character(0), 
                          CampaignName = character(0),
                          Cost = numeric(0),
                          Clicks= integer(0),
                          Impressions = integer(0),  
                          sessions = integer(0), 
                          transactionRevenue  = numeric(0),
                          allGoalsCompletions = integer(0), 
                          CPC = numeric(0),
                          CTR = numeric(0), 
                          CPA  = numeric(0), 
                          ROI = numeric(0)))
      }else{
        report.google.ga.placement2<-google_analytics_4(ga_view_id,
                                                        date_range = c(date_start, date_end), 
                                                        metrics = c("adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions[6:length(goals_and_transactions)]), 
                                                        dimensions = c("adPlacementDomain","campaign"), 
                                                        dim_filters = my_filter_clause, anti_sample = TRUE)  
        report.google.ga.placement<-inner_join(report.google.ga.placement1, report.google.ga.placement2)
        
        report.google.ga.placement<-report.google.ga.placement[, c("adPlacementDomain","campaign", "adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions)]
      }
    }else if(length(goals_and_transactions) <= 15){
      report.google.ga.placement1<-google_analytics_4(ga_view_id,
                                                      date_range = c(date_start, date_end), 
                                                      metrics = c("adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions[1:5]), 
                                                      dimensions = c("adPlacementDomain","campaign"), 
                                                      dim_filters = my_filter_clause, anti_sample = TRUE)  
      if(is.null(report.google.ga.placement1)){
        
        return(data.frame(Placement = character(0), 
                          CampaignName = character(0),
                          Cost = numeric(0),
                          Clicks= integer(0),
                          Impressions = integer(0),  
                          sessions = integer(0), 
                          transactionRevenue  = numeric(0),
                          allGoalsCompletions = integer(0), 
                          CPC = numeric(0),
                          CTR = numeric(0), 
                          CPA  = numeric(0), 
                          ROI = numeric(0)))
      }else{
        report.google.ga.placement2<-google_analytics_4(ga_view_id,
                                                        date_range = c(date_start, date_end), 
                                                        metrics = c("adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions[6:10]), 
                                                        dimensions = c("adPlacementDomain","campaign"), 
                                                        dim_filters = my_filter_clause, anti_sample = TRUE)  
        report.google.ga.placement3<-google_analytics_4(ga_view_id,
                                                        date_range = c(date_start, date_end), 
                                                        metrics = c("adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions[11:length(goals_and_transactions)]), 
                                                        dimensions = c("adPlacementDomain","campaign"), 
                                                        dim_filters = my_filter_clause, anti_sample = TRUE)  
        
        report.google.ga.placement<-inner_join(report.google.ga.placement1, report.google.ga.placement2)
        report.google.ga.placement<-inner_join(report.google.ga.placement, report.google.ga.placement3)
        
        report.google.ga.placement<-report.google.ga.placement[, c("adPlacementDomain","campaign", "adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions)]
      }
    }else{
      {
        report.google.ga.placement1<-google_analytics_4(ga_view_id,
                                                        date_range = c(date_start, date_end), 
                                                        metrics = c("adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions[1:5]), 
                                                        dimensions = c("adPlacementDomain","campaign"), 
                                                        dim_filters = my_filter_clause, anti_sample = TRUE)  
        if(is.null(report.google.ga.placement1)){
          
          return(data.frame(Placement = character(0), 
                            CampaignName = character(0),
                            Cost = numeric(0),
                            Clicks= integer(0),
                            Impressions = integer(0),  
                            sessions = integer(0), 
                            transactionRevenue  = numeric(0),
                            allGoalsCompletions = integer(0), 
                            CPC = numeric(0),
                            CTR = numeric(0), 
                            CPA  = numeric(0), 
                            ROI = numeric(0)))
        }else{
          report.google.ga.placement2<-google_analytics_4(ga_view_id,
                                                          date_range = c(date_start, date_end), 
                                                          metrics = c("adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions[6:10]), 
                                                          dimensions = c("adPlacementDomain","campaign"), 
                                                          dim_filters = my_filter_clause, anti_sample = TRUE)  
          report.google.ga.placement3<-google_analytics_4(ga_view_id,
                                                          date_range = c(date_start, date_end), 
                                                          metrics = c("adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions[11:15]), 
                                                          dimensions = c("adPlacementDomain","campaign"), 
                                                          dim_filters = my_filter_clause, anti_sample = TRUE)  
          report.google.ga.placement4<-google_analytics_4(ga_view_id,
                                                          date_range = c(date_start, date_end), 
                                                          metrics = c("adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions[15:length(goals_and_transactions)]), 
                                                          dimensions = c("adPlacementDomain","campaign"), 
                                                          dim_filters = my_filter_clause, anti_sample = TRUE)  
          
          report.google.ga.placement<-inner_join(report.google.ga.placement1, report.google.ga.placement2)
          report.google.ga.placement<-inner_join(report.google.ga.placement, report.google.ga.placement3)
          report.google.ga.placement<-inner_join(report.google.ga.placement, report.google.ga.placement4)
          
          report.google.ga.placement<-report.google.ga.placement[, c("adPlacementDomain","campaign", "adCost", "adClicks", "impressions", "sessions", "transactionRevenue", goals_and_transactions)]
        }
      }
    }
    
    colnames(report.google.ga.placement)<-c("Placement", "CampaignName",  "Cost", "Clicks", "Impressions", 
                  "sessions",  "transactionRevenue", goals_and_transactions)
    
    #add sum statistic for each placement
    report.google.all<-total_placement_stat(report.google.ga.placement)
    
    
    report.google.total<-rbind(report.google.ga.placement, report.google.all)
    
    #sum up goal completion
    if (length(goals_and_transactions)==1){
      report.google.total$allGoalsCompletions<-report.google.total[,goals_and_transactions]
    }else{
      report.google.total$allGoalsCompletions<-apply(report.google.total[,goals_and_transactions], 1, sum)
    }
    
    cols<-setdiff(colnames(report.google.total), goals_and_transactions)
    report.google.total<-report.google.total[cols]
    report.google.total$CPC<-report.google.total$Cost/report.google.total$Clicks
    report.google.total$CTR<-report.google.total$Clicks/report.google.total$Impressions
    report.google.total$CPA<-report.google.total$Cost/report.google.total$allGoalsCompletions
    report.google.total$ROI<-
      round(100*(report.google.total$transactionRevenue - report.google.total$Cost)/report.google.total$Cost)
    return(report.google.total)
    
}
dif_report_places<-function(report1, report2){
  tmp<-left_join(report2, report1, by<-c("Placement", "CampaignName"))
  result<-tmp[,c("Placement", "CampaignName")]
  
  
  result$Cost <- count_dif(tmp$Cost.y, tmp$Cost.x)
  
  result$Clicks <- count_dif(tmp$Clicks.y, tmp$Clicks.x)
  
  result$Impressions <- count_dif(tmp$Impressions.y, tmp$Impressions.x)
  
  result$sessions <- count_dif(tmp$sessions.y, tmp$sessions.x)
  
  result$transactionRevenue <- count_dif(tmp$transactionRevenue.y, tmp$transactionRevenue.x)
  
  result$allGoalsCompletions <- count_dif(tmp$allGoalsCompletions.y, tmp$allGoalsCompletions.x)
  
  result$CPC <- count_dif(tmp$CPC.y, tmp$CPC.x)

  result$CTR <- count_dif(tmp$CTR.y, tmp$CTR.x)
  
  result$CPA <- count_dif(tmp$CPA.y, tmp$CPA.x)
  
  result$ROI <- count_dif(tmp$ROI.y, tmp$ROI.x)
  
  return(result)
}

form_reports_places<-function(date_start1=Sys.Date()-16, date_end1=Sys.Date()-9, date_start2=Sys.Date()-8, date_end2=Sys.Date()-1, updateProgress = NULL){
  #for progress
  n = 11
  
  if (is.function(updateProgress)) {
    updateProgress(2/n, "authentification")
  }
  auth(google.account, yandex.account)
  
  utm_camp_names<<-get_utm_campaign_names()
  
  
  #report yandex
  if (is.function(updateProgress)) {
    updateProgress(3/n, "load yandex data for period 1")
  }  
  report.ya.1<-make_report_ya_places(date_start1, date_end1)
  
  
  if (is.function(updateProgress)) {
    updateProgress(4/n, "load yandex data for period 2")
  }
  report.ya.2<-make_report_ya_places(date_start2, date_end2)
  
  ##remove_nan_inf
  if(nrow(report.ya.1) != 0){
    is.na(report.ya.1)<-sapply(report.ya.1, is.infinite)
    is.na(report.ya.1)<-sapply(report.ya.1, is.nan)
  }
  if(nrow(report.ya.2) != 0){
    is.na(report.ya.2)<-sapply(report.ya.2, is.infinite)
    is.na(report.ya.2)<-sapply(report.ya.2, is.nan)
  }
  
  #report google
  if (is.function(updateProgress)) {
    updateProgress(5/n, "load google data for period 1")
  }    
  
  report.google.1<-make_report_google_places(date_start1, date_end1)
  
  if (is.function(updateProgress)) {
    updateProgress(6/n, "load google data for period 2")
  }    
  
  report.google.2<-make_report_google_places(date_start2, date_end2)
  
  if(nrow(report.google.1) != 0){
    is.na(report.google.1)<-sapply(report.google.1, is.infinite)
    is.na(report.google.1)<-sapply(report.google.1, is.nan)
  }
  if(nrow(report.google.2) != 0){
    is.na(report.google.2)<-sapply(report.google.2, is.infinite)
    is.na(report.google.2)<-sapply(report.google.2, is.nan)  
  }
  
 
  
  if (is.function(updateProgress)) {
    updateProgress(8/n, "create diff report")
  }  
  report.ya.dif<-dif_report_places(report.ya.1, report.ya.2)
  report.google.dif<-dif_report_places(report.google.1, report.google.2)  
  
  if(nrow(report.ya.dif) != 0){ 
    is.na(report.ya.dif)<-sapply(report.ya.dif, is.infinite)
    is.na(report.ya.dif)<-sapply(report.ya.dif, is.nan)  
  }
  if(nrow(report.google.dif) != 0){   
    is.na(report.google.dif)<-sapply(report.google.dif, is.infinite)
    is.na(report.google.dif)<-sapply(report.google.dif, is.nan)  
  }
  
  
  
  
  ##remove na rows from report tables
  if (is.function(updateProgress)) {
    updateProgress(9/n, "remove na rows from report tables")
  }  
  if (remove_na){
    report.ya.1 <- remove_na_rows(report.ya.1)
    report.ya.2 <- remove_na_rows(report.ya.2)
    report.ya.dif <- remove_na_rows(report.ya.dif)
    
    report.google.1 <- remove_na_rows(report.google.1)
    report.google.2 <- remove_na_rows(report.google.2)
    report.google.dif <- remove_na_rows(report.google.dif)
    
  }
  #conversion
  report.ya.1$allGoalsCompletions <- report.ya.1$allGoalsCompletions/report.ya.1$sessions
  report.ya.2$allGoalsCompletions <- report.ya.2$allGoalsCompletions/report.ya.2$sessions
  
  report.google.1$allGoalsCompletions <- report.google.1$allGoalsCompletions/report.google.1$sessions
  report.google.2$allGoalsCompletions <- report.google.2$allGoalsCompletions/report.google.2$sessions
  
  
  
  
  if (is.function(updateProgress)) {
    updateProgress(10/n, "round values")
  }  
  report.ya.1 <- round_values(report.ya.1)
  report.ya.2 <- round_values(report.ya.2)
  report.google.1 <- round_values(report.google.1)
  report.google.2 <- round_values(report.google.2)
  
  
  if (is.function(updateProgress)) {
    updateProgress(11/n, "rename columns")
  }  
  
  cnames<-c("Площадка",	"Название кампании", "Расход", "Клики", "Показы", "Сессии","Доход", "Конверсия","CPC", "CTR", 
            "CPA", "ROI")
  colnames(report.ya.1)<-cnames
  colnames(report.ya.2)<-cnames
  colnames(report.ya.dif)<-cnames
  colnames(report.google.1)<-cnames
  colnames(report.google.2)<-cnames
  colnames(report.google.dif)<-cnames
  
  
  return(list(report.ya.1, report.ya.2, report.ya.dif, report.google.1, report.google.2, report.google.dif))
  
}