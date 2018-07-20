library(googleAnalyticsR)
library(googleAuthR)
library(devtools)
library(dplyr)
library(ryandexdirect)
library(RAdwords)
library(tidyverse)
library(plyr)
library(rmarkdown)
library(tidyr)


#dates are comming from shiny
remove_na = T

google.account=NA
yandex.account=NA
ga_view_id=172465481#
yalogin<-"obruchalki-direct"#"montegrappa-direct"#
goals_ga_numbers<-c(1, 2, 3, 4, 5, 6, 7, 8,9,10)#c(4, 2, 11, 9)
goals_and_transactions<-c("transactions")

out_dir<-gsub("/[^/]+$", "/Results", getwd())
utm_camp_names<-NA

get_utm_campaign_names<-function(){
  print(yalogin)
  print(my_token)
  my_campaign <- yadirGetCampaignList(Login = yalogin, Token = my_token)
  my_ads <- yadirGetAds(CampaignIds = my_campaign$Id, 
                        Login = yalogin, 
                        Token = my_token)
  my_ads$UtmCampaignName<-sub(".*utm_campaign=([a-z_-]+)(\\|{campaign_id})?&.*", "\\1", my_ads$TextAdHref,perl=TRUE)
  return(unique(my_ads[,c("CampaignId", "UtmCampaignName")]))
}
make_report_ya<-function(date_start=Sys.Date()-8, date_end=Sys.Date()-1){
  #get data from direct
  report.direct<-yadirGetReport(ReportType = "CRITERIA_PERFORMANCE_REPORT", 
                                DateRangeType="CUSTOM_DATE", DateFrom = date_start, DateTo = date_end, 
                                FieldNames = c("CampaignName","CampaignId","Impressions","Criteria", "CriteriaType", "Clicks","Cost", "AvgClickPosition", "AvgImpressionPosition"), 
                                FilterList=NULL, IncludeVAT = "NO", IncludeDiscount = "NO", Login = yalogin, Token = my_token)
  
  if(nrow(report.direct)==0){
    return(data.frame(campaign_id = integer(0), 
                      campaign = character(0), 
                      keyword = character(0), 
                      Cost = numeric(0),
                      sessions = integer(0), 
                      Impressions = integer(0), 
                      Clicks= integer(0), 
                      CPC = numeric(0), 
                      AvgClickPosition = numeric(0), 
                      AvgImpressionPosition = numeric(0), 
                      CTR = numeric(0), 
                      allGoalsCompletions = integer(0), 
                      transactionRevenue  = numeric(0), 
                      CPA  = numeric(0), 
                      ROI = numeric(0)))
  }
  
  report.direct$AvgClickPosition<-as.numeric(as.vector(report.direct$AvgClickPosition))
  report.direct$AvgImpressionPosition<-as.numeric(as.vector(report.direct$AvgImpressionPosition))
  
  #work with non keyword campaigns
  report.direct.keyword<-dplyr::filter(report.direct, CriteriaType=="KEYWORD")
  regexp1<-"\\s?-.*"
  regexp2<-"[+!\"]"
  report.direct.keyword<-mutate(report.direct.keyword, Criteria_edt_1=gsub(regexp1, "", Criteria, perl=T))
  report.direct.keyword<-mutate(report.direct.keyword, Criteria_edt_2=gsub(regexp2, "", Criteria_edt_1, perl=T))
  report.direct.keyword<-mutate(report.direct.keyword, Criteria_edt=tolower(Criteria_edt_2))
  
  
  report.direct.keyword<-report.direct.keyword[, c(-(ncol(report.direct.keyword)-2), -(ncol(report.direct.keyword)-1))]
  
  report.direct.non_keyword<-dplyr::filter(report.direct, CriteriaType!="KEYWORD")
  
  report.direct.non_keyword<-
    ddply(report.direct.non_keyword,.(CampaignId),
          function(x){
            y<-apply(x[,c("Impressions", "Clicks", "Cost")],2,sum)
            y<-c(CampaignName=max(as.character(x[,c("CampaignName")])), Criteria=NA, CriteriaType = max(as.vector(x[,c("CriteriaType")])),
                 y, AvgClickPosition=max(x[,c("AvgClickPosition")]), AvgImpressionPosition=max(x[,"AvgImpressionPosition"]))
            
          })
  report.direct.non_keyword$Impressions<-as.integer(report.direct.non_keyword$Impressions)
  report.direct.non_keyword$Clicks<-as.integer(report.direct.non_keyword$Clicks)
  report.direct.non_keyword$AvgClickPosition<-as.numeric(report.direct.non_keyword$AvgClickPosition)
  report.direct.non_keyword$AvgImpressionPosition<-as.numeric(report.direct.non_keyword$AvgImpressionPosition)
  report.direct.non_keyword$Cost<-as.numeric(report.direct.non_keyword$Cost)
  
  #change campain name according to utm
  report.direct.keyword<-inner_join(report.direct.keyword, utm_camp_names)
  report.direct.keyword$CampaignName<-report.direct.keyword$UtmCampaignName
  report.direct.keyword$UtmCampaignName<-NULL
  
  report.direct.non_keyword<-inner_join(report.direct.non_keyword, utm_camp_names)
  report.direct.non_keyword$CampaignName<-report.direct.non_keyword$UtmCampaignName
  report.direct.non_keyword$UtmCampaignName<-NULL
  
  
  #get data from ga
  
  #"yandex / (cpc|cpm)"
  source_medium_filter <- dim_filter(dimension="sourceMedium",operator="REGEXP",expressions="yandex / (cpc)")
  
#  campaign_filter <- dim_filter(dimension="campaign",operator="REGEXP",expressions="[A-Za-z_-]+\\|[0-9]+")
  
  
#  my_filter_clause <- filter_clause_ga4(list(source_medium_filter, campaign_filter), operator = "AND")
  
  my_filter_clause <- filter_clause_ga4(list(source_medium_filter))
  
  
  report.ga.ya.cpc<-NA
  if (length(goals_and_transactions) <= 8){
    report.ga.ya.cpc<-google_analytics_4(ga_view_id,
                                         date_range = c(date_start, date_end), 
                                         metrics = c("sessions", "transactionRevenue", goals_and_transactions), 
                                         dimensions = c("campaign", "keyword", "sourceMedium"), 
                                         dim_filters = my_filter_clause, anti_sample = TRUE)
    #if empty
    if (is.null(report.ga.ya.cpc)){
      
      report.ga.ya.cpc <- data.frame(campaign=character(0), 
                                  keyword=character(0), 
                                  sourceMedium=character(0), 
                                  sessions=integer(0), 
                                  transactionRevenue=numeric(0))
      goals_and_transactions_df <- data.frame(matrix(0,0,length(goals_and_transactions)))
      colnames(goals_and_transactions_df) <- goals_and_transactions
      report.ga.ya.cpc <- cbind(report.ga.ya.cpc, goals_and_transactions_df)
      
    }
  }else if(length(goals_and_transactions) <= 16){
    report.ga.ya.cpc1<-google_analytics_4(ga_view_id,
                                         date_range = c(date_start, date_end), 
                                         metrics = c("sessions", "transactionRevenue", goals_and_transactions[1:8]), 
                                         dimensions = c("campaign", "keyword", "sourceMedium"), 
                                         dim_filters = my_filter_clause, anti_sample = TRUE)   
    #if empty
    if (is.null(report.ga.ya.cpc1)){
      
      report.ga.ya.cpc <- data.frame(campaign=character(0), 
                                     keyword=character(0), 
                                     sourceMedium=character(0), 
                                     sessions=integer(0), 
                                     transactionRevenue=numeric(0))
      goals_and_transactions_df <- data.frame(matrix(0,0,length(goals_and_transactions)))
      colnames(goals_and_transactions_df) <- goals_and_transactions
      report.ga.ya.cpc <- cbind(report.ga.ya.cpc, goals_and_transactions_df)
      
    }else{
      report.ga.ya.cpc2<-google_analytics_4(ga_view_id,
                                            date_range = c(date_start, date_end), 
                                            metrics = c("sessions", goals_and_transactions[9:length(goals_and_transactions)]), 
                                            dimensions = c("campaign", "keyword", "sourceMedium"), 
                                            dim_filters = my_filter_clause, anti_sample = TRUE)  
      report.ga.ya.cpc<-inner_join(report.ga.ya.cpc1, report.ga.ya.cpc2)
      report.ga.ya.cpc<-report.ga.ya.cpc[,c( "campaign", "keyword", "sourceMedium", "sessions", "transactionRevenue", goals_and_transactions)]
    }
  }else{
    report.ga.ya.cpc1<-google_analytics_4(ga_view_id,
                                          date_range = c(date_start, date_end), 
                                          metrics = c("sessions", "transactionRevenue", goals_and_transactions[1:8]), 
                                          dimensions = c("campaign", "keyword", "sourceMedium"), 
                                          dim_filters = my_filter_clause, anti_sample = TRUE) 
    #if empty
    if (is.null(report.ga.ya.cpc1)){
      
      report.ga.ya.cpc <- data.frame(campaign=character(0), 
                                     keyword=character(0), 
                                     sourceMedium=character(0), 
                                     sessions=integer(0), 
                                     transactionRevenue=numeric(0))
      goals_and_transactions_df <- data.frame(matrix(0,0,length(goals_and_transactions)))
      colnames(goals_and_transactions_df) <- goals_and_transactions
      report.ga.ya.cpc <- cbind(report.ga.ya.cpc, goals_and_transactions_df)
      
    }else{
      report.ga.ya.cpc2<-google_analytics_4(ga_view_id,
                                            date_range = c(date_start, date_end), 
                                            metrics = c("sessions", goals_and_transactions[9:16]), 
                                            dimensions = c("campaign", "keyword", "sourceMedium"), 
                                            dim_filters = my_filter_clause, anti_sample = TRUE) 
      report.ga.ya.cpc3<-google_analytics_4(ga_view_id,
                                            date_range = c(date_start, date_end), 
                                            metrics = c("sessions", goals_and_transactions[17:length(goals_and_transactions)]), 
                                            dimensions = c("campaign", "keyword", "sourceMedium"), 
                                            dim_filters = my_filter_clause, anti_sample = TRUE)  
      
      report.ga.ya.cpc<-inner_join(report.ga.ya.cpc1, report.ga.ya.cpc2)
      report.ga.ya.cpc<-inner_join(report.ga.ya.cpc, report.ga.ya.cpc3)
      report.ga.ya.cpc<-report.ga.ya.cpc[,c( "campaign", "keyword", "sourceMedium", "sessions", "transactionRevenue", goals_and_transactions)]
    }
    
  }
  
  report.ga.ya.cpc<-mutate(report.ga.ya.cpc, campaign_id=sub("[A-Za-z_-]+\\|([0-9]+)", "\\1", campaign))
  report.ga.ya.cpc$campaign_id<-as.integer(report.ga.ya.cpc$campaign_id)
  
  #get keywords
  
  report.ga.ya.cpc.keyword<-dplyr::filter(report.ga.ya.cpc, keyword != "(not set)")
  
#  report.ga.ya.cpc.keyword<-ddply(report.ga.ya.cpc.keyword,
#                                  .(campaign_id, keyword),
#                                  function(x){
#                                    y<-apply(x[,c(-1,-2,-3, -ncol(report.ga.ya.cpc.keyword))],2,sum); 
#                                    y<-c(campaign=max(x[,1]), sourceMedium=max(x[,3]), y)
#                                  })
  #make numeric from character columns
  report.ga.ya.cpc.keyword[, c(-1,-2,-3)]<-apply(report.ga.ya.cpc.keyword[, c(-1,-2,-3)], 2, as.numeric)
  
  
  #get rsya and retargeting with placements
  report.ga.ya.cpc.keyword.no_rsya_site<-filter(report.ga.ya.cpc.keyword,  !grepl('^.*\\|.*\\|?', keyword))
  
  report.ga.ya.cpc.keyword.rsya_site<-filter(report.ga.ya.cpc.keyword, grepl('^.*\\|.*\\|?', keyword))
  
  df<-extract(report.ga.ya.cpc.keyword.rsya_site, keyword, into = c('keyword', 'placement'), '(.+)\\|([^|]+)\\|?$')
  report.ga.ya.cpc.keyword.rsya_site$keyword[!is.na(df$keyword)]<-df$keyword[!is.na(df$keyword)]
  
  
 
  report.ga.ya.cpc.keyword.rsya_site<- ddply(report.ga.ya.cpc.keyword.rsya_site,
                                                                            .(campaign, keyword),
                                                                            function(x){
                                                                              y<-apply(x[,c(-1,-2,-3, -ncol(report.ga.ya.cpc.keyword.rsya_site))],2,sum);
                                                                              y<-c(campaign_id=max(x[,ncol(report.ga.ya.cpc.keyword.rsya_site)]), sourceMedium=max(x[,3]), y)
                                                                            })
  #make numeric from character columns
  if (nrow(report.ga.ya.cpc.keyword.rsya_site) != 0){
    report.ga.ya.cpc.keyword.rsya_site[, c(-1,-2,-3)]<-apply( report.ga.ya.cpc.keyword.rsya_site[, c(-1,-2,-3)], 2, as.numeric)
  }
    
  #ascribe campain_id
  camp_id<-unique(filter(report.direct.keyword[,c("CampaignName", "CampaignId")], CampaignName %in% report.ga.ya.cpc.keyword.rsya_site$campaign))
  report.ga.ya.cpc.keyword.rsya_site<-inner_join(report.ga.ya.cpc.keyword.rsya_site, camp_id, by = c("campaign" = "CampaignName"))
  report.ga.ya.cpc.keyword.rsya_site$campaign_id<-report.ga.ya.cpc.keyword.rsya_site$CampaignId
  
  
  
  report.ga.ya.cpc.keyword<-rbind(report.ga.ya.cpc.keyword.no_rsya_site, report.ga.ya.cpc.keyword.rsya_site[, names(report.ga.ya.cpc.keyword.no_rsya_site)])
  
  report.ga.ya.cpc.keyword[, c(-1,-2,-3)]<-apply(report.ga.ya.cpc.keyword[, c(-1,-2,-3)], 2, as.numeric)
  
  #get non-keywords
  
  report.ga.ya.cpc.non_keyword<-dplyr::filter(report.ga.ya.cpc, keyword == "(not set)")
  
  report.ga.ya.cpc.non_keyword<-ddply(report.ga.ya.cpc.non_keyword,
                                      .(campaign),
                                      function(x){
                                        y<-apply(x[,c(-1,-2,-3, -ncol(report.ga.ya.cpc.non_keyword))],2,sum);
                                        y<-c(campaign_id=max(x[,ncol(report.ga.ya.cpc.non_keyword)]), keyword=max(x[,2]), sourceMedium=max(x[,3]), y)
                                      })
  #make numeric from character columns
  if (nrow(report.ga.ya.cpc.non_keyword) != 0){
    report.ga.ya.cpc.non_keyword[, c(-1,-2,-3)]<-apply(report.ga.ya.cpc.non_keyword[, c(-1,-2,-3)], 2, as.numeric)
  }
  
  
  #join
  #keyword
  report.ya.keyword<-full_join(report.ga.ya.cpc.keyword, report.direct.keyword, by = c("campaign" = "CampaignName", "keyword" = "Criteria_edt"))
  #correct placement campaign_id
  report.ya.keyword$CampaignId <- apply(report.ya.keyword[, c("campaign_id", "CampaignId")],1,function(x){ if (!is.na(x[1]) & is.na(x[2])) {return(x[1])} else{ return(x[2])}})
  
  #non_keyword
  report.ya.non_keyword<-full_join(report.ga.ya.cpc.non_keyword, report.direct.non_keyword, by = c("campaign" = "CampaignName"))
  # bind into one result table
  report.ya<-rbind(report.ya.keyword, report.ya.non_keyword[,colnames(report.ya.keyword)])
  if (length(goals_and_transactions)==1){
    report.ya$allGoalsCompletions<-report.ya[,goals_and_transactions]
  }else{
    report.ya$allGoalsCompletions<-apply(report.ya[,goals_and_transactions], 1, sum)
  }
  report.ya$CPC<-round(report.ya$Cost/report.ya$Clicks, 2)
  report.ya$CTR<-round(report.ya$Clicks/report.ya$Impressions, 2)
  report.ya$CPA<-round(report.ya$Cost/report.ya$allGoalsCompletions, 2)
  report.ya$ROI<-round(100*(report.ya$transactionRevenue - report.ya$Cost)/report.ya$Cost)
  
  result<-report.ya[, c("CampaignId", "campaign", "keyword", "Cost","sessions", "Impressions", "Clicks", "CPC", "AvgClickPosition", "AvgImpressionPosition", "CTR", "allGoalsCompletions", "transactionRevenue", "CPA", "ROI")]
  names(result)[1]<-"campaign_id"
  return(result)
}
#report google adwords 
make_report_google<-function(date_start, date_end){
  source_medium_filter <- dim_filter(dimension="sourceMedium",operator="REGEXP",expressions="google / (cpc)")
  
  
  
  my_filter_clause <- filter_clause_ga4(list(source_medium_filter))
  
  
  
  report.ga.google.cpc1<-google_analytics_4(ga_view_id,
                                            date_range = c(date_start, date_end), 
                                            metrics = c("impressions", "adClicks", "adCost", "transactionRevenue"), 
                                            dimensions = c("campaign", "keyword","adwordsCampaignID"), 
                                            dim_filters = my_filter_clause, anti_sample = TRUE)  
  #if empty
  if (is.null(report.ga.google.cpc1)){
    return(data.frame(campaign_id = integer(0), 
                      campaign = character(0), 
                      keyword = character(0), 
                      Cost = numeric(0),
                      sessions = integer(0), 
                      Impressions = integer(0), 
                      Clicks= integer(0), 
                      CPC = numeric(0), 
                      AvgClickPosition = numeric(0), 
                      AvgImpressionPosition = numeric(0), 
                      CTR = numeric(0), 
                      allGoalsCompletions = integer(0), 
                      transactionRevenue  = numeric(0), 
                      CPA  = numeric(0), 
                      ROI = numeric(0)))
    
  }
  report.ga.google.cpc2<-NA
  if (length(goals_and_transactions)<=8){
    report.ga.google.cpc2<-google_analytics_4(ga_view_id,
                                         date_range = c(date_start, date_end), 
                                         metrics = c("impressions", "sessions", goals_and_transactions), 
                                         dimensions = c("campaign", "keyword"), 
                                         dim_filters = my_filter_clause, anti_sample = TRUE)
  }else if(length(goals_and_transactions)<=16){
    report.ga.google.cpc2.1<-google_analytics_4(ga_view_id,
                                              date_range = c(date_start, date_end), 
                                              metrics = c("impressions", "sessions", goals_and_transactions[1:8]), 
                                              dimensions = c("campaign", "keyword"), 
                                              dim_filters = my_filter_clause, anti_sample = TRUE)
    report.ga.google.cpc2.2<-google_analytics_4(ga_view_id,
                                                date_range = c(date_start, date_end), 
                                                metrics = c("impressions", "sessions", goals_and_transactions[9:length(goals_and_transactions)]), 
                                                dimensions = c("campaign", "keyword"), 
                                                dim_filters = my_filter_clause, anti_sample = TRUE)
    report.ga.google.cpc2<-inner_join(report.ga.google.cpc2.1, report.ga.google.cpc2.2)
    report.ga.google.cpc2<-report.ga.google.cpc2[, c("campaign", "keyword", "impressions", "sessions", goals_and_transactions)]
  }else{
    report.ga.google.cpc2.1<-google_analytics_4(ga_view_id,
                                                date_range = c(date_start, date_end), 
                                                metrics = c("impressions", "sessions", goals_and_transactions[1:8]), 
                                                dimensions = c("campaign", "keyword"), 
                                                dim_filters = my_filter_clause, anti_sample = TRUE)
    report.ga.google.cpc2.2<-google_analytics_4(ga_view_id,
                                                date_range = c(date_start, date_end), 
                                                metrics = c("impressions", "sessions", goals_and_transactions[9:16]), 
                                                dimensions = c("campaign", "keyword"), 
                                                dim_filters = my_filter_clause, anti_sample = TRUE)
    report.ga.google.cpc2.3<-google_analytics_4(ga_view_id,
                                                date_range = c(date_start, date_end), 
                                                metrics = c("impressions", "sessions", goals_and_transactions[17:length(goals_and_transactions)]), 
                                                dimensions = c("campaign", "keyword"), 
                                                dim_filters = my_filter_clause, anti_sample = TRUE)
    
    report.ga.google.cpc2<-inner_join(report.ga.google.cpc2.1, report.ga.google.cpc2.2)
    report.ga.google.cpc2<-inner_join(report.ga.google.cpc2, report.ga.google.cpc2.3)
    
    report.ga.google.cpc2<-report.ga.google.cpc2[, c("campaign", "keyword", "impressions", "sessions", goals_and_transactions)]
    
  }
  
  

  report.ga.google.cpc<-inner_join(report.ga.google.cpc1, report.ga.google.cpc2, by=c("campaign", "keyword","impressions"))
  
  
  report.ga.google.cpc[, c(-1,-2, -3)]<-apply(report.ga.google.cpc[, c(-1,-2, -3)], 2, as.numeric)
  
  if (length(goals_and_transactions)==1){
    report.ga.google.cpc$allGoalsCompletions<-report.ga.google.cpc[,goals_and_transactions]
  }else{
    report.ga.google.cpc$allGoalsCompletions<-apply(report.ga.google.cpc[,goals_and_transactions], 1, sum)
  }
    
  report.ga.google.cpc$CPC<-round(report.ga.google.cpc$adCost/report.ga.google.cpc$adClicks, 2)
  report.ga.google.cpc$CTR<-round(report.ga.google.cpc$adClicks/report.ga.google.cpc$impressions, 2)
  report.ga.google.cpc$CPA<-round(report.ga.google.cpc$adCost/report.ga.google.cpc$allGoalsCompletions, 2)
  report.ga.google.cpc$ROI<-round(100*(report.ga.google.cpc$transactionRevenue - report.ga.google.cpc$adCost)/report.ga.google.cpc$adCost)
  
  
  report.ga.google.cpc$AvgClickPosition<-NA
  report.ga.google.cpc$AvgImpressionPosition<-NA
  
#  c("campaign_id", "CampaignName", "keyword", "Cost","sessions", "Impressions", "Clicks", "CPC", "AvgClickPosition", "AvgImpressionPosition", "CTR", "allGoalsCompletions", "transactionRevenue", "CPA", "ROI")
  report.ga.google<-report.ga.google.cpc[, c("adwordsCampaignID", "campaign", "keyword", "adCost","sessions", "impressions", "adClicks", "CPC", "AvgClickPosition", "AvgImpressionPosition", "CTR", "allGoalsCompletions", "transactionRevenue", "CPA", "ROI")]
  colnames(report.ga.google)<-c("campaign_id", "campaign", "keyword", "Cost","sessions", "Impressions", "Clicks", "CPC", "AvgClickPosition", "AvgImpressionPosition", "CTR", "allGoalsCompletions", "transactionRevenue", "CPA", "ROI")
  
  return(report.ga.google)
}
count_dif <- function(x1, x2){
  result <- as.integer(100*(x2 - x1)/x1)
  #if some data appeard in the second period, difference is 100%
  result[x1 == 0 & x2 != 0]<-100
  return(result)
}
dif_report<-function(report1, report2){
  tmp<-full_join(report2, report1, by<-c("campaign", "keyword"))
  tmp[is.na(tmp)]<-0
  #if some data appeard in the second period, difference is 100%
  
  
  result<-data.frame(campaign_id=tmp[, c("campaign_id.x")])
  
  
  result$campaign <- tmp$campaign
  result$keyword <- tmp$keyword
  result$Cost <- count_dif(tmp$Cost.y, tmp$Cost.x) 
  
  result$sessions <- count_dif(tmp$sessions.y, tmp$sessions.x)
  
  result$Impressions <- count_dif(tmp$Impressions.y, tmp$Impressions.x)
  
  result$Clicks <- count_dif(tmp$Clicks.y, tmp$Clicks.x)
  
  result$CPC <- count_dif(tmp$CPC.y, tmp$CPC.x)
  
  
  result$AvgClickPosition<-
    ifelse(is.na(tmp$AvgClickPosition.x)|tmp$AvgClickPosition.x=="--"|is.na(tmp$AvgClickPosition.y)|tmp$AvgClickPosition.y=="--", 
           NA, as.integer((as.numeric(tmp$AvgClickPosition.x) - as.numeric(tmp$AvgClickPosition.y))/as.numeric(tmp$AvgClickPosition.y))*100)

  
  result$AvgImpressionPosition<-
    ifelse(is.na(tmp$AvgImpressionPosition.x)|tmp$AvgImpressionPosition.x=="--"|is.na(tmp$AvgImpressionPosition.y)|tmp$AvgImpressionPosition.y=="--", 
           NA, as.integer((as.numeric(tmp$AvgImpressionPosition.x) - as.numeric(tmp$AvgImpressionPosition.y))/as.numeric(tmp$AvgImpressionPosition.y)*100))
  
  
  result$CTR <- count_dif(tmp$CTR.y, tmp$CTR.x)
  
  result$allGoalsCompletions <- count_dif(tmp$allGoalsCompletions.y, tmp$allGoalsCompletions.x)
  
  result$transactionRevenue <- count_dif(tmp$transactionRevenue.y, tmp$transactionRevenue.x)
  
  result$CPA <- count_dif(tmp$CPA.y, tmp$CPA.x)
  
  result$ROI <- count_dif(tmp$ROI.y, tmp$ROI.x)
  
  return(result)
}



summarise_by_campain<-function(report){
  b<-dplyr::summarise(group_by(report, campaign_id, campaign), 
                      Cost=sum(Cost, na.rm = TRUE), 
                      sessions=sum(sessions, na.rm=TRUE), 
                      Impressions=sum(Impressions, na.rm=TRUE), 
                      Clicks = sum(Clicks, na.rm=TRUE), 
                      AvgClickPosition=mean(AvgClickPosition, na.rm=TRUE), 
                      AvgImpressionPosition=mean(AvgImpressionPosition, na.rm=TRUE), 
                      allGoalsCompletions=sum(allGoalsCompletions, na.rm=TRUE), 
                      transactionRevenue=sum(transactionRevenue, na.rm=TRUE))
  b<-data.frame(b)
  
  b$keyword<-rep(" все", nrow(b))
  
  b$CPC<-round(b$Cost/b$Clicks, 2)
  
  b$CTR<-round(b$Clicks/b$Impressions, 2)
  
  b$CPA<-round(b$Cost/b$allGoalsCompletions, 2)
  

  b$ROI<-round(100*(b$transactionRevenue - b$Cost)/b$Cost)
  
  b<-b[, c("campaign_id","campaign", "keyword", "Cost", "sessions", "Impressions", "Clicks", "CPC", "AvgClickPosition",  
    "AvgImpressionPosition", "CTR", "allGoalsCompletions", "transactionRevenue", "CPA", "ROI")]                

  is.na(b)<-sapply(b, is.infinite)
  
  is.na(b)<-sapply(b, is.nan)
  
  
  return(b)
}
round_values<-function(report){
  num_cols<-purrr::map_lgl(report, is.numeric)
  report[num_cols]<-round(report[num_cols],2)
 
  return(report)
}
remove_na_rows<-function(report){
  not_na_rows<-apply(report[,c(-1,-2,-3)], 1, function(x){any(!is.na(x))})
  return(report[not_na_rows,])
}

#MAIN
form_reports<-function(date_start1, date_end1, date_start2, date_end2, updateProgress = NULL){
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
  report.ya.1<-make_report_ya(date_start1, date_end1)
  
 
  if (is.function(updateProgress)) {
    updateProgress(4/n, "load yandex data for period 2")
  }
  report.ya.2<-make_report_ya(date_start2, date_end2)
  
  ##remove_nan_inf
  if (nrow(report.ya.1) != 0){
    is.na(report.ya.1)<-sapply(report.ya.1, is.infinite)
    is.na(report.ya.1)<-sapply(report.ya.1, is.nan)
  }
  if (nrow(report.ya.2) != 0){  
    is.na(report.ya.2)<-sapply(report.ya.2, is.infinite)
    is.na(report.ya.2)<-sapply(report.ya.2, is.nan)
  }
  

  #report google
  if (is.function(updateProgress)) {
    updateProgress(5/n, "load google data for period 1")
  }    
  
  report.google.1<-make_report_google(date_start1, date_end1)
  
  if (is.function(updateProgress)) {
    updateProgress(6/n, "load google data for period 2")
  }    
  
  report.google.2<-make_report_google(date_start2, date_end2)
  
  if(nrow(report.google.1) != 0){
    is.na(report.google.1)<-sapply(report.google.1, is.infinite)
    is.na(report.google.1)<-sapply(report.google.1, is.nan)
  }
  if(nrow(report.google.2) != 0){
    is.na(report.google.2)<-sapply(report.google.2, is.infinite)
    is.na(report.google.2)<-sapply(report.google.2, is.nan)
  }
  
  
  ##add summary
  if (is.function(updateProgress)) {
    updateProgress(7/n, "add summary")
  }
  
  if (nrow(report.ya.1) != 0){
    report.ya.1<-rbind(summarise_by_campain(report.ya.1), report.ya.1)
  }
  if (nrow(report.ya.2) != 0){
    report.ya.2<-rbind(summarise_by_campain(report.ya.2), report.ya.2)
  }
  if (nrow(report.google.1) != 0){
    report.google.1<-rbind(summarise_by_campain(report.google.1), report.google.1)
  }
  if (nrow(report.google.2) != 0){
    report.google.2<-rbind(summarise_by_campain(report.google.2), report.google.2)
  }
  
  
  if (is.function(updateProgress)) {
    updateProgress(8/n, "create diff report")
  }  
  report.ya.dif<-dif_report(report.ya.1, report.ya.2)
  report.google.dif<-dif_report(report.google.1, report.google.2)  
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
    report.ya.1<-remove_na_rows(report.ya.1)
    report.ya.2<-remove_na_rows(report.ya.2)
    report.ya.dif<-remove_na_rows(report.ya.dif)
    
    report.google.1<-remove_na_rows(report.google.1)
    report.google.2<-remove_na_rows(report.google.2)
    report.google.dif<-remove_na_rows(report.google.dif)
    
  }
#conversion
  report.ya.1$allGoalsCompletions<-report.ya.1$allGoalsCompletions/report.ya.1$sessions
  report.ya.2$allGoalsCompletions<-report.ya.2$allGoalsCompletions/report.ya.2$sessions
  
  report.google.1$allGoalsCompletions<-report.google.1$allGoalsCompletions/report.google.1$sessions
  report.google.2$allGoalsCompletions<-report.google.2$allGoalsCompletions/report.google.2$sessions

  
  
  
  if (is.function(updateProgress)) {
    updateProgress(10/n, "round values")
  }  
  report.ya.1<-round_values(report.ya.1)
  report.ya.2<-round_values(report.ya.2)
  report.google.1<-round_values(report.google.1)
  report.google.2<-round_values(report.google.2)
  
  
  if (is.function(updateProgress)) {
    updateProgress(11/n, "rename columns")
  }  
  
  cnames<-c("campaign_id",	"Название кампании", "Ключевые слова",   "Расход", "Сессии", 
            "Показы", "Клики", "CPC", "Ср. поз. клика", "Ср. поз. показа", "CTR", "Конверсия","Доход", 
            "CPA", "ROI")
  
  colnames(report.ya.1)<-cnames
  colnames(report.ya.2)<-cnames
  colnames(report.ya.dif)<-cnames
  colnames(report.google.1)<-cnames
  colnames(report.google.2)<-cnames
  colnames(report.google.dif)<-cnames
  
  
  return(list(report.ya.1, report.ya.2, report.ya.dif, report.google.1, report.google.2, report.google.dif))
}

init<-function(gaview_id=ga_view_id, ya_login=yalogin, goals=goals_ga_numbers, google_account="sz.mastim", yandex_account="stbinario"){
  ga_view_id<<-gaview_id
  yalogin<<-ya_login
  goals_ga_numbers<<-goals
  
  #goals for ga + transactions
  goals_cmpl<-c()
  if (!is.null(goals_ga_numbers)){
    goals_cmpl<-paste("goal", goals_ga_numbers, "Completions", sep="")  
  }
  goals_and_transactions<<-c("transactions", goals_cmpl)

  google.account<<-google_account
  yandex.account<<-yandex_account
  

}
auth<-function(google_account="sz.mastim", yandex_account="stbinario" ){
  #authentification
  #yandex.direct
  ya_fname<-paste(yandex_account, ".yadirAuth.RData", sep="")
  load(ya_fname)#readChar(ya_fname, file.info(ya_fname)$size)
  my_token<<-token$access_token
  #ga
  gar_auth(paste(google_account,".httr-oauth", sep=""))
}