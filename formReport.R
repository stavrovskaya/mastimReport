library(googleAnalyticsR)
library(googleAuthR)
library(devtools)
library(dplyr)
library(ryandexdirect)
library(RAdwords)
library(tidyverse)
library(plyr)
library(rmarkdown)


#dates are comming from shiny
remove_na = T

google.account=NA
yandex.account=NA
ga_view_id=120758474
yalogin<-"biolatic-project"
goals_ga_numbers<-c(6, 12, 13, 15, 16, 17, 3, 20)

out_dir<-gsub("/[^/]+$", "/Results", getwd())

#adwords_id="434-145-7765"
#Био-китай 647-210-4409


make_report_ya<-function(date_start, date_end){
  #get data from direct
  report.direct<-yadirGetReport(ReportType = "CRITERIA_PERFORMANCE_REPORT", 
                                DateRangeType="CUSTOM_DATE", DateFrom = date_start, DateTo = date_end, 
                                FieldNames = c("CampaignName","CampaignId","Impressions","Criteria", "CriteriaType", "Clicks","Cost", "AvgClickPosition", "AvgImpressionPosition"), 
                                FilterList=NULL, IncludeVAT = "NO", IncludeDiscount = "NO", Login = yalogin, Token = my_token)
  
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
            y<-apply(x[,c(-1,-2,-4,-5, -8, -9)],2,sum)
            y<-c(CampaignName=max(levels(x[,1])), Criteria=NA, CriteriaType = max(as.vector(x[,5])),
                 y, AvgClickPosition=max(x[,8]), AvgImpressionPosition=max(x[,9]))
            
          })
  report.direct.non_keyword$Impressions<-as.integer(report.direct.non_keyword$Impressions)
  report.direct.non_keyword$Clicks<-as.integer(report.direct.non_keyword$Clicks)
  report.direct.non_keyword$AvgClickPosition<-as.numeric(report.direct.non_keyword$AvgClickPosition)
  report.direct.non_keyword$AvgImpressionPosition<-as.numeric(report.direct.non_keyword$AvgImpressionPosition)
  report.direct.non_keyword$Cost<-as.numeric(report.direct.non_keyword$Cost)
  
  #campaign_id.non_keyword<-unique(report.direct.non_keyword$CampaignId)
  
  #ads_info.non_keyword<-yadirGetAds(CampaignIds = campaign_id.non_keyword, Login = yalogin, Token = my_token)
  #ad_groups_info.non_keyword<-yadirGetAdGroups(CampaignIds = campaign_id.non_keyword, Login = yalogin, Token = my_token)
  
  #get data from ga
  
  #"yandex / (cpc|cpm)"
  source_medium_filter <- dim_filter(dimension="sourceMedium",operator="REGEXP",expressions="yandex / (cpc)")
  
  campaign_filter <- dim_filter(dimension="campaign",operator="REGEXP",expressions="[A-Za-z_-]+\\|[0-9]+")
  
  
  my_filter_clause <- filter_clause_ga4(list(source_medium_filter, campaign_filter), operator = "AND")
  
  
  goals<-paste("goal", goals_ga_numbers, "Completions", sep="")
  
  report.ga.ya.cpc<-NA
  if (length(goals) <= 8){
    report.ga.ya.cpc<-google_analytics_4(ga_view_id,
                                         date_range = c(date_start, date_end), 
                                         metrics = c("sessions", "transactionRevenue", goals), 
                                         dimensions = c("campaign", "keyword", "sourceMedium"), 
                                         dim_filters = my_filter_clause, anti_sample = TRUE)
  }else if(length(goals) <= 16){
    report.ga.ya.cpc1<-google_analytics_4(ga_view_id,
                                         date_range = c(date_start, date_end), 
                                         metrics = c("sessions", "transactionRevenue", goals[1:8]), 
                                         dimensions = c("campaign", "keyword", "sourceMedium"), 
                                         dim_filters = my_filter_clause, anti_sample = TRUE)    
    report.ga.ya.cpc2<-google_analytics_4(ga_view_id,
                                          date_range = c(date_start, date_end), 
                                          metrics = c("sessions", goals[9:length(goals)]), 
                                          dimensions = c("campaign", "keyword", "sourceMedium"), 
                                          dim_filters = my_filter_clause, anti_sample = TRUE)  
    report.ga.ya.cpc<-inner_join(report.ga.ya.cpc1, report.ga.ya.cpc2)
    report.ga.ya.cpc<-report.ga.ya.cpc[,c( "campaign", "keyword", "sourceMedium", "sessions", "transactionRevenue", goals)]
  }else{
    report.ga.ya.cpc1<-google_analytics_4(ga_view_id,
                                          date_range = c(date_start, date_end), 
                                          metrics = c("sessions", "transactionRevenue", goals[1:8]), 
                                          dimensions = c("campaign", "keyword", "sourceMedium"), 
                                          dim_filters = my_filter_clause, anti_sample = TRUE)    
    report.ga.ya.cpc2<-google_analytics_4(ga_view_id,
                                          date_range = c(date_start, date_end), 
                                          metrics = c("sessions", goals[9:16]), 
                                          dimensions = c("campaign", "keyword", "sourceMedium"), 
                                          dim_filters = my_filter_clause, anti_sample = TRUE) 
    report.ga.ya.cpc3<-google_analytics_4(ga_view_id,
                                          date_range = c(date_start, date_end), 
                                          metrics = c("sessions", goals[17:length(goals)]), 
                                          dimensions = c("campaign", "keyword", "sourceMedium"), 
                                          dim_filters = my_filter_clause, anti_sample = TRUE)  
    
    report.ga.ya.cpc<-inner_join(report.ga.ya.cpc1, report.ga.ya.cpc2)
    report.ga.ya.cpc<-inner_join(report.ga.ya.cpc, report.ga.ya.cpc3)
    report.ga.ya.cpc<-report.ga.ya.cpc[,c( "campaign", "keyword", "sourceMedium", "sessions", "transactionRevenue", goals)]
    
  }
  report.ga.ya.cpc<-mutate(report.ga.ya.cpc, campaign_id=sub("[A-Za-z_-]+\\|([0-9]+)", "\\1", campaign))
  report.ga.ya.cpc$campaign_id<-as.integer(report.ga.ya.cpc$campaign_id)
  
  #make numeric from character columns
  
  report.ga.ya.cpc.keyword<-dplyr::filter(report.ga.ya.cpc, keyword != "(not set)")
  
  report.ga.ya.cpc.keyword<-ddply(report.ga.ya.cpc.keyword,
                                  .(campaign_id, keyword),
                                  function(x){
                                    y<-apply(x[,c(-1,-2,-3, -ncol(report.ga.ya.cpc.keyword))],2,sum); 
                                    y<-c(campaign=max(x[,1]), sourceMedium=max(x[,3]), y)
                                  })
  #make numeric from character columns
  report.ga.ya.cpc.keyword[, c(-1,-2,-3, -4)]<-apply(report.ga.ya.cpc.keyword[, c(-1,-2,-3, -4)], 2, as.numeric)
  
  
  report.ga.ya.cpc.non_keyword<-dplyr::filter(report.ga.ya.cpc, keyword == "(not set)")
  
  report.ga.ya.cpc.non_keyword<-ddply(report.ga.ya.cpc.non_keyword,
                                      .(campaign_id),
                                      function(x){
                                        y<-apply(x[,c(-1,-2,-3, -ncol(report.ga.ya.cpc.non_keyword))],2,sum);
                                        y<-c(campaign=max(x[,1]), keyword=max(x[,2]), sourceMedium=max(x[,3]), y)
                                      })
  #make numeric from character columns
  report.ga.ya.cpc.non_keyword[, c(-1,-2,-3, -4)]<-data.frame(apply(report.ga.ya.cpc.non_keyword[, c(-1,-2,-3, -4)], 2, as.numeric))
  
  
  #join
  #keyword
  report.ya.keyword<-inner_join(report.ga.ya.cpc.keyword, report.direct.keyword, by = c("campaign_id" = "CampaignId", "keyword" = "Criteria_edt"))
  #non_keyword
  report.ya.non_keyword<-inner_join(report.ga.ya.cpc.non_keyword, report.direct.non_keyword, by = c("campaign_id" = "CampaignId"))
  # bind into one result table
  report.ya<-rbind(report.ya.keyword, report.ya.non_keyword[,colnames(report.ya.keyword)])
  report.ya$allGoalsCompletions<-apply(report.ya[,goals], 1, sum)
  report.ya$CPC<-round(report.ya$Cost/report.ya$Clicks, 2)
  report.ya$CTR<-round(report.ya$Clicks/report.ya$Impressions, 2)
  report.ya$CPA<-round(report.ya$Cost/report.ya$allGoalsCompletions, 2)
  report.ya$ROI<-round(100*(report.ya$transactionRevenue - report.ya$Cost)/report.ya$Cost)
  
  
  return(report.ya[, c("campaign_id", "CampaignName", "keyword", "Cost","sessions", "Impressions", "Clicks", "CPC", "AvgClickPosition", "AvgImpressionPosition", "CTR", "allGoalsCompletions", "transactionRevenue", "CPA", "ROI")])
}
#report google adwords 
make_report_google<-function(date_start, date_end){
  source_medium_filter <- dim_filter(dimension="sourceMedium",operator="REGEXP",expressions="google / (cpc)")
  
  
  
  my_filter_clause <- filter_clause_ga4(list(source_medium_filter))
  
  
  goals<-paste("goal", goals_ga_numbers, "Completions", sep="")
  
  report.ga.google.cpc1<-google_analytics_4(ga_view_id,
                                            date_range = c(date_start, date_end), 
                                            metrics = c("impressions", "adClicks", "adCost", "transactionRevenue"), 
                                            dimensions = c("campaign", "keyword","adwordsCampaignID"), 
                                            dim_filters = my_filter_clause, anti_sample = TRUE)  
  report.ga.google.cpc2<-NA
  if (length(goals)<=8){
    report.ga.google.cpc2<-google_analytics_4(ga_view_id,
                                         date_range = c(date_start, date_end), 
                                         metrics = c("impressions", "sessions", goals), 
                                         dimensions = c("campaign", "keyword"), 
                                         dim_filters = my_filter_clause, anti_sample = TRUE)
  }else if(length(goals)<=16){
    report.ga.google.cpc2.1<-google_analytics_4(ga_view_id,
                                              date_range = c(date_start, date_end), 
                                              metrics = c("impressions", "sessions", goals[1:8]), 
                                              dimensions = c("campaign", "keyword"), 
                                              dim_filters = my_filter_clause, anti_sample = TRUE)
    report.ga.google.cpc2.2<-google_analytics_4(ga_view_id,
                                                date_range = c(date_start, date_end), 
                                                metrics = c("impressions", "sessions", goals[9:length(goals)]), 
                                                dimensions = c("campaign", "keyword"), 
                                                dim_filters = my_filter_clause, anti_sample = TRUE)
    report.ga.google.cpc2<-inner_join(report.ga.google.cpc2.1, report.ga.google.cpc2.2)
    report.ga.google.cpc2<-report.ga.google.cpc2[, c("campaign", "keyword", "impressions", "sessions", goals)]
  }else{
    report.ga.google.cpc2.1<-google_analytics_4(ga_view_id,
                                                date_range = c(date_start, date_end), 
                                                metrics = c("impressions", "sessions", goals[1:8]), 
                                                dimensions = c("campaign", "keyword"), 
                                                dim_filters = my_filter_clause, anti_sample = TRUE)
    report.ga.google.cpc2.2<-google_analytics_4(ga_view_id,
                                                date_range = c(date_start, date_end), 
                                                metrics = c("impressions", "sessions", goals[9:16]), 
                                                dimensions = c("campaign", "keyword"), 
                                                dim_filters = my_filter_clause, anti_sample = TRUE)
    report.ga.google.cpc2.3<-google_analytics_4(ga_view_id,
                                                date_range = c(date_start, date_end), 
                                                metrics = c("impressions", "sessions", goals[17:length(goals)]), 
                                                dimensions = c("campaign", "keyword"), 
                                                dim_filters = my_filter_clause, anti_sample = TRUE)
    
    report.ga.google.cpc2<-inner_join(report.ga.google.cpc2.1, report.ga.google.cpc2.2)
    report.ga.google.cpc2<-inner_join(report.ga.google.cpc2, report.ga.google.cpc2.3)
    
    report.ga.google.cpc2<-report.ga.google.cpc2[, c("campaign", "keyword", "impressions", "sessions", goals)]
    
  }
  report.ga.google.cpc<-inner_join(report.ga.google.cpc1, report.ga.google.cpc2, by=c("campaign", "keyword","impressions"))
  
  
  report.ga.google.cpc[, c(-1,-2, -3)]<-apply(report.ga.google.cpc[, c(-1,-2, -3)], 2, as.numeric)
  
  report.ga.google.cpc$allGoalsCompletions<-apply(report.ga.google.cpc[,goals], 1, sum)
  report.ga.google.cpc$CPC<-round(report.ga.google.cpc$adCost/report.ga.google.cpc$adClicks, 2)
  report.ga.google.cpc$CTR<-round(report.ga.google.cpc$adClicks/report.ga.google.cpc$impressions, 2)
  report.ga.google.cpc$CPA<-round(report.ga.google.cpc$adCost/report.ga.google.cpc$allGoalsCompletions, 2)
  report.ga.google.cpc$ROI<-round(100*(report.ga.google.cpc$transactionRevenue - report.ga.google.cpc$adCost)/report.ga.google.cpc$adCost)
  
  
  report.ga.google.cpc$AvgClickPosition<-NA
  report.ga.google.cpc$AvgImpressionPosition<-NA
  
#  c("campaign_id", "CampaignName", "keyword", "Cost","sessions", "Impressions", "Clicks", "CPC", "AvgClickPosition", "AvgImpressionPosition", "CTR", "allGoalsCompletions", "transactionRevenue", "CPA", "ROI")
  report.ga.google<-report.ga.google.cpc[, c("adwordsCampaignID", "campaign", "keyword", "adCost","sessions", "impressions", "adClicks", "CPC", "AvgClickPosition", "AvgImpressionPosition", "CTR", "allGoalsCompletions", "transactionRevenue", "CPA", "ROI")]
  colnames(report.ga.google)<-c("campaign_id", "CampaignName", "keyword", "Cost","sessions", "Impressions", "Clicks", "CPC", "AvgClickPosition", "AvgImpressionPosition", "CTR", "allGoalsCompletions", "transactionRevenue", "CPA", "ROI")
  
  return(report.ga.google)
}

dif_report<-function(report1, report2){
  tmp<-left_join(report2, report1, by<-c("campaign_id", "keyword"))
  result<-data.frame(campaign_id=tmp[, c("campaign_id")])
  result$CampaignName<-tmp$CampaignName.x
  result$keyword<-tmp$keyword
  result$Cost<-as.integer((tmp$Cost.x-tmp$Cost.y)/tmp$Cost.y*100)
  result$sessions<-as.integer((tmp$sessions.x - tmp$sessions.y)/tmp$sessions.y*100)
  result$Impressions<-as.integer((tmp$Impressions.x - tmp$Impressions.y)/tmp$Impressions.y*100)
  result$Clicks<-as.integer((tmp$Clicks.x - tmp$Clicks.y)/tmp$Clicks.y*100)
  result$CPC<-as.integer((tmp$Cost.x - tmp$Cost.y)/tmp$Cost.y*100)
  result$AvgClickPosition<-
    ifelse(is.na(tmp$AvgClickPosition.x)|tmp$AvgClickPosition.x=="--"|is.na(tmp$AvgClickPosition.y)|tmp$AvgClickPosition.y=="--", 
           NA, as.integer((as.numeric(tmp$AvgClickPosition.x) - as.numeric(tmp$AvgClickPosition.y))/as.numeric(tmp$AvgClickPosition.y))*100)
  result$AvgImpressionPosition<-
    ifelse(is.na(tmp$AvgImpressionPosition.x)|tmp$AvgImpressionPosition.x=="--"|is.na(tmp$AvgImpressionPosition.y)|tmp$AvgImpressionPosition.y=="--", 
           NA, as.integer((as.numeric(tmp$AvgImpressionPosition.x) - as.numeric(tmp$AvgImpressionPosition.y))/as.numeric(tmp$AvgImpressionPosition.y)*100))
  
  result$CTR<-as.integer((tmp$CTR.x - tmp$CTR.y)/tmp$CTR.y*100)
  result$allGoalsCompletions<-as.integer((tmp$allGoalsCompletions.x - tmp$allGoalsCompletions.y)/tmp$allGoalsCompletions.y*100)
  result$transactionRevenue<-as.integer((tmp$transactionRevenue.x - tmp$transactionRevenue.y)/tmp$transactionRevenue.y*100)
  result$CPA<-as.integer((tmp$CPA.x - tmp$CPA.y)/tmp$CPA.y*100)
  result$ROI<-as.integer((tmp$ROI.x - tmp$ROI.y)/tmp$ROI.y*100)
  return(result)
}
remove_Inf_Nan<-function(x){
  x[is.infinite(x)]<-NA
  x[is.nan(x)]<-NA
  return(x)
}
format_Rub<-function(x){
  sprintf("?. %.2f", x)
}
format_Percent<-function(x){
  sprintf("%.2f %%", 100*x)
}
remove_na_rows<-function(report){
  not_na_rows<-apply(report[,c(-1,-2,-3)], 1, function(x){any(!is.na(x))})
  return(report[not_na_rows,])
}
add_blanc_rows<-function(report){
  na_str<-report[1,]
  na_str[,]<-NA
  report_result<-na_str
  
  report<-arrange(report, campaign_id,	CampaignName,	keyword)
  counts<-dplyr::count(report, campaign_id)
  last_ind<-1
  for (i in 1:nrow(counts)){
    report_result<-rbind(report_result, report[last_ind:as.numeric(last_ind+counts[i,2]-1),])
    last_ind<-as.numeric(last_ind+counts[i,2])
    report_result<-rbind(report_result, na_str)
  }
  return(report_result[c(-1),])
}
summarise_by_campain<-function(report){
  b<-dplyr::summarise(group_by(report, campaign_id, CampaignName), Cost=sum(Cost, na.rm = TRUE), sessions=sum(sessions, na.rm=TRUE), Impressions=sum(Impressions, na.rm=TRUE), Clicks = sum(Clicks, na.rm=TRUE), AvgClickPosition=mean(AvgClickPosition, na.rm=TRUE), AvgImpressionPosition=mean(AvgImpressionPosition, na.rm=TRUE), allGoalsCompletions=mean(allGoalsCompletions, na.rm=TRUE), transactionRevenue=sum(transactionRevenue, na.rm=TRUE))
  b<-data.frame(b)
  b$keyword<-rep(enc2utf8(" все"), nrow(b))
  b$CPC<-round(b$Cost/b$Clicks, 2)
  b$CTR<-round(b$Clicks/b$Impressions, 2)
  b$CPA<-round(b$Cost/b$allGoalsCompletions, 2)
  b$ROI<-round(100*(b$transactionRevenue - b$Cost)/b$Cost)
  b<-b[, c(1,2,11,3,4,5,6,12,7,8,13,9,10,14,15)]
  b<-data.frame(b[,c(1,2,3)],apply(b[,c(-1,-2,-3)], 2, remove_Inf_Nan))
  return(b)
}
round_values<-function(report){
  report[, 4]<-round(report[, 4], 2)
  report[, 9]<-round(report[, 9], 2)
  report[, 10]<-round(report[, 10], 2)
  report[, 12]<-round(report[, 12], 2)
  return(report)
}
form_reports<-function(date_start1, date_end1, date_start2, date_end2, updateProgress = NULL){
  #for progress
  n = 11
  
  if (is.function(updateProgress)) {
    updateProgress(2/n, "authentification")
  }
  auth(google.account, yandex.account)

  
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
  report.ya.1<-data.frame(report.ya.1[,c(1,2,3)],apply(report.ya.1[,c(-1,-2,-3)], 2, remove_Inf_Nan))
  report.ya.2<-data.frame(report.ya.2[,c(1,2,3)],apply(report.ya.2[,c(-1,-2,-3)], 2, remove_Inf_Nan))
  
  #report google
  if (is.function(updateProgress)) {
    updateProgress(5/n, "load google data for period 1")
  }    
  
  report.google.1<-make_report_google(date_start1, date_end1)
  
  if (is.function(updateProgress)) {
    updateProgress(6/n, "load google data for period 2")
  }    
  
  report.google.2<-make_report_google(date_start2, date_end2)
  
  
  ##add summary
  if (is.function(updateProgress)) {
    updateProgress(7/n, "add summary")
  }
  report.ya.1<-rbind(summarise_by_campain(report.ya.1), report.ya.1)
  report.ya.2<-rbind(summarise_by_campain(report.ya.2), report.ya.2)
  report.google.1<-rbind(summarise_by_campain(report.google.1), report.google.1)
  report.google.2<-rbind(summarise_by_campain(report.google.2), report.google.2)
  
  
  if (is.function(updateProgress)) {
    updateProgress(8/n, "create diff report")
  }  
  report.ya.dif<-dif_report(report.ya.1, report.ya.2)
  report.google.dif<-dif_report(report.google.1, report.google.2)  
  
  report.ya.dif<-data.frame(report.ya.dif[,c(1,2,3)],apply(report.ya.dif[,c(-1,-2,-3)], 2, remove_Inf_Nan))
  report.google.dif<-data.frame(report.google.dif[,c(1,2,3)],apply(report.google.dif[,c(-1,-2,-3)], 2, remove_Inf_Nan))
  
  
  
  
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
  cnames<-c("campaign_id",	"Название кампании",	"Ключевые слова",	"Расход",	"Сессии",	"Показы",	"Клики",	"CPC",	"Ср. поз. клика",	"Ср. поз. показа",	"CTR",	"Конверсия",	"Доход",	"CPA",	"ROI")
  colnames(report.ya.1)<-cnames
  colnames(report.ya.2)<-cnames
  colnames(report.ya.dif)<-cnames
  colnames(report.google.1)<-cnames
  colnames(report.google.2)<-cnames
  colnames(report.google.dif)<-cnames
  
  
  return(list(report.ya.1, report.ya.2, report.ya.dif, report.google.1, report.google.2, report.google.dif))
}

init<-function(gaview_id, ya_login, goals, google_account="adv.binario", yandex_account="stbinario"){
  ga_view_id<<-gaview_id
  yalogin<<-ya_login
  goals_ga_numbers<<-goals

  google.account<<-google_account
  yandex.account<<-yandex_account
}
auth<-function(google_account="adv.binario", yandex_account="stbinario" ){
  #authentification
  #yandex.direct
  ya_fname<-paste(yandex_account, ".ya.token.txt", sep="")
  my_token <<- readChar(ya_fname, file.info(ya_fname)$size)#yadirGetToken()
  #ga
  gar_auth(paste(google_account,".httr-oauth", sep=""))
}