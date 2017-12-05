library(googleAnalyticsR)
library(devtools)
library(dplyr)
library(ryandexdirect)
library(RAdwords)
library(tidyverse)
library(plyr)
library(rmarkdown)


#dates are comming from shiny
remove_na = T

ga_view_id=120758474
yalogin<-"biolatic-project"
goals_ga_numbers<-c(6, 12, 13, 15, 16, 17, 3, 20)




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
  
  report.ga.ya.cpc<-google_analytics_4(ga_view_id,
                                       date_range = c(date_start, date_end), 
                                       metrics = c("sessions", "transactionRevenue", goals), 
                                       dimensions = c("campaign", "keyword", "sourceMedium"), 
                                       dim_filters = my_filter_clause, anti_sample = TRUE)
  
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
  report.ya$CPC<-report.ya$Cost/report.ya$Clicks
  report.ya$CTR<-report.ya$Clicks/report.ya$Impressions
  report.ya$CPA<-report.ya$Cost/report.ya$allGoalsCompletions
  report.ya$ROI<-100*(report.ya$transactionRevenue - report.ya$Cost)/report.ya$Cost
  
  
  return(report.ya[, c("campaign_id", "CampaignName", "keyword", "Cost","sessions", "Impressions", "Clicks", "CPC", "AvgClickPosition", "AvgImpressionPosition", "CTR", "allGoalsCompletions", "transactionRevenue", "CPA", "ROI")])
}
dif_report<-function(report1, report2){
  tmp<-left_join(report2, report1, by<-c("campaign_id", "keyword"))
  result<-data.frame(campaign_id=tmp[, c("campaign_id")])
  result$CampaignName<-tmp$CampaignName.x
  result$keyword<-tmp$keyword
  result$Cost<-(tmp$Cost.x-tmp$Cost.y)/tmp$Cost.y
  result$sessions<-(tmp$sessions.x - tmp$sessions.y)/tmp$sessions.y
  result$Impressions<-(tmp$Impressions.x - tmp$Impressions.y)/tmp$Impressions.y
  result$Clicks<-(tmp$Clicks.x - tmp$Clicks.y)/tmp$Clicks.y
  result$CPC<-(tmp$Cost.x - tmp$Cost.y)/tmp$Cost.y
  result$AvgClickPosition<-
    ifelse(is.na(tmp$AvgClickPosition.x)|tmp$AvgClickPosition.x=="--"|is.na(tmp$AvgClickPosition.y)|tmp$AvgClickPosition.y=="--", 
           NA, (as.numeric(tmp$AvgClickPosition.x) - as.numeric(tmp$AvgClickPosition.y))/as.numeric(tmp$AvgClickPosition.y))
  result$AvgImpressionPosition<-
    ifelse(is.na(tmp$AvgImpressionPosition.x)|tmp$AvgImpressionPosition.x=="--"|is.na(tmp$AvgImpressionPosition.y)|tmp$AvgImpressionPosition.y=="--", 
           NA, (as.numeric(tmp$AvgImpressionPosition.x) - as.numeric(tmp$AvgImpressionPosition.y))/as.numeric(tmp$AvgImpressionPosition.y))
  
  result$CTR<-(tmp$CTR.x - tmp$CTR.y)/tmp$CTR.y
  result$allGoalsCompletions<-(tmp$allGoalsCompletions.x - tmp$allGoalsCompletions.y)/tmp$allGoalsCompletions.y
  result$transactionRevenue<-(tmp$transactionRevenue.x - tmp$transactionRevenue.y)/tmp$transactionRevenue.y
  result$CPA<-(tmp$CPA.x - tmp$CPA.y)/tmp$CPA.y
  result$ROI<-(tmp$ROI.x - tmp$ROI.y)/tmp$ROI.y
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
form_reports<-function(date_start1, date_end1, date_start2, date_end2){
  auth()
  report.ya.1<-make_report_ya(date_start1, date_end1)
  report.ya.2<-make_report_ya(date_start2, date_end2)
  
  report.ya.dif<-dif_report(report.ya.1, report.ya.2)
  
  #remove_nan_inf
  report.ya.1<-data.frame(report.ya.1[,c(1,2,3)],apply(report.ya.1[,c(-1,-2,-3)], 2, remove_Inf_Nan))
  report.ya.2<-data.frame(report.ya.2[,c(1,2,3)],apply(report.ya.2[,c(-1,-2,-3)], 2, remove_Inf_Nan))
  
  report.ya.dif<-data.frame(report.ya.dif[,c(1,2,3)],apply(report.ya.dif[,c(-1,-2,-3)], 2, remove_Inf_Nan))
  
  #remove na rows from report tables
  if (remove_na){
    report.ya.1<-remove_na_rows(report.ya.1)
    report.ya.2<-remove_na_rows(report.ya.2)
    report.ya.dif<-remove_na_rows(report.ya.dif)
  }
  
  
  
  #add blanc rows between campaigns
  
  
  write.csv(file="report.ya.1.csv", add_blanc_rows(report.ya.1), row.names = F)
  write.csv(file="report.ya.2.csv", add_blanc_rows(report.ya.2), row.names = F)
  write.csv(file="report.ya.dif.csv", add_blanc_rows(report.ya.dif), row.names = F)
  
  
  
  render("reportVV.Rmd", "html_document", output_file = paste("dif_",yalogin, "_report.html", sep=""), output_dir = yalogin,
         params=list(fname="report.ya.dif.csv", type="dif", date1_start=date_start1, date1_end=date_end1, date2_start=date_start2, date2_end=date_end2, name="report: comparison of two periods", prog_name = yalogin))
  render("reportVV.Rmd", "html_document", output_file = paste("period1_", yalogin, "_report.html", sep=""), output_dir = yalogin,
         params=list(fname="report.ya.1.csv", type="plane", date1_start=date_start1, date1_end=date_end1, name="report: period 1", prog_name = yalogin))
  render("reportVV.Rmd", "html_document", output_file = paste("period2_",yalogin, "_report.html", sep=""), output_dir = yalogin,
         params=list(fname="report.ya.2.csv", type="plane", date1_start=date_start2, date1_end=date_end2, name="report: period 2", prog_name = yalogin))
}

init<-function(gaview_id, ya_login, goals){
  ga_view_id<<-gaview_id
  yalogin<<-ya_login
  goals_ga_numbers<<-goals
  
}
auth<-function(){
  #authentification
  #yandex.direct
  my_token <<- "AQAAAAAEsOOqAAMfZVjVmhM-R0P0sq5TZznWzhs"#yadirGetToken()
  #ga
  ga_auth()
}