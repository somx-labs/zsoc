if (!require('shiny')) install.packages("shiny")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('dplyr')) install.packages("dplyr")
if (!require('DT')) install.packages("DT")
if (!require('scales')) install.packages("scales")
if (!require('data.table')) install.packages("data.table")

# Clear environment
rm(list=ls(all=TRUE))

# Change directory
# If you are getting error set this path correctly
setwd('data/')

dataf <- fread("dataf_sentiment.csv", header= T, sep = ",")
#dataf <- read.csv("dataf_sentiment.csv")
dataf <- data.frame(dataf)


shinyServer(function(input, output) {
  data <- reactive({
  })
  
  output$reactionplot <- renderPlot({
    if(0 == length(input$company)){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = sum(num_reactions))    
    }
    else if("All" %in% input$company){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = sum(num_reactions))
    }else {
      tdata <- dataf %>% filter(website %in% tolower(input$company)) %>% filter(status_pub_year == as.integer(input$Year)) %>% group_by(website, status_pub_year, status_pub_month) %>% summarise(count = sum(num_reactions))
    }
    
    ggplot(tdata, aes(x=status_pub_month, y=count, group=website)) + 
      geom_line(aes(color=website)) + 
      geom_point(aes(color=website)) +
      labs(title="Reactions per month",x=paste0("Year ", input$Year), y = "Reactions") +
      scale_x_discrete(limits=month.abb) +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#ef3b2c", "#0868ac", "#8c510a"), name=c("Company")) +
      expand_limits(x = 0, y = 0) +
      theme(legend.position="top")
      
  })
  
  output$commentsplot <- renderPlot({
    if(0 == length(input$company)){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = sum(num_comments))    
    }
    else if("All" %in% input$company){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = sum(num_comments))
    }else {
      tdata <- dataf %>% filter(website %in% tolower(input$company)) %>% filter(status_pub_year == as.integer(input$Year)) %>% group_by(website, status_pub_year, status_pub_month) %>% summarise(count = sum(num_comments))
    }
    ggplot(tdata, aes(x=status_pub_month, y=count, group=website)) + 
      geom_line(aes(color=website)) + 
      geom_point(aes(color=website)) +
      labs(title="Comments per month",x=paste0("Year ", input$Year), y = "Comments") +
      scale_x_discrete(limits=month.abb) +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#ef3b2c", "#0868ac", "#8c510a"), name=c("Company")) +
      expand_limits(x = 0, y = 0) +
      theme(legend.position="top")
  })
  
  output$shareplot <- renderPlot({
    if(0 == length(input$company)){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = sum(num_shares))    
    }
    else if("All" %in% input$company){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = sum(num_shares))
    }else {
      tdata <- dataf %>% filter(website %in% tolower(input$company)) %>% filter(status_pub_year == as.integer(input$Year)) %>% group_by(website, status_pub_year, status_pub_month) %>% summarise(count = sum(num_shares))
    }
    ggplot(tdata, aes(x=status_pub_month, y=count, group=website)) + 
      geom_line(aes(color=website)) + 
      geom_point(aes(color=website)) +
      labs(title="Shares per month",x=paste0("Year ", input$Year), y = "Shares") +
      scale_x_discrete(limits=month.abb) +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#ef3b2c", "#0868ac", "#8c510a"), name=c("Company")) +
      expand_limits(x = 0, y = 0) +
      theme(legend.position="top")
  })
  
  output$postplot <- renderPlot({
    if(0 == length(input$company)){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == tolower(input$statusType)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = n())    
    }
    else if("All" %in% input$company){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == tolower(input$statusType)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = n())
    }else {
      tdata <- dataf %>% filter(website %in% tolower(input$company), status_type == tolower(input$statusType)) %>% filter(status_pub_year == as.integer(input$Year)) %>% group_by(website, status_pub_year, status_pub_month) %>% summarise(count = n())
    }
    
    ggplot(tdata, aes(x=status_pub_month, y=count, group=website)) + 
      geom_line(aes(color=website)) + 
      geom_point(aes(color=website)) +
      labs(title=paste0("Total ", input$statusType, " Posted/Month") ,x=paste0("Year ", input$Year), y = input$statusType) +
      scale_x_discrete(limits=month.abb) +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#ef3b2c", "#0868ac", "#8c510a"), name=c("Company")) +
      expand_limits(x = 0, y = 0) +
      theme(legend.position="top")
    
  })
  
  output$videoplot <- renderPlot({
    if(0 == length(input$company)){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == "video") %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = n())    
    }
    else if("All" %in% input$company){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == "video") %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = n())
    }else {
      tdata <- dataf %>% filter(website %in% tolower(input$company)) %>% filter(status_pub_year == as.integer(input$Year), status_type == "video") %>% group_by(website, status_pub_year, status_pub_month) %>% summarise(count = n())
    }
    ggplot(tdata, aes(x=status_pub_month, y=count, group=website)) + 
      geom_line(aes(color=website)) + 
      geom_point(aes(color=website)) +
      labs(title="Videos per month",x=paste0("Year ", input$Year), y = "Video") +
      scale_x_discrete(limits=month.abb) +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#ef3b2c", "#0868ac", "#8c510a"), name=c("Company")) +
      expand_limits(x = 0, y = 0) +
      theme(legend.position="top")
  })
  
  output$linksplot <- renderPlot({
    if(0 == length(input$company)){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == "link") %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = n())    
    }
    else if("All" %in% input$company){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == "link") %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = n())
    }else {
      tdata <- dataf %>% filter(website %in% tolower(input$company)) %>% filter(status_pub_year == as.integer(input$Year), status_type == "link") %>% group_by(website, status_pub_year, status_pub_month) %>% summarise(count = n())
    }
    ggplot(tdata, aes(x=status_pub_month, y=count, group=website)) + 
      geom_line(aes(color=website)) + 
      geom_point(aes(color=website)) +
      labs(title="Links per month",x=paste0("Year ", input$Year), y = "Links") +
      scale_x_discrete(limits=month.abb) +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#ef3b2c", "#0868ac", "#8c510a"), name=c("Company")) +
      expand_limits(x = 0, y = 0) +
      theme(legend.position="top")
  })
  
  output$photosplot <- renderPlot({
    if(0 == length(input$company)){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == "photo") %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = n())    
    }
    else if("All" %in% input$company){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == "photo") %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = n())
    }else {
      tdata <- dataf %>% filter(website %in% tolower(input$company)) %>% filter(status_pub_year == as.integer(input$Year), status_type == "photo") %>% group_by(website, status_pub_year, status_pub_month) %>% summarise(count = n())
    }
    ggplot(tdata, aes(x=status_pub_month, y=count, group=website)) + 
      geom_line(aes(color=website)) + 
      geom_point(aes(color=website)) +
      labs(title="Photos per month",x=paste0("Year ", input$Year), y = "Photos") +
      scale_x_discrete(limits=month.abb) +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#ef3b2c", "#0868ac", "#8c510a"), name=c("Company")) +
      expand_limits(x = 0, y = 0) +
      theme(legend.position="top")
  })
  
  output$postreactionplot <- renderPlot({
    if(0 == length(input$company)){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == tolower(input$statusType)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = sum(num_reactions))    
    }
    else if("All" %in% input$company){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == tolower(input$statusType)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = sum(num_reactions))
    }else {
      tdata <- dataf %>% filter(website %in% tolower(input$company), status_pub_year == as.integer(input$Year),status_type == tolower(input$statusType)) %>% group_by(website, status_pub_year, status_pub_month) %>% summarise(count = sum(num_reactions))
    }
    ggplot(tdata, aes(x=status_pub_month, y=count, group=website)) + 
      geom_line(aes(color=website)) + 
      geom_point(aes(color=website)) +
      labs(title=paste0("Reactions(Like,haha,angry...) on ", input$statusType ," Posted/Month"),x=paste0("Year ", input$Year), y = "Count") +
      scale_x_discrete(limits=month.abb) +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#ef3b2c", "#0868ac", "#8c510a"), name=c("Company")) +
      expand_limits(x = 0, y = 0) +
      theme(legend.position="top")
  })
  
  output$postcommentsplot <- renderPlot({
    if(0 == length(input$company)){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == tolower(input$statusType)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = sum(num_comments))    
    }
    else if("All" %in% input$company){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == tolower(input$statusType)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = sum(num_comments))
    }else {
      tdata <- dataf %>% filter(website %in% tolower(input$company), status_type == tolower(input$statusType), status_pub_year == as.integer(input$Year)) %>% group_by(website, status_pub_year, status_pub_month) %>% summarise(count = sum(num_comments))
    }
    ggplot(tdata, aes(x=status_pub_month, y=count, group=website)) + 
      geom_line(aes(color=website)) + 
      geom_point(aes(color=website)) +
      labs(title=paste0("Comments for ",input$statusType, " Posted/Month"),x=paste0("Year ", input$Year), y = "Count") +
      scale_x_discrete(limits=month.abb) +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#ef3b2c", "#0868ac", "#8c510a"), name=c("Company")) +
      expand_limits(x = 0, y = 0) +
      theme(legend.position="top")
  })
  

  output$postsharedplot <- renderPlot({
    if(0 == length(input$company)){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == tolower(input$statusType)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = sum(num_shares))    
    }
    else if("All" %in% input$company){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year), status_type == tolower(input$statusType)) %>% group_by(website, status_pub_year, status_pub_month)  %>% summarise(count = sum(num_shares))
    }else {
      tdata <- dataf %>% filter(website %in% tolower(input$company), status_type == tolower(input$statusType), status_pub_year == as.integer(input$Year)) %>% group_by(website, status_pub_year, status_pub_month) %>% summarise(count = sum(num_shares))
    }
    ggplot(tdata, aes(x=status_pub_month, y=count, group=website)) + 
      geom_line(aes(color=website)) + 
      geom_point(aes(color=website)) +
      labs(title=paste0(input$statusType, " Shared/Month"),x=paste0("Year ", input$Year), y = "Count") +
      scale_x_discrete(limits=month.abb) +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#ef3b2c", "#0868ac", "#8c510a"), name=c("Company")) +
      expand_limits(x = 0, y = 0) +
      theme(legend.position="top")
  })
  
  output$posttimeplot <- renderPlot({
    
    if(("All" %in% input$month)){
      tdata <- dataf
    }else {
      tdata <- dataf %>% filter(status_pub_month == match(input$month, month.abb))
    }
    
    if((0 == length(input$company))){
      tdata <- tdata %>% filter(status_pub_year == as.integer(input$Year), status_type == tolower(input$statusType)) %>% group_by(website, status_pub_year, status_pub_hour)  %>% summarise(count = n(), responses = sum(num_reactions, num_comments, num_shares))    
    }
    else if(("All" %in% input$company)){
      tdata <- tdata %>% filter(status_pub_year == as.integer(input$Year), status_type == tolower(input$statusType)) %>% group_by(website, status_pub_year, status_pub_hour)  %>% summarise(count = n(), responses = sum(num_reactions, num_comments, num_shares))
    }else {
      tdata <- tdata %>% filter(website %in% tolower(input$company), status_pub_year == as.integer(input$Year), status_type == tolower(input$statusType)) %>% group_by(website, status_pub_year, status_pub_hour) %>% summarise(count = n(), responses = sum(num_reactions, num_comments, num_shares))
    }
    
    ggplot(tdata, aes(x=status_pub_hour, y = count, group=website)) +
      geom_point(aes(color=website, size = responses)) +
      #expand_limits(x = 0, y = 0) +
      scale_x_continuous(breaks = scales::pretty_breaks(n=23)) +
      labs(title=paste0("Post Time Analysis for ", input$statusType), x="Hour", y = "Count") +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#ef3b2c", "#0868ac", "#8c510a"), name=c("Company")) +
      scale_size_continuous(labels = comma, name = c("Responses")) +
      theme(legend.position="top")
  })
  
  output$toppost <- DT::renderDataTable(DT::datatable({
    if(("All" %in% input$month)){
      tdata <- dataf
    }
    else {
      tdata <- dataf %>% filter(status_pub_month == match(input$month, month.abb))
    }
    
    if((0 == length(input$company))){
      tdata <- tdata %>% filter(status_pub_year == as.integer(input$Year))
    }
    else if("All" %in% input$company){
      tdata <- tdata %>% filter(status_pub_year == as.integer(input$Year))
    }
    else {
      tdata <- tdata %>% filter(status_pub_year == as.integer(input$Year), website %in% tolower(input$company))
    }
    
    top_post_d <- tdata %>% group_by(website)  %>% top_n(n = 5, wt = num_reactions) %>% select(status_message, num_reactions, num_comments, num_shares, website)
  }))
  
  output$sentcoplot <- renderPlot({
    if((0 == length(input$company))){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year)) %>% group_by(status_pub_year, status_pub_month) %>% summarise(positive = mean(positive), negative = mean(negative))
    }
    else if(("All" %in% input$company)){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year)) %>% group_by(status_pub_month) %>% summarise(positive = mean(positive), negative = mean(negative))
    }else {
      tdata <- dataf %>% filter(website == tolower(input$company), status_pub_year == as.integer(input$Year)) %>% group_by(status_pub_month) %>% summarise(positive = mean(positive), negative = mean(negative))
    }
    
    posnegtime <- melt(tdata, id=c("status_pub_month"))
    names(posnegtime) <- c("timestamp", "sentiment", "meanvalue")
    posnegtime$sentiment = factor(posnegtime$sentiment,levels(posnegtime$sentiment)[c(2,1)])
    
    ggplot(data = posnegtime, aes(x = timestamp, y = meanvalue, group = sentiment)) +
      geom_line(size = 1.0, alpha = 0.5, aes(color = sentiment)) +
      geom_point(aes(color = sentiment)) +
      ylim(0, NA) + 
      scale_colour_manual(values = c("firebrick3", "springgreen4")) +
      theme(legend.position="top") +
      ylab("Average Sentiment Score") + 
      scale_x_discrete(limits=month.abb) +
      xlab(paste0("Month of ", input$Year)) +
      ggtitle(paste0("Sentiment Over Time for ", input$company))
    
  })
  
  output$sentallplot <- renderPlot({
    if((0 == length(input$company))){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year))
    }
    else if(("All" %in% input$company)){
      tdata <- dataf %>% filter(status_pub_year == as.integer(input$Year))
    }else {
      tdata <- dataf %>% filter(website == tolower(input$company), status_pub_year == as.integer(input$Year))
    }    
    
    sentimentTotals <- data.frame(colSums(tdata[,c(23:32)]))
    names(sentimentTotals) <- "count"
    sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
    rownames(sentimentTotals) <- NULL
    
    ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
      geom_bar(aes(fill = sentiment), stat = "identity") +
      scale_fill_brewer(palette="Spectral") +
      theme(legend.position = "none") +
      xlab("Sentiment Type") + ylab("Count") + ggtitle(paste0("Total Sentiment Score for ",input$company," Statuses"))
  })
  
  output$pdfviewer <- renderText({
    return('<iframe style="height:600px; width:100%" src="https://dcrucs.co/zsoc/dcrucs_zsoc_report.pdf"></iframe>')
  })
})