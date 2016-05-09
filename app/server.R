shinyServer(function(input, output, session){ 

  values <- reactiveValues()
  
#   observe({
#     
#     updateSelectInput(session, 'division', choices=switch(input$country,
#                                                           Greece      = 'Superleague',
#                                                           England     = c('Premier League', 'Championship', 'League 1', 'League 2', 'Conference'),
#                                                           Scotland    = c('Premier  League', 'Division 1', 'Division 2', 'Division 3'),
#                                                           Germany     = c('Bundesliga 1', 'Bundesliga 2'),
#                                                           Italy       = c('Serie A', 'Serie B'),
#                                                           Spain       = c('Primera Division', 'Segunda Division'),
#                                                           France      = c('Ligue 1', 'Ligue 2'),
#                                                           Netherlands = 'Eredivisie',
#                                                           Belgium     = 'Jupiler League',
#                                                           Portugal    = 'Liga 1',
#                                                           Turkey      = 'Ligi 1')
#                       ) 
#     
#     updateSelectInput(session, 'season', choices=switch(input$country,
#                                                         Greece      = rev(paste(1994:2016, shift(1994:2016, type='lead'), sep='-'))[-1],
#                                                         England     = rev(paste(1993:2016, shift(1993:2016, type='lead'), sep='-'))[-1],
#                                                         Scotland    = rev(paste(1994:2016, shift(1994:2016, type='lead'), sep='-'))[-1],
#                                                         Germany     = rev(paste(1993:2016, shift(1993:2016, type='lead'), sep='-'))[-1],
#                                                         Italy       = rev(paste(1993:2016, shift(1993:2016, type='lead'), sep='-'))[-1],
#                                                         Spain       = rev(paste(1993:2016, shift(1993:2016, type='lead'), sep='-'))[-1],
#                                                         France      = rev(paste(1993:2016, shift(1993:2016, type='lead'), sep='-'))[-1],
#                                                         Netherlands = rev(paste(1993:2016, shift(1993:2016, type='lead'), sep='-'))[-1],
#                                                         Belgium     = rev(paste(1995:2016, shift(1995:2016, type='lead'), sep='-'))[-1],
#                                                         Portugal    = rev(paste(1994:2016, shift(1994:2016, type='lead'), sep='-'))[-1],
#                                                         Turkey      = rev(paste(1994:2016, shift(1994:2016, type='lead'), sep='-'))[-1]),
#                       )
#     
#     values$country <- input$country
#   
#   })
#   
#   dataInput <- reactive({
#     
#     country_code       <- switch(isolate(values$country),
#                                  Greece      = 'G',
#                                  England     = 'E',
#                                  Scotland    = 'SC',
#                                  Germany     = 'D',
#                                  Italy       = 'I',
#                                  Spain       = 'SP',
#                                  France      = 'F',
#                                  Netherlands = 'N',
#                                  Belgium     = 'B',
#                                  Portugal    = 'P',
#                                  Turkey      = 'T')
#     
#     division_code      <- switch(input$division,
#                                  `Premier League`    = '0',
#                                  Championship        = '1',
#                                  `League 1`          = '2',
#                                  `League 2`          = '3',
#                                  Conference          = 'C',
#                                  `Premier  League`   = '0',
#                                  `Division 1`        = '1',
#                                  `Division 2`        = '2',
#                                  `Division 3`        = '3',
#                                  `Bundesliga 1`      = '1',
#                                  `Bundesliga 2`      = '2',
#                                  `Serie A`           = '1',
#                                  `Serie B`           = '2',
#                                  `Primera Division`  = '1',
#                                  `Segunda Division`  = '2',
#                                  `Ligue 1`           = '1',
#                                  `Ligue 2`           = '2',
#                                  Eredivisie          = '1',
#                                  `Jupiler League`    = '1',
#                                  `Liga 1`            = '1',
#                                  `Ligi 1`            = '1',
#                                  Superleague         = '1'
#     )
#     
#     season_code        <- paste0(substr(input$season, 3, 4), substr(input$season, 8, 9)) 
#     
#     print(country_code)
#     print(division_code)
#     print(season_code)
#     
#     footy_set <- fread(paste0('http://www.football-data.co.uk/mmz4281/', season_code, '/', country_code, division_code, '.csv'))
#     #setDT(footy_set[, grep('^X', names(footy_set), invert=TRUE, value=TRUE)])
#     footy_set[, Date := as.IDate(Date, format='%d/%m/%y')]
#     footy_set
#     
#   }) 
  
  
  output$raw <- DT::renderDataTable(DT::datatable({
    
    #reuse later
    #footy_table     <- dataInput()
    footy_table      <- footy_tab
    #reuse later
    #footy_table     <- footy_table[between(Date, input$date_range[1], input$date_range[2]), ]
    values$data      <- footy_table
    footy_home_goals <- footy_table[, list(Goals=sum(FTHG),`Total Shots`=sum(HS),`On Target Shots`=sum(HST),`No of Games`=.N),by='HomeTeam']
    setnames(footy_home_goals, 'HomeTeam', 'Team')
    footy_away_goals <- footy_table[, list(Goals=sum(FTAG),`Total Shots`=sum(AS),`On Target Shots`=sum(AST),`No of Games`=.N),by='AwayTeam']
    setnames(footy_away_goals, 'AwayTeam', 'Team')
    footy_all_goals  <- rbindlist(list(footy_home_goals, footy_away_goals))
    values$aggr_data <- footy_all_goals[, list(Goals=sum(Goals),
                                               `Total Shots`=sum(`Total Shots`),
                                               `On Target Shots`=sum(`On Target Shots`),
                                               `No of Games`=sum(`No of Games`)), 
                                        by='Team'][, `Shots per Goal` := `Total Shots` / Goals]
    
    footy_table
    
  }, options=list(autoWidth=TRUE, scrollX=TRUE, columnDefs=list(list(width = '90px', targets = 2)))))
  
  outputOptions(output, 'raw', suspendWhenHidden=FALSE)
  
  output$league_table <- renderTable({
    
    footy_tab  <- values$data
    
    teams <- unique(c(footy_tab[ , unique(HomeTeam)], footy_tab[ , unique(AwayTeam)]))
    
    home_team_stats <- lapply(teams, function(x) {
      
        footy_tab[HomeTeam==x, list(Team                 = x,
                                    Played1              = .N, 
                                    Points1              = sum(ifelse(FTR == 'H', 1L, 0L)) * 3 + sum(ifelse(FTR == 'D', 1L, 0L)),
                                    `Home Wins`          = sum(ifelse(FTR == 'H', 1L, 0L)), 
                                    `Home Draws`         = sum(ifelse(FTR == 'D', 1L, 0L)),
                                    `Home Losses`        = sum(ifelse(FTR == 'A', 1L, 0L)),
                                    `Home Scored`  = sum(FTHG),
                                    `Home Conceded`= sum(FTAG)),
                  by='HomeTeam']
      
    })
    
    away_team_stats <- lapply(teams, function(x) {
      
      footy_tab[AwayTeam==x, list(Team                 = x,
                                  Played2              = .N, 
                                  Points2              = sum(ifelse(FTR == 'A', 1L, 0L)) * 3 + sum(ifelse(FTR == 'D', 1L, 0L)),
                                  `Away Wins`          = sum(ifelse(FTR == 'A', 1L, 0L)), 
                                  `Away Draws`         = sum(ifelse(FTR == 'D', 1L, 0L)),
                                  `Away Losses`        = sum(ifelse(FTR == 'H', 1L, 0L)),
                                  `Away Scored`  = as.integer(sum(FTAG)),
                                  `Away Conceded`= as.integer(sum(FTHG))),
                by='AwayTeam']
    })
    
    home_team_stats <- rbindlist(home_team_stats)
    away_team_stats <- rbindlist(away_team_stats)
    
    team_stats <- cbind(home_team_stats, away_team_stats)[, list(Team, 
                                                                 Played = Played1 + Played2, 
                                                                 `Home Wins`, `Home Draws`, `Home Losses`, `Home Scored`, `Home Conceded`, 
                                                                 `Away Wins`, `Away Draws`, `Away Losses`, `Away Scored`, `Away Conceded`,
                                                                 `Total Wins`    = `Home Wins`     + `Away Wins`,
                                                                 `Total Draws`   = `Home Draws`    + `Away Draws`,
                                                                 `Total Losses`  = `Home Losses`   + `Away Losses`,
                                                                 `Total Scored`  = `Home Scored`   + `Away Scored`,
                                                                 `Total Conceded`= `Home Conceded` + `Away Conceded`,
                                                                 `Goal Diff` = `Home Scored` + `Away Scored` - `Home Conceded` - `Away Conceded`, 
                                                                 Points      = as.integer(Points1 + Points2))][,
                                                                 total_scored := `Home Scored` + `Away Scored`]
    setorder(team_stats, -Points, -`Goal Diff`, -total_scored)
    team_stats[, Pos := as.character(.I)]
    team_stats[, total_scored := NULL]
    team_stats <- team_stats[, c('Pos', names(team_stats)[-length(names(team_stats))]), with = FALSE]

    names(team_stats) <- gsub('Home|Away|Total', '', names(team_stats))
    
    team_stats
    
    
  }, 
  include.rownames = FALSE, 
  add.to.row=list(
    pos=list(0, 0), 
    command=c("<TR> <TH colspan=3>  </TH> 
                    <TH colspan=5 style = 'text-align:center'> Home </TH>
                    <TH colspan=5 style = 'text-align:center'> Away </TH>
                    <TH colspan=5 style = 'text-align:center'> Total </TH>
                    <TH colspan=2> </TH>
              </TR>" ,
              paste("<TR>", paste(lapply(c('Pos','Team','Played',
                                           'Wins','Draws','Losses','For','Against',
                                           'Wins','Draws','Losses','For','Against',
                                           'Wins','Draws','Losses','For','Against',
                                           'Goal Diff','Points'), 
                                         function(x) paste('<TH>', x, '</TH>')), collapse=''), '</TR>'))
                  ),
  include.colnames=FALSE)
  
  output$goals_per_team <- renderHighchart({
    
    goals_final <- values$aggr_data
    highchart() %>%
      hc_title(text = "Shots and Goals per Team") %>%
      hc_xAxis(categories = goals_final$Team) %>%
      hc_yAxis(
        list(
          title = list(text = "Goals / Shots / Shots on Target"),
          align = "left",
          showFirstLabel = FALSE,
          showLastLabel = FALSE
        ),
        list(
          title = list(text = "Average Shots needed to Score First Goal"),
          align = "right",
          showFirstLabel = FALSE,
          showLastLabel = FALSE,
          opposite = TRUE
        )
      ) %>%
      hc_add_series(name  = 'Total Shots',     type = 'column', data=goals_final$`Total Shots`) %>%
      hc_add_series(name  = 'Shots On Target', type = 'column', data=goals_final$`On Target Shots`) %>%
      hc_add_series(name  = 'Goals',           type = 'column', data=goals_final$Goals) %>%
      hc_add_series(name  = 'Average Shots to Score', type = 'spline', 
                    data  = ceiling(goals_final$`Shots per Goal`),
                    yAxis = 1) %>%
      hc_tooltip(crosshairs = TRUE, shared=TRUE)
          
  })

  output$goals_over_time <- renderHighchart({
    
    footy_tab          <- values$data
    team_individual    <- footy_tab[HomeTeam == input$team | AwayTeam == input$team, 
                                    list(Date, HomeTeam, AwayTeam, FTHG, FTAG, HS, AS, HST, AST, Score = paste(FTHG, FTAG, sep='-'))]
    team_individual_m  <- melt(team_individual, measure.vars=c('HomeTeam', 'AwayTeam'))
    team_individual_m2  <- team_individual_m[value == input$team,]
    setorder(team_individual_m2, Date)
    team_individual_m3  <- team_individual_m[value != input$team,]
    setorder(team_individual_m3, Date)
    if(input$HA == 'Home'){
      team_individual_m2 <- 
        team_individual_m2[variable == 'HomeTeam', list(Date, Goals = FTHG, `Total Shots` = HS, `On Target Shots` = HST, Score)]
      team_individual_m3 <-
        team_individual_m3[variable == 'AwayTeam', list(Date, value)]
    }else if(input$HA == 'Away'){
      team_individual_m2 <- 
        team_individual_m2[variable == 'AwayTeam', list(Date, Goals = FTAG, `Total Shots` = AS, `On Target Shots` = AST, Score)]
      team_individual_m3 <-
        team_individual_m3[variable == 'HomeTeam', list(Date, value)]
    }else{
      team_individual_m2 <- 
        team_individual_m2[, Goals             := ifelse(variable == 'HomeTeam', FTHG, FTAG)][, 
                             `Total Shots`     := ifelse(variable == 'HomeTeam',   HS,   AS)][,
                             `On Target Shots` := ifelse(variable == 'HomeTeam',  HST,  AST)][, 
                             Score := Score]
    }
    
    highchart() %>%
      hc_title(text = input$team) %>%
      hc_xAxis(list(categories = paste(team_individual_m3$value, format(team_individual_m3$Date, '%d/%m'), sep=' ')),
               list(categories = team_individual_m2$Score, opposite=TRUE)) %>%
      hc_add_series(name  = 'Total Shots'    , type = 'spline', data=team_individual_m2$`Total Shots`) %>%
      hc_add_series(name  = 'On Target Shots', type = 'spline', data=team_individual_m2$`On Target Shots`) %>%
      hc_add_series(name  = 'Goals'          , type = 'spline', data=team_individual_m2$Goals, xAxis=1) %>%
      hc_tooltip(crosshairs = TRUE, shared=TRUE)
    
  })

  output$goals_total <- renderHighchart({ 
    
    goals_final <- values$aggr_data
    highchart() %>%
      hc_add_series(name = "Value", type = "funnel",
                    data = list(list(y = sum(goals_final$`Total Shots`), name = "Total Shots"),
                                list(y = sum(goals_final$`On Target Shots`), name = "Total Shots on Target"),
                                list(y = sum(goals_final$Goals), name = "Total Goals Scored")))
    
  })

  output$rate <- renderText({
    goals_final <- values$aggr_data
    ceiling(sum(goals_final$`Total Shots`) / sum(goals_final$Goals))
  })
  

  output$team_results <- renderHighchart({
    
    footy_tab  <- values$data
    HA   <- switch(input$results_ha, Home = 'HomeTeam', Away = 'AwayTeam', All = 'All')
    FTHT <- switch(input$FTHT,       `Full Time` = 'FTR', `Half Time` = 'HTR')
    if(HA %in% c('HomeTeam', 'AwayTeam')){
      footy_result            <- footy_tab[, .N, by=c(HA, FTHT)]
      footy_result_dcast      <- dcast(footy_result, as.formula(paste(HA, '~', FTHT)), value.var='N')
      if(HA == 'AwayTeam') setnames(footy_result_dcast, c('H','A'), c('A','H'))
      setnames(footy_result_dcast, HA, 'Team')
      footy_result_dcast <- na_converter(footy_result_dcast)
      footy_all_result        <- footy_result_dcast[, list(Team = Team, Wins = H, Draws = D, Losses = A)]
    }else{
      footy_home_result       <- footy_tab[, .N, by=c('HomeTeam', FTHT)]
      footy_home_result_dcast <- dcast(footy_home_result, as.formula(paste('HomeTeam ~', FTHT)), value.var='N')
      setnames(footy_home_result_dcast, 'HomeTeam', 'Team')
      footy_away_result_dcast <- na_converter(footy_home_result_dcast)
      footy_away_result       <- footy_tab[, .N, by=c('AwayTeam', FTHT)]
      footy_away_result_dcast <- dcast(footy_away_result, as.formula(paste('AwayTeam ~', FTHT)), value.var='N')
      setnames(footy_away_result_dcast, c('H','A'), c('A','H'))
      setnames(footy_away_result_dcast, 'AwayTeam', 'Team')
      footy_away_result_dcast <- na_converter(footy_away_result_dcast)
      footy_all_result        <- rbindlist(list(footy_home_result_dcast, footy_away_result_dcast), use.names=TRUE)
      footy_all_result        <- footy_all_result[, list(Wins = sum(H), Draws = sum(D), Losses = sum(A)), by='Team']
    }

    highchart() %>%
      hc_xAxis(categories = footy_all_result$Team) %>%
      hc_add_series(name='Wins'  , type='column', data=footy_all_result$Wins)   %>%
      hc_add_series(name='Draws' , type='column', data=footy_all_result$Draws)  %>%
      hc_add_series(name='Losses', type='column', data=footy_all_result$Losses) %>%
      hc_colors(c('#90ed7d', '#cccccc', '#ff3333')) %>%
      hc_tooltip(crosshairs = TRUE, shared=TRUE)
    
  })


  output$result_over_time <- renderHighchart({
    
    footy_tab <- values$data
    FTHT <- switch(input$result_FTHT, `Full Time` = 'FTR', `Half Time` = 'HTR' )
    team_individual    <- footy_tab[HomeTeam == input$result_team | AwayTeam == input$result_team, 
                                    c('Date', 'HomeTeam', 'AwayTeam', FTHT), with=FALSE]
    setnames(team_individual, FTHT, 'Result')
    team_individual_m  <- melt(team_individual, measure.vars=c('HomeTeam', 'AwayTeam'))
    team_individual_m2  <- team_individual_m[value == input$result_team,]
    setorder(team_individual_m2, Date)
    team_individual_m3  <- team_individual_m[value != input$result_team,]
    setorder(team_individual_m3, Date)
    if(input$result_HA == 'Home'){
      team_individual_m2 <- 
        team_individual_m2[variable == 'HomeTeam', list(Date, Result)]
      team_individual_m2[Result == 'H', Result := 'W'][Result == 'A', Result := 'L']
      team_individual_m3 <-
        team_individual_m3[variable == 'AwayTeam', list(Date, value)]
    }else if(input$result_HA == 'Away'){
      team_individual_m2 <- 
        team_individual_m2[variable == 'AwayTeam', list(Date, Result)]
      team_individual_m2[Result == 'H', Result := 'L'][Result == 'A', Result := 'W']
      team_individual_m3 <-
        team_individual_m3[variable == 'HomeTeam', list(Date, value)]
    }else{
      team_individual_m2[variable=='HomeTeam' & Result == 'H', Result := 'W'][variable=='HomeTeam' & Result == 'A', Result := 'L']
      team_individual_m2[variable=='AwayTeam' & Result == 'H', Result := 'L'][variable=='AwayTeam' & Result == 'A', Result := 'W']
      
    }
    
    jav_func <- JS('function() {
                       if(this.y == 2) {
                          return this.series.name + ": Draw";
                       }else if(this.y == 1) {
                          return this.series.name + ": Loss";
                       }else{
                          return this.series.name + ": Win";
                       }
                   }')
    
    highchart() %>%
      hc_title(text = input$result_team) %>%
      hc_yAxis(title = list(text = "Result"),
               gridLineWidth  = 0,
               showFirstLabel = FALSE,
               showLastLabel  = FALSE,
               #keeping this for now as an example of how to use the formatter. useHTML must be enabled
               labels         = list(enabled=FALSE, useHTML=TRUE, formatter=JS('function() {return this.value;}')),
               plotBands = list(
                 list(from = 0.5, to = 1.55, color = "#ffcccc", label = list(text = "Losses", y =  40, style    = list(`font-size`='20px'))),
                 list(from = 1.55, to = 2.45, color = "#e6e6e6", label = list(text = "Draws", rotation = 90, 
                                                                            y = -30, zIndex=20, style=list(`font-size`='20px'))),
                 list(from = 2.45, to = 3.5, color = "#d8f9d2", label = list(text = "Wins"  , y = -40, style    = list(`font-size`='20px'))))) %>% 
      hc_xAxis(categories = paste(team_individual_m3$value, format(team_individual_m3$Date, '%d/%m'), sep=' ')) %>%
      hc_add_series(name  = 'Result', type = 'spline', data=as.numeric(factor(team_individual_m2$Result, levels=c('L','D','W')))) %>%
      hc_tooltip(crosshairs = TRUE, useHTML=TRUE, formatter = jav_func) %>%
      hc_legend(enabled=FALSE)
        
  })


  output$total_results_FT <- renderHighchart ({
  
    footy_tab <- values$data
    if(input$results_percent == 'Absolute') {
      highchart() %>%
        hc_title(text = 'Full Time Wins and Draws') %>%
        hc_yAxis(title = list(text = '')) %>%
        hc_xAxis(categories = c('Home Wins', 'Draws', 'Away Wins')) %>%
        hc_add_series(name = 'Value', type = 'column', data = footy_tab[, .N, keyby='FTR'][c('H','D','A'),][, N], colorByPoint=TRUE) %>%
        hc_colors(c('#90ed7d', '#cccccc', '#ff3333')) %>%
        hc_legend(enabled=FALSE)
    } else {
      highchart() %>%
        hc_title(text = 'Full Time Wins and Draws') %>%
        hc_yAxis(title  = list(text = ''),
                 labels = list(useHTML=TRUE, formatter=JS('function() {return this.value + "%" ;}'))) %>%
        hc_xAxis(categories = c('Home Wins', 'Draws', 'Away Wins')) %>%
        hc_add_series(name = 'Value', type = 'column', data = footy_tab[, .N, keyby='FTR'][c('H','D','A'),][, N/sum(N)*100], colorByPoint=TRUE) %>%
        hc_colors(c('#90ed7d', '#cccccc', '#ff3333')) %>%
        hc_legend(enabled=FALSE) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>:{point.y:.2f}%')
    }
  
  })

  output$total_results_HT <- renderHighchart ({
    
    footy_tab <- values$data
    if(input$results_percent == 'Absolute') {
    highchart() %>%
      hc_title(text = 'Half Time Wins and Draws') %>%
      hc_yAxis(title = list(text = '')) %>%
      hc_xAxis(categories = c('Home Wins', 'Draws', 'Away Wins')) %>%
      hc_add_series(name = 'Value', type = 'column', data = footy_tab[, .N, keyby='HTR'][c('H','D','A'),][,N], colorByPoint=TRUE) %>%
      hc_colors(c('#90ed7d', '#cccccc', '#ff3333')) %>%
      hc_legend(enabled=FALSE)
    } else {
      highchart() %>%
        hc_title(text = 'Half Time Wins and Draws') %>%
        hc_yAxis(title = list(text = ''),
                 labels = list(useHTML=TRUE, formatter=JS('function() {return this.value + "%" ;}'))) %>%
        hc_xAxis(categories = c('Home Wins', 'Draws', 'Away Wins')) %>%
        hc_add_series(name = 'Value', type = 'column', data = footy_tab[, .N, keyby='HTR'][c('H','D','A'),][, N/sum(N)*100], colorByPoint=TRUE) %>%
        hc_colors(c('#90ed7d', '#cccccc', '#ff3333')) %>%
        hc_legend(enabled=FALSE) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>:{point.y:.2f}%')
    }
    
  })

  output$team_OU <- renderHighchart ({
  
    footy_tab  <- isolate(values$data)
    HA   <- switch(input$ou_ha, Home = 'HomeTeam', Away = 'AwayTeam', All = 'All')
    FTHT <- switch(input$ou_FTHT,       `Full Time` = 'FT', `Half Time` = 'HT')
    if(HA %in% c('HomeTeam', 'AwayTeam')){
      if(FTHT == 'FT'){
        footy_tab[, total_goals := FTHG+FTAG]
        footy_tab[,`Over/Under 0.5` := ifelse(total_goals<0.5, 'Under 0.5', 'Over 0.5')][,
                   `Over/Under 1.5` := ifelse(total_goals<1.5, 'Under 1.5', 'Over 1.5')][,
                   `Over/Under 2.5` := ifelse(total_goals<2.5, 'Under 2.5', 'Over 2.5')][,
                   `Over/Under 3.5` := ifelse(total_goals<3.5, 'Under 3.5', 'Over 3.5')][,
                   `Over/Under 4.5` := ifelse(total_goals<4.5, 'Under 4.5', 'Over 4.5')][,
                   `Over/Under 5.5` := ifelse(total_goals<5.5, 'Under 5.5', 'Over 5.5')][,
                   `Over/Under 6.5` := ifelse(total_goals<6.5, 'Under 6.5', 'Over 6.5')][,
                   `Over/Under 7.5` := ifelse(total_goals<7.5, 'Under 7.5', 'Over 7.5')]
      }else if(FTHT == 'HT'){
        footy_tab[, total_goals := HTHG+HTAG]
        footy_tab[,`Over/Under 0.5` := ifelse(total_goals<0.5, 'Under 0.5', 'Over 0.5')][,
                   `Over/Under 1.5` := ifelse(total_goals<1.5, 'Under 1.5', 'Over 1.5')][,
                   `Over/Under 2.5` := ifelse(total_goals<2.5, 'Under 2.5', 'Over 2.5')][,
                   `Over/Under 3.5` := ifelse(total_goals<3.5, 'Under 3.5', 'Over 3.5')][,
                   `Over/Under 4.5` := ifelse(total_goals<4.5, 'Under 4.5', 'Over 4.5')][,
                   `Over/Under 5.5` := ifelse(total_goals<5.5, 'Under 5.5', 'Over 5.5')][,
                   `Over/Under 6.5` := ifelse(total_goals<6.5, 'Under 6.5', 'Over 6.5')][,
                   `Over/Under 7.5` := ifelse(total_goals<7.5, 'Under 7.5', 'Over 7.5')]
      }
      #print(footy_tab)
      footy_result       <- footy_tab[, .N, by=c(HA, input$OU)]
      footy_result_dcast <- dcast(footy_result, as.formula(paste0(HA, '~ `', input$OU, '`')), value.var='N')
      setnames(footy_result_dcast, HA, 'Team')
      footy_result_dcast <- na_converter(footy_result_dcast)
      footy_all_result   <- footy_result_dcast
    }else{
      if(FTHT == 'FT'){
        footy_tab[, total_goals := FTHG+FTAG]
        footy_tab[,`Over/Under 0.5` := ifelse(total_goals<0.5, 'Under 0.5', 'Over 0.5')][,
                   `Over/Under 1.5` := ifelse(total_goals<1.5, 'Under 1.5', 'Over 1.5')][,
                   `Over/Under 2.5` := ifelse(total_goals<2.5, 'Under 2.5', 'Over 2.5')][,
                   `Over/Under 3.5` := ifelse(total_goals<3.5, 'Under 3.5', 'Over 3.5')][,
                   `Over/Under 4.5` := ifelse(total_goals<4.5, 'Under 4.5', 'Over 4.5')][,
                   `Over/Under 5.5` := ifelse(total_goals<5.5, 'Under 5.5', 'Over 5.5')][,
                   `Over/Under 6.5` := ifelse(total_goals<6.5, 'Under 6.5', 'Over 6.5')][,
                   `Over/Under 7.5` := ifelse(total_goals<7.5, 'Under 7.5', 'Over 7.5')]
      }else if(FTHT == 'HT'){
        footy_tab[, total_goals := FTHG+FTAG]
        footy_tab[,`Over/Under 0.5` := ifelse(total_goals<0.5, 'Under 0.5', 'Over 0.5')][,
                   `Over/Under 1.5` := ifelse(total_goals<1.5, 'Under 1.5', 'Over 1.5')][,
                   `Over/Under 2.5` := ifelse(total_goals<2.5, 'Under 2.5', 'Over 2.5')][,
                   `Over/Under 3.5` := ifelse(total_goals<3.5, 'Under 3.5', 'Over 3.5')][,
                   `Over/Under 4.5` := ifelse(total_goals<4.5, 'Under 4.5', 'Over 4.5')][,
                   `Over/Under 5.5` := ifelse(total_goals<5.5, 'Under 5.5', 'Over 5.5')][,
                   `Over/Under 6.5` := ifelse(total_goals<6.5, 'Under 6.5', 'Over 6.5')][,
                   `Over/Under 7.5` := ifelse(total_goals<7.5, 'Under 7.5', 'Over 7.5')]
      }
      footy_home_result       <- footy_tab[, .N, by=c('HomeTeam', input$OU)]
      footy_home_result_dcast <- dcast(footy_home_result, as.formula(paste0('HomeTeam ~ `', input$OU, '`')), value.var='N')
      setnames(footy_home_result_dcast, 'HomeTeam', 'Team')
      footy_home_result_dcast <- na_converter(footy_home_result_dcast)
      
      footy_away_result       <- footy_tab[, .N, by=c('AwayTeam', input$OU)]
      footy_away_result_dcast <- dcast(footy_away_result, as.formula(paste0('AwayTeam ~ `', input$OU, '`')), value.var='N')
      setnames(footy_away_result_dcast, 'AwayTeam', 'Team')
      footy_away_result_dcast <- na_converter(footy_away_result_dcast)
      
      footy_all_result        <- rbindlist(list(footy_home_result_dcast, footy_away_result_dcast), use.names=TRUE)
      footy_all_result        <- footy_all_result[, lapply(.SD, sum), by='Team']
    }
    
    Over  <- names(footy_all_result)[like(names(footy_all_result), 'Over*')]
    Under <- names(footy_all_result)[like(names(footy_all_result), 'Under*')]
    highchart() %>%
      hc_xAxis(categories=footy_all_result[, Team]) %>%
      hc_add_series(name = Over , type = 'column', data = unname(unlist(footy_all_result[, Over , with=FALSE]))) %>%
      hc_add_series(name = Under, type = 'column', data = unname(unlist(footy_all_result[, Under, with=FALSE]))) %>%
      hc_tooltip(shared=TRUE, crosshairs=TRUE)
      
    
  })

  
  output$OU_over_time <- renderHighchart({
  
    footy_tab <- values$data
    FTHT <- switch(input$OUFTHT_over_time, `Full Time` = 'FT', `Half Time` = 'HT' )
    team_individual    <- footy_tab[HomeTeam == input$OU_team | AwayTeam == input$OU_team, 
                                    c('Date', 'HomeTeam', 'AwayTeam', 'FTHG','FTAG', 'HTHG','HTAG'), 
                                    with=FALSE]
    if(FTHT=='FT'){
      team_individual[, total_goals := FTHG+FTAG]
    }else{
      team_individual[, total_goals := HTHG+HTAG]
    }
    team_individual_m  <- melt(team_individual, measure.vars=c('HomeTeam', 'AwayTeam'))
    team_individual_m2 <- team_individual_m[value == input$OU_team,]
    setorder(team_individual_m2, Date)
    team_individual_m2[,`Over/Under 0.5` := ifelse(total_goals<0.5, 'Under 0.5', 'Over 0.5')][,
                        `Over/Under 1.5` := ifelse(total_goals<1.5, 'Under 1.5', 'Over 1.5')][,
                        `Over/Under 2.5` := ifelse(total_goals<2.5, 'Under 2.5', 'Over 2.5')][,
                        `Over/Under 3.5` := ifelse(total_goals<3.5, 'Under 3.5', 'Over 3.5')][,
                        `Over/Under 4.5` := ifelse(total_goals<4.5, 'Under 4.5', 'Over 4.5')][,
                        `Over/Under 5.5` := ifelse(total_goals<5.5, 'Under 5.5', 'Over 5.5')][,
                        `Over/Under 6.5` := ifelse(total_goals<6.5, 'Under 6.5', 'Over 6.5')][,
                        `Over/Under 7.5` := ifelse(total_goals<7.5, 'Under 7.5', 'Over 7.5')]
    team_individual_m3  <- team_individual_m[value != input$OU_team,]
    setorder(team_individual_m3, Date)
    if(input$OU_overtime_HA == 'Home'){
      team_individual_m2 <- 
        team_individual_m2[variable == 'HomeTeam', c('Date', input$OU_overtime), with=FALSE]
      team_individual_m3 <-
        team_individual_m3[variable == 'AwayTeam', list(Date, value)]
    }
    if(input$OU_overtime_HA == 'Away'){
      team_individual_m2 <- 
        team_individual_m2[variable == 'AwayTeam', c('Date', input$OU_overtime), with=FALSE]
      team_individual_m3 <-
        team_individual_m3[variable == 'HomeTeam', list(Date, value)]
    }
    
    over  <- paste0('Over' , gsub("[[:alpha:]]|/", '', input$OU_overtime))
    under <- paste0('Under', gsub("[[:alpha:]]|/", '', input$OU_overtime))
    
    jav_func <- JS(paste0(
      'function() {
                   if(this.y == 1) {
                   return this.series.name + ":',  under, '";
                   }else if(this.y == 2) {
                   return this.series.name + ":',  over, '";
                   }
                   }')
    )
  
    highchart() %>%
      hc_title(text = input$OU_overtime) %>%
      hc_yAxis(title = list(text = 'Result'),
               gridLineWidth  = 0,
               showFirstLabel = FALSE,
               showLastLabel  = FALSE,
               labels         = list(enabled=FALSE, useHTML=TRUE),
               plotBands = list(
                 list(from = 0.5, to = 1.5, color = "#ffcccc", label = list(text = under, y =  80, style = list(`font-size`='20px'))),
                 list(from = 1.5, to = 2.5, color = "#e6e6e6", label = list(text = over , y = -70, style = list(`font-size`='20px'))))) %>% 
      hc_xAxis(categories = paste(team_individual_m3$value, format(team_individual_m3$Date, '%d/%m'), sep=' ')) %>%
      hc_add_series(name  = 'Result', type = 'spline', data=as.numeric(factor(team_individual_m2[[input$OU_overtime]], levels=c(under, over)))) %>%
      hc_tooltip(crosshairs = TRUE, useHTML=TRUE, formatter = jav_func) %>%
      hc_legend(enabled=FALSE)
  
  })
  
  output$OU_05 <- renderHighchart({
    
    footy_tab <- values$data
    FTHT <- switch(input$OUFTHT_overall, `Full Time` = 'FT', `Half Time` = 'HT' )
    if(FTHT=='FT'){
      footy_tab[, total_goals := FTHG+FTAG]
    }else{
      footy_tab[, total_goals := HTHG+HTAG]
    }
    
    footy_tab[, `Over/Under 0.5` := ifelse(total_goals<0.5, 'Under 0.5', 'Over 0.5')]
    
    mytab <- footy_tab[, .N, by=c('Over/Under 0.5')]
    setorder(mytab, 'Over/Under 0.5')
    highchart() %>%
      hc_title(text = 'Over/Under 0.5') %>%
      hc_xAxis(categories = mytab[, `Over/Under 0.5`]) %>%
      hc_add_series(name = 'Matches', type = 'column', data = mytab[, N], colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)
    
  })  

  output$OU_15 <- renderHighchart({
    
    footy_tab <- values$data
    FTHT <- switch(input$OUFTHT_overall, `Full Time` = 'FT', `Half Time` = 'HT' )
    if(FTHT=='FT'){
      footy_tab[, total_goals := FTHG+FTAG]
    }else{
      footy_tab[, total_goals := HTHG+HTAG]
    }
    
    footy_tab[, `Over/Under 1.5` := ifelse(total_goals<1.5, 'Under 1.5', 'Over 1.5')]
    
    mytab <- footy_tab[, .N, by=c('Over/Under 1.5')]
    setorder(mytab, 'Over/Under 1.5')
    
    highchart() %>%
      hc_title(text = 'Over/Under 1.5') %>%
      hc_xAxis(categories = mytab[, `Over/Under 1.5`]) %>%
      hc_add_series(name = 'Matches', type = 'column', data = mytab[, N], colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)
    
  })  

  output$OU_25 <- renderHighchart({
    
    footy_tab <- values$data
    FTHT <- switch(input$OUFTHT_overall, `Full Time` = 'FT', `Half Time` = 'HT' )
    if(FTHT=='FT'){
      footy_tab[, total_goals := FTHG+FTAG]
    }else{
      footy_tab[, total_goals := HTHG+HTAG]
    }
    
    footy_tab[, `Over/Under 2.5` := ifelse(total_goals<2.5, 'Under 2.5', 'Over 2.5')]
    
    mytab <- footy_tab[, .N, by=c('Over/Under 2.5')]
    setorder(mytab, 'Over/Under 2.5')
    
    highchart() %>%
      hc_title(text = 'Over/Under 2.5') %>%
      hc_xAxis(categories = mytab[, `Over/Under 2.5`]) %>%
      hc_add_series(name = 'Matches', type = 'column', data = mytab[, N], colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)
    
  })  

  output$OU_35 <- renderHighchart({
    
    footy_tab <- values$data
    FTHT <- switch(input$OUFTHT_overall, `Full Time` = 'FT', `Half Time` = 'HT' )
    if(FTHT=='FT'){
      footy_tab[, total_goals := FTHG+FTAG]
    }else{
      footy_tab[, total_goals := HTHG+HTAG]
    }
    
    footy_tab[, `Over/Under 3.5` := ifelse(total_goals<3.5, 'Under 3.5', 'Over 3.5')]
    
    mytab <- footy_tab[, .N, by=c('Over/Under 3.5')]
    setorder(mytab, 'Over/Under 3.5')
    
    highchart() %>%
      hc_title(text = 'Over/Under 3.5') %>%
      hc_xAxis(categories = mytab[, `Over/Under 3.5`]) %>%
      hc_add_series(name = 'Matches', type = 'column', data = mytab[, N], colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)
    
  })  

  output$OU_45 <- renderHighchart({
    
    footy_tab <- values$data
    FTHT <- switch(input$OUFTHT_overall, `Full Time` = 'FT', `Half Time` = 'HT' )
    if(FTHT=='FT'){
      footy_tab[, total_goals := FTHG+FTAG]
    }else{
      footy_tab[, total_goals := HTHG+HTAG]
    }
    
    footy_tab[, `Over/Under 4.5` := ifelse(total_goals<4.5, 'Under 4.5', 'Over 4.5')]
    
    mytab <- footy_tab[, .N, by=c('Over/Under 4.5')]
    setorder(mytab, 'Over/Under 4.5')
    
    highchart() %>%
      hc_title(text = 'Over/Under 4.5') %>%
      hc_xAxis(categories = mytab[, `Over/Under 4.5`]) %>%
      hc_add_series(name = 'Matches', type = 'column', data = mytab[, N], colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)
    
  })  

  output$OU_55 <- renderHighchart({
    
    footy_tab <- values$data
    FTHT <- switch(input$OUFTHT_overall, `Full Time` = 'FT', `Half Time` = 'HT' )
    if(FTHT=='FT'){
      footy_tab[, total_goals := FTHG+FTAG]
    }else{
      footy_tab[, total_goals := HTHG+HTAG]
    }
    
    footy_tab[, `Over/Under 5.5` := ifelse(total_goals<5.5, 'Under 5.5', 'Over 5.5')]
    
    mytab <- footy_tab[, .N, by=c('Over/Under 5.5')]
    setorder(mytab, 'Over/Under 5.5')
    
    highchart() %>%
      hc_title(text = 'Over/Under 5.5') %>%
      hc_xAxis(categories = mytab[, `Over/Under 5.5`]) %>%
      hc_add_series(name = 'Matches', type = 'column', data = mytab[, N], colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)
    
  })  

  output$OU_65 <- renderHighchart({
    
    footy_tab <- values$data
    FTHT <- switch(input$OUFTHT_overall, `Full Time` = 'FT', `Half Time` = 'HT' )
    if(FTHT=='FT'){
      footy_tab[, total_goals := FTHG+FTAG]
    }else{
      footy_tab[, total_goals := HTHG+HTAG]
    }
    
    footy_tab[, `Over/Under 6.5` := ifelse(total_goals<6.5, 'Under 6.5', 'Over 6.5')]
    
    mytab <- footy_tab[, .N, by=c('Over/Under 6.5')]
    setorder(mytab, 'Over/Under 6.5')
    
    highchart() %>%
      hc_title(text = 'Over/Under 6.5') %>%
      hc_xAxis(categories = mytab[, `Over/Under 6.5`]) %>%
      hc_add_series(name = 'Matches', type = 'column', data = mytab[, N], colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)
    
  })  
  
  output$OU_75 <- renderHighchart({
    
    footy_tab <- values$data
    FTHT <- switch(input$OUFTHT_overall, `Full Time` = 'FT', `Half Time` = 'HT' )
    if(FTHT=='FT'){
      footy_tab[, total_goals := FTHG+FTAG]
    }else{
      footy_tab[, total_goals := HTHG+HTAG]
    }
    
    footy_tab[, `Over/Under 7.5` := ifelse(total_goals<7.5, 'Under 7.5', 'Over 7.5')]
    
    mytab <- footy_tab[, .N, by=c('Over/Under 7.5')]
    setorder(mytab, 'Over/Under 7.5')
    
    highchart() %>%
      hc_title(text = 'Over/Under 7.5') %>%
      hc_xAxis(categories = mytab[, `Over/Under 7.5`]) %>%
      hc_add_series(name = 'Matches', type = 'column', data = mytab[, N], colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)
    
  })  

  output$cards_fouls_team <- renderHighchart({
    
    footy_tab <- values$data
    footy_home_cards <- footy_tab[, list(`Yellow Cards`=sum(HY),`Red Cards`=sum(HR),`Fouls`=sum(HF),`No of Games`=.N),by='HomeTeam']
    setnames(footy_home_cards, 'HomeTeam', 'Team')
    footy_away_cards <- footy_tab[, list(`Yellow Cards`=sum(AY),`Red Cards`=sum(AR),`Fouls`=sum(AF),`No of Games`=.N),by='AwayTeam']
    setnames(footy_away_cards, 'AwayTeam', 'Team')
    footy_all_cards  <- rbindlist(list(footy_home_cards, footy_away_cards))
    footy_all_cards  <- footy_all_cards[, list(`Yellow Cards`= sum(`Yellow Cards`),
                                               `Red Cards`   = sum(`Red Cards`),
                                               `Fouls`       = sum(Fouls),
                                               `No of Games` = sum(`No of Games`)),
                                        by='Team']
    highchart() %>%
      hc_title(text = "Yellow and Red Cards per Team") %>%
      hc_xAxis(categories = footy_all_cards[, Team]) %>%
      hc_yAxis(
        list(
          title = list(text = "Cards"),
          align = "left",
          showFirstLabel = FALSE,
          showLastLabel = FALSE
        ),
        list(
          title = list(text = "Fouls"),
          align = "right",
          showFirstLabel = FALSE,
          showLastLabel = FALSE,
          opposite = TRUE
        )
      ) %>%
      hc_add_series(name  = 'Yellow Cards', type = 'column', data=footy_all_cards[, `Yellow Cards`])   %>%
      hc_add_series(name  = 'Red Cards'   , type = 'column', data=footy_all_cards[, `Red Cards`])      %>%
      hc_add_series(name  = 'Fouls'       , type = 'spline', data=footy_all_cards[, Fouls], yAxis=1) %>%
      hc_colors(c(hc_get_colors()[1], hc_get_colors()[2], hc_get_colors()[4])) %>%
      hc_tooltip(crosshairs = TRUE, shared=TRUE)
    
  })

  output$cards_over_time <- renderHighchart({
    
    footy_tab          <- values$data
    team_individual    <- footy_tab[HomeTeam == input$cards_team | AwayTeam == input$cards_team, 
                                    list(Date, HomeTeam, AwayTeam, HY, AY, HR, AR, HF, AF)]
    team_individual_m  <- melt(team_individual, measure.vars=c('HomeTeam', 'AwayTeam'))
    team_individual_m2 <- team_individual_m[value == input$cards_team,]
    setorder(team_individual_m2, Date)
    team_individual_m3 <- team_individual_m[value != input$cards_team,]
    setorder(team_individual_m3, Date)
    if(input$cards_HA == 'Home'){
      team_individual_m2 <- 
        team_individual_m2[variable == 'HomeTeam', list(Date, `Yellow Cards` = HY, `Red Cards` = HR, Fouls = HF)]
      team_individual_m3 <-
        team_individual_m3[variable == 'AwayTeam', list(Date, value)]
    }else if(input$cards_HA == 'Away'){
      team_individual_m2 <- 
        team_individual_m2[variable == 'AwayTeam', list(Date, `Yellow Cards` = AY, `Red Cards` = AR, Fouls = AF)]
      team_individual_m3 <-
        team_individual_m3[variable == 'HomeTeam', list(Date, value)]
    }else{
      team_individual_m2 <- 
        team_individual_m2[, `Yellow Cards` := ifelse(variable == 'HomeTeam', HY, AY)][, 
                             `Red Cards`    := ifelse(variable == 'HomeTeam', HR, AR)][,
                             Fouls          := ifelse(variable == 'HomeTeam', HF, AF)]
    }
    
    highchart() %>%
      hc_title(text = input$cards_team) %>%
      hc_xAxis(list(categories = paste(team_individual_m3$value, format(team_individual_m3$Date, '%d/%m'), sep=' '))) %>%
      hc_yAxis(
        list(
          title = list(text = "Cards"),
          align = "left",
          showFirstLabel = FALSE,
          showLastLabel = FALSE
        ),
        list(
          title = list(text = "Fouls"),
          align = "right",
          showFirstLabel = FALSE,
          showLastLabel = FALSE,
          opposite = TRUE
        )
      ) %>%
      hc_add_series(name  = 'Yellow Cards', type = 'column', data=team_individual_m2[,`Yellow Cards`]) %>%
      hc_add_series(name  = 'Red Cards'   , type = 'column', data=team_individual_m2[,`Red Cards`]) %>%
      hc_add_series(name  = 'Fouls'       , type = 'spline', data=team_individual_m2[,Fouls], yAxis=1) %>%
      hc_tooltip(crosshairs = TRUE, shared=TRUE)
    
  })

  output$referees <- renderHighchart({
    
    footy_tab <- values$data
    footy_tab[, `Yellow Cards` := HY + AY][, `Red Cards` := HR + AR]
    refs <- footy_tab[, list(`Yellow Cards` = sum(`Yellow Cards`), `Red Cards` = sum(`Red Cards`), `No of Games` = .N), by='Referee']
    setorder(refs, -`Yellow Cards`)
    
    if(input$abso_ave == 'Absolute'){
      highchart() %>%
        hc_title(text = 'Referees') %>%
        hc_xAxis(categories = refs[, Referee]) %>%
        hc_add_series(name = 'Yellow Cards', type = 'bar', data = refs[, `Yellow Cards`]) %>%
        hc_add_series(name = 'Red Cards'   , type = 'bar', data = refs[, `Red Cards`]) %>%
        hc_add_series(name = 'No of Games' , type = 'bar', data = refs[, `No of Games`]) %>%
        hc_tooltip(crosshairs = TRUE, shared=TRUE)
    }else{
      highchart() %>%
        hc_title(text = 'Referees') %>%
        hc_xAxis(categories = refs[, Referee]) %>%
        hc_add_series(name = 'Yellow Cards', type = 'bar', data = round(refs[, `Yellow Cards`] / refs[, `No of Games`], 2)) %>%
        hc_add_series(name = 'Red Cards'   , type = 'bar', data = round(refs[, `Red Cards`]    / refs[, `No of Games`], 2)) %>%
        hc_tooltip(crosshairs = TRUE, shared=TRUE) 
        
    }
    
  })  


  output$total_cards_fouls <- renderHighchart({
    
    footy_tab <- values$data
    footy_tab[, `Yellow Cards` := HY + AY][, `Red Cards` := HR + AR][, Fouls := HF + AF]
    all_cards <-  footy_tab[, list(`Yellow Cards` = sum(`Yellow Cards`), `Red Cards` = sum(`Red Cards`), Fouls = sum(Fouls))]
    no_of_games <- footy_tab[, .N]
                                                                                                                                                  
    all_cards <- as.data.table(t(all_cards), keep.rownames=TRUE)
    all_cards$V2 <- all_cards$V1/no_of_games
    
    
    highchart() %>%
      hc_title(text = 'Total Cards / Fouls') %>%
      hc_xAxis(categories = all_cards[, rn] ) %>%
      hc_add_series(name = 'Value', type = 'column', data = if(input$abso_ave=='Absolute') all_cards[, V1] else all_cards[, V2], 
                    colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)%>%
      hc_tooltip(followPointer = TRUE, pointFormat = '<span style="color:{series.color}">{series.name}</span>:{point.y:.2f}')
      
    
  })  


  output$corners_per_team <- renderHighchart({
    
    footy_tab  <- values$data
    if(input$abso_OU == 'Over/Under'){
      HA   <- switch(input$corner_ha, Home = 'HomeTeam', Away = 'AwayTeam', All = 'All')
      footy_tab[, total_corners := HC+AC]
      footy_tab[,`Corners O/U  5.5` := ifelse(total_corners< 5.5, 'Under  5.5', 'Over  5.5')][,
                 `Corners O/U  8.5` := ifelse(total_corners< 8.5, 'Under  8.5', 'Over  8.5')][,
                 `Corners O/U 10.5` := ifelse(total_corners<10.5, 'Under 10.5', 'Over 10.5')][,
                 `Corners O/U 13.5` := ifelse(total_corners<13.5, 'Under 13.5', 'Over 13.5')]
      if(HA %in% c('HomeTeam', 'AwayTeam')){
        footy_result       <- footy_tab[, .N, by=c(HA, input$cornerOU)]
        footy_result_dcast <- dcast(footy_result, as.formula(paste0(HA, '~ `', input$cornerOU, '`')), value.var='N')
        setnames(footy_result_dcast, HA, 'Team')
        footy_result_dcast <- na_converter(footy_result_dcast)
        footy_all_result   <- footy_result_dcast
      }else{
        footy_home_result       <- footy_tab[, .N, by=c('HomeTeam', input$cornerOU)]
        footy_home_result_dcast <- dcast(footy_home_result, as.formula(paste0('HomeTeam ~ `', input$cornerOU, '`')), value.var='N')
        setnames(footy_home_result_dcast, 'HomeTeam', 'Team')
        footy_home_result_dcast <- na_converter(footy_home_result_dcast)
        
        footy_away_result       <- footy_tab[, .N, by=c('AwayTeam', input$cornerOU)]
        footy_away_result_dcast <- dcast(footy_away_result, as.formula(paste0('AwayTeam ~ `', input$cornerOU, '`')), value.var='N')
        setnames(footy_away_result_dcast, 'AwayTeam', 'Team')
        footy_away_result_dcast <- na_converter(footy_away_result_dcast)
        
        footy_all_result        <- rbindlist(list(footy_home_result_dcast, footy_away_result_dcast), use.names=TRUE)
        footy_all_result        <- footy_all_result[, lapply(.SD, sum), by='Team']
      }
      
      Over  <- names(footy_all_result)[like(names(footy_all_result), 'Over*')]
      Under <- names(footy_all_result)[like(names(footy_all_result), 'Under*')]
      highchart() %>%
        hc_yAxis(list(title = list(text = "No of Games"))) %>%
        hc_xAxis(categories=footy_all_result[, Team]) %>%
        hc_add_series(name = Over , type = 'column', data = unname(unlist(footy_all_result[, Over , with=FALSE]))) %>%
        hc_add_series(name = Under, type = 'column', data = unname(unlist(footy_all_result[, Under, with=FALSE]))) %>%
        hc_tooltip(shared=TRUE, crosshairs=TRUE)
    }else{
      HA   <- switch(input$corner_ha, Home = 'HomeTeam', Away = 'AwayTeam', All = 'All')
      if(HA == 'HomeTeam'){
        footy_result       <- footy_tab[, list(Corners = sum(HC), `No of Games` = .N), by=HA]
        setnames(footy_result, HA, 'Team')  
      }else if(HA == 'AwayTeam'){
        footy_result       <- footy_tab[, list(Corners = sum(AC), `No of Games` = .N), by=HA]
        setnames(footy_result, HA, 'Team')
      }else{  
        footy_home_result       <- footy_tab[, list(Corners = sum(HC), `No of Games` = .N), by='HomeTeam']
        setnames(footy_home_result, HA, 'Team')  
        footy_away_result       <- footy_tab[, list(Corners = sum(AC), `No of Games` = .N), by='AwayTeam']
        setnames(footy_away_result, HA, 'Team')  
        
        footy_result <- rbindlist(list(footy_home_result, footy_away_result), use.names=TRUE)
        footy_result <- footy_result[, lapply(.SD, sum), by='Team']
      }
      
      if(input$abso_OU == 'Total') {
        highchart() %>%
          hc_yAxis(list(title = list(text = "Corners"))) %>%
          hc_xAxis(categories=footy_result[, Team])   %>%
          hc_add_series(name = 'Corners' , type = 'column', data = footy_result[, Corners]) %>%
          hc_tooltip(shared=TRUE, crosshairs=TRUE)
      }else{
        highchart() %>%
          hc_yAxis(list(title = list(text = "Corners"))) %>%
          hc_xAxis(categories=footy_result[, Team])   %>%
          hc_add_series(name = 'Corners' , type = 'column', data = round(footy_result[, Corners] / footy_result[, `No of Games`], 2) ) %>%
          hc_tooltip(shared=TRUE, crosshairs=TRUE)
      }
      
    }
    
  })
  
  output$corners_over_time <- renderHighchart({
    
    footy_tab          <- values$data
    team_individual    <- footy_tab[HomeTeam == input$corner_team | AwayTeam == input$corner_team, list(Date, HomeTeam, AwayTeam, HC, AC)]
    team_individual_m  <- melt(team_individual, measure.vars=c('HomeTeam', 'AwayTeam'))
    team_individual_m2  <- team_individual_m[value == input$corner_team,]
    setorder(team_individual_m2, Date)
    team_individual_m3  <- team_individual_m[value != input$corner_team,]
    setorder(team_individual_m3, Date)
    if(input$corner_HA_overtime == 'Home'){
      team_individual_m2 <- 
        team_individual_m2[variable == 'HomeTeam', list(Date, Corners = HC)]
      team_individual_m3 <-
        team_individual_m3[variable == 'AwayTeam', list(Date, value)]
    }else if(input$corner_HA_overtime == 'Away'){
      team_individual_m2 <- 
        team_individual_m2[variable == 'AwayTeam', list(Date, Corners = AC)]
      team_individual_m3 <-
        team_individual_m3[variable == 'HomeTeam', list(Date, value)]
    }else{
      team_individual_m2 <- 
        team_individual_m2[, Corners := ifelse(variable == 'HomeTeam', HC, AC)] 
    }
    
    highchart() %>%
      hc_title(text = input$corner_team) %>%
      hc_xAxis(list(categories = paste(team_individual_m3$value, format(team_individual_m3$Date, '%d/%m'), sep=' '))) %>%
      hc_add_series(name  = 'Corners', type = 'spline', data=team_individual_m2$Corners) %>%
      hc_tooltip(crosshairs = TRUE, shared=TRUE)
    
  })

  output$corners_55 <- renderHighchart({
    
    footy_tab <- values$data
    footy_tab[, total_corners := HC+AC]
    
    footy_tab[, `Corners O/U 5.5` := ifelse(total_corners<5.5, 'Under 5.5', 'Over 5.5')]
    
    mytab <- footy_tab[, .N, by=c('Corners O/U 5.5')]
    setorder(mytab, 'Corners O/U 5.5')
    
    highchart() %>%
      hc_title(text = 'Corners O/U 5.5') %>%
      hc_xAxis(categories = mytab[, `Corners O/U 5.5`]) %>%
      hc_add_series(name = 'Matches', type = 'column', data = mytab[, N], colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)
    
  }) 

  output$corners_85 <- renderHighchart({
    
    footy_tab <- values$data
    footy_tab[, total_corners := HC+AC]
    
    footy_tab[, `Corners O/U 8.5` := ifelse(total_corners<8.5, 'Under 8.5', 'Over 8.5')]
    
    mytab <- footy_tab[, .N, by=c('Corners O/U 8.5')]
    setorder(mytab, 'Corners O/U 8.5')
    
    highchart() %>%
      hc_title(text = 'Corners O/U 8.5') %>%
      hc_xAxis(categories = mytab[, `Corners O/U 8.5`]) %>%
      hc_add_series(name = 'Matches', type = 'column', data = mytab[, N], colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)
    
  }) 

  output$corners_105 <- renderHighchart({
    
    footy_tab <- values$data
    footy_tab[, total_corners := HC+AC]
    
    footy_tab[, `Corners O/U 10.5` := ifelse(total_corners<10.5, 'Under 10.5', 'Over 10.5')]
    
    mytab <- footy_tab[, .N, by=c('Corners O/U 10.5')]
    setorder(mytab, 'Corners O/U 10.5')
    
    highchart() %>%
      hc_title(text = 'Corners O/U 10.5') %>%
      hc_xAxis(categories = mytab[, `Corners O/U 10.5`]) %>%
      hc_add_series(name = 'Matches', type = 'column', data = mytab[, N], colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)
    
  }) 

  output$corners_135 <- renderHighchart({
    
    footy_tab <- values$data
    footy_tab[, total_corners := HC+AC]
    
    footy_tab[, `Corners O/U 13.5` := ifelse(total_corners<13.5, 'Under 13.5', 'Over 13.5')]
    
    mytab <- footy_tab[, .N, by=c('Corners O/U 13.5')]
    setorder(mytab, 'Corners O/U 13.5')
    
    highchart() %>%
      hc_title(text = 'Corners O/U 13.5') %>%
      hc_xAxis(categories = mytab[, `Corners O/U 13.5`]) %>%
      hc_add_series(name = 'Matches', type = 'column', data = mytab[, N], colorByPoint=TRUE) %>%
      hc_legend(enabled = FALSE)
    
  }) 


#css for DataTables vertical lines
#http://stackoverflow.com/questions/3313456/css-borders-between-table-columns-only
#http://www.w3schools.com/css/css_pseudo_classes.asp
#table td:nth-child(3) {
#border-left: none;
#}

})