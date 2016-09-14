shinyServer(function(input, output, session) { 

  #declare reactive values------------------------------------------------------
  values <- reactiveValues()
  
  #an observe function that updates sidebar selectInputs------------------------
  observe({

    #update division when selecting country
    updateSelectInput(
      session, 
      'division', 
      choices = switch(input$country,
                       Greece      = 'Superleague',
                       England     = c('Premier League',
                                       'Championship', 
                                       'League 1', 
                                       'League 2', 
                                       'Conference'),
                       Scotland    = c('Premier  League', 
                                       'Division 1', 
                                       'Division 2', 
                                       'Division 3'),
                       Germany     = c('Bundesliga 1', 
                                       'Bundesliga 2'),
                       Italy       = c('Serie A', 
                                       'Serie B'),
                       Spain       = c('Primera Division', 
                                       'Segunda Division'),
                       France      = c('Ligue 1', 
                                       'Ligue 2'),
                       Netherlands = 'Eredivisie',
                       Belgium     = 'Jupiler League',
                       Portugal    = 'Liga 1',
                       Turkey      = 'Ligi 1')
    )

    #update years when selecting country
    updateSelectInput(
      session, 
      'season', 
      choices = switch(input$country,
                       Greece      = rev(paste(1994:2017, 
                                               shift(1994:2017, type = 'lead'),
                                               sep = '-'))[-1],
                       England     = rev(paste(1993:2017,
                                               shift(1993:2017, type = 'lead'),
                                               sep = '-'))[-1],
                       Scotland    = rev(paste(1994:2017,
                                               shift(1994:2017, type = 'lead'),
                                               sep = '-'))[-1],
                       Germany     = rev(paste(1993:2017,
                                               shift(1993:2017, type = 'lead'),
                                               sep='-'))[-1],
                       Italy       = rev(paste(1993:2017,
                                               shift(1993:2017, type = 'lead'),
                                               sep='-'))[-1],
                       Spain       = rev(paste(1993:2017,
                                               shift(1993:2017, type = 'lead'),
                                               sep = '-'))[-1],
                       France      = rev(paste(1993:2017,
                                               shift(1993:2017, type = 'lead'),
                                               sep = '-'))[-1],
                       Netherlands = rev(paste(1993:2017,
                                               shift(1993:2017, type = 'lead'),
                                               sep = '-'))[-1],
                       Belgium     = rev(paste(1995:2017,
                                               shift(1995:2017, type = 'lead'),
                                               sep = '-'))[-1],
                       Portugal    = rev(paste(1994:2017,
                                               shift(1994:2017, type = 'lead'),
                                               sep = '-'))[-1],
                       Turkey      = rev(paste(1994:2017,
                                               shift(1994:2017, type = 'lead'),
                                               sep = '-'))[-1]
                )
    )

    #pass country to global reactive values
    values$country <- input$country

  })

  #reactive function that downloads the dataset from---------------------------- 
  #football-data.co.uk----------------------------------------------------------
  dataInput <- reactive({

    #traslate country to code
    country_code <- switch(isolate(values$country),
                           Greece      = 'G',
                           England     = 'E',
                           Scotland    = 'SC',
                           Germany     = 'D',
                           Italy       = 'I',
                           Spain       = 'SP',
                           France      = 'F',
                           Netherlands = 'N',
                           Belgium     = 'B',
                           Portugal    = 'P',
                           Turkey      = 'T'
    )

    #translate division to code
    division_code <- switch(input$division,
                            `Premier League`    = '0',
                            Championship        = '1',
                            `League 1`          = '2',
                            `League 2`          = '3',
                            Conference          = 'C',
                            `Premier  League`   = '0',
                            `Division 1`        = '1',
                            `Division 2`        = '2',
                            `Division 3`        = '3',
                            `Bundesliga 1`      = '1',
                            `Bundesliga 2`      = '2',
                            `Serie A`           = '1',
                            `Serie B`           = '2',
                            `Primera Division`  = '1',
                            `Segunda Division`  = '2',
                            `Ligue 1`           = '1',
                            `Ligue 2`           = '2',
                            Eredivisie          = '1',
                            `Jupiler League`    = '1',
                            `Liga 1`            = '1',
                            `Ligi 1`            = '1',
                            Superleague         = '1'
    )

    #create season code
    season_code <- paste0(substr(input$season, 3, 4), 
                          substr(input$season, 8, 9))
    
    #create url and read csv
    footy_set <- read.csv(paste0('http://www.football-data.co.uk/mmz4281/', 
                                 season_code, 
                                 '/', 
                                 country_code, 
                                 division_code, 
                                 '.csv'))
    
    #some files contain empty lines which cause problems with date conversions
    #remove them
    footy_set <- footy_set[!footy_set$Date == '', ]
    
    #remove empty columns 
    footy_set <- footy_set[, grep('^X', 
                                  names(footy_set), 
                                  invert = TRUE, 
                                  value = TRUE)]
    
    #convert to data.table for super fast manipulations
    setDT(footy_set)
    
    footy_set[, Date := as.IDate(Date, format='%d/%m/%y')]

    #update all in-tab teams (second graphs in each tab)
    updateSelectInput(session, 
                      'team',
                      choices = sort(unique(c(footy_set[, unique(HomeTeam)],
                                              footy_set[, unique(AwayTeam)]))))
    
    updateSelectInput(session,
                      'result_team', 
                      choices = sort(unique(c(footy_set[, unique(HomeTeam)],
                                              footy_set[, unique(AwayTeam)]))))
    
    updateSelectInput(session,
                      'OU_team',
                      choices = sort(unique(c(footy_set[, unique(HomeTeam)],
                                              footy_set[, unique(AwayTeam)]))))
    
    updateSelectInput(session, 
                      'cards_team',
                      choices = sort(unique(c(footy_set[, unique(HomeTeam)],
                                              footy_set[, unique(AwayTeam)]))))
    
    updateSelectInput(session,
                      'corner_team',
                      choices = sort(unique(c(footy_set[, unique(HomeTeam)],
                                              footy_set[, unique(AwayTeam)]))))
    
    #save min and max Date for on-going seasons. In August min and max year are the same
    min_y <- footy_set[, year(min(Date))]
    max_y <- min_y + 1
    updateSliderInput(session, 
                      'date_range',
                      value=c(as.IDate(paste0(footy_set[, min_y],
                                              '-08-01')),
                              as.IDate(paste0(footy_set[, max_y],
                                              '-06-30'))),
                      min=as.IDate(paste0(footy_set[, min_y],
                                          '-08-01')),
                      max=as.IDate(paste0(footy_set[, max_y],
                                          '-06-30')))
   
    footy_set

  })
  
  #RAW TAB---------------------------------------------------------------------- 
  #this feeds the rest of the tabs with the data--------------------------------
  output$raw <- DT::renderDataTable(DT::datatable({
    
    footy_table <- dataInput()

    footy_table <- footy_table[between(Date, 
                                       input$date_range[1],
                                       input$date_range[2]), ]
    
    values$data <- footy_table

    footy_table
    
  }, options = list(autoWidth = TRUE, dom = 'tp', scrollX = TRUE)))
  
  #setting raw tab to never suspend so that------------------------------------- 
  #any changes are taken into account and so that-------------------------------
  #the data can circulate to the rest of the tabs-------------------------------
  outputOptions(output, 'raw', suspendWhenHidden=FALSE)

  #download data for the raw data
  output$downloadData <- downloadHandler(
       filename = function() {
         paste('data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(values$data, con, row.names = FALSE)
       }
     )
  
  #LEAGUE TABLE TAB-------------------------------------------------------------
  #the following function creates the league table and outputs as an HTMLtable--
  output$league_table <- render_tableHTML({
    
    footy_tab  <- values$data

    teams <- unique(c(footy_tab[ , unique(HomeTeam)],
                      footy_tab[ , unique(AwayTeam)]))
    
    #home team stats
    home_team_stats <- lapply(teams, function(x) {
      
      footy_tab[HomeTeam == x, 
                list(Team            = x,
                     Played1         = .N, 
                     Points1         = sum(ifelse(FTR == 'H', 1L, 0L)) * 3 +
                                       sum(ifelse(FTR == 'D', 1L, 0L)),
                     `Home Wins`     = sum(ifelse(FTR == 'H', 1L, 0L)), 
                     `Home Draws`    = sum(ifelse(FTR == 'D', 1L, 0L)),
                     `Home Losses`   = sum(ifelse(FTR == 'A', 1L, 0L)),
                     `Home Scored`   = sum(FTHG),
                     `Home Conceded` = sum(FTAG)),
                by='HomeTeam']
      
    })
    
    #away team stats
    away_team_stats <- lapply(teams, function(x) {
      
      footy_tab[AwayTeam==x, 
                list(Team           = x,
                     Played2        = .N, 
                     Points2        = sum(ifelse(FTR == 'A', 1L, 0L)) * 3 +
                                      sum(ifelse(FTR == 'D', 1L, 0L)),
                     `Away Wins`    = sum(ifelse(FTR == 'A', 1L, 0L)), 
                     `Away Draws`   = sum(ifelse(FTR == 'D', 1L, 0L)),
                     `Away Losses`  = sum(ifelse(FTR == 'H', 1L, 0L)),
                     `Away Scored`  = as.integer(sum(FTAG)),
                     `Away Conceded`= as.integer(sum(FTHG))),
                by='AwayTeam']
    })
    
    #make a data.table out of the lists
    home_team_stats <- rbindlist(home_team_stats)
    away_team_stats <- rbindlist(away_team_stats)
    
    #combine home and away
    team_stats <- cbind(home_team_stats, away_team_stats)
    
    try({
     team_stats <- 
      team_stats[, list(Team, 
                        Played = Played1 + Played2, 
                        `Home Wins`,
                        `Home Draws`,
                        `Home Losses`,
                        `Home Scored`,
                        `Home Conceded`, 
                        `Away Wins`,
                        `Away Draws`,
                        `Away Losses`,
                        `Away Scored`, 
                        `Away Conceded`,
                        `Total Wins` = `Home Wins` + `Away Wins`,
                        `Total Draws` = `Home Draws` + `Away Draws`,
                        `Total Losses` = `Home Losses` + `Away Losses`,
                        `Total Scored` = `Home Scored` + `Away Scored`,
                        `Total Conceded` = `Home Conceded` + `Away Conceded`,
                        `Goal Diff` = `Home Scored` + 
                                      `Away Scored` - 
                                      `Home Conceded` -
                                      `Away Conceded`, 
                        Points = as.integer(Points1 + Points2))]
     team_stats[, total_scored := `Home Scored` + `Away Scored`]
    
    #set order and add position and goal difference
    setorder(team_stats, -Points, -`Goal Diff`, -total_scored)
    team_stats[, Pos := as.character(.I)]
    team_stats[, total_scored := NULL]
    
    team_stats <- team_stats[, c('Pos',
                                 names(team_stats)[-length(names(team_stats))]),
                             with = FALSE]

    names(team_stats) <- gsub('Home|Away|Total', '', names(team_stats))
    
    }, silent=TRUE)
    
    names(team_stats) <- c('Pos', 
                           'Team', 
                           'Played', 
                           'Wins', 
                           'Draws', 
                           'Losses',
                           'For',
                           'Against',
                           'Wins',
                           'Draws',
                           'Losses',
                           'For',
                           'Against',
                           'Wins',
                           'Draws',
                           'Losses',
                           'For',
                           'Against',
                           'Goal Diff',
                           'Points')
    
    #return
    tableHTML(team_stats, 
              rownames = FALSE,
              second_header = list(c(3, 5, 5, 5, 2), c('', 'Home', 'Away', 'Total', '')),
              widths = c(25, 140, rep(60, 16), 130, 60),
              border = 0) %>%
     add_css_header(css = list('text-align', 'center'), headers = 1:ncol(team_stats)) %>%
     add_css_column(css = list('text-align', 'center'), column_names = names(team_stats)) %>%
     add_css_second_header(css = list('text-align', 'center'), second_headers = 1:5) %>%
     add_css_row(css = list('background-color', '#428bca'), rows = 1:2) %>%
     add_css_row(css = list('background-color', '#f2f2f2'), 
                 rows = odd(3:(nrow(team_stats) + 2))) %>%
     add_css_second_header(css = list(c('font-size', 'height'), c('25px', '40px')), 
                           second_headers = 1:5) %>%
     add_css_row(css = list('height', '30px'), rows = 2:(nrow(team_stats) + 2)) %>%
     add_css_column(css = list('border-right', '1px solid gray'), column_names = c('Played', 'Against')) 
    } 
  )
  
  #SHOTS AND GOALS TAB----------------------------------------------------------
  #aggregations of goals and shots per team-------------------------------------
  output$goals_per_team <- renderHighchart({
    
    footy_tab <- values$data
    validate(need('HS' %in% names(footy_tab), 
                  'Shots Information is not Available \nfor this league'))
    
    #home goals
    footy_home_goals <- footy_tab[, 
                                  list(Goals = sum(FTHG),
                                       `Total Shots` = sum(HS),
                                       `On Target Shots` = sum(HST),
                                       `No of Games` = .N),
                                  by = 'HomeTeam']
    setnames(footy_home_goals, 'HomeTeam', 'Team')
    
    #away goals
    footy_away_goals <- footy_tab[, 
                                  list(Goals = sum(FTAG),
                                       `Total Shots` = sum(AS),
                                       `On Target Shots` = sum(AST),
                                       `No of Games` = .N),
                                  by = 'AwayTeam']
    setnames(footy_away_goals, 'AwayTeam', 'Team')
    
    #bind home and away
    footy_all_goals  <- rbindlist(list(footy_home_goals, footy_away_goals))
    
    #account for home, away and all
    if(input$shots_HA == 'Home'){
      goals_final <- footy_home_goals[, `Shots per Goal` := `Total Shots` / Goals]
    }else if(input$shots_HA == 'Home'){
      goals_final <- footy_away_goals[, `Shots per Goal` := `Total Shots` / Goals]
    }else{
      goals_final <- footy_all_goals[, 
                                     list(Goals = sum(Goals),
                                          `Total Shots` = sum(`Total Shots`),
                                          `On Target Shots` = sum(`On Target Shots`),
                                          `No of Games` = sum(`No of Games`)), 
                                     by = 'Team']
      
      goals_final[, `Shots per Goal` := `Total Shots` / Goals]
    
    }
    
    #order according to teams
    setorder(goals_final, Team)
    
    #account for total
    if(input$shots_total_perteam == 'Total'){
      highchart() %>%
        hc_title(text = "Shots and Goals per Team") %>%
        hc_xAxis(categories = goals_final[, Team]) %>%
        hc_yAxis(
          list(
            title = list(text = "Goals / Shots / Shots on Target"),
            align = "left"
          ),
          list(
            title = list(text = "Average Shots to Score"),
            align = "right",
            opposite = TRUE
          )
        ) %>%
        hc_add_series(name  = 'Total Shots',     
                      type  = 'column', 
                      data  = goals_final[, `Total Shots`]) %>%
        hc_add_series(name  = 'Shots On Target', 
                      type  = 'column', 
                      data  = goals_final[, `On Target Shots`]) %>%
        hc_add_series(name  = 'Goals',           
                      type  = 'column', 
                      data  = goals_final[, Goals]) %>%
        hc_add_series(name  = 'Average Shots to Score', type = 'spline', 
                      data  = ceiling(goals_final[, `Shots per Goal`]),
                      yAxis = 1) %>%
        hc_tooltip(crosshairs = TRUE, shared=TRUE)
    }else if(input$shots_total_perteam == 'Per Game') {
      highchart() %>%
        hc_title(text = "Shots and Goals per Team") %>%
        hc_xAxis(categories = goals_final[, Team]) %>%
        hc_yAxis(
          list(
            title = list(text = "Goals / Shots / Shots on Target"),
            align = "left"
          ),
          list(
            title = list(text = "Average Shots to Score"),
            align = "right",
            opposite = TRUE
          )
        ) %>%
        hc_add_series(name  = 'Average Shots',     
                      type  = 'column', 
                      data  = round(goals_final[, `Total Shots`] / 
                                      goals_final[, `No of Games`], 
                                    2)) %>%
        hc_add_series(name  = 'Average Shots On Target', 
                      type  = 'column', 
                      data  = round(goals_final[, `On Target Shots`] /
                                      goals_final[, `No of Games`],
                                    2)) %>%
        hc_add_series(name  = 'Average Goals',           
                      type  = 'column', 
                      data  = round(goals_final[, Goals] /
                                      goals_final[, `No of Games`],
                                    2)) %>%
        hc_add_series(name  = 'Average Shots to Score', 
                      type  = 'spline', 
                      data  = ceiling(goals_final[, `Shots per Goal`]),
                      yAxis = 1) %>%
        hc_tooltip(crosshairs = TRUE, shared=TRUE)
    }
          
  })

  #Goals and shots over time per team-------------------------------------------
  output$goals_over_time <- renderHighchart({
    
    footy_tab <- values$data
    #validation in case HS is missing
    validate(need('HS' %in% names(footy_tab), 
                  'Shots Information is not Available \nfor this league'))
    
    team_individual <- footy_tab[HomeTeam == input$team | AwayTeam == input$team, 
                                 list(Date, 
                                      HomeTeam, 
                                      AwayTeam, 
                                      FTHG, 
                                      FTAG, 
                                      HS, 
                                      AS, 
                                      HST, 
                                      AST, 
                                      Score = paste(FTHG, FTAG, sep ='-'))]
    
    team_individual_m <- melt(team_individual, 
                               measure.vars = c('HomeTeam', 'AwayTeam'))
    team_individual_m2 <- team_individual_m[value == input$team,]
    setorder(team_individual_m2, Date)
    
    team_individual_m3  <- team_individual_m[value != input$team,]
    setorder(team_individual_m3, Date)
    
    #account for Home/Away/All
    if (input$HA == 'Home') {
     
      team_individual_m2 <- 
        team_individual_m2[variable == 'HomeTeam', 
                           list(Date, 
                                Goals = FTHG, 
                                `Total Shots` = HS, 
                                `On Target Shots` = HST,
                                Score)]
      
      team_individual_m3 <-
        team_individual_m3[variable == 'AwayTeam', list(Date, value)]
 
    } else if (input$HA == 'Away') {
     
      team_individual_m2 <- 
        team_individual_m2[variable == 'AwayTeam', 
                           list(Date, 
                                Goals = FTAG,
                                `Total Shots` = AS,
                                `On Target Shots` = AST,
                                Score)]
      
      team_individual_m3 <-
        team_individual_m3[variable == 'HomeTeam', list(Date, value)]
      
    } else {
     
      team_individual_m2 <- 
        team_individual_m2[, Goals := ifelse(variable == 'HomeTeam',
                                             FTHG,
                                             FTAG)][, 
                             `Total Shots` := ifelse(variable == 'HomeTeam',
                                                     HS,
                                                     AS)][,
                             `On Target Shots` := ifelse(variable == 'HomeTeam',
                                                         HST,
                                                         AST)][, 
                             Score := Score]
      
    }
    
    #plot
    highchart() %>%
      hc_title(text = paste('Shots / Goals - ', input$team)) %>%
      hc_xAxis(list(categories = paste(team_individual_m3$value,
                                       format(team_individual_m3$Date, '%d/%m'),
                                       sep = ' ')),
               list(categories = team_individual_m2$Score, opposite = TRUE)) %>%
      hc_add_series(name = 'Total Shots',
                    type = 'spline',
                    data = team_individual_m2$`Total Shots`) %>%
      hc_add_series(name = 'On Target Shots',
                    type = 'spline',
                    data=team_individual_m2$`On Target Shots`) %>%
      hc_add_series(name = 'Goals',
                    type = 'spline',
                    data = team_individual_m2$Goals,
                    xAxis = 1) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE)
    
  })

  #total goals and shots--------------------------------------------------------
  output$goals_total <- renderHighchart({ 
    
    footy_tab <- values$data
    validate(need('HS' %in% names(footy_tab),
                  'Shots Information is not Available \nfor this league'))
    
    #home goals
    footy_home_goals <- footy_tab[, list(Goals = sum(FTHG),
                                         `Total Shots` = sum(HS),
                                         `On Target Shots` = sum(HST),
                                         `No of Games` = .N),
                                  by = 'HomeTeam']
    setnames(footy_home_goals, 'HomeTeam', 'Team')
    
    #away goals
    footy_away_goals <- footy_tab[, list(Goals = sum(FTAG),
                                         `Total Shots` = sum(AS),
                                         `On Target Shots` = sum(AST),
                                         `No of Games` = .N),
                                  by = 'AwayTeam']
    setnames(footy_away_goals, 'AwayTeam', 'Team')
    
    #bind the sets
    footy_all_goals <- rbindlist(list(footy_home_goals,
                                      footy_away_goals))
    
    goals_final <- footy_all_goals[, 
                                   list(Goals = sum(Goals),
                                        `Total Shots` = sum(`Total Shots`),
                                        `On Target Shots` = sum(`On Target Shots`),
                                        `No of Games` = sum(`No of Games`)), 
                                   by = 'Team']
    goals_final[, `Shots per Goal` := `Total Shots` / Goals]
    
    #plot
    highchart() %>%
      hc_title(text = 'League Shots and Goals') %>%
      hc_add_series(name = "Value", 
                    type = "funnel",
                    data = list(list(y = sum(goals_final$`Total Shots`), 
                                     name = "Total Shots"),
                                list(y = sum(goals_final$`On Target Shots`), 
                                     name = "Total Shots on Target"),
                                list(y = sum(goals_final$Goals), 
                                     name = "Total Goals Scored")))
    
  })

  #bubble - shots per goal------------------------------------------------------
  output$rate <- renderText({
   
    footy_tab <- values$data
    validate(need('HS' %in% names(footy_tab), 
                  'Shots Information is not Available \nfor this league'))
    
    #home goals
    footy_home_goals <- footy_tab[, 
                                  list(Goals = sum(FTHG),
                                       `Total Shots` = sum(HS),
                                       `On Target Shots` = sum(HST),
                                       `No of Games` = .N), 
                                  by = 'HomeTeam']
    setnames(footy_home_goals, 'HomeTeam', 'Team')
    
    #away goals
    footy_away_goals <- footy_tab[, 
                                  list(Goals = sum(FTAG),
                                       `Total Shots` = sum(AS),
                                       `On Target Shots` = sum(AST),
                                       `No of Games` = .N),
                                  by = 'AwayTeam']
    setnames(footy_away_goals, 'AwayTeam', 'Team')
    
    #bind goals
    footy_all_goals  <- rbindlist(list(footy_home_goals, footy_away_goals))
    
    goals_final <- footy_all_goals[, 
                                   list(Goals = sum(Goals),
                                        `Total Shots` = sum(`Total Shots`),
                                        `On Target Shots` = sum(`On Target Shots`),
                                        `No of Games` = sum(`No of Games`)), 
                                   by = 'Team']
    goals_final[, `Shots per Goal` := `Total Shots` / Goals]
    
    #get the ceiling of the shots - needs to be integer
    ceiling(sum(goals_final$`Total Shots`) / sum(goals_final$Goals))
    
  })
  

  #RESULTS TAB------------------------------------------------------------------
  #results per team aggregated--------------------------------------------------
  output$team_results <- renderHighchart({
    
    footy_tab <- values$data
    
    #HA and FTHT selections
    HA   <- switch(input$results_ha, 
                   Home = 'HomeTeam', 
                   Away = 'AwayTeam', 
                   All = 'All')
    FTHT <- switch(input$FTHT,
                   `Full Time` = 'FTR',
                   `Half Time` = 'HTR')
    
    #validation in case HS is not there
    if (FTHT == 'HTR') {
     validate(need('HS' %in% names(footy_tab), 
                   'Half Time Results are not Available \nfor this league'))
    }
    
    #account for hometeam and awayteam
    if (HA %in% c('HomeTeam', 'AwayTeam')) {
     
      footy_result <- footy_tab[, .N, by = c(HA, FTHT)]
      footy_result_dcast <- dcast(footy_result, 
                                  as.formula(paste(HA, '~', FTHT)),
                                  value.var = 'N')
      
      if (HA == 'AwayTeam') setnames(footy_result_dcast, c('H','A'), c('A','H'))
      
      setnames(footy_result_dcast, HA, 'Team')
      
      footy_result_dcast <- na_converter(footy_result_dcast)
      footy_all_result <- footy_result_dcast[, 
                                             list(Team = Team, 
                                                  Wins = H, 
                                                  Draws = D, 
                                                  Losses = A)]
      
    } else {
     
      footy_home_result <- footy_tab[, .N, by = c('HomeTeam', FTHT)]
      footy_home_result_dcast <- dcast(footy_home_result, 
                                       as.formula(paste('HomeTeam ~', FTHT)), 
                                       value.var = 'N')
      setnames(footy_home_result_dcast, 'HomeTeam', 'Team')
      
      footy_away_result_dcast <- na_converter(footy_home_result_dcast)
      footy_away_result       <- footy_tab[, .N, by = c('AwayTeam', FTHT)]
      footy_away_result_dcast <- dcast(footy_away_result, 
                                       as.formula(paste('AwayTeam ~', FTHT)), 
                                       value.var = 'N')
      setnames(footy_away_result_dcast, c('H','A'), c('A','H'))
      setnames(footy_away_result_dcast, 'AwayTeam', 'Team')
      footy_away_result_dcast <- na_converter(footy_away_result_dcast)
      footy_all_result <- rbindlist(list(footy_home_result_dcast, 
                                         footy_away_result_dcast),
                                    use.names = TRUE)
      footy_all_result <- footy_all_result[, 
                                           list(Wins = sum(H), 
                                                Draws = sum(D), 
                                                Losses = sum(A)), 
                                           by = 'Team']
      
    }
    
    #order data according to team
    setorder(footy_all_result, Team)
    
    #plot
    highchart() %>%
      hc_title(text = "Results per Team") %>%
      hc_xAxis(categories = footy_all_result[, Team]) %>%
      hc_add_series(name = 'Wins', 
                    type = 'column', 
                    data = footy_all_result[, Wins])   %>%
      hc_add_series(name = 'Draws', 
                    type = 'column',
                    data = footy_all_result[, Draws])  %>%
      hc_add_series(name = 'Losses', 
                    type = 'column',
                    data = footy_all_result[, Losses]) %>%
      hc_colors(c('#90ed7d', '#cccccc', '#ff3333')) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE)
    
  })

  #results over time per team---------------------------------------------------
  output$result_over_time <- renderHighchart({
    
    footy_tab <- values$data
    
    #account for FT and HT
    FTHT <- switch(input$result_FTHT, `Full Time` = 'FTR', `Half Time` = 'HTR')
    
    #validation in case HTR is missing
    if (FTHT == 'HTR') {
     validate(need('HTR' %in% names(footy_tab), 
                   'Half Time Results are not Available \nfor this league'))
    }
    
    team_individual <- footy_tab[HomeTeam == input$result_team | 
                                  AwayTeam == input$result_team, 
                                 c('Date', 'HomeTeam', 'AwayTeam', FTHT), 
                                 with=FALSE]
    setnames(team_individual, FTHT, 'Result')
    
    #melt data to create home and away
    team_individual_m <- melt(team_individual, 
                              measure.vars = c('HomeTeam', 'AwayTeam'))
    team_individual_m2  <- team_individual_m[value == input$result_team,]
    setorder(team_individual_m2, Date)
    team_individual_m3  <- team_individual_m[value != input$result_team,]
    setorder(team_individual_m3, Date)
    
    if (input$result_HA == 'Home') {
      team_individual_m2 <- 
        team_individual_m2[variable == 'HomeTeam', list(Date, Result)]
      
      team_individual_m2[Result == 'H', Result := 'W'][Result == 'A', 
                                                       Result := 'L']
      
      team_individual_m3 <-
        team_individual_m3[variable == 'AwayTeam', list(Date, value)]
      
    } else if (input$result_HA == 'Away'){
      team_individual_m2 <- 
        team_individual_m2[variable == 'AwayTeam', list(Date, Result)]
      team_individual_m2[Result == 'H', Result := 'L'][Result == 'A', 
                                                       Result := 'W']
      team_individual_m3 <-
        team_individual_m3[variable == 'HomeTeam', list(Date, value)]
    } else {
      team_individual_m2[variable=='HomeTeam' & Result == 'H', 
                         Result := 'W'][variable == 'HomeTeam' & Result == 'A', 
                                        Result := 'L']
      team_individual_m2[variable=='AwayTeam' & Result == 'H', 
                         Result := 'L'][variable=='AwayTeam' & Result == 'A', 
                                        Result := 'W']
      
    }
    
    #specify ad_hoc javascript function for formatting hovering
    jav_func <- JS('function() {
                       if (this.y == 2) {
                          return this.series.name + ": Draw";
                       } else if (this.y == 1) {
                          return this.series.name + ": Loss";
                       } else {
                          return this.series.name + ": Win";
                       }
                   }')
    
    plot
    highchart() %>%
      hc_title(text = input$result_team, margin = 10) %>%
      hc_yAxis(title = list(text = "Result"),
               max = 3.5,
               min = 0.5,
               startOnTick = FALSE,
               gridLineWidth  = 0,
               #keeping this for now as an example of how to use the formatter. 
               #useHTML must be enabled
               labels = list(enabled = FALSE, 
                             useHTML = TRUE, 
                             formatter = JS('function() {return this.value;}')),
               plotBands = list(
                 list(from = 0.5,  
                      to = 1.5, 
                      color = "#ffcccc", 
                      label = list(text = "Losses", 
                                   y =  40, 
                                   style = list(`font-size`='20px'))),
                 list(from = 1.5,  
                      to = 2.5, 
                      color = "#e6e6e6", 
                      label = list(text = "Draws", 
                                   rotation = 90, 
                                   y = -30, 
                                   zIndex = 20, 
                                   style = list(`font-size` = '20px'))),
                 list(from = 2.5,  
                      to = 3.5, 
                      color = "#d8f9d2", 
                      label = list(text = "Wins", 
                                   y = -25, 
                                   style = list(`font-size` = '20px'))))) %>% 
      hc_xAxis(categories = paste(team_individual_m3$value, 
                                  format(team_individual_m3$Date, '%d/%m'), 
                                  sep = ' ')) %>%
      hc_add_series(name = 'Result', 
                    type = 'spline', 
                    data = as.numeric(factor(team_individual_m2$Result, 
                                             levels = c('L','D','W')))) %>%
      hc_tooltip(crosshairs = TRUE, 
                 useHTML = TRUE, 
                 formatter = jav_func) %>%
      hc_legend(enabled = FALSE)
        
  })

  #results total graphs FT------------------------------------------------------
  output$total_results_FT <- renderHighchart ({
  
    footy_tab <- values$data
    
    if(input$results_percent == 'Absolute') {
     
      highchart() %>%
        hc_title(text = 'Full Time Wins and Draws') %>%
        hc_yAxis(title = list(text = '')) %>%
        hc_xAxis(categories = c('Home Wins', 'Draws', 'Away Wins')) %>%
        hc_add_series(name = 'Value', 
                      type = 'column', 
                      data = footy_tab[, .N, keyby = 'FTR'][
                        c('H','D','A'),][
                         , N],
                      colorByPoint = TRUE) %>%
        hc_colors(c('#90ed7d', '#cccccc', '#ff3333')) %>%
        hc_legend(enabled = FALSE)
     
    } else {
     
      highchart() %>%
        hc_title(text = 'Full Time Wins and Draws') %>%
        hc_yAxis(title  = list(text = ''),
                 labels = list(useHTML = TRUE, 
                               formatter = JS('function() {
                                                return this.value + "%" ;
                                              }'))) %>%
        hc_xAxis(categories = c('Home Wins', 'Draws', 'Away Wins')) %>%
        hc_add_series(name = 'Value', 
                      type = 'column', 
                      data = footy_tab[, .N, keyby = 'FTR'][
                        c('H','D','A'),][
                         , N/sum(N)*100],
                      colorByPoint = TRUE) %>%
        hc_colors(c('#90ed7d', '#cccccc', '#ff3333')) %>%
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">
                                  {series.name}</span>:{point.y:.2f}%')
     
    }
  
  })
  
  #results total graphs FT------------------------------------------------------
  output$total_results_HT <- renderHighchart ({
    
    footy_tab <- values$data
    
    #validate HTR is in the data set
    validate(need('HTR' %in% names(footy_tab),
                  'Half Time Results are not Available \nfor this league'))
    
    if (input$results_percent == 'Absolute') {
    
     highchart() %>%
      hc_title(text = 'Half Time Wins and Draws') %>%
      hc_yAxis(title = list(text = '')) %>%
      hc_xAxis(categories = c('Home Wins', 'Draws', 'Away Wins')) %>%
      hc_add_series(name = 'Value', 
                    type = 'column', 
                    data = footy_tab[, .N, keyby = 'HTR'][c('H','D','A'),][,N], 
                    colorByPoint = TRUE) %>%
      hc_colors(c('#90ed7d', '#cccccc', '#ff3333')) %>%
      hc_legend(enabled = FALSE)
     
    } else {
     
      highchart() %>%
        hc_title(text = 'Half Time Wins and Draws') %>%
        hc_yAxis(title = list(text = ''),
                 labels = list(useHTML=TRUE, 
                               formatter=JS('function() {
                                              return this.value + "%" ;
                                            }'))) %>%
        hc_xAxis(categories = c('Home Wins', 'Draws', 'Away Wins')) %>%
        hc_add_series(name = 'Value', 
                      type = 'column', 
                      data = footy_tab[, .N, keyby = 'HTR'][
                        c('H','D','A'),][
                          , N / sum(N) * 100], 
                      colorByPoint=TRUE) %>%
        hc_colors(c('#90ed7d', '#cccccc', '#ff3333')) %>%
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">
                                  {series.name}</span>:{point.y:.2f}%')
    }
    
  })

  #OVER UNDER TAB---------------------------------------------------------------
  #over under per team aggregated-----------------------------------------------
  output$team_OU <- renderHighchart ({
  
    footy_tab  <- values$data
    
    #create HA and FTHT
    HA <- switch(input$ou_ha, Home = 'HomeTeam', Away = 'AwayTeam', All = 'All')
    FTHT <- switch(input$ou_FTHT, `Full Time` = 'FT', `Half Time` = 'HT')
    
    if (FTHT == 'HT') {
     validate(need('HTHG' %in% names(footy_tab), 
                   'Half Time Goals are not Available \nfor this league'))
    }
    
    #account for home team and away team
    if (HA %in% c('HomeTeam', 'AwayTeam')) {
     
     #account for FT and half time
      if (FTHT == 'FT'){
       
        footy_tab[, total_goals := FTHG + FTAG]
        footy_tab[,`O/U 0.5` := ifelse(total_goals < 0.5, 
                                              'Under 0.5', 
                                              'Over 0.5')][,
                   `O/U 1.5` := ifelse(total_goals < 1.5, 
                                              'Under 1.5', 
                                              'Over 1.5')][,
                   `O/U 2.5` := ifelse(total_goals < 2.5,
                                              'Under 2.5',
                                              'Over 2.5')][,
                   `O/U 3.5` := ifelse(total_goals < 3.5, 
                                              'Under 3.5', 
                                              'Over 3.5')][,
                   `O/U 4.5` := ifelse(total_goals < 4.5, 
                                              'Under 4.5', 
                                              'Over 4.5')][,
                   `O/U 5.5` := ifelse(total_goals < 5.5, 
                                              'Under 5.5', 
                                              'Over 5.5')][,
                   `O/U 6.5` := ifelse(total_goals < 6.5, 
                                              'Under 6.5', 
                                              'Over 6.5')][,
                   `O/U 7.5` := ifelse(total_goals < 7.5, 
                                              'Under 7.5', 
                                              'Over 7.5')]
        
      } else if (FTHT == 'HT') {
       
        footy_tab[, total_goals := HTHG + HTAG]
        footy_tab[, `O/U 0.5` := ifelse(total_goals < 0.5, 
                                               'Under 0.5', 
                                               'Over 0.5')][,
                    `O/U 1.5` := ifelse(total_goals < 1.5, 
                                               'Under 1.5', 
                                               'Over 1.5')][,
                    `O/U 2.5` := ifelse(total_goals < 2.5, 
                                               'Under 2.5', 
                                               'Over 2.5')][,
                    `O/U 3.5` := ifelse(total_goals < 3.5, 
                                               'Under 3.5', 
                                               'Over 3.5')][,
                    `O/U 4.5` := ifelse(total_goals < 4.5, 
                                               'Under 4.5', 
                                               'Over 4.5')][,
                    `O/U 5.5` := ifelse(total_goals < 5.5, 
                                               'Under 5.5', 
                                               'Over 5.5')][,
                    `O/U 6.5` := ifelse(total_goals < 6.5, 
                                               'Under 6.5', 
                                               'Over 6.5')][,
                    `O/U 7.5` := ifelse(total_goals < 7.5, 
                                               'Under 7.5', 
                                               'Over 7.5')]
        
      }
     
      footy_result <- footy_tab[, .N, by = c(HA, input$OU)]
      footy_result_dcast <- dcast(footy_result, 
                                  as.formula(paste0(HA, '~ `', input$OU, '`')),
                                  value.var = 'N')
      setnames(footy_result_dcast, HA, 'Team')
      footy_result_dcast <- na_converter(footy_result_dcast)
      footy_all_result <- footy_result_dcast
      
    }else{
     
      if (FTHT == 'FT'){
       
        footy_tab[, total_goals := FTHG + FTAG]
        footy_tab[,`O/U 0.5` := ifelse(total_goals < 0.5, 
                                              'Under 0.5', 
                                              'Over 0.5')][,
                   `O/U 1.5` := ifelse(total_goals < 1.5, 
                                              'Under 1.5', 
                                              'Over 1.5')][,
                   `O/U 2.5` := ifelse(total_goals < 2.5, 
                                              'Under 2.5', 
                                              'Over 2.5')][,
                   `O/U 3.5` := ifelse(total_goals < 3.5, 
                                              'Under 3.5', 
                                              'Over 3.5')][,
                   `O/U 4.5` := ifelse(total_goals < 4.5, 
                                              'Under 4.5', 
                                              'Over 4.5')][,
                   `O/U 5.5` := ifelse(total_goals < 5.5, 
                                              'Under 5.5', 
                                              'Over 5.5')][,
                   `O/U 6.5` := ifelse(total_goals < 6.5, 
                                              'Under 6.5', 
                                              'Over 6.5')][,
                   `O/U 7.5` := ifelse(total_goals < 7.5, 
                                              'Under 7.5', 
                                              'Over 7.5')]
        
      } else if (FTHT == 'HT') {
       
        footy_tab[, total_goals := FTHG + FTAG]
        footy_tab[,`O/U 0.5` := ifelse(total_goals < 0.5, 
                                              'Under 0.5', 
                                              'Over 0.5')][,
                   `O/U 1.5` := ifelse(total_goals < 1.5, 
                                              'Under 1.5', 
                                              'Over 1.5')][,
                   `O/U 2.5` := ifelse(total_goals < 2.5, 
                                              'Under 2.5', 
                                              'Over 2.5')][,
                   `O/U 3.5` := ifelse(total_goals < 3.5, 
                                              'Under 3.5', 
                                              'Over 3.5')][,
                   `O/U 4.5` := ifelse(total_goals < 4.5, 
                                              'Under 4.5', 
                                              'Over 4.5')][,
                   `O/U 5.5` := ifelse(total_goals < 5.5, 
                                              'Under 5.5', 
                                              'Over 5.5')][,
                   `O/U 6.5` := ifelse(total_goals < 6.5, 
                                              'Under 6.5', 
                                              'Over 6.5')][,
                   `O/U 7.5` := ifelse(total_goals < 7.5, 
                                              'Under 7.5', 
                                              'Over 7.5')]
        
      }
     
      #home
      footy_home_result <- footy_tab[, .N, by = c('HomeTeam', input$OU)]
      footy_home_result_dcast <- dcast(footy_home_result, 
                                       as.formula(paste0('HomeTeam ~ `', 
                                                         input$OU, '`')), 
                                       value.var = 'N')
      setnames(footy_home_result_dcast, 'HomeTeam', 'Team')
      footy_home_result_dcast <- na_converter(footy_home_result_dcast)
      
      #away
      footy_away_result <- footy_tab[, .N, by = c('AwayTeam', input$OU)]
      footy_away_result_dcast <- dcast(footy_away_result, 
                                       as.formula(paste0('AwayTeam ~ `', 
                                                         input$OU, '`')), 
                                       value.var = 'N')
      setnames(footy_away_result_dcast, 'AwayTeam', 'Team')
      footy_away_result_dcast <- na_converter(footy_away_result_dcast)
      
      #all
      footy_all_result <- rbindlist(list(footy_home_result_dcast, footy_away_result_dcast), 
                                    use.names = TRUE)
      footy_all_result <- footy_all_result[, lapply(.SD, sum), by = 'Team']
    }
    
    #get over and under names
    Over  <- names(footy_all_result)[like(names(footy_all_result), 'Over*')]
    Under <- names(footy_all_result)[like(names(footy_all_result), 'Under*')]
    
    #order data according to Team
    setorder(footy_all_result, Team)
    
    #plot
    highchart() %>%
      hc_xAxis(categories=footy_all_result[, Team]) %>%
      hc_add_series(name = Over , 
                    type = 'column', 
                    data = unname(unlist(footy_all_result[, 
                                                          Over , 
                                                          with = FALSE]))) %>%
      hc_add_series(name = Under, 
                    type = 'column', 
                    data = unname(unlist(footy_all_result[, 
                                                          Under, 
                                                          with = FALSE]))) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
      
    
  })

  #Over under per team over time------------------------------------------------
  output$OU_over_time <- renderHighchart({
  
    footy_tab <- values$data
    
    #account for FT and HT
    FTHT <- switch(input$OUFTHT_over_time, 
                   `Full Time` = 'FT', 
                   `Half Time` = 'HT' )
    
    if (FTHT == 'HT') {
     validate(need('HTHG' %in% names(footy_tab), 
                   'Half Time Goals are not Available \nfor this league'))
    }
    
    #get FTHG or HTHG only
    if (FTHT == 'FT'){
      team_individual <- footy_tab[HomeTeam == input$OU_team | 
                                     AwayTeam == input$OU_team, 
                                   c('Date', 
                                     'HomeTeam', 
                                     'AwayTeam', 
                                     'FTHG',
                                     'FTAG'), 
                                   with = FALSE]  
    } else {
      team_individual <- footy_tab[HomeTeam == input$OU_team | 
                                     AwayTeam == input$OU_team, 
                                   c('Date', 
                                     'HomeTeam', 
                                     'AwayTeam', 
                                     'HTHG',
                                     'HTAG'), 
                                   with = FALSE]  
    }
    
    if (FTHT == 'FT') {
      team_individual[, total_goals := FTHG + FTAG]
    } else {
      team_individual[, total_goals := HTHG + HTAG]
    }
    
    #melt to create each team's stats
    team_individual_m  <- melt(team_individual, 
                               measure.vars = c('HomeTeam', 'AwayTeam'))
    team_individual_m2 <- team_individual_m[value == input$OU_team, ]
    setorder(team_individual_m2, Date)
    
    team_individual_m2[,`O/U 0.5` := ifelse(total_goals < 0.5, 
                                                   'Under 0.5', 
                                                   'Over 0.5')][,
                        `O/U 1.5` := ifelse(total_goals < 1.5, 
                                                   'Under 1.5', 
                                                   'Over 1.5')][,
                        `O/U 2.5` := ifelse(total_goals < 2.5, 
                                                   'Under 2.5', 
                                                   'Over 2.5')][,
                        `O/U 3.5` := ifelse(total_goals < 3.5, 
                                                   'Under 3.5', 
                                                   'Over 3.5')][,
                        `O/U 4.5` := ifelse(total_goals < 4.5, 
                                                   'Under 4.5', 
                                                   'Over 4.5')][,
                        `O/U 5.5` := ifelse(total_goals < 5.5, 
                                                   'Under 5.5', 
                                                   'Over 5.5')][,
                        `O/U 6.5` := ifelse(total_goals < 6.5, 
                                                   'Under 6.5', 
                                                   'Over 6.5')][,
                        `O/U 7.5` := ifelse(total_goals < 7.5, 
                                                   'Under 7.5', 
                                                   'Over 7.5')]
    
    team_individual_m3  <- team_individual_m[value != input$OU_team,]
    setorder(team_individual_m3, Date)
    
    if (input$OU_overtime_HA == 'Home') {
      team_individual_m2 <- 
        team_individual_m2[variable == 'HomeTeam', 
                           c('Date', input$OU_overtime), with = FALSE]
      team_individual_m3 <-
        team_individual_m3[variable == 'AwayTeam', list(Date, value)]
    }
    
    if (input$OU_overtime_HA == 'Away') {
      team_individual_m2 <- 
        team_individual_m2[variable == 'AwayTeam', 
                           c('Date', input$OU_overtime), with = FALSE]
      team_individual_m3 <-
        team_individual_m3[variable == 'HomeTeam', list(Date, value)]
    }
    
    #get over and under names
    over  <- paste0('Over' , gsub("[[:alpha:]]|/", '', input$OU_overtime))
    under <- paste0('Under', gsub("[[:alpha:]]|/", '', input$OU_overtime))
    
    #specify ad-hoc javascript function for hover labels
    jav_func <- JS(paste0(
      'function() {
                   if(this.y == 1) {
                   return this.series.name + ":',  under, '";
                   }else if(this.y == 2) {
                   return this.series.name + ":',  over, '";
                   }
                   }')
    )
  
    #plot
    highchart() %>%
      hc_title(text = paste(input$OU_overtime, '-', input$OU_team)) %>%
      hc_yAxis(title = list(text = 'Result'),
               gridLineWidth  = 0,
               min = 0.5,
               max = 2.5,
               startOnTick = FALSE,
               labels = list(enabled = FALSE, useHTML = TRUE),
               plotBands = list(
                 list(from = 0.5, 
                      to = 1.5, 
                      color = "#ffcccc", 
                      label = list(text = under, 
                                   y =  50, 
                                   style = list(`font-size`='20px'))),
                 list(from = 1.5, 
                      to = 2.5, 
                      color = "#b3e0ff", 
                      label = list(text = over , 
                                   y = -50, 
                                   style = list(`font-size` = '20px'))))) %>% 
      hc_xAxis(categories = paste(team_individual_m3$value, 
                                  format(team_individual_m3$Date, '%d/%m'), 
                                  sep = ' ')) %>%
      hc_add_series(name  = 'Result', 
                    color = "#666666", 
                    type = 'spline', 
                    data = as.numeric(factor(team_individual_m2[[input$OU_overtime]],
                                             levels = c(under, over)))) %>%
      hc_tooltip(crosshairs = TRUE, useHTML = TRUE, formatter = jav_func) %>%
      hc_legend(enabled = FALSE)
  
  })
  
  #over under total 0.5---------------------------------------------------------
  output$OU_05 <- renderHighchart({
    
    footy_tab <- values$data
    FTHT <- switch(input$OUFTHT_overall, 
                   `Full Time` = 'FT', 
                   `Half Time` = 'HT')
    
    if (FTHT == 'HT') {
     validate(need('HTHG' %in% names(footy_tab), 
                   'Half Time Goals are not Available \nfor this league'))
    }
    
    #account for FT and HT
    if (FTHT == 'FT'){
      footy_tab[, total_goals := FTHG + FTAG]
    } else {
      footy_tab[, total_goals := HTHG + HTAG]
    }
    
    footy_tab[, `Over/Under 0.5` := ifelse(total_goals < 0.5, 
                                           'Under 0.5', 
                                           'Over 0.5')]
    
    mytab <- footy_tab[, .N, by = c('Over/Under 0.5')]
    setorder(mytab, 'Over/Under 0.5')
    
    #plot
    highchart() %>%
      hc_title(text = 'Over/Under 0.5') %>%
      hc_yAxis(title = list(text = 'Matches')) %>%
      hc_xAxis(categories = mytab[, `Over/Under 0.5`]) %>%
      hc_add_series(name = 'Matches', 
                    type = 'column', 
                    data = mytab[, N], 
                    colorByPoint=TRUE) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_legend(enabled = FALSE)
    
  })  

  #over under total 1.5---------------------------------------------------------
  output$OU_15 <- renderHighchart({
    
    footy_tab <- values$data
    
    #account for FT and HT
    FTHT <- switch(input$OUFTHT_overall, 
                   `Full Time` = 'FT', 
                   `Half Time` = 'HT' )
    
    if (FTHT == 'HT') {
     validate(need('HTHG' %in% names(footy_tab), ''))
    }
    
    #full time - half time
    if (FTHT == 'FT') {
      footy_tab[, total_goals := FTHG + FTAG]
    } else {
      footy_tab[, total_goals := HTHG + HTAG]
    }
    
    footy_tab[, `Over/Under 1.5` := ifelse(total_goals < 1.5,
                                           'Under 1.5',
                                           'Over 1.5')]
    
    mytab <- footy_tab[, .N, by = c('Over/Under 1.5')]
    setorder(mytab, 'Over/Under 1.5')
    
    #plot
    highchart() %>%
      hc_title(text = 'Over/Under 1.5') %>%
      hc_yAxis(title = list(text = 'Matches')) %>%
      hc_xAxis(categories = mytab[, `Over/Under 1.5`]) %>%
      hc_add_series(name = 'Matches', 
                    type = 'column', 
                    data = mytab[, N], 
                    colorByPoint = TRUE) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_legend(enabled = FALSE)
    
  })  

  #over under total 2.5---------------------------------------------------------
  output$OU_25 <- renderHighchart({
    
    footy_tab <- values$data
    
    #account for FT and HT
    FTHT <- switch(input$OUFTHT_overall, 
                   `Full Time` = 'FT', 
                   `Half Time` = 'HT')
    
    if (FTHT == 'HT') {
     validate(need('HTHG' %in% names(footy_tab), ''))
    }
    
    if (FTHT=='FT') {
      footy_tab[, total_goals := FTHG + FTAG]
    } else {
      footy_tab[, total_goals := HTHG + HTAG]
    }
    
    footy_tab[, `Over/Under 2.5` := ifelse(total_goals < 2.5, 
                                           'Under 2.5', 
                                           'Over 2.5')]
    
    mytab <- footy_tab[, .N, by = c('Over/Under 2.5')]
    setorder(mytab, 'Over/Under 2.5')
    
    #plot
    highchart() %>%
      hc_title(text = 'Over/Under 2.5') %>%
      hc_yAxis(title = list(text = 'Matches')) %>%
      hc_xAxis(categories = mytab[, `Over/Under 2.5`]) %>%
      hc_add_series(name = 'Matches', 
                    type = 'column', 
                    data = mytab[, N], 
                    colorByPoint = TRUE) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_legend(enabled = FALSE)
    
  })  

  #over under total 3.5---------------------------------------------------------
  output$OU_35 <- renderHighchart({
    
    footy_tab <- values$data
    
    FTHT <- switch(input$OUFTHT_overall, 
                   `Full Time` = 'FT', 
                   `Half Time` = 'HT')
    
    if (FTHT == 'HT') {
     validate(need('HTHG' %in% names(footy_tab), ''))
    }
    
    if (FTHT == 'FT') {
      footy_tab[, total_goals := FTHG + FTAG]
    } else {
      footy_tab[, total_goals := HTHG + HTAG]
    }
    
    footy_tab[, `Over/Under 3.5` := ifelse(total_goals < 3.5, 
                                           'Under 3.5', 
                                           'Over 3.5')]
    
    mytab <- footy_tab[, .N, by = c('Over/Under 3.5')]
    setorder(mytab, 'Over/Under 3.5')
    
    #plot
    highchart() %>%
      hc_title(text = 'Over/Under 3.5') %>%
      hc_yAxis(title = list(text = 'Matches')) %>%
      hc_xAxis(categories = mytab[, `Over/Under 3.5`]) %>%
      hc_add_series(name = 'Matches', 
                    type = 'column', 
                    data = mytab[, N], 
                    colorByPoint = TRUE) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_legend(enabled = FALSE)
    
  })  

  #over under total 4.5---------------------------------------------------------
  output$OU_45 <- renderHighchart({
    
    footy_tab <- values$data
    
    FTHT <- switch(input$OUFTHT_overall, 
                   `Full Time` = 'FT', 
                   `Half Time` = 'HT')
    
    if (FTHT == 'HT') {
     validate(need('HTHG' %in% names(footy_tab), ''))
    }
    
    if (FTHT == 'FT'){
      footy_tab[, total_goals := FTHG + FTAG]
    } else {
      footy_tab[, total_goals := HTHG + HTAG]
    }
    
    footy_tab[, `Over/Under 4.5` := ifelse(total_goals < 4.5,
                                           'Under 4.5',
                                           'Over 4.5')]
    
    mytab <- footy_tab[, .N, by = c('Over/Under 4.5')]
    setorder(mytab, 'Over/Under 4.5')
    
    #plot
    highchart() %>%
      hc_title(text = 'Over/Under 4.5') %>%
      hc_yAxis(title = list(text = 'Matches')) %>%
      hc_xAxis(categories = mytab[, `Over/Under 4.5`]) %>%
      hc_add_series(name = 'Matches', 
                    type = 'column', 
                    data = mytab[, N], 
                    colorByPoint = TRUE) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_legend(enabled = FALSE)
    
  })  

  #over under total 5.5---------------------------------------------------------
  output$OU_55 <- renderHighchart({
    
    footy_tab <- values$data
    
    FTHT <- switch(input$OUFTHT_overall, 
                   `Full Time` = 'FT', 
                   `Half Time` = 'HT')
    
    if (FTHT == 'HT') {
     validate(need('HTHG' %in% names(footy_tab), ''))
    }
    
    if (FTHT == 'FT'){
      footy_tab[, total_goals := FTHG + FTAG]
    } else {
      footy_tab[, total_goals := HTHG + HTAG]
    }
    
    footy_tab[, `Over/Under 5.5` := ifelse(total_goals < 5.5,
                                           'Under 5.5',
                                           'Over 5.5')]
    
    mytab <- footy_tab[, .N, by = c('Over/Under 5.5')]
    setorder(mytab, 'Over/Under 5.5')
    
    #plot
    highchart() %>%
      hc_title(text = 'Over/Under 5.5') %>%
      hc_yAxis(title = list(text = 'Matches')) %>%
      hc_xAxis(categories = mytab[, `Over/Under 5.5`]) %>%
      hc_add_series(name = 'Matches', 
                    type = 'column', 
                    data = mytab[, N], 
                    colorByPoint=TRUE) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_legend(enabled = FALSE)
    
  })  

  output$OU_65 <- renderHighchart({
    
    footy_tab <- values$data
    
    FTHT <- switch(input$OUFTHT_overall, 
                   `Full Time` = 'FT', 
                   `Half Time` = 'HT' )
    
    if (FTHT == 'HT') {
     validate(need('HTHG' %in% names(footy_tab), ''))
    }
    
    if (FTHT == 'FT'){
      footy_tab[, total_goals := FTHG+FTAG]
    } else {
      footy_tab[, total_goals := HTHG+HTAG]
    }
    
    footy_tab[, `Over/Under 6.5` := ifelse(total_goals < 6.5,
                                           'Under 6.5',
                                           'Over 6.5')]
    
    mytab <- footy_tab[, .N, by = c('Over/Under 6.5')]
    setorder(mytab, 'Over/Under 6.5')
    
    highchart() %>%
      hc_title(text = 'Over/Under 6.5') %>%
      hc_yAxis(title = list(text = 'Matches')) %>%
      hc_xAxis(categories = mytab[, `Over/Under 6.5`]) %>%
      hc_add_series(name = 'Matches', 
                    type = 'column', 
                    data = mytab[, N], 
                    colorByPoint = TRUE) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_legend(enabled = FALSE)
    
  })  
  
  output$OU_75 <- renderHighchart({
    
    footy_tab <- values$data
    
    FTHT <- switch(input$OUFTHT_overall, 
                   `Full Time` = 'FT', 
                   `Half Time` = 'HT' )
    
    if (FTHT == 'HT') {
     validate(need('HTHG' %in% names(footy_tab), ''))
    }
    
    if (FTHT == 'FT'){
      footy_tab[, total_goals := FTHG + FTAG]
    } else {
      footy_tab[, total_goals := HTHG + HTAG]
    }
    
    footy_tab[, `Over/Under 7.5` := ifelse(total_goals < 7.5,
                                           'Under 7.5',
                                           'Over 7.5')]
    
    mytab <- footy_tab[, .N, by = c('Over/Under 7.5')]
    setorder(mytab, 'Over/Under 7.5')
    
    highchart() %>%
      hc_title(text = 'Over/Under 7.5') %>%
      hc_yAxis(title = list(text = 'Matches')) %>%
      hc_xAxis(categories = mytab[, `Over/Under 7.5`]) %>%
      hc_add_series(name = 'Matches', 
                    type = 'column', 
                    data = mytab[, N], 
                    colorByPoint = TRUE) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_legend(enabled = FALSE)
    
  })  

  #CARDS AND FOULS TAB----------------------------------------------------------
  #cards and fouls per team aggregated------------------------------------------
  output$cards_fouls_team <- renderHighchart({
    
    footy_tab <- values$data
    
    validate(need('HY' %in% names(footy_tab), 
                  'Cards and Fouls are not Available \nfor this league'))
    
    #home cards
    footy_home_cards <- footy_tab[, list(`Yellow Cards` = sum(HY),
                                         `Red Cards` = sum(HR),
                                         `Fouls` = sum(HF),
                                         `No of Games` = .N),
                                  by = 'HomeTeam']
    setnames(footy_home_cards, 'HomeTeam', 'Team')
    
    #away cards
    footy_away_cards <- footy_tab[, list(`Yellow Cards` = sum(AY),
                                         `Red Cards` = sum(AR),
                                         `Fouls` = sum(AF),
                                         `No of Games` = .N),
                                  by = 'AwayTeam']
    setnames(footy_away_cards, 'AwayTeam', 'Team')
    
    if (input$cards_fouls_HA == 'Home') {
      footy_all_cards <- footy_home_cards 
    } else if (input$cards_fouls_HA == 'Away') {
      footy_all_cards <- footy_away_cards
    } else if (input$cards_fouls_HA == 'All') {
      footy_all_cards <- rbindlist(list(footy_home_cards, 
                                        footy_away_cards))
      footy_all_cards <- footy_all_cards[, 
                                         list(`Yellow Cards` = sum(`Yellow Cards`),
                                              `Red Cards`    = sum(`Red Cards`),
                                              `Fouls`        = sum(Fouls),
                                              `No of Games`  = sum(`No of Games`)),
                                         by='Team']
    }
    
    #order data set according to Team
    setorder(footy_all_cards, Team)
    
    #plotting
    if (input$cards_fouls_total == 'Total') {
      highchart() %>%
        hc_title(text = "Cards and Fouls per Team") %>%
        hc_xAxis(categories = footy_all_cards[, Team]) %>%
        hc_yAxis(
          list(
            title = list(text = "Cards"),
            align = "left"
          ),
          list(
            title = list(text = "Fouls"),
            align = "right",
            opposite = TRUE
          )
        ) %>%
        hc_add_series(name = 'Yellow Cards', 
                      type = 'column', 
                      data = footy_all_cards[, `Yellow Cards`]) %>%
        hc_add_series(name = 'Red Cards', 
                      type = 'column', 
                      data = footy_all_cards[, `Red Cards`]) %>%
        hc_add_series(name = 'Fouls', 
                      type = 'spline', 
                      data = footy_all_cards[, Fouls], 
                      yAxis=1) %>%
        hc_colors(c('#cccc00', '#cc0000', '#404040')) %>%
        hc_tooltip(crosshairs = TRUE, shared=TRUE)
    } else if (input$cards_fouls_total == 'Per Game') {
      highchart() %>%
        hc_title(text = "Cards and Fouls per Team") %>%
        hc_xAxis(categories = footy_all_cards[, Team]) %>%
        hc_yAxis(
          list(
            title = list(text = "Cards"),
            align = "left"
          ),
          list(
            title = list(text = "Fouls"),
            align = "right",
            opposite = TRUE
          )
        ) %>%
        hc_add_series(name = 'Yellow Cards', 
                      type = 'column', 
                      data = footy_all_cards[, `Yellow Cards`] / 
                       footy_all_cards[, `No of Games`]) %>%
        hc_add_series(name = 'Red Cards',
                      type = 'column', 
                      data = footy_all_cards[, `Red Cards`] /
                       footy_all_cards[, `No of Games`]) %>%
        hc_add_series(name = 'Fouls',
                      type = 'spline', 
                      data = footy_all_cards[, Fouls] /
                       footy_all_cards[, `No of Games`], 
                      yAxis = 1)   %>%
        hc_colors(c('#cccc00', '#cc0000', '#404040')) %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE)
      }
    
  })

  #cards over time per team-----------------------------------------------------
  output$cards_over_time <- renderHighchart({
    
    footy_tab <- values$data
    
    #validate that HY is in the names
    validate(need('HY' %in% names(footy_tab), 
                  'Cards and Fouls are not Available \nfor this league'))
    
    #select team and columns
    team_individual <- footy_tab[HomeTeam == input$cards_team | 
                                   AwayTeam == input$cards_team, 
                                 list(Date, 
                                      HomeTeam, 
                                      AwayTeam, 
                                      HY, 
                                      AY, 
                                      HR, 
                                      AR, 
                                      HF, 
                                      AF)]
    
    #melt per home and away
    team_individual_m  <- melt(team_individual, 
                               measure.vars=c('HomeTeam', 'AwayTeam'))
    team_individual_m2 <- team_individual_m[value == input$cards_team,]
    setorder(team_individual_m2, Date)
    team_individual_m3 <- team_individual_m[value != input$cards_team,]
    setorder(team_individual_m3, Date)
    
    if (input$cards_HA == 'Home') {
      team_individual_m2 <- 
        team_individual_m2[variable == 'HomeTeam', 
                           list(Date, 
                                `Yellow Cards` = HY, 
                                `Red Cards` = HR, 
                                Fouls = HF)]
      team_individual_m3 <-
        team_individual_m3[variable == 'AwayTeam', list(Date, value)]
    } else if (input$cards_HA == 'Away') {
      team_individual_m2 <- 
        team_individual_m2[variable == 'AwayTeam', 
                           list(Date, 
                                `Yellow Cards` = AY, 
                                `Red Cards` = AR, 
                                Fouls = AF)]
      team_individual_m3 <-
        team_individual_m3[variable == 'HomeTeam', list(Date, value)]
    } else {
      team_individual_m2 <- 
        team_individual_m2[, `Yellow Cards` := ifelse(variable == 'HomeTeam', 
                                                      HY, 
                                                      AY)][, 
                             `Red Cards`    := ifelse(variable == 'HomeTeam', 
                                                      HR, 
                                                      AR)][,
                             Fouls          := ifelse(variable == 'HomeTeam', 
                                                      HF,  
                                                      AF)]
    }
    
    #plot
    highchart() %>%
      hc_title(text = paste("Cards / Fouls -", input$cards_team)) %>%
      hc_xAxis(list(categories = paste(team_individual_m3$value, 
                                       format(team_individual_m3$Date, '%d/%m'),
                                       sep = ' '))) %>%
      hc_yAxis(
        list(
          title = list(text = "Cards"),
          align = "left"
        ),
        list(
          title = list(text = "Fouls"),
          align = "right",
          opposite = TRUE
        )
      ) %>%
      hc_add_series(name = 'Yellow Cards', 
                    type = 'column', 
                    data = team_individual_m2[, `Yellow Cards`]) %>%
      hc_add_series(name = 'Red Cards', 
                    type = 'column', 
                    data = team_individual_m2[, `Red Cards`]) %>%
      hc_add_series(name = 'Fouls',
                    type = 'spline', 
                    data = team_individual_m2[, Fouls],
                    yAxis = 1) %>%
      hc_colors(c('#cccc00', '#cc0000', '#404040')) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE)
    
  })

  #referees chart---------------------------------------------------------------
  output$referees <- renderHighchart({
    
    footy_tab <- values$data
    
    #validate that referees column exists
    validate(need('Referee' %in% names(footy_tab), 
                  'Referee information is not available \nfor this dataset'))
    
    footy_tab[, `Yellow Cards` := HY + AY][, `Red Cards` := HR + AR]
    refs <- footy_tab[, list(`Yellow Cards` = sum(`Yellow Cards`), 
                             `Red Cards` = sum(`Red Cards`), 
                             `No of Games` = .N), 
                      by = 'Referee']
    setorder(refs, -`Yellow Cards`)
    
    if (input$abso_ave == 'Total') {
     #plot
      highchart() %>%
        hc_title(text = 'Referees') %>%
        hc_xAxis(categories = refs[, Referee]) %>%
        hc_add_series(name = 'Yellow Cards', 
                      type = 'bar', 
                      data = refs[, `Yellow Cards`]) %>%
        hc_add_series(name = 'Red Cards', 
                      type = 'bar', 
                      data = refs[, `Red Cards`]) %>%
        hc_add_series(name = 'No of Games', 
                      type = 'bar', 
                      data = refs[, `No of Games`]) %>%
        hc_colors(c('#cccc00', '#cc0000', '#404040')) %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE)
    } else {
     #plot
      highchart() %>%
        hc_title(text = 'Referees') %>%
        hc_xAxis(categories = refs[, Referee]) %>%
        hc_add_series(name = 'Yellow Cards', 
                      type = 'bar', 
                      data = round(refs[, `Yellow Cards`] / 
                                     refs[, `No of Games`], 
                                   2)) %>%
        hc_add_series(name = 'Red Cards',
                      type = 'bar', 
                      data = round(refs[, `Red Cards`] / 
                                     refs[, `No of Games`],
                                   2)) %>%
        hc_colors(c('#cccc00', '#cc0000')) %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE) 
        
    }
    
  })  


  #total cards and fouls in the league------------------------------------------
  output$total_cards_fouls <- renderHighchart({
    
    footy_tab <- values$data
    
    validate(need('HY' %in% names(footy_tab), 
                  'Cards and Fouls are not Available \nfor this league'))
    
    footy_tab[, `Yellow Cards` := HY + AY][,
                `Red Cards` := HR + AR][,
                Fouls := HF + AF]
    all_cards <- footy_tab[, list(`Yellow Cards` = sum(`Yellow Cards`), 
                                  `Red Cards` = sum(`Red Cards`), 
                                  Fouls = sum(Fouls))]
    
    no_of_games <- footy_tab[, .N]
    
    all_cards    <- as.data.table(t(all_cards), keep.rownames = TRUE)
    all_cards$V2 <- all_cards$V1 / no_of_games
    
    #plot
    highchart() %>%
      hc_title(text = 'Total Cards / Fouls') %>%
      hc_xAxis(categories = all_cards[, rn] ) %>%
      hc_add_series(name = 'Value', 
                    type = 'column', 
                    data = if(input$abso_ave == 'Total') {
                             all_cards[, V1]
                           } else {
                             all_cards[, V2]
                           }, 
                    colorByPoint = TRUE) %>%
      hc_colors(c('#cccc00', '#cc0000', '#404040')) %>%
      hc_legend(enabled = FALSE)%>%
      hc_tooltip(followPointer = TRUE, 
                 pointFormat = '<span style="color:{series.color}">
                                {series.name}</span>:{point.y:.2f}')
      
    
  })  

  #CORNERS TAB------------------------------------------------------------------
  #corners per team aggregated--------------------------------------------------
  output$corners_per_team <- renderHighchart({
    
    footy_tab  <- values$data
    
    #validate that HC is in the column names
    validate(need('HC' %in% names(footy_tab), 
                  'Corner infomation is not Available \nfor this league'))
    
    #create over under part
    if (input$abso_OU == 'Over/Under') {
      HA   <- switch(input$corner_ha, 
                     Home = 'HomeTeam', 
                     Away = 'AwayTeam', 
                     All = 'All')
      footy_tab[, total_corners := HC + AC]
      footy_tab[,`O/U 5.5` := ifelse(total_corners < 5.5, 
                                              'Under  5.5', 
                                              'Over  5.5')][,
                 `O/U 8.5` := ifelse(total_corners < 8.5, 
                                              'Under  8.5', 
                                              'Over  8.5')][,
                 `O/U 10.5` := ifelse(total_corners < 10.5, 
                                              'Under 10.5', 
                                              'Over 10.5')][,
                 `O/U 13.5` := ifelse(total_corners < 13.5, 
                                              'Under 13.5', 
                                              'Over 13.5')]
      if (HA %in% c('HomeTeam', 'AwayTeam')) {
        footy_result <- footy_tab[, .N, by = c(HA, input$cornerOU)]
        footy_result_dcast <- dcast(footy_result, 
                                    as.formula(paste0(HA, 
                                                      '~ `', 
                                                      input$cornerOU, '`')), 
                                    value.var = 'N')
        setnames(footy_result_dcast, HA, 'Team')
        footy_result_dcast <- na_converter(footy_result_dcast)
        footy_all_result   <- footy_result_dcast
      } else {
        footy_home_result <- footy_tab[, .N, by = c('HomeTeam', input$cornerOU)]
        footy_home_result_dcast <- dcast(footy_home_result, 
                                         as.formula(paste0('HomeTeam ~ `',
                                                           input$cornerOU, '`')),
                                         value.var = 'N')
        setnames(footy_home_result_dcast, 'HomeTeam', 'Team')
        footy_home_result_dcast <- na_converter(footy_home_result_dcast)
        
        footy_away_result <- footy_tab[, .N, by = c('AwayTeam', input$cornerOU)]
        footy_away_result_dcast <- dcast(footy_away_result, 
                                         as.formula(paste0('AwayTeam ~ `',
                                                           input$cornerOU, '`')),
                                         value.var = 'N')
        setnames(footy_away_result_dcast, 'AwayTeam', 'Team')
        footy_away_result_dcast <- na_converter(footy_away_result_dcast)
        
        footy_all_result <- rbindlist(list(footy_home_result_dcast, 
                                           footy_away_result_dcast), 
                                      use.names=TRUE)
        footy_all_result <- footy_all_result[, lapply(.SD, sum), by = 'Team']
      }
      
      #get over and under names
      Over  <- names(footy_all_result)[like(names(footy_all_result), 'Over*')]
      Under <- names(footy_all_result)[like(names(footy_all_result), 'Under*')]
      
      #set order to data set according to Team
      setorder(footy_all_result, Team)
      
      #plot
      highchart() %>%
        hc_title(text = 'Corners per Team') %>%
        hc_yAxis(list(title = list(text = "No of Games"))) %>%
        hc_xAxis(categories=footy_all_result[, Team]) %>%
        hc_add_series(name = Over , 
                      type = 'column', 
                      data = unname(unlist(footy_all_result[, 
                                                            Over , 
                                                            with = FALSE]))) %>%
        hc_add_series(name = Under, 
                      type = 'column', 
                      data = unname(unlist(footy_all_result[, 
                                                            Under, 
                                                            with = FALSE]))) %>%
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE)
    } else {
      HA <- switch(input$corner_ha, 
                   Home = 'HomeTeam', 
                   Away = 'AwayTeam', 
                   All = 'All')
      if (HA == 'HomeTeam') {
        footy_result <- footy_tab[, list(Corners = sum(HC), 
                                         `No of Games` = .N), 
                                  by=HA]
        setnames(footy_result, HA, 'Team')  
      } else if (HA == 'AwayTeam') {
        footy_result <- footy_tab[, list(Corners = sum(AC), 
                                         `No of Games` = .N), 
                                  by=HA]
        setnames(footy_result, HA, 'Team')
      } else {  
        footy_home_result <- footy_tab[, list(Corners = sum(HC), 
                                              `No of Games` = .N), 
                                       by='HomeTeam']
        setnames(footy_home_result, 'HomeTeam', 'Team')  
        footy_away_result <- footy_tab[, list(Corners = sum(AC), 
                                              `No of Games` = .N), 
                                       by='AwayTeam']
        setnames(footy_away_result, 'AwayTeam', 'Team')  
        
        footy_result <- rbindlist(list(footy_home_result, 
                                       footy_away_result), use.names = TRUE)
        footy_result <- footy_result[, lapply(.SD, sum), by = 'Team']
      }
      
      #set order to data set according to Team
      setorder(footy_result, Team)
      
      if(input$abso_OU == 'Total') {
        highchart() %>%
          hc_title(text = 'Corners per Team') %>%
          hc_yAxis(list(title = list(text = "Corners"))) %>%
          hc_xAxis(categories=footy_result[, Team])   %>%
          hc_add_series(name = 'Corners' , 
                        type = 'column', 
                        data = footy_result[, Corners]) %>%
          hc_legend(enabled = FALSE) %>%
          hc_tooltip(shared=TRUE, crosshairs=TRUE)
      }else{
        highchart() %>%
          hc_title(text = 'Corners per Team') %>%
          hc_yAxis(list(title = list(text = "Corners"))) %>%
          hc_xAxis(categories=footy_result[, Team])   %>%
          hc_add_series(name = 'Corners' , 
                        type = 'column', 
                        data = round(footy_result[, Corners] / 
                                       footy_result[, `No of Games`], 
                                     2) ) %>%
          hc_legend(enabled = FALSE) %>%
          hc_tooltip(shared = TRUE, crosshairs = TRUE)
      }
      
    }
    
  })
  
  #corners per team over time---------------------------------------------------
  output$corners_over_time <- renderHighchart({
    
    footy_tab <- values$data
    
    #validate that HC is available in the data
    validate(need('HC' %in% names(footy_tab), 
                  'Corner infomation is not Available \nfor this league'))
    
    team_individual <- footy_tab[HomeTeam == input$corner_team |
                                   AwayTeam == input$corner_team, 
                                 list(Date, HomeTeam, AwayTeam, HC, AC)]
    
    #melt data set to get home and away
    team_individual_m <- melt(team_individual, 
                               measure.vars=c('HomeTeam', 'AwayTeam'))
    team_individual_m2 <- team_individual_m[value == input$corner_team,]
    setorder(team_individual_m2, Date)
    team_individual_m3 <- team_individual_m[value != input$corner_team,]
    setorder(team_individual_m3, Date)
    if (input$corner_HA_overtime == 'Home') {
      team_individual_m2 <- 
        team_individual_m2[variable == 'HomeTeam', list(Date, Corners = HC)]
      team_individual_m3 <-
        team_individual_m3[variable == 'AwayTeam', list(Date, value)]
    } else if (input$corner_HA_overtime == 'Away'){
      team_individual_m2 <- 
        team_individual_m2[variable == 'AwayTeam', list(Date, Corners = AC)]
      team_individual_m3 <-
        team_individual_m3[variable == 'HomeTeam', list(Date, value)]
    } else {
      team_individual_m2 <- 
        team_individual_m2[, Corners := ifelse(variable == 'HomeTeam', HC, AC)] 
    }
    
    #plot
    highchart() %>%
      hc_title(text = paste('Corners - ', input$corner_team)) %>%
      hc_xAxis(list(categories = paste(team_individual_m3$value, 
                                       format(team_individual_m3$Date, '%d/%m'),
                                       sep=' '))) %>%
      hc_add_series(name  = 'Corners', 
                    type = 'spline', 
                    data = team_individual_m2$Corners) %>%
      hc_legend(enabled = FALSE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE)
    
  })

  #corners over under 5.5 for league--------------------------------------------
  output$corners_55 <- renderHighchart({
    
    footy_tab <- values$data
    
    #validate that HC is in the data
    validate(need('HC' %in% names(footy_tab), 
                  'Corner infomation is not Available \nfor this league'))
    
    footy_tab[, total_corners := HC + AC]
    
    footy_tab[, `Corners O/U 5.5` := ifelse(total_corners < 5.5, 
                                            'Under 5.5', 
                                            'Over 5.5')]
    
    mytab <- footy_tab[, .N, by = c('Corners O/U 5.5')]
    setorder(mytab, 'Corners O/U 5.5')
    
    #plot
    highchart() %>%
      hc_title(text = 'Corners O/U 5.5') %>%
      hc_xAxis(categories = mytab[, `Corners O/U 5.5`]) %>%
      hc_add_series(name = 'Matches', 
                    type = 'column', 
                    data = mytab[, N], 
                    colorByPoint = TRUE) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_legend(enabled = FALSE)
    
  }) 

  #corners over under 8.5 for league--------------------------------------------
  output$corners_85 <- renderHighchart({
    
    footy_tab <- values$data
    
    #validate HC exists in the data
    validate(need('HC' %in% names(footy_tab), ''))
    footy_tab[, total_corners := HC + AC]
    
    footy_tab[, `Corners O/U 8.5` := ifelse(total_corners < 8.5, 
                                            'Under 8.5', 
                                            'Over 8.5')]
    
    mytab <- footy_tab[, .N, by = c('Corners O/U 8.5')]
    setorder(mytab, 'Corners O/U 8.5')
    
    #plot
    highchart() %>%
      hc_title(text = 'Corners O/U 8.5') %>%
      hc_xAxis(categories = mytab[, `Corners O/U 8.5`]) %>%
      hc_add_series(name = 'Matches', 
                    type = 'column', 
                    data = mytab[, N], 
                    colorByPoint = TRUE) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_legend(enabled = FALSE)
    
  }) 

  #corners over under 10.5 for league-------------------------------------------
  output$corners_105 <- renderHighchart({
    
    footy_tab <- values$data
    
    #validate for HC
    validate(need('HC' %in% names(footy_tab), ''))
    footy_tab[, total_corners := HC + AC]
    
    footy_tab[, `Corners O/U 10.5` := ifelse(total_corners < 10.5, 
                                             'Under 10.5', 
                                             'Over 10.5')]
    
    mytab <- footy_tab[, .N, by = c('Corners O/U 10.5')]
    setorder(mytab, 'Corners O/U 10.5')
    
    #plot
    highchart() %>%
      hc_title(text = 'Corners O/U 10.5') %>%
      hc_xAxis(categories = mytab[, `Corners O/U 10.5`]) %>%
      hc_add_series(name = 'Matches', 
                    type = 'column', 
                    data = mytab[, N], 
                    colorByPoint = TRUE) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_legend(enabled = FALSE)
    
  }) 

  #corners over under 13.5 for league-------------------------------------------
  output$corners_135 <- renderHighchart({
    
    footy_tab <- values$data
    
    #validate for HC
    validate(need('HC' %in% names(footy_tab), ''))
    footy_tab[, total_corners := HC + AC]
    
    footy_tab[, `Corners O/U 13.5` := ifelse(total_corners < 13.5, 
                                             'Under 13.5', 
                                             'Over 13.5')]
    
    mytab <- footy_tab[, .N, by = c('Corners O/U 13.5')]
    setorder(mytab, 'Corners O/U 13.5')
    
    #plot
    highchart() %>%
      hc_title(text = 'Corners O/U 13.5') %>%
      hc_xAxis(categories = mytab[, `Corners O/U 13.5`]) %>%
      hc_add_series(name = 'Matches', 
                    type = 'column', 
                    data = mytab[, N], 
                    colorByPoint = TRUE) %>%
      hc_colors(c(hc_get_colors()[1], '#ff9999')) %>%
      hc_legend(enabled = FALSE)
    
  }) 

})