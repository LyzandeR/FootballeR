shinyUI(fixedPage(title='FootballeR',
  #top spacer
  br(),
  tags$head(includeCSS('www/football.css')),
  tags$head(tags$script(src="floating_sidebar.js")),
  tags$head(tags$script(src="table_highlight.js")),
  fixedRow(
    column(width=11, h1("FootballeR")),
    div(style = "right:250px;position:absolute;", column(width=1 , img(src="sport-1019941__180.jpg", height = 80, width = 200)))
  ),
  fixedRow(column(width=12, hr())),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput('country', 'Select Country', choices=c('England', 'Scotland', 'Germany', 'Italy', 'Spain',
                                                         'France', 'Netherlands', 'Belgium', 'Portugal', 'Turkey',
                                                         'Greece'), selected='England'),
      
      selectInput('division', "Select Division", choices=c('Premier League', 'Championship', 'League 1', 'League 2', 'Conference'),
                  selected='Premier League'),
      
      selectInput('season', 'Select Season', choices=rev(paste(1993:2016, shift(1993:2016, type='lead'), sep='-'))[-1], 
                  selected=c('2015-2016')),
      
      sliderInput('date_range', "Select the Date Range", value=c(as.IDate('2015-08-01'), as.IDate('2016-06-01')),
                  min=as.IDate('2015-08-01'), max=as.IDate('2016-06-01')),
      
      width=3),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel('League Table',
               fixedRow(tableOutput('league_table')
      )),
      tabPanel('Goals', 
               fixedRow(
                  column(highchartOutput('goals_per_team' , height='600px'), width=12)),
               fixedRow(
                  column(highchartOutput('goals_over_time', height='600px'), width=10),
                  column(selectInput('team', 'Select Team',      choices=c('Arsenal', 'Chelsea'),  selected='Arsenal'),
                         selectInput(  'HA', 'Select Home/Away', choices=c('Home', 'Away', 'All'), selected='Home'   ), width=2)),
               fixedRow(
                  column(highchartOutput('goals_total', height = '370px'), width=6),
                  column(tags$div(class="icon", id="orange", h1("Average League Shots per Goal"), h1(textOutput("rate"))), width=4)     
      )),
      tabPanel('Results', 
               fixedRow(
                  column(highchartOutput('team_results', height='600px'), width=10),
                  column(selectInput('results_ha', 'Select Home/Away', choices=c('Home', 'Away', 'All')   , selected='Home'),
                         selectInput('FTHT'      , 'Select FT/HT'    , choices=c('Full Time', 'Half Time'), selected='Full TIme'),
                         width=2)),
               fixedRow(
                  column(highchartOutput('result_over_time', height='600px'), width=10),
                  column(selectInput('result_team', 'Select Team'     , choices=c('Arsenal', 'Chelsea')    , selected='Arsenal'),
                         selectInput('result_HA'  , 'Select Home/Away', choices=c('Home', 'Away', 'All')   , selected='Home'),
                         selectInput('result_FTHT', 'Select FT/HT'    , choices=c('Full Time', 'Half Time'), selected='Full TIme'),
                         width=2)),
               fixedRow(
                 column(highchartOutput('total_results_FT', height='400px'), width=5),
                 column(highchartOutput('total_results_HT', height='400px'), width=5),
                 column(radioButtons('results_percent', 'Absolute values or percentages', choices=c('Absolute', 'Percentages'), selected='Absolute'),
                        width=2)
      )),  
      tabPanel('Over / Under', 
               fixedRow(
                 column(highchartOutput('team_OU', height='600px'), width=10),
                 column(selectInput('ou_ha'  , 'Select Home/Away' , choices=c('Home', 'Away', 'All')   , selected='Home'),
                        selectInput('ou_FTHT', 'Select FT/HT'     , choices=c('Full Time', 'Half Time'), selected='Full TIme'),
                        selectInput('OU'     , 'Select Over/Under', choices=c('Over/Under 0.5',
                                                                              'Over/Under 1.5',
                                                                              'Over/Under 2.5',
                                                                              'Over/Under 3.5',
                                                                              'Over/Under 4.5',
                                                                              'Over/Under 5.5',
                                                                              'Over/Under 6.5',
                                                                              'Over/Under 7.5'), selected='Over/Under 2.5'),
                        width=2)
               ),
               fixedRow(
                 column(highchartOutput('OU_over_time', height='600px'), width=10),
                 column(selectInput('OU_team', 'Select Team'     , choices=c('Arsenal', 'Chelsea')    , selected='Arsenal'),
                        selectInput('OU_overtime_HA'  , 'Select Home/Away' , choices=c('Home', 'Away', 'All')   , selected='Home'),
                        selectInput('OUFTHT_over_time', 'Select FT/HT'     , choices=c('Full Time', 'Half Time'), selected='Full TIme'),
                        selectInput('OU_overtime'     , 'Select Over/Under', choices=c('Over/Under 0.5',
                                                                                       'Over/Under 1.5',
                                                                                       'Over/Under 2.5',
                                                                                       'Over/Under 3.5',
                                                                                       'Over/Under 4.5',
                                                                                       'Over/Under 5.5',
                                                                                       'Over/Under 6.5',
                                                                                       'Over/Under 7.5'), selected='Over/Under 2.5'),
                        width=2)
               ),
                div(id = 'parent',
                  div(class="image", highchartOutput("OU_05", height="400px", width="320px")),
                  div(class="image", highchartOutput("OU_15", height="400px", width="320px")),
                  div(class="image", highchartOutput("OU_25", height="400px", width="320px")),
                  div(class="custom_slider", 
                      selectInput('OUFTHT_overall', 'Select FT/HT', choices=c('Full Time', 'Half Time'), selected='Full TIme')),
                  div(style="clear:both")),
                div(id = 'parent',
                  div(class="image", highchartOutput("OU_35", height="400px", width="320px")),
                  div(class="image", img(src="sport-1019941__180.jpg", height = 250, width = 320)),
                  div(class="image", highchartOutput("OU_45", height="400px", width="320px")),
                  div(style="clear:both")),
                div(id = 'parent',
                  div(class="image", highchartOutput("OU_55", height="400px", width="320px")),
                  div(class="image", highchartOutput("OU_65", height="400px", width="320px")),
                  div(class="image", highchartOutput("OU_75", height="400px", width="320px")),
                  div(style="clear:both"))
      ),
      tabPanel('Cards / Fouls', 
               fixedRow(
                 column(highchartOutput('cards_fouls_team', height='600px'), width=10)),
               fixedRow(
                 column(highchartOutput('cards_over_time', height='600px'), width=10),
                 column(selectInput('cards_team', 'Select Team'      , choices=c('Arsenal', 'Chelsea')    , selected='Arsenal'),
                        selectInput('cards_HA'  , 'Select Home/Away' , choices=c('Home', 'Away', 'All'), selected='Home'),
                        width=2)),
               fixedRow(
                 column(highchartOutput('referees', height='500px'), width=5),
                 column(highchartOutput('total_cards_fouls', height='500px'), width=5),
                 column(radioButtons('abso_ave', 'Absolute or Average', choices=c('Absolute', 'Average'), selected='Average'), width=2)
      )),
      tabPanel('Corners',
               fixedRow(
                 column(highchartOutput('corners_per_team', height='600px'), width=10),
                 column(selectInput ('corner_ha', 'Select Home/Away'  , choices=c('Home', 'Away', 'All'), selected='Home'),
                        radioButtons('abso_OU'  , 'Total - Over/Under', choices=c('Total', 'Per Game', 'Over/Under'), selected='Total'),
                        conditionalPanel(condition="input.abso_OU=='Over/Under'",
                        selectInput('cornerOU' , 'Select Over/Under', choices=c('Corners O/U  5.5',
                                                                                'Corners O/U  8.5',
                                                                                'Corners O/U 10.5',
                                                                                'Corners O/U 13.5'), selected='Over/Under 5.5')),
                        width=2)),
               fixedRow(
                 column(highchartOutput('corners_over_time', height='600px'), width=10),
                 column(selectInput('corner_team', 'Select Team'     , choices=c('Arsenal', 'Chelsea'), selected='Arsenal'),
                        selectInput('corner_HA_overtime'  , 'Select Home/Away' , choices=c('Home', 'Away', 'All'), selected='Home'),
                        width=2)),
               fixedRow(
                 column(highchartOutput('corners_55', height='500px'), width=5),
                 column(highchartOutput('corners_85', height='500px'), width=5)),
               fixedRow(
                 column(highchartOutput('corners_105', height='500px'), width=5),
                 column(highchartOutput('corners_135', height='500px'), width=5)
      )),
      tabPanel('Raw Table', 
               fixedRow(
                 DT::dataTableOutput('raw')       
      ))
    ),    
  width=9)
  )
))



