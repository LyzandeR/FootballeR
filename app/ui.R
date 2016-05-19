shinyUI(navbarPage(title='FootballeR', fluid = FALSE, collapsible = FALSE,
             tabPanel('Welcome', 
                      div(class='bg', 
                          div(class='first_line', strong('FootballeR')),
                          br(),
                          div(class='second_line', 'A Portal to Detailed European Football Statistics'),
                          br(),
                          br(),
                          br(),
                          img(class='divider', src="black-divider-no-background-md.png"),
                          br(),
                          br(),
                          br(),
                          div(id='row_images', 
                              img(class='flags', src="England.png"), 
                              img(class='flags', src="Scotland.png"),
                              img(class='flags', src="Germany.png"),
                              img(class='flags', src="Italy.png"),
                              img(class='flags', src="Spain.png"),
                              img(class='flags', src="France.png"),
                              img(class='flags', src="Netherlands.png"),
                              img(class='flags', src="Belgium.png"),
                              img(class='flags', src="Portugal.png"),
                              img(class='flags', src="Turkey.png"),
                              img(class='flags', src="Greece.png")
                          ),
                          br(),
                          br(),
                          div(class='third_line', 'Click the Application button at the top to begin'),
                          br(),
                          div(class='third_line', 'Or'),
                          div(class='third_line', 'Find out more at:'),
                          a(href="#", id='front_button', class="btn btn-default", role="button",'WIKI')
                      ),
                      img(class='background', src='football_4-wallpaper-1920x1080_new_cropped.jpg'),
                      div(class="footer", "Developed with ", icon('heart'), " in R and Rstudio. For the latest Development and Licence: ",
                          a(id="github_icon", href="https://github.com/LyzandeR/FootballeR", icon('github')))),
             
             
             tabPanel('Application',
  tags$head(includeCSS('www/football.css')),
  tags$head(tags$script(src="floating_sidebar.js")),
  
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
               br(),
               fixedRow(tableOutput('league_table')
      )),
      tabPanel('Goals', 
               br(),
               fixedRow(
                  column(highchartOutput('goals_per_team' , height='450px'), width=10),
                  column(br(), br(),selectInput('shots_HA', 'Select Home/Away', choices=c('Home', 'Away', 'All'), selected='Home'),
                         selectInput('shots_total_perteam', 'Select Total / Per Team'    ,
                                     choices=c('Total', 'Per Game'), selected='Total'), width=2)),
               br(),
               fixedRow(
                  column(highchartOutput('goals_over_time', height='450px'), width=10),
                  column(br(), br(),selectInput('team', 'Select Team',      choices=c('Arsenal', 'Chelsea'),  selected='Arsenal'),
                         selectInput(  'HA', 'Select Home/Away', choices=c('Home', 'Away', 'All'), selected='Home'   ), width=2)),
               br(),
               fixedRow(
                  column(highchartOutput('goals_total', height = '350px'), width=6),
                  column(tags$div(class="icon", id="orange", h1("Average League Shots per Goal"), h1(textOutput("rate"))), width=4)     
      )),
      tabPanel('Results',
               br(),
               fixedRow(
                  column(highchartOutput('team_results', height='450px'), width=10),
                  column(br(), br(),selectInput('results_ha', 'Select Home/Away', choices=c('Home', 'Away', 'All')   , selected='Home'),
                         selectInput('FTHT'      , 'Select FT/HT'    , choices=c('Full Time', 'Half Time'), selected='Full TIme'),
                         width=2)),
               br(),
               fixedRow(
                  column(highchartOutput('result_over_time', height='450px'), width=10),
                  column(br(), br(),selectInput('result_team', 'Select Team'     , choices=c('Arsenal', 'Chelsea')    , selected='Arsenal'),
                         selectInput('result_HA'  , 'Select Home/Away', choices=c('Home', 'Away', 'All')   , selected='Home'),
                         selectInput('result_FTHT', 'Select FT/HT'    , choices=c('Full Time', 'Half Time'), selected='Full TIme'),
                         width=2)),
               br(),
               fixedRow(
                 column(highchartOutput('total_results_FT', height='350px'), width=5),
                 column(highchartOutput('total_results_HT', height='350px'), width=5),
                 column(br(), br(),radioButtons('results_percent', 'Absolute values or percentages', choices=c('Absolute', 'Percentages'), 
                                                selected='Absolute'),
                        width=2)
      )),  
      tabPanel('Over / Under',
               br(),
               fixedRow(
                 column(highchartOutput('team_OU', height='450px'), width=10),
                 column(br(), br(), selectInput('ou_ha'  , 'Select Home/Away' , choices=c('Home', 'Away', 'All')   , selected='Home'),
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
               br(),
               fixedRow(
                 column(highchartOutput('OU_over_time', height='450px'), width=10),
                 column(br(), br(), selectInput('OU_team', 'Select Team'     , choices=c('Arsenal', 'Chelsea')    , selected='Arsenal'),
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
                  div(class="image", highchartOutput("OU_05", height="300px", width="320px")),
                  div(class="image", highchartOutput("OU_15", height="300px", width="320px")),
                  div(class="image", highchartOutput("OU_25", height="300px", width="320px")),
                  div(class="custom_slider", 
                      br(), br(), selectInput('OUFTHT_overall', 'Select FT/HT', choices=c('Full Time', 'Half Time'), selected='Full TIme')),
                  div(style="clear:both")),
                div(id = 'parent',
                  div(class="image", highchartOutput("OU_35", height="300px", width="320px")),
                  div(class="image", style="width:320px;background:white;height:300px;"),
                  div(class="image", highchartOutput("OU_45", height="300px", width="320px")),
                  div(style="clear:both")),
                div(id = 'parent',
                  div(class="image", highchartOutput("OU_55", height="300px", width="320px")),
                  div(class="image", highchartOutput("OU_65", height="300px", width="320px")),
                  div(class="image", highchartOutput("OU_75", height="300px", width="320px")),
                  div(style="clear:both"))
      ),
      tabPanel('Cards / Fouls',
               br(),
               fixedRow(
                 column(highchartOutput('cards_fouls_team', height='450px'), width=10),
                 column(br(), br(),selectInput('cards_fouls_HA', 'Select Home/Away', choices=c('Home', 'Away', 'All'), selected='Home'),
                        selectInput('cards_fouls_total', 'Select Total / Per Team'    ,
                                    choices=c('Total', 'Per Game'), selected='Total'), width=2)),
               br(),
               fixedRow(
                 column(highchartOutput('cards_over_time', height='450px'), width=10),
                 column(br(),br(),selectInput('cards_team', 'Select Team'      , choices=c('Arsenal', 'Chelsea')    , selected='Arsenal'),
                        selectInput('cards_HA'  , 'Select Home/Away' , choices=c('Home', 'Away', 'All'), selected='Home'),
                        width=2)),
               br(),
               fixedRow(
                 column(highchartOutput('referees', height='500px'), width=5),
                 column(highchartOutput('total_cards_fouls', height='450px'), width=5),
                 column(br(),br(),radioButtons('abso_ave', 'Absolute or Average', choices=c('Absolute', 'Average'), selected='Average'), width=2)
      )),
      tabPanel('Corners',
               br(),
               fixedRow(
                 column(highchartOutput('corners_per_team', height='450px'), width=10),
                 column(br(),br(),selectInput ('corner_ha', 'Select Home/Away'  , choices=c('Home', 'Away', 'All'), selected='Home'),
                        radioButtons('abso_OU'  , 'Total - Over/Under', choices=c('Total', 'Per Game', 'Over/Under'), selected='Total'),
                        conditionalPanel(condition="input.abso_OU=='Over/Under'",
                        selectInput('cornerOU' , 'Select Over/Under', choices=c('Corners O/U  5.5',
                                                                                'Corners O/U  8.5',
                                                                                'Corners O/U 10.5',
                                                                                'Corners O/U 13.5'), selected='Over/Under 5.5')),
                        helpText('Note: Over/Under is based on total corners in a game'),
                        width=2)),
               br(),
               fixedRow(
                 column(highchartOutput('corners_over_time', height='450px'), width=10),
                 column(br(),br(),selectInput('corner_team', 'Select Team'     , choices=c('Arsenal', 'Chelsea'), selected='Arsenal'),
                        selectInput('corner_HA_overtime'  , 'Select Home/Away' , choices=c('Home', 'Away', 'All'), selected='Home'),
                        width=2)),
               fixedRow(
                 br(),
                 column(highchartOutput('corners_55', height='300px'), width=5),
                 column(highchartOutput('corners_85', height='300px'), width=5)),
               fixedRow(
                 column(highchartOutput('corners_105', height='300px'), width=5),
                 column(highchartOutput('corners_135', height='300px'), width=5)
      )),
      tabPanel('Raw Table',
               br(),
               fixedRow(
                 column(DT::dataTableOutput('raw', width='100%'), width=10),
                 br(),br(),column(downloadButton('downloadData', 'Download Data'), width=2)
      ))
    ),    
  width=9)
  )
)))



