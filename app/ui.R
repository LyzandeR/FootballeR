shinyUI(
  navbarPage(
    title = 'FootballeR',
    fluid = FALSE,
    collapsible = FALSE,
    #tab welcome top of app
    tabPanel('Welcome', 
          div(class = 'bg', 
              div(class = 'first_line', strong('FootballeR')),
              br(),
              div(class = 'second_line', 
                  'A Portal to Detailed European Football Statistics'),
              br(),
              br(),
              br(),
              img(class = 'divider', 
                  src = "black-divider-no-background-md.png"),
              br(),
              br(),
              br(),
              div(id = 'row_images', 
                  img(class = 'flags', src = "England.png"), 
                  img(class = 'flags', src = "Scotland.png"),
                  img(class = 'flags', src = "Germany.png"),
                  img(class = 'flags', src = "Italy.png"),
                  img(class = 'flags', src = "Spain.png"),
                  img(class = 'flags', src = "France.png"),
                  img(class = 'flags', src = "Netherlands.png"),
                  img(class = 'flags', src = "Belgium.png"),
                  img(class = 'flags', src = "Portugal.png"),
                  img(class = 'flags', src = "Turkey.png"),
                  img(class = 'flags', src = "Greece.png")
              ),
              br(),
              br(),
              div(class = 'third_line', 
                  'Click the Application button at the top to begin'),
              br(),
              div(class = 'third_line', 'Or'),
              div(class = 'third_line', 'Find out more at:'),
              a(href='https://github.com/LyzandeR/FootballeR',
                id='front_button', 
                class="btn btn-default", 
                role="button",
                'WIKI')
          ),
          img(class = 'background', 
              src = 'football_4-wallpaper-1920x1080_new_cropped.jpg')

    ),
     
    #tab Application top of app
    tabPanel(
      'Application',
      tags$head(includeCSS('www/football.css')),
      tags$head(tags$script(src="floating_sidebar.js")),
      sidebarLayout(
       
        #sidebar starts here
        sidebarPanel(
         
          selectInput('country', 
                      'Select Country', 
                      choices = c('England', 
                                  'Scotland', 
                                  'Germany', 
                                  'Italy', 
                                  'Spain',
                                  'France', 
                                  'Netherlands', 
                                  'Belgium', 
                                  'Portugal', 
                                  'Turkey',
                                  'Greece'),
                      selected = 'England'),
          
          selectInput('division', 
                      'Select Division', 
                      choices = c('Premier League', 
                                  'Championship', 
                                  'League 1', 
                                  'League 2', 
                                  'Conference'),
                      selected = 'Premier League'),
          
          selectInput('season',
                      'Select Season',
                      choices = rev(paste(1993:2017, shift(1993:2017, type='lead'), 
                                          sep = '-'))[-1], 
                      selected = c('2016-2017')),
          
          sliderInput('date_range',
                      'Select the Date Range', 
                      value = c(as.IDate('2016-08-01'), as.IDate('2017-06-30')),
                      min = as.IDate('2016-08-01'), 
                      max = as.IDate('2017-06-30')),
          
          width = 3
        ),
         
        #main panel starts here
        mainPanel(
         
          tabsetPanel(
           
            #tab league table in Application tab
            tabPanel('League Table',
                     br(),
                     fixedRow(
                      tableHTML_output('league_table', height = '800px')
                     )),
            
            #tab Goals in Application tab
            tabPanel('Goals', 
                     br(),
                     fixedRow(
                       column(highchartOutput('goals_per_team'), 
                              width=10),
                       column(br(),
                              br(),
                              selectInput('shots_HA', 
                                          'Select Home/Away',
                                          choices = c('Home', 'Away', 'All'),
                                          selected = 'Home'),
                              selectInput('shots_total_perteam', 
                                          'Total / Per Team'    ,
                                          choices = c('Total', 'Per Game'), 
                                          selected='Total'), 
                              width=2)
                       ),
                       br(),
                       fixedRow(
                       column(highchartOutput('goals_over_time', 
                                              height='450px'), 
                              width=10),
                       column(br(), 
                              br(),
                              selectInput('team', 
                                          'Select Team',
                                          choices = init_teams,
                                          selected = 'Arsenal'),
                              selectInput('HA',
                                          'Select Home/Away', 
                                          choices = c('Home', 'Away', 'All'), 
                                          selected = 'Home'),
                              width = 2)
                       ),
                              br(),
                              fixedRow(
                              column(highchartOutput('goals_total',
                                                     height = '350px'),
                                     width = 6),
                              column(tags$div(class = "icon",
                                              id = "orange", 
                                              h1("Average League Shots per Goal"),
                                              h1(textOutput("rate"))), 
                                     width = 4)     
                              )),
             
            #tab Results in Application tab
            tabPanel('Results',
                     br(),
                     fixedRow(
                        column(highchartOutput('team_results', 
                                               height = '450px'), 
                               width = 10),
                        column(br(), 
                               br(), selectInput('results_ha',
                                                 'Select Home/Away', 
                                                 choices = c('Home', 'Away', 'All'),
                                                 selected = 'Home'),
                               selectInput('FTHT',
                                           'Select FT/HT',
                                           choices = c('Full Time', 'Half Time'),
                                           selected = 'Full TIme'),
                               width = 2)
                     ),
                     br(),
                     fixedRow(
                        column(highchartOutput('result_over_time', 
                                               height = '450px'), 
                               width = 10),
                        column(br(),
                               br(),
                               selectInput('result_team',
                                           'Select Team',
                                           choices = init_teams,
                                           selected = 'Arsenal'),
                               selectInput('result_HA',
                                           'Select Home/Away', 
                                           choices = c('Home', 'Away', 'All'),
                                           selected = 'Home'),
                               selectInput('result_FTHT',
                                           'Select FT/HT',
                                           choices = c('Full Time', 'Half Time'),
                                           selected = 'Full TIme'),
                               width=2)
                     ),
                     br(),
                     fixedRow(
                       column(highchartOutput('total_results_FT', 
                                              height = '350px'),
                              width = 5),
                       column(highchartOutput('total_results_HT', 
                                              height = '350px'), 
                              width = 5),
                       column(br(),
                              br(),
                              radioButtons('results_percent', 
                                           'Absolute values or percentages',
                                           choices = c('Absolute', 'Percentages'), 
                                           selected = 'Absolute'),
                              width = 2)
                     )),  
            
            #tab Over Under in Application tab
            tabPanel('Over / Under',
                     br(),
                     fixedRow(
                       column(highchartOutput('team_OU', height='450px'), 
                              width=10),
                       column(br(), 
                              br(), 
                              selectInput('ou_ha',
                                          'Select Home/Away',
                                          choices = c('Home', 'Away', 'All'),
                                          selected = 'Home'),
                              selectInput('ou_FTHT', 
                                          'Select FT/HT',
                                          choices = c('Full Time', 'Half Time'),
                                          selected = 'Full TIme'),
                              selectInput('OU',
                                          'Select Over/Under', 
                                          choices = c('O/U 0.5',
                                                      'O/U 1.5',
                                                      'O/U 2.5',
                                                      'O/U 3.5',
                                                      'O/U 4.5',
                                                      'O/U 5.5',
                                                      'O/U 6.5',
                                                      'O/U 7.5'), 
                                          selected = 'O/U 2.5'),
                              width = 2)
                     ),
                     br(),
                     fixedRow(
                       column(highchartOutput('OU_over_time', height='450px'), 
                              width = 10),
                       column(br(), 
                              br(), 
                              selectInput('OU_team', 
                                          'Select Team',
                                          choices = init_teams,
                                          selected = 'Arsenal'),
                              selectInput('OU_overtime_HA',
                                          'Select Home/Away',
                                          choices = c('Home', 'Away', 'All'),
                                          selected = 'Home'),
                              selectInput('OUFTHT_over_time',
                                          'Select FT/HT',
                                          choices = c('Full Time', 'Half Time'),
                                          selected = 'Full TIme'),
                              selectInput('OU_overtime',
                                          'Select Over/Under', 
                                          choices = c('O/U 0.5',
                                                      'O/U 1.5',
                                                      'O/U 2.5',
                                                      'O/U 3.5',
                                                      'O/U 4.5',
                                                      'O/U 5.5',
                                                      'O/U 6.5',
                                                      'O/U 7.5'), 
                                          selected = 'O/U 2.5'),
                              width = 2)
                     ),
                     br(),
                     div(id = 'parent',
                       div(class = "image", 
                           highchartOutput("OU_05", 
                                           height = "300px", 
                                           width = "270px")),
                       div(class = "image", 
                           highchartOutput("OU_15", 
                                           height = "300px", 
                                           width = "270px")),
                       div(class = "image", 
                           highchartOutput("OU_25", 
                                           height = "300px", 
                                           width = "270px")),
                       div(class = "custom_slider", 
                           br(), 
                           br(),
                           selectInput('OUFTHT_overall', 
                                       'Select FT/HT', 
                                       choices = c('Full Time', 'Half Time'), 
                                       selected = 'Full TIme')),
                       div(style = "clear:both")),
                     div(id = 'parent',
                       div(class = "image", 
                           highchartOutput("OU_35",
                                           height = "300px", 
                                           width = "270px")),
                       div(class = "image", 
                           style = "width:270px;background:white;height:300px;"),
                       div(class = "image", 
                           highchartOutput("OU_45", 
                                           height = "300px", 
                                           width = "270px")),
                       div(style = "clear:both")),
                     div(id = 'parent',
                       div(class = "image", 
                           highchartOutput("OU_55", 
                                           height = "300px", 
                                           width = "270px")),
                       div(class = "image", 
                           highchartOutput("OU_65", 
                                           height = "300px", 
                                           width = "270px")),
                       div(class = "image", 
                           highchartOutput("OU_75", 
                                           height = "300px", 
                                           width = "270px")),
                       div(style = "clear:both"))),
            
            #tab Cards in Application tab
            tabPanel('Cards / Fouls',
                     br(),
                     fixedRow(
                       column(highchartOutput('cards_fouls_team', 
                                              height = '450px'), 
                              width = 10),
                       column(br(), 
                              br(),
                              selectInput('cards_fouls_HA', 
                                          'Select Home/Away',
                                          choices = c('Home', 'Away', 'All'), 
                                          selected = 'Home'),
                              selectInput('cards_fouls_total', 
                                          'Total / Per Team'    ,
                                          choices = c('Total', 'Per Game'), 
                                          selected = 'Total'), 
                              width = 2)
                      ),
                      br(),
                      fixedRow(
                        column(highchartOutput('cards_over_time', 
                                               height = '450px'), 
                               width = 10),
                        column(br(),
                               br(),
                               selectInput('cards_team', 
                                           'Select Team', 
                                           choices = init_teams,
                                           selected = 'Arsenal'),
                               selectInput('cards_HA',
                                           'Select Home/Away',
                                           choices = c('Home', 'Away', 'All'),
                                           selected = 'Home'),
                               width = 2)
                      ),
                      br(),
                      fixedRow(
                        column(highchartOutput('referees', height='500px'), 
                               width = 5),
                        column(highchartOutput('total_cards_fouls', 
                                               height = '450px'), 
                               width = 5),
                        column(br(),
                               br(),
                               radioButtons('abso_ave', 
                                            'Absolute or Average', 
                                            choices = c('Total', 'Per Game'),
                                            selected = 'Total'), 
                               width = 2)
                      )),
            
            #tab Corners in Application tab
            tabPanel('Corners',
                     br(),
                     fixedRow(
                       column(highchartOutput('corners_per_team', 
                                              height = '450px'), 
                              width = 10),
                       column(br(),
                              br(),
                              selectInput ('corner_ha', 
                                           'Select Home/Away', 
                                           choices = c('Home', 'Away', 'All'), 
                                           selected = 'Home'),
                              radioButtons('abso_OU',
                                           'Total - Over/Under', 
                                           choices = c('Total', 
                                                       'Per Game', 
                                                       'Over/Under'), 
                                           selected = 'Total'),
                              conditionalPanel(condition = "input.abso_OU == 'Over/Under'",
                              selectInput('cornerOU',
                                          'Select Over/Under', 
                                          choices = c('O/U 5.5',
                                                      'O/U 8.5',
                                                      'O/U 10.5',
                                                      'O/U 13.5'), 
                                          selected = 'O/U 5.5')),
                              helpText('Note: Over/Under is based on 
                                       total corners in a game'),
                              width = 2)
                     ),
                     br(),
                     fixedRow(
                       column(highchartOutput('corners_over_time', 
                                              height = '450px'), 
                              width = 10),
                       column(br(),
                              br(),
                              selectInput('corner_team', 'Select Team',
                                          choices = init_teams,
                                          selected = 'Arsenal'),
                              selectInput('corner_HA_overtime',
                                          'Select Home/Away',
                                          choices = c('Home', 'Away', 'All'), 
                                          selected = 'Home'),
                              width = 2)
                     ),
                     fixedRow(
                       br(),
                       column(highchartOutput('corners_55', height = '300px'), 
                              width = 5),
                       column(highchartOutput('corners_85', height = '300px'), 
                              width = 5)
                     ),
                     fixedRow(
                       column(highchartOutput('corners_105', height = '300px'), 
                              width = 5),
                       column(highchartOutput('corners_135', height = '300px'), 
                              width = 5)
                     )),
            
            #tab Raw table in Application tab
            tabPanel('Raw Table',
                     br(),
                     fixedRow(
                       column(DT::dataTableOutput('raw', width = '100%'), 
                              width = 10),
                       br(),
                       br(),
                       column(downloadButton('downloadData', 
                                             'Download Data'), 
                              width = 2)
                     ))
          ),    
        width = 9)
      )
    ),
    tabPanel('About',
      
     fixedRow(        
       column(
        img(src = 'stadium-pic.jpg', id = 'ribbon-about', height = 230),
        width = 12
       )
     ),
     br(),
     
     fixedRow(
      
      column(
       HTML('<ul class="nav nav-pills nav-justified">
                 <li class="active">
                 <a href="#about-tab-1" data-toggle="tab" data-value="Author">Author</a>
                 </li>
                 <li>
                 <a href="#about-tab-2" data-toggle="tab" data-value="Data/Charts">Data / Charts</a>
                 </li>
                 <li>
                 <a href="#about-tab-3" data-toggle="tab" data-value="Contact">Contact</a>
                 </li>
              </ul>'),
       width = 12
       )
     ),
     
     HTML('<div class="container-fluid">
              <div class="tab-content">
                 <div class="tab-pane active" data-value="Author" id="about-tab-1">'),
                 
                 br(),
                 br(),
                 br(),
                
                 column(width = 2),
                 column(h2('Author'), div(id = 'about', 
                        HTML('Theo Boutaris is currently working as a Senior 
                        Data Scientist using primarily R as his main programming language. He holds a Bachelor\'s 
                        degree in Finance and a Master\'s degree in Finance, Investments and
                        Risk Management.<br> <br>By combining his love for statistics with
                        his passion about football, he has constructed multiple models to predict
                        football results. Drawing all his information and data from the vast football
                        and programming communities he decided it was time to give something back!<br><br>
                        FootballeR is a free tool designed for punters, researchers or simply
                        football enthusiasts who require something more than the traditional static
                        tables offered in most websites. <br><br>Theo currently lives in London and
                        he remains a proud supporter of Aris Salonica and Crystal Palace!')),
                        width = 8),
                 column(width = 2),
     
     HTML('     </div>
                <div class="tab-pane" data-value="Data/Charts" id="about-tab-2">'),
                
                br(),
                br(),
                br(),
                
                column(width = 2),
                column(h2('Data'), 
                       div(id = 'about', 'The data in this application comes from and is owned by', 
                       a(id = 'footy', 'football-data.co.uk', 
                         href = 'http://www.football-data.co.uk/data'),
                       ' a free football and betting database. 
                         Football-data offers many more services than just data. 
                         Feel free to pay them a visit!'),
                       br(),
                       h2('Graphs'),
                       div(id = 'about', 'The graphs in this application come from the',
                       a(id = 'HC', 'Highcharts', href = 'http://www.highcharts.com'),
                       ' JavaScript graphics library. Visit their website for more information!'),
                       width = 8),
                column(width = 2),
               
     HTML('     </div>
                <div class="tab-pane" data-value="Contacts" id="about-tab-3">'),
                
                br(),
                br(),
                br(),
                
                column(width = 2),
                column(h2('Contact'), 
                       div(id = 'about', 'If you would like to contact the author about the application feel
                           free to drop him an email at:',
                       a(id = 'email', 'teoboot2007@hotmail.com', 
                         href = 'mailto:teoboot2007@hotmail.com')),
                       br(),
                       h2('Issues'),
                       div(id = 'about', 'If you found an issue or bug with the 
                       application please do post it as such
                       on the development page, under the ', 
                       a(id = 'HC', 'issues tab', href = 'https://github.com/LyzandeR/FootballeR/issues'),
                       'in order to have a look.'),
                       width = 8 ),
                column(width = 2),
                
     HTML('     </div>
              </div>
           </div>')
    ),
    
    div(class = "footer", 
        "Developed with ", 
        icon('heart'),
        " in R and Rstudio. For the latest Development and Licence: ",
        a(id="github_icon", 
          href="https://github.com/LyzandeR/FootballeR", 
          icon('github')))
  )
)








