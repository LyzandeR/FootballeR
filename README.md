# FootballeR
This is a web-based interactive dashboard about European Football. It is an application designed for punters, researchers or simply football enthusiasts who want something more than the static and difficult to understand tables that most of the companies offer. This free (and ad-free) application offers you interactive and customisable graphs and tables in order for you to find exactly what you are looking for in an easy and nice way through charts. The application contains data from 11 countries:

- England
- Scotland
- Germany
- Italy
- Spain
- France
- The Nertherlands
- Belgium
- Portugal 
- Greece 
- Turkey

since 1993 and up to five divisions depth!

# Application

To start using the application visit the following link in shinyapps:

[link will be provided shortly] 

and click the "Application" button at the top of the front page to begin!

![screenshot](https://github.com/LyzandeR/FootballeR/blob/master/app/www/App_screenshot.png)

**Or if you are an R user you can easily type on your console:**

`shiny::runGitHub('FootballeR', 'LyzandeR', subidr = 'app')`

You would also need the following up-to-date packages to be installed on your machine:

- data.table
- shiny
- DT
- highcharter

# Details

FootballeR is split into 7 sections as depicted in the app's tabs:

- League Table
- Goals
- Results
- Over / Under
- Cards / Fouls
- Corners
- Raw Table

#### Sidebar

On the left hand side you can select the country, division and year you would like to view information about. You can also specify a date range to view stats for. Keep in mind that whenever you **change a choice on the sidebar, this will affect all of the tabs**.  


#### League Table

This tab shows information about the league table as it was according to your sidebar choices and between the dates specified at the slider on the sidebar. Try adjusting the date range slider to see how the league table was (or could have been) between different dates. (Notice that any date changes on the date slider will affect all of the tabs and not just the league table).

#### Goals 

This tab shows information about the goals and shots of each of the teams.

The first from the top graph shows the aggregated shots, shots on target and goals of each of the teams. Notice that for the first graph the orange line corresponds to the right y Axis whereas the bars correspond to the left y Axis. You can view information about:

- Home/Away/All games
- Total Shots and Goals or Per Game 

The second graph shows the individual performances of each of the teams. The games are in order as played i.e. the first game on the left is the oldest and the last game on the right is the most recent. You can select:

- The team of Interest 
- Home/Away/All Shots and Goals.

The funnel shows the total number of Shots, On Target Shots and Goals for the underlying league.

The bubble shows the average number of shots (total shots not shots on target) that in general teams need to take in order to score.

#### Results

This tab shows information about the results of each of the teams (Wins - Draws - Losses).

The first from the top graph shows the aggregated Wins, Draws and Losses of each of the teams. You can select between:

- Home/Away/All
- Half Time and Full Time

The second graph shows the individual results of each of the teams. The games are in order as played i.e. the first game on the left is the oldest and the last game on the right is the most recent. You can select:

- The team of Interest 
- Home/Away/All results

The next two graphs show the total number of League Wins and Draws for the underlying league for both Half Time and Full Time.

#### Over / Under

This is probably the most interactive of the charts with a plethora of information. The tab is dedicated to showing aggregated and individual Over and Under performances. 

The first graph shows aggregated Over / Under information per Team. You can select:

- Home/Away/All
- Full Time or Half Time
- Any Over / Under combination between 0.5 to 7.5

The second graph shows the individual performances of each of the teams. The games are in order as played i.e. the first game on the left is the oldest and the last game on the right is the most recent. Selections of:

- The team of Interest
- Home/Away/All games
- Full Time or Half Time
- Any Over / Under combination between 0.5 to 7.5

The rest of the graphs show total league information for all Over/Under combinations between 0.5 to 7.5 Goals. You can still choose:

- Full Time or Half Time

#### Cards / Fouls

This tab shows information about the Yellow and Red Cards of the league chosen and also contains information about the fouls commited.

The first graph shows aggregated Card and Foul information per Team. Notice that the black line on the graph corresponds to the right Y axis whereas the bars correspond to the left Y axis. You can select:

- Home/Away/All
- Total number of cards/Fouls or Per Game

The second graph shows the individual cards and fouls of each of the teams. The games are in order as played i.e. the first game on the left is the oldest and the last game on the right is the most recent. You can select:

- The team of Interest
- Home/Away/All games

The bottom left graph shows information about the referees of the league and the yellow and red cards they brandished.

The bottom right graph shows the total number of cards and fouls in the league selected.

For the above two graphs you can select to see:

- Total or Average cards / fouls

#### Corners

The tab contains information about the corners of the selected league.

The first graph shows the aggregated corners per team. You can select:

- Home/Away/All games. **This option is based on corners won by the selected team only**
- Total or Per Game corners. **This option is based on corners won by the selected team only**
- Over / Under among 5.5, 8.5, 10.5, 13.5. **This option is based on total corners won in a game**

The second graph shows the individual corners won by each of the teams. The games are in order as played i.e. the first game on the left is the oldest and the last game on the right is the most recent. You can select:

- The team of Interest
- Home/Away/All games

The rest of the graphs show total league information for Corner Over/Under combinations of 5.5, 8.5, 10.5 and 13.5 corners. 

#### Raw Data

This tab contains the raw data as downloaded from football.data.co.uk. You can click the download button to download a csv copy.

# Data

All the data are provided from and owned by the amazing and free data base of European Football at [football-data.co.uk]. If you haven't already you can visit their site for betting information, live scores, historical data (including betting data) and much more!

By using this application you agree to the terms and conditions of using their data. You can visit the corresponding part of their [website] for more information. 

football-data.co.uk and the developer cannot be held responsible for any mistakes in the data and offer no guarantee that the data is correct. If you do find any mistakes though, you are adviced to email football-data.co.uk so that the data can be corrected.

Football-Data maintains full copyright over the data files.

#### Data Updates

The application connects to football-data.co.uk so it implements data updates as soon as they occur at football-data.co.uk .

# Charts

The charts in this app come from the awesome JavaScript charting framework [Highcharts]. You can find out more about this project on their website. Notice that although this application is open source and free of charge and ads, in order to use it for your business you need a highcharts licence (apart from the author's permission - check the licence part of this wiki below). For more information visit [pricing at highcharts]. The port of the JavaScript highcharts to R has been done with the also amazing [Highcharter] R package.

# Development

This application has been developed in R and Rstudio.

# Licence

Copyright (c) 2016 Theodoros Boutaris. All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

- The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
- To copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, the written permission of the original author is required.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

#### Contact

To contact the author please send an email to [teoboot2007@hotmail.com]


[[link will be provided shortly]]: <#>
[football-data.co.uk]:   <http://www.football-data.co.uk/data.php>
[website]: <http://www.football-data.co.uk/disclaimer.php>
[Highcharts]: <http://www.highcharts.com/>
[pricing at highcharts]: <http://shop.highcharts.com/highcharts/>
[Highcharter]: <http://jkunst.com/highcharter/>
[teoboot2007@hotmail.com]: <mailto:teoboot2007@hotmail.com>










