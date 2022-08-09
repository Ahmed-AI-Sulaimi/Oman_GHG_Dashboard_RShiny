#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#========================= Packages/Libraries =================================#


library(shiny)
library(sf)
library(raster)
library(ggplot2)
library(data.table)
library(leaflet)
library(tidyverse)
library(ggthemes)
library(stringr)
library(scales)
library(DT)
library(data.table)
library(forcats)
library(spdep)
library(classInt)
library(grid)
library(gridExtra)
library(skimr)
library(lattice)
library(shinydashboard)
library(ncdf4)
library(sp)
library(plotly)
library(shinyWidgets)
library(shinythemes)
library(skimr)
library(tmap)
library(lubridate)



# setwd('/srv/shiny-server/GHG-Oman')

#========================= data pre-processing ================================#

# Load Oman and Governorate polygons
oman_gov <- sf::read_sf('data/Governorates_f.gpkg') %>% sf::st_transform(4326)
oman_prov <- sf::read_sf('data/Provinces_f.gpkg') %>% sf::st_transform(4326)
oman <- sf::st_as_sf(sf::st_union(oman_gov)) 



oman_gov_select <- sf::st_as_sf(oman_gov) %>% dplyr::select(governorate) %>% sf::st_transform(4236)
# oman_prov_select <- sf::st_as_sf(oman_prov) %>% dplyr::select(`province`)  
# probably unnecessary
# mapview(oman, col.regions ='yellow')

descr<-read_csv('data/desc.csv', show_col_types = F)

# Read EAC4 CAMS global reanalysis - Monthly Raster Data
# ch4_total_column <- raster::brick('data/2020_eac4.nc', varname = 'tc_ch4') %>% terra::mask(oman)

co  <-  raster::brick('data/2020_eac4.nc', varname = 'tcco') %>% raster::crop(oman)
o3  <-  raster::brick('data/2020_eac4.nc', varname = 'gtco3')%>% raster::crop(oman)

# h2o <-  raster::brick('data/2020_eac4.nc', varname = 'tcw')%>% crop(oman)

# Read EGG4 CAMS global reanalysis - co2 column and molar fraction data CO2 and CH4
# co2_accumulated  <- raster::brick('data/2020_egg4.nc',varname = 'aco2rec') %>% terra::mask(oman)

co2 <- raster::brick('data/2020_egg4.nc', varname = 'tcco2')%>% crop(oman)
ch4 <- raster::brick('data/2020_egg4.nc', varname = 'tcch4')%>% crop(oman)

# ch4_total_column <- raster::brick('data/2020_eac4.nc', varname = 'tc_ch4') %>% terra::mask(oman)

# Read data masked to Oman

comask  <-  raster::brick('data/2020_eac4.nc', varname = 'tcco') %>% mask(oman)
o3mask  <-  raster::brick('data/2020_eac4.nc', varname = 'gtco3')%>% mask(oman)

# h2omask <-  raster::brick('data/2020_eac4.nc', varname = 'tcw')%>% mask(oman)

# Read EGG4 CAMS global reanalysis - co2 column and molar fraction data CO2 and CH4
# co2_accumulated  <- raster::brick('data/2020_egg4.nc',varname = 'aco2rec') %>% terra::mask(oman)

co2mask <- raster::brick('data/2020_egg4.nc', varname = 'tcco2')%>% mask(oman)
ch4mask <- raster::brick('data/2020_egg4.nc', varname = 'tcch4')%>% mask(oman)

listofbricks<-list(co2,co,o3,co2mask,ch4mask,comask,o3mask)

for (r in listofbricks){ # reproject raster
  crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
}



# Create List of GHG
ghglist <- list(co2, ch4, co, o3)
names(ghglist) <- c('co2','ch4', 'co', 'o3')
sqrlist<-list(co, o3)
smallareas<-list('Muscat','Musandam','Al - Batinah South')
# source fit-for-purpose functions:

# 1- get_timeSeries function composed by aland manurung and modified
# from https://github.com/alandmanurung/pollutant-map.git
  

# 2- leaflet decending legend function composed by mpriem89 from https://github.com/rstudio/leaflet/issues/256#issuecomment-440290201

source('data/ffp_functions.R')


# Choices for ghgs
ghg_choices <- c('co2', 'ch4', 'co', 'o3')
ghg_vars <- c(
  'CO2 Concentration (mg/m3)' = 'co2',
  'CH4 Concentration (mg/m3)' = 'ch4',
  'CO Total Column (mg/m2)' = 'co',
  'Ozone Total Column (mg/m2)' = 'o3'
  #  'Water' = 'h2o'
)
ghg_cf <- c(
  'Total Carbon Footprint (Tons per Capita)' = 'cfp',
  'CO2 and CO (Tons per Capita)' = 'co2co',
  'CO2e equivelant in Methane (Tons per Capita)' = 'ch4'
)

# Choices for oman admin units
source('data/oman_admin.R', echo = FALSE)




oman_ext_month <- oman %>% extent()

timeSeries_extract0r <- function(date_start, date_end, area){
  

  
  dates <- names(co2)
  ghgTS <- tibble(
    id = dates,
    date = as.Date(id, 'X%Y.%m.%d'),
    co2 = NA,
    ch4 = NA,
    co  = NA,
    o3  = NA

  ) %>%
    column_to_rownames('id')
  
  #  ghglist <- list(co2, ch4, co, o3)
  
  if(area == 'Muscat'| area == 'Musandam' | area == 'Al - Batinah South'){
    maskArea <- st_centroid(subset(oman_gov,oman_gov$governorate==area))
    
    
  }else if (area == 'All') {
    maskArea <- oman
    
  } else {
    maskArea <- subset(oman_gov, oman_gov$governorate == area)
    
  }
  
  ghgTS['co2']<- raster::extract(x= co2,y=maskArea, fun = mean) %>% as.numeric() * 1.800            # PPM To milligram per cubic meter for CO2
  ghgTS['ch4']<- raster::extract(x= ch4,y=maskArea, fun = mean) %>% as.numeric() * (1/1000) * 0.656 # PPB To milligram per cubic meter for CH4
  ghgTS['co' ]<- raster::extract(x= co ,y=maskArea, fun = mean) %>% as.numeric() * 1e6
  ghgTS['o3' ]<- raster::extract(x= o3 ,y=maskArea, fun = mean) %>% as.numeric() * 1e6
  ghgTS %>%
    dplyr::filter(date >= date_start & date <= date_end) %>%
    dplyr::as_tibble()
    
}



#=======================##=======================# more data #=======================##=======================#



om_bld_permits<-readr::read_csv('data/om_bld_permits_opt.csv', show_col_types = F) 


om_pop_monthly<-readr::read_csv('data/oman_mo_gov_pop_data_wide.csv', show_col_types = F) %>% 
#  dplyr::select(-`Total Sultanate`) %>%
  tidyr::pivot_longer(!date, names_to = 'governorates',values_to = 'population') %>% 
  dplyr::mutate(year = str_sub(date,start = 1,end=4)) %>% 
  dplyr::mutate(date = as.Date(format(date, "%Y-%m-%d")))

om_pop_yearly<-om_pop_monthly %>% 
  dplyr::group_by(year, governorates) %>% 
  summarise(population = max(population))


### ####################################################################### ###

tsl<- list()
for(g in governorates_na){
  pri<-timeSeries_extract0r(date_start = '2012-01-01',date_end = '2020-12-01',area = g)
  
  pri['gov']<-g
  
  tsl[[g]] <- pri
  
}


omangovareas <- oman_gov %>% dplyr::select(c(2,4,6:10)) %>% sf::st_drop_geometry()# %>% 




bound_tbl <- dplyr::bind_rows(tsl) %>% rename(governorate = gov)
bound_tbl_1 <- bound_tbl %>% left_join(x = omangovareas)


new_tbl<- bound_tbl_1 %>%
  dplyr::mutate(Shape_Vol = Shape__Are*(10000-elev_avg_mean)) %>%
  dplyr::mutate(Shape_mol = Shape_Vol*(2.477/1000)) %>%
  dplyr::mutate(co2_tons_per_km2 = (co2)) %>% # from mg/m3 to tonnes/km3 (1e-9/1e-9) cancels out
  dplyr::mutate(ch4_tons_per_km2 = (ch4)) %>% # from tonnes/km3 (1e-9/1e-9) cancels out
  dplyr::mutate(co2_tonnage = co2_tons_per_km2*(BuiltUp_Areas_area*1e-6)*BuiltUp_Areas_pc) %>%
  dplyr::mutate(ch4_tonnage = ch4_tons_per_km2*(BuiltUp_Areas_area*1e-6)*BuiltUp_Areas_pc) %>% 
  dplyr::mutate(methane_co2e= (ch4_tonnage)*25)

bound_tbl_3<-new_tbl %>% left_join(om_pop_monthly, 
                                   by = c('date'='date',
                                          'governorate'='governorates'))%>% drop_na() %>% 
  dplyr::mutate(co2_tons_per_capita = co2_tonnage/(population)) %>% 
  dplyr::mutate(ch4_tons_per_capita = ch4_tonnage/(population)) 


expt_tbl_1<-bound_tbl_1 %>% dplyr::mutate('CO2 Tonnes/km2' = co2,
                                          'CH4 Tonnes/km2' = ch4,
                                          'CO Tonnes/km2' = co*(1e-9/1e-6),
                                          'O3 Tonnes/km2' = o3*(1e-9/1e-6)) %>% 
  dplyr::select(-c(2:7,9:12))
  

bound_tbl_Summary_gov_1 <- dplyr::mutate(bound_tbl_3, year = as.POSIXlt(date)$year+1900)  %>%
  dplyr::group_by(year,governorate) %>%
  dplyr::summarise(Shape__Are = mean(Shape__Are),
                   # co2_avg = mean(co2),                                           #
                   # ch4_avg = mean(ch4),                                           #
                   co2_tons_per_km2 =  mean(co2_tons_per_km2),                     #1
                   ch4_tons_per_km2 =  mean(ch4_tons_per_km2),                     #2 ===========
                   co_tons_per_km2 = mean(co*(1e-9/1e-6)),                                     #3 mg/m2 = tons/km2 where 1e-6 cancels out
                   o3_tons_per_km2 = mean(o3*(1e-9/1e-6)),                                     #4
                   population_estimate = last(population),                         #5
                   population_density  = (population_estimate/(Shape__Are*1e-6)),  #6
                   co2_tons_per_capita = mean(co2_tons_per_capita),                #7
                   ch4_tons_per_capita = mean(ch4_tons_per_capita)                 #8
                   ) #%>% 
bound_tbl_Summary_gov_2 <- dplyr::left_join(bound_tbl_Summary_gov_1, 
                                            om_bld_permits, by = c('year'='year', 'governorate'='governorate'))

var_list<-c(
  'CO2 Emissions (Tons/Km2)' = 'co2_tons_per_km2',               #1
  'CH4 Emissions (Tons/Km2)' = 'ch4_tons_per_km2',               #2
  'CO Average Total Column (Tons/Km2)' =    'co_tons_per_km2',  #3
  'Ozone Average Total Column (tons/km2)' = 'o3_tons_per_km2',  #4 ===========
  'Population' = 'population_estimate',                         #5             
  'Population Density(Pers/Km2)' = 'population_density',        #6
  'CO2 Tonnes per capita'='co2_tons_per_capita',                #7
  'CH4 Tonnes per capita'='ch4_tons_per_capita',                #8
  'Number of Residential Building permits' = 'Residential',     #9             
  'Number of Commercial permits' = 'Commercial',                #10
  'Number of Commercial/ Residential'='Commercial_Residential', #11
  'Number of Industrial permits'='Industrial',                  #12
  'Number of Agricultural permits'='Agricultural'               #13
)



  
oman_gov_select<-oman_gov_select %>% dplyr::rename(geometry = geom)
oman_gov_select_sp<- as_Spatial(oman_gov_select)  










#==============================================================================#
  # load dependencies
# from the following github https://github.com/sdesabbata/BivariateTMap.git
source('data/bivariate_choro/bivariate_tmap.R')
  



#==============================================================================#

ts_om<-timeSeries_extract0r('2012-01-01', '2020-12-01', 'All')
ts_om['governorate']<-'All'

om_pop_monthly["governorates"][om_pop_monthly["governorates"] == "Total Sultanate"] <- "All"


oman_sqkm<- oman_gov %>% 
  dplyr::mutate(BuiltUp_Areas_area = BuiltUp_Areas_area/1000000) %>%
  dplyr::select(c(governorate, BuiltUp_Areas_area)) %>%
  sf::st_drop_geometry()

oman_sqkm<-oman_sqkm %>% 
  add_row(governorate = "All", BuiltUp_Areas_area = 34560.64) %>% 
  dplyr::rename(area_sqkm = 'BuiltUp_Areas_area')



ts_om<-ts_om %>% dplyr::bind_rows(bound_tbl)

ts_om<- ts_om %>% left_join(om_pop_monthly,
                            by = c('date'='date',
                                   'governorate'='governorates')) %>% drop_na()
ts_om<-ts_om %>% left_join(oman_sqkm,
                           by = c('governorate'='governorate'))

ts_om<-ts_om %>%
  dplyr::mutate(
  co2co = ((co2+co)*area_sqkm)/population,
  ch4 = (ch4*25*area_sqkm)/population) %>% 
  dplyr::mutate(
    cfp = co2co+ch4
  ) %>% 
  dplyr::select(-c('co2','co','o3')) 

  # TS_Table_2 <- ts_om %>% dplyr::filter(governorate != 'All')
  # 
  # mTS_2 <- TS_Table_2 %>%
  #   dplyr::select(c('date','governorate','cfp','co2co', 'ch4'))


########## Time series summaries ##############
mean_om_tbl_1<-expt_tbl_1 %>%
  dplyr::group_by(date) %>% 
  dplyr::summarise(`CO2 Tonnes/km2`=mean(`CO2 Tonnes/km2`),
                   `CH4 Tonnes/km2`=mean(`CH4 Tonnes/km2`),
                   `CO Tonnes/km2` =mean(`CO Tonnes/km2`),
                   `O3 Tonnes/km2` =mean(`O3 Tonnes/km2`))

co2_ts_om <- ts(data = mean_om_tbl_1$`CO2 Tonnes/km2`, 
              start = c(year(first(mean_om_tbl_1$date)), month(first(mean_om_tbl_1$date))), 
              end = c(year(last(mean_om_tbl_1$date)), month(last(mean_om_tbl_1$date))), 
              frequency = 12)
ch4_ts_om <- ts(data = mean_om_tbl_1$`CH4 Tonnes/km2`, 
                start = c(year(first(mean_om_tbl_1$date)), month(first(mean_om_tbl_1$date))), 
                end = c(year(last(mean_om_tbl_1$date)), month(last(mean_om_tbl_1$date))), 
                frequency = 12)
co_ts_om <- ts(data = mean_om_tbl_1$`CO Tonnes/km2`, 
                start = c(year(first(mean_om_tbl_1$date)), month(first(mean_om_tbl_1$date))), 
                end = c(year(last(mean_om_tbl_1$date)), month(last(mean_om_tbl_1$date))), 
                frequency = 12)
o3_ts_om <- ts(data = mean_om_tbl_1$`O3 Tonnes/km2`, 
               start = c(year(first(mean_om_tbl_1$date)), month(first(mean_om_tbl_1$date))), 
               end = c(year(last(mean_om_tbl_1$date)), month(last(mean_om_tbl_1$date))), 
               frequency = 12)

co2_om_stl <- stl(x = co2_ts_om, s.window = "periodic")
ch4_om_stl <- stl(x = ch4_ts_om, s.window = "periodic")
co_om_stl <- stl(x = co_ts_om, s.window = "periodic")  
o3_om_stl <- stl(x = o3_ts_om, s.window = "periodic")

#========================= dashboard UI =======================================#


# Define UI for application that draws a histogram

UI<- dashboardPage( skin = 'black',# start with title 
                    dashboardHeader(title = 'Exploring GHG Emissions 2012-2020 - Oman',
                                    titleWidth = 500,
                                    tags$li(class='dropdown',
                                            tags$a(href ='https://github.com/Ahmed-AI-Sulaimi/Oman_GHG_Dashboard_RShiny.git', # #2# adjust this later
                                                   icon('github'),'Source Code',
                                                   target='_blank')
                                    )
                    ), # dashboard sidebar contains 
                    dashboardSidebar(width = 250, 
                                     sidebarMenu(
                                       id = 'sidebar', 
                                       menuItem('GHG Emissions data view (Monthly)',tabName ='GHGE',
                                                icon = icon('map')),
                                       menuItem('Advanced view (Yearly)',tabName ='EXP',
                                                icon = icon('dashboard')),
                                       menuItem('Dataset',tabName ='dataset',
                                                icon = icon('database'))
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'GHGE',
                                tabBox(id='m1',width = 12,height = 880,
                                       tabPanel(title = 'Monthly Mean GHG Map View',
                                                tags$style(".nav-tabs {background: #f4f4f4;}
                .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom 
                .nav-tabs li.active a {background-color: #fff; border-color: #fff;}
                .nav-tabs-custom .nav-tabs li.active {border-top-color: #314a6d;}
                .nav-tabs-custom {box-shadow:2px 2px 2px lightgrey}
                .tab-pane {background-color: #fff; border-color: #f4f4f4;}
                .box.box-solid.box-primary>.box-header {
                color:#f4f4f4;
                background:#f4f4f4}
                .box.box-solid.box-primary{border-bottom-color:#f4f4f4;
                                           border-left-color:#f4f4f4;
                                           border-right-color:#f4f4f4;
                                           border-top-color:#f4f4f4;
                                           box-shadow:2px 2px 2px lightgrey;}
                .panel panel-default{border-bottom-color:#666666;
                                           border-left-color:#666666;
                                           border-right-color:#666666;
                                           border-top-color:#666666;
                                           box-shadow:2px 2px 2px black;}
                .panel-heading {margin-left: 20px;}
                .panel panel-info{margin-left: 20px;
                .shiny-input-container label {margin-left: 20px;}
                .shiny-notification {width: 200%; height: 200%;}
                .shiny-notification-panel { margin-left: auto; margin-right: auto; width: 150%; max-width: 600px;}
                .navbar-custom-menu .navbar-nav li .dropdown-menu {width:900px;hight: 200%;}"
                                                           
                                                ),
                                                tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                                                box(width = 5, height = 780,solidHeader = T, collapsible = F,status = 'primary',
                                                    leafletOutput("map_monthly",height = 760),
                                                    absolutePanel(
                                                      id = "controls_monthly",
                                                      class = "panel panel-default",
                                                      fixed = T,
                                                      draggable = T,
                                                      top =140,
                                                      left = 320,
                                                      right = "auto",
                                                      bottom = "auto",
                                                      width = 150,
                                                      height = "auto",
                                                  
                                                      h4(" Parameter Settings"),
                                                      
                                                      selectInput("ghg_monthly", " Greenhouse Gas:", ghg_vars,
                                                                  width = 125)
                                                      ,
                                                      
                                                      airDatepickerInput(
                                                        "date_monthly",
                                                        " Date:",
                                                        value= "2015-01-01",
                                                        minDate = "2012-01-01",
                                                        maxDate = "2020-12-01",
                                                        view = "months",
                                                        minView = "months",
                                                        width = 125,
                                                        dateFormat = 'yyyy-MM'
                                                        
                                                      ))
                                                ),
                                                box(
                                                  selectInput(inputId = "gov_month", label = "Governorate",choices = governorates_choices[-11],
                                                              selected = 'All'),status = 'primary',
                                                  solidHeader = T, collapsible = F,height = 160, width = 2,
                                                  footer ='Choose \'All\' to view data for all of Oman.'),
                                                box(uiOutput("ghg_indicators"),status = 'primary',
                                                  fluidRow(valueBoxOutput(outputId = 'CO2_index',width = 3),
                                                           valueBoxOutput(outputId = 'CH4_index',width = 3),
                                                           valueBoxOutput(outputId = 'CO_index',width = 3),
                                                           valueBoxOutput(outputId = 'O3_index',width = 3)),
                                                  solidHeader = T, collapsible = F,height = 160, width = 5),
                                                tabBox(id='charts1', height = 620, width = 7,
                                                       tabPanel(title = 'Time-series',
                                                                tabPanel(title = 'time-series',
                                                                         uiOutput("mt_govName"),
                                                                         plotlyOutput("monthlyTS", height = '510px'),
                                                                         h6("Contains modified Copernicus Atmosphere Monitoring Service Data",
                                                                            style = "font-size:12px;"))
                                                       ),
                                                       tabPanel(title = 'Governorates bar-chart', ##)) ## ui addition 24_06 here
                                                                uiOutput("bc_govName"),
                                                                plotlyOutput("bar_chart_sn",height = '510px'),
                                                                h6("Adjust parameter settings to view change variable and date",
                                                                   style = "font-size:14px;")))
                                       ),
                                       ########### carbon footprint #############
                                       
                                       ########### carbon footprint #############
                                       
                                       tabPanel(title = 'Monthly Carbon footprint Map View',icon = icon(lib = 'glyphicon','fire'),
                                                box(width = 5, height = 780,solidHeader = T, collapsible = F,status = 'primary',
                                                    leafletOutput("map_cf_monthly",height = 760),
                                                    absolutePanel(
                                                      id = "controls_cf_monthly",
                                                      class = "panel panel-default",
                                                      fixed = TRUE,
                                                      draggable = F,
                                                      top =140,
                                                      left = 320,
                                                      right = "auto",
                                                      bottom = "auto",
                                                      width = 170,
                                                      height = "auto",
                                                      h4(" Parameter Settings"),
                                                      selectInput("cf_monthly", " Carbon Footprint Variable:", ghg_cf, width = 125),
                                                      airDatepickerInput(
                                                        "date_cf_monthly",
                                                        " Date:",
                                                        value= "2015-01-01",
                                                        minDate = "2013-01-01",
                                                        maxDate = "2020-12-01",
                                                        view = "months",
                                                        minView = "months",
                                                        dateFormat = "yyyy-MM",width = 125
                                                      ))
                                                ),
                                                box(
                                                  selectInput(inputId = "gov_cf_month", 
                                                              label = "Governorate",
                                                              choices = governorates_choices[-11],
                                                              selected = 'All'),status = 'primary',
                                                  solidHeader = T, collapsible = F,height = 160, width = 2,
                                                  footer ='Choose \'All\' to view data for all of Oman.'),
                                                box(uiOutput("cf_indicators"),status = 'primary',
                                                    fluidRow(valueBoxOutput(outputId = 'TCF_index',width = 4), # total carbon footprint
                                                             valueBoxOutput(outputId = 'CO2COCF_index',width = 4), # tons per capita co2
                                                             valueBoxOutput(outputId = 'CH4CF_index',width = 4)), # tons per capita ch4
                                                    solidHeader = T, collapsible = F,height = 160, width = 5),
                                                tabBox(id='charts1', height = 620, width = 7,
                                                       tabPanel(title = 'Time-series',
                                                                tabPanel(title = 'time-series',
                                                                         uiOutput("mt_cf_govName"),
                                                                         plotlyOutput("monthlyTScf", height = '480px'),
                                                                         h6("Contains modified Copernicus Atmosphere Monitoring Service Data",
                                                                            style = "font-size:12px;"))
                                                       ),
                                                       tabPanel(title = 'Governorates bar-chart', ##)) ## 
                                                                uiOutput("bc_cf_govName"),
                                                                plotlyOutput("bar_chart_cf_sn",height = '510px'),
                                                                h6("Adjust system parameters to view change variable and date",
                                                                   style = "font-size:14px;"))
                                                 )
                                       )
                                       )
                                ),
                        tabItem(tabName = 'EXP',
                                tabBox(id='EXP1',width = 12, height = 880,
                                       tabPanel(title = 'Exploratory visualisation',
                                                box(solidHeader = T, collapsible = F,height = 780, width = 5,status ='primary',
                                                    plotOutput(width = 'auto', height = 750,outputId = 'bivariate_map'
                                                               
                                                    )
                                                ),
                                                box(status ='primary',
                                                    selectInput(inputId = "ghg_biv1", label = "Variable X",choices = var_list,
                                                                selected = 'co2_tons_per_km2'),
                                                    selectInput(inputId = "ghg_biv2", label = "Variable Y",choices = var_list,
                                                                selected = 'ch4_tons_per_km2'),
                                                    sliderInput(inputId="yearly_slider_1", label='Select time range (years)',
                                                                min=2012, max=2020, value=c(2012,2020), dragRange = T),
                                                    # selectInput(inputId = "gov_biv", label = "Highlight Governorate(s)",choices = governorates_choices[-c(9,11)],
                                                    #             multiple = T),
                                                    solidHeader = T, collapsible = F,width = 2, height = 780,
                                                    footer ='Select \'Variable X\' for the X Axis, \'Variable Y\' for the Y Axis, and the time range to Explore the relationship between the selected variables in the specified time range.'),
                                                
                                                box(solidHeader = T, collapsible = F,height = 780, width = 5,status ='primary',
                                                    plotlyOutput(outputId = "scatterplotly",  height = 750,width = 'auto'))
                                       )
                                )
                                         
                                         ),
                        tabItem(tabName = 'dataset',
                                tabBox(id = 'd1', width = 12, height = 880,
                                       tabPanel(title = 'Tabular monthly GHG data', icon = icon('table'),
                                                dataTableOutput('data_tbl_ts_ghg', height = 680)),
                                       tabPanel(title = 'Summary statistics', icon = icon('address-card'),
                                                box(solidHeader = T, collapsible = F,height = 780, width = 5,status ='primary',
                                                    verbatimTextOutput(outputId = 'summary'
                                                               
                                                    )
                                                ),
                                                box(status ='primary',
                                                    selectInput("summ_ghg_monthly", " Greenhouse Gas:", ghg_vars,
                                                                width = 'auto'),
                                                    selectInput("summ_ghg_area", " Governorate:", governorates_choices[-11],
                                                                width = 'auto', selected = 'All'),
                                                    solidHeader = T, collapsible = F,width = 2, height = 780,
                                                    footer ='Select governorate and GHG variable to filter outputs to specific area/ variable.'),
                                                box(solidHeader = T, collapsible = F,height = 780, width = 5,status ='primary',
                                                    plotOutput(width = 'auto', height = 750,outputId = 'Summaryplot'
                                                               
                                                    )
                                                )
                                                )
                                ))
                      )
                    )
)



# # Define server logic required to draw a histogram
SERVER <- function(input, output,session) {  
  # Create Leaflet Map
  output$map_monthly <- renderLeaflet({
    leaflet(oman_gov, options = leafletOptions(zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap) %>% 
      setView(lng = 54.53, lat = 21.17, zoom = 6) %>% 
      # Base groups
      addProviderTiles(providers$OpenStreetMap , group = "OpenStreetMap") %>% 
      addMiniMap(
        tiles = providers$Esri.WorldImagery,position = 'bottomleft',minimized = T,
        toggleDisplay = TRUE)
  })
  
  
  #==================for Carbon Footprint map stability==========================#
  TS_Table_2 <- ts_om %>% dplyr::filter(governorate != 'All')
  
  mTS_2 <- TS_Table_2 %>%
    dplyr::select(c('date','governorate','cfp','co2co', 'ch4')) %>%
    base::subset(date== '2015-01-01') %>%
    dplyr::left_join(oman_gov_select, by=c('governorate'='governorate'))
  # 
  # 
  mTS_2 <- within(mTS_2, cfp[governorate == 'Musandam']   <- NA)
  mTS_2 <- within(mTS_2, co2co[governorate == 'Musandam'] <- NA)
  mTS_2 <- within(mTS_2, ch4[governorate == 'Musandam']   <- NA)
  
  cf_table <- mTS_2 %>%
    base::subset(date==  '2015-01-01') %>%
    dplyr::select(c('date','governorate','cfp')) %>%
    dplyr::left_join(oman_gov_select, by=c('governorate'='governorate')) %>% st_as_sf() %>% 
    dplyr::filter(date==  '2015-01-01')
  # 
  
  pal <- colorBin(palette = 'YlOrBr', domain = cf_table$`cfp`,5, bins = c(0,15,30,50,250), pretty = FALSE)
  popup1 <-paste0('Month mean Total','<br>', 'Carbon Footprint: ', '<b>',
                  as.character(round(cf_table$`cfp`, digits = 2)),
                  '</b>',' in CO2e Tons per capita','</br>')
  
  #==================for Carbon Footprint map stability==========================#  
  
  output$map_cf_monthly <- renderLeaflet({
    leaflet(cf_table, options = leafletOptions(zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap) %>% 
      setView(lng = mean(c(oman_ext_month[1], oman_ext_month[2])),
              lat = mean(c(oman_ext_month[3], oman_ext_month[4])), zoom = 6) %>% 
      # Base groups
      addProviderTiles(providers$OpenStreetMap , group = "OpenStreetMap") %>%
      addMiniMap(
        tiles = providers$Esri.WorldImagery,position = 'bottomleft',minimized = T,
        toggleDisplay = TRUE) %>%
      addPolygons(
        fillColor = ~pal(cfp),
        fillOpacity = .80,
        opacity = 0.90,
        color = "black",stroke = T,weight = 0.7,
        smoothFactor = 0.5,
        popup = popup1
      )%>%
      addLegend_decreasing(position = "bottomright", pal = pal ,
                           values =cf_table$cfp,decreasing = T,
                           labFormat = labelFormat(digits = 2),na.label = 'No data',
                           title =  paste0('Month mean Total ',
                                           '<br>','Carbon Footprint in ', '</br>',
                                           '<br>','CO2e Tons per capita ', '</br>'))
  })
    
    
    
    
    
    
    
    
    
    
    
  
  
  
  
  
  
  output$monthlyTS <- renderPlotly({
    TS_Table <- timeSeries_extract0r("2012-01-01", "2020-12-01", "All")
    
    
    mTS <- TS_Table %>%
      pivot_longer(cols = -date, names_to = "gh_gas", values_to = "value") %>% 
      mutate(gh_gas = fct_relevel(gh_gas, c('co2', 'ch4', 'co', 'o3'))) %>%
      ggplot() +
      geom_line(aes(x = date, y = value, color = gh_gas), show.legend = FALSE) +
      facet_wrap(
        gh_gas ~ .,
        ncol = 1,
        scales = "free_y",
        labeller = labeller(
          gh_gas = c(
            "co2" = 'Carbon Dioxide (CO2)',
            "ch4" = 'Methane (CH4)',
            "co" =  'Carbon Monoxide (CO)',
            "o3" =  'Ozone (O3)'
            # "h2o"=  'Water Vapour'
          ))
      ) +
      scale_colour_manual(values = c('#e41a1c','#984ea3','#4daf4a','#377eb8'))+
      + scale_x_date(date_breaks = "6 months" , date_labels = "%m/%y")+
      labs(x = "Date", y = "") +
      ggthemes::theme_few()+
      theme(legend.position = "none", 
            panel.margin.y = unit(1.3, "lines"),
            axis.text.x = element_text(size = 9, angle = 20, colour = "black",
                                       vjust = 1, hjust = 1)
            )
    
    ggplotly(mTS)
  })
  
  output$monthlyTScf<- renderPlotly({
    TS_Table <- ts_om 
    # 
    # 
    mTS_1 <- TS_Table %>%
      dplyr::select(c('date','governorate','cfp','co2co', 'ch4')) %>%
      dplyr::filter(governorate == input$gov_cf_month) %>%
      dplyr::select(-'governorate') %>% 
      tidyr::pivot_longer(cols = -date, names_to = "cf_variable", values_to = "value") %>% 
      dplyr::mutate(cf_variable = fct_relevel(cf_variable, c('cfp', 'co2co', 'ch4'))) %>%
      ggplot() +
      geom_line(aes(x = date, y = value, color = cf_variable), show.legend = FALSE) +
      facet_wrap(
        cf_variable ~ .,
        ncol = 1,
        scales = "free_y",
        labeller = labeller(
          cf_variable = c(
            "cfp" = 'Total Carbon Footprint (Tons per Capita)',
            "co2co" = 'Carbon Dioxide & Carbon Monoxide (Tons per Capita)',
            "ch4" =  'CO2 equivlant amount of Methane (Tons per Capita)'
            #           # "h2o"=  'Water Vapour'
          ))
      ) +
      scale_colour_manual(values = c('#fec44f','#e41a1c','#984ea3'))+
      scale_x_date(date_breaks = "6 months" , date_labels = "%m/%y")+
      labs(x = "Date", y = "") +
      ggthemes::theme_few()+
      theme(legend.position = "none",
            panel.margin.y = unit(.9, "lines"),
            panel.spacing.y = unit(.1, "lines"),
            panel.spacing.x = unit(.05, "lines"),
            axis.text.x = element_text(size = 9, angle = 20, colour = "black",
                                       vjust = 1, hjust = 1)
      )
    # 
    ggplotly(mTS_1)
  })
  
  
  bound_tbl_Summary_gov_disp<-
    filter(bound_tbl_Summary_gov_2, between(year, 2013, 2019))
    
  
  
  observe({
    bound_tbl_Summary_gov_disp<-
      filter(bound_tbl_Summary_gov_2, between(year, input$yearly_slider_1[1], input$yearly_slider_1[2])) %>%
      dplyr::mutate(
        dplyr::across(
          c('co2_tons_per_km2':'o3_tons_per_km2'),
          # mutating the values to the total population
          function(x){ ifelse(governorate == "Musandam", NA, x) }
        )
      ) %>% 
      tidyr::drop_na()
    
    
   output$scatterplotly <-
     renderPlotly({

       attach(bound_tbl_Summary_gov_disp)
       h_quan<-quantile(get(input$ghg_biv1), probs = c(0.33,0.66))# 0%, 33%, 66%, 100% quantiles
       v_quan<-quantile(get(input$ghg_biv2), probs = c(0.33,0.66))#
       minx<-min(get(input$ghg_biv1))
       maxx<-max(get(input$ghg_biv1))
       miny<-min(get(input$ghg_biv2))
       maxy<-max(get(input$ghg_biv2))
       
       ptp<-ggplot(bound_tbl_Summary_gov_disp,
                   aes(x=get(input$ghg_biv1),
                       y=get(input$ghg_biv2),
                       text = paste(
                         names(var_list[grep(input$ghg_biv1, colnames(bound_tbl_Summary_gov_disp))-3]),'=',round(get(input$ghg_biv1), 3),'\n',
                         names(var_list[grep(input$ghg_biv2, colnames(bound_tbl_Summary_gov_disp))-3]),'=',round(get(input$ghg_biv2), 3),'\n'
                         )
                       )
                   ) +
      annotate("rect",
               xmin = minx,
               xmax = h_quan[[1]],
               ymin = miny,
               ymax = v_quan[[1]], fill= biv_RdBu[1], alpha = .7)  +
      annotate("rect",
               xmin = h_quan[[1]],
               xmax = h_quan[[2]],
               ymin = miny,
               ymax = v_quan[[1]],
               fill= biv_RdBu[2], alpha = .7)  +
      annotate("rect", xmin = h_quan[[2]],
               xmax = maxx,
               ymin = miny,
               ymax = v_quan[[1]],
               fill= biv_RdBu[3], alpha = .7)  +
      #=========================================================================================================================#
      annotate("rect",
               xmin = minx,
               xmax = h_quan[[1]],
               ymin = v_quan[[1]],
               ymax = v_quan[[2]], fill= biv_RdBu[4], alpha = .7)  +
      annotate("rect",
               xmin = h_quan[[1]],
               xmax = h_quan[[2]],
               ymin = v_quan[[1]],
               ymax = v_quan[[2]], fill= biv_RdBu[5], alpha = .7)  +
      annotate("rect",
               xmin = h_quan[[2]],
               xmax =  maxx,
               ymin = v_quan[[1]],
               ymax = v_quan[[2]], fill= biv_RdBu[6], alpha = .7)  +
      #=========================================================================================================================#
      annotate("rect",
               xmin = minx,
               xmax = h_quan[[1]],
               ymin = v_quan[[2]],
               ymax = maxy, fill = biv_RdBu[7], alpha = .7)  +
      annotate("rect",
               xmin = h_quan[[1]],
               xmax = h_quan[[2]],
               ymin = v_quan[[2]],
               ymax = maxy, fill = biv_RdBu[8], alpha = .7)  +
      annotate("rect",
               xmin = h_quan[[2]],
               xmax = maxx,
               ymin = v_quan[[2]],
               ymax = maxy, fill= biv_RdBu[9], alpha = .7)
    # detach(bound_tbl_Summary_gov_disp)
    ptf<-ptp +
      geom_point(mapping = aes(group = governorate),col = 'black', size = 2.2,
                 shape = 21, fill ='grey99') +
      xlim(minx,maxx)+
      ylim(miny,maxy)+
      # labs(x= names(var_list[grep(input$ghg_biv1, colnames(bound_tbl_Summary_gov_disp))-3]), # 'Variable X',
      #      y= names(var_list[grep(input$ghg_biv2, colnames(bound_tbl_Summary_gov_disp))-3]) # 'Variable Y',
      #      )+
      geom_hline(yintercept=v_quan,color="gray20",linetype=2)+
      geom_vline(xintercept=h_quan,color="gray20",linetype=2)+
      theme_bw()
    
    detach(bound_tbl_Summary_gov_disp)
    #print(ptf)

    ggplotly(ptf,tooltip = c("text","governorate"))  %>% 
      style(hoverinfo = "none", traces = 1:9) %>% 
      layout(xaxis = list(title = paste0( names(var_list[grep(input$ghg_biv1, colnames(bound_tbl_Summary_gov_disp))-3]) )),
             yaxis = list(title = paste0( names(var_list[grep(input$ghg_biv2, colnames(bound_tbl_Summary_gov_disp))-3]) ))
             ) %>% 
      highlight(on = "plotly_hover", off = "plotly_deselect", color = "red" )
    



   })
   
   
   

   
   
   
   
   
   
    output$bivariate_map<- renderPlot({
      tmapr_1<-bound_tbl_Summary_gov_disp %>% 
        dplyr::filter(between(year, input$yearly_slider_1[1], 
                              input$yearly_slider_1[2])) %>% 
        dplyr::group_by(governorate)%>%
        dplyr::summarise(Shape__Are = mean(Shape__Are),
                         co2_tons_per_km2 =  mean(co2_tons_per_km2),                     #1
                         ch4_tons_per_km2 =  mean(ch4_tons_per_km2),                     #2 ===========
                         co_tons_per_km2  =  mean(co_tons_per_km2),                      #3
                         o3_tons_per_km2  =  mean(o3_tons_per_km2),                      #4
                         population_estimate = last(population),                         #5
                         population_density  = last(population_density),                 #6
                         co2_tons_per_capita = mean(co2_tons_per_capita),                #7
                         ch4_tons_per_capita = mean(ch4_tons_per_capita),                #8
                         Residential = sum(Residential),
                         Commercial  = sum(Commercial),
                         Commercial_Residential = sum(Commercial_Residential),
                         Industrial  = sum(Industrial),
                         Agricultural= sum(Agricultural)
        ) %>%
        dplyr::mutate(
          dplyr::across(
            c('co2_tons_per_km2':'o3_tons_per_km2'),
            # mutating the values to the total population
            function(x){ ifelse(governorate == "Musandam", NA, x) }
          )
        )
      tmapr_2<- sp::merge(x =oman_gov_select_sp, y=bound_tbl_Summary_gov_disp, by="governorate",  duplicateGeoms = TRUE)
      tmapr_3<- bivariate_choropleth(bivmap_dataset = tmapr_2,
          bivmap_vars = c(input$ghg_biv1,input$ghg_biv2),
          bvColors = biv_RdBu,bivmap_scale = F)
      tmapr_3
   })
   
}) 
  

  
  #bar_chart_sn
  #==================#   barchart   #===================#
  output$bar_chart_sn<-renderPlotly({
    
    BC <- bound_tbl  %>%
      pivot_longer(cols = !c(date, governorate), names_to = "gh_gas", values_to = "value") %>%
      dplyr::filter(governorate!= 'Musandam') %>% 
      dplyr::filter(date == input$date_monthly) %>%
      dplyr::filter(gh_gas == input$ghg_monthly) %>%
      dplyr::arrange(desc(value)) %>%
      ggplot(mapping = aes(
        text = paste0(
          names(
          which(
            ghg_vars == first(input$ghg_monthly)
            )
          ),
          ' = ',round(value, 4),'\n',
          'on ',input$date_monthly,'\n',
          'Governorate: ',governorate
        )
        )
      ) +
      geom_segment(aes(x = min(value), 
                       xend = value, y = reorder(governorate, value), 
                       yend = reorder(governorate, value)),
                   alpha = .3
      ) +
      geom_point(aes(x = value, y = reorder(governorate, value)), 
                 size = 4, pch = 21, bg = 4, col = 1, show.legend = FALSE) +
      theme_bw()+
      #labs(x = paste0("Gas Variable:", names(input$ghg_monthly), ',', input$date_monthly), y = "Governorates") +
      theme(legend.position = "none",
            panel.spacing.y = unit(.1, "lines"),
            panel.spacing.x = unit(.05, "lines"),
            axis.text.x = element_text(size = 9, angle = 0, colour = "black",
                                       vjust = 1, hjust = 1)
            )
    ggplotly(BC, tooltip = 'text') %>% 
      layout(xaxis = list(title = paste0(names(ghg_vars[grep(input$ghg_monthly, colnames(bound_tbl))-1]),'\n',
                                         print(input$date_monthly)
                                         )
                          ),
             yaxis = list(title = paste0('governorate'))
             )
  })
  
  #========================== Carbon footprint Bar-chart ======================#
  
  BC_Table_3 <- ts_om %>% dplyr::filter(governorate != c('All', 'Musandam')) %>%
    dplyr::select(c('date','governorate','cfp','co2co', 'ch4')) 
  
  output$bar_chart_cf_sn<-renderPlotly({
    
    BC_Table_3 <- ts_om %>% dplyr::filter(governorate != c('All', 'Musandam')) %>%
      dplyr::select(c('date','governorate','cfp','co2co', 'ch4'))
    BC_3 <- BC_Table_3  %>%
      pivot_longer(cols = !c(date, governorate), names_to = "cf_var", values_to = "value") %>%
      dplyr::filter(governorate!= 'Musandam') %>% 
      dplyr::filter(date == input$date_cf_monthly) %>%
      dplyr::filter(cf_var == input$cf_monthly) %>%
      dplyr::arrange(desc(value)) %>%
      ggplot(
        mapping = aes(
          text = paste0(
            names(
              which(
                ghg_cf == first(input$cf_monthly)
              )
            ),
            ' = ',round(value, 3),'\n',
            'on ',input$date_cf_monthly,'\n',
            'Governorate: ',governorate
          )
        )
      ) +
      geom_segment(aes(x = min(value), 
                       xend = value, y = reorder(governorate, value), 
                       yend = reorder(governorate, value)),
                   alpha = .3
      ) +
      geom_point(aes(x = value, y = reorder(governorate, value)), 
                 size = 4, pch = 21, bg = 4, col = 1, show.legend = FALSE) +
      theme_bw()+
      labs( y = "Governorates") +
      theme(legend.position = "none",
            panel.spacing.y = unit(.1, "lines"),
            panel.spacing.x = unit(.05, "lines"),
            axis.text.x = element_text(size = 9, angle = 0, colour = "black",
                                       vjust = 1, hjust = 1)
      )
    ggplotly(BC_3, tooltip = 'text') %>% 
      layout(xaxis = list(title = paste0(names(ghg_cf[grep(input$cf_monthly, colnames(BC_Table_3))-2]),'\n',
                                         print(input$date_cf_monthly)
                                         )
                          )
             ) 
    
  })
  
  
  
  
  
  
  
  
  #================================#  #========================================#
  
  output$data_tbl_ts_ghg = DT::renderDataTable(server = FALSE,{
    DT::datatable(expt_tbl_1,
                  extensions=c("Buttons",'Scroller'),
                  options = list(dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 
                                             'excel', 'pdf', 
                                             'print'),
                                 scrollY = 800,
                                 scroller = TRUE,
                                 pageLength=15)
    )%>% formatRound(digits=c(3,5,3,3), columns = c(3, 4, 5, 6))
  })
  
  
  
  
  observe({
    ghg_month <- input$ghg_monthly
    #cfp_month <- input$cfp_monthly
    
    
    
    if (input$ghg_monthly %in% 'co'){
      
      # Monthly Plot
      mapdate <- input$date_monthly
      mapdate <- paste0("X", format(mapdate, "%Y.%m.%d"))
      
      mapRast_month <- ghglist[[ghg_month]] %>% subset(mapdate)
      
      ghgTable <- oman_gov %>%
        dplyr::mutate(
          var_value= raster::extract(x= mapRast_month,y=oman_gov, fun = mean)  %>% 
            unlist() %>% '*'( 1e6)
        )
      
      pal <- colorBin(palette = 'Greens', domain = ghgTable$var_value,5, pretty = FALSE)
      popup1 <-paste0('Month mean Total column','<br>', 'of Carbon Monoxide: ', '<b>',
                      as.character(round(ghgTable$`var_value`, digits = 2)),
                      'mg/m','<sup>',2,'</sup>','</b>','</br>','<br>','in ', as.character(ghgTable$`governorate`),'</br>')
      
      leafletProxy("map_monthly", data = ghgTable) %>% 
        clearImages() %>% clearShapes() %>%
        clearControls()%>% 
        addPolygons(
          fillColor = ~pal(var_value),
          fillOpacity = .80,
          opacity = 0.90,
          color = "black",stroke = T,weight = 0.7,
          smoothFactor = 0.5,
          popup = popup1
        )%>%  
        addLegend_decreasing(position = "bottomright", pal = pal , 
                  values =ghgTable$var_value,decreasing = T,
                  labFormat = labelFormat(digits = 2),na.label = 'No data',
                  title =  paste0('Total column of','<br>','Carbon Monoxide in mg/m','<sup>','2','</sup>','</br>'))


      
    } else if (input$ghg_monthly %in% 'o3') {
      
      # Monthly Plot
      mapdate <- input$date_monthly
      mapdate <- paste0("X", format(mapdate, "%Y.%m.%d"))
      
      mapRast_month <- ghglist[[ghg_month]] %>% subset(mapdate)

      ghgTable <- oman_gov %>%
        dplyr::mutate(
          var_value= raster::extract(x= mapRast_month,y=oman_gov, fun = mean) %>%
            unlist() %>%'*'(1e6)
          )
      pal <- colorBin(palette = 'Blues', domain = ghgTable$var_value,5, pretty = FALSE)
      popup1<-paste0('Month mean Total column','<br>', 'of Ozone: ', '<b>',
                     as.character(round(ghgTable$`var_value`, digits = 2)),
                     'mg/m','<sup>','2','</sup>','</b>','</br>','<br>','in ', as.character(ghgTable$`governorate`),'</br>')
      leafletProxy("map_monthly", data = ghgTable) %>% 
        clearControls()%>%
        clearShapes() %>% 
        clearImages() %>% 
        addPolygons(
          fillColor = ~pal(var_value),
          fillOpacity = .80,
          opacity = 0.90,
          color = "black",stroke = T,weight = 0.7,
          smoothFactor = 0.5,
          popup = popup1
        )%>% 
        addLegend_decreasing(position = "bottomright", pal = pal , 
                  values =ghgTable$var_value,decreasing = T,
                  labFormat = labelFormat(digits = 2),na.label = 'No data',
                  title =  paste0('Total column of','<br>','Ozone in mg/cm','<sup>','2','</sup>','</br>')) 
      
     
      
      
      
    } else if (input$ghg_monthly %in% 'co2') {
      
      # Monthly Plot
      mapdate <- input$date_monthly
      mapdate <- paste0("X", format(mapdate, "%Y.%m.%d"))
      mapRast_month <- ghglist[[ghg_month]] %>% subset(mapdate)

      ghgTable <- oman_gov %>%
        mutate(
          var_value= raster::extract(x= mapRast_month,y=oman_gov, fun = mean) %>% 
            unlist() %>% '*' (1.8)
        )
      
      pal <- colorBin(palette = 'Reds', domain = ghgTable$var_value,5, pretty = FALSE)
      popup1 <-paste0('Month mean concentration','<br>', 'of Carbon Dioxide: ', '<b>',
                      as.character(round(ghgTable$`var_value`, digits = 2)),
                      'mg/m','<sup>','3','</sup>','</b>','</br>','<br>','in ', as.character(ghgTable$`governorate`),'</br>')
      
      leafletProxy("map_monthly", data = ghgTable) %>% 
        clearControls()%>%
        clearShapes() %>% 
        clearImages() %>% 
        addPolygons(
          fillColor = ~pal(var_value),
          fillOpacity = .80,
          opacity = 0.90,
          color = "black",stroke = T,weight = 0.7,
          smoothFactor = 0.5, popup = popup1
        )%>% 
        addLegend_decreasing(position = "bottomright", pal = pal , 
                  values =ghgTable$var_value,decreasing = T,
                  labFormat = labelFormat(digits = 2),na.label = 'No data',
                  title =  paste0('Concentration of','<br>','Carbon Dioxide in mg/m','<sup>','3','</sup>','</br>'))
      
      
      
    } else {
      
      # Monthly Plot
      mapdate <- input$date_monthly
      mapdate <- paste0("X", format(mapdate, "%Y.%m.%d"))
      
      mapRast_month <- ghglist[[ghg_month]] %>% subset(mapdate)

      ghgTable <- oman_gov %>%
        mutate(
          var_value= raster::extract(x= mapRast_month,y=oman_gov, fun = mean) %>%
            unlist() %>%'*'(1/1000) %>% '*'(0.656)
        )
      
      pal <- colorBin(palette = 'Purples', domain = ghgTable$var_value,5, pretty = FALSE)
      popup1 <-paste0('Month mean concentration','<br>', 'of Methane: ','<b>',
                      as.character(round(ghgTable$`var_value`, digits = 4)),
                      ' mg/m','<sup>','3','</sup>','</b>','</br>','<br>','in ', as.character(ghgTable$`governorate`),'</br>')
      leafletProxy("map_monthly", data = ghgTable) %>%
        clearControls()%>%
        clearShapes() %>% 
        clearImages() %>% 
        addPolygons(
          fillColor = ~pal(var_value),
          fillOpacity = .80,
          opacity = 0.90,
          color = "black",stroke = T,weight = 0.7,
          smoothFactor = 0.5, popup = popup1
        )%>% 
        addLegend_decreasing(position = "bottomright", pal = pal , 
                  values =ghgTable$var_value,decreasing = T,
                  labFormat = labelFormat(digits = 5),na.label = 'No data',
                  title = paste0('Concentration of','<br>','Methane in mg/m','<sup>','3','</sup>','</br>'))
      
      
    }
    
  })
  
  
  
  
  
  
  
  ###==========================================================================#
  
  observe({
    
    TS_Table_2 <- ts_om %>% dplyr::filter(governorate != 'All')
    cf_monthly <- input$cf_monthly
    mapdatecf  <- input$date_cf_monthly
    mapdatecf  <- paste0(format(mapdatecf, "%Y-%m-%d"))
    
    mTS_2 <- TS_Table_2 %>%
      dplyr::select(c('date','governorate','cfp','co2co', 'ch4')) %>%
      base::subset(date== input$date_cf_monthly) %>%
      dplyr::left_join(oman_gov_select, by=c('governorate'='governorate'))
    # 
    # 
    mTS_2 <- within(mTS_2, cfp[governorate == 'Musandam']   <- NA)
    mTS_2 <- within(mTS_2, co2co[governorate == 'Musandam'] <- NA)
    mTS_2 <- within(mTS_2, ch4[governorate == 'Musandam']   <- NA)
    # 
    # 
    if (input$cf_monthly %in% 'cfp') {
    # 
    # 
    #   # Monthly Plot
     
    # 
       cf_table <- mTS_2 %>%
         base::subset(date== input$date_cf_monthly) %>%
         dplyr::select(c('date','governorate','cfp')) %>%
         dplyr::left_join(oman_gov_select, by=c('governorate'='governorate')) %>% st_as_sf() %>% 
         dplyr::filter(date== input$date_cf_monthly)
    # 
       pal <- colorBin(palette = 'YlOrBr', domain = cf_table$`cfp`,5, bins = c(0,15,30,50,250), pretty = FALSE)
       popup1 <-paste0('Month mean Total', 'Carbon Footprint: ',
                       '<br>', '<b>',as.character(round(cf_table$`cfp`, digits = 2)),'</b>',' in CO2e Tons per capita','</br>',
                       '<br>','in ', as.character(cf_table$`governorate`) ,'</br>')
    # 
       leafletProxy("map_cf_monthly", data = cf_table) %>%
         clearImages() %>% clearShapes() %>%
         clearControls()%>%
         addPolygons(
           fillColor = ~pal(cfp),
           fillOpacity = .80,
           opacity = 0.90,
           color = "black",stroke = T,weight = 0.7,
           smoothFactor = 0.5,
           popup = popup1
           )%>%
         addLegend_decreasing(position = "bottomright", pal = pal ,
                              values =cf_table$cfp,decreasing = T,
                              labFormat = labelFormat(digits = 2),na.label = 'No data',
                              title =  paste0('Month mean Total ',
                                              '<br>','Carbon Footprint in ', '</br>',
                                              '<br>','CO2e Tons per capita ', '</br>'))
    # 
    # 
    # 
     } else if (input$cf_monthly %in% 'co2co') {

      # Monthly Plot
      cf_table <- mTS_2 %>%
        base::subset(date== input$date_cf_monthly) %>%
        dplyr::select(c('date','governorate','co2co')) %>%
        dplyr::left_join(oman_gov_select, by=c('governorate'='governorate')) %>% st_as_sf() %>% 
        dplyr::filter(date== input$date_cf_monthly)

      pal <- colorBin(palette = 'Reds', domain = cf_table$co2co,5, bins = c(0,15,30,50,250), pretty = FALSE)
      popup1 <-paste0('Month mean Total','<br>', 'Carbon Footprint',' (CO','<sub>','2 ','</sub>','&' ,'CO): ', '<b>',
                      as.character(round(cf_table$`co2co`, digits = 2)),
                      '</b>',' in CO2e Tons per capita','</br>',
                      '<br>','in ', as.character(cf_table$`governorate`) ,'</br>')

      leafletProxy("map_cf_monthly", data = cf_table) %>%
        clearImages() %>% clearShapes() %>%
        clearControls()%>%
        addPolygons(
          fillColor = ~pal(co2co),
          fillOpacity = .80,
          opacity = 0.90,
          color = "black",stroke = T,weight = 0.7,
          smoothFactor = 0.5,
          popup = popup1
        )%>%
        addLegend_decreasing(position = "bottomright", pal = pal ,
                             values =cf_table$co2co,decreasing = T,
                             labFormat = labelFormat(digits = 2),na.label = 'No data',
                             title =  paste0('Month mean Total ',
                                             '<br>','Carbon Footprint ',' (CO','<sub>','2 ','</sub>','&' ,'CO) ', ' in ', '</br>',
                                             '<br>','CO2e Tons per capita ', '</br>'))


    }
    else {

      # Monthly Plot

      cf_table <- mTS_2 %>%
        base::subset(date== input$date_cf_monthly) %>%
        dplyr::select(c('date','governorate','ch4')) %>%
        dplyr::left_join(oman_gov_select, by=c('governorate'='governorate')) %>% st_as_sf() %>% 
        dplyr::filter(date== input$date_cf_monthly)

      pal <- colorBin(palette = 'Purples', domain = cf_table$ch4,5, bins = c(0,.3,.5,.7,4.1), pretty = FALSE)
      popup1 <-paste0('Month mean Total','<br>', 'Carbon Footprint (Methane CO2e): ', '<b>',
                      as.character(round(cf_table$`ch4`, digits = 2)),
                      '</b>',' in CO2e Tons per capita','</br>',
                      '<br>','in ', as.character(cf_table$`governorate`) ,'</br>')

      leafletProxy("map_cf_monthly", data = cf_table) %>%
        clearImages() %>% clearShapes() %>%
        clearControls()%>%
        addPolygons(
          fillColor = ~pal(ch4),
          fillOpacity = .80,
          opacity = 0.90,
          color = "black",stroke = T,weight = 0.7,
          smoothFactor = 0.5,
          popup = popup1
        )%>%
        addLegend_decreasing(position = "bottomright", pal = pal ,
                             values =cf_table$ch4,decreasing = T,
                             labFormat = labelFormat(digits = 2),na.label = 'No data',
                             title =  paste0('Month mean Total ',
                                             '<br>','Carbon Footprint (Methane CO2e) in ', '</br>',
                                             '<br>','CO2e Tons per capita ', '</br>')
                             )

    }
  # 
  })
  ###==========================================================================#
  

  ###==========================================================================#
  # Observe for state input in Map
  observe({
    # Monthly GHG mean map
    
    area_month <- input$gov_month
    mapdate <- input$date_monthly
    ghg_var    <- input$ghg_monthly
    dispdate <- paste0("X", format(mapdate, "%Y.%m.%d"))
    
    # Monthly Carbon footprint map
    
    area_cf_month <- input$gov_cf_month
    mapdatecf <- input$date_cf_monthly
    cf_var    <- input$cf_monthly
    discfpdate <- paste0(format(mapdatecf, "%Y-%m-%d"))
    
    # Change the text in Time Series
    output$mt_govName <- renderUI({
      h5(id = "mt_govName", paste("GHG monthly mean Time Series of", ifelse(area_month != "All", area_month, "All of Oman")), style = "font-size:13px;")
    
    })
    output$ghg_indicators<- renderUI({
      h5(id = "ghg_indicators", paste("GHG monthly mean ",format(input$date_monthly, "%m/%Y"),"of", 
                                      ifelse(area_month != "All", area_month, "All of Oman")), style = "font-size:13px;")
    })
    
    output$cf_indicators<- renderUI({
      h5(id = "ghg_indicators", paste("Carbon footprint indicators, ",format(input$date_cf_monthly, "%m/%Y"),"of", 
                                      ifelse(area_cf_month != "All", area_cf_month, "All of Oman")), style = "font-size:13px;")
    })
    
    output$mt_cf_govName <- renderUI({
      h5(id = "mt_govName", paste("Carbon footprint monthly mean Time Series of", ifelse(area_cf_month != "All", area_cf_month, "All of Oman")), style = "font-size:13px;")
      
    })
    
    mapdate <- paste0("X", format(mapdate, "%Y.%m.%d"))
    
    mapRast_monthco2 <- ghglist[['co2']] %>% subset(mapdate)
    mapRast_monthch4 <- ghglist[['ch4']] %>% subset(mapdate)
    mapRast_monthco <- ghglist[['co']] %>% subset(mapdate)
    mapRast_montho3 <- ghglist[['o3']] %>% subset(mapdate)
    
    ghgTable2 <- oman_gov %>%
      mutate(
        co2_value= raster::extract(x= mapRast_monthco2,y=oman_gov, fun = mean) %>%
          unlist() %>% '*'(1.8),
        ch4_value= raster::extract(x= mapRast_monthch4,y=oman_gov, fun = mean) %>%
          unlist() %>% '*'(1/1000) %>%'*'(0.656),
        co_value= raster::extract(x= mapRast_monthco,y=oman_gov, fun = mean) %>%
          unlist() %>% '*'(1e6),
        o3_value= raster::extract(x= mapRast_montho3,y=oman_gov, fun = mean) %>%
          unlist() %>% '*'(1e6)
      ) %>% sf::st_drop_geometry()
    
    if (area_month != "All") {
      selectedvalueco2<-ghgTable2 %>% dplyr::filter(governorate==area_month) %>% 
        dplyr::select(co2_value)%>% 
        round(digits = 1) %>% as.character()
      
      selectedvaluech4<-ghgTable2 %>% dplyr::filter(governorate==area_month) %>% 
        dplyr::select(ch4_value)%>% 
        round(digits = 3) %>% as.character()
      
      selectedvalueco<-ghgTable2 %>% dplyr::filter(governorate==area_month) %>% 
        dplyr::select(co_value)%>% 
        round(digits = 1) %>% as.character()
      
      selectedvalueo3<-ghgTable2 %>% dplyr::filter(governorate==area_month) %>% 
        dplyr::select(o3_value)%>% 
        round(digits = 0) %>% as.character()
      
      output$CO2_index<-renderValueBox({
        valueBox(
          paste0(selectedvalueco2),
          color = "red",
          subtitle =  'CO2 mg/m3'
        )
        
      })
      output$CH4_index<-renderValueBox({
        valueBox(
          paste0(selectedvaluech4),
          color = "purple",
          subtitle = tags$p(paste0('CH4 in mg/m3'), style = "font-size: 90%;")
        )
      })
      output$CO_index<-renderValueBox({
        valueBox(
          paste0(selectedvalueco),
          color = "green",
          subtitle = 'CO in mg/m2'
        )
      })
      output$O3_index<-renderValueBox({
        valueBox(
          paste0(selectedvalueo3),
          color = "blue",
          subtitle = 'O3 in mg/m2'
        )
      })
    } else {
      selectedvalueco2all <- ghgTable2 %>% dplyr::filter(!is.na(co2_value)) %>% dplyr::select(co2_value) %>%
        summarise(mean=mean(co2_value)) %>% round(digits = 1)%>% as.character()
      
      selectedvaluech4all <- ghgTable2 %>% dplyr::filter(!is.na(ch4_value)) %>% dplyr::select(ch4_value) %>%
        dplyr::summarise(mean=mean(ch4_value)) %>% round(digits = 3)%>% as.character()
      
      
      selectedvaluecoall  <- ghgTable2 %>% dplyr::filter(!is.na(co_value)) %>% dplyr::select(co_value) %>%
        dplyr::summarise(mean=mean(co_value)) %>% round(digits = 1)%>% as.character()
      
      selectedvalueo3all  <- ghgTable2 %>% dplyr::filter(!is.na(o3_value)) %>% dplyr::select(o3_value) %>%
        dplyr::summarise(mean=mean(o3_value)) %>% round(digits = 0)%>% as.character()
      
      
      
      output$CO2_index<-renderValueBox({
        valueBox(
          value = tags$p(paste0(selectedvalueco2all), style = "font-size: 80%;"),
          color = "red",
          subtitle = 'CO2 mg/m3'
          )
        
      })
      output$CH4_index<-renderValueBox({
        valueBox(value = tags$p(paste0(selectedvaluech4all), style = "font-size: 80%;"),
          color = "purple",
          subtitle = tags$p(paste0('CH4 in mg/m3'), style = "font-size: 90%;")
        )
      })
      output$CO_index<-renderValueBox({
        valueBox(
          value = tags$p(paste0(selectedvaluecoall), style = "font-size: 80%;"),
          color = "green",
          subtitle = 'CO in mg/m2'
        )
      })
      output$O3_index<-renderValueBox({
        valueBox(
          value = tags$p(paste0(selectedvalueo3all), style = "font-size: 80%;"),
          color = "blue",
          subtitle = 'O3 in mg/m2'
        )
      })
    }
    

###==========   cfp indicators   ===============###    
    
    
    
    
    
    
    if (area_cf_month != "All") {
      selectedvaluecfp<-TS_Table_2 %>% 
        base::subset(date== input$date_cf_monthly) %>%
        dplyr::filter(governorate==area_cf_month) %>% 
        dplyr::select(cfp)%>% 
        round(digits = 3) %>% as.character()
      
      selectedvaluech4_cf<-TS_Table_2 %>% 
        base::subset(date== input$date_cf_monthly) %>%
        dplyr::filter(governorate==area_cf_month) %>% 
        dplyr::select(ch4)%>% 
        round(digits = 4) %>% as.character()
      
      selectedvalueco2co<-TS_Table_2 %>% 
        base::subset(date== input$date_cf_monthly) %>%
        dplyr::filter(governorate==area_cf_month) %>% 
        dplyr::select(co2co)%>% 
        round(digits = 3) %>% as.character()

      
      output$TCF_index<-renderValueBox({
        valueBox(
          value =  tags$p(paste0(selectedvaluecfp), style = "font-size: 85%;"),
          color = "yellow",
          subtitle =  tags$p(paste0('Total Carbon Footprint','\n','(Tons per capita)'), style = "font-size: 95%;")
        )
        
      })
      output$CO2COCF_index<-renderValueBox({
        valueBox(
          value = tags$p(paste0(selectedvalueco2co), style = "font-size: 85%;"),
          color = "red",
          subtitle = tags$p(paste0('Carbon Footprint (CO2 & CO)','\n','(Tons per capita)'), style = "font-size: 95%;")
        )
      })
      output$CH4CF_index<-renderValueBox({
        valueBox(
          value = tags$p(paste0(selectedvaluech4_cf), style = "font-size: 85%;"),
          color = "purple",
          subtitle = tags$p(paste0('CO2 equivelant in Methane ','\n','(Tons per capita)'), style = "font-size: 95%;")
        )
      })
   
    } else {
      selectedvaluecfpall <- TS_Table_2 %>% 
        dplyr::filter(governorate !='Musandam') %>%
        base::subset(date== input$date_cf_monthly) %>%
        dplyr::filter(!is.na(cfp)) %>% dplyr::select(cfp) %>%
        summarise(mean=sum(cfp)) %>% round(digits = 3) %>% as.character()
      
      selectedvaluech4_cfall <- TS_Table_2 %>% 
        dplyr::filter(governorate !='Musandam') %>%
        base::subset(date== input$date_cf_monthly) %>%
        dplyr::filter(!is.na(ch4)) %>% dplyr::select(ch4) %>%
        dplyr::summarise(mean=sum(ch4)) %>% round(digits = 4)%>% as.character()
      
      
      selectedvalueco2coall  <- TS_Table_2 %>% 
        dplyr::filter(governorate !='Musandam') %>%
        dplyr::filter(date==mapdatecf)%>%
        dplyr::filter(!is.na(co2co)) %>% dplyr::select(co2co) %>%
        dplyr::summarise(mean=sum(co2co)) %>% round(digits = 3)%>% as.character()
      
      
      
      
      output$TCF_index<-renderValueBox({
        valueBox(
          value = tags$p(paste0(selectedvaluecfpall), style = "font-size: 85%;"),
          color = "yellow",
          subtitle = tags$p(paste0('Total Carbon Footprint','\n','(Tons per capita)'), style = "font-size: 95%;")
        )
        
      })
      output$CO2COCF_index<-renderValueBox({
        valueBox(value = tags$p(paste0(selectedvalueco2coall),  style = "font-size: 85%;"),
                 color = "red",
                 subtitle = tags$p(paste0('Carbon Footprint (CO2 & CO)','\n','(Tons per capita)'), style = "font-size: 95%;")
        )
      })
      output$CH4CF_index<-renderValueBox({
        valueBox(
          value = tags$p(paste0(selectedvaluech4_cfall), style = "font-size: 85%;"),
          color = "purple",
          subtitle = tags$p(paste0('CO2e equivelant in Methane ','\n','(Tons per capita)'), style = "font-size: 95%;")
        )
      })

    }
    
    
    
    ###==========   cfp indicators   ===============### 
    
    
    
    
    
    ###### MAP
    # Monthly Map
    if (area_month != "All") {
      area_ghg_month <- subset(oman_gov, oman_gov$governorate == area_month)
      area_ext_month <- area_ghg_month %>% extent()
      
      
      leafletProxy("map_monthly", data = area_ghg_month) %>%
        removeShape("selectedgov") %>%
        flyTo(
          lng = mean(c(area_ext_month[1], area_ext_month[2])),
          lat = mean(c(area_ext_month[3], area_ext_month[4])),
          zoom = 7
        ) %>%
        addPolygons(
          layerId = "selectedgov",
          color = "turquoise",
          opacity = .80,
          fillOpacity = 0.05
        )
      
    } else {
      leafletProxy("map_monthly") %>%
        removeShape("selectedgov") %>%
        flyTo(lng = mean(c(oman_ext_month[1], oman_ext_month[2])),
              lat = mean(c(oman_ext_month[3], oman_ext_month[4])), 
              zoom = 6.5)
    }
    
    
    
    
    ###### MAP
    
    
    
    # area_cf_month <- input$gov_cf_month
    # mapdatecf <- input$date_cf_monthly
    # cf_var    <- input$cf_monthly
    # discfpdate <- paste0(format(mapdatecf, "%Y-%m-%d")
    
    # Monthly Carbon footprint Map
     if (area_cf_month != "All") {
       area_cf_cf_month <- subset(oman_gov, oman_gov$governorate == area_cf_month)
       area_ext_month_cf <- area_cf_cf_month %>% extent()
    #   
    #   
       leafletProxy("map_cf_monthly", data = area_cf_cf_month) %>%
         removeShape("selectedgov") %>%
         flyTo(
           lng = mean(c(area_ext_month_cf[1], area_ext_month_cf[2])),
           lat = mean(c(area_ext_month_cf[3], area_ext_month_cf[4])),
           zoom = 6
         ) %>%
         addPolygons(
           layerId = "selectedgov",
           color = "turquoise",
           opacity = .80,
           fillOpacity = 0.05
         )
    #   
     } else {
       leafletProxy("map_cf_monthly") %>%
         removeShape("selectedgov") %>%
         flyTo(lng = mean(c(oman_ext_month[1], oman_ext_month[2])),
               lat = mean(c(oman_ext_month[3], oman_ext_month[4])), 
               zoom = 6.5)
     }
    
    
    
    
    
    
    
    
    
    
    
    ###### TIME - SERIES
    TS_Table <- timeSeries_extract0r("2012-01-01", "2020-12-01", area_month)
    
    output$monthlyTS <- renderPlotly({
      
      
      mTS <- TS_Table %>% 
        pivot_longer(cols = -date, names_to = "gh_gas", values_to = "value") %>% 
        mutate(gh_gas = fct_relevel(gh_gas, c('co2', 'ch4', 'co', 'o3'))) %>% 
        ggplot() +
        geom_line(aes(x = date, y = value, color = gh_gas), show.legend = FALSE) +
        facet_wrap(
          gh_gas ~ .,
          ncol = 1,
          scales = "free_y",
          labeller = labeller(
            gh_gas = c(
              "co2" = 'Carbon Dioxide (CO2) in mg/m3',
              "ch4" = 'Methane (CH4) in mg/m3',
              "co" =  'Carbon Monoxide (CO) in mg/m2',
              "o3" =  'Ozone (O3) in mg/m2'
            ))
        )+
        scale_colour_manual(values = c('#e41a1c','#984ea3','#4daf4a','#377eb8'))+
        scale_x_date(date_breaks = "6 months" , date_labels = "%m/%y")+
        ggthemes::theme_few()+
        labs(x = "Date", y = "") + 
        theme(legend.position = "none", 
              panel.spacing.y = unit(.1, "lines"),
              panel.spacing.x = unit(.05, "lines"),
              panel.margin.y = unit(0.19, "lines"),
              axis.text.x = element_text(size = 9, angle = 20, colour = "black",
                                         vjust = 1, hjust = 1))
      ggplotly(mTS)
    })
    
    
    # expt_tbl_1
    observe({
      
      
      
      
      
      
      output$summary<- renderPrint({
        # input$summ_ghg_area
        # input$summ_ghg_monthly
        if (input$summ_ghg_area !='All'){
          expt_tbl_1 %>% filter(governorate==input$summ_ghg_area)%>% skim()
          
        }else{
          expt_tbl_1 %>% skim()  
        }
        
      
        }) 
      
      if(input$summ_ghg_monthly %in% 'co2' & input$summ_ghg_area =='All'){
        output$Summaryplot<- renderPlot({
          par(cex=1, cex.lab = 2, cex.axis=1.65, cex=2)
          plot(co2_om_stl)
        }) 
      } else if(input$summ_ghg_monthly %in% 'ch4' & input$summ_ghg_area=='All'){
        output$Summaryplot<- renderPlot({
          par(cex=1, cex.lab = 2, cex.axis=1.65, cex=2)
          plot(ch4_om_stl)
        })
      } else if(input$summ_ghg_monthly %in% 'co' & input$summ_ghg_area=='All'){
        output$Summaryplot<- renderPlot({
          par(cex=1, cex.lab = 2, cex.axis=1.65, cex=2)
          plot(co_om_stl)
        })
        }else if(input$summ_ghg_monthly %in% 'o3' & input$summ_ghg_area=='All'){
          output$Summaryplot<- renderPlot({
            par(cex=1, cex.lab = 2, cex.axis=1.65, cex=2)
            plot(o3_om_stl)
          }) ###   ***   ###   ***   ###
      } else if(input$summ_ghg_monthly %in% 'co2' & input$summ_ghg_area !='All'){
        output$Summaryplot<- renderPlot({
          
          
          summ_tbl_1<-expt_tbl_1 %>%
            filter(governorate==input$summ_ghg_area) %>% 
            dplyr::group_by(date) %>% 
            dplyr::summarise(`CO2 Tonnes/km2`=mean(`CO2 Tonnes/km2`),
                             `CH4 Tonnes/km2`=mean(`CH4 Tonnes/km2`),
                             `CO Tonnes/km2` =mean(`CO Tonnes/km2`),
                             `O3 Tonnes/km2` =mean(`O3 Tonnes/km2`))
          
          co2_ts_om_1 <- ts(data = summ_tbl_1$`CO2 Tonnes/km2`, 
                          start = c(year(first(summ_tbl_1$date)), month(first(summ_tbl_1$date))), 
                          end = c(year(last(summ_tbl_1$date)), month(last(summ_tbl_1$date))), 
                          frequency = 12)
          
          co2_sel_stl <- stl(x = co2_ts_om_1, s.window = "periodic")

          
          par(cex=1, cex.lab = 2, cex.axis=1.65, cex=2)
          plot(co2_sel_stl)
        }) 
      } else if(input$summ_ghg_monthly %in% 'ch4' & input$summ_ghg_area!='All'){
        output$Summaryplot<- renderPlot({
          
          summ_tbl_1<-expt_tbl_1 %>%
            filter(governorate==input$summ_ghg_area) %>% 
            dplyr::group_by(date) %>% 
            dplyr::summarise(`CO2 Tonnes/km2`=mean(`CO2 Tonnes/km2`),
                             `CH4 Tonnes/km2`=mean(`CH4 Tonnes/km2`),
                             `CO Tonnes/km2` =mean(`CO Tonnes/km2`),
                             `O3 Tonnes/km2` =mean(`O3 Tonnes/km2`))
          
          ch4_ts_om_1 <- ts(data = summ_tbl_1$`CH4 Tonnes/km2`, 
                            start = c(year(first(summ_tbl_1$date)), month(first(summ_tbl_1$date))), 
                            end = c(year(last(summ_tbl_1$date)), month(last(summ_tbl_1$date))), 
                            frequency = 12)
          
          ch4_sel_stl <- stl(x = ch4_ts_om_1, s.window = "periodic")
          
          
          par(cex=1, cex.lab = 2, cex.axis=1.65, cex=2)
          plot(ch4_sel_stl)
          
          
        })
      } else if(input$summ_ghg_monthly %in% 'co' & input$summ_ghg_area!='All'){
        output$Summaryplot<- renderPlot({
          
          
          summ_tbl_1<-expt_tbl_1 %>%
            filter(governorate==input$summ_ghg_area) %>% 
            dplyr::group_by(date) %>% 
            dplyr::summarise(`CO2 Tonnes/km2`=mean(`CO2 Tonnes/km2`),
                             `CH4 Tonnes/km2`=mean(`CH4 Tonnes/km2`),
                             `CO Tonnes/km2` =mean(`CO Tonnes/km2`),
                             `O3 Tonnes/km2` =mean(`O3 Tonnes/km2`))
          
          co_ts_om_1 <- ts(data = summ_tbl_1$`CO Tonnes/km2`, 
                            start = c(year(first(summ_tbl_1$date)), month(first(summ_tbl_1$date))), 
                            end = c(year(last(summ_tbl_1$date)), month(last(summ_tbl_1$date))), 
                            frequency = 12)
          
          co_sel_stl <- stl(x = co_ts_om_1, s.window = "periodic")
          
          
          par(cex=1, cex.lab = 2, cex.axis=1.65, cex=2)
          plot(co_sel_stl)
          
          
          
        })
      }else if(input$summ_ghg_monthly %in% 'o3' & input$summ_ghg_area!='All'){
        output$Summaryplot<- renderPlot({
          
          
          summ_tbl_1<-expt_tbl_1 %>%
            filter(governorate==input$summ_ghg_area) %>% 
            dplyr::group_by(date) %>% 
            dplyr::summarise(`CO2 Tonnes/km2`=mean(`CO2 Tonnes/km2`),
                             `CH4 Tonnes/km2`=mean(`CH4 Tonnes/km2`),
                             `CO Tonnes/km2` =mean(`CO Tonnes/km2`),
                             `O3 Tonnes/km2` =mean(`O3 Tonnes/km2`))
          
          o3_ts_om_1 <- ts(data = summ_tbl_1$`O3 Tonnes/km2`, 
                           start = c(year(first(summ_tbl_1$date)), month(first(summ_tbl_1$date))), 
                           end = c(year(last(summ_tbl_1$date)), month(last(summ_tbl_1$date))), 
                           frequency = 12)
          
          o3_sel_stl <- stl(x = o3_ts_om_1, s.window = "periodic")
          
          
          par(cex=1, cex.lab = 2, cex.axis=1.65, cex=2)
          plot(o3_sel_stl)
          
          
          
          
        })
        
      }
      
    })

    
    

    
  })
  

    
  
 
  
}












# 
# # Run the application 
shinyApp(ui = UI, server = SERVER)

