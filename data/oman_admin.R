governorates_choices <- c(
  "All" = "All",
  "Muscat" = "Muscat",
  
  "Al - Batinah North" = "Al - Batinah North",#1
  
  "Al - Batinah South" = "Al - Batinah South",
  
  "Musandam" = "Musandam",
  
  "Al - Dhahira" = "Al - Dhahira",#2
  
  "Al - Dakhliyah" = "Al - Dakhliyah",
  
  "Al - Sharqiyah South" = "Al - Sharqiyah South",
  
  "Al - Sharqiyah North" = "Al - Sharqiyah North",
  
  "Dhofar" = "Dhofar",
  "Al - Wusta" = "Al - Wusta",
  
  "Al - Buraymi" = "Al - Buraymi"
)

governorates_choices <- governorates_choices[order(governorates_choices)]

provinces_choices <- c(
  "All"                             ="All",
  "Mutrah"                          = "Mutrah",
  "Bawshar"                         = "Bawshar",
  "Al - Seeb"                       = "Al - Seeb",                    
  "Al - Amrat"                      = "Al - Amrat",
  "Muscat"                          = "Muscat",
  "Qurayyat"                        = "Qurayyat",
  "Sohar"                           = "Sohar",
  "Shinas"                          = "Shinas",
  "Liwa"                            = "Liwa",
  "Saham"                           = "Saham",
  "Al - Khaburah"                   = "Al - Khaburah",
  "Al - Suwayq"                     = "Al - Suwayq",
  "Al - Rustaq"                     = "Al - Rustaq", 
  "Nakhal"                          = "Nakhal",
  "Wadi al - Maawil"                = "Wadi al - Maawil",
  "Al - Awabi"                      = "Al - Awabi",
  "Al - Musanah"                    = "Al - Musanah",
  "Barka"                           ="Barka",
  "Khasab"                          = "Khasab",
  "Bukha"                           = "Bukha",
  "Daba"                            = "Daba",
  "Madha"                           = "Madha",
  "Ibri"                            = "Ibri",
  "Yanqul"                          = "Yanqul",
  "Dank"                            = "Dank",
  "Nizwa"                           = "Nizwa",
  "Samail"                          = "Samail",
  "Bahla"                           = "Bahla",
  "Adam"                            = "Adam",
  "Al - Hamra"                      = "Al - Hamra",
  "Manah"                           = "Manah",
  "Izki"                            = "Izki",
  "Bid Bid"                         = "Bid Bid",
  "Sur"                             = "Sur",
  "Al - Kamil wa al - Wafi"         = "Al - Kamil wa al - Wafi",
  "Jaalan Bani Bu Hasan"            = "Jaalan Bani Bu Hasan",
  "Jaalan Bani Bu Ali"              = "Jaalan Bani Bu Ali",
  "Masirah"                         = "Masirah",
  "Ibra"                            = "Ibra",
  "Bidiyah"                         = "Bidiyah",
  "Al - Qabil"                      = "Al - Qabil",
  "Al - Mudaybi"                    = "Al - Mudaybi",
  "Dima wa al - Taiyyin"            = "Dima wa al - Taiyyin",
  "Wadi Bani Khalid"                = "Wadi Bani Khalid",
  "Hayma"                           = "Hayma",
  "Muhut"                           = "Muhut",
  "Al - Duqm"                       = "Al - Duqm",
  "Al - Jazer"                      = "Al - Jazer",
  "Salalah"                         = "Salalah",
  "Thumrayt"                        ="Thumrayt",
  "Taqah"                           = "Taqah",
  "Mirbat"                          = "Mirbat",
  "Sadh"                            = "Sadh",
  "Rakhyut"                         = "Rakhyut",
  "Dalkut"                          = "Dalkut",
  "Muqshin"                         = "Muqshin",
  "Shalim wa juzur al - Hallaniyat" = "Shalim wa juzur al - Hallaniyat",
  "Al - Mazuynah"                   = "Al - Mazuynah",
  "Al - Buraymi"                    = "Al - Buraymi",
  "Mahdah"                          = "Mahdah",
  "Al - Sunaynah"                   = "Al - Sunaynah")


provinces_choices <- provinces_choices[order(provinces_choices)]


governorates_na <- c(
  "Muscat",
  
  "Al - Batinah North",
  
  "Al - Batinah South",
  
  "Musandam",
  
  "Al - Dhahira",#2
  
  "Al - Dakhliyah",
  
  "Al - Sharqiyah South",
  
  "Al - Sharqiyah North",
  
  "Dhofar",
  
  "Al - Wusta",
  
  "Al - Buraymi"
)




dates_of_data<- list("2012-01-01","2012-02-01","2012-03-01","2012-04-01","2012-05-01","2012-06-01","2012-07-01","2012-08-01","2012-09-01","2012-10-01","2012-11-01"
                     ,"2012-12-01","2013-01-01","2013-02-01","2013-03-01","2013-04-01","2013-05-01","2013-06-01","2013-07-01","2013-08-01","2013-09-01","2013-10-01"
                     ,"2013-11-01","2013-12-01","2014-01-01","2014-02-01","2014-03-01","2014-04-01","2014-05-01","2014-06-01","2014-07-01","2014-08-01","2014-09-01"
                     ,"2014-10-01","2014-11-01","2014-12-01","2015-01-01","2015-02-01","2015-03-01","2015-04-01","2015-05-01","2015-06-01","2015-07-01","2015-08-01"
                     ,"2015-09-01","2015-10-01","2015-11-01","2015-12-01","2016-01-01","2016-02-01","2016-03-01","2016-04-01","2016-05-01","2016-06-01","2016-07-01"
                     ,"2016-08-01","2016-09-01","2016-10-01","2016-11-01","2016-12-01","2017-01-01","2017-02-01","2017-03-01","2017-04-01","2017-05-01","2017-06-01"
                     ,"2017-07-01","2017-08-01","2017-09-01","2017-10-01","2017-11-01","2017-12-01","2018-01-01","2018-02-01","2018-03-01","2018-04-01","2018-05-01"
                     ,"2018-06-01","2018-07-01","2018-08-01","2018-09-01","2018-10-01","2018-11-01","2018-12-01","2019-01-01","2019-02-01","2019-03-01","2019-04-01"
                     ,"2019-05-01","2019-06-01","2019-07-01","2019-08-01","2019-09-01","2019-10-01","2019-11-01","2019-12-01","2020-01-01","2020-02-01","2020-03-01"
                     ,"2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01")


