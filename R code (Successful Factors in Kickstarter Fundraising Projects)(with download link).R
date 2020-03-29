
# Codes for data analysis in Kickstarter projects


URL      <- "https://drive.google.com/u/2/uc?id=1Yq-ShI0J_lPgF2rRFhJ9nP6kAJORYXcM&export=download"

destfile <- "C:/data_analysis/kickstarter.csv"

download.file(URL, destfile)

kickstarter <- read.csv(destfile)

summary(kickstarter)

kickstarter_cleaned <- kickstarter[,-c(1,2,3,5,6,8,9,11,13,15)]

head(kickstarter_cleaned, 20)

summary(kickstarter_cleaned)


ste <- kickstarter$state

place <-kickstarter$country

count_success_us <- length(intersect(which(ste=="successful"), which(place=="US")))

count_fail_us <- length(intersect(which(ste!="successful"), which(place=="US")))

success_rate_us = (count_success_us / (count_success_us + count_fail_us)) 


count_success_gb <- length(intersect(which(ste=="successful"), which(place=="GB")))

count_fail_gb <- length(intersect(which(ste!="successful"), which(place=="GB")))

success_rate_gb = (count_success_gb / (count_success_gb + count_fail_gb)) 


count_success_ca <- length(intersect(which(ste=="successful"), which(place=="CA")))

count_fail_ca <- length(intersect(which(ste!="successful"), which(place=="CA")))

success_rate_ca = (count_success_ca / (count_success_ca + count_fail_ca)) 


count_success_au <- length(intersect(which(ste=="successful"), which(place=="AU")))

count_fail_au <- length(intersect(which(ste!="successful"), which(place=="AU")))

success_rate_au = (count_success_au / (count_success_au + count_fail_au)) 


count_success_de <- length(intersect(which(ste=="successful"), which(place=="DE")))

count_fail_de <- length(intersect(which(ste!="successful"), which(place=="DE")))

success_rate_de = (count_success_de / (count_success_de + count_fail_de)) 



combined <- c(success_rate_us, success_rate_gb, success_rate_ca, success_rate_au, success_rate_de)

barplot(combined, ylim=c(0,1), main="Successful Rate in Top 5 Countries of Kickerstarter Projects", horiz=FALSE,
        names.arg=c("United States", "Britain", "Canada", "Australia", "Germany"))

head(combined)


gl <- kickstarter$goal

count_success_I <- length(intersect(which(ste=="successful"), which(gl<=5000)))

count_fail_I <- length(intersect(which(ste!="successful"), which(gl<=5000)))

success_rate_I = (count_success_I / (count_success_I + count_fail_I)) 


count_success_II <- length(intersect(which(ste=="successful"), which(gl<=15000))) - count_success_I

count_fail_II <- length(intersect(which(ste!="successful"), which(gl<=15000))) - count_fail_I

success_rate_II = (count_success_II / (count_success_II + count_fail_II)) 


count_success_III <- length(intersect(which(ste=="successful"), which(gl<=30000))) - (count_success_I + count_success_II)

count_fail_III <- length(intersect(which(ste!="successful"), which(gl<=30000))) - (count_fail_I + count_fail_II)

success_rate_III = (count_success_III / (count_success_III + count_fail_III)) 


count_success_IV <- length(intersect(which(ste=="successful"), which(gl<=100000))) - (count_success_I + count_success_II + count_success_III)

count_fail_IV <- length(intersect(which(ste!="successful"), which(gl<=100000))) - (count_fail_I + count_fail_II + count_fail_III)

success_rate_IV = (count_success_IV / (count_success_IV + count_fail_IV)) 


count_success_V <- length(intersect(which(ste=="successful"), which(gl>100000))) 

count_fail_V <- length(intersect(which(ste!="successful"), which(gl>100000))) 

success_rate_V = (count_success_V / (count_success_V + count_fail_V)) 

combined <- c(success_rate_I, success_rate_II, success_rate_III, success_rate_IV, success_rate_V)


barplot(combined, ylim=c(0,1), main="Successful Rate in 5 Catagories of Goals set by fundraisers", horiz=FALSE,
        names.arg=c("I", "II", "III", "IV", "V"))

head(combined)


category <- kickstarter$main_category

count_success_film <- length(intersect(which(ste=="successful"), which(category=="Film & Video")))

count_fail_film <- length(intersect(which(ste!="successful"), which(category=="Film & Video")))

success_rate_film = (count_success_film / (count_success_film + count_fail_film)) 



count_success_music <- length(intersect(which(ste=="successful"), which(category=="Music")))

count_fail_music <- length(intersect(which(ste!="successful"), which(category=="Music")))

success_rate_music = (count_success_music / (count_success_music + count_fail_music)) 



count_success_publish <- length(intersect(which(ste=="successful"), which(category=="Publishing")))

count_fail_publish <- length(intersect(which(ste!="successful"), which(category=="Publishing")))

success_rate_publish = (count_success_publish / (count_success_publish + count_fail_publish))



count_success_game <- length(intersect(which(ste=="successful"), which(category=="Games")))

count_fail_game <- length(intersect(which(ste!="successful"), which(category=="Games")))

success_rate_game = (count_success_game / (count_success_game + count_fail_game))



count_success_tech <- length(intersect(which(ste=="successful"), which(category=="Technology")))

count_fail_tech <- length(intersect(which(ste!="successful"), which(category=="Technology")))

success_rate_tech = (count_success_tech / (count_success_tech + count_fail_tech))



count_success_design <- length(intersect(which(ste=="successful"), which(category=="Design")))

count_fail_design <- length(intersect(which(ste!="successful"), which(category=="Design")))

success_rate_design = (count_success_design / (count_success_design + count_fail_design))


combined <- c(success_rate_film, success_rate_music, success_rate_publish, success_rate_game, success_rate_tech, success_rate_design)


barplot(combined, ylim=c(0,1), main="Successful Rate in Main Catagories of Business", horiz=FALSE,
        names.arg=c("Film & Video", "Music","Publishing", "Games", "Technology", "Design"))

head(combined)


















 


