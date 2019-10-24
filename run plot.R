run = read.csv('C:/Users/bozaca01/Downloads/Activities (3).csv')

run$min = sapply(1:length(run$Avg.Pace), function(x) strsplit(as.character(run$Avg.Pace)[x], ":")[[1]][1])
run$sec = sapply(1:length(run$Avg.Pace), function(x) strsplit(as.character(run$Avg.Pace)[x], ":")[[1]][2])

run$min = as.numeric(run$min) * 60
run$sec = as.numeric(run$sec)

run$paceTot = run$min + run$sec

run = run[run$paceTot<1000,]

run$dateDay = sapply(1:length(run$Date), function(x) strsplit(as.character(run$Date)[x], " ")[[1]][1])

run$dateDay = as.Date(run$dateDay)

run$race = NA
run$race[run$dateDay == '2019-10-19' & run$paceTot < 400] = 'East River 5000'
run$race[run$dateDay == '2019-09-08' & run$paceTot < 400] = '5th Ave. Mile'
run$race[run$dateDay == '2019-06-08' & run$paceTot < 410] = 'Adidas 5K'
run$race[run$dateDay == '2019-04-13' & run$paceTot < 400] = 'Boston 5K'
run$race[run$dateDay == '2019-06-16' & run$paceTot < 400] = 'Harlem Mile'
run$race[run$dateDay == '2019-01-01' & run$paceTot < 450] = 'Midnight Run'
run$race[run$dateDay == '2018-11-04'] = 'NYC Marathon'
run$race[run$dateDay == '2018-10-07'] = 'Chicago Marathon'

colnames(run)[2] = 'Date2'
colnames(run)[33] = 'Date'

pal = wes_palette("Zissou1", 270, type = "continuous")

ggplot(run, aes(paceTot, Avg.HR, size = Distance, colour = as.factor(run$Date), label = race)) + geom_point() + geom_label_repel(size = 3, colour = 'gray22', segment.size=0.2, nudge_x = -15, nudge_y = 2) + theme_minimal() + labs(x = 'Average pace, seconds', y = 'Agerage heart rate, bpm') + scale_colour_manual(values = pal, name = 'Date', breaks = c('2019-10-01', '2019-09-01', '2019-08-01', '2019-07-01', '2019-06-01', '2019-05-01', '2019-04-01', '2019-03-01', '2019-02-01', '2019-01-01', '2018-12-01', '2018-11-01', '2018-10-01')) + theme(text = element_text(size=15)) + geom_smooth(colour = 'gray', alpha = 0.08, size = 0.5) 

