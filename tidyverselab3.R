library(tidyverse)
library(jsonlite)
library(shiny)

Essentias <- "EssentiaOutput"
names.of.files <- list.files(Essentias)

json.files <- names.of.files |>
  str_subset(".json")

cleaned.songs <- data.frame(artist = character(), album =character(), track=character(),
                            overall.loudness = numeric(), spectral.energy= numeric(), 
                            dissonance= numeric(), pitch.salience = numeric(),
                            bpm = numeric(), beats.loudness = numeric(), 
                            danceability = numeric(), tuning.frequency = numeric())

for(i in 1:length(json.files)){
  current.filename <- json.files[i]
  parts.of.name <- str_split(current.filename, "-", simplify = T)
  artist1 <- parts.of.name[1]
  album1 <- parts.of.name[2]
  track1 <- sub(".json", "", parts.of.name[3])
  
  #loading in the json file and extracting each item 
  object <- fromJSON(paste0(Essentias, "/",current.filename))
  
  new.data <- tibble(
    artist = artist1,
    album = album1,
    track = track1,
    overall.loudness = object$lowlevel$loudness_ebu128$integrated,
    spectral.energy = object$lowlevel$spectral_energy$mean,
    dissonance = object$lowlevel$dissonance$mean,
    pitch.salience = object$lowlevel$pitch_salience$mean,
    bpm = object$rhythm$bpm,
    beats.loudness = object$rhythm$beats_loudness$mean,
    danceability = object$rhythm$danceability,
    tuning.frequency = object$tonal$tuning_frequency
  )
  
  cleaned.songs <- rbind(cleaned.songs, new.data)
}


#reading the Essentia spreadsheet and creating new average value columns
csv.Essentia <- read_csv("./EssentiaOutput/EssentiaModelOutput.csv")
csv.Essentia$valence <- rowMeans(csv.Essentia[,c("deam_valence",
                                                 "emo_valence", 
                                                 "muse_valence")])

csv.Essentia$arousal <- rowMeans(csv.Essentia[,c("deam_arousal",
                                                 "emo_arousal", 
                                                 "muse_arousal")])

csv.Essentia$aggressive <- rowMeans(csv.Essentia[,c("nn_aggressive",
                                                    "eff_aggressive")])

csv.Essentia$happy <- rowMeans(csv.Essentia[,c("nn_happy",
                                               "eff_happy")])

csv.Essentia$party <- rowMeans(csv.Essentia[,c("nn_party",
                                               "eff_party")])

csv.Essentia$relax <- rowMeans(csv.Essentia[,c("nn_relax",
                                               "eff_relax")])

csv.Essentia$sad <- rowMeans(csv.Essentia[,c("nn_sad",
                                             "eff_sad")])

csv.Essentia$acoustic <- rowMeans(csv.Essentia[,c("nn_acoustic",
                                                  "eff_acoustic")])

csv.Essentia$electronic <- rowMeans(csv.Essentia[,c("nn_electronic",
                                                    "eff_electronic")])

csv.Essentia$instrumental <- rowMeans(csv.Essentia[,c("nn_instrumental",
                                                      "eff_instrumental")])

col.number <- which(colnames(csv.Essentia) == "eff_timbre_bright")
colnames(csv.Essentia)[col.number] <- "timbreBright"
csv.Essentia <- csv.Essentia[,c("artist", "album", "track","timbreBright", "instrumental", "electronic", 
                                "acoustic", "sad", "relax", "party", "happy",
                                "aggressive", "arousal", "valence")]

csv.LIWC <- read.csv("LIWCOutput/LIWCOutput.csv")

# PROF: WHY IS THIS NAMED berkshire.*? THIS DOES NOT
#       SEEM TO BE MEANINGFUL TO THE PROBLEM/SOLUTION
#Merging all three files together and renaming function column
berkshire.half <- merge(csv.LIWC, csv.Essentia, all.y = TRUE)
berkshire.hathaway <- merge(berkshire.half,cleaned.songs)
colnames(berkshire.hathaway)[colnames(berkshire.hathaway) == "function."] <- "func"


#final step of Part 2, creating new csv files
training.csv <- berkshire.hathaway[which(!berkshire.hathaway$track == "Allentown"),]
testing.csv <- berkshire.hathaway[which(berkshire.hathaway$track == "Allentown"),]
write.csv(training.csv, file = "training.csv")
write.csv(testing.csv, file = "testing.csv")

#box plots for seeing where this 
# PROF: YOU DON'T HAVE tidyverse LOADED SO THIS WON'T WORK
#       ALSO, THIS SHOULD BE training.csv (SEE LINE 94)
trainingdata.csv <- read_csv("trainingdata.csv")
# PROF: YOU DON'T HAVE tidyverse LOADED SO THIS WON'T WORK
#       ALSO, THIS SHOULD BE testing.csv (SEE LINE 95)
testingdata.csv <- read_csv("testingdata.csv")
Front.Bottoms <- trainingdata.csv[trainingdata.csv$artist == "The Front Bottoms",]
Manchester.Orchestra <- trainingdata.csv[trainingdata.csv$artist == "Manchester Orchestra",]
All.Get.Out <- trainingdata.csv[trainingdata.csv$artist == "All Get Out",]

boxplot(Front.Bottoms$Tone, Manchester.Orchestra$Tone, All.Get.Out$Tone, testingdata.csv$Tone,
        main = "Figure 1: Tone sound analysis",
        names = c("TFB", "MO", "AGO", "Allen"),
        col = c("maroon"),
        horizontal = TRUE
)

boxplot(Front.Bottoms$emotion, Manchester.Orchestra$emotion, All.Get.Out$emotion,testingdata.csv$emotion,
        main = "Figure 2: emotion analysis",
        names = c("TFB", "MO", "AGO", "Allen"),
        col = "lightblue",
        horizontal = TRUE
)

