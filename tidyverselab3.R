library(tidyverse)
library(jsonlite)

#took out only json files
Essentias <- "EssentiaOutput"
names.of.files <- list.files(Essentias)
json.files <- names.of.files |>
  str_subset(".json")

#recoded the loop into a function to enable tidyverse usage of map_dfr
process.songs <- function(current.filename) {
  #isolate naming parts 
  parts.of.name <- str_split(current.filename, "-", simplify = T)
  artist1 <- parts.of.name[1]
  album1 <- parts.of.name[2]
  track1 <- sub(".json", "", parts.of.name[3])
  
  #loading in the json file and extracting each item 
  object <- fromJSON(paste0(Essentias, "/",current.filename))
  
  # create a tibble with all the necessary pieces
  tibble(
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
}

cleaned.songs <- json.files %>%
  map_dfr(process.songs)


#reading the Essentia spreadsheet and creating new average value columns
csv.Essentia <- read_csv("./EssentiaOutput/EssentiaModelOutput.csv")

cleaned.csv.Essentia <- csv.Essentia %>% 
  rename("timbreBright" = "eff_timbre_bright") %>% 
  mutate(
    valence = rowMeans(select(., deam_valence, emo_valence, muse_valence), na.rm = TRUE),
    arousal = rowMeans(select(., deam_arousal, emo_arousal, muse_arousal), na.rm = TRUE),
    aggressive = rowMeans(select(., nn_aggressive, eff_aggressive), na.rm = TRUE),
    happy = rowMeans(select(., nn_happy, eff_happy), na.rm = TRUE),
    party = rowMeans(select(., nn_party, eff_party), na.rm = TRUE),
    relax = rowMeans(select(., nn_relax, eff_relax), na.rm = TRUE),
    sad = rowMeans(select(., nn_sad, eff_sad), na.rm = TRUE),
    acoustic = rowMeans(select(., nn_acoustic, eff_acoustic), na.rm = TRUE),
    electronic = rowMeans(select(., nn_electronic, eff_electronic), na.rm = TRUE),
    instrumental = rowMeans(select(., nn_instrumental, eff_instrumental), na.rm = TRUE)
  ) %>% 
  select(artist, album, track, track, valence, timbreBright, instrumental, electronic,
         acoustic, sad, relax, party, happy, aggressive, arousal)
  # should this be dplyr::select or not?


csv.LIWC <- read.csv("LIWCOutput/LIWCOutput.csv")

#Merging all three files together and renaming function column
data.frame.list <- list(csv.LIWC,cleaned.songs,cleaned.csv.Essentia)
complied.data <- data.frame.list %>% 
  reduce(full_join)
complied.data <- complied.data %>%
  rename("func" = "function.")


#final step of Part 2, creating new csv files
training.csv <- complied.data %>% 
  filter(track != "Allentown")
testing.csv <- complied.data %>% 
  filter(track == "Allentown")
write_csv(training.csv, file = "training.csv")
write_csv(testing.csv, file = "testing.csv")
# This method works too -- future ref
# training.csv <- filter(complied.data, track != "Allentown")
# testing.csv <- filter(complied.data, track == "Allentown")


#box plots for seeing where this 
trainingdata.csv <- read_csv("training.csv")
testingdata.csv <- read_csv("testing.csv")
allentown <- filter(testingdata.csv, album == "Allentown")

# group together the data by artists
combined_data <- bind_rows(
  All.Get.Out <-
    filter(trainingdata.csv, artist == "All Get Out") %>% mutate(Artist = "AGO"),
  Manchester.Orchestra <-
    filter(trainingdata.csv,artist == "Manchester Orchestra") %>% 
    mutate(Artist = "MO"),
  Front.Bottoms <-
    filter(trainingdata.csv,artist == "The Front Bottoms") %>%
    mutate(Artist = "TFB"),
  allentown %>% mutate(Artist = "Allen")
)

# Create the ggplot boxplots
ggplot(combined_data, aes(x = Tone, y = Artist, fill = Artist)) +
  geom_boxplot() +
  geom_vline(xintercept = allentown$Tone, size =1, linetype = "dashed", color = "maroon") + 
  labs(
    title = "Figure 1: Emotional Tone sound analysis",
    x = "Tone",
    y = "Artist"
  ) +
  theme_bw() +
  theme(legend.position = "none")

ggplot(combined_data, aes(x = emotion, y = Artist, fill = Artist)) +
  geom_boxplot() +
  geom_vline(xintercept = allentown$emotion, size =1, linetype = "dashed") + 
  labs(
    title = "Figure 2: Emotion analysis",
    x = "Emotion",
    y = "Artist"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


