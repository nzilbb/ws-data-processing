# Just to show you that `tidyverse` is a collection of packages, we can load
# individual tidyverse packages rather than *all* of the core tidyverse packages.
library(dplyr)
library(tidyr)
# readr provides some extra functions to reading data.
library(readr)

# This package helps to manage the paths to files.
library(here)

# Some 'base R' data processing

# Read in data
vowels <- read.csv(here('data', 'qb_vowels.csv'))

# Look at the first values
head(vowels)

# Look at some summary info
summary(vowels)

# Look at a factor variable summary
summary(factor(vowels$vowel))

# Get a particular column
word_frequencies <- vowels$word_freq

# Get the first hundred values
word_frequencies[1:100]

# More examples are provided at https://nzilbb.github.io/statistics_workshops/chapters/data_processing.html
# I won't enter them *all* for you here!

# Tidyverse approaches

# Read in data using `readr` package's `read_csv()` function
vowels <- read_csv(here('data', 'qb_vowels.csv'))
## Note the different output in the console from `read.csv()` above.

# Create a new column with midpoint and tidy some names using `rename()`
vowels <- vowels %>%
  mutate(
    word_freq_ln = log(word_freq + 1)
  ) %>%
  rename(
    F1_midpoint = F1_50,
    F2_midpoint = F2_50
  )

# Filter vowel tokens.
vowels_filtered <- vowels |> 
  filter(
    following_segment_category == "other",
    !is.na(amplitude),
    between(F1_midpoint, 300, 1000),
    vowel_duration > 0.01 | word_freq < 1000
  )

# Why does this cause an error?
vowels |> 
  mutate(
    bad_column = c(1, 2, 3, 4)
  )

# Get just the participant info
participant_metadata <- vowels_filtered |> 
  select(
    speaker,
    contains('participant_')
  ) |> 
  unique()

# Apply `dplyr` verbs to groups in the data
vowels_filtered <- vowels_filtered |> 
  # Group the data for each speaker
  group_by(speaker) |> 
  # Produce columns with the speaker mean F1 and speaker mean amplitude.
  mutate(
    mean_F1 = mean(F1_midpoint),
    mean_amplitude = mean(amplitude)
  ) |> 
  # remove the grouping structure (so that now, e.g., `mean()` won't take
  # account of the groups)
  ungroup()

# Use `group_by()` and `summarise()` to create a new data frame with a row for
# each group.
vowels_summary <- vowels_filtered |> 
  group_by(speaker) |> 
  summarise(
    n = n(),
    mean_F1 = mean(F1_midpoint),
    mean_amplitude = mean(amplitude),
    gender = first(participant_gender),
    age_category = first(participant_age_category)
  )

# Look at vowels_summary and vowels_filtered. How do they differ?

# Let's pivot.

## Split F1_midpoint and F2_midpoint into distinct rows (now a 'formant reading'
## is our observation rather than an individual vowel token.) That is, make the
## data 'longer'.
vowels_long <- vowels_filtered |> 
  pivot_longer(
    # Which columns do we want to 'lengthen'?
    cols = c(F1_midpoint, F2_midpoint),
    # What do we want to name the column which identifies the kind of formant 
    # reading?
    names_to = "formant_type",
    # What do we want to name the column containing the formant values?
    values_to = "formant_value"
  )

# What is we want a single row for each speaker, with their mean value for each
# vowel as columns.
vowels_wide <- vowels_filtered |>
  # Select only relevant data
  select(
    speaker, vowel, F1_midpoint, F2_midpoint,
    contains(match = "participant_")
  ) |> 
  pivot_wider(
    # Column names come from 'vowel' column
    names_from = vowel,
    # Values come from both midpoint columns
    values_from = c(F1_midpoint, F2_midpoint),
    # We aggregate using the `mean` function if there is more than
    # one value for each cell of the table. i.e., the `F1_midpoint_DRESS`
    # column will end up with the mean F1 value across *all* the speakers tokens
    # of DRESS.
    values_fn = mean
  )

