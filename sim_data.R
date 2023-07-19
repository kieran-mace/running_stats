library(tidyverse)
simulate_week = function(
    weekNumberOfWorkouts = 0,
    weekNumberOfCardioWorkouts = 0,
    weekNumberOfRuns = 0,
    numberOfBikeRides = 0,
    weekStartMonth = 0,
    weekStartDay = 0,
    weekStartYear = 0,
    weekYearsOld = 0, #could change if user changes birthday in apple health,
    weekIntensity = 0.0, #out of a 5.0 scale, based on average heart rate,
    weekTimeInZone1 = 0.0,
    weekTimeInZone2 = 0.0,
    weekTimeInZone3 = 0.0,
    weekTimeInZone4 = 0.0,
    weekTimeInZone5 = 0.0,
    weekTotalDistance = 0.0,
    weekTrackAndFieldDistance = 0.0, #meters,
    weekDistanceCalculatedIntoRunningPace = 0.0, #meters,
    weekDistanceCalculatedIntoBikingPace = 0.0, #meters,
    weekElevationGain = 0.0, #meters,
    weekTimeExercising = 0.0, #seconds,
    weekTimeCalculatedIntoRunningPace = 0.0, #seconds,
    weekTimeCalculatedIntoBikingPace = 0.0, #seconds,
    weekTimeExercisingWithZoneData = 0.0, #seconds,
    weekTimeTrackAndField = 0.0,
    weekTimeHIIT = 0.0,
    weekTimeStrength = 0.0,
    weekTimeOther = 0.0,
    weekCalories = 0.0,
    ...){
  tibble(
    weekNumberOfWorkouts = weekNumberOfWorkouts,
    weekNumberOfCardioWorkouts = weekNumberOfCardioWorkouts,
    weekNumberOfRuns = weekNumberOfRuns,
    numberOfBikeRides = numberOfBikeRides,
    weekStartMonth = weekStartMonth,
    weekStartDay = weekStartDay,
    weekStartYear = weekStartYear,
    weekYearsOld = weekYearsOld,
    weekIntensity = weekIntensity,
    weekTimeInZone1 = weekTimeInZone1,
    weekTimeInZone2 = weekTimeInZone2,
    weekTimeInZone3 = weekTimeInZone3,
    weekTimeInZone4 = weekTimeInZone4,
    weekTimeInZone5 = weekTimeInZone5,
    weekTotalDistance = weekTotalDistance,
    weekTrackAndFieldDistance = weekTrackAndFieldDistance,
    weekDistanceCalculatedIntoRunningPace = weekDistanceCalculatedIntoRunningPace,
    weekDistanceCalculatedIntoBikingPace = weekDistanceCalculatedIntoBikingPace,
    weekElevationGain = weekElevationGain,
    weekTimeExercising = weekTimeExercising,
    weekTimeCalculatedIntoRunningPace = weekTimeCalculatedIntoRunningPace,
    weekTimeCalculatedIntoBikingPace = weekTimeCalculatedIntoBikingPace,
    weekTimeExercisingWithZoneData = weekTimeExercisingWithZoneData,
    weekTimeTrackAndField = weekTimeTrackAndField,
    weekTimeHIIT = weekTimeHIIT,
    weekTimeStrength = weekTimeStrength,
    weekTimeOther = weekTimeOther,
    weekCalories = weekCalories,
    ...)
}

base_data =  tibble(name = c('John Smith', 'James Cameron', 'Elon Musk')) |>
  mutate(best_mile_time = rnorm(3, 6, 1),
         worst_mile_time = best_mile_time + abs(rnorm(3, 3, .1)),
         best_worst_delta = worst_mile_time - best_mile_time,
         weekNumberOfWorkouts = list(rep(c(1,2,3,4,3,2,1,0,0,0,0,5,0), 4), rep(2, 52), c(rep(0, 26), rep(4, 26)))) |>
  unnest(weekNumberOfWorkouts) |>
  group_by(name) |>
  mutate(weeknum = 1:52,
         weekTimeExercising = map_dbl(weekNumberOfWorkouts, \(x) sum(runif(x, min = 0, max = 2*60*60))),
         last4weeksExercising = weekTimeExercising +
           lag(weekTimeExercising, n = 1, default = 0) +
           lag(weekTimeExercising, n = 2, default = 0) +
           lag(weekTimeExercising, n = 3, default = 0),
         mile_time = worst_mile_time - (last4weeksExercising / 80000)*best_worst_delta)


out_data = pmap_dfr(base_data, simulate_week)

# readr::write_rds(out_data, 'outdata.rds')
googlesheets4::write_sheet(out_data)
