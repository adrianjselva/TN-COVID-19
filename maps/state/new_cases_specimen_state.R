daily_cases_specimen_state_map <- function() {
  nds <- create_state_specimen_collection(specimen_collection_dataset)
  
  return(state_map(tail(nds$NEW_CASES, n = 1),
                   "New cases:",
                   'rgb(105, 201, 245)',
                   "Daily Cases by Specimen Collection Date"))
}