# Helpers
library(lubridate)
library(stringr)
library(tidyverse)

#' Read state FIPS codes and return a tibble.
read_fips = function() {
  read_csv("data/State_FIPS.csv",
    col_types = cols(FIPS = col_character()))
}

#' Read data about representatives.
#' Expects https://github.com/unitedstates/congress-legislators
#' to be in a peer directory
read_reps = function() {
  reps = yaml::yaml.load_file('../congress-legislators/legislators-current.yaml') %>%
    map_df(~bind_cols(.$name, tibble(bioguide=.$id$bioguide), .$bio, .$terms[[length(.$terms)]])) %>% 
    filter(type=='rep', ymd(end) > Sys.Date()) %>% # Filter only current reps
    mutate(district=str_pad(district, 2, pad='0'), # Pad district to two chars
           code=paste0(str_sub(party, 1, 1), '-',  # Code like D-MA-07
                       state, '-', district))
  reps
}

#' Read committees. Skip sub-committes for now
read_committees = function() {
  comms = yaml::yaml.load_file('../congress-legislators/committees-current.yaml') %>%     map_df(~as_tibble(.[setdiff(names(.), 'subcommittees')]))
  comms
}

#' Find all reps on a committee
#' @param comm Thomas code for the committee, e.g. 'HSAG'
by_committee = function(reps, comm) {
  all_assignments = yaml::yaml.load_file('../congress-legislators/committee-membership-current.yaml')
  this_comm = all_assignments[[comm]] %>% map_df(~as_tibble(.)) %>% 
    select(bioguide, title)
  reps %>% right_join(this_comm, by='bioguide')
}

#' Make a cross-reference of committee ID and name to House member bioguide
house_committee_xref = function() {
  comms = read_committees() %>% filter(type!='senate') %>% select(name, thomas_id)
  assignments = yaml::yaml.load_file('../congress-legislators/committee-membership-current.yaml')
  assignments = assignments[names(assignments) %in% comms$thomas_id]
  tibble(thomas_id=names(assignments), 
                        members=map(assignments, ~map_df(., as_tibble))) %>% 
    unnest %>% select(thomas_id, bioguide) %>% 
    inner_join(comms)
}

#' Get just the reps in the given districts.
#' @param districts Vector of districts in the form MA-05
by_district = function(reps, districts) {
  str_split_fixed(districts, '-', 2) %>% 
    as_tibble %>% 
    set_names(c('state', 'district')) %>% 
    left_join(reps)
}
