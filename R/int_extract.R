int_extract <- function (smapc.tp1) {


  lag = function(str) {
    substr(str, 12, 14) == "lag"
  }
  constant = function(str) {
    substr(str, 1, 9) == "const_for"
  }
  extract_from = function (str) {
    str_replace(str, "[a-z_]+([0-9]+)[a-z_]+[0-9]+", "\\1") %>% as.numeric()
  }
  extract_to = function (str){
    str_replace(str, "[a-z_]+[0-9]+[a-z_]+([0-9]+)", "\\1") %>% as.numeric()
  }


  smapc.tp1 %>%
    as_tibble()%>%
    select(-1) %>%
    mutate(time = 1:nrow( smapc.tp1))%>%
    pivot_longer(!time, names_to = "sp_pair", values_to = "strength" ) %>%
    filter(!lag(sp_pair)) %>%
    filter(!constant(sp_pair))%>%
    mutate (from = extract_from(sp_pair), to = extract_to(sp_pair)) %>%
    select(-2) %>%
    filter(!from==to) %>%
    mutate (species_to_species = str_c ( d.name[from],   d.name[to], sep = " -> "))%>%
    group_by(species_to_species)%>%
    mutate(mean_strength = mean(na.omit(strength))) %>%
    ungroup() %>%
    arrange(mean_strength)

}
