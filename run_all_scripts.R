fig_scripts <- list.files("scripts/", full.names = T)

purrr::walk(fig_scripts, source)