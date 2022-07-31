qmd_files <- list.files(pattern = "README.qmd", recursive = T)

for (i in 1:length(qmd_files)) {
  quarto::quarto_render(qmd_files[i])
}