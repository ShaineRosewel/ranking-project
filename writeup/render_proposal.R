print(getwd())
rmarkdown::render(
  "/home/realiseshewon/PDev/kde-ranking/writeup/proposal.Rmd",
  output_file = paste0(
    "/home/realiseshewon/PDev/kde-ranking/writeup/Matala_proposal-manuscript_",
    format(Sys.time(), "%Y-%m-%d_%H-%M"),
    ".pdf"
  )
)