library(stringr)
library(rmarkdown)
render("test.Rmd", output_format = "pdf_document")
ref_all_0 <- sapply(grep('latexPart_ref',dir('out_text',full.names = T),value = T), function(x){
  paste0(readLines(x),collapse = '\n')
})
ref_all_0 <- paste0(unlist(ref_all_0),collapse = '\n')
ref_all_0 <- gsub('\\end{thebibliography}','',ref_all_0, fixed=TRUE)
ref_all_0 <- gsub('\\begin{thebibliography}{99}','',ref_all_0, fixed=TRUE)

test.tex <- paste0(readLines('test.tex'),collapse = '\n')
ref_strings <- str_extract_all(ref_all_0, "\\{ref.*?\\}")[[1]]
for (i in 1:length(ref_strings)) {
  ref_all_0 <- gsub(ref_strings[i],paste0('{ref',i,'}'),ref_all_0, fixed=TRUE)
  test.tex <- gsub(ref_strings[i],paste0('{ref',i,'}'),test.tex, fixed=TRUE)
}


test.tex.repair <- gsub('\\end{document}',
                        paste0("\\begin{thebibliography}{99}\n",
                               ref_all_0,
                               "\\end{thebibliography}\n",
                               "\\end{document}\n"),
                        test.tex, fixed=TRUE)

writeLines(test.tex.repair,'paper_promt.tex')
