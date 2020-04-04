# downloading the available non-influenza data from Texas DSHS
# https://www.dshs.state.tx.us/IDCU/disease/influenza/surveillance/2015---2016-Texas-Influenza-Surveillance-Activity-Report.xls
# following this example: https://www.brodrigues.co/blog/2018-06-10-scraping_pdfs/

#set up work environment
library(glue)
library(pdftools)
library(purrr)



#test with extracting data from original pdf
focal_pdf_url <- "https://www.dshs.state.tx.us/WorkArea/linkit.aspx?LinkIdentifier=id&ItemID=8590001598"
urls <- focal_pdf_url #
pdf_names <- "first_pdf.pdf" #glue("report_{country}.pdf")

walk2(urls, pdf_names, download.file, mode = "wb")

raw_text <- map(pdf_names, pdf_text)
