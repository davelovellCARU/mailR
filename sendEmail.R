library("mailR")
library("stringr")
library("utf8")
library("dplyr")
library("magrittr")
library("purrr")


sendEmail <- function(recipients = NULL,
                      subject = NULL,
                      body = NULL,
                      password = NULL,
                      html = TRUE,
                      attachments = NULL) {
  
  ## If the 'body' is an rmarkdown file, knit it ------
  if(stringr::str_detect(body, "\\.[Rr][Mm][Dd]$")){
    body <- rmarkdown::render(
      body
      )
      
  }
  
mailR::send.mail(
  from = "david.lovell@churcharmy.org",
  to = recipients,
  subject = subject,
  body = body,
  html = html,
  attach.files = attachments,
  smtp = 
    list(
      host.name = "smtp.office365.com",
      port = 587,
      user.name = "david.lovell@churcharmy.org",
      passwd = password,
      tls = TRUE
    ),
  authenticate = TRUE
)
}

recipients <- read.csv("secret/recipients.csv") 

names(recipients) <- c("fname", "sname", "email1", "email2")

recipients %<>% mutate(gottenBack = 
                         case_when(fname == "Erika" ~ TRUE,
                                   fname == "Ben" ~ TRUE,
                                   fname == "Ruth" ~ TRUE,
                                   fname == "Mark" ~ TRUE,
                                   fname == "James" ~ TRUE,
                                   fname == "Tonia" ~ TRUE,
                                   TRUE ~ FALSE),
                       context = 
                         case_when(
                           fname == "Tonia" ~ "Aylesbury Kingsbrook",
                           fname == "James" ~ "Broughton",
                           fname == "Ben" ~ "Arborfield Green",
                           fname == "Ruth" ~ "Bracknell",
                           fname == "Gareth" ~ "Newton Leys", 
                           fname == "John" ~ "Bicester Ecotown",
                           fname == "Ian & Erica" ~ "Bicester Kingsmere",
                           fname == "Mark" ~ "Great Western Park"
                         ))

recipients %<>% mutate(docname = 
                         paste0("attachments/", context, " Pioneer Interview Questions.docx"))



recipients %<>% mutate(markdown = 
                         case_when(gottenBack ~ "mailBodyGottenBack.Rmd",
                                   TRUE ~ "mailBody.Rmd"))

recipients %<>% rowwise

recipients %<>% mutate(
  emailBody = 
    {
  recipientName <- fname
  rmarkdown::render(markdown,
                    output_dir = "emails",
                    output_file = fname)
               })


recipients %<>% mutate(additionalEmails = list(c("andy.wier@churcharmy.org", "andrew.wooding@churcharmy.org")))

recipients %<>% mutate(
  allEmails = list(c(email1, email2, additionalEmails) %>%
                       {.[.!=""]})
)

recipients %<>% mutate(
  attachments = list(c(docname, "attachments/Project Information Sheet and consent form - Oxford Evaluation.docx"))
)

recipients %<>% ungroup

recipients %>% mutate(
  pmap(list(
    attachments,
    emailBody,
    allEmails),
    
       ~ {
         sendEmail(
           recipients = ..3,
           subject = "New Communities Research",
           attachments = ..1,
           body = ..2,
           password = "NO NOT ALLOWED"
         )
       }
  )
)

