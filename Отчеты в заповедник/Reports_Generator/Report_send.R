library(mailR)

# kand.nauka@yandex.ru

rmarkdown::render("report_generator.Rmd")
current_report_name <- paste("Отчет В. М. Хайтова за", format(Sys.time(), "%d-%m-%Y"),".docx")
file.copy("report_generator.docx", current_report_name)

send.mail(from="polydora@rambler.ru",
          encoding = "utf-8",
          to="kand.nauka@yandex.ru",
          subject="Отчет В. М. Хайтова",
          body="letter_text.txt",
          html=T,
          smtp=list(host.name = "smtp.rambler.ru",
                    port = 465,
                    user.name = "polydora@rambler.ru",
                    passwd = "Mussel",
                    ssl = T),
          authenticate=T,
          attach.files=current_report_name)
