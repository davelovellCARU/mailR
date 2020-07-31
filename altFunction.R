function (from, to, subject = "", body = "", encoding = "iso-8859-1", 
          html = FALSE, inline = FALSE, smtp = list(), authenticate = FALSE, 
          send = TRUE, attach.files = NULL, debug = FALSE, ...) 
{
  if (length(from) != 1) 
    stop("Argument 'from' must be a single (valid) email address.")
  if (!length(to) > 0) 
    stop("Argument 'to' must have at least one single (valid) email address.")
  if (!all(c("host.name") %in% names(smtp))) 
    stop("Check documentation to include all mandatory parameters to establisg SMTP connection.")
  dots <- list(...)
  
  email <- .resolveEncoding(email, encoding)
  
  if (html && inline) 
    email <- .jnew("org.apache.commons.mail.ImageHtmlEmail")
  else if (html) 
    email <- .jnew("org.apache.commons.mail.HtmlEmail")
  else if (!is.null(attach.files)) 
    email <- .jnew("org.apache.commons.mail.MultiPartEmail")
  else email <- .jnew("org.apache.commons.mail.SimpleEmail")
  if (debug) 
    email$setDebug(TRUE)
  if (!is.null(attach.files)) {
    attachments <- .createEmailAttachments(attach.files, 
                                           dots)
    sapply(attachments, email$attach)
  }
  if (.jclass(email) == "org.apache.commons.mail.ImageHtmlEmail") {
    image.files.references <- str_extract_all(body, email$REGEX_IMG_SRC)
    pattern <- "\"([^\"]*)\""
    image.files.locations <- gsub("\"", "", sapply(str_extract_all(image.files.references[[1]], 
                                                                   pattern), "[[", 1))
    file.resolver <- .resolveInlineImages(image.files.locations)
    email$setDataSourceResolver(file.resolver)
  }
  email$setHostName(smtp$host.name)
  if ("port" %in% names(smtp)) 
    email$setSmtpPort(as.integer(smtp$port))
  if (authenticate == TRUE) 
    email$setAuthenticator(.authenticateSMTP(smtp))
  if ("ssl" %in% names(smtp)) 
    if (smtp$ssl) 
      email$setSSL(TRUE)
  if ("tls" %in% names(smtp)) 
    if (smtp$tls) 
      email$setTLS(TRUE)
  email$setFrom(from)
  email$setSubject(subject)
  if (file.exists(body)) 
    body <- readChar(body, file.info(body)$size)
  if (html) {
    email$setHtmlMsg(as.character(body))
    email$setTextMsg("Your email client does not support HTML messages")
  }
  else email$setMsg(as.character(body))
  if (.valid.email(to)) 
    sapply(to, email$addTo)
  if ("cc" %in% names(dots)) {
    if (.valid.email(dots$cc)) 
      sapply(dots$cc, email$addCc)
  }
  if ("bcc" %in% names(dots)) {
    if (.valid.email(dots$bcc)) 
      sapply(dots$bcc, email$addBcc)
  }
  if ("replyTo" %in% names(dots)) {
    if (.valid.email(dots$replyTo)) 
      sapply(dots$replyTo, email$addReplyTo)
  }
  if (send) 
    .jTryCatch(email$send())
  return(email)
}