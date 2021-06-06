


.pkgenv <- new.env(parent=emptyenv())


.pkgenv$tags_addr <- c("ADDR",paste0("ADR",1:3),"CITY","STAE","POST",
                            "CTRY","PHON","FAX","EMAIL","WWW")
.pkgenv$tags_file_sour <- c("SOUR","VERS","NAME","CORP",.pkgenv$tags_addr, "DATA","DATE","COPR")