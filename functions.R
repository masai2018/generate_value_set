################################################################################
### a collection of functions
### version controlled by git
################################################################################


## Get Medical Claim
##' Please notice, read all column for both 2016 and 2017 year will EXCEED MEMORY!!!!
##' Please notice, read all column for both 2016 and 2017 year will EXCEED MEMORY!!!!
##' Please notice, read all column for both 2016 and 2017 year will EXCEED MEMORY!!!!
GetMC <- function(year = 2017,
                  select = NULL,
                  class = 'measure',
                  dir.raw = 'E:/CT APCD/Hongfei/output/APCD by month/'){
  ## get MC for measurement/calendar year
  ## e.g. measurement year: 2017 refers to Oct.2016-Sep.2017
  ## e.g. calendar year: 2017 refers to Jan.2017-Dec.2017
  if (class == 'measure'){
    pathchr <- paste0(dir.raw, 'MEDICAL_',
                      c(paste0(year-1, "_", sprintf("%02d", 10:12)), paste0(year, "_", sprintf("%02d", 1:9))),
                      ".csv")
  } else if (class == 'calendar'){
    pathchr <- paste0(dir.raw, 'MEDICAL_',
                      c(paste0(year, "_", sprintf("%02d", 1:12))),
                      ".csv")
  } else if (!class %in% 'calendar'){
    stop ('"class" can only be "measure" or "calendar"')
  }
  data <- data.table::data.table()
  cat ('start reading data: total', num.chunk <- length(pathchr), 'chunks. \n', as.character(Sys.time()), '\n')
  for (i in 1:num.chunk) {
    data <- rbind(data, datachunk <- fread(pathchr[i],
                                           header = TRUE,
                                           select = select,
                                           colClasses = 'character',
                                           encoding = 'UTF-8'))
    cat(i, '/', num.chunk, 'finished,' , as.character(Sys.time()), '\n')
  }
  return( data )
}

### Example ###
# col.keep <- c('MEDICAL_CLAIM_SERVICE_LINE_ID',
#               'INTERNAL_MEMBER_ID',
#               'PROCEDURE_CODE',
#               'MEDICAL_CLAIM_HEADER_ID',
#               'ORIG_NPI')
# MC_2016 <- GetMC(year = 2016, select = col.keep) #### Please notice, read all column for both 2016 and 2017 year will EXCEED MEMORY!!!!
# MC_2017 <- GetMC(year = 2017, select = col.keep) #### Please notice, read all column for both 2016 and 2017 year will EXCEED MEMORY!!!!
### End of Example ###


## aggregate MC and PC
GetClaim <- function(year = 2017,
                     type = NULL,
                     select = NULL,
                     class = 'measure',
                     dir.raw = 'E:/CT APCD/Hongfei/output/APCD by month'){
  ## get Claim data for measurement/calendar year
  ## e.g. measurement year: 2017 refers to Oct.2016-Sep.2017
  ## e.g. calendar year: 2017 refers to Jan.2017-Dec.2017
  ##
  if (! type %in% c('medical', 'pharmacy')) stop('"type" argument can only be "medical" or "pharmacy"')
  type <- toupper(type)
  if (class == 'measure'){
    pathchr <- paste0(dir.raw, '/', type, '_',
                      c(paste0(year-1, "_", sprintf("%02d", 10:12)), paste0(year, "_", sprintf("%02d", 1:9))),
                      ".csv")
  } else if (class == 'calendar'){
    pathchr <- paste0(dir.raw, '/', type, '_',
                      c(paste0(year, "_", sprintf("%02d", 1:12))),
                      ".csv")
  } else if (!class %in% 'calendar'){
    stop ('"class" can only be "measure" or "calendar"')
  }
  data <- data.table::data.table()
  cat ('start reading data: total', num.chunk <- length(pathchr), 'chunks. \n', as.character(Sys.time()), '\n')
  for (i in 1:num.chunk) {
    data <- rbind(data, datachunk <- fread(pathchr[i],
                                           header = TRUE,
                                           select = select,
                                           colClasses = 'character',
                                           encoding = 'UTF-8'))
    cat(i, '/', num.chunk, 'finished,' , as.character(Sys.time()), '\n')
  }
  return(data)
}


## Get Value Set
GetMC_VS <- function(vsname,
                     year = 2017,
                     select = NULL,
                     class = 'measure',
                     dir.raw = 'E:/CT APCD/Hongfei/output/value_set/',
                     takeUnique = TRUE){
  ## get MC for specific Value Set for measurement/calendar year
  ## e.g. measurement year: 2017 refers to Oct.2016-Sep.2017
  ## e.g. calendar year: 2017 refers to Jan.2017-Dec.2017
  dir.vs <- paste0(dir.raw, '#', vsname, '/')
  if (!dir.exists(dir.vs)){
    stop('"vsname" does not exists, please check it!! ')
  }
  dir.vs.procedure <- paste0(dir.vs, '#PROCEDURE_CODE/')
  dir.vs.revenue <- paste0(dir.vs, '#REVENUE_CODE/')
  pathchr <- character()
  if (class == 'measure'){
    if (dir.exists(dir.vs.procedure)){
      pathchr <- c(pathchr, paste0(dir.vs.procedure, 'MEDICAL_',
                                   c(paste0(year-1, "_", sprintf("%02d", 10:12)), paste0(year, "_", sprintf("%02d", 1:9))),
                                   ".csv"))
    }
    if (dir.exists(dir.vs.revenue)){
      pathchr <- c(pathchr, paste0(dir.vs.revenue, 'MEDICAL_',
                                   c(paste0(year-1, "_", sprintf("%02d", 10:12)), paste0(year, "_", sprintf("%02d", 1:9))),
                                   ".csv"))
    }
    if (length(pathchr) == 0){
      stop('The "pathchr" is empty, please examine')
    }
  } else if (class == 'calendar'){
    if (dir.exists(dir.vs.procedure)){
      pathchr <- c(pathchr, paste0(dir.vs.procedure, 'MEDICAL_',
                                   c(paste0(year, "_", sprintf("%02d", 1:12))),
                                   ".csv"))
    }
    if (dir.exists(dir.vs.revenue)){
      pathchr <- c(pathchr, paste0(dir.vs.revenue, 'MEDICAL_',
                                   c(paste0(year, "_", sprintf("%02d", 1:12))),
                                   ".csv"))
    }
  }
  if (length(pathchr) == 0){
    stop('The "pathchr" is empty, please examine')
  } else if (!class %in% c('measure', 'calendar')){
    stop ('"class" can only be "measure" or "calendar"')
  }
  data <- data.table::data.table()
  cat ('start reading data: total', num.chunk <- length(pathchr), 'chunks. \n', as.character(Sys.time()), '\n')
  if (takeUnique == TRUE){
    for (i in 1:num.chunk) {
      data <- unique(rbind(data, datachunk <- fread(pathchr[i],
                                                    header = TRUE,
                                                    select = select,
                                                    colClasses = 'character',
                                                    encoding = 'UTF-8')))
      cat(i, '/', num.chunk, 'finished,' , as.character(Sys.time()), '\n')
    }
  } else if (takeUnique == FALSE){
    for (i in 1:num.chunk) {
      data <- rbind(data, datachunk <- fread(pathchr[i],
                                             header = TRUE,
                                             select = select,
                                             colClasses = 'character',
                                             encoding = 'UTF-8'))
      cat(i, '/', num.chunk, 'finished,' , as.character(Sys.time()), '\n')
    }
  } else if (!takeUnique %in% c(TRUE, FALSE)){
    stop('takeUnique option can only be TRUE or FALSE!! Please check!! ')
  }
  return(data)
}


##' cat to console as well as to file
mycat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = TRUE) {
  cat(..., file = '', sep = sep, fill = fill, labels = labels, append = append)
  cat(..., file = file, sep = sep, fill = fill, labels = labels, append = append)
}


## extract value set
match_regexp <- function(vs=vs.dx.icd.regexp, fclaim=finfo$fname[3], col.match='DIAGNOSIS_CODE', chunksize=1e6,
                         col.out=c('MEDICAL_CLAIM_SERVICE_LINE_ID', 'INTERNAL_MEMBER_ID', 'first_service_dt'),
                         test=T, verbose=T, towrite=!test, header=T, sep='|',
                         dir_raw = NULL,
                         dir_out = NULL, long.vs.nchunk = 1000){
  if(is.null(dir_out)) dir_out <- paste0(getwd(), '/output/', gsub('.txt', '_by_vs/', fclaim, fixed = T))
  if(!dir.exists(dir_out)) dir.create(dir_out)
  flog <- paste0(dir_out, 'log.txt')
  fout.vs.long <- paste0(dir_out, 'long_vs.rda')
  fclaim0 <- fclaim
  fclaim <- paste0(dir_raw, fclaim)
  ## my cat function: cat to console while also to file
  mycat <- function(..., file = flog, sep = " ", fill = FALSE, labels = NULL, append = TRUE) {
    cat(..., file = '', sep = sep, fill = fill, labels = labels, append = append)
    cat(..., file = file, sep = sep, fill = fill, labels = labels, append = append)
  }
  if( !'Value Set Name' %in% colnames(vs) ) stop('Need "Value Set Name" column in vs!')
  if( (!'code_regexp' %in% colnames(vs)) & (!'Code' %in% colnames(vs)) ) stop('Need "code_regexp" or "Code" column in vs!')
  if('code_regexp' %in% colnames(vs) & 'nchunk' %in% colnames(vs)){
    nvs.chunk <- vs$nchunk
    vs.long <- vs[nvs.chunk >= long.vs.nchunk, ]
    if(dim(vs.long)[1] > 0) mycat('vs.long: too long value sets not matched: \n', vs.long$`Value Set Name`, '\n' )
    if(towrite) {
      cat('vs.long saved in ', fout.vs.long, '\n \n')
      save(vs.long, file=fout.vs.long)
    }
    vs <- vs[nvs.chunk < long.vs.nchunk, ] %>% mutate(`Value Set Name` = gsub('/', '', `Value Set Name`, fixed=T))

  }
  vs.name <- unique(vs$`Value Set Name` )
  nvs <- length(vs.name)
  con <- file(fclaim, open="r")
  ic <- 1
  nr <- chunksize
  fh <- NULL
  if(header) fh <- scan(con, nlines=1, what = character(), sep=sep)
  if(is.null(col.out))  col.out <- fh
  if(!col.match %in% col.out) {
    mycat('matched col added to output col... \n')
    col.out <- c(col.out, col.match)
  }

  if(verbose) mycat('BEGIN match value sets in ', fclaim0,
                    ', \n nvs = ', nvs,  ', chunksize = ', chunksize, ', col matched = ', col.match,
                    '\n col output = ', col.out, '... \n')
  ## begin reading data in chunks
  while(nr==chunksize){
    if(verbose) mycat( as.character(Sys.time()), '\n read in chunk #', ic, '...\n')

    if(chunksize == 0){  ## read whole file in one, fread faster
      ff <- fread(fclaim, colClasses='character', select=col.out)
    } else {
      ff <- data.table(read.table(con, header=F, sep=sep, colClasses='character', col.names=fh,
                                  nrows=chunksize, blank.lines.skip = TRUE, fill = TRUE))
      ff <- ff[, match(col.out, colnames(ff)), with=F]
    }

    colnames(ff)[match(col.match, colnames(ff))] <- 'MYCODE'

    ff[, MYCODE:=gsub(' ', '', MYCODE, fixed = T)]
    for(ivs in 1:nvs){
      out.file.name <- gsub("ct_|split_", "", fclaim0)
      fout <- paste0(dir_out, gsub('.csv', '#', out.file.name, fixed = T), col.match, '#', vs.name[ivs], '.csv')

      if(ic == 1 & file.exists(fout)) return('file to write already exist, please check!')

      if('code_regexp' %in% colnames(vs)){
        cat(vs$code_regexp[ivs], '\n')
        ftmp <- ff[grepl(vs$code_regexp[ivs], MYCODE),]
      } else if('Code' %in% colnames(vs)){
        ftmp <- ff[MYCODE %in% vs$Code[vs$`Value Set Name`==vs.name[ivs]], ]
      }
      colnames(ftmp)[match('MYCODE', colnames(ftmp))] <- col.match
      if(towrite) write.table(ftmp, file=fout, sep=',',  append=T, row.names = F, col.names = ifelse(ic==1, T, F))
      if(verbose) mycat('--value set #', ivs, vs.name[ivs], ' matched: ', dim(ftmp)[1], '\n' )
      if(test & ic == 1 & ivs == 1) return( 'END test run: first chunk, first vs!')
    }

    if(verbose) mycat(as.character(Sys.time()), '\n  chunk #', ic, 'done! \n')
    nr <- nrow(ff)
    ic <- ic + 1
  }
  close(con)  ## close file connection
  if(verbose) mycat('END match value sets in ', fclaim,
                    ', \n nvs = ', nvs, ', nrow = ', (ic-1)*chunksize + nr,
                    ', nchunk = ', ic, '! \n' )
}

## extract value set Hongfei's version
match_regexp_hf <- function(vs=vs.dx.icd.regexp, fclaim=finfo$fname[3], col.match='DIAGNOSIS_CODE', chunksize=1e6,
                         col.out=c('MEDICAL_CLAIM_SERVICE_LINE_ID', 'INTERNAL_MEMBER_ID', 'first_service_dt'),
                         test=T, verbose=T, towrite=!test, header=T, sep='|',
                         dir_raw = 'E:/CT_APCD/Commercial_and_Medicare_data_5-24-18',
                         dir_out = NULL, long.vs.nchunk = 1000){
  if(is.null(dir_out)) dir_out <- paste0(getwd(), '/output/', gsub('.csv', '_by_vs/', fclaim, fixed = T))
  if(!dir.exists(dir_out)) dir.create(dir_out)
  flog <- paste0(dir_out, 'log.txt')
  fout.vs.long <- paste0(dir_out, 'long_vs.rda')
  fclaim0 <- fclaim
  fclaim <- paste0(dir_raw, fclaim)
  mycat <- function(..., file = flog, sep = " ", fill = FALSE, labels = NULL, append = TRUE) {
    cat(..., file = '', sep = sep, fill = fill, labels = labels, append = append)
    cat(..., file = file, sep = sep, fill = fill, labels = labels, append = append)
  }
  if( !'Value Set Name' %in% colnames(vs) ) stop('Need "Value Set Name" column in vs!')
  if( (!'code_regexp' %in% colnames(vs)) & (!'Code' %in% colnames(vs)) ) stop('Need "code_regexp" or "Code" column in vs!')
  if('code_regexp' %in% colnames(vs) & 'nchunk' %in% colnames(vs)){
    nvs.chunk <- vs$nchunk
    vs.long <- vs[nvs.chunk >= long.vs.nchunk, ]
    if(dim(vs.long)[1] > 0) mycat('vs.long: too long value sets not matched: \n', vs.long$`Value Set Name`, '\n' )
    if(towrite) {
      cat('vs.long saved in ', fout.vs.long, '\n \n')
      save(vs.long, file=fout.vs.long)
    }
    vs <- vs[nvs.chunk < long.vs.nchunk, ] %>% mutate(`Value Set Name` = gsub('/', '', `Value Set Name`, fixed=T))

  }
  vs.name <- unique(vs$`Value Set Name` )
  nvs <- length(vs.name)
  ic <- 1
  nr <- chunksize
  fh <- NULL
  if(is.null(col.out))  col.out <- fh
  if(!col.match %in% col.out) {
    mycat('matched col added to output col... \n')
    col.out <- c(col.out, col.match)
  }

  if(verbose) mycat('BEGIN match value sets in ', fclaim0,
                    ', \n nvs = ', nvs,  ', chunksize = ', chunksize, ', col matched = ', col.match,
                    '\n col output = ', col.out, '... \n')
  while(nr==chunksize){
    if(verbose) mycat( as.character(Sys.time()), '\n read in chunk #', ic, '...\n')

    if(chunksize == 0){  ## read whole file in one, fread faster
      ff <- fread(fclaim, colClasses='character', select=col.out, encoding = 'UTF-8')
    } else {
      ff <- data.table(read.table(con, header=F, sep=sep, colClasses='character', col.names=fh,
                                  nrows=chunksize, blank.lines.skip = TRUE, fill = TRUE))
      ff <- ff[, match(col.out, colnames(ff)), with=F]
    }

    colnames(ff)[match(col.match, colnames(ff))] <- 'MYCODE'

    ff[, MYCODE:=gsub(' ', '', MYCODE, fixed = T)]
    for(ivs in 1:nvs){
      dir_out_vs <- paste0(dir_out, '#', vs.name[ivs])
      if(!dir.exists(dir_out_vs)) dir.create(dir_out_vs)
      dir_out_vs_code <- paste0(dir_out_vs,'/#', col.match)
      if(!dir.exists(dir_out_vs_code)) dir.create(dir_out_vs_code)
      fout <- paste0(dir_out_vs_code, '/', fclaim0)
      if(ic == 1 & file.exists(fout)) return('file to write already exist, please check!')
      if('code_regexp' %in% colnames(vs)){
        cat(vs$code_regexp[ivs], '\n')
        ftmp <- ff[grepl(vs$code_regexp[ivs], MYCODE),]
      } else if('Code' %in% colnames(vs)){
        ftmp <- ff[MYCODE %in% vs$Code[vs$`Value Set Name`==vs.name[ivs]], ]
      }
      colnames(ftmp)[match('MYCODE', colnames(ftmp))] <- col.match
      if(towrite) write.table(ftmp, file=fout, sep=',',  append=T, row.names = F, col.names = ifelse(ic==1, T, F))
      if(verbose) mycat('--value set #', ivs, vs.name[ivs], ' matched: ', dim(ftmp)[1], '\n' )
      if(test & ic == 1 & ivs == 1) return( 'END test run: first chunk, first vs!')
    }

    if(verbose) mycat(as.character(Sys.time()), '\n  chunk #', ic, 'done! \n')
    nr <- nrow(ff)
    ic <- ic + 1
  }
  if(verbose) mycat('END match value sets in ', fclaim,
                    ', \n nvs = ', nvs, ', nrow = ', (ic-1)*chunksize + nr,
                    ', nchunk = ', ic, '! \n' )
}

## extract value set (RData version)
match_regexp_rdata <- function(vs=vs.dx.icd.regexp, fclaim = NULL, col.match='DIAGNOSIS_CODE', chunksize=1e6,
                         col.out=c('MEDICAL_CLAIM_SERVICE_LINE_ID', 'INTERNAL_MEMBER_ID', 'first_service_dt'),
                         test=T, verbose=T, towrite=!test, header=T, sep='|',
                         dir_raw = NULL,
                         dir_out = NULL, long.vs.nchunk = 1000){
  if(is.null(dir_out)) dir_out <- paste0(getwd(), '/output/', gsub('.txt', '_by_vs/', fclaim, fixed = T))
  if(!dir.exists(dir_out)) dir.create(dir_out)
  flog <- paste0(dir_out, 'log.txt')
  fout.vs.long <- paste0(dir_out, 'long_vs.rda')
  fclaim0 <- fclaim
  fclaim <- paste0(dir_raw, fclaim)
  ## my cat function: cat to console while also to file
  mycat <- function(..., file = flog, sep = " ", fill = FALSE, labels = NULL, append = TRUE) {
    cat(..., file = '', sep = sep, fill = fill, labels = labels, append = append)
    cat(..., file = file, sep = sep, fill = fill, labels = labels, append = append)
  }
  if( !'Value Set Name' %in% colnames(vs) ) stop('Need "Value Set Name" column in vs!')
  if( (!'code_regexp' %in% colnames(vs)) & (!'Code' %in% colnames(vs)) ) stop('Need "code_regexp" or "Code" column in vs!')
  if('code_regexp' %in% colnames(vs) & 'nchunk' %in% colnames(vs)){
    nvs.chunk <- vs$nchunk
    vs.long <- vs[nvs.chunk >= long.vs.nchunk, ]
    if(dim(vs.long)[1] > 0) mycat('vs.long: too long value sets not matched: \n', vs.long$`Value Set Name`, '\n' )
    if(towrite) {
      cat('vs.long saved in ', fout.vs.long, '\n \n')
      save(vs.long, file=fout.vs.long)
    }
    vs <- vs[nvs.chunk < long.vs.nchunk, ] %>% mutate(`Value Set Name` = gsub('/', '', `Value Set Name`, fixed=T))

  }
  vs.name <- unique(vs$`Value Set Name` )
  nvs <- length(vs.name)
  con <- file(fclaim, open="r")
  ic <- 1
  nr <- chunksize
  fh <- NULL
  if(header) fh <- scan(con, nlines=1, what = character(), sep=sep)
  if(is.null(col.out))  col.out <- fh
  if(!col.match %in% col.out) {
    mycat('matched col added to output col... \n')
    col.out <- c(col.out, col.match)
  }

  if(verbose) mycat('BEGIN match value sets in ', fclaim0,
                    ', \n nvs = ', nvs,  ', chunksize = ', chunksize, ', col matched = ', col.match,
                    '\n col output = ', col.out, '... \n')
  ## begin reading data in chunks
  while(nr==chunksize){
    if(verbose) mycat( as.character(Sys.time()), '\n read in chunk #', ic, '...\n')

    if(chunksize == 0){  ## read whole file in one, fread faster
        ff <- load(fclaim)
        ff <- ff[, names(ff) %in% col.out]
    } else {
      ff <- data.table(read.table(con, header=F, sep=sep, colClasses='character', col.names=fh,
                                  nrows=chunksize, blank.lines.skip = TRUE, fill = TRUE))
      ff <- ff[, match(col.out, colnames(ff)), with=F]
    }

    colnames(ff)[match(col.match, colnames(ff))] <- 'MYCODE'

    ff[, MYCODE:=gsub(' ', '', MYCODE, fixed = T)]
    for(ivs in 1:nvs){
      fout <- paste0(dir_out, gsub('.txt', '#', fclaim0, fixed = T), col.match, '#', vs.name[ivs], '.csv')

      if(ic == 1 & file.exists(fout)) return('file to write already exist, please check!')

      if('code_regexp' %in% colnames(vs)){
        cat(vs$code_regexp[ivs], '\n')
        ftmp <- ff[grepl(vs$code_regexp[ivs], MYCODE),]
      } else if('Code' %in% colnames(vs)){
        ftmp <- ff[MYCODE %in% vs$Code[vs$`Value Set Name`==vs.name[ivs]], ]
      }
      colnames(ftmp)[match('MYCODE', colnames(ftmp))] <- col.match
      if(towrite) write.table(ftmp, file=fout, sep=',',  append=T, row.names = F, col.names = ifelse(ic==1, T, F))
      if(verbose) mycat('--value set #', ivs, vs.name[ivs], ' matched: ', dim(ftmp)[1], '\n' )
      if(test & ic == 1 & ivs == 1) return( 'END test run: first chunk, first vs!')
    }

    if(verbose) mycat(as.character(Sys.time()), '\n  chunk #', ic, 'done! \n')
    nr <- nrow(ff)
    ic <- ic + 1
  }
  close(con)  ## close file connection
  if(verbose) mycat('END match value sets in ', fclaim,
                    ', \n nvs = ', nvs, ', nrow = ', (ic-1)*chunksize + nr,
                    ', nchunk = ', ic, '! \n' )
}

## get regular expression (regexp)
get_regexp <- function(vs){
  vs.regexp <- vs %>% mutate(Code=gsub('.', '', Code, fixed=T),  # remove dot '.'
                             Code=gsub(' ', '', Code, fixed=T),  # remove blanks ' '
                             nc=nchar(Code),                     # count number of characters in code
                             code_1=substr(Code, nc,   nc),      # the last digit
                             code_2=substr(Code, nc-1, nc-1),    # the last but 2 digit
                             code_ =substr(Code, 1,    nc-2)) %>%   # remaining code
    group_by(`Value Set Name`, code_, code_2) %>%                # aggregate the last digit to regexp
    summarise(code_1_regexp=paste0('[', paste(code_1, collapse=''), ']')) %>%
    group_by(`Value Set Name`, code_, code_1_regexp) %>%         # aggregate the last but 2 digit to regexp
    summarise(code_2_regexp=paste0('[', paste(code_2, collapse=''), ']')) %>%
    group_by(`Value Set Name`) %>%     # get regexp for each VS, '^': matching from 1st digit; '|': either
    summarise(nchunk=n(),            # number of regexp chunks
              code_regexp = paste0(paste0('^', code_), code_2_regexp, code_1_regexp, collapse='|'))
  return(vs.regexp)
}




get_medical_vs <- function(vs.list,
                           year.list = 2015:2017,
                           code.sys = c("revenue_code", "procedure_code"),
                           select = NULL,
                           dir.raw = "E:/CT_APCD/shared/intermediate_data/APCD_modified/value_set/medical_claim/",
                           takeUnique = TRUE,
                           fill = TRUE){
    ## check basic argument.
    if (length(vs.list) == 0){
        stop("`vs.list` argument cannot be empty!")
    }
    require(data.table)

    file_name <- paste0(
        "medical_",
        outer(outer(
            year.list, code.sys, paste, sep = "#"),
            vs.list, paste, sep = "#"))
    dat <- data.table()
    cat("read following intermediate files: \n")

    for (file_name_i in file_name){
        pathchr_i <- paste0(dir.raw, file_name_i, ".csv")
        if (file.exists(pathchr_i)){
            if (!is.null(select)){
                cname <- names(read.csv(
                    file = pathchr_i,
                    nrow = 1
                ))
                select0 <- cname[tolower(cname) %in% select]
            } else{
              select0 = NULL
            }
            dat_i <- fread(pathchr_i, header = TRUE, colClasses = 'character', encoding = "UTF-8", select = select0)
            colnames(dat_i) <- tolower(colnames(dat_i))
            dat <- rbind(dat,
                         dat_i)
            cat(file_name_i, '\n')
        }
    }
    if (takeUnique){
        dat <- unique(dat)
    }
    cat(" ^_^ intermediate file has already been read!")
    return(dat)
}



match_regexp_crt <- function(vs,
                             ff,
                             col.match,
                             dir_out,
                             fname_out,
                             long.vs.nchunk = 1000){
    # if(!vs$code_regexp == "" && vs$nchunk <= long.vs.nchunk){
  if(dim(vs)[1] > 0 && vs$nchunk <= long.vs.nchunk){
        colnames(ff)[match(col.match, colnames(ff))] <- 'MYCODE'
        ff[, MYCODE:=gsub(' ', '', ff$MYCODE, fixed = T)]
        ftmp <- ff[grepl(vs$code_regexp, MYCODE), ]
        colnames(ftmp)[match('MYCODE', colnames(ftmp))] <- col.match
        fwrite(ftmp, file.path(dir_out, fname_out))
    }
}

medical_crt_icd <- function(vs.name,
                            year,
                            dir.raw = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim/",
                            dir.crt9 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_crt9/",
                            dir.crt10 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_crt10/",
                            dir.hedis = "E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/"){
    # load(file.path(dir.hedis, "2018_v2_vs.regexp.rda"))
    ##
    fname <- paste0("medical_", year, "#ICD_PROCEDURE_CODE#", vs.name, ".csv")
    pathchr_i <- file.path(dir.raw, fname)
    dat_i <- fread(pathchr_i, header = TRUE, colClasses = 'character', encoding = "UTF-8")
    dat_i9 <- dat_i[ICD_VERSION_IND == '9']
    dat_i10 <- dat_i[ICD_VERSION_IND == '0']

    match_regexp_crt(
        vs = as.data.table(vs.proc.icd9.regexp)[`Value Set Name` == vs.name],
        ff = dat_i9,
        col.match <- 'ICD_PROCEDURE_CODE',
        dir_out = dir.crt9,
        fname_out = fname
    )
    match_regexp_crt(
        vs = as.data.table(vs.proc.icd10.regexp)[`Value Set Name` == vs.name],
        ff = dat_i10,
        col.match <- 'ICD_PROCEDURE_CODE',
        dir_out = dir.crt10,
        fname_out = fname
    )
}

check_CRT <- function(vs.list,
                      year.list,
                      code.sys = 'ICD_PROCEDURE_CODE',
                      dir.record = "E:/CT_APCD/shared/intermediate_data/APCD_modified/value_set/record/",
                      ...
                      ){
    ## read in record file
    file_crt_rec <- fread(file.path(dir.record, "file_CRT_rec.csv"))

    ## correction
    file_name <- NULL
    for (vs.name in vs.list){
        year.uncrt <- colnames(file_crt_rec)[which(file_crt_rec[`Value Set Name` == vs.name] == 0)]
        file_name <- paste0("medical_",
                            outer(year.uncrt, code.sys, paste, sep = '#'),
                            "#",
                            vs.name)
        cat(as.character(Sys.time()), "\n", "start correcting for ", vs.name, "intermediate files for year: ", year.uncrt, "\n")
        medical_crt_icd(
            vs.name = vs.name,
            year = year.uncrt
        )
        cat(as.character(Sys.time()), "\n correction finished ! \n")
    }
    file_crt_rec[`Value Set Name` == vs.name] <- c(vs.name,
                                                   mapply(max,
                                                          c(as.numeric('2012' %in% year.list),
                                                            as.numeric('2013' %in% year.list),
                                                            as.numeric('2014' %in% year.list),
                                                            as.numeric('2015' %in% year.list),
                                                            as.numeric('2016' %in% year.list),
                                                            as.numeric('2017' %in% year.list)),
                                                          file_crt_rec[`Value Set Name` == vs.name, 2:7]))
    fwrite(file_crt_rec, file.path(dir.record, "file_CRT_rec.csv"))
}

get_medical_vs_CRT <- function(vs.list,
                               year.list,
                               code.sys = c("procedure_code", "revenue_code"),   # icd_procedure_code
                               select = NULL,
                               ...
                               ){
    ## check basic argument.
    if (length(vs.list) == 0){
        stop("`vs.list` argument cannot be empty!")
    }

    require(data.table)
    ### reading in data
    dat <- data.table()
    cat("read following intermediate files: \n")
    if ("icd_procedure_code" %in% code.sys){
        ## check if corrected, if not, correct the files before read in.
        check_CRT(
            vs.list = vs.list,
            year.list = year.list,
            ...
        )
        dat9 <- get_medical_vs(
            vs.list = vs.list,
            year.list = year.list,
            code.sys = "icd_procedure_code",
            dir.raw = "E:/CT_APCD/shared/intermediate_data/APCD_modified/value_set/medical_claim_crt9/",
            takeUnique = takeUnique,
            fill = fill
        )
        dat10 <- get_medical_vs(
            vs.list = vs.list,
            year.list = year.list,
            code.sys = "icd_procedure_code",
            dir.raw = "E:/CT_APCD/shared/intermediate_data/APCD_modified/value_set/medical_claim_crt10/",
            takeUnique = takeUnique,
            fill = fill
        )
        dat <- rbind(dat, dat9, dat10)
    }
    if ("procedure_code" %in% code.sys){
        dat <- rbind(dat, get_medical_vs(
            vs.list = vs.list,
            year.list = year.list,
            select = select,
            code.sys = "procedure_code",
            dir.raw = "E:/CT_APCD/shared/intermediate_data/APCD_modified/value_set/medical_claim/",
            # takeUnique = takeUnique,
            fill = fill
        ))
    }
    if ("revenue_code" %in% code.sys){
      dat <- rbind(dat, get_medical_vs(
            vs.list = vs.list,
            year.list = year.list,
            select = select,
            code.sys = "revenue_code",
            dir.raw = "E:/CT_APCD/shared/intermediate_data/APCD_modified/value_set/medical_claim/",
            # takeUnique = takeUnique,
            fill = fill
        ))
    }
    if ("type_of_bill_code" %in% code.sys){
      dat <- rbind(dat, get_medical_vs(
        vs.list = vs.list,
        year.list = year.list,
        code.sys = "type_of_bill_code",
        dir.raw = "E:/CT_APCD/shared/intermediate_data/APCD_modified/value_set/medical_claim/",
        # takeUnique = takeUnique,
        fill = fill
      ))
    }
}


## added by Sai
medical_crt_dx_icd <- function(vs.name,
                            dir.raw = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/diagnosis/",
                            dir.crt9 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/diagnosis_crt9",
                            dir.crt10 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/diagnosis_crt10",
                            dir.hedis = "E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/"){
  # load(file.path(dir.hedis, "2018_v2_vs.regexp.rda"))
  ##
  fname <- paste0("#", vs.name, ".csv")
  pathchr_i <- file.path(dir.raw, fname)
  dat_i <- fread(pathchr_i, header = TRUE, colClasses = 'character', encoding = "UTF-8")
  dat_i9 <- dat_i[ICD_VERSION_IND == '9']
  dat_i10 <- dat_i[ICD_VERSION_IND == '0']
  
  match_regexp_crt(
    vs = as.data.table(vs.dx.icd9.regexp)[`Value Set Name` == vs.name],
    ff = dat_i9,
    col.match <- 'DIAGNOSIS_CODE',
    dir_out = dir.crt9,
    fname_out = fname
  )
  match_regexp_crt(
    vs = as.data.table(vs.dx.icd10.regexp)[`Value Set Name` == vs.name],
    ff = dat_i10,
    col.match <- 'DIAGNOSIS_CODE',
    dir_out = dir.crt10,
    fname_out = fname
  )
}

medical_crt_dx_icd2 <- function(vs.name,
                            year,
                            dir.raw = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx/",
                            dir.crt9 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx_crt9/",
                            dir.crt10 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx_crt10/",
                            dir.hedis = "E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/"){
  # load(file.path(dir.hedis, "2018_v2_vs.regexp.rda"))
  ##
  fname <- paste0("medical_", year, "#DIAGNOSIS_CODE#", vs.name, ".csv")
  pathchr_i <- file.path(dir.raw, fname)
  dat_i <- fread(pathchr_i, header = TRUE, colClasses = 'character', encoding = "UTF-8")
  dat_i9 <- dat_i[ICD_VERSION_IND == '9']
  dat_i10 <- dat_i[ICD_VERSION_IND == '0']
  
  match_regexp_crt(
    vs = as.data.table(vs.dx.icd9.regexp)[`Value Set Name` == vs.name],
    ff = dat_i9,
    col.match <- 'DIAGNOSIS_CODE',
    dir_out = dir.crt9,
    fname_out = fname
  )
  match_regexp_crt(
    vs = as.data.table(vs.dx.icd10.regexp)[`Value Set Name` == vs.name],
    ff = dat_i10,
    col.match <- 'DIAGNOSIS_CODE',
    dir_out = dir.crt10,
    fname_out = fname
  )
}

medical_crt_icd_proc <- function(vs.name,
                               dir.raw = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/icd_procedure/",
                               dir.crt9 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/icd_procedure_crt9",
                               dir.crt10 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/icd_procedure_crt10",
                               dir.hedis = "E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/"){
  load(file.path(dir.hedis, "2018_v2_vs.regexp.rda"))
  ##
  fname <- paste0("MEDICAL_CLAIM_ICD_PROCEDURE#ICD_PROCEDURE_CODE#", vs.name, ".csv")
  pathchr_i <- file.path(dir.raw, fname)
  dat_i <- fread(pathchr_i, header = TRUE, colClasses = 'character', encoding = "UTF-8")
  dat_i9 <- dat_i[ICD_VERSION_IND == '9']
  dat_i10 <- dat_i[ICD_VERSION_IND == '0']
  
  match_regexp_crt(
    vs = as.data.table(vs.proc.icd9.regexp)[`Value Set Name` == vs.name],
    ff = dat_i9,
    col.match <- 'ICD_PROCEDURE_CODE',
    dir_out = dir.crt9,
    fname_out = fname
  )
  match_regexp_crt(
    vs = as.data.table(vs.proc.icd10.regexp)[`Value Set Name` == vs.name],
    ff = dat_i10,
    col.match <- 'ICD_PROCEDURE_CODE',
    dir_out = dir.crt10,
    fname_out = fname
  )
}


medical_crt_icd_proc_long <- function(vs.name,
                              dir.raw = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/icd_procedure/",
                              dir.crt9 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/icd_procedure_crt9",
                              dir.crt10 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/icd_procedure_crt10",
                              dir.hedis = "E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/"){
  fname <- paste0("MEDICAL_CLAIM_ICD_PROCEDURE#ICD_PROCEDURE_CODE#", vs.name, ".csv")
  pathchr_i <- file.path(dir.raw, fname)
  dat_i <- fread(pathchr_i, header = TRUE, colClasses = 'character', encoding = "UTF-8")
  dat_i9 <- dat_i[ICD_VERSION_IND == '9']
  dat_i10 <- dat_i[ICD_VERSION_IND == '0']
  vs.tmp9 <- filter(vs.proc.icd9.regexp, `Value Set Name` %in% vs.name)
  if(dim(vs.tmp9)[1] > 0){
    tmp9 <- strsplit(vs.tmp9$code_regexp, split='|', fixed=T)[[1]]
    nn9 <- ceiling(length(tmp9) / 500)
    vs.tmp9 <- data.frame(`Value Set Name` = rep(vs.name, nn9), code_regexp=NA, nchunk = 0)
    for(ii in 1:nn9){
      vs.tmp9$code_regexp[ii] <- paste(tmp9[((ii-1)*500+1) : (ii*500)], collapse='|')
      vs.tmp9$`Value Set Name`[ii] <- paste(vs.name, ii)
      vs.tmp9$nchunk[ii] <- 500
      match_regexp_crt(
        vs = as.data.table(vs.tmp9[ii,]),
        ff = dat_i9,
        col.match <- 'ICD_PROCEDURE_CODE',
        dir_out = dir.crt9,
        fname_out = paste0("MEDICAL_CLAIM_ICD_PROCEDURE#ICD_PROCEDURE_CODE#", 
                           vs.tmp9$`Value Set Name`[ii], ".csv")
      )
    }
  }
  vs.tmp10 <- filter(vs.proc.icd10.regexp, `Value Set Name` %in% vs.name)
  if (dim(vs.tmp10)[1] > 0){
  tmp10 <- strsplit(vs.tmp10$code_regexp, split='|', fixed=T)[[1]]
  nn10 <- ceiling(length(tmp10) / 500)
  vs.tmp10 <- data.frame(`Value Set Name` = rep(vs.name, nn10), code_regexp=NA, nchunk = 0)
  for(ii in 1:nn10){
    vs.tmp10$code_regexp[ii] <- paste(tmp10[((ii-1)*500+1) : (ii*500)], collapse='|')
    vs.tmp10$`Value Set Name`[ii] <- paste(vs.name, ii) 
    vs.tmp10$nchunk[ii] <- 500
    match_regexp_crt(
      vs = as.data.table(vs.tmp10[ii,]),
      ff = dat_i10,
      col.match <- 'ICD_PROCEDURE_CODE',
      dir_out = dir.crt10,
      fname_out = paste0("MEDICAL_CLAIM_ICD_PROCEDURE#ICD_PROCEDURE_CODE#", 
                                     vs.tmp10$`Value Set Name`[ii], ".csv")
    )
  }
  }
}

medical_crt_icd_proc_long2 <- function(vs.name,
                                       year,
                                       dir.raw = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim",
                                       dir.crt9 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_crt9",
                                       dir.crt10 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_crt10",
                                       dir.hedis = "E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/"){
  fname <- paste0("medical_", year, "#ICD_PROCEDURE_CODE#", vs.name, ".csv")
  pathchr_i <- file.path(dir.raw, fname)
  dat_i <- fread(pathchr_i, header = TRUE, colClasses = 'character', encoding = "UTF-8")
  dat_i9 <- dat_i[ICD_VERSION_IND == '9']
  dat_i10 <- dat_i[ICD_VERSION_IND == '0']
  vs.tmp9 <- filter(vs.proc.icd9.regexp, `Value Set Name` %in% vs.name)
  if(dim(vs.tmp9)[1] > 0){
    tmp9 <- strsplit(vs.tmp9$code_regexp, split='|', fixed=T)[[1]]
    nn9 <- ceiling(length(tmp9) / 500)
    vs.tmp9 <- data.frame(`Value Set Name` = rep(vs.name, nn9), code_regexp=NA, nchunk = 0)
    for(ii in 1:nn9){
      vs.tmp9$code_regexp[ii] <- paste(tmp9[((ii-1)*500+1) : (ii*500)], collapse='|')
      vs.tmp9$`Value Set Name`[ii] <- paste(vs.name, ii)
      vs.tmp9$nchunk[ii] <- 500
      match_regexp_crt(
        vs = as.data.table(vs.tmp9[ii,]),
        ff = dat_i9,
        col.match <- 'ICD_PROCEDURE_CODE',
        dir_out = dir.crt9,
        fname_out = paste0("medical_", year, "#ICD_PROCEDURE_CODE#", 
                           vs.tmp9$`Value Set Name`[ii], ".csv")
      )
    }
  }
  vs.tmp10 <- filter(vs.proc.icd10.regexp, `Value Set Name` %in% vs.name)
  if (dim(vs.tmp10)[1] > 0){
    tmp10 <- strsplit(vs.tmp10$code_regexp, split='|', fixed=T)[[1]]
    nn10 <- ceiling(length(tmp10) / 500)
    vs.tmp10 <- data.frame(`Value Set Name` = rep(vs.name, nn10), code_regexp=NA, nchunk = 0)
    for(ii in 1:nn10){
      vs.tmp10$code_regexp[ii] <- paste(tmp10[((ii-1)*500+1) : (ii*500)], collapse='|')
      vs.tmp10$`Value Set Name`[ii] <- paste(vs.name, ii) 
      vs.tmp10$nchunk[ii] <- 500
      match_regexp_crt(
        vs = as.data.table(vs.tmp10[ii,]),
        ff = dat_i10,
        col.match <- 'ICD_PROCEDURE_CODE',
        dir_out = dir.crt10,
        fname_out = paste0("medical_", year, "#ICD_PROCEDURE_CODE#", 
                           vs.tmp10$`Value Set Name`[ii], ".csv")
      )
    }
  }
}

medical_crt_dx_icd_long2 <- function(vs.name,
                             year,
                             dir.raw = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx",
                             dir.crt9 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx_crt9",
                             dir.crt10 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx_crt10",
                             dir.hedis = "E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/"){
  fname <- paste0("medical_", year, "#DIAGNOSIS_CODE#", vs.name, ".csv")
  pathchr_i <- file.path(dir.raw, fname)
  dat_i <- fread(pathchr_i, header = TRUE, colClasses = 'character', encoding = "UTF-8")
  dat_i9 <- dat_i[ICD_VERSION_IND == '9']
  dat_i10 <- dat_i[ICD_VERSION_IND == '0']
  vs.tmp9 <- filter(vs.dx.icd9.regexp, `Value Set Name` %in% vs.name)
  if(dim(vs.tmp9)[1] > 0){
    tmp9 <- strsplit(vs.tmp9$code_regexp, split='|', fixed=T)[[1]]
    nn9 <- ceiling(length(tmp9) / 500)
    vs.tmp9 <- data.frame(`Value Set Name` = rep(vs.name, nn9), code_regexp=NA, nchunk = 0)
    for(ii in 1:nn9){
      vs.tmp9$code_regexp[ii] <- paste(tmp9[((ii-1)*500+1) : (ii*500)], collapse='|')
      vs.tmp9$`Value Set Name`[ii] <- paste(vs.name, ii)
      vs.tmp9$nchunk[ii] <- 500
      match_regexp_crt(
        vs = as.data.table(vs.tmp9[ii,]),
        ff = dat_i9,
        col.match <- 'DIAGNOSIS_CODE',
        dir_out = dir.crt9,
        fname_out = paste0("medical_", year, "#DIAGNOSIS_CODE#", 
                           vs.tmp9$`Value Set Name`[ii], ".csv")
      )
    }
  }
  vs.tmp10 <- filter(vs.dx.icd10.regexp, `Value Set Name` %in% vs.name)
  if (dim(vs.tmp10)[1] > 0){
    tmp10 <- strsplit(vs.tmp10$code_regexp, split='|', fixed=T)[[1]]
    nn10 <- ceiling(length(tmp10) / 500)
    vs.tmp10 <- data.frame(`Value Set Name` = rep(vs.name, nn10), code_regexp=NA, nchunk = 0)
    for(ii in 1:nn10){
      vs.tmp10$code_regexp[ii] <- paste(tmp10[((ii-1)*500+1) : (ii*500)], collapse='|')
      vs.tmp10$`Value Set Name`[ii] <- paste(vs.name, ii) 
      vs.tmp10$nchunk[ii] <- 500
      match_regexp_crt(
        vs = as.data.table(vs.tmp10[ii,]),
        ff = dat_i10,
        col.match <- 'DIAGNOSIS_CODE',
        dir_out = dir.crt10,
        fname_out = paste0("medical_", year, "#DIAGNOSIS_CODE#", 
                           vs.tmp10$`Value Set Name`[ii], ".csv")
      )
    }
  }
}
medical_crt_dx_icd_long <- function(vs.name,
                               dir.raw = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/diagnosis/",
                               dir.crt9 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/diagnosis_crt9",
                               dir.crt10 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/diagnosis_crt10",
                               dir.hedis = "E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/"){
  # load(file.path(dir.hedis, "2018_v2_vs.regexp.rda"))
  fname <- paste0("#", vs.name, ".csv")
  pathchr_i <- file.path(dir.raw, fname)
  dat_i <- fread(pathchr_i, header = TRUE, colClasses = 'character', encoding = "UTF-8")
  dat_i9 <- dat_i[ICD_VERSION_IND == '9']
  dat_i10 <- dat_i[ICD_VERSION_IND == '0']
  vs.tmp9 <- filter(vs.dx.icd9.regexp, `Value Set Name` %in% vs.name)
  if(dim(vs.tmp9)[1] > 0){
    tmp9 <- strsplit(vs.tmp9$code_regexp, split='|', fixed=T)[[1]]
    nn9 <- ceiling(length(tmp9) / 500)
    vs.tmp9 <- data.frame(`Value Set Name` = rep(vs.name, nn9), code_regexp=NA, nchunk = 0)
    for(ii in 1:nn9){
      vs.tmp9$code_regexp[ii] <- paste(tmp9[((ii-1)*500+1) : (ii*500)], collapse='|')
      vs.tmp9$`Value Set Name`[ii] <- paste(vs.name, ii)
      vs.tmp9$nchunk[ii] <- 500
      match_regexp_crt(
        vs = as.data.table(vs.tmp9[ii,]),
        ff = dat_i9,
        col.match <- 'DIAGNOSIS_CODE',
        dir_out = dir.crt9,
        fname_out = paste0("#", vs.tmp9$`Value Set Name`[ii], ".csv")
      )
    }
  }
  vs.tmp10 <- filter(vs.dx.icd10.regexp, `Value Set Name` %in% vs.name)
  if (dim(vs.tmp10)[1] > 0){
    tmp10 <- strsplit(vs.tmp10$code_regexp, split='|', fixed=T)[[1]]
    nn10 <- ceiling(length(tmp10) / 500)
    vs.tmp10 <- data.frame(`Value Set Name` = rep(vs.name, nn10), code_regexp=NA, nchunk = 0)
    for(ii in 1:nn10){
      vs.tmp10$code_regexp[ii] <- paste(tmp10[((ii-1)*500+1) : (ii*500)], collapse='|')
      vs.tmp10$`Value Set Name`[ii] <- paste(vs.name, ii) 
      vs.tmp10$nchunk[ii] <- 500
      match_regexp_crt(
        vs = as.data.table(vs.tmp10[ii,]),
        ff = dat_i10,
        col.match <- 'DIAGNOSIS_CODE',
        dir_out = dir.crt10,
        fname_out = paste0("#", vs.tmp10$`Value Set Name`[ii], ".csv")
      )
    }
  }
}



