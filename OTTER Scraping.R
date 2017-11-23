get_county_tanks <- function(county_num) {
  remDr$open(silent = TRUE)
  
  remDr$navigate("https://apps.com.ohio.gov/fire/otter/#")
  
  ###locate the reset button and hit it
  webElem_resetbutton <- remDr$findElement(using = 'class', 
                                           "resetForm")
  
  webElem_resetbutton$clickElement()
  Sys.sleep(5)
  
  ### select a county
  webElem_countid <- remDr$findElement(using = 'xpath',
                                       paste0("//select[(@id = 'CountyId')]/option[@value = '",  
                                              county_num , "']")
  )
  webElem_countid$clickElement()
  
  ## find and click the search button
  webElem_searchbutton <- remDr$findElement(using = 'class', 
                                            "SearchFacilities")
  
  webElem_searchbutton$clickElement()
  Sys.sleep(5)
  
  county_tanks <- list()
  for (j in 1:150) {
    doc <- htmlParse(remDr$getPageSource()[[1]])
    fac_tab <- readHTMLTable(doc)[[1]] %>% as_tibble()
    
    if (j > 1) {
      check <- suppressWarnings(dataCompareR::rCompare(fac_tab, prev_tab))
      if (length(check$mismatches) == 0) break
    }
    
    county_tanks[[j]] <- fac_tab
    
    prev_tab <- fac_tab
    
    ###find and click next button
    webElem_nextbutton <- remDr$findElement(using = 'class', "next")
    webElem_nextbutton$clickElement()
    
    Sys.sleep(2)
  }
  
  county_tanks <- bind_rows(county_tanks)
  
  remDr$close()
  return(county_tanks)
}

get_facilities <- function(facility_ID) {
  remDr$open(silent = TRUE)
  
  remDr$navigate("https://apps.com.ohio.gov/fire/otter/#")
  
  ###locate the reset button and hit it
  webElem_resetbutton <- remDr$findElement(using = 'class', 
                                           "resetForm")
  
  webElem_resetbutton$clickElement()
  Sys.sleep(3)
  
  ###find the id search box and type ID
  webElem_IDsearch <- remDr$findElement(using = "id", "FacilityNumber")
  webElem_IDsearch$sendKeysToElement(list(facility_ID))
  
  ## find and click the search button
  webElem_searchbutton <- remDr$findElement(using = 'class', "SearchFacilities")
  
  webElem_searchbutton$clickElement()
  Sys.sleep(3)
  
  ## find and click the first link
  for(i in 1:20){
    try({
      Sys.sleep(0.5 * i)
      webElems_links <- remDr$findElements(using = 'xpath', "//a[@href]")
      link_headers <-  unlist(lapply(webElems_links, function(x) x$getElementText()))
      print(link_headers[1:5])
      webElems_link <- webElems_links[[which(grepl(facility_ID, link_headers))[1]]]
      webElems_link$clickElement()
      break
    }, silent = F)
  }
  
  
  
  ## click the detal link
  Sys.sleep(2)
  for(i in 1:20){
    try({
      Sys.sleep(0.5 * i)
      webElem_tankLinks <- remDr$findElements(using = 'xpath', "//a[@href]")
      link_headers <-  unlist(lapply(webElem_tankLinks, function(x) x$getElementText()))
      print(link_headers[1:5])
      webElem_tank <- webElem_tankLinks[[which(grepl("Details", link_headers))]]
      break #break/exit the for-loop
    }, silent = FALSE)
  }
  
  
  
  webElem_tank$clickElement()
  
  ###get the tank list
  doc <- htmlParse(remDr$getPageSource()[[1]])
  fac_tab <- readHTMLTable(doc)[[1]] %>% as_tibble()
  
  ### process the list to remove things shouldn't be captured
  fac_tab1 <- fac_tab[, -1]
  col_names <- fac_tab1[2, ] %>% t %>% as.vector
  fac_tab1 <- fac_tab1[-nrow(fac_tab1), ]
  fac_tab1 <- fac_tab1[-(1:2), ]
  colnames(fac_tab1) <- col_names
  
  remDr$closeall()
  
  fac_tab1$Facility_ID <- facility_ID
  
  return(fac_tab1)
  
}

get_facilities_safe  <- function(facility_ID) {
  tryCatch({
    return(get_facilities(facility_ID))
    }, error=function(e) {
      return(NULL)
    }
  )

}

get_releases <- function(facility_ID) {
  remDr$open(silent = TRUE)
  
  remDr$navigate("https://apps.com.ohio.gov/fire/otter/#")
  
  ###locate the reset button and hit it
  webElem_resetbutton <- remDr$findElement(using = 'class',
                                           "resetForm")

  webElem_resetbutton$clickElement()
  Sys.sleep(2)

  ###find the id search box and type ID
  webElem_IDsearch <- remDr$findElement(using = "id", "FacilityNumber")
  webElem_IDsearch$sendKeysToElement(list(facility_ID))
  
  ## find and click the search button
  webElem_searchbutton <- remDr$findElement(using = 'class', "SearchFacilities")
  
  webElem_searchbutton$clickElement()
  Sys.sleep(2)
  
  webElem_releaselinks <- remDr$findElements(using = 'xpath', 
              "//*[contains(concat( ' ', @class, ' ' ), concat( ' ' , 'releaseLink', ' '  ))]")
  release_links_txt <- unlist(lapply(webElem_releaselinks, function(x) x$getElementText()))
  
  get_release_detail  <- function(webElem_releaselink) {
    release_txt <- webElem_releaselink$getElementText()[[1]]
    webElem_releaselink$clickElement()
    
    Sys.sleep(3)
    
    currWindow <-  remDr$getCurrentWindowHandle()
    
    windows <- remDr$getWindowHandles()
    
    remDr$switchToWindow(windows[[2]])
    
    
    for(i in 1:20){
      try({
        Sys.sleep(0.5 * i)
        webElem_status  <- remDr$findElement(using = 'xpath',  '//*[@id="RefStatusesID"]')
        webElem_LTFstatus  <- remDr$findElement(using = 'xpath',  '//*[@id="RefLTFCodesID"]')
        webElem_CaClasses  <- remDr$findElement(using = 'xpath',  '//*[@id="RefCaClassesID"]')
        webElem_remarks  <- remDr$findElement(using = 'xpath',  ' //*[@id="div_content10"]/div[4]/div[4]')
        webElem_Substatus  <- remDr$findElement(using = 'xpath',  '//*[@id="RefSubStatusesID"]')
        webElem_LeakAutopsyCauses1  <- remDr$findElement(using = 'xpath', 
                                            '//*[@id="divLeakAutopsyCauses"]/table/tbody/tr[1]/td[1]/input')
        webElem_LeakAutopsyCauses2  <- remDr$findElement(using = 'xpath', 
                                            '//*[@id="divLeakAutopsyCauses"]/table/tbody/tr[2]/td[1]/input')
        webElem_LeakAutopsyCauses3  <- remDr$findElement(using = 'xpath', 
                                            '//*[@id="divLeakAutopsyCauses"]/table/tbody/tr[3]/td[1]/input')
        webElem_LeakAutopsyCauses4  <- remDr$findElement(using = 'xpath', 
                                            '//*[@id="divLeakAutopsyCauses"]/table/tbody/tr[4]/td[1]/input')
        webElem_LeakAutopsyCauses5  <- remDr$findElement(using = 'xpath', 
                                            '//*[@id="divLeakAutopsyCauses"]/table/tbody/tr[5]/td[1]/input')
        webElem_LeakAutopsyCauses6  <- remDr$findElement(using = 'xpath', 
                                            '//*[@id="divLeakAutopsyCauses"]/table/tbody/tr[6]/td[1]/input')
        webElem_LeakAutopsyCauses7  <- remDr$findElement(using = 'xpath', 
                                            '//*[@id="divLeakAutopsyCauses"]/table/tbody/tr[7]/td[1]/input')

        break

        cat("status", i, "\n")
      }, silent = F)
    }
    
    opts <- webElem_status$selectTag()
    release_status <- opts$text[opts$selected]
    
    opts <- webElem_LTFstatus$selectTag()
    LTFstatuss <- opts$text[opts$selected]
    
    opts <- webElem_CaClasses$selectTag()
    CaClasses <- opts$text[opts$selected]
    
    opts <- webElem_Substatus$selectTag()
    Substatus <- opts$text[opts$selected]
    
    remarks <-  webElem_remarks$getElementText()[[1]]
    assign_checked <- function(x) {
      if (is_empty(x)) "false" else x[[1]]
    }
    
    cause1 <- webElem_LeakAutopsyCauses1$getElementAttribute("checked") %>% assign_checked()
    cause2 <- webElem_LeakAutopsyCauses2$getElementAttribute("checked") %>% assign_checked()
    cause3 <- webElem_LeakAutopsyCauses3$getElementAttribute("checked") %>% assign_checked()
    cause4 <- webElem_LeakAutopsyCauses4$getElementAttribute("checked") %>% assign_checked()
    cause5 <- webElem_LeakAutopsyCauses5$getElementAttribute("checked") %>% assign_checked()
    cause6 <- webElem_LeakAutopsyCauses6$getElementAttribute("checked") %>% assign_checked()
    cause7 <- webElem_LeakAutopsyCauses7$getElementAttribute("checked") %>% assign_checked()
    
    
    
    df <- tibble(Facility_ID = facility_ID, ReleaseNumber = release_txt
                 , Releasestatus = release_status, LTFstatuss = LTFstatuss, 
                 CaClasses = CaClasses, 
                 remarks = remarks, 
                 Substatus = Substatus,
                 cause1 = cause1, 
                 cause2 = cause2, 
                 cause3 = cause3, 
                 cause4 = cause4, 
                 cause5 = cause5, 
                 cause6 = cause6, 
                 cause7 = cause7
                 )
    remDr$closeWindow()
    remDr$switchToWindow(currWindow[[1]])
    return(df)
  }
 
  get_release_detail_safe  <- function(webElem_releaselink) {
    tryCatch({
      for(i in 1:5){
        try({
            return(get_release_detail(webElem_releaselink))
        })
      }
    }, error=function(e) {
      #print(facility_ID)
      cat(facility_ID, "\n")
      return(NULL)
    }
  )}
  
  rel_df <- bind_rows(lapply(webElem_releaselinks, get_release_detail_safe))
  remDr$closeall()
  P <<- P + 1 
  cat(P, " finished", "\n")
  return(rel_df)
} 

get_releases_safe <-  function(facility_ID) {
  tryCatch({
    for (i in 1:20) {
      try({
          Sys.sleep( (i-1) * 1.5 )
          if(i > 1) cat("try ", i, "\n")
          rlt <- get_releases(facility_ID)
          if(!is.null(rlt)) {
              return(rlt)
              break
          }
      } , silent = F) 
    }
  }, error = function(e) return(NULL)
  )
  return(NULL)
}