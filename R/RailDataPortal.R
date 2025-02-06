nrdp_authenticate = function(username = Sys.getenv("NRDP_username"),
                             password = Sys.getenv("NRDP_password")){

  # Make the POST request
  form_data <- list(
    username = username,
    password = password
  )

  response <- httr::POST(
    url =  'https://opendata.nationalrail.co.uk/authenticate',
    body = form_data,
    encode = "form"
  )

  json = httr::content(response)

  return(json)


}

nrdp_timetable = function(destfile = "timetable.zip",
                          username = Sys.getenv("NRDP_username"),
                          password = Sys.getenv("NRDP_password")){

  url = "https://opendata.nationalrail.co.uk/api/staticfeeds/3.0/timetable"

  token = nrdp_authenticate(username, password)

  response <- httr::GET(
    url =  url,
    httr::add_headers(`X-Auth-Token` = token$token)
  )

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    # Write the content of the response to a file
    writeBin(httr:::content(response, "raw"), destfile)
    cat("File downloaded successfully to", destfile)
  } else {
    cat("Failed to download the file. Status code:", status_code(response))
  }


}

nrdp_timetable = function(destfile = "timetable.zip",
                          username = Sys.getenv("NRDP_username"),
                          password = Sys.getenv("NRDP_password"),
                          url = "https://opendata.nationalrail.co.uk/api/staticfeeds/3.0/timetable"){



  token = nrdp_authenticate(username, password)

  response <- httr::GET(
    url =  url,
    httr::add_headers(`X-Auth-Token` = token$token)
  )

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    # Write the content of the response to a file
    writeBin(httr:::content(response, "raw"), destfile)
    cat("File downloaded successfully to", destfile)
  } else {
    cat("Failed to download the file. Status code:", status_code(response))
  }


}


nrdp_fares = function(destfile = "fares.zip",
                          username = Sys.getenv("NRDP_username"),
                          password = Sys.getenv("NRDP_password"),
                          url = "https://opendata.nationalrail.co.uk/api/staticfeeds/2.0/fares"){



  token = nrdp_authenticate(username, password)

  response <- httr::GET(
    url =  url,
    httr::add_headers(`X-Auth-Token` = token$token)
  )

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    # Write the content of the response to a file
    writeBin(httr:::content(response, "raw"), destfile)
    cat("File downloaded successfully to", destfile)
  } else {
    cat("Failed to download the file. Status code:", status_code(response))
  }


}

