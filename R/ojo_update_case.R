#' Update a case record in the Open Justice Oklahoma Database
#'
#' Makes an API which triggers a fresh scrape of all case information for a given case id
#'
#' @aliases ojo_update_case
#' @export ojo_update_case
#' @examples
#' \dontrun{
#' ojo_update_case(id)
#' }
#'

ojo_update_case <- function(case_id = NA, district = NA, case_number = NA, ..., .silent = T) {

  ## Check if case_id is supplied
  if(is.na(case_id)) {
    ## If not, then both district and case_number must be supplied
    if(is.na(district) |
       is.na(case_number)) {
      stop("Either 'case_id' or both 'district' and 'case_number' must be supplied")
    }

    if(!silent) {
      print("Using 'district' and 'case_number' to create API call")
    }
  }

  if(!silent) {
    print("Using 'case_id' to create API call")
  }

  ## Create URL

  # Sample ID
  #{"district": "CUSTER", "case_number": "SC-2006-00077"}

  d <- case_id |>
    stringr::str_extract("(?<=district\\W{4})[[:alpha:]]*") |>
    tolower()

  cn <- case_id |>
    stringr::str_extract("(?<=case_number\\W{4})(\\w*-*)*")

  record_url <- stringr::str_c("https://www.oscn.net/dockets/GetCaseInformation.aspx?",
                                  "db=",
                                  d,
                                  "&number=",
                                  cn)


  ## Use POST

  res <- httr::POST(url = "http://34.122.35.127/case",
             body = record_url,
             add_headers(
               Connection = "keep-alive",
               `Accept-Encoding` = "gzip, deflate, br",
               Accept = "*/*",
               api_key = Sys.getenv("NEW_OJO_UPDATE_API")
             ),
             user_agent("PostmanRuntime/7.28.3"),
             content_type("text/plain"),
             accept_json(),
             timeout(300))


  ## Parse Response


  ## Sample Success
  # Completed: ((upserted_parties 5) (upserted_attorneys 2) (upserted_representations 0)
  #             (upserted_attorney_addresses 2) (upserted_issues 0) (upserted_citations 0)
  #             (upserted_appellate_count 0) (upserted_counts 0) (upserted_minutes 68)
  #             (upserted_events 16) (upserted_addresses 5) (upserted_marital_statuses 5)
  #             (upserted_person_records 5) (upserted_cases 1)
  #             (stats
  #               ((fetching_time 33191.511392593384) (parsing_time 99.988222122192383)
  #                 (db_time 179.02493476867676))))

  ## Sample Fail
  # Completed: ((upserted_parties 0) (upserted_attorneys 0) (upserted_representations 0)
  #             (upserted_attorney_addresses 0) (upserted_issues 0) (upserted_citations 0)
  #             (upserted_appellate_count 0) (upserted_counts 0) (upserted_minutes 0)
  #             (upserted_events 0) (upserted_addresses 0) (upserted_marital_statuses 0)
  #             (upserted_person_records 0) (upserted_cases 0)
  #             (stats ((fetching_time 0) (parsing_time 0) (db_time 37.070989608764648))))
}

