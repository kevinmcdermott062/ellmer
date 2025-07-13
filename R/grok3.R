library(httr)
library(R6)

#' Grok 3 API Client
#'
#' An R6 class to interact with xAI's Grok 3 API.
#' @export
Grok3Client <- R6Class("Grok3Client",
                       public = list(
                         api_key = NULL,
                         model = NULL,
                         base_url = "https://api.x.ai/v1",
                         
                         #' Initialize the Grok3Client
                         #' @param api_key API key for xAI (defaults to XAI_API_KEY environment variable).
                         #' @param model Grok 3 model to use (default: "grok-3").
                         initialize = function(api_key = NULL, model = "grok-3") {
                           if (is.null(api_key)) {
                             api_key <- Sys.getenv("XAI_API_KEY")
                             if (api_key == "") {
                               stop("XAI_API_KEY environment variable or api_key parameter is required.")
                             }
                           }
                           self$api_key <- api_key
                           self$model <- model
                         },
                         
                         #' Send a chat completion request
                         #' @param prompt The user prompt.
                         #' @param max_tokens Maximum tokens for the response (default: 1000).
                         #' @param stream Enable streaming (default: FALSE).
                         #' @return Parsed API response.
                         chat_completion = function(prompt, max_tokens = 1000, stream = FALSE) {
                           headers <- add_headers(
                             `Authorization` = paste("Bearer", self$api_key),
                             `Content-Type` = "application/json"
                           )
                           
                           body <- list(
                             model = self$model,
                             messages = list(list(role = "user", content = prompt)),
                             max_tokens = max_tokens,
                             stream = stream
                           )
                           
                           response <- tryCatch({
                             POST(
                               url = paste0(self$base_url, "/chat/completions"),
                               headers,
                               body = body,
                               encode = "json"
                             )
                           }, error = function(e) {
                             stop("Grok 3 API error: ", e$message)
                           })
                           
                           if (status_code(response) != 200) {
                             stop("API request failed with status ", status_code(response), ": ", content(response, "text"))
                           }
                           
                           content(response, as = "parsed")
                         },
                         
                         #' Retrieve available Grok 3 models
                         #' @return List of available models.
                         get_models = function() {
                           headers <- add_headers(
                             `Authorization` = paste("Bearer", self$api_key)
                           )
                           
                           response <- tryCatch({
                             GET(
                               url = paste0(self$base_url, "/models"),
                               headers
                             )
                           }, error = function(e) {
                             stop("Error retrieving models: ", e$message)
                           })
                           
                           if (status_code(response) != 200) {
                             stop("API request failed with status ", status_code(response), ": ", content(response, "text"))
                           }
                           
                           content(response, as = "parsed")
                         }
                       )
)

#' Chat function for Grok 3 API
#'
#' A wrapper for Grok3Client to use with the gander package.
#' @param prompt The user prompt.
#' @param model Grok 3 model to use (default: "grok-3").
#' @param max_tokens Maximum tokens for the response (default: 1000).
#' @param api_key Optional API key (defaults to XAI_API_KEY environment variable).
#' @return The response text from the Grok 3 API.
#' @export
chat_grok3 <- function(prompt, model = "grok-3", max_tokens = 1000, api_key = NULL) {
  client <- Grok3Client$new(api_key = api_key, model = model)
  response <- client$chat_completion(prompt, max_tokens = max_tokens, stream = FALSE)
  response$choices[[1]]$message$content
}



