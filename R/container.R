#' Quick running docker container
#'
#' @param image docker image name (character)
#' @param name docker container name (character). By default the project's name will be used.
#' @param theme_succession a logical value indicating whether employ local RStudio theme & setting.
#' @param ... other parameter set to docker::docker$clientcontainers$run object
#' @import docker
#' @importFrom here here
#' @examples
#' \dontrun{
#' rss_instance <- docker_quick_start(image = "uribo/tiny_rocker_minimum",
#' remove  = TRUE,
#' detach  = TRUE)
#' rss_instance$start()
#' rss_instance$stop()
#' }
#' @export
docker_quick_start <- function(image,
                               name = NULL,
                               theme_succession = FALSE,
                               ...) {
  client <- docker::docker$from_env()
  project_path <- here::here()
  container_name <- basename(project_path)

  mount <- list(list("bind" = paste0("/home/rstudio/", container_name),
                     "mode" = "rw"))
  names(mount) <- project_path

  if (dir.exists("~/.R/snippets") == TRUE) {
    mount[[2]] <- list("bind" = "/home/rstudio/.R/snippets",
                       "mode" = "rw")
    names(mount)[2] <- path.expand("~/.R/snippets")

  }

  container_name <- if (is.null(name) == TRUE) {
    container_name
  } else {
    NULL
  }

  rss_instance <- client$containers$run(image   = image,
                                        volumes = mount,
                                        name    = container_name,
                                        environment = list("ROOT" = "TRUE"),
                                        ports   = list("8787/tcp" = "8787"),
                                        ...)

  rss_instance$start()

  # Wait a while...
  Sys.sleep(3)
  browseURL("http://localhost:8787")

  if (theme_succession == TRUE) {
    container_id <- client$containers$list(all = TRUE, filters = list("name" = container_name))[[1]]$id

    Sys.sleep(1)
    system(
      paste('docker cp',
            list.files("~/.rstudio-desktop/monitored/user-settings",
                       full.names = TRUE),
            paste0(container_id,
                   ':/home/rstudio/.rstudio/monitored/user-settings/'))
    )
  }

  return(rss_instance)
}

#' Containers process view
#'
#' @import docker
#' @importFrom dplyr select
#' @importFrom purrr map map_chr
#' @importFrom stringr str_extract str_replace_all
#' @importFrom tibble tibble
containers_view <- function() {

  client <- docker::docker$from_env()

  res <- client$containers$list(all = TRUE) %>%
    purrr::map_chr(~ .$id) %>%
    purrr::map(~ client$containers$get(container_id = .x))

  # res %>% purrr::map_dfr(., ~ .$labels %>%
  #                       tibble::as_data_frame(.))

  df_containers <- res %>%
    tibble::tibble(
      id = purrr::map_chr(., ~ .$id),
      image = purrr::map(., ~ .$image %>% as.character %>%
                           stringr::str_extract(pattern = "'.+'")) %>%
        purrr::map_chr(
          ~ if (is.na(.x) == TRUE) {
            NA_character_
          } else {
            stringr::str_replace_all(., pattern = "'", replacement = "")
          }
        ),
      name = purrr::map_chr(., ~ .$name),
      status = purrr::map_chr(., ~ .$status)
    ) %>%
    dplyr::select(-1)

  return(df_containers)
}
