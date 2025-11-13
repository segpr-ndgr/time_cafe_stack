find_owncloud_path <- function() {
  home <- normalizePath("~") # Obtém o diretório home do usuário

  # Possíveis locais onde o OwnCloud pode estar instalado
  possible_paths <- c(
    file.path(home, "ownCloud"),
    file.path(home, "Owncloud"),
    file.path(home, "OwnCloud"),
    file.path("C:/Users", Sys.getenv("USERNAME"), "ownCloud"),
    file.path("C:/Users", Sys.getenv("USERNAME"), "OwnCloud"),
    file.path("/mnt/owncloud"),
    file.path("/Volumes/ownCloud")
  )

  # Verifica qual diretório existe e retorna o primeiro encontrado
  owncloud_path <- possible_paths[file.exists(possible_paths)]

  if (length(owncloud_path) > 0) {
    return(owncloud_path[1])  # Retorna o primeiro caminho encontrado
  } else {
    stop("OwnCloud não encontrado! Verifique se está instalado e sincronizado.")
  }
}

