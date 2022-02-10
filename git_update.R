#!/usr/bin/Rscript
# Sorry this isn't elegant but necessary for the cron tab to work
setwd("~/Projects/Graphics/2020-log-scales/")

# Set up authentication via ssh
cred <- git2r::cred_ssh_key("~/.ssh/id_rsa.pub", "~/.ssh/id_rsa")
repo <- git2r::repository()
git2r::config(repo = repo, global = F, "Susan-auto", "srvanderplas@gmail.com")

# Log job start
httr::POST("https://hc-ping.com/00fb59c2-a334-44ad-899a-1927e3d18023/start")

# Check repo status
status <- git2r::status()

tmp <- status$unstaged
modified <- names(tmp) == "modified"
modified <- unlist(tmp[modified])

# If db has been modified
if ("lineups-pilot-app/exp_data.db" %in% modified |
    "you-draw-it-development/you-draw-it-pilot-app/you_draw_it_data.db" %in% modified | 
    "estimation-development/estimation-pilot-app/estimation_data.db" %in% modified) {

  # Add changed db to commit and commit
  git2r::add(repo = '.', "lineups-pilot-app/exp_data.db")
  try(git2r::commit("lineups-pilot-app/exp_data.db", message = "Update lineup data"))
  git2r::add(repo = '.', "you-draw-it-development/you-draw-it-pilot-app/you_draw_it_data.db")
  try(git2r::commit("you-draw-it-development/you-draw-it-pilot-app/you_draw_it_data.db", message = "Update you-draw-it data"))
  git2r::add(repo = '.', "estimation-development/estimation-pilot-app/estimation_data.db")
  try(git2r::commit("estimation-development/estimation-pilot-app/estimation_data.db", message = "Update estimation data"))
  
  # Update
  git2r::pull(repo = repo, credentials = cred)
  git2r::push(getwd(), credentials = cred)

  if (length(git2r::status()$unstaged$conflicted) > 0) {
    # Log merge conflict, signal failure (Susan gets an email)
    httr::POST("https://hc-ping.com/00fb59c2-a334-44ad-899a-1927e3d18023/fail", body = "Merge conflict")
  } else {
    # Log success
    httr::POST("https://hc-ping.com/00fb59c2-a334-44ad-899a-1927e3d18023", body = "Changes pushed")
  }
} else {
  # Log no changes
  httr::POST("https://hc-ping.com/00fb59c2-a334-44ad-899a-1927e3d18023", body = "No changes")
}

git2r::config(repo = repo, global = F, "Susan Vanderplas", "srvanderplas@gmail.com")
