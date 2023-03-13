# Authentication steps for the first time connecting R Studio with GitHub

'https://carpentries.github.io/sandpaper-docs/github-pat.html'

#By Zhian Kamvar


#PAT: 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

library("usethis")
library("credentials")
git_sitrep() # git situation report

#Read more about what this GitHub remote configurations means at:
'https://happygitwithr.com/common-remote-setups.html'

usethis::git_remotes()
usethis::use_github(organisation = "Burning-Issues-Watersheds")

usethis::create_github_token()
gitcreds::gitcreds_set() 
usethis::git_sitrep()
usethis::git_credentials_forget()

#Source: https://nceas.github.io/scicomp-workshop-collaborative-coding/#github

# Install the `usethis` and `gitcreds` packages
#install.packages(c("usethis", "gitcreds"))
library(usethis)
library(gitcreds)

# Add your GitHub username and email
usethis::use_git_config(user.name = "Francisco J. Guerrero",
                        user.email = "guerrero.francisco.jose@gmail.com")

# Create a token (Note this will open a GitHub browser tab)
# See steps 6-10 in GitHub's tutorial (link below)
usethis::create_github_token()

# Copy your Personal Access Token at the end of the above step!

# Now, give your token to RStudio
# After you run this line you'll follow some prompts in the "Console" tab of RStudio
gitcreds::gitcreds_set()

usethis::git_sitrep()
