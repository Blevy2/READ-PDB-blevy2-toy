# testpush.R

# just a test file to see if Chris can push from RStudio on MARS server
# first attempt failed - user name and login approach no longer supported

# so went to https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token
# and followed directions to create personal access token
# success! - not so fast, PAT is not stored

# so on to the next method
# https://gist.github.com/Z3tt/3dab3535007acf108391649766409421
## create a personal access token for authentication:
usethis::browse_github_token() 

## set personal access token:
credentials::set_github_pat("MARS") # MARS is the name I saved the token as




