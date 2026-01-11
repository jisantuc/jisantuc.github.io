# `jisantu.github.io`

## Deployment

Notes for future me:

* set personal access token with read/write permissions (repository -> `CONTENT` in permissions-speak)
  in the repo actions secrets. This token expires every `n` days, so if you
  get auth failures, you probably need to rotate it.
* don't forget to run commands in the CI shell since it has so many fewer dependencies
