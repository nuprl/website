## Setup

1. Create personal access token on github with `public_repo` permission.
2. Install travis gem `gem install travis`.
3. Encrypt access token with:

```
travis encrypt  GH_TOKEN=THEGENERATEDTOKEN -r nuprl/website
```

Pasting the result in the `.travis.yml` file inside the env section.

4. The rest should be well-enough documented by `.travis.yml`, and the
   associated scripts `install-racket.sh` and `deploy.sh`.
