css:
  tailwindcss -i static/input.css -o static/output.css

js:
  spago bundle

hs:
  openapi-generator-cli generate -g haskell-yesod -i specification/specification.yaml -o generated/
  mkdir -p lib/Jweb config
  cp generated/src/Jweb/Types.hs lib/Jweb/
  cp generated/config/routes.yesodroutes config/
  cp specification/specification.yaml static/

ts:
  npx tsc

test-db := "jweb-test.db"

test: ts
  rm -f {{test-db}} {{test-db}}-*
  spago test

serve-test:
  #!/usr/bin/env bash
  export JWEB_DB_PATH={{test-db}}
  export JWEB_TEST_MODE=1
  cabal run jweb

all: css js hs
