css:
  tailwindcss -i static/input.css -o static/output.css

js:
  spago bundle

hs:
  openapi-generator-cli generate -g haskell-yesod -i specification/specification.yaml -o generated/
  mkdir -p lib/Jweb config
  cp generated/src/Jweb/Types.hs lib/Jweb/Types.hs
  cp generated/config/routes.yesodroutes config/routes.yesodroutes

all: css js hs
