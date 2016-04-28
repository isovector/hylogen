# [*H Y L O G E N*](https://hylogen.com)

Hylogen is a purely functional language [embedded in Haskell](https://wiki.haskell.org/Embedded_domain_specific_language) for live-coding fragment shaders.

## Setup
```
cabal update
cabal install hylogen
```

## Usage

```haskell
-- ./Main.hs
module Main where
import Hylogen

color = vec4 (a, a, a, 1)
  where
    a = cos(X uvN * sin(time/ 10) * 10 + X mouse)
      + sin(Y uvN * sin(time / 10) * 10 + Y mouse)

main = putStrLn $ toGLSL $ color
```

#### 1. run hylogen server

```
hylogen Main.hs
```

#### 2. play!
Visit [localhost:5678](http://localhost:5678) in your browser.

Changes in `Main.hs` will now be propagated in realtime to your WebGL rendering context!

## inspiration
- [The_Force](https://github.com/shawnlawson/The_Force)

## resources
[hackage](https://hackage.haskell.org/package/hylogen)

[examples](https://github.com/sleexyz/hylogen-yay)
