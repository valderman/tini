tini: a Tiny INI file and generic configuration library
=======================================================

What?
-----
This library provides tools for working with INI-like configuration files.
Unlike most other INI libraries, it is not blazing fast or built with
cutting-edge libraries. Instead, it assumes that you are using INI-like
configuration files because you want something simple and lightweight.

Tini provides the following features:

* A simple interface to INI configuration: reading/writing an INI object
  object to/from files and getting/setting/removing its values.
* Automatic de/serialization of lists, optional values and base types.
* High-level generic configuration type library built on top, freeing you
  from ever having to think about how you interact with your INI files.
* Certified bloat-free: only depends on @base@.
* Small and auditable: <450 lines of code excluding documentation.


Why?
----
Don't use a sledgehammer with several dozen transitive dependencies when a
light touch will do the trick.

I tried to find an INI parsing library which wouldn't triple the number of
dependencies in a small, security-critical project I was working on, so I
wrote my own. This library depends only on @base@, and uses simple string
splitting to parse INI files.
This approach may be significantly slower and less "proper" than using a
state of the art parsing combinator library, but it's also a lot simpler.


How?
----
### Reading/writing an INI file:
Tini provides simple basic functionality for reading and writing INI files.

**config.ini**:
```
[User]
name = Bilbo
; Bilbo is ooold!
age = 110
```

**example1.hs**:
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Tini

main = do
  Just ini <- readIniFile "config.ini"
  let Just user = get ini "User.name"
      Just age = get ini "User.age"
  putStrLn ("Hello " ++ user ++ ", you just got one year older!")
  writeIniFile "config.ini" (set ini "User.age" (age+1 :: Int))
```

**Output**:
`Hello Bilbo, you just got one year older!`

**config.ini, post-update**:
```
[User]
name = Bilbo
; Bilbo is ooold!
age = 111
```


### Automatically (de)serializing a config
Often, you don't really care about the specifics of your configuration; you
just want to be able to serialize and de-serialize a configuration data type
in a more robust manner than what `show` and `read` provides.
This is when you use `Data.Tini.Configurable`.

**config.ini**:
```
; Note the lack of a section heading!
name = Bilbo
age = 110
```

**example2.hs**:
```haskell
{-# LANGUAGE DeriveGeneric #-}
import Data.Tini.Configurable

data User = User
  { name :: String
  , age :: Int
  } deriving Generic

instance Configurable User where
  defaultConfig = User { name = "Default User", age = 0 }

main = do
  conf <- readConfigFile "config.ini"
  putStrLn ("Hello " ++ name conf ++ ", you just got one year older!")
  updateConfigFile "config.ini" (conf { age = age conf + 1 })
```

**config.ini, post-update**:
```
; Note the lack of a section heading!
name = Bilbo
age = 111
```


### Adding a section header
Sometimes we want to split our configuration into several different types,
but still keep our whole configuration in one single file.
To accomplish this, we can associate a *section* which each configurable
type.

Note that the `updateConfigFile` function does not clobber the config file,
but rather updates it with the contents of the configuration object being
written.

**config.ini**:
```
[User]
name = Bilbo
age = 110
```

**example3.hs**:
```haskell
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
import Data.Tini (readIniFile)
import Data.Tini.Configurable

data User = User
  { name :: String
  , age :: Int
  } deriving Generic

data NetworkConfig = NetworkConfig
  { hostname :: String
  , bandwidthLimit :: Maybe Int -- KB/s
  } deriving Generic

instance Configurable User where
  defaultConfig = User
    { name = "Default User"
    , age = 0
    }
  sectionName = "User"

instance Configurable NetworkConfig where
  defaultConfig = NetworkConfig
    { hostname = ""
    , bandwidthLimit = Nothing
    }
  sectionName = "Network"

main = do
  Just ini <- readIniFile "config.ini"
  let user = fromIni ini
      network = fromIni ini
  putStrLn ("Hello " ++ name user ++ ", you just got one year older!")
  updateConfigFile "config.ini" (user { age = age user + 1 })
  putStrLn ("You also just got bandwidth limited!")
  updateConfigFile "config.ini" (network { bandwidthLimit = Just 100 })
```

**config.ini, post-update**:
```
[User]
name = Bilbo
age = 111

[Network]
hostname =
bandwidthLimit = 100
```


### Excluding fields
It may be that we have some values in our configuration that we want to be
easily configurable when recompiling our program, but which we *don't* want
the end user of the program to be able to modify.
One example might be the location of the configuration file itself.

We can accomplish this by adding the field representing the location of
the configuration file to the `ExcludedFields` associated type.
As `ExcludedFiles` is a type-level list, we can check at compile-time that
all fields listed therein are actually fields of our configuration type.
Adding any other field to that list results in a type error.

**config.ini**:
```
userName = Bilbo
userAge = 110
```

**example4.hs**:
```haskell
{-# LANGUAGE DeriveGeneric, DataKinds, TypeFamilies #-}
import Data.Tini.Configurable

data Config = Config
  { userName :: String
  , userAge :: Int
  , configLocation :: FilePath
  } deriving Generic

instance Configurable Config where
  defaultConfig = Config
    { userName = "Default User"
    , userAge = 0
    , configLocation = ""
    }
  type ExcludedFields Config = '["configLocation"]

myConfig = Config
  { userName = "My Default User"
  , userAge = 0
  , configLocation = "config.ini"
  }

main = do
  conf <- readConfigFileWith myConfig (configLocation myConfig)
  putStrLn ("Hello " ++ userName conf ++ ", you just got one year older!")
  updateConfigFile "config.ini" (conf { userAge = userAge conf + 1 })
```

**config.ini, post-update**:
```
name = Bilbo
age = 111
```