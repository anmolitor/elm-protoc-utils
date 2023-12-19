# Elm Protoc Utils

Utility functions for usage in [protoc-gen-elm](https://github.com/anmolitor/protoc-gen-elm) and your protobuf application.

## Contents

- Functions that are used by [protoc-gen-elm](https://github.com/anmolitor/protoc-gen-elm)'s generated code (e.g. encoding JSON to bytes using Hex Encoding)
- Functions that help bridge that gap between "Protobuf" Elm Types and "Standard" Elm Types (e.g. timestampToPosix)

## Why an extra package?

I do not want protoc-gen-elm to generate lots of code but some tasks are pretty complex. One option would be to generate utility functions like this package but only if they are used, however, this pushes a lot of complexity towards the code generator, especially for more complicated functions that should be tested as well.
This also enables us to use sub-dependencies. See it this way: if protoc-gen-elm generated code that used 3 sub-dependencies, you would to manually have to add these dependencies to your elm.json file. This way, you (hopefully) only need to add this one.
