[%%import "../config.h"]
[%%if DUNE_BUILD_PROFILE = "release"]

let unsafe_is_safe = false

[%%else]

let unsafe_is_safe = true

[%%endif]
