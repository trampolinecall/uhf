{
    roots = [
        "^Main.main$",
        "^Paths_.*",
        "^.+\\.case_.*",
        "^.+\\.test_.*",
        "^.+\\.tests",
        "^UHF\\.Util\\.Prelude\\.trace.*",
        "^UHF\\.Diagnostic\\.Codes\\.CodeTH\\..*", -- only used in template haskell splices
        "UHF.IO.Location.SpanHelper\\..*", -- only used in tests
    ],
    type-class-roots = True,
}
