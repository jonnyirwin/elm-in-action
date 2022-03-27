module Test.Generated.Main exposing (main)

import PhotoGrooveTests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 289506016409139
        , processes = 8
        , globs =
            []
        , paths =
            [ "/workspaces/elm-in-action/tests/PhotoGrooveTests.elm"
            ]
        }
        [ ( "PhotoGrooveTests"
          , [ Test.Runner.Node.check PhotoGrooveTests.decoderTest
            , Test.Runner.Node.check PhotoGrooveTests.slidersTest
            , Test.Runner.Node.check PhotoGrooveTests.slidHueSetsHue
            , Test.Runner.Node.check PhotoGrooveTests.noPhotosNoThumbnails
            , Test.Runner.Node.check PhotoGrooveTests.thumbnailsWork
            , Test.Runner.Node.check PhotoGrooveTests.urlFuzzer
            , Test.Runner.Node.check PhotoGrooveTests.clickThumbnail
            ]
          )
        ]