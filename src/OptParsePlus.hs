module OptParsePlus
  ( argS, argT, completePrintables, optT, parserPrefs, parseOpts, parseOpts'
  , parsecArgument, parseNE, parsecOption
  , parsecReader, parsecReadM, readT, sepByNE, textualArgument, textualOption
  , usageFailure, usageFailureCode

  , ToDoc( toDoc ), (⊞)
  , finalFullStop, listDQOr, listSlash, listDQSlash, listW, toDocT, toDocTs
  )
where

import Prelude  ( Int, fromIntegral )

-- base --------------------------------

import Control.Applicative  ( Alternative, many )
import Control.Monad        ( return )
import Data.Bifunctor       ( first )
import Data.Foldable        ( Foldable, foldr, toList )
import Data.Function        ( ($), (&), id )
import Data.Functor         ( fmap )
import Data.List            ( intersperse )
import Data.List.NonEmpty   ( NonEmpty( (:|) ), fromList )
import Data.Maybe           ( fromMaybe )
import Data.Typeable        ( Typeable )
import Data.Word            ( Word8 )
import System.Environment   ( getArgs, getProgName )
import System.Exit          ( ExitCode( ExitFailure, ExitSuccess )
                            , exitSuccess, exitWith )
import System.IO            ( IO, hPutStrLn, putStr, putStrLn, stderr )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable, Textual, toString )

-- extra -------------------------------

import Data.List.Extra  ( unsnoc )

-- lens --------------------------------

import Control.Lens.Tuple  ( _2 )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, liftIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪), (∤) )
import Data.MoreUnicode.Either       ( pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Functor      ( (⊳), (⊳⊳) )
import Data.MoreUnicode.Lens         ( (⊢) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Maybe        ( pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.String       ( 𝕊 )

-- optparse-applicative ----------------

import qualified  Options.Applicative.Extra
import qualified  Options.Applicative.Types

import Options.Applicative.BashCompletion
                              ( bashCompletionParser )
import Options.Applicative.Builder
                              ( ArgumentFields, HasCompleter, InfoMod, Mod
                              , OptionFields, ReadM
                              , argument, columns, completeWith, eitherReader
                              , failureCode, fullDesc, info, option, prefs
                              )
import Options.Applicative.Common
                              ( runParserInfo )
import Options.Applicative.Extra
                              ( ParserFailure, ParserPrefs, renderFailure )
import Options.Applicative.Help.Pretty
                              ( Doc, (<+>), comma, dquotes, empty, fillSep
                              , punctuate, space, text, vcat )
import Options.Applicative.Internal
                              ( runP )
import Options.Applicative.Types
                              ( Context, Parser, ParserFailure( ParserFailure )
                              , ParserInfo, ParserHelp
                              , ParserResult( CompletionInvoked, Failure
                                            , Success )
                              , execCompletion, infoParser
                              )

-- parsec ------------------------------

import Text.Parsec  ( Parsec, SourceName, parse )

-- parsec-plus -------------------------

import ParsecPlus  ( ParseError, Parsecable, parsec )

-- parsers -----------------------------

import Text.Parser.Combinators  ( eof, sepBy1 )

-- terminal-size -----------------------

import qualified  System.Console.Terminal.Size  as  TerminalSize

-- textual-plus ------------------------

import TextualPlus  ( parseTextual )

-- text --------------------------------

import Data.Text  ( Text, intercalate, pack, unpack, words )

--------------------------------------------------------------------------------

readT ∷ (Textual α, Typeable α) ⇒ ReadM α
readT = eitherReader parseTextual

----------------------------------------

argT ∷ (Textual α, Typeable α) ⇒ Mod ArgumentFields α → Parser α
argT = argument readT

{-# DEPRECATED argS "use `argT` instead" #-}
argS ∷ (Textual α, Typeable α) ⇒ Mod ArgumentFields α → Parser α
argS = argT

----------------------------------------

optT ∷ (Textual α, Typeable α) ⇒ Mod OptionFields α → Parser α
optT = option readT

----------------------------------------

-- | an option that produces a textual value

textualOption ∷ (Textual α, Typeable α) ⇒ Mod OptionFields α → Parser α
textualOption = option readT

----------------------------------------

-- | an argement that produces a textual value

textualArgument ∷ (Textual α, Typeable α) ⇒ Mod ArgumentFields α → Parser α
textualArgument = argument readT

----------------------------------------

-- | arg/option completer using Printables

completePrintables ∷ (Foldable φ, Printable α, HasCompleter χ) ⇒ φ α → Mod χ γ
completePrintables = completeWith ∘ fmap toString ∘ toList

----------------------------------------

{- | Standard parser preferences.  Input is terminal width. -}
parserPrefs ∷ ℕ → ParserPrefs
parserPrefs width = let -- width = (fromIntegral $ fromMaybe 80 w)
                     in prefs $ {- showHelpOnError ⊕ -} {- showHelpOnEmpty ⊕ -} columns (fromIntegral width)

----------------------------------------

{- | Common exit code for usage errors, including --help (so that scripts that
     call --help get errors)
 -}
usageFailureCode ∷ Word8
usageFailureCode = 2

{- | Common exit code for usage errors, digestable by `Options.Applicative` -}
usageFailure ∷ InfoMod α
usageFailure = failureCode (fromIntegral usageFailureCode)

----------------------------------------

-- | Handle `ParserResult`.
handleParseResult :: ParserResult a -> IO a
handleParseResult (Success a) = return a
handleParseResult (Failure failure) = do
      progn <- getProgName
      let (msg, exit) = renderFailure failure progn
      case exit of
        ExitSuccess -> putStrLn msg
        _           -> hPutStrLn stderr msg
      exitWith exit
handleParseResult (CompletionInvoked compl) = do
      progn <- getProgName
      msg <- execCompletion compl progn
      putStr msg
      exitSuccess

--------------------

{-| A variant on `Options.Applicative.Extra.parserFailure`, that returns exit 2
    in case of --help. -}
parserFailure' ∷ ParserPrefs → ParserInfo a
               → Options.Applicative.Types.ParseError → [Context]
               → ParserFailure ParserHelp
parserFailure' pprefs pinfo msg ctx =
  let
    pf@(ParserFailure f) =
      Options.Applicative.Extra.parserFailure pprefs pinfo msg ctx
  in
    case msg of
      Options.Applicative.Types.ShowHelpText {} →
        ParserFailure $ (& _2 ⊢ ExitFailure (fromIntegral usageFailureCode)) ∘ f
      _ → pf


{-| A variant on `Options.Applicative.Extra.execParserPure`, that calls
    our `parserFailure'`. -}
execParserPure' ∷ ParserPrefs       -- ^ Global preferences for this parser
                → ParserInfo a      -- ^ Description of the program to run
                → [𝕊]               -- ^ Program arguments
                → ParserResult a
execParserPure' pprefs pinfo args =
  case runP p pprefs of
    (𝕽 (𝕽 r), _) → Success r
    (𝕽 (𝕷 c), _) → CompletionInvoked c
    (𝕷 err, ctx) → Failure $ parserFailure' pprefs pinfo err ctx
  where
    pinfo' = pinfo
      { infoParser = (𝕷 ⊳ bashCompletionParser pinfo pprefs)
                   ∤ (𝕽 ⊳ infoParser pinfo) }
    p = runParserInfo pinfo' args

----------------------------------------

{-| A variant on `Options.Applicative.Extra.customExecParser`, that calls
    our `execParserPure'`. -}
customExecParser' ∷ ParserPrefs → ParserInfo a → IO a
customExecParser' pprefs pinfo =
  execParserPure' pprefs pinfo ⊳ getArgs ≫ handleParseResult

----------------------------------------

{- | Parse options, with description, helper, shows help on error and missing
     parameters.  Also exits 2 if --help is called - this is because the exit
     code is most commonly used within scripts, where calling --help is almost
     certainly not what was intended.
-}
parseOpts ∷ MonadIO μ ⇒ -- | base infomod for parser; typically `progDesc
                        --   "some description"`
                        InfoMod α
                      → Parser α   -- ^ proggie opts parser
                      → μ α
parseOpts baseinfo prsr = liftIO $ do
  width ← fromMaybe 80 ⊳ (TerminalSize.width @Int ⊳⊳ TerminalSize.size)
  let pprefs = parserPrefs (fromIntegral width)
  customExecParser' pprefs (info prsr (fullDesc ⊕ baseinfo ⊕ usageFailure))

{- | DEPRECATED parseOpts' "use parseOpts" -}
parseOpts' ∷ MonadIO μ ⇒ -- | base infomod for parser; typically `progDesc
                         --   "some description"`
                         InfoMod α
                       → Parser α   -- ^ proggie opts parser
                       → μ α
parseOpts' = parseOpts

----------------------------------------

parsecReader ∷ Parsecable α ⇒ ReadM α
parsecReader = eitherReader (\ s → first show $ parsec @_ @ParseError s s)

----------------------------------------

parsecOption ∷ Parsecable α ⇒ Mod OptionFields α → Parser α
parsecOption = option parsecReader

----------------------------------------

parsecArgument ∷ Parsecable α ⇒ Mod ArgumentFields α → Parser α
parsecArgument = argument parsecReader

----------------------------------------

infixr 6 ⊞
(⊞) ∷ Doc → Doc → Doc
(⊞) = (<+>)

{- | Simple auto-conversions to Doc. -}
class ToDoc α where
  toDoc ∷ α → Doc

instance ToDoc Doc where
  toDoc = id

instance ToDoc Text where
  {- | The text is broken (on spaces) into words; and then re-joined with
       breaking spaces (⊞) between them, to form a flowable paragraph. -}
  toDoc ts = fillSep $ fmap (text ∘ unpack) (words ts)

instance ToDoc [Text] where
  {- | The text is broken (on spaces) into words; and then re-joined with
       breaking spaces (⊞) between them, to form a flowable paragraph. -}
  toDoc ts = fillSep $ fmap (text ∘ unpack) (ts ≫ words)

instance ToDoc [[Text]] where
  {- | Each text list is assembled into a flowing paragraph; each para is
       separated by a blank line. -}
  toDoc tss = vcat $ intersperse space (toDoc ⊳ tss)

instance ToDoc [Doc] where
  {- | Each doc is separated by a blank line. -}
  toDoc ds = vcat $ intersperse space ds

toDocT ∷ Text → Doc
toDocT = toDoc

toDocTs ∷ [Text] → Doc
toDocTs = toDoc

{- | Create a list by joining words (which are surrounded with double-quotes)
     with ", ", except for the last, which is joined with "or". -}
listDQOr ∷ [𝕊] → Doc
listDQOr (unsnoc → 𝕹)     = empty
listDQOr (unsnoc → 𝕵 (ws,w)) =
  fillSep (punctuate comma (dquotes ∘ text ⊳ ws)) ⊞ text "or" ⊞ dquotes (text w)

{- | Create a list by joining words (showable things) with ", ". -}
listW ∷ Show α ⇒ [α] → Doc
listW xs = toDoc $ intercalate ", " (pack ∘ show ⊳ xs)

{- | Create a list by joining strings with "/". -}
listSlash ∷ [𝕊] → Doc
listSlash xs = toDoc $ intercalate "/" (pack ⊳ xs)

{- | Create a list by joining double-quoted strings with "/". -}
listDQSlash ∷ [𝕊] → Doc
listDQSlash []     = empty
listDQSlash (x:xs) =
  foldr (\ a b → a ⊕ text "/" ⊕ b) (dquotes $ text x) (dquotes ∘ text ⊳ xs)

{- | Add a full stop (period) to the final doc in a list. -}
finalFullStop ∷ [Doc] → [Doc]
finalFullStop (unsnoc → 𝕵 (ds,d)) = ds ⊕ [d ⊕ text "."]
finalFullStop (unsnoc → 𝕹)     = []

----------------------------------------

{- | Parse a NonEmpty list of things; like `some`, but more strongly typed. -}
parseNE ∷ Parser α → Parser (NonEmpty α)
parseNE p = (:|) ⊳ p ⊵ many p

{- | Parse a NonEmpty list of things with a separator; like `sepBy1`, but more
     strongly typed. -}
sepByNE ∷ Alternative γ ⇒ γ α → γ σ → γ (NonEmpty α)
sepByNE x s = fromList ⊳ sepBy1 x s

{- | Create a `ReadM` from a parsec parser. -}
parsecReadM ∷ SourceName → Parsec 𝕊 () α → ReadM α
parsecReadM nm p = eitherReader (\ s → first show $ parse (p ⋪ eof) nm s)

-- that's all, folks! ----------------------------------------------------------
