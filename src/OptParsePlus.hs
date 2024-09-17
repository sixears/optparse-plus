{-# LANGUAGE UnicodeSyntax #-}
module OptParsePlus
  ( OptReader(..)
  , ToDoc(toDoc)
  , argS
  , argT
  , completePrintables
  , finalFullStop
  , handleParserResult
  , listDQOr
  , listDQSlash
  , listSlash
  , listW
  , optT
  , parseNE
  , parseOpts
  , parseOptsPure
  , parseOpts_
  , parsecArgument
  , parsecOption
  , parsecReadM
  , parsecReader
  , parserPrefs
  , readMCommaSet
  , readNT
  , readT
  , textualArgument
  , textualOption
  , toDocT
  , toDocTs
  , twidth
  , usageFailure
  , usageFailureCode
  , (⊞)
  ) where

import Base1

import Prelude ( Int, error )

-- base --------------------------------

import System.Environment qualified

import Data.Foldable      ( Foldable )
import Data.List          ( intersperse )
import Data.Maybe         ( fromMaybe )
import Data.Typeable      ( typeOf )
import System.Environment ( getProgName )
import System.Exit        ( exitSuccess, exitWith )
import System.IO          ( hPutStrLn, putStr, putStrLn, stderr )
import Text.Read          ( read )

-- data-textual ------------------------

import Data.Textual ( Parsed(Malformed, Parsed), Textual, parseText )

-- extra -------------------------------

import Data.List.Extra ( unsnoc )

-- lens --------------------------------

import Control.Lens.Tuple ( _2 )

-- nonempty-containers -----------------

import Data.Set.NonEmpty ( NESet )

-- optparse-applicative ----------------

import Options.Applicative.Extra qualified
import Options.Applicative.Types qualified

import Options.Applicative.BashCompletion ( bashCompletionParser )
import Options.Applicative.Builder        ( ArgumentFields, HasCompleter,
                                            InfoMod, Mod, OptionFields, ReadM,
                                            argument, columns, completeWith,
                                            eitherReader, failureCode, fullDesc,
                                            info, option, prefs )
import Options.Applicative.Common         ( runParserInfo )
import Options.Applicative.Extra          ( ParserFailure, ParserPrefs,
                                            renderFailure )
import Options.Applicative.Help.Pretty    ( Doc, comma, dquotes, emptyDoc,
                                            fillSep, pretty, punctuate, space,
                                            vcat, (<+>) )
import Options.Applicative.Internal       ( runP )
import Options.Applicative.Types          ( Context, Parser,
                                            ParserFailure(ParserFailure),
                                            ParserHelp, ParserInfo,
                                            ParserResult(CompletionInvoked, Failure, Success),
                                            execCompletion, infoParser )

-- parsec ------------------------------

import Text.Parsec ( Parsec, SourceName, parse )

-- parsec-plus -------------------------

import ParsecPlus ( ParseError, Parsecable, parsec )

-- parsers -----------------------------

import Text.Parser.Char        ( anyChar, char, digit )
import Text.Parser.Combinators ( eof )

-- parser-plus -------------------------

import ParserPlus ( commaSet )

-- terminal-size -----------------------

import System.Console.Terminal.Size qualified as TerminalSize

-- text --------------------------------

import Data.Text ( intercalate, pack, unpack, words )

--------------------------------------------------------------------------------

{-| class for things that have a `ReadM` instance -}
class OptReader α where
  {-| `ReadM` instance for `α` -}
  readM ∷ ReadM α

------------------------------------------------------------

{-| convert a text word to Doc.  This used to be in optparse-applicative
    0.17.1.0, but was removed in 0.18.0.0 when that lib switched to using the
    prettyprinter library -}
text ∷ 𝕊 → Doc
text = pretty

{- | Parse a printable value, give user-friendly error messages.
     This is mostly an adapter from `Printable` to `Either`; to work with, e.g.,
     `Options.Applicative.eitherReader`.
 -}
parseTextual ∷ ∀ β α η . (Textual β, Printable α, Typeable β, MonadError 𝕊 η) ⇒ α → η β
parseTextual (toText → z) =
  let fromParsed (Parsed a)      = a
      -- this function exists solely to provide a hypothetical value to reflect
      -- on
      fromParsed (Malformed _ _) = error "this should never be evaluated"
      parsedZ                    = parseText z
      typ                        = typeOf $ fromParsed parsedZ
   in case parsedZ of
        Parsed a       → return a
        Malformed [] x → throwError ∘ toString $
                           [fmtT|failed to parse '%t' as '%w': %s|] z typ x
        Malformed xs x → let msg = [fmtT|failed to parse '%t' as '%w': [%L] %s|]
                                   z typ xs x
                          in throwError (toString msg)

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
parserPrefs width = prefs $ columns (fromIntegral width)

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

{- | in case of a failed opts parse: write a message to stderr & exit; in case
     of `CompletionInvoked`, exit normally. -}
handleParserResult ∷ MonadIO μ ⇒ ParserResult a → μ a
handleParserResult (Success a) = return a
handleParserResult (Failure failure) = liftIO $ do
  progn ← getProgName
  let (msg, exit) = renderFailure failure progn
  case exit of
    ExitSuccess → putStrLn msg
    _           → hPutStrLn stderr msg
  exitWith exit
handleParserResult (CompletionInvoked compl) = liftIO $ do
  progn ← getProgName
  msg ← execCompletion compl progn
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

{- | Terminal width of stdout, in characters; default to 80 if terminal has no
     width (e.g., is a file redirect. -}
twidth ∷ MonadIO μ ⇒ μ ℕ
twidth = let w = (TerminalSize.width @Int ⊳⊳ TerminalSize.size)
          in liftIO $ fromIntegral ∘ fromMaybe 80 ⊳ w

{- | Parse options, with description, helper, shows help on error and missing
     parameters.  Also exits 2 if --help is called - this is because the exit
     code is most commonly used within scripts, where calling --help is almost
     certainly not what was intended.
-}
parseOpts_ ∷ MonadIO μ ⇒ [𝕊]       -- ^ cli arguments
                       → InfoMod α -- ^ base infomod for parser; typically
                                   --   `progDesc "some description"`
                       → Parser α  -- ^ proggie opts parser
                       → μ α
parseOpts_ args baseinfo prsr =
  handleParserResult ∘ parseOptsPure args baseinfo prsr ≪ twidth

{- | parse an argument list, adding in our standard settings -}
parseOptsPure ∷ [𝕊] → InfoMod α → Parser α → ℕ → ParserResult α
parseOptsPure args baseinfo prsr width =
  let pprefs   = parserPrefs width
      mods     = fullDesc ⊕ baseinfo ⊕ usageFailure
   in execParserPure' pprefs (info prsr mods) args

----------

{- | parse the arguments given on the command line -}
parseOpts ∷ MonadIO μ ⇒ -- | base infomod for parser; typically `progDesc
                        --   "some description"`
                        InfoMod α
                      → Parser α   -- ^ proggie opts parser
                      → μ α
parseOpts mod p =
  liftIO System.Environment.getArgs ≫ \ args → parseOpts_ args mod p

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

instance ToDoc 𝕋 where
  {- | The text is broken (on spaces) into words; and then re-joined with
       breaking spaces (⊞) between them, to form a flowable paragraph. -}
  toDoc ts = fillSep $ fmap (text ∘ unpack) (words ts)

instance ToDoc [𝕋] where
  {- | The text is broken (on spaces) into words; and then re-joined with
       breaking spaces (⊞) between them, to form a flowable paragraph. -}
  toDoc ts = fillSep $ fmap (text ∘ unpack) (ts ≫ words)

instance ToDoc [[𝕋]] where
  {- | Each text list is assembled into a flowing paragraph; each para is
       separated by a blank line. -}
  toDoc tss = vcat $ intersperse space (toDoc ⊳ tss)

instance ToDoc [Doc] where
  {- | Each doc is separated by a blank line. -}
  toDoc ds = vcat $ intersperse space ds

toDocT ∷ 𝕋 → Doc
toDocT = toDoc

toDocTs ∷ [𝕋] → Doc
toDocTs = toDoc

{- | Create a list by joining words (which are surrounded with double-quotes)
     with ", ", except for the last, which is joined with "or". -}
listDQOr ∷ [𝕊] → Doc
listDQOr (unsnoc → 𝕹)     = emptyDoc
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
listDQSlash []     = emptyDoc
listDQSlash (x:xs) =
  foldr (\ a b → a ⊕ text "/" ⊕ b) (dquotes $ text x) (dquotes ∘ text ⊳ xs)

{- | Add a full stop (period) to the final doc in a list. -}
finalFullStop ∷ [Doc] → [Doc]
finalFullStop (unsnoc → 𝕵 (ds,d)) = ds ⊕ [d ⊕ text "."]
finalFullStop (unsnoc → 𝕹)        = []

----------------------------------------

{- | Parse a NonEmpty list of things; like `some`, but more strongly typed. -}
parseNE ∷ Parser α → Parser (NonEmpty α)
parseNE p = (:|) ⊳ p ⊵ many p

----------------------------------------

{- | Create a `ReadM` from a parsec parser. -}
parsecReadM ∷ SourceName → Parsec 𝕊 () α → ReadM α
parsecReadM nm p = eitherReader (\ s → first show $ parse (p ⋪ eof) nm s)

----------------------------------------

{- | Parse arguments/options of the form "nn=title", where nn is a natural
     number and title is text. -}
readNT ∷ ReadM (ℕ,𝕋)
readNT =
  parsecReadM "" ((,) ⊳ (read ⊳ some digit) ⋪ char '=' ⊵ (pack ⊳ some anyChar))

----------------------------------------

readMCommaSet ∷ Ord α ⇒ SourceName → Parsec 𝕊 () α → ReadM (NESet α)
readMCommaSet nm p = eitherReader $ commaSet nm p

-- that's all, folks! ----------------------------------------------------------
