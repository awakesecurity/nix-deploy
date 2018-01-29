{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS -fno-warn-orphans        #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Main where

import           Control.Applicative    (empty, (<|>))
import           Control.Exception      (SomeException)
import qualified Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty     as NonEmpty
import           Data.Maybe
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text.IO
import           NeatInterpolation
import qualified Options.Applicative    as Options
import           Options.Generic
import           Prelude                hiding (FilePath)
import qualified System.IO
import           Turtle                 (ExitCode (..), FilePath, fp, liftIO, s,
                                         (%), (</>))
import qualified Turtle
import           Turtle.Line

data Options w
    = Path
      { direction      :: Direction
      , sudo           :: w ::: Bool               <?> "Prepend with sudo"
      , noSign         :: w ::: Bool               <?> "Don't sign payload (not recommended)"
      , path           :: w ::: Maybe FilePath     <?> "Path to deploy"
      , profilePath    :: w ::: Maybe FilePath     <?> "Path to parent profile directory (default: /nix/var/nix/profiles)"
      , profileName    :: w ::: Maybe Line         <?> "Name of profile to set (example: upgrade-tools)"

      }
    | System
      { direction      :: Direction
      , noSign         :: w ::: Bool               <?> "Don't sign payload (not recommended)"
      , path           :: w ::: Maybe FilePath     <?> "Path to deploy"
      , systemName     :: w ::: Maybe Line         <?> "Alternative system profile name (default: system)"
      , switchMethod   :: Maybe SwitchMethod
      }
  deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

data SwitchMethod
    = Switch
    | Boot
    | Test
    | DryActivate
    | Reboot
    -- ^ Same as `Boot` except followed by a @reboot@
    deriving (Eq, Show, ParseFields)

instance ParseField SwitchMethod where
  readField = Options.readerError "Internal, fatal error: unexpected use of readField"
  parseField _ _ _ =
        Options.flag' Switch      (Options.long "switch")
    <|> Options.flag' Boot        (Options.long "boot")
    <|> Options.flag' Test        (Options.long "test")
    <|> Options.flag' DryActivate (Options.long "dry-activate")
    <|> Options.flag' Reboot      (Options.long "reboot")

instance ParseRecord SwitchMethod where
  parseRecord = fmap Options.Generic.getOnly parseRecord

renderSwitch :: SwitchMethod -> Text
renderSwitch Switch      = "switch"
renderSwitch Boot        = "boot"
renderSwitch Test        = "test"
renderSwitch DryActivate = "dry-activate"
renderSwitch Reboot      = "boot"

data Direction = To Line | From Line
  deriving (Show, ParseFields)

instance ParseField Direction where
  readField = Options.readerError "Internal, fatal error: unexpected use of readField"
  parseField _ _ _ = (To <$> parseTo) <|> (From <$> parseFrom)
    where
      parseTo    = parser "to" "Deploy software to this address (ex: user@192.168.0.1)"
      parseFrom  = parser "from" "Deploy software from this address (ex: user@192.168.0.1)"
      line       = Options.maybeReader (textToLine . Text.pack)
      parser l h =
        (Options.option line $
         (  Options.metavar "USER@HOST"
         <> Options.long    l
         <> Options.help    h
         )
        )

instance ParseRecord Direction where
  parseRecord = fmap Options.Generic.getOnly parseRecord

instance ParseRecord Line where
  parseRecord = fmap Options.Generic.getOnly parseRecord

instance ParseFields Line where

instance ParseField Line where
  readField = Options.maybeReader (textToLine . Text.pack)
  parseField h m c = do
    let metavar = "LINE"
    let line    = Options.maybeReader (textToLine . Text.pack)
    case m of
      Nothing ->
        (Options.argument line
         (  Options.metavar metavar
         <> foldMap (Options.help . Text.unpack) h))
      Just name ->
        (Options.option line
         (  Options.metavar metavar
         <> foldMap Options.short c
         <> Options.long (Text.unpack name)
         <> foldMap (Options.help . Text.unpack) h))

renderDirection :: Direction -> (Text, Line)
renderDirection (To target)   = ("to",   target)
renderDirection (From target) = ("from", target)

progSummary :: Text
progSummary = "Deploy software or an entire NixOS system configuration to another NixOS system"

main :: IO ()
main =
  unwrapRecord progSummary >>= \case
    Path{..}   -> do
      pathText <-
        case path of
          Just p  -> pure (Turtle.format fp p)
          Nothing -> liftIO pathFromStdin

      let (txDir, target) = renderDirection direction
      let targetText = lineToText target

      let sign = not noSign
      when sign $ exchangeKeys targetText

      stderrLines [text|[+] Copying $pathText|]
      let transfer =
            Turtle.procs "nix-copy-closure"
              ((if sign then ["--sign"] else []) <>
              [ (Turtle.format ("--"%s) txDir)
              , "--gzip"
              , targetText
              , pathText
              ])
              empty

      -- Transfer path to target
      liftIO (Control.Exception.catch transfer (errorHandler [text|
[x] Failed transferring $pathText $txDir $targetText

    1. $pathText does not exist
    2. Make sure you have an authorized key configured on $targetText
|]))

      let sudoAnyway =
            case profilePath of
              Nothing -> True -- The default profile path requires `sudo`
              _       -> sudo
          updateProfile line = do
            let profile = fromMaybe "/nix/var/nix/profiles" profilePath
                       </> Turtle.fromText (lineToText line)
            let profileText = Turtle.format fp profile
            setProfile direction sudoAnyway profileText pathText
      mapM_ updateProfile profileName

    System{..} -> do

      pathText <-
        case path of
          Just p  -> pure (Turtle.format fp p)
          Nothing -> liftIO pathFromStdin

      let profileText =
            case systemName of
              Nothing -> "/nix/var/nix/profiles/system"
              Just p  ->
                let profiles = Turtle.fromText "/nix/var/nix/profiles/system-profiles"
                    name     = Turtle.fromText (lineToText p)
                in Turtle.format fp (profiles </> name)

      let (txDir, target) = renderDirection direction
      let targetText = lineToText target

      let sign = not noSign
      when sign $ exchangeKeys targetText

      stderrLines [text|[+] Installing system: $pathText|]

      let transfer =
            Turtle.procs "nix-copy-closure"
              ((if sign then ["--sign"] else []) <>
              [ (Turtle.format ("--"%s) txDir)
              , "--gzip"
              , targetText
              , pathText
              ])
              empty

      let method = fromMaybe Test switchMethod
      let switchSystem =
            Turtle.procs "ssh"
              [ targetText
              , "sudo"
              , "/nix/var/nix/profiles/system/bin/switch-to-configuration"
              , (renderSwitch method)
              ]
              empty

      -- Transfer path to target
      liftIO (Control.Exception.catch transfer (errorHandler [text|
[x] Failed transferring $pathText $txDir $targetText

    1. $pathText may not exist, make sure you built it with nix-build first
    2. Make sure you have an authorized key configured on $targetText so that you can SSH
|]))

      setProfile direction True profileText pathText

      -- Switch to the new system configuration
      liftIO (Control.Exception.catch switchSystem (errorHandler [text|
[x] Failed to switch $targetText to $pathText

    You need `sudo` privileges on the target machine
|]))
      when (method == Reboot)$ do
        let success = 
              stderrLines [text|[+] $pathText successfully activated, $targetText is rebooting|]
        rebootCmd targetText >>= \case
          -- This is the exit code returned by `ssh` when the machine closes
          -- the connection due to a successful reboot.  We can't really
          -- distinguish the connection being closed for other reasons,
          -- unfortunately, so we have to assume that this meant success
          ExitFailure 255 -> success
          ExitFailure _   ->
            Turtle.die [text|[x] Failed to reboot $targetText after activating $pathText at $profileText|]
          -- The command should always fail because the remote machine closes
          -- connection when rebooting, but we include this case for
          -- completeness
          ExitSuccess -> success

-- | Given a 'Text' that may have newlines, split using
-- 'Turtle.Line.textToLines' and print each line to stderr.
stderrLines :: MonadIO io => Text -> io ()
stderrLines = mapM_ Turtle.err . textToLines

-- | Given an error preamble and 'SomeException', format the exception
-- message and render it with the preamble into a useful error.
errorHandler :: MonadIO io => Text -> SomeException -> io ()
errorHandler msg err = do
  let excText0 = Text.justifyRight 4 ' ' <$> Text.lines (Text.pack $ show err)
  let excText1 = Text.unlines excText0

  Turtle.die [text|
$msg

Original error was:

$excText1
|]


rebootCmd :: MonadIO io => Text -> io Turtle.ExitCode
rebootCmd target = Turtle.shell [text|ssh $target sudo reboot|] empty

pathFromStdin :: IO Text
pathFromStdin = do
  let h = System.IO.stdin
  System.IO.hWaitForInput h (-1)

  ls <- Turtle.textToLines <$> Text.IO.hGetContents h

  pure (Turtle.lineToText $ NonEmpty.head ls)


exchangeKeys :: Text -> IO ()
exchangeKeys host = do
  -- When performing a distributed build you need to share a key pair
  -- (both the public and private key) with the machine you're
  -- deploying to (or from). Both machines must store the same private
  -- key at `/etc/nix/signing-key.sec` and the same public key at
  -- `/etc/nix/signing-key.pub`. The private must also be only
  -- user-readable and not group- or world-readable (i.e. `400`
  -- permissions using `chmod` notation).
  --
  -- By default, neither machine will have a key pair installed.  This script
  -- will first ensure that the remote machine has a key pair (creating one if
  -- if missing) and copy the remote key pair to the local machine.  We
  -- install the remote key pair locally on every run of this script because we
  -- do not assume that all remote machines share the same key pair.  Quite the
  -- opposite: every production machine should have a unique signing key pair.
  let privateKey = "/etc/nix/signing-key.sec"
  let publicKey  = "/etc/nix/signing-key.pub"

  let handler0 :: SomeException -> IO ()
      handler0 e = do
          let exceptionText = Text.pack (show e)
          let msg           = [text|
[x] Could not ensure that the remote machine has signing keys installed

    Debugging tips:

    1. Check if you can log into the remote machine by running:

        $ ssh $host

    2. If you can log in, then check if you have permission to `sudo` without a
       password by running the following command on the remote machine:

        $ sudo -n true
        $ echo $?
        0

    Original error: $exceptionText
|]
          Turtle.die msg

  let openssl :: Turtle.Format a a
      openssl =
          "$(nix-build --no-out-link \"<nixpkgs>\" -A libressl)/bin/openssl"
  let fmt = "ssh "%s%" '"
              % "test -e "%fp%" || "
              % "sudo sh -c \""
                  % "(umask 277 && "%openssl%" genrsa -out "%fp%" 2048) && "
                  % openssl%" rsa -in "%fp%" -pubout > "%fp
              % "\""
          % "'"
  let cmd = Turtle.format fmt host privateKey privateKey privateKey publicKey
  Control.Exception.handle handler0 (Turtle.shells cmd empty)

  let mirror path = Turtle.runManaged $ do
          -- NB: path shouldn't is a FilePath and won't have any
          -- newlines, so this should be okay
          stderrLines (Turtle.format ("[+] Downloading: "%fp) path)

          localPath <- Turtle.mktempfile "/tmp" "signing-key"
          let download =
                  Turtle.procs "rsync"
                      [ "--archive"
                      , "--checksum"
                      , "--rsh", "ssh"
                      , "--rsync-path", "sudo rsync"
                      , Turtle.format (s%":"%fp) host path
                      , Turtle.format fp localPath
                      ]
                      empty
          let handler1 :: SomeException -> IO ()
              handler1 e = do
                  let pathText      = Turtle.format fp path
                  let exceptionText = Text.pack (show e)
                  let msg           = [text|
[x] Could not download: $pathText

    Debugging tips:

    1. Check if you can log into the remote machine by running:

        $ ssh $host

    2. If you can log in, then check if you have permission to `sudo` without a
       password by running the following command on the remote machine:

        $ sudo -n true
        $ echo $?
        0

    3. If you can `sudo` without a password, then check if the file exists by
       running the following command on the remote machine:

        $ test -e $pathText
        $ echo $?
        0

    Original error: $exceptionText
|]
                  Turtle.die msg

          liftIO (Control.Exception.handle handler1 download)

          exitCode <- Turtle.shell "sudo -n true 2>/dev/null" empty

          -- NB: path shouldn't is a FilePath and won't have any
          -- newlines, so this should be okay
          Turtle.err (Turtle.unsafeTextToLine $ Turtle.format ("[+] Installing: "%fp) path)

          case exitCode of
              ExitFailure _ -> do
                  Turtle.err ""
                  Turtle.err "    This will prompt you for your `sudo` password"
              _             -> do
                  return ()

          let install =
                  Turtle.procs "sudo"
                      [ "mv"
                      , Turtle.format fp localPath
                      , Turtle.format fp path
                      ]
                      empty
          let handler2 :: SomeException -> IO ()
              handler2 e = do
                  let pathText      = Turtle.format fp path
                  let exceptionText = Text.pack (show e)
                  let msg           = [text|
[x] Could not install: $pathText

    Debugging tips:

    1. Check to see that you have permission to `sudo` by running:

        $ sudo true
        $ echo $?
        0

    Original error: $exceptionText
|]
                  Turtle.die msg

          liftIO (Control.Exception.handle handler2 install)

  mirror privateKey
  mirror publicKey


setProfile
    :: MonadIO io
    => Direction
    -> Bool
    -- ^ `True` to use `sudo`
    -> Text
    -- ^ Profile name (such as @"system"@, or @"default"@)
    -> Text
    -- ^ File path to install (rendered as `Text`)
    -> io ()
setProfile direction sudo profileText pathText = do
  command <- case direction of
    To target -> do
      let command =
            Turtle.procs "ssh"
              (   [ lineToText target
                  ]
              ++  (if sudo then [ "sudo" ] else [])
              ++  [ "nix-env"
                  , "--profile"
                  , profileText
                  , "--set"
                  , pathText
                  ]
              )
              empty
      return command
    From _ -> do
      let command =
            if sudo
            then
              Turtle.procs "sudo"
                [ "nix-env"
                , "--profile"
                , profileText
                , "--set"
                , pathText
                ]
                empty
            else
              Turtle.procs "nix-env"
                [ "--profile"
                , profileText
                , "--set"
                , pathText
                ]
                empty

      return command

  -- Set or create a profile pointing at the transferred path
  let msg = [text|[x] Failed setting $profileText to $pathText|]
  liftIO (Control.Exception.handle (errorHandler msg) command)
