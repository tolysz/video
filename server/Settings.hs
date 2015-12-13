-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Control.Exception           (throw)
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util         --  (WidgetFileSettings, widgetFileNoReload,
                                   --  widgetFileReload)
import qualified Facebook as FB
import  Database.Neo.Rest (Neo4jConf)

import Text.Lucius (luciusFile, luciusFileReload)

import Text.Coffee

import Types

-- Old minifier
import qualified Data.Text.Lazy as TL
import System.Process              (readProcess)
import Language.Haskell.TH.Syntax (qRunIO)
-- import Text.Lucius (luciusRTMinified)
-- import qualified Data.Text.Lazy.Encoding as TLE
-- errorIntro fps s = "Error minifying " ++ show fps ++ ": " ++ s

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Text
    -- ^ Base for all generated URLs.
    , appStaticRoot             :: Maybe Text
        -- ^ Base for all generated URLs.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code
    , appGoogleServerKey        :: Maybe Text
    -- ^ Google Api Server Key
    , appGoogleBrowserKey       :: Maybe Text
    -- ^ Google Api Browser Key
    , appGoogleWebAppOAuth      :: Maybe OAuth2Google
    -- ^ OAuth2 config
    , appSiteVerification       :: Maybe Text
    -- ^ google site verification for push messages
    , appFbCredentials          :: FB.Credentials
    -- ^ Facebook creds
    , appDevelopment            :: Bool

    , appNeo4jConf              :: Neo4jConf
    -- ^ creds for neo

    }

compiledAsDevel :: Bool
compiledAsDevel =
#if DEVELOPMENT
                True
#else
                False
#endif

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev = compiledAsDevel

        appStaticDir              <- o .:  "static-dir"
        appDatabaseConf           <- o .:  "database"
        appRoot                   <- o .:  "approot"
        appStaticRoot             <- o .:  "app-static-root"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .:  "port"
        appIpFromHeader           <- o .:  "ip-from-header"

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appCopyright              <- o .:  "copyright"
        appAnalytics              <- o .:? "analytics"
        appGoogleServerKey        <- o .:? "google-api-server"
        appGoogleBrowserKey       <- o .:? "google-api-browser"
        appGoogleWebAppOAuth      <- o .:? "google-oauth"
        appSiteVerification       <- o .:? "google-site-verification"

        appDevelopment            <- pure defaultDev

        fbId                      <- o .:? "facebook-app-id"     .!= ""
        fbApp                     <- o .:? "facebook-app-name"   .!= ""
        fbSecret                  <- o .:? "facebook-app-secret" .!= ""

        appNeo4jConf              <- o .: "neo4j"

        let appFbCredentials = FB.Credentials fbApp fbId fbSecret
        return AppSettings {..}


-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
-- widgetFileSettings = def
widgetFileSettings = def { wfsLanguages = \hset -> defaultTemplateLanguages hset ++
    [ TemplateLanguage True  "coffee"  Text.Coffee.coffeeFile   Text.Coffee.coffeeFileReload
--     , TemplateLanguage True  "lucius"  (\fp -> do
--          x <- luciusFile fp
--          xx <- $(x)
--          qRunIO (processCSS xx ))   luciusFileReload
    ] }
-- | How static files should be combined.

processCSS :: String -> IO String
processCSS = readProcess "postcss" ["--use", "autoprefixer"]

combineSettings :: CombineSettings
combineSettings = def {
--   csCssPreProcess :: TL.Text -> IO TL.Text
        csCssPreProcess =
              fmap TL.pack
                  . processCSS
                  . TL.unpack
                  . TL.replace "'/static/" "'../"
                  . TL.replace "\"/static/" "\"../"

-- csCssPostProcess = \fps ->
--         either (error . (errorIntro fps)) (return . TLE.encodeUtf8)
--        . flip luciusRTMinified []
--       . TLE.decodeUtf8
      }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
