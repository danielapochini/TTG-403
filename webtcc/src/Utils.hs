{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Utils where
import Routes
import Yesod
import Yesod.Static
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Text.Julius
import Text.Lucius  
import Text.Hamlet
import Text.Cassius 
import Network.Mail.Mime 
import Network.Mail.SMTP 
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.Text.Lazy.Encoding 
 

-----------------------  WIDGETS   ------------------ 

cliWid :: Widget
cliWid = [whamlet| 
    _{MsgAdmin1} 
|]

contWid :: Widget
contWid = [whamlet| 
    _{MsgContato4} 
|]

funcWid :: Widget
funcWid = [whamlet| 
    _{MsgAdmin2} 
|]

entWid :: Widget
entWid = [whamlet| 
    _{MsgEntrega} 
|]

filWid :: Widget
filWid = [whamlet| 
    _{MsgFilial} 
|]

logWid :: Widget
logWid = [whamlet| 
    _{MsgLogin1}
|]
 
perfWidget = do   
            toWidget $(juliusFile "templates/julius/perfil.julius") 
cadWidget = do    
            toWidget $(cassiusFile "templates/cassius/form.cassius") 
            toWidget $(juliusFile "templates/julius/list.julius")

listWidget = do
            toWidget $(cassiusFile "templates/cassius/list.cassius") 
            toWidget $(juliusFile "templates/julius/list.julius")
        
geoWidget = do
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addStylesheetRemote "https://api.mapbox.com/mapbox.js/v2.4.0/mapbox.css"
            addStylesheetRemote "https://api.tiles.mapbox.com/mapbox.js/plugins/leaflet-label/v0.2.1/leaflet.label.css"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            addScriptRemote "https://cdn.firebase.com/js/client/2.2.1/firebase.js"
            addScriptRemote "https://api.tiles.mapbox.com/mapbox.js/v2.1.6/mapbox.js"
            addScriptRemote "https://api.tiles.mapbox.com/mapbox.js/plugins/leaflet-label/v0.2.1/leaflet.label.js"

masterWidget = do
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css  
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
            toWidget $(luciusFile "templates/lucius/principal.lucius") 

customWidget :: Widget -> Widget
customWidget hamletWidget = do
            masterWidget
            $(whamletFile "templates/widgets/header.hamlet") 
            hamletWidget
            $(whamletFile "templates/widgets/footer.hamlet")  

padmWidget :: Widget -> Widget
padmWidget hamletWidget = do
            masterWidget 
            hamletWidget
            $(whamletFile "templates/widgets/footer.hamlet")      
      
      
widgetForm :: Route SauipeExpress -> Enctype -> Widget -> Widget -> Widget
-- função que gera formulários em forma genérica 
widgetForm x enctype widget novaWidget = [whamlet|
            <h1>
                ^{novaWidget}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input ."btn btn-primary" type="reset" value="_{MsgContato12}" #"limpar">
                <input ."btn btn-primary" type="submit" value="_{MsgCadastroBtn}" #"cadastrar">
            <p>_{MsgCadastro}
|]

data Email = Email {nomeEmail, remetenteEmail, assuntoEmail:: Text, msgEmail :: Textarea} 

estruturaEmail (Email n r a m) = simpleMail' to' from' titulo mensagem
       where titulo = T.concat [T.pack "Novo Contato de: ", n , " - SauipeExpress"]
             to' = Address (Just "sauipexpress@gmail.com") "sauipexpress@gmail.com" 
             from' = Address (Just n) r
             mensagem = decodeUtf8 $ renderHtml [shamlet|
                            Formulário de Contato - Sauipe Express
                            Nome: #{n} 
                            Email: #{r}
                            Assunto: #{a} 
                            Mensagem: #{m}
                        |]
             

enviarEmail = sendMailWithLogin' host port login password