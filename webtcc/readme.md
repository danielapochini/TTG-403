Pasta SRC: Foundation.hs, Handlers.hs e Routes.hs

Foundation -> Instancias de Yesod, BD;
Handlers -> Codigos da Aplicação, Estrutura das páginas;
Routes -> Rotas.

Pasta Messages: en/pt-BR, textos para a internacionalização
a chamada é feita utilizando _{MsgNomedoTexto}, é necessário ter as duas versões,
se não, não compila.

Pasta STATIC: Arquivos estaticos, imagens, js, cs etc

PASTA TEMPLATES: cassius, hamlet, julius, whamlet
arquivos em Cassius/Lucius -> CSS
    toWidget $(luciusFile "templates/lucius/principal.lucius") 
    toWidget $(cassiusFile "templates/cassius/admin.cassius")
    
arquivos em Hamlet  -> html da tag head 
    toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
    
arquivos em Julius  -> JavaScript
    toWidget $(juliusFile "templates/julius/geoadmin.julius")
    
arquivos em Whamlet -> html da tag body
    toWidget $(whamletFile "templates/whamlet/admin.hamlet") 

PASTA TEMPLATES/WIDGETS: footer e header.
a chamada é feita utilizando ^{footer} e ^{header} dentro
das páginas whamlet em templates.

============================================

widgetForm :: Route SauipeExpress -> Enctype -> Widget -> Widget -> Widget
-- função que gera formulários em forma genérica 
widgetForm x enctype widget novaWidget = [whamlet|
            <h1>
                ^{novaWidget}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input ."btn btn-primary" type="submit" value="_{MsgCadastroBtn}" #"cadastrar">
            <h3>_{MsgCadastro}
|]

Chamada do formulário personalizado dentro de uma página whamlet (templates/whamlet/cadusuario.hamlet):

^{widgetForm CadFuncionarioR enctype widget funcWid} 

funcWid fica no lugar do ^{novaWidget}, é o arquivo de texto da internacionalização em forma de widget.
As declarações estão nas primeiras linhas de Handlers.hs

