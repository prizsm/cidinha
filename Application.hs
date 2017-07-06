{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}

module Application where

import Foundation
import Yesod

-- AQUI MORAM OS HANDLERS
-- import Add
-- PARA CADA NOVO GRUPO DE HANDLERS, CRIAR UM ARQUIVO
-- DE HANDLER NOVO E IMPORTAR AQUI
import Handler.Disciplina
import Handler.Aluno
import Handler.Usuario
------------------
mkYesodDispatch "App" resourcesApp

getHomeR :: Handler Html
getHomeR = do
    sess <- lookupSession "_ID"
    defaultLayout $ do
        addStylesheet $ StaticR estilo_css
        [whamlet|
            <h1> Meu primeiro site em Haskell!
            <ul>
                <li> <a href=@{AlunoR}>Cadastro de aluno</a> |
                <li> <a href=@{ListAluR}>Listagem de aluno</a> |
                <li> <a href=@{DiscR}>Cadastro de disciplina</a> |
                <li> <a href=@{ListDiscR}>Listagem de disciplina</a> |
                $maybe _ <- sess
                    <li> 
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Logout">
                $nothing
                    <li> <a href=@{LoginR}>Login
        |]