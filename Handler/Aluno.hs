{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Aluno where

import Foundation
import Yesod
import Data.Text
import Control.Applicative
import Database.Persist.Postgresql

-- /aluno/cadastro       AlunoR    GET POST
-- /aluno/listagem       ListAluR  GET

formAluno :: Form Aluno
formAluno = renderDivs $ Aluno
    <$> areq textField "Nome do aluno" Nothing
    <*> areq textField "RG"            Nothing
    <*> areq intField  "Idade"         Nothing

getAlunoR :: Handler Html
getAlunoR = do
            (widget, enctype) <- generateFormPost formAluno
            defaultLayout [whamlet|
             <form method=post action=@{AlunoR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
            |]

postAlunoR :: Handler Html
postAlunoR = do
            ((result, _), _) <- runFormPost formAluno
            case result of
                FormSuccess aluno -> do
                    alid <- runDB $ insert aluno
                    defaultLayout [whamlet|
                        Cadastrado com sucesso #{fromSqlKey alid}!
                    |]
                _ -> redirect HomeR

-- SELECT * FROM aluno ORDER BY nome
getListAluR :: Handler Html
getListAluR = do
            alunos <- runDB $ selectList [] [Asc AlunoNome]
            defaultLayout $(whamletFile "templates/table.hamlet")
                
postDelAlunoR :: AlunoId -> Handler Html
postDelAlunoR alid = do 
                runDB $ delete alid
                redirect ListAluR

footerzinho :: Widget
footerzinho = [whamlet|
                  <footer>
                      VAI "CORINTHIANS"
              |]