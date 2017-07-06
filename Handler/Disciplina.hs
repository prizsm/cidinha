{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Disciplina where

import Foundation
import Yesod
import Data.Text
import Control.Applicative
import Database.Persist.Postgresql

formDisciplina :: Form Disciplina
formDisciplina = renderDivs $ Disciplina
    <$> areq textField "Nome da disciplina" Nothing
    <*> areq textField "Sigla"              Nothing

getDiscR :: Handler Html
getDiscR = do
            (widget, enctype) <- generateFormPost formDisciplina
            defaultLayout [whamlet|
             <form method=post action=@{DiscR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
         |]

postDiscR :: Handler Html
postDiscR = do
            ((result, _), _) <- runFormPost formDisciplina
            case result of
                FormSuccess disciplina -> do
                    disid <- runDB $ insert disciplina
                    defaultLayout [whamlet|
                        Cadastrada com sucesso #{fromSqlKey disid}!
                    |]
                _ -> redirect HomeR

-- SELECT * FROM aluno ORDER BY nome
getListDiscR :: Handler Html
getListDiscR = do
            disciplinas <- runDB $ selectList [] [Asc DisciplinaNome]
            defaultLayout $ do
                [whamlet|
                    <table>
                        <tr> 
                            <td> id  
                            <td> nome
                            <td> sigla 
                        $forall Entity disid disciplina <- disciplinas
                            <tr>
                                <form action=@{DelDiscR disid} method=post> 
                                    <td> #{fromSqlKey           disid}  
                                    <td> #{disciplinaNome  disciplina} 
                                    <td> #{disciplinaSigla disciplina} 
                                    <td> <input type="submit">
                |]
                
postDelDiscR :: DisciplinaId -> Handler Html
postDelDiscR disid = do 
                runDB $ delete disid
                redirect ListDiscR