{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod
import Data.Text
import Control.Applicative

formProd :: Form Produtok
formProd = renderDivs $ Produtok
    <$> areq textField "Nome do produto" Nothing
    <*> areq doubleField "Preço"         Nothing

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
                <h1> Bem-vindo à página!
           |]

getProdutoR :: Handler Html
getProdutoR = do
         (widget, enctype) <- generateFormPost formProd
         defaultLayout [whamlet|
             <form method=post action=@{ProdutoR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
         |]
         
postProdutoR :: Handler Html
postProdutoR = do
    ((result, _), _) <- runFormPost formProd
    case result of
        FormSuccess produto -> do
            runDB $ insert produto
            defaultLayout [whamlet|
                Cadastrado com sucesso!
            |]
        _ -> redirect HomeR