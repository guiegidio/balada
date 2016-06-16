{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
module Handlers where
import Rotas
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Yesod.Static
import Text.Lucius
import Text.Cassius
import Database.Persist.Postgresql
--adicionado
import Database.Persist
import Database.Persist.Sqlite
-- import Database.Persist.Sql.Raw 
import Database.Persist.Sql (rawQuery)



mkYesodDispatch "Balada" pRoutes

--FORMULARIOS
formPessoa :: Form Pessoas
formPessoa = renderDivs $ Pessoas <$>
    areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="CPF",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","11")]} Nothing<*>
    areq textField "Nome: " Nothing <*>
    areq textField "Login: " Nothing <*>
    areq passwordField "Senha: " Nothing <*>
    areq textField "Cidade: " Nothing <*>
    areq textField "E-mail: " Nothing <*>
    areq textField "DDD: " Nothing <*>
    areq textField "Celular: " Nothing

formEstabelecimento :: Form Estabelecimento
formEstabelecimento = renderDivs $ Estabelecimento <$>
    areq textField "Nome: " Nothing <*>
    areq textField "Endereço: " Nothing <*>
    areq textField "Cidade: " Nothing <*>
    areq textField "Email: " Nothing <*>
    areq textField "DDD: " Nothing <*>
    areq textField "Telefone: " Nothing <*>
    areq (selectField categs) "Categoria: " Nothing <*>
    areq (selectField listaFaixapreco) "Preço: " Nothing

--Form T_categoria_estab
formCategoria :: Form Categoria_estab
formCategoria = renderDivs $ Categoria_estab <$>
    areq textField "Categoria: " Nothing 

--Form T_sub_categ_estab
formSubcategoria :: Form Sub_categ_estab
formSubcategoria = renderDivs $ Sub_categ_estab <$>
    areq textField "Categoria" Nothing <*>
    areq textField "Subcategoria: " Nothing

--Form T_faixa_preco    
formFaixapreco :: Form Faixa_preco
formFaixapreco =  renderDivs $ Faixa_preco <$>
    areq textField "Nome: " Nothing <*>
    areq textField "Valor Inicial: " Nothing <*>
    areq textField "Valor Final: " Nothing 
    
formDia :: Form Dia_evento
formDia =  renderDivs $ Dia_evento <$>
    areq textField "Dia: " Nothing 

--Form T_F_interesse_pessoa

--Form Login
formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
          areq textField "Login: " Nothing <*>
          areq passwordField "Senha: " Nothing
    
--Lista Drop Down
-- testar o funfar
estabs = do 
        entidades <- runDB $ selectList [] [Asc EstabelecimentoNome_estab]
        optionsPairs $ fmap(\ent -> (estabelecimentoNome_estab $ entityVal ent, entityKey ent)) entidades

categs = do 
        entidades <- runDB $ selectList [] [Asc Categoria_estabNome_categoria]
        optionsPairs $ fmap(\ent -> (categoria_estabNome_categoria $ entityVal ent, entityKey ent)) entidades
    
listaSubcategoria = do 
                   entidades <- runDB $ selectList [] [Asc Sub_categ_estabNome_sub_categ] 
                   optionsPairs $ fmap(\ent ->(sub_categ_estabNome_sub_categ $ entityVal ent, entityKey ent)) entidades

listaClassificacao = do
                    entidades <- runDB $ selectList [] [Asc Classificacao_estabNome_classificacao] 
                    optionsPairs $ fmap(\ent ->(classificacao_estabNome_classificacao $ entityVal ent, entityKey ent)) entidades
    
listaFaixapreco = do
                 entidades <- runDB $ selectList [] [Asc Faixa_precoNome_faixa_preco] 
                 optionsPairs $ fmap(\ent ->(faixa_precoNome_faixa_preco $ entityVal ent, entityKey ent)) entidades
    
listaDia = do
          entidades <- runDB $ selectList [] [Asc Dia_eventoDc_dia] 
          optionsPairs $ fmap(\ent ->(dia_eventoDc_dia $ entityVal ent, entityKey ent)) entidades

--PAGINAS
getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
          addStylesheet $ StaticR style_css
          toWidget $ $(whamletFile "Hamlets/home.hamlet")

--Alcool na mesa
getCadpessoaR :: Handler Html
getCadpessoaR = do 
    (widget, enctype) <-generateFormPost formPessoa
    defaultLayout $ do 
          addStylesheet $ StaticR style_css
          toWidget $ $(whamletFile "Hamlets/pessoas.hamlet")

getCadestabR :: Handler Html
getCadestabR = do 
    (widget, enctype) <-generateFormPost formEstabelecimento
    defaultLayout $ do
    addStylesheet $ StaticR style_css
    toWidget$ $(whamletFile "Hamlets/estabelecimento.hamlet")
    
getCadcategoriaR :: Handler Html
getCadcategoriaR = do 
    (widget, enctype) <-generateFormPost formCategoria
    defaultLayout $ do
    addStylesheet $ StaticR style_css
    toWidget$ $(whamletFile "Hamlets/categoria.hamlet")

getCadsubcategR :: Handler Html
getCadsubcategR = do
    (widget, enctype) <-generateFormPost formSubcategoria
    defaultLayout $ do
    addStylesheet $ StaticR style_css
    toWidget$ $(whamletFile "Hamlets/subcategoria.hamlet")

getCaddiaR :: Handler Html 
getCaddiaR = do
    (widget, enctype) <-generateFormPost formDia
    defaultLayout $ do
    addStylesheet $ StaticR style_css
    toWidget$ $(whamletFile "Hamlets/dia.hamlet")

getCadfaixaprecoR :: Handler Html    
getCadfaixaprecoR = do
    (widget, enctype) <-generateFormPost formFaixapreco
    defaultLayout $ do
    addStylesheet $ StaticR style_css
    toWidget$ $(whamletFile "Hamlets/preco.hamlet")
 
--adicionado: https://github.com/yesodweb/yesod/wiki/RawSQL site de referência    
--getPesquisaR :: Handler Html
--getPesquisaR = do 
--    estabelecimento <- selectEstab pattern 
--    defaultLayout $ do
--    addStylesheet $ StaticR style_css
--    toWidget$ $(whamletFile "Hamlets/pesquisa.hamlet")
--  where
--    selectEstab :: Text -> Handler[Entity Estabelecimento]
--    selectEstab t = runDB $ rawSQL s [toPersistValue t]
--    where s = "SELECT ?? FROM estabelecimento WHERE cidade_estab = ? ORDER BY nome_estab"


--alcool na garrafa
postCadpessoaR :: Handler Html
postCadpessoaR = do
    ((result, _),_) <- runFormPost formPessoa
    case result of
        FormSuccess pessoa -> (runDB $ insert pessoa) >>= \cpf_pessoa -> redirect (CadpessoaR)
        _ -> redirect ErroR
        
postCadestabR :: Handler Html
postCadestabR = do
    ((result, _),_) <- runFormPost formEstabelecimento
    case result of
        FormSuccess estab -> (runDB $ insert estab) >>= \cd_estabelecimento -> redirect (CadestabR)
        _ -> redirect ErroR

postCadcategoriaR :: Handler Html
postCadcategoriaR = do
    ((result, _),_) <- runFormPost formCategoria
    case result of
        FormSuccess categoria -> (runDB $ insert categoria) >>= \cd_categoria_estab -> redirect (CadcategoriaR)
        _ -> redirect ErroR
        
postCadsubcategR :: Handler Html
postCadsubcategR = do
    ((result, _),_) <- runFormPost formSubcategoria
    case result of
        FormSuccess subcategoria -> (runDB $ insert subcategoria) >>= \cd_sub_categ_estab -> redirect (CadsubcategR)
        _ -> redirect ErroR
        
postCadfaixaprecoR :: Handler Html
postCadfaixaprecoR = do
    ((result, _),_) <- runFormPost formFaixapreco
    case result of
        FormSuccess faixapreco -> (runDB $ insert faixapreco) >>= \cd_faixa_preco -> redirect (CadfaixaprecoR)
        _ -> redirect ErroR
        
postCaddiaR :: Handler Html
postCaddiaR = do
    ((result, _),_) <- runFormPost formDia
    case result of
        FormSuccess dia -> (runDB $ insert dia) >>= \cd_dia -> redirect (CadfaixaprecoR)
        _ -> redirect ErroR



--Limbo Mental
getAdminR :: Handler Html
getAdminR = defaultLayout $ do 
          addStylesheet $ StaticR style_css
          toWidget $ $(whamletFile "Hamlets/administrador.hamlet")



getLoginR :: Handler Html
getLoginR = do 
            (widget, enctype) <-generateFormPost formLogin
            defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget$ $(whamletFile "Hamlets/login.hamlet")
    

postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do 
                   pessoas <- runDB $ selectFirst [PessoasLogin ==. login, PessoasSenha ==. senha] []
                   case pessoas of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)
               _ -> redirect ErroR

getPerfilR :: PessoasId -> Handler Html
getPerfilR uid = do
      pessoas <- runDB $ get404 uid
      defaultLayout $ do
        addStylesheet $ StaticR style_css
        toWidget$ $(whamletFile "Hamlets/perfil.hamlet")
      
getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
     <h1> Erro de cadastro
|]

getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout $ do
      addStylesheet $ StaticR style_css
      toWidget$ $(whamletFile "Hamlets/logout.hamlet")
    
getListaestabsR :: Handler Html
getListaestabsR = do
    allEstabs <- runDB $ selectList [] [Asc EstabelecimentoNome_estab]
    defaultLayout $ do
        addStylesheet $ StaticR style_css
        toWidget $ $(whamletFile "Hamlets/listaestab.hamlet")

--getListapessoasR :: Handler Html
--getListapessoasR = do
--        listaPessoas <- runDB $ selectList [] [Asc PessoasNome]
--        defaultLayout $ do
--            addStylesheet $ StaticR style_css
--            toWidget $ $(whamletFile "Hamlets/listapessoas.hamlet")
        
--getDelpessoasR :: PessoasId -> Handler Html
--getDelpessoasR pid = do
--        runDB $ get404 pid
--        runDB $ delete $ pid
--        redirect ListapessoasR

getAboutR = defaultLayout [whamlet|
    Essa pagina foi criada para acelerar o processo de decidir roles!!!
    <a href=@{HomeR}>Back Home
|]
