{-# LANGUAGE TemplateHaskell #-}

module Database.Beam.Traced.Syntax.TH where

import Language.Haskell.TH
import qualified Data.Aeson as Aeson

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat $ mapM f xs

concatMapM2 :: (Monad m) => (a -> m ([b], [c])) -> [a] -> m ([b], [c])
concatMapM2 f = go id id
  where
    go bufB bufC [] = do
        pure (bufB [], bufC [])
    go bufB bufC (x : xs) = do
        (bs, cs) <- f x
        go (bufB . (bs ++)) (bufC . (cs ++)) xs

mapT2 :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
mapT2 fa fb ~(a, b) = (fa a, fb b)

data BuildContext = BuildContext
    { parsynTypeName :: Name
    , parsynConName :: Name
    , parsynFieldAName :: Name
    , parsynFieldBName :: Name
    , astTypeName :: Name
    , astConName :: Name
    }

buildSyntaxInstances ::
    BuildContext -> [Name] -> Q [Dec]
buildSyntaxInstances buildContext classNameList = do
    concatMapM
        (buildInstance buildContext)
        classNameList

buildInstance :: BuildContext -> Name -> Q [Dec]
buildInstance buildContext className = do
    ClassI
        (ClassD _ _ [KindedTV typeParamName StarT] _ classItems)
        _
      <-
        reify className
    let classParamType = VarT typeParamName
    saType <- VarT <$> newName "sa"
    sbType <- VarT <$> newName "sb"
    (parItems, serItems) <-
        concatMapM2
            (buildInstanceItem buildContext classParamType saType sbType)
            classItems
    pure
        [ InstanceD
            Nothing
            [ ConT className `AppT` saType
            , ConT className `AppT` sbType
            ]
            (   ConT className
              `AppT`
                (ConT (parsynTypeName buildContext)
                    `AppT` saType
                    `AppT` sbType
                )
            )
            parItems
        , InstanceD
            Nothing
            []
            (ConT className `AppT` (ConT (astTypeName buildContext)))
            serItems
        ]

buildInstanceItem ::
    BuildContext -> Type -> Type -> Type -> Dec -> Q ([Dec], [Dec])
buildInstanceItem buildContext classParamType saType sbType itemDecl = do
    case itemDecl of
        OpenTypeFamilyD
            (TypeFamilyHead
                famName
                [KindedTV _ StarT]
                (KindSig StarT)
                Nothing
            )
          ->
            buildTypeInstance buildContext saType sbType famName
        SigD methodName methodType ->
            buildMethod buildContext classParamType methodName methodType
        _ ->
            fail $ "unexpected class item: " <> show itemDecl

buildTypeInstance ::
    BuildContext -> Type -> Type -> Name -> Q ([Dec], [Dec])
buildTypeInstance buildContext saType sbType famName = do
    let parInstanceDecs =
            [ TySynInstD $
                TySynEqn
                    Nothing
                    (   ConT famName
                      `AppT`
                        (ConT (parsynTypeName buildContext)
                            `AppT` saType
                            `AppT` sbType
                        )
                    )
                    (   ConT (parsynTypeName buildContext)
                      `AppT`
                        (ConT famName `AppT` saType)
                      `AppT`
                        (ConT famName `AppT` sbType)
                    )
            ]
    let serInstanceDecs =
            [ TySynInstD $
                TySynEqn
                    Nothing
                    (ConT famName `AppT` (ConT (astTypeName buildContext)))
                    (ConT (astTypeName buildContext))
            ]
    pure (parInstanceDecs, serInstanceDecs)

buildMethod ::
    BuildContext -> Type -> Name -> Type -> Q ([Dec], [Dec])
buildMethod buildContext classParamType methodName methodType = do
    methodParams <-
        processMethodType buildContext classParamType methodType
    let paramPats = map (\(pn, _, _) -> VarP pn) methodParams
    let exprA =
            foldl
                (\v (_, ea, _) -> AppE v ea)
                (VarE methodName)
                methodParams
    let exprB =
            foldl
                (\v (_, _, eb) -> AppE v eb)
                (VarE methodName)
                methodParams
    let parMethodDecs =
            [ FunD
                methodName
                [ Clause
                    paramPats
                    (NormalB $
                        ConE (parsynConName buildContext)
                            `AppE` exprA
                            `AppE` exprB
                    )
                    []
                ]
            ]
    let methodNameExpr = LitE (StringL (nameBase methodName))
    astJsonExpr <-
        case methodParams of
            [] -> do
                pure $
                    ConE 'Aeson.String
                  `AppE`
                    methodNameExpr
            [(pn, _, _)] -> do
                pure $
                    VarE 'Aeson.object
                  `AppE`
                    ListE
                        [   VarE '(Aeson..=)
                          `AppE`
                            methodNameExpr
                          `AppE`
                            VarE pn
                        ]
            _ -> do
                pure $
                    VarE 'Aeson.object
                  `AppE`
                    ListE
                        [   VarE '(Aeson..=)
                          `AppE`
                            methodNameExpr
                          `AppE`
                            (TupE $ map
                                (\(pn, _, _) -> Just (VarE pn))
                                methodParams
                            )
                        ]
    let serMethodDecs =
            [ FunD
                methodName
                [ Clause
                    paramPats
                    (NormalB $
                        ConE (astConName buildContext)
                            `AppE` astJsonExpr
                    )
                    []
                ]
            ]
    pure (parMethodDecs, serMethodDecs)

processMethodType ::
    BuildContext ->
    Type ->
    Type ->
    Q [(Name, Exp, Exp)]
processMethodType buildContext classParamType =
    go (0 :: Int) []
  where
    go i methodParams (AppT (AppT ArrowT methodParamType) nextType) = do
        paramName <- newName ("a" <> show i)
        let paramE = VarE paramName
        (pmapA, pmapB) <-
            buildParamMapping
                buildContext
                classParamType methodParamType
        let (exprA, exprB) =
                if pmapA == pmapB
                    then (paramE, paramE)
                    else (AppE pmapA paramE, AppE pmapB paramE)
        go (i + 1) ((paramName, exprA, exprB) : methodParams) nextType
    go _ methodParams resultType = do
        if resultType == classParamType
            then pure (reverse methodParams)
            else fail $ "unexpected method result type: " <> show resultType

buildParamMapping ::
    BuildContext -> Type -> Type -> Q (Exp, Exp)
buildParamMapping buildContext classParamType = go
  where
    wrapperFunctors = [ListT, ConT ''Maybe]
    go (AppT (AppT (TupleT 2) tt1) tt2) = do
        (tt1MapA, tt1MapB) <-
            buildParamMapping buildContext classParamType tt1
        (tt2MapA, tt2MapB) <-
            buildParamMapping buildContext classParamType tt2
        if tt1MapA == tt1MapB && tt2MapA == tt2MapB
            then pure (VarE 'id, VarE 'id)
            else
                pure
                    ( AppE (AppE (VarE 'mapT2) tt1MapA) tt2MapA
                    , AppE (AppE (VarE 'mapT2) tt1MapB) tt2MapB
                    )
    go (AppT conType inType)
        | conType `elem` wrapperFunctors = do
            (inMapA, inMapB) <-
                buildParamMapping
                    buildContext
                    classParamType inType
            if inMapA == inMapB
                then pure (VarE 'id, VarE 'id)
                else
                    pure
                        ( AppE (VarE 'fmap) inMapA
                        , AppE (VarE 'fmap) inMapB
                        )
        | inType == classParamType = do
            pure
                ( VarE (parsynFieldAName buildContext)
                , VarE (parsynFieldBName buildContext)
                )
    go aType
        | aType == classParamType = do
            pure
                ( VarE (parsynFieldAName buildContext)
                , VarE (parsynFieldBName buildContext)
                )
        | otherwise = do
            pure (VarE 'id, VarE 'id)
