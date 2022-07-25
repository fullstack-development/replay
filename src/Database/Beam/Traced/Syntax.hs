{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Traced.Syntax where

import Database.Beam.Backend.SQL
import Database.Beam.Traced.Syntax.TH
import Data.Proxy
import qualified Data.Aeson as Aeson

type TracedSyntax = ParallelSyntax SyntaxAST

data ParallelSyntax sa sb = ParallelSyntax sa sb
    deriving (Show, Eq)

getAst :: ParallelSyntax sa sb -> sa
getAst (ParallelSyntax sa _) = sa

getSyn :: ParallelSyntax sa sb -> sb
getSyn (ParallelSyntax _ sb) = sb

newtype SyntaxAST = SyntaxAST { unwrapSyntaxAST :: Aeson.Value }
    deriving (Show, Eq)
    deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

buildSyntaxInstances
    (BuildContext
        ''ParallelSyntax
        'ParallelSyntax
        'getAst
        'getSyn
        ''SyntaxAST
        'SyntaxAST
    )
    [ ''IsSql92AggregationExpressionSyntax
    , ''IsSql92AggregationSetQuantifierSyntax
    , ''IsSql92DataTypeSyntax
    , ''IsSql92ExpressionSyntax
    , ''IsSql92ExtractFieldSyntax
    , ''IsSql92FieldNameSyntax
    , ''IsSql92FromOuterJoinSyntax
    , ''IsSql92FromSyntax
    , ''IsSql92GroupingSyntax
    , ''IsSql92InsertSyntax
    , ''IsSql92InsertValuesSyntax
    , ''IsSql92OrderingSyntax
    , ''IsSql92ProjectionSyntax
    , ''IsSql92QuantifierSyntax
    , ''IsSql92SelectSyntax
    , ''IsSql92SelectTableSyntax
    , ''IsSql92Syntax
    , ''IsSql92TableNameSyntax
    , ''IsSql92TableSourceSyntax
    , ''IsSql92UpdateSyntax
    , ''IsSql99AggregationExpressionSyntax
    , ''IsSql99CommonTableExpressionSelectSyntax
    , ''IsSql99CommonTableExpressionSyntax
    , ''IsSql99ConcatExpressionSyntax
    , ''IsSql99DataTypeSyntax
    , ''IsSql99ExpressionSyntax
    , ''IsSql99FunctionExpressionSyntax
    , ''IsSql99RecursiveCommonTableExpressionSelectSyntax
    , ''IsSql99SelectSyntax
    , ''IsSql2003BinaryAndVarBinaryDataTypeSyntax
    , ''IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax
    , ''IsSql2003EnhancedNumericFunctionsExpressionSyntax
    , ''IsSql2003ExpressionAdvancedOLAPOperationsSyntax
    , ''IsSql2003ExpressionElementaryOLAPOperationsSyntax
    , ''IsSql2003ExpressionSyntax
    , ''IsSql2003FirstValueAndLastValueExpressionSyntax
    , ''IsSql2003FromSyntax
    , ''IsSql2003LeadAndLagExpressionSyntax
    , ''IsSql2003NthValueExpressionSyntax
    , ''IsSql2003NtileExpressionSyntax
    , ''IsSql2003OrderingElementaryOLAPOperationsSyntax
    , ''IsSql2003WindowFrameBoundSyntax
    , ''IsSql2003WindowFrameBoundsSyntax
    , ''IsSql2003WindowFrameSyntax
    , ''IsSql2008BigIntDataTypeSyntax
    ]

instance
    (IsSql92DeleteSyntax sa, IsSql92DeleteSyntax sb) =>
    IsSql92DeleteSyntax (ParallelSyntax sa sb)
  where
    type Sql92DeleteTableNameSyntax (ParallelSyntax sa sb) =
        ParallelSyntax (Sql92DeleteTableNameSyntax sa) (Sql92DeleteTableNameSyntax sb)
    type Sql92DeleteExpressionSyntax (ParallelSyntax sa sb) =
        ParallelSyntax (Sql92DeleteExpressionSyntax sa) (Sql92DeleteExpressionSyntax sb)

    deleteStmt name mbAlias mbWhere =
        ParallelSyntax
            (deleteStmt (getAst name) mbAlias (fmap getAst mbWhere))
            (deleteStmt (getSyn name) mbAlias (fmap getSyn mbWhere))
    deleteSupportsAlias _ =
        deleteSupportsAlias (Proxy :: Proxy sa) &&
        deleteSupportsAlias (Proxy :: Proxy sb)

instance
    (HasSqlValueSyntax sa ty, HasSqlValueSyntax sb ty) =>
    HasSqlValueSyntax (ParallelSyntax sa sb) ty
  where
    sqlValueSyntax x =
        ParallelSyntax
            (sqlValueSyntax x)
            (sqlValueSyntax x)

instance IsSql92DeleteSyntax SyntaxAST where
    type Sql92DeleteTableNameSyntax SyntaxAST = SyntaxAST
    type Sql92DeleteExpressionSyntax SyntaxAST = SyntaxAST

    deleteStmt name mbAlias mbWhere =
        SyntaxAST $
            Aeson.object
                [ "deleteStmt" Aeson..= (name, mbAlias, mbWhere)
                ]
    deleteSupportsAlias _ = True

instance (Aeson.ToJSON ty) => HasSqlValueSyntax SyntaxAST ty where
    sqlValueSyntax x = SyntaxAST (Aeson.toJSON x)

instance {-# OVERLAPPING #-} HasSqlValueSyntax SyntaxAST SqlNull where
    sqlValueSyntax SqlNull = SyntaxAST (Aeson.String "SqlNull")
