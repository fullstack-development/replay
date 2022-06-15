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

{-
instance
    (IsSql92Syntax tr, IsSql92Syntax sn) =>
    IsSql92Syntax (TracedSyntax tr sn)
  where
    type Sql92SelectSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92SelectSyntax tr) (Sql92SelectSyntax sn)
    type Sql92UpdateSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92UpdateSyntax tr) (Sql92UpdateSyntax sn)
    type Sql92InsertSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92InsertSyntax tr) (Sql92InsertSyntax sn)
    type Sql92DeleteSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92DeleteSyntax tr) (Sql92DeleteSyntax sn)

    selectCmd cmd = TracedSyntax (selectCmd (_tr cmd)) (selectCmd (_sn cmd))
    insertCmd cmd = TracedSyntax (insertCmd (_tr cmd)) (insertCmd (_sn cmd))
    updateCmd cmd = TracedSyntax (updateCmd (_tr cmd)) (updateCmd (_sn cmd))
    deleteCmd cmd = TracedSyntax (deleteCmd (_tr cmd)) (deleteCmd (_sn cmd))

instance
    (IsSql92SelectSyntax tr, IsSql92SelectSyntax sn) =>
    IsSql92SelectSyntax (TracedSyntax tr sn)
  where
    type Sql92SelectSelectTableSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92SelectSelectTableSyntax tr) (Sql92SelectSelectTableSyntax sn)
    type Sql92SelectOrderingSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92SelectOrderingSyntax tr) (Sql92SelectOrderingSyntax sn)

    selectStmt table ordList mbLim mbOfs =
        TracedSyntax
            (selectStmt (_tr table) (map _tr ordList) mbLim mbOfs)
            (selectStmt (_sn table) (map _sn ordList) mbLim mbOfs)

instance
    (IsSql92SelectTableSyntax tr, IsSql92SelectTableSyntax sn) =>
    IsSql92SelectTableSyntax (TracedSyntax tr sn)
  where
    type Sql92SelectTableSelectSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92SelectTableSelectSyntax tr) (Sql92SelectTableSelectSyntax sn)
    type Sql92SelectTableExpressionSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92SelectTableExpressionSyntax tr) (Sql92SelectTableExpressionSyntax sn)
    type Sql92SelectTableProjectionSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92SelectTableProjectionSyntax tr) (Sql92SelectTableProjectionSyntax sn)
    type Sql92SelectTableFromSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92SelectTableFromSyntax tr) (Sql92SelectTableFromSyntax sn)
    type Sql92SelectTableGroupingSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92SelectTableGroupingSyntax tr) (Sql92SelectTableGroupingSyntax sn)
    type Sql92SelectTableSetQuantifierSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92SelectTableSetQuantifierSyntax tr) (Sql92SelectTableSetQuantifierSyntax sn)

    selectTableStmt mbSetQuant proj mbFrom mbWhere mbGroup mbHaving =
        TracedSyntax
            (selectTableStmt
                (fmap _tr mbSetQuant)
                (_tr proj)
                (fmap _tr mbFrom)
                (fmap _tr mbWhere)
                (fmap _tr mbGroup)
                (fmap _tr mbHaving)
            )
            (selectTableStmt
                (fmap _sn mbSetQuant)
                (_sn proj)
                (fmap _sn mbFrom)
                (fmap _sn mbWhere)
                (fmap _sn mbGroup)
                (fmap _sn mbHaving)
            )
    unionTables isd sel1 sel2 =
        TracedSyntax
            (unionTables isd (_tr sel1) (_tr sel2))
            (unionTables isd (_sn sel1) (_sn sel2))
    intersectTables isd sel1 sel2 =
        TracedSyntax
            (intersectTables isd (_tr sel1) (_tr sel2))
            (intersectTables isd (_sn sel1) (_sn sel2))
    exceptTable isd sel1 sel2 =
        TracedSyntax
            (exceptTable isd (_tr sel1) (_tr sel2))
            (exceptTable isd (_sn sel1) (_sn sel2))

instance
    (IsSql92InsertSyntax tr, IsSql92InsertSyntax sn) =>
    IsSql92InsertSyntax (TracedSyntax tr sn)
  where
    type Sql92InsertValuesSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92InsertValuesSyntax tr) (Sql92InsertValuesSyntax sn)
    type Sql92InsertTableNameSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92InsertTableNameSyntax tr) (Sql92InsertTableNameSyntax sn)

    insertStmt name fields values =
        TracedSyntax
            (insertStmt (_tr name) fields (_tr values))
            (insertStmt (_sn name) fields (_sn values))

instance
    (IsSql92InsertValuesSyntax tr, IsSql92InsertValuesSyntax sn) =>
    IsSql92InsertValuesSyntax (TracedSyntax tr sn)
  where
    type Sql92InsertValuesExpressionSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92InsertValuesExpressionSyntax tr) (Sql92InsertValuesExpressionSyntax sn)
    type Sql92InsertValuesSelectSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92InsertValuesSelectSyntax tr) (Sql92InsertValuesSelectSyntax sn)

    insertSqlExpressions exprs =
        TracedSyntax
            (insertSqlExpressions (fmap (fmap _tr) exprs))
            (insertSqlExpressions (fmap (fmap _sn) exprs))
    insertFromSql sql =
        TracedSyntax
            (insertFromSql (_tr sql))
            (insertFromSql (_sn sql))

instance
    (IsSql92UpdateSyntax tr, IsSql92UpdateSyntax sn) =>
    IsSql92UpdateSyntax (TracedSyntax tr sn)
  where
    type Sql92UpdateTableNameSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92UpdateTableNameSyntax tr) (Sql92UpdateTableNameSyntax sn)
    type Sql92UpdateFieldNameSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92UpdateFieldNameSyntax tr) (Sql92UpdateFieldNameSyntax sn)
    type Sql92UpdateExpressionSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92UpdateExpressionSyntax tr) (Sql92UpdateExpressionSyntax sn)

    updateStmt name sets mbWhere =
        TracedSyntax
            (updateStmt (_tr name) (map (\(a, b) -> (_tr a, _tr b)) sets) (fmap _tr mbWhere))
            (updateStmt (_sn name) (map (\(a, b) -> (_sn a, _sn b)) sets) (fmap _sn mbWhere))

instance
    (IsSql92DeleteSyntax tr, IsSql92DeleteSyntax sn) =>
    IsSql92DeleteSyntax (TracedSyntax tr sn)
  where
    type Sql92DeleteTableNameSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92DeleteTableNameSyntax tr) (Sql92DeleteTableNameSyntax sn)
    type Sql92DeleteExpressionSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92DeleteExpressionSyntax tr) (Sql92DeleteExpressionSyntax sn)

    deleteStmt name mbAlias mbWhere =
        TracedSyntax
            (deleteStmt (_tr name) mbAlias (fmap _tr mbWhere))
            (deleteStmt (_sn name) mbAlias (fmap _sn mbWhere))
    deleteSupportsAlias _ =
        deleteSupportsAlias (Proxy :: Proxy sn)

instance
    (IsSql92FieldNameSyntax tr, IsSql92FieldNameSyntax sn) =>
    IsSql92FieldNameSyntax (TracedSyntax tr sn)
  where
    qualifiedField a b =
        TracedSyntax
            (qualifiedField a b)
            (qualifiedField a b)
    unqualifiedField a =
        TracedSyntax
            (unqualifiedField a)
            (unqualifiedField a)

instance
    (IsSql92QuantifierSyntax tr, IsSql92QuantifierSyntax sn) =>
    IsSql92QuantifierSyntax (TracedSyntax tr sn)
  where
    quantifyOverAll =
        TracedSyntax
            quantifyOverAll
            quantifyOverAll
    quantifyOverAny =
        TracedSyntax
            quantifyOverAny
            quantifyOverAny

instance
    (IsSql92ExtractFieldSyntax tr, IsSql92ExtractFieldSyntax sn) =>
    IsSql92ExtractFieldSyntax (TracedSyntax tr sn)
  where
    secondsField =
        TracedSyntax
            secondsField
            secondsField
    minutesField =
        TracedSyntax
            minutesField
            minutesField
    hourField =
        TracedSyntax
            hourField
            hourField
    dayField =
        TracedSyntax
            dayField
            dayField
    monthField =
        TracedSyntax
            monthField
            monthField
    yearField =
        TracedSyntax
            yearField
            yearField

instance
    (IsSql92DataTypeSyntax tr, IsSql92DataTypeSyntax sn) =>
    IsSql92DataTypeSyntax (TracedSyntax tr sn)
  where
    domainType a =
        TracedSyntax
            (domainType a)
            (domainType a)
    charType a b =
        TracedSyntax
            (charType a b)
            (charType a b)
    varCharType a b =
        TracedSyntax
            (varCharType a b)
            (varCharType a b)
    nationalCharType a =
        TracedSyntax
            (nationalCharType a)
            (nationalCharType a)
    nationalVarCharType a =
        TracedSyntax
            (nationalVarCharType a)
            (nationalVarCharType a)
    bitType a =
        TracedSyntax
            (bitType a)
            (bitType a)
    varBitType a =
        TracedSyntax
            (varBitType a)
            (varBitType a)
    numericType a =
        TracedSyntax
            (numericType a)
            (numericType a)
    decimalType a =
        TracedSyntax
            (decimalType a)
            (decimalType a)
    intType =
        TracedSyntax
            intType
            intType
    smallIntType =
        TracedSyntax
            smallIntType
            smallIntType
    floatType a =
        TracedSyntax
            (floatType a)
            (floatType a)
    doubleType =
        TracedSyntax
            doubleType
            doubleType
    realType =
        TracedSyntax
            realType
            realType
    dateType =
        TracedSyntax
            dateType
            dateType
    timeType a b =
        TracedSyntax
            (timeType a b)
            (timeType a b)
    timestampType a b =
        TracedSyntax
            (timestampType a b)
            (timestampType a b)

instance
    (IsSql99DataTypeSyntax tr, IsSql99DataTypeSyntax sn) =>
    IsSql99DataTypeSyntax (TracedSyntax tr sn)
  where
    characterLargeObjectType =
        TracedSyntax
            characterLargeObjectType
            characterLargeObjectType
    binaryLargeObjectType =
        TracedSyntax
            binaryLargeObjectType
            binaryLargeObjectType
    booleanType =
        TracedSyntax
            booleanType
            booleanType
    arrayType elem b =
        TracedSyntax
            (arrayType (_tr elem) b)
            (arrayType (_sn elem) b)
    rowType rows =
        TracedSyntax
            (rowType (map (\(n, t) -> (n, _tr t)) rows))
            (rowType (map (\(n, t) -> (n, _sn t)) rows))

instance
    (IsSql2008BigIntDataTypeSyntax tr, IsSql2008BigIntDataTypeSyntax sn) =>
    IsSql2008BigIntDataTypeSyntax (TracedSyntax tr sn)
  where
    bigIntType =
        TracedSyntax
            bigIntType
            bigIntType

instance
    (IsSql92AggregationSetQuantifierSyntax tr, IsSql92AggregationSetQuantifierSyntax sn) =>
    IsSql92AggregationSetQuantifierSyntax (TracedSyntax tr sn)
  where
    setQuantifierDistinct =
        TracedSyntax
            setQuantifierDistinct
            setQuantifierDistinct

instance
    (IsSql92ExpressionSyntax tr, IsSql92ExpressionSyntax sn) =>
    IsSql92ExpressionSyntax (TracedSyntax tr sn)
  where
    type Sql92ExpressionQuantifierSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92ExpressionQuantifierSyntax tr) (Sql92ExpressionQuantifierSyntax sn)
    type Sql92ExpressionValueSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92ExpressionValueSyntax tr) (Sql92ExpressionValueSyntax sn)
    type Sql92ExpressionSelectSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92ExpressionSelectSyntax tr) (Sql92ExpressionSelectSyntax sn)
    type Sql92ExpressionFieldNameSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92ExpressionFieldNameSyntax tr) (Sql92ExpressionFieldNameSyntax sn)
    type Sql92ExpressionCastTargetSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92ExpressionCastTargetSyntax tr) (Sql92ExpressionCastTargetSyntax sn)
    type Sql92ExpressionExtractFieldSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92ExpressionExtractFieldSyntax tr) (Sql92ExpressionExtractFieldSyntax sn)

    -- valueE = Expression_Value
    -- rowE = Expression_Row

    -- isNullE = Expression_IsNull
    -- isNotNullE = Expression_IsNotNull
    -- isTrueE = Expression_IsTrue
    -- isNotTrueE = Expression_IsNotTrue
    -- isFalseE = Expression_IsFalse
    -- isNotFalseE = Expression_IsNotFalse
    -- isUnknownE = Expression_IsUnknown
    -- isNotUnknownE = Expression_IsNotUnknown

    -- caseE = Expression_Case
    -- coalesceE = Expression_Coalesce
    -- nullIfE = Expression_NullIf
    -- positionE = Expression_Position
    -- extractE = Expression_Extract
    -- castE = Expression_Cast

    -- fieldE = Expression_FieldName

    -- betweenE = Expression_Between
    -- andE = Expression_BinOp "AND"
    -- orE = Expression_BinOp "OR"

    -- eqE = Expression_CompOp "=="
    -- neqE = Expression_CompOp "<>"
    -- ltE = Expression_CompOp "<"
    -- gtE = Expression_CompOp ">"
    -- leE = Expression_CompOp "<="
    -- geE = Expression_CompOp ">="
    -- addE = Expression_BinOp "+"
    -- subE = Expression_BinOp "-"
    -- mulE = Expression_BinOp "*"
    -- divE = Expression_BinOp "/"
    -- modE = Expression_BinOp "%"
    -- likeE = Expression_BinOp "LIKE"
    -- overlapsE = Expression_BinOp "OVERLAPS"

    -- notE = Expression_UnOp "NOT"
    -- negateE = Expression_UnOp "-"

    -- charLengthE = Expression_CharLength
    -- octetLengthE = Expression_OctetLength
    -- bitLengthE = Expression_BitLength
    -- absE = Expression_Abs
    -- lowerE = Expression_Lower
    -- upperE = Expression_Upper
    -- trimE = Expression_Trim

    -- subqueryE = Expression_Subquery
    -- uniqueE = Expression_Unique
    -- existsE = Expression_Exists

    -- currentTimestampE = Expression_CurrentTimestamp

    -- defaultE = Expression_Default
    -- inE = Expression_In

instance
    (IsSql99FunctionExpressionSyntax tr, IsSql99FunctionExpressionSyntax sn) =>
    IsSql99FunctionExpressionSyntax (TracedSyntax tr sn)
  where
    -- functionNameE = Expression_NamedFunction
    -- functionCallE = Expression_FunctionCall

instance
    (IsSql99ExpressionSyntax tr, IsSql99ExpressionSyntax sn) =>
    IsSql99ExpressionSyntax (TracedSyntax tr sn)
  where
    -- distinctE = Expression_Distinct
    -- similarToE = Expression_BinOp "SIMILAR TO"
    -- instanceFieldE = Expression_InstanceField
    -- refFieldE = Expression_RefField

instance
    (IsSql92AggregationExpressionSyntax tr, IsSql92AggregationExpressionSyntax sn) =>
    IsSql92AggregationExpressionSyntax (TracedSyntax tr sn)
  where
    type Sql92AggregationSetQuantifierSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92AggregationSetQuantifierSyntax tr) (Sql92AggregationSetQuantifierSyntax sn)

    -- countAllE = Expression_CountAll
    -- countE q = Expression_Agg "COUNT" q . pure
    -- sumE q   = Expression_Agg "SUM" q . pure
    -- minE q   = Expression_Agg "MIN" q . pure
    -- maxE q   = Expression_Agg "MAX" q . pure
    -- avgE q   = Expression_Agg "AVG" q . pure

instance
    (IsSql99AggregationExpressionSyntax tr, IsSql99AggregationExpressionSyntax sn) =>
    IsSql99AggregationExpressionSyntax (TracedSyntax tr sn)
  where
    -- everyE q = Expression_Agg "EVERY" q . pure
    -- someE q  = Expression_Agg "SOME" q . pure
    -- anyE q   = Expression_Agg "ANY" q . pure

instance
    ( IsSql2003EnhancedNumericFunctionsExpressionSyntax tr
    , IsSql2003EnhancedNumericFunctionsExpressionSyntax sn
    ) =>
    IsSql2003EnhancedNumericFunctionsExpressionSyntax (TracedSyntax tr sn)
  where
    -- lnE    = Expression_BuiltinFunction "LN" . pure
    -- expE   = Expression_BuiltinFunction "EXP" . pure
    -- sqrtE  = Expression_BuiltinFunction "SQRT" . pure
    -- ceilE  = Expression_BuiltinFunction "CEIL" . pure
    -- floorE = Expression_BuiltinFunction "FLOOR" . pure
    -- powerE a b = Expression_BuiltinFunction "POWER" [a, b]

instance
    ( IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax tr
    , IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax sn
    ) =>
    IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax (TracedSyntax tr sn)
  where
    -- stddevPopE q  = Expression_Agg "STDDEV_POP" q . pure
    -- stddevSampE q = Expression_Agg "STDDEV_SAMP" q . pure
    -- varPopE q     = Expression_Agg "VAR_POP" q . pure
    -- varSampE q    = Expression_Agg "VAR_SAMP" q . pure

    -- covarPopE q a b      = Expression_Agg "COVAR_POP" q [a, b]
    -- covarSampE q a b     = Expression_Agg "COVAR_SAMP" q [a, b]
    -- corrE q a b          = Expression_Agg "CORR" q [a, b]
    -- regrSlopeE q a b     = Expression_Agg "REGR_SLOPE" q [a, b]
    -- regrInterceptE q a b = Expression_Agg "REGR_INTERCEPT" q [a, b]
    -- regrCountE q a b     = Expression_Agg "REGR_COUNT" q [a, b]
    -- regrRSquaredE q a b  = Expression_Agg "REGR_R2" q [a, b]
    -- regrAvgXE q a b      = Expression_Agg "REGR_AVGX" q [a, b]
    -- regrAvgYE q a b      = Expression_Agg "REGR_AVGY" q [a, b]
    -- regrSXXE q a b       = Expression_Agg "REGR_SXX" q [a, b]
    -- regrSXYE q a b       = Expression_Agg "REGR_SXY" q [a, b]
    -- regrSYYE q a b       = Expression_Agg "REGR_SYY" q [a, b]

instance
    (IsSql2003NtileExpressionSyntax tr, IsSql2003NtileExpressionSyntax sn) =>
    IsSql2003NtileExpressionSyntax (TracedSyntax tr sn)
  where
    -- ntileE = Expression_Agg "NTILE" Nothing . pure

instance
    (IsSql2003LeadAndLagExpressionSyntax tr, IsSql2003LeadAndLagExpressionSyntax sn) =>
    IsSql2003LeadAndLagExpressionSyntax (TracedSyntax tr sn)
  where
    -- leadE x Nothing Nothing   = Expression_Agg "LEAD" Nothing [x]
    -- leadE x (Just y) Nothing  = Expression_Agg "LEAD" Nothing [x, y]
    -- leadE x (Just y) (Just z) = Expression_Agg "LEAD" Nothing [x, y, z]
    -- leadE x Nothing (Just z)  =
        -- Expression_Agg "LEAD" Nothing [x, Expression_Value (Value (Aeson.Number 1)), z]

    -- lagE x Nothing Nothing  = Expression_Agg "LAG" Nothing [x]
    -- lagE x (Just y) Nothing = Expression_Agg "LAG" Nothing [x, y]
    -- lagE x (Just y) (Just z) = Expression_Agg "LAG" Nothing [x, y, z]
    -- lagE x Nothing (Just z)  =
        -- Expression_Agg "LAG" Nothing [x, Expression_Value (Value (Aeson.Number 1)), z]

instance
    (IsSql2003NthValueExpressionSyntax tr, IsSql2003NthValueExpressionSyntax sn) =>
    IsSql2003NthValueExpressionSyntax (TracedSyntax tr sn)
  where
  -- nthValueE a b = Expression_Agg "NTH_VALUE" Nothing [a, b]

instance
    (IsSql2003ExpressionSyntax tr, IsSql2003ExpressionSyntax sn) =>
    IsSql2003ExpressionSyntax (TracedSyntax tr sn)
  where
    type Sql2003ExpressionWindowFrameSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql2003ExpressionWindowFrameSyntax tr) (Sql2003ExpressionWindowFrameSyntax sn)

    -- overE = Expression_Over
    -- rowNumberE = Expression_Agg "ROW_NUMBER" Nothing []

instance
    (IsSql92ProjectionSyntax tr, IsSql92ProjectionSyntax sn) =>
    IsSql92ProjectionSyntax (TracedSyntax tr sn)
  where
    type Sql92ProjectionExpressionSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92ProjectionExpressionSyntax tr) (Sql92ProjectionExpressionSyntax sn)

    projExprs projList =
        TracedSyntax
            (projExprs (map (\(e, n) -> (_tr e, n)) projList))
            (projExprs (map (\(e, n) -> (_sn e, n)) projList))

instance
    (IsSql92OrderingSyntax tr, IsSql92OrderingSyntax sn) =>
    IsSql92OrderingSyntax (TracedSyntax tr sn)
  where
    type Sql92OrderingExpressionSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92OrderingExpressionSyntax tr) (Sql92OrderingExpressionSyntax sn)

    ascOrdering col =
        TracedSyntax
            (ascOrdering (_tr col))
            (ascOrdering (_sn col))
    descOrdering col =
        TracedSyntax
            (descOrdering (_tr col))
            (descOrdering (_sn col))

instance
    (IsSql92GroupingSyntax tr, IsSql92GroupingSyntax sn) =>
    IsSql92GroupingSyntax (TracedSyntax tr sn)
  where
    type Sql92GroupingExpressionSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92GroupingExpressionSyntax tr) (Sql92GroupingExpressionSyntax sn)

    groupByExpressions exprs =
        TracedSyntax
            (groupByExpressions (fmap _tr exprs))
            (groupByExpressions (fmap _sn exprs))

instance
    (IsSql92TableNameSyntax tr, IsSql92TableNameSyntax sn) =>
    IsSql92TableNameSyntax (TracedSyntax tr sn)
  where
    tableName a b =
        TracedSyntax
            (tableName a b)
            (tableName a b)

instance
    (IsSql92TableSourceSyntax tr, IsSql92TableSourceSyntax sn) =>
    IsSql92TableSourceSyntax (TracedSyntax tr sn)
  where
    type Sql92TableSourceSelectSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92TableSourceSelectSyntax tr) (Sql92TableSourceSelectSyntax sn)
    type Sql92TableSourceExpressionSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92TableSourceExpressionSyntax tr) (Sql92TableSourceExpressionSyntax sn)
    type Sql92TableSourceTableNameSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92TableSourceTableNameSyntax tr) (Sql92TableSourceTableNameSyntax sn)

    tableNamed name =
        TracedSyntax
            (tableNamed (_tr name))
            (tableNamed (_sn name))
    tableFromSubSelect sel =
        TracedSyntax
            (tableFromSubSelect (_tr sel))
            (tableFromSubSelect (_sn sel))
    tableFromValues vals =
        TracedSyntax
            (tableFromValues (fmap (fmap _tr) vals))
            (tableFromValues (fmap (fmap _sn) vals))

instance
    (IsSql92FromSyntax tr, IsSql92FromSyntax sn) =>
    IsSql92FromSyntax (TracedSyntax tr sn)
  where
    type Sql92FromTableSourceSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92FromTableSourceSyntax tr) (Sql92FromTableSourceSyntax sn)
    type Sql92FromExpressionSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql92FromExpressionSyntax tr) (Sql92FromExpressionSyntax sn)

    fromTable src mbAlias =
        TracedSyntax
            (fromTable (_tr src) mbAlias)
            (fromTable (_sn src) mbAlias)
    innerJoin src1 src2 mbOn =
        TracedSyntax
            (innerJoin (_tr src1) (_tr src2) (fmap _tr mbOn))
            (innerJoin (_sn src1) (_sn src2) (fmap _sn mbOn))
    leftJoin src1 src2 mbOn =
        TracedSyntax
            (leftJoin (_tr src1) (_tr src2) (fmap _tr mbOn))
            (leftJoin (_sn src1) (_sn src2) (fmap _sn mbOn))
    rightJoin src1 src2 mbOn =
        TracedSyntax
            (rightJoin (_tr src1) (_tr src2) (fmap _tr mbOn))
            (rightJoin (_sn src1) (_sn src2) (fmap _sn mbOn))
instance
    (HasSqlValueSyntax tr a, HasSqlValueSyntax sn a) =>
    HasSqlValueSyntax (TracedSyntax tr sn) a
  where
    sqlValueSyntax x =
        TracedSyntax
            (sqlValueSyntax x)
            (sqlValueSyntax x)

instance
    (IsSql2003WindowFrameSyntax tr, IsSql2003WindowFrameSyntax sn) =>
    IsSql2003WindowFrameSyntax (TracedSyntax tr sn)
  where
    type Sql2003WindowFrameExpressionSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql2003WindowFrameExpressionSyntax tr) (Sql2003WindowFrameExpressionSyntax sn)
    type Sql2003WindowFrameOrderingSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql2003WindowFrameOrderingSyntax tr) (Sql2003WindowFrameOrderingSyntax sn)
    type Sql2003WindowFrameBoundsSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql2003WindowFrameBoundsSyntax tr) (Sql2003WindowFrameBoundsSyntax sn)

    frameSyntax mbPartList mbOrdList mbBound =
        TracedSyntax
            (frameSyntax
                (fmap (fmap _tr) mbPartList)
                (fmap (fmap _tr) mbOrdList)
                (fmap _tr mbBound)
            )
            (frameSyntax
                (fmap (fmap _sn) mbPartList)
                (fmap (fmap _sn) mbOrdList)
                (fmap _sn mbBound)
            )

instance
    (IsSql2003WindowFrameBoundsSyntax tr, IsSql2003WindowFrameBoundsSyntax sn) =>
    IsSql2003WindowFrameBoundsSyntax (TracedSyntax tr sn)
  where
    type Sql2003WindowFrameBoundsBoundSyntax (TracedSyntax tr sn) =
        TracedSyntax (Sql2003WindowFrameBoundsBoundSyntax tr) (Sql2003WindowFrameBoundsBoundSyntax sn)

    fromToBoundSyntax from mbTo =
        TracedSyntax
            (fromToBoundSyntax (_tr from) (fmap _tr mbTo))
            (fromToBoundSyntax (_sn from) (fmap _sn mbTo))

instance
    (IsSql2003WindowFrameBoundSyntax tr, IsSql2003WindowFrameBoundSyntax sn) =>
    IsSql2003WindowFrameBoundSyntax (TracedSyntax tr sn)
  where
    unboundedSyntax =
        TracedSyntax
            unboundedSyntax
            unboundedSyntax
    nrowsBoundSyntax a =
        TracedSyntax
            (nrowsBoundSyntax a)
            (nrowsBoundSyntax a)
-}
