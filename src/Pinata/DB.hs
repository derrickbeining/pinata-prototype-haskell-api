module Pinata.DB where

import Data.Function ((&))
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time (UTCTime)

import Data.Time.LocalTime (LocalTime)
import qualified Opaleye as DB

-------------------------------------------------------------------------------
type F a = DB.Field a

-------------------------------------------------------------------------------
data TimestampedRow' a b c = TimestampedRow
  { record :: a
  , recordCreatedAt :: b
  , recordUpdatedAt :: c
  }

$(makeAdaptorAndInstance "pTimestampedRow" ''TimestampedRow')

-------------------------------------------------------------------------------
type TimestampedRow a = TimestampedRow' a UTCTime UTCTime

type TimestampedWriteField a =
  TimestampedRow' a (Maybe (F DB.SqlTimestamptz)) (Maybe (F DB.SqlTimestamptz))

type TimestampedReadField a =
  TimestampedRow' a (F DB.SqlTimestamptz) (F DB.SqlTimestamptz)

-------------------------------------------------------------------------------
withTimestampFields ::
  a ->
  TimestampedRow'
    a
    (DB.TableFields (Maybe (F DB.SqlTimestamptz)) (F DB.SqlTimestamptz))
    (DB.TableFields (Maybe (F DB.SqlTimestamptz)) (F DB.SqlTimestamptz))
withTimestampFields mapping =
  TimestampedRow
    { record = mapping
    , recordCreatedAt = DB.tableField "created_at"
    , recordUpdatedAt = DB.tableField "updated_at"
    }

-------------------------------------------------------------------------------
withTimestamp ::
  [row] ->
  [TimestampedRow' row (Maybe timestamp) (Maybe timestamp)]
withTimestamp = map f
  where
    f r =
      TimestampedRow
        { record = r
        , recordCreatedAt = Nothing
        , recordUpdatedAt = Nothing
        }

-------------------------------------------------------------------------------
updateTimestampedRecord ::
  Default DB.Updater (TimestampedRow' record t t) fieldsW =>
  (record -> record) ->
  TimestampedRow' record t t ->
  fieldsW
updateTimestampedRecord f =
  DB.updateEasy
    ( \r ->
        r
          { record = f (record r)
          }
    )

-------------------------------------------------------------------------------

-- | This is needed because, for some reason, some tables use `timestamp` instead of `timestamptz`
-- | for the `created_at` and `updated_at` fields.
type LocalTimestampedRow a = TimestampedRow' a LocalTime LocalTime

type LocalTimestampedWriteField a =
  TimestampedRow' a (Maybe (F DB.SqlTimestamp)) (Maybe (F DB.SqlTimestamp))

type LocalTimestampedReadField a =
  TimestampedRow' a (F DB.SqlTimestamp) (F DB.SqlTimestamp)

-------------------------------------------------------------------------------
withLocalTimestampFields ::
  a ->
  TimestampedRow'
    a
    (DB.TableFields (Maybe (F DB.SqlTimestamp)) (F DB.SqlTimestamp))
    (DB.TableFields (Maybe (F DB.SqlTimestamp)) (F DB.SqlTimestamp))
withLocalTimestampFields mapping =
  TimestampedRow
    { record = mapping
    , recordCreatedAt = DB.tableField "created_at"
    , recordUpdatedAt = DB.tableField "updated_at"
    }

-------------------------------------------------------------------------------
withLocalTimestamp ::
  [row] ->
  [TimestampedRow' row (Maybe timestamp) (Maybe timestamp)]
withLocalTimestamp = map f
  where
    f r =
      TimestampedRow
        { record = r
        , recordCreatedAt = Nothing
        , recordUpdatedAt = Nothing
        }

-------------------------------------------------------------------------------
updateLocalTimestampedRecord ::
  Default DB.Updater (TimestampedRow' record t t) fieldsW =>
  (record -> record) ->
  TimestampedRow' record t t ->
  fieldsW
updateLocalTimestampedRecord f =
  DB.updateEasy
    ( \r ->
        r
          { record = f (record r)
          }
    )
