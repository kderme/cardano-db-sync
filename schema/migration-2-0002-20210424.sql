-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 2 THEN
    EXECUTE 'CREATe TABLE "pool_metadata_ref"("id" SERIAL8  PRIMARY KEY UNIQUE,"pool_id" INT8 NOT NULL,"url" VARCHAR NOT NULL,"hash" hash32type NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_metadata_ref" ADD CONSTRAINT "unique_pool_metadata_ref" UNIQUE("pool_id","hash")' ;
    EXECUTE 'ALTER TABLE "pool_metadata_ref" ADD CONSTRAINT "pool_metadata_ref_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool_hash"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'CREATe TABLE "pool_metadata"("id" SERIAL8  PRIMARY KEY UNIQUE,"pool_id" INT8 NOT NULL,"ticker_name" VARCHAR NOT NULL,"hash" hash32type NOT NULL,"metadata" VARCHAR NOT NULL,"pmr_id" INT8 NULL)' ;
    EXECUTE 'ALTER TABLE "pool_metadata" ADD CONSTRAINT "unique_pool_metadata" UNIQUE("pool_id","hash")' ;
    EXECUTE 'ALTER TABLE "pool_metadata" ADD CONSTRAINT "pool_metadata_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool_hash"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "pool_metadata" ADD CONSTRAINT "pool_metadata_pmr_id_fkey" FOREIGN KEY("pmr_id") REFERENCES "pool_metadata_ref"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'CREATe TABLE "pool_metadata_fetch_error"("id" SERIAL8  PRIMARY KEY UNIQUE,"fetch_time" timestamp NOT NULL,"pool_id" INT8 NOT NULL,"pmr_id" INT8 NOT NULL,"fetch_error" VARCHAR NOT NULL,"retry_count" uinteger NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_metadata_fetch_error" ADD CONSTRAINT "unique_pool_metadata_fetch_error" UNIQUE("fetch_time","pool_id","retry_count")' ;
    EXECUTE 'ALTER TABLE "pool_metadata_fetch_error" ADD CONSTRAINT "pool_metadata_fetch_error_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool_hash"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "pool_metadata_fetch_error" ADD CONSTRAINT "pool_metadata_fetch_error_pmr_id_fkey" FOREIGN KEY("pmr_id") REFERENCES "pool_metadata_ref"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'CREATe TABLE "delisted_pool"("id" SERIAL8  PRIMARY KEY UNIQUE,"pool_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "delisted_pool" ADD CONSTRAINT "unique_delisted_pool" UNIQUE("pool_id")' ;
    EXECUTE 'ALTER TABLE "delisted_pool" ADD CONSTRAINT "delisted_pool_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool_hash"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'CREATe TABLE "reserved_ticker"("id" SERIAL8  PRIMARY KEY UNIQUE,"name" VARCHAR NOT NULL,"pool_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "reserved_ticker" ADD CONSTRAINT "unique_reserved_ticker" UNIQUE("name")' ;
    EXECUTE 'ALTER TABLE "reserved_ticker" ADD CONSTRAINT "reserved_ticker_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool_hash"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'CREATe TABLE "admin_user"("id" SERIAL8  PRIMARY KEY UNIQUE,"username" VARCHAR NOT NULL,"password" VARCHAR NOT NULL)' ;
    EXECUTE 'ALTER TABLE "admin_user" ADD CONSTRAINT "unique_admin_user" UNIQUE("username")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
