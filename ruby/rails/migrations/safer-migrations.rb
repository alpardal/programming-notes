# source/details: https://medium.com/doctolib/stop-worrying-about-postgresql-locks-in-your-rails-migrations-3426027e9cc9
class AddingIndexesSafely < ActiveRecord::Migration[6.0]
  disable_ddl_transaction!

  def change
    old_value = query_value('SHOW statement_timeout')
    execute "SET statement_timeout TO '0s'"

    add_index :appointments, :patient_id, algorithm: :concurrently
    execute "SET statement_timeout TO '#{quote(old_value)}s'"
  end
end

class AddForeignKeysSafely < ActiveRecord::Migration[6.0]
  def change
    old_value = query_value('SHOW statement_timeout')
    execute "SET statement_timeout TO '5s'"

    add_foreign_key :appointments, :patient, validate: false

    # validating the foreign key is not blocking, we can afford a long running query
    execute "SET statement_timeout TO '0s'"

    validate_foreign_key :appointments, :patient

    execute "SET statement_timeout TO '#{quote(old_value)}s'"
  end
end
