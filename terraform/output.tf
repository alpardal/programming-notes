
# you can grab the value by running
#   `terraform output snapshot_arn`
output "snapshot_arn" {
  value = data.aws_db_snapshot.db_snapshot.db_snapshot_arn
  description = "the arn for the captured snapshot"
}
