provider "aws" {
  region = "us-east-1"
  # profile = "prevu"
}

locals {
  sync_app_db_instances = "sg-0cb1c146b6b8114ef" # allows db conection *from* sync app instances
}

resource "random_password" "db_password" {
  length  = 16
  special = false
}
resource "random_string" "db_username" {
  length  = 20
  upper   = false
  number  = false
  lower   = true
  special = false
}

resource "aws_db_instance" "sync_db_1" {
  engine                 = "postgres"
  instance_class         = "db.r5.large"
  identifier             = "rds-listing-sync-db1"
  storage_type           = "gp2"
  allocated_storage      = 40   # in GBs
  max_allocated_storage  = 1000 # ditto
  username               = random_string.db_username.result
  password               = random_password.db_password.result
  db_subnet_group_name   = "sng-production-private-subnets"
  vpc_security_group_ids = [local.sync_app_db_instances]
  storage_encrypted      = true
  publicly_accessible    = false
}

locals {
  database_url = join("", [
    "postgres://",
    aws_db_instance.sync_db_1.username,
    ":",
    aws_db_instance.sync_db_1.password,
    "@",
    aws_db_instance.sync_db_1.endpoint,
    "/listing_sync_production"
  ])
}

output "database_url" {
  value = local.database_url
}
