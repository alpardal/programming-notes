provider "aws" {
  profile = "default"
  region  = var.region
  version = "~> 2.19"
}

variable "private_key" {
  type    = string
  default = "bla"
}

locals {
  something = var.private_key
}

data "aws_ami" "ubuntu" {
  most_recent = true

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-bionic-18.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["099720109477"] # Canonical
}

data "aws_vpc" "production_vpc" {
  id = "vpc-27383474828347483"
}

data "aws_subnet" "public_subnet" {
  vpc_id = data.aws_vpc.production_vpc.id
  id     = "subnet-91928371928371928"
}

resource "aws_db_snapshot" "db_snapshot" {
  db_instance_identifier = data.aws_db_instance.acceptance_db.id
  db_snapshot_identifier = "acceptance-db-${formatdate("YYYYMMDDhhmmss", timestamp())}"
}

resource "aws_instance" "worker" {
  ami           = data.aws_ami.ubuntu.id
  instance_type = "t3.medium"
  # disable_api_termination     = true
  associate_public_ip_address = true
  key_name                    = var.private_key
  iam_instance_profile        = "S3-access"

  subnet_id = data.aws_subnet.public_subnet.id

  vpc_security_group_ids = [
    "sg-22939483749394920"
  ]

  credit_specification {
    cpu_credits = "standard"
  }

  tags = {
    Name = "my-worker"
  }
  volume_tags = {
    Name = "my-worker"
  }

  root_block_device {
    volume_type           = "gp2"
    volume_size           = 20 # in GBs
    delete_on_termination = true
  }

  provisioner "remote-exec" {
    inline = [
      "sudo apt-get install -y python"
    ]
  }

  connection {
    type        = "ssh"
    user        = "ubuntu"
    private_key = file("~/.ssh/${var.private_key}.pem")
    host        = self.public_ip
  }
}
