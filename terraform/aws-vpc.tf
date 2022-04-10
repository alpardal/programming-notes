#
#  create a new vpc with a public subnet and launch an ec2 instance within it
#
provider "aws" {
  profile = "andre"
  region  = "sa-east-1"
}

variable "my_ip" {
  type    = string
  default = "186.223.172.16/32"
}

variable "private_key" {
  type    = string
  default = "andre-tests"
}

locals {
  default_tags = {
    Name   = "test",
    System = "aws-tests"
  }
}

data "aws_ami" "ubuntu_latest" {
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

resource "aws_vpc" "test" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_hostnames = true
  tags                 = local.default_tags
}

resource "aws_internet_gateway" "gw" {
  vpc_id = aws_vpc.test.id
}

resource "aws_subnet" "public" {
  vpc_id                  = aws_vpc.test.id
  cidr_block              = "10.0.1.0/24"
  map_public_ip_on_launch = true

  tags = local.default_tags
}

resource "aws_route_table" "routes" {
  vpc_id = aws_vpc.test.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.gw.id
  }
}

resource "aws_route_table_association" "assoc" {
  subnet_id      = aws_subnet.public.id
  route_table_id = aws_route_table.routes.id
}

resource "aws_security_group" "ssh_access" {
  name        = "ssh-access"
  description = "allows ssh access"
  vpc_id      = aws_vpc.test.id

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = [var.my_ip]
  }
}

resource "aws_instance" "worker" {
  depends_on = [aws_route_table_association.assoc]

  ami                         = data.aws_ami.ubuntu_latest.id
  instance_type               = "t3.micro"
  associate_public_ip_address = true
  key_name                    = var.private_key

  subnet_id              = aws_subnet.public.id
  vpc_security_group_ids = [aws_security_group.ssh_access.id]

  credit_specification {
    cpu_credits = "standard"
  }

  tags = {
    Name = "my-worker"
  }

  user_data = <<-EOF
    #!bin/bash
    echo 'started!' >/home/ubuntu/user-log
  EOF
}

output "instance_dns" {
  value = aws_instance.worker.public_dns
}

output "ssh" {
  value = "ssh ubuntu@${aws_instance.worker.public_dns}"
}
