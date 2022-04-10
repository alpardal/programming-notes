
variable "private_key" {
  type = string
  default = "bla"
}

locals {
  something = var.private_key
}
