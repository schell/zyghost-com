provider "aws" {
  region = "us-west-1"
}

provider "aws" {
  alias = "oregon"
  region = "us-west-2"
}

provider "aws" {
  alias = "virginia"
  region = "us-east-1"
}

terraform {
  backend "s3" {
    bucket = "zyghost-terraform"
    key    = "zyghost"
    region = "us-west-2"
  }
}

module "root" {
  source = "../mars/modules/static_site"
  zone_id = "Z9RURLYZAEX1T"
  domain_name = "zyghost.com"
}

module "preview" {
  source = "../mars/modules/static_site"
  zone_id = "Z9RURLYZAEX1T"
  domain_name = "preview.zyghost.com"
}
