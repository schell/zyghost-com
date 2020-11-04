provider "aws" {
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

// variables
locals {
  domain_name = "preview.zyghost.com"
}

data "aws_route53_zone" "zone" {
  provider = aws.virginia
  zone_id = "Z9RURLYZAEX1T"
}

// origin id & cloudfront
resource "aws_cloudfront_origin_access_identity" "origin_identity" {
  comment = "identity for ${local.domain_name} access origin"
}

// s3
resource "aws_s3_bucket" "site_bucket" {
  bucket = local.domain_name
  acl = "private"

  policy = <<POLICY
{
    "Version": "2008-10-17",
    "Id": "PolicyForCloudFrontPrivateContent",
    "Statement": [
        {
            "Sid": "1",
            "Effect": "Allow",
            "Principal": {
                "AWS": "${aws_cloudfront_origin_access_identity.origin_identity.iam_arn}"
            },
            "Action": "s3:GetObject",
            "Resource": "arn:aws:s3:::${local.domain_name}/*"
        }
    ]
}
POLICY

  provisioner "local-exec" {
    when = destroy
    command = "aws s3 rm s3://${self.id}/ --recursive"
  }
}

// s3 site sync
resource "null_resource" "aws_sync" {
  provisioner "local-exec" {
    command = "aws s3 sync build/site s3://${local.domain_name}/"
  }

  triggers = {
    always_run = timestamp()
  }

  depends_on = [aws_s3_bucket.site_bucket]
}


// ssl & route53 record stuff
resource "aws_acm_certificate" "cert" {
  provider = aws.virginia
  validation_method = "DNS"
  domain_name = local.domain_name
  tags = {
    Name = "cert"
    Domain = local.domain_name
  }
  lifecycle {
    create_before_destroy = true
  }
}

resource "aws_route53_record" "cert_validation" {
  for_each = {
    for dvo in aws_acm_certificate.cert.domain_validation_options : dvo.domain_name => {
      name   = dvo.resource_record_name
      record = dvo.resource_record_value
      type   = dvo.resource_record_type
    }
  }

  provider = aws.virginia
  zone_id = data.aws_route53_zone.zone.id
  ttl = 60

  name = each.value.name
  type = each.value.type
  records = [each.value.record]
}


resource "aws_acm_certificate_validation" "cert_validation" {
  provider                = aws.virginia
  certificate_arn         = aws_acm_certificate.cert.arn
  validation_record_fqdns = [for record in aws_route53_record.cert_validation : record.fqdn]

  timeouts {
    create = "60m"
  }
  lifecycle {
    ignore_changes = [id]
  }
}

resource "aws_cloudfront_distribution" "distribution" {
  origin {
    domain_name = aws_s3_bucket.site_bucket.bucket_regional_domain_name
    origin_id = aws_s3_bucket.site_bucket.id
    s3_origin_config {
      origin_access_identity = aws_cloudfront_origin_access_identity.origin_identity.cloudfront_access_identity_path
    }
  }
  enabled = true
  is_ipv6_enabled = true
  comment = "${local.domain_name} cloudfront"
  default_root_object = "index.html"
  aliases = [local.domain_name]

  default_cache_behavior {
    allowed_methods  = ["GET", "HEAD", "OPTIONS"]
    cached_methods   = ["GET", "HEAD"]
    target_origin_id = aws_s3_bucket.site_bucket.id

    forwarded_values {
      query_string = false

      cookies {
        forward = "none"
      }
    }
    viewer_protocol_policy = "redirect-to-https"
    min_ttl                = 0
    default_ttl            = 3600
    max_ttl                = 86400
  }

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  // Here's where our certificate is loaded in!
  viewer_certificate {
    acm_certificate_arn = aws_acm_certificate_validation.cert_validation.certificate_arn
    ssl_support_method  = "sni-only"
    minimum_protocol_version = "TLSv1.1_2016"
  }
}


// last bit of route53 stuff
resource "aws_route53_record" "main" {
  zone_id = data.aws_route53_zone.zone.zone_id
  name = ""
  type = "A"
  alias {
    name = aws_cloudfront_distribution.distribution.domain_name
    zone_id = aws_cloudfront_distribution.distribution.hosted_zone_id
    evaluate_target_health = false
  }
}
